// Copyright (c) 2022 Contributors to the Eclipse Foundation
//
// See the NOTICE file(s) distributed with this work for additional
// information regarding copyright ownership.
//
// This program and the accompanying materials are made available under the
// terms of the Eclipse Public License 2.0 which is available at
// http://www.eclipse.org/legal/epl-2.0
//
// SPDX-License-Identifier: EPL-2.0

package storage

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"net/http"
	"net/url"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/eclipse-kanto/software-update/hawkbit"
)

// TestHTTPServer encapsulates a test HTTP(S) server and supplementary data
type TestHTTPServer struct {
	name string
	size int64
	srv  *http.Server
	t    *testing.T
}

var (
	// testOnceRange is used to add HTTP alias only once for the range server.
	testOnceRange sync.Once
	// testOnceSimple is used to add HTTP alias only once for the simple server.
	testOnceSimple sync.Once

	failCountBadStatus int
	failCopyError      bool
	corruptFileError   bool
)

var (
	// testAliases is used to add HTTP alias only once.
	testAliases map[string]string = make(map[string]string)
)

// handlerSimpleDynamicAlias handles incoming HTTP requests without range header support.
func handlerSimpleDynamicAlias(writer http.ResponseWriter, request *http.Request) {
	alias := request.URL.Path[strings.LastIndex(request.URL.Path, "/")+1:]
	body := testAliases[alias]

	writer.Header().Set("Content-Type", "text/plain")
	writer.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, alias))
	writer.Header().Set("Content-Length", strconv.Itoa(len(body)))
	writer.Write([]byte(body))
}

// NewTestHTTPServer initializes a test HTTP(s) server, without starting it
func NewTestHTTPServer(addr string, name string, size int64, t *testing.T) *TestHTTPServer {
	return &TestHTTPServer{name: name, size: size, srv: &http.Server{Addr: addr}, t: t}
}

// Host starts a HTTP(s) server.
func (w *TestHTTPServer) Host(simple bool, secure bool, cert string, key string) {

	// Add HTTP alias only once.
	if simple {
		testOnceSimple.Do(func() { http.HandleFunc(fmt.Sprintf("/%s", w.name), w.handlerSimple) })
	} else {
		testOnceRange.Do(func() { http.HandleFunc(fmt.Sprintf("/%s", w.name), w.handlerRange) })
	}

	// Start HTTP server in separate goroute.
	if secure {
		go func() {
			if err := w.srv.ListenAndServeTLS(cert, key); err != nil && err != http.ErrServerClosed {
				w.t.Errorf("Failed to start secure http server: %v", err)
			}
		}()
	} else {
		go func() {
			if err := w.srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				w.t.Errorf("Failed to start http server: %v", err)
			}
		}()
	}

	time.Sleep(1 * time.Second)
}

func setIncorrectBehavior(failCountBadStatusNum int, doFailCopyError bool, doCorruptFileError bool) {
	failCountBadStatus = failCountBadStatusNum
	failCopyError = doFailCopyError
	corruptFileError = doCorruptFileError
}

// setAlias sets HTTP alias (only once).
func (w *TestHTTPServer) setAlias(alias string, body string) {
	if _, ok := testAliases[alias]; !ok {
		http.HandleFunc(fmt.Sprintf("/%s", alias), handlerSimpleDynamicAlias)
	}
	testAliases[alias] = body
}

// AddInstallScript adds install script HTTP alias (only once).
func (w *TestHTTPServer) AddInstallScript() {
	if runtime.GOOS == "windows" {
		w.setAlias("install.bat", "@echo off\n(\necho message=My final message!) > status\nping 127.0.0.1\n")
	} else {
		w.setAlias("install.sh", "#!/bin/sh\necho 'message=My final message!\n' > status\nsleep 5\n")
	}
}

// Close closes the http server.
func (w *TestHTTPServer) Close() {
	if err := w.srv.Shutdown(context.Background()); err != nil {
		w.t.Errorf("failed shutdown of http server: %v", err)
	}
}

// handlerRange handles incoming HTTP requests with range header support.
func (w *TestHTTPServer) handlerRange(writer http.ResponseWriter, request *http.Request) {
	if failCountBadStatus > 0 {
		failCountBadStatus--
		writer.WriteHeader(http.StatusBadRequest)
		return
	}
	// Set default headers for txt file.
	writer.Header().Set("Content-Type", "text/plain")
	writer.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, w.name))
	writer.Header().Set("Accept-Ranges", "bytes")

	// Parse range header -> 'Range: bytes=<begin>-<end>'
	r := request.Header.Get("range")
	if r == "" { // No range is defined
		writer.Header().Set("Content-Length", strconv.Itoa(int(w.size)))
		if failCopyError {
			write(writer, w.size/3, corruptFileError)
		} else {
			write(writer, w.size, corruptFileError)
		}
		return
	}

	// Client requests a part of the file
	var err error
	begin, end := int64(0), int64(w.size-1)
	r = r[6:] // Strip the "bytes=", left over is now "begin-end"
	pos := strings.Index(r, "-")
	if pos < 0 {
		http.Error(writer, "invalid values for header 'Range'", http.StatusBadRequest)
		return
	}

	// Parse begin part of the range
	sb := r[:pos]
	if len(sb) > 0 {
		begin, err = strconv.ParseInt(sb, 10, 64)
		if err != nil || begin > w.size {
			http.Error(writer, "invalid values for header 'Range' begin", http.StatusBadRequest)
		}
	}

	// Parse end part of the range
	se := r[pos+1:]
	if len(se) > 0 {
		end, err = strconv.ParseInt(se, 10, 64)
		if err != nil || end > w.size || begin >= end {
			http.Error(writer, "invalid values for header 'Range' end", http.StatusBadRequest)
		}
	}

	writer.Header().Set("Content-Length", strconv.FormatInt(end-begin+1, 10))
	writer.Header().Set("Content-Range", fmt.Sprintf("bytes %d-%d/%d", begin, end, w.size))
	writer.WriteHeader(http.StatusPartialContent)

	// Send the (end-begin) amount of bytes to the client
	if failCopyError {
		write(writer, (end-begin+1)/3, corruptFileError)
	} else {
		write(writer, end-begin+1, corruptFileError)
	}
}

// handlerSimple handles incoming HTTP requests without range header support.
func (w *TestHTTPServer) handlerSimple(writer http.ResponseWriter, request *http.Request) {
	writer.Header().Set("Content-Type", "text/plain")
	writer.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, w.name))
	writer.Header().Set("Content-Length", strconv.Itoa(int(w.size)))
	write(writer, w.size, corruptFileError)
}

// write file content.
func write(writer http.ResponseWriter, size int64, corruptFile bool) {
	b := []byte("11111111111111111111111111111111111111111111111111")
	if corruptFile {
		b[0] = '0'
	}
	s := int64(len(b))
	for i := s; i < size; i += s {
		writer.Write(b)
	}
	remainder := size % s
	writer.Write(b[:remainder])
}

func isSecure(link string, t *testing.T) bool {
	u, err := url.Parse(link)
	if err != nil {
		t.Fatalf("cannot parse url %v", link)
	}
	return u.Scheme == "https"
}

// GenerateSoftwareArtifacts generates array of SoftwareArtifactAction based on the registered HTTP aliases with the given names.
func (w *TestHTTPServer) GenerateSoftwareArtifacts(secure bool, names ...string) []*hawkbit.SoftwareArtifactAction {
	var protocol hawkbit.Protocol
	if secure {
		protocol = hawkbit.HTTPS
	} else {
		protocol = hawkbit.HTTP
	}
	res := make([]*hawkbit.SoftwareArtifactAction, len(names))
	for i, name := range names {
		// If alias name is "install", add its file extension: Windows = .bat | Linux/Mac/etc = .sh
		alias := name
		if alias == "install" {
			if runtime.GOOS == "windows" {
				alias += ".bat"
			} else {
				alias += ".sh"
			}
		}
		body := testAliases[alias]

		// Calculate alias SHA256 hash
		hType := sha256.New()
		hType.Write([]byte(body))
		hash := hex.EncodeToString(hType.Sum(nil))

		// Create SoftwareArtifactAction for coresponding alias
		res[i] = &hawkbit.SoftwareArtifactAction{
			Filename: alias,
			Download: map[hawkbit.Protocol]*hawkbit.Links{
				protocol: {URL: fmt.Sprintf("%v://localhost%s/%s", protocol, w.srv.Addr, alias)},
			},
			Checksums: map[hawkbit.Hash]string{
				hawkbit.SHA256: hash,
			},
			Size: len(body),
		}
	}
	return res
}
