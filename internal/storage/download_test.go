// Copyright (c) 2021 Contributors to the Eclipse Foundation
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
	"fmt"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

var (
	// testOnceRange is used to add HTTP alias only once for the range server.
	testOnceRange sync.Once
	// testOnceSimple is used to add HTTP alias only once for the simple server.
	testOnceSimple sync.Once
)

const (
	validCert   = "testdata/valid_cert.pem"
	validKey    = "testdata/valid_key.pem"
	expiredCert = "testdata/expired_cert.pem"
	expiredKey  = "testdata/expired_key.pem"
	extCert     = "testdata/ext_cert.pem"
	extKey      = "testdata/ext_key.pem"
)

type web struct {
	name string
	size int64
	srv  *http.Server
	t    *testing.T
}

// TestDownloadToFile tests downloadToFile function, using non-secure protocol(s).
func TestDownloadToFile(t *testing.T) {
	testDownloadToFile([]*Artifact{
		{ // An Artifact with MD5 checksum.
			FileName: "test.txt", Size: 65536, Link: "http://localhost:43234/test.txt",
			HashType:  "MD5",
			HashValue: "ab2ce340d36bbaafe17965a3a2c6ed5b",
		},
		{ // An Artifact with SHA1 checksum.
			FileName: "test.txt", Size: 65536, Link: "http://localhost:43234/test.txt",
			HashType:  "SHA1",
			HashValue: "cd3848697cb42f5be9902f6523ec516d21a8c677",
		},
		{ // An Artifact with SHA256 checksum.
			FileName: "test.txt", Size: 65536, Link: "http://localhost:43234/test.txt",
			HashType:  "SHA256",
			HashValue: "4eefb9a7a40a8b314b586a00f307157043c0bbe4f59fa39cba88773680758bc3",
		},
	}, "", "", t)
}

// TestDownloadToFileSecureSystemPool tests downloadToFile function, using secure protocol(s) and certificates from system pool.
func TestDownloadToFileSecureSystemPool(t *testing.T) {
	// SystemCertPool does not run on Windows:
	//https://github.com/golang/go/issues/16736
	//Fixed in 1.18
	if runtime.GOOS != "linux" {
		t.Skip("this test only runs on Linux.")
	}
	sslCertFile := os.Getenv("SSL_CERT_FILE")
	certFiles := strings.Split(sslCertFile, string(os.PathListSeparator))
	containsTestCert := false
	for _, file := range certFiles {
		if strings.HasSuffix(strings.TrimSpace(file), filepath.Base(validCert)) {
			containsTestCert = true
			break
		}
	}
	if !containsTestCert {
		t.Skipf("Please set SSL_CERT_FILE variable to point to the location"+
			" of %s certificate file. Current value is \"%s\".", validCert, sslCertFile)
	}
	testDownloadToFileSecure("", "", t)
}

// TestDownloadToFileSecureCustomCertificate tests downloadToFile function, using secure protocol(s) and a custom certificate.
func TestDownloadToFileSecureCustomCertificate(t *testing.T) {
	testDownloadToFileSecure(validCert, validKey, t)
}

func testDownloadToFileSecure(certFile, certKey string, t *testing.T) {
	testDownloadToFile([]*Artifact{
		{ // An Artifact with MD5 checksum.
			FileName: "test.txt", Size: 65536, Link: "https://localhost:43234/test.txt",
			HashType:  "MD5",
			HashValue: "ab2ce340d36bbaafe17965a3a2c6ed5b",
		},
		{ // An Artifact with SHA1 checksum.
			FileName: "test.txt", Size: 65536, Link: "https://localhost:43234/test.txt",
			HashType:  "SHA1",
			HashValue: "cd3848697cb42f5be9902f6523ec516d21a8c677",
		},
		{ // An Artifact with SHA256 checksum.
			FileName: "test.txt", Size: 65536, Link: "https://localhost:43234/test.txt",
			HashType:  "SHA256",
			HashValue: "4eefb9a7a40a8b314b586a00f307157043c0bbe4f59fa39cba88773680758bc3",
		},
	}, certFile, certKey, t)
}

func testDownloadToFile(arts []*Artifact, certFile, certKey string, t *testing.T) {
	for _, art := range arts {
		t.Run(art.HashType, func(t *testing.T) {
			// Prepare
			dir := "_tmp-download"
			if err := os.MkdirAll(dir, 0755); err != nil {
				t.Fatalf("failed create temporary directory: %v", err)
			}

			// Remove temporary directory at the end
			defer os.RemoveAll(dir)

			// Start Web server
			srv := host(":43234", art.FileName, int64(art.Size), false, isSecure(art.Link, t), validCert, validKey, t)
			defer srv.close()
			name := filepath.Join(dir, art.FileName)

			// 1. Resume downlaod of corrupted temporary file.
			WriteLn(filepath.Join(dir, prefix+art.FileName), "wrong start")
			if err := downloadArtifact(name, art, nil, certFile, make(chan struct{})); err == nil {
				t.Fatal("downlaod of corrupted temporary file must fail")
			}

			// 2. Cancel in the middle of the download operation.
			done := make(chan struct{})
			callback := func(bytes int64) {
				close(done)
			}
			if err := downloadArtifact(name, art, callback, certFile, done); err != ErrCancel {
				t.Fatalf("failed to cancel download operation: %v", err)
			}
			if _, err := os.Stat(filepath.Join(dir, prefix+art.FileName)); os.IsNotExist(err) {
				t.Fatal("missing partial download artifact")
			}

			// 3. Resume previous download operation.
			callback = func(bytes int64) { /* Do nothing. */ }
			if err := downloadArtifact(name, art, callback, certFile, make(chan struct{})); err != nil {
				t.Fatalf("failed to download artifact: %v", err)
			}
			check(name, art.Size, t)

			// 4. Download available file.
			if err := downloadArtifact(name, art, callback, certFile, make(chan struct{})); err != nil {
				t.Fatalf("failed to download artifact: %v", err)
			}
			check(name, art.Size, t)

			// Remove downloaded file.
			if err := os.Remove(name); err != nil {
				t.Fatalf("failed to remove downloaded artifact: %v", err)
			}

			// 5. Try to resume with file bigger than expected.
			WriteLn(filepath.Join(dir, prefix+art.FileName), "1111111111111")
			art.Size -= 10
			if err := downloadArtifact(name, art, nil, certFile, make(chan struct{})); err == nil {
				t.Fatal("validate resume with file bigger than expected")
			}

			// 6. Try to resume from missing link.
			WriteLn(filepath.Join(dir, prefix+art.FileName), "1111111111111")
			art.Link = "http://localhost:43234/test-missing.txt"
			if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
				t.Fatal("failed to validate with missing link")
			}

		})
	}
}

// TestDownloadToFileError tests downloadToFile function for some edge cases.
func TestDownloadToFileError(t *testing.T) {
	// Prepare
	dir := "_tmp-download"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	art := &Artifact{
		FileName: "test-simple.txt", Size: 65536, Link: "http://localhost:43234/test-simple.txt",
		HashType:  "MD5",
		HashValue: "ab2ce340d36bbaafe17965a3a2c6ed5b",
	}

	// Start Web server
	srv := host(":43234", art.FileName, int64(art.Size), true, isSecure(art.Link, t), extCert, extKey, t)
	defer srv.close()
	name := filepath.Join(dir, art.FileName)

	// 1. Resume is not supported.
	WriteLn(filepath.Join(dir, prefix+art.FileName), "1111")
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err != nil {
		t.Fatalf("failed to download file artifact: %v", err)
	}
	check(name, art.Size, t)

	// 2. Try with missing checksum.
	art.HashValue = ""
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
		t.Fatal("validatted with missing checksum")
	}

	// 3. Try with missing link.
	art.Link = "http://localhost:43234/test-missing.txt"
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
		t.Fatal("failed to validate with missing link")
	}

	// 4. Try with wrong checksum type.
	art.Link = "http://localhost:43234/test-simple.txt"
	art.HashType = ""
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
		t.Fatal("validate with wrong checksum type")
	}

	// 5. Try with wrong checksum format.
	art.HashValue = ";;"
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
		t.Fatal("validate with wrong checksum format")
	}

	// 6. Try to download file bigger than expected.
	art.HashType = "MD5"
	art.HashValue = "ab2ce340d36bbaafe17965a3a2c6ed5b"
	art.Size -= 10
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
		t.Fatal("validate with file bigger than expected")
	}

}

// TestDownloadToFileSecureError tests HTTPS file download function for bad/expired TLS certificates.
func TestDownloadToFileSecureError(t *testing.T) {
	// Prepare
	dir := "_tmp-download"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	art := &Artifact{
		FileName: "test.txt", Size: 65536, Link: "https://localhost:43234/test.txt",
		HashType:  "MD5",
		HashValue: "ab2ce340d36bbaafe17965a3a2c6ed5b",
	}

	// Start Web servers secure
	srvSecureInv := host(":43234", art.FileName, int64(art.Size), true, true, expiredCert, expiredKey, t)
	defer srvSecureInv.close()
	srvSecureExt := host(":43235", art.FileName, int64(art.Size), true, true, extCert, extKey, t)
	defer srvSecureExt.close()
	srvSecureVal := host(":43236", art.FileName, int64(art.Size), true, true, validCert, validKey, t)
	defer srvSecureVal.close()
	name := filepath.Join(dir, art.FileName)

	// 1. Try download with expired certificate
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
		t.Fatalf("validated with expired certificate: %v", err)
	}
	if err := downloadArtifact(name, art, nil, expiredCert, make(chan struct{})); err == nil {
		t.Fatalf("validated with expired certificate: %v", err)
	}

	// 2. Try download with unauthorized certificate
	art.Link = "https://localhost:43235/test.txt"
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
		t.Fatalf("validated with unauthorized certificate: %v", err)
	}

	// 3. Try download with unknown certificate
	art.Link = "https://localhost:43236/test.txt"
	if err := downloadArtifact(name, art, nil, extCert, make(chan struct{})); err == nil {
		t.Fatalf("validated with certificate, different than configured one: %v", err)
	}
}

// check that file with this name exists and its size is the same.
func check(name string, expected int, t *testing.T) {
	if stat, err := os.Stat(name); os.IsNotExist(err) || stat.Size() != int64(expected) {
		t.Fatalf("corrupted download artifact: %v != %v", stat.Size(), expected)
	}
}

// host create and start a HTTP server.
func host(addr string, name string, size int64, simple bool, secure bool, cert string, key string, t *testing.T) *web {
	// Create new HTTP server.
	w := &web{name: name, size: size, srv: &http.Server{Addr: addr}, t: t}

	// Add HTTP alias only once.
	if simple {
		testOnceSimple.Do(func() { http.HandleFunc(fmt.Sprintf("/%s", name), w.handlerSimple) })
	} else {
		testOnceRange.Do(func() { http.HandleFunc(fmt.Sprintf("/%s", name), w.handlerRange) })
	}

	// Start HTTP server in separate goroute.
	if secure {
		go func() {
			if err := w.srv.ListenAndServeTLS(cert, key); err != nil && err != http.ErrServerClosed {
				t.Errorf("The secure server is not started: %v", err)
			}
		}()
	} else {
		go func() {
			if err := w.srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				t.Errorf("Failed to start web server: %v", err)
			}
		}()
	}

	time.Sleep(1 * time.Second)

	return w
}

// close the http server.
func (w *web) close() {
	if err := w.srv.Shutdown(context.Background()); err != nil {
		w.t.Errorf("failed shutdown web server: %v", err)
	}
}

// handlerRange handles incoming HTTP requests with range header support.
func (w *web) handlerRange(writer http.ResponseWriter, request *http.Request) {
	// Set default headers for txt file.
	writer.Header().Set("Content-Type", "text/plain")
	writer.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, w.name))
	writer.Header().Set("Accept-Ranges", "bytes")

	// Parse range header -> 'Range: bytes=<begin>-<end>'
	r := request.Header.Get("range")
	if r == "" { // No range is defined
		writer.Header().Set("Content-Length", strconv.Itoa(int(w.size)))
		write(writer, w.size)
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
	write(writer, end-begin+1)
}

// handlerSimple handles incoming HTTP requests without range header support.
func (w *web) handlerSimple(writer http.ResponseWriter, request *http.Request) {
	writer.Header().Set("Content-Type", "text/plain")
	writer.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, w.name))
	writer.Header().Set("Content-Length", strconv.Itoa(int(w.size)))
	write(writer, w.size)
}

// write file content.
func write(writer http.ResponseWriter, size int64) {
	b := []byte("11111111111111111111111111111111111111111111111111")
	s := int64(len(b))
	for i := int64(s); i < size; i += s {
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
