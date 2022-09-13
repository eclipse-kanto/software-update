// Copyright (c) 2021 Contributors to the Eclipse Foundation
//
// See the NOTICE file(s) distributed with this work for additional
// information regarding copyright ownership.
//
// This program and the accompanying materials are made available under the
// terms of the Eclipse Public License 2.0 which is available at
// https://www.eclipse.org/legal/epl-2.0, or the Apache License, Version 2.0
// which is available at https://www.apache.org/licenses/LICENSE-2.0.
//
// SPDX-License-Identifier: EPL-2.0 OR Apache-2.0

package storage

import (
	"bytes"
	"crypto/md5"
	"crypto/sha1"
	"crypto/sha256"
	"crypto/tls"
	"crypto/x509"
	"encoding/hex"
	"fmt"
	"hash"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

const prefix = "_temporary-"

var secureCiphers = supportedCipherSuites()

// downloadArtifact tries to resume previous download operation or perform a new download.
func downloadArtifact(to string, artifact *Artifact, progress progressBytes, serverCert string, retryCount int, retryInterval time.Duration,
	done chan struct{}) error {
	logger.Infof("download [%s] to file [%s]", artifact.Link, to)

	// Check for available file.
	if _, err := os.Stat(to); !os.IsNotExist(err) {
		logger.Debugf("file exists, check its checksum: %s", to)
		if err = validate(to, artifact.HashType, artifact.HashValue); err == nil {
			logger.Debugf("file already available: %s", to)
			if progress != nil {
				progress(int64(artifact.Size))
			}
			return nil
		}
		logger.Debugf("available file with wrong checksum, remove it: %s", to)
		if err := os.Remove(to); err != nil {
			return err
		}
	}

	// Download to temporary file.
	tmp := filepath.Join(filepath.Dir(to), prefix+filepath.Base(to))

	// Do not leave failed download files.
	var dError error
	defer func() {
		// Do not remove temporary file on cancel operation.
		if dError == ErrCancel {
			return
		}
		// Try to remove failed download file.
		if _, err := os.Stat(tmp); !os.IsNotExist(err) {
			if err = os.Remove(tmp); err != nil {
				logger.Debugf("failed to remove failed download file: %v", err)
			}
		}
	}()

	if stat, err := os.Stat(tmp); !os.IsNotExist(err) {
		// Try to resume previous download.
		if _, dError = resume(tmp, stat.Size(), artifact, progress, serverCert, retryCount, retryInterval, done); dError != nil {
			return dError
		}
	} else {
		// No available previous download, perform a full download.
		source, remainingRetries, _, err := openResource(artifact, 0, serverCert, retryCount, retryInterval)
		if err != nil {
			return err
		}
		defer source.Close()

		if _, dError = download(tmp, source, artifact, progress, serverCert, remainingRetries, retryInterval, done); dError != nil {
			return dError
		}
	}

	// Rename to the original file name.
	return os.Rename(tmp, to)
}

func resume(to string, offset int64, artifact *Artifact, progress progressBytes, serverCert string, retryCount int,
	retryInterval time.Duration, done chan struct{}) (int64, error) {
	// Send the HTTP request and get its response.
	source, remainingRetries, resumeSupported, err := openResource(artifact, offset, serverCert, retryCount, retryInterval)
	if err != nil {
		return 0, err
	}
	defer source.Close()

	// Check if HTTP server support Range header. If not, delete existing file and perform regular download
	if !resumeSupported {
		logger.Infof("resume is not supported, remove previous file: %s", to)
		if err := os.Remove(to); err != nil {
			logger.Errorf("error removing partially downloaded file %s", to)
			return 0, err
		}
		return download(to, source, artifact, progress, serverCert, remainingRetries, retryInterval, done)
	}

	// Download the rest of the file.
	logger.Debugf("resume previous download of file: %s", to)
	file, err := os.OpenFile(to, os.O_WRONLY|os.O_CREATE|os.O_APPEND, 0755)
	if err != nil {
		return 0, err
	}
	defer file.Close()

	if progress != nil {
		progress(offset)
	}
	return downloadFile(file, source, to, offset, artifact, progress, serverCert, remainingRetries, retryInterval, done)
}

func downloadFile(file *os.File, input io.ReadCloser, to string, offset int64, artifact *Artifact,
	progress progressBytes, serverCert string, retryCount int, retryInterval time.Duration, done chan struct{}) (int64, error) {
	w, err := copy(file, input, int64(artifact.Size)-offset, progress, done)
	if err == nil {
		err = validate(to, artifact.HashType, artifact.HashValue)
		offset = 0 // in case of error, re-download the file
		w = 0
	} else {
		logger.Debugf("written bytes: %v", w)
		offset += w
	}
	if err == nil {
		return w, nil
	}
	retryCount--
	for retryCount >= 0 {
		var deltaBytes int64
		logger.Errorf("error copying artifact %s, remaining attempts - %d, cause: %v", file.Name(), retryCount, err)
		logger.Infof("%v timeout until next attempt", retryInterval)
		file.Close()
		time.Sleep(time.Duration(retryInterval))
		logger.Infof("retrying to download artifact %s, current bytes written - %d", file.Name(), offset)
		deltaBytes, err = resume(to, offset, artifact, progress, serverCert, 0, 0, done)
		if err == nil {
			break
		}
		offset += deltaBytes
		retryCount--
	}
	return w, err
}

func openResource(artifact *Artifact, offset int64, serverCert string, retryCount int, retryInterval time.Duration) (io.ReadCloser, int, bool, error) {
	var err error
	var source io.ReadCloser
	var resumeSupported bool
	for retryCount >= 0 {
		source, resumeSupported, err = getInput(artifact, offset, serverCert)
		if err == nil {
			return source, retryCount, resumeSupported, nil
		}
		retryCount--
		if retryCount > 0 {
			logger.Errorf("error downloading artifact %s, remaining attempts - %d, cause: %v", artifact.Link, retryCount, err)
			logger.Infof("%v timeout until next attempt", retryInterval)
			if retryInterval > 0 {
				time.Sleep(retryInterval)
			}
		}
	}
	return nil, 0, false, err
}

func getInput(artifact *Artifact, offset int64, serverCert string) (io.ReadCloser, bool, error) {
	if artifact.Local { // a file
		return getFileInput(artifact.Link, offset)
	}

	response, err := requestDownload(artifact.Link, offset, serverCert) // not a file
	if err != nil {
		return nil, false, err
	}

	// HTTP Status code is NOT in the 2xx range
	if response.StatusCode < http.StatusOK || response.StatusCode >= http.StatusMultipleChoices {
		return nil, false, fmt.Errorf("http status code is not in the 2xx range: %v", response.StatusCode)
	}
	logger.Debugf("download response for artifact %s - %v", artifact.Link, response)
	return response.Body, supportsResume(response), nil
}

func getFileInput(location string, offset int64) (io.ReadCloser, bool, error) {
	file, err := os.Open(location)
	if err != nil {
		return nil, false, fmt.Errorf("error opening file - %s: %v", location, err)
	}
	logger.Infof("opened local file artifact - %s", file)
	if offset > 0 {
		_, err = file.Seek(offset, 0)
	}
	return file, err == nil, nil // if err != nil, resume is not supported
}

func requestDownload(link string, offset int64, serverCert string) (*http.Response, error) {
	// Create new HTTP request with Range header.
	request, err := http.NewRequest(http.MethodGet, link, nil)
	if err != nil {
		return nil, fmt.Errorf("error doing http(s) request to %s", link)
	}
	if offset > 0 {
		request.Header.Set("Range", fmt.Sprintf("bytes=%v-", offset))
	}

	var transport http.Transport
	var caCertPool *x509.CertPool
	if len(serverCert) > 0 {
		caCert, err := ioutil.ReadFile(serverCert)
		if err != nil {
			return nil, fmt.Errorf("error reading CA certificate file - \"%s\"", serverCert)
		}
		caCertPool = x509.NewCertPool()
		caCertPool.AppendCertsFromPEM(caCert)
	}

	u, _ := url.Parse(link) // MUST not return error, since http(s) request was done to that url
	if u.Scheme == "https" {
		config := &tls.Config{
			InsecureSkipVerify: false,
			RootCAs:            caCertPool,
			CipherSuites:       secureCiphers,
			MinVersion:         tls.VersionTLS12,
			MaxVersion:         tls.VersionTLS13,
		}
		transport = http.Transport{
			TLSClientConfig: config,
		}
	}

	// Send the HTTP request and get its response.
	client := &http.Client{Transport: &transport}
	return client.Do(request)
}

func download(to string, in io.ReadCloser, artifact *Artifact, progress progressBytes,
	serverCert string, retryCount int, retryInterval time.Duration, done chan struct{}) (int64, error) {
	file, err := os.OpenFile(to, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0755)
	if err != nil {
		return 0, err
	}
	defer file.Close()

	return downloadFile(file, in, to, 0, artifact, progress, serverCert, retryCount, retryInterval, done)
}

func copy(dst io.Writer, src io.Reader, size int64, progress progressBytes, done chan struct{}) (w int64, err error) {
	buf := make([]byte, 32*1024)
	for {
		select {
		case <-done:
			return w, ErrCancel
		default:
			nr, er := src.Read(buf)
			if nr > 0 {
				nw, ew := dst.Write(buf[0:nr])
				if ew != nil {
					return w, ew
				}
				w += int64(nw)
				if size > 0 && w > size {
					return w, ErrFileSizeExceeded
				}
				if progress != nil {
					progress(int64(nw))
				}
			}
			if er != nil {
				if er != io.EOF {
					err = er
				}
				return w, err
			}
		}
	}
}

func validate(fName string, hashType string, hashExpected string) error {
	logger.Infof("Validate [%s] with %s", fName, hashType)

	// Convert hex string representation of the hash to byte array.
	hashBytes := bytes.TrimSpace([]byte(hashExpected))
	expected := make([]byte, len(hashBytes)/2)
	if _, err := hex.Decode(expected, hashBytes); err != nil {
		return err
	}

	// Calculate file hash.
	actual, err := checksum(fName, hashType)
	if err != nil {
		return err
	}

	// Compare calculated hash with the expected hash.
	if bytes.Equal(actual, expected) {
		return nil
	}
	return fmt.Errorf("checksum does not match: %s != %s", hex.EncodeToString(actual), hashExpected)
}

func checksum(fName string, hashType string) ([]byte, error) {
	// Get hash algorithm instance.
	var hType hash.Hash
	switch strings.ToUpper(hashType) {
	case "SHA256":
		hType = sha256.New()
	case "SHA1":
		hType = sha1.New()
	case "MD5":
		hType = md5.New()
	default:
		return nil, fmt.Errorf("unknown hash type: %s", hashType)
	}

	// Open the file to calculate its hash.
	file, err := os.Open(fName)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	// Calculated file hash.
	if _, err := io.Copy(hType, file); err != nil {
		return nil, err
	}
	return hType.Sum(nil), nil
}

func supportedCipherSuites() []uint16 {
	cs := tls.CipherSuites()
	cid := make([]uint16, len(cs))
	for i := range cs {
		cid[i] = cs[i].ID
	}
	return cid
}

func supportsResume(response *http.Response) bool {
	return !(response.Header.Get("Accept-Ranges") != "bytes" || response.Header.Get("Content-Range") == "")
}
