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

	"github.com/eclipse-kanto/software-update/internal/logger"
)

const prefix = "_temporary-"

var secureCiphers = supportedCipherSuites()

// downloadArtifact tries to resume previous download operation or perform a new download.
func downloadArtifact(to string, artifact *Artifact, progress progressBytes, serverCert string, done chan struct{}) error {
	logger.Infof("Download [%s] to file [%s]", artifact.Link, to)

	// Check for available file.
	if _, err := os.Stat(to); !os.IsNotExist(err) {
		logger.Debugf("File exists, check its checksum: %s", to)
		if err = validate(to, artifact.HashType, artifact.HashValue); err == nil {
			logger.Debugf("File already available: %s", to)
			if progress != nil {
				progress(int64(artifact.Size))
			}
			return nil
		}
		logger.Debugf("Available file with wrong checksum, remove it: %s", to)
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
				logger.Debugf("Failed to remove failed download file: %v", err)
			}
		}
	}()

	if stat, err := os.Stat(tmp); !os.IsNotExist(err) {
		// Try to resume previous download.
		if dError = resume(tmp, stat.Size(), artifact, progress, serverCert, done); dError != nil {
			return dError
		}
	} else {
		// No available previous download, perform a full download.
		response, err := requestDownload(artifact.Link, 0, serverCert)
		if err != nil {
			return err
		}
		defer response.Body.Close()

		// HTTP Status code is NOT in the 2xx range
		if response.StatusCode < http.StatusOK || response.StatusCode >= http.StatusMultipleChoices {
			return fmt.Errorf("http status code is not in the 2xx range: %v", response.StatusCode)
		}

		if dError = download(tmp, response.Body, artifact, progress, done); dError != nil {
			return dError
		}
	}

	// Rename to the original file name.
	return os.Rename(tmp, to)
}

func resume(to string, offset int64, artifact *Artifact, progress progressBytes, serverCert string, done chan struct{}) error {
	// Send the HTTP request and get its response.
	response, err := requestDownload(artifact.Link, offset, serverCert)
	if err != nil {
		return err
	}
	defer response.Body.Close()

	// HTTP Status code is NOT in the 2xx range
	if response.StatusCode < http.StatusOK || response.StatusCode >= http.StatusMultipleChoices {
		return fmt.Errorf("http status code is not in the 2xx range: %v", response.StatusCode)
	}

	// Check if HTTP server support Range header. If not, delete existing file and perform regular download
	if response.Header.Get("Accept-Ranges") != "bytes" || response.Header.Get("Content-Range") == "" {
		logger.Infof("Resume is not supported, remove previous file: %s", to)
		if err := os.Remove(to); err != nil {
			return err
		}
		return download(to, response.Body, artifact, progress, done)
	}

	// Download the rest of the file.
	logger.Debugf("Resume previous download of file: %s", to)
	file, err := os.OpenFile(to, os.O_WRONLY|os.O_CREATE|os.O_APPEND, 0755)
	if err != nil {
		return err
	}
	defer file.Close()

	if progress != nil {
		progress(offset)
	}
	if w, err := copy(file, response.Body, int64(artifact.Size)-offset, progress, done); err != nil {
		logger.Debugf("Written bytes: %v", w)
		return err
	}
	return validate(to, artifact.HashType, artifact.HashValue)
}

func requestDownload(link string, offset int64, serverCert string) (*http.Response, error) {
	// Create new HTTP request with Range header.
	request, err := http.NewRequest(http.MethodGet, link, nil)
	if err != nil {
		logger.Errorf("Error doing http(s) request to %s", link)
		return nil, err
	}
	if offset > 0 {
		request.Header.Set("Range", fmt.Sprintf("bytes=%v-", offset))
	}

	var transport http.Transport
	var caCertPool *x509.CertPool
	if len(serverCert) > 0 {
		caCert, err := ioutil.ReadFile(serverCert)
		if err != nil {
			logger.Errorf("Error reading CA certificate file - \"%s\"", serverCert)
			return nil, err
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

func download(to string, in io.ReadCloser, artifact *Artifact, progress progressBytes, done chan struct{}) error {
	file, err := os.OpenFile(to, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0755)
	if err != nil {
		return err
	}
	defer file.Close()

	if _, err = copy(file, in, int64(artifact.Size), progress, done); err != nil {
		return err
	}

	return validate(to, artifact.HashType, artifact.HashValue)
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
