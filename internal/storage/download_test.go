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
	"crypto/x509"
	"encoding/pem"
	"io/ioutil"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"testing"
)

const (
	validCert      = "testdata/valid_cert.pem"
	validKey       = "testdata/valid_key.pem"
	expiredCert    = "testdata/expired_cert.pem"
	expiredKey     = "testdata/expired_key.pem"
	untrustedCert  = "testdata/untrusted_cert.pem"
	untrustedKey   = "testdata/untrusted_key.pem"
	sslCertFileEnv = "SSL_CERT_FILE"
)

var sslCertFile string

func isCertAddedToSystemPool(t *testing.T, certFile string) bool {
	t.Helper()

	certs, err := x509.SystemCertPool()
	if err != nil {
		t.Logf("error getting system certificate pool - %v", err)
		return false
	}
	data, err := ioutil.ReadFile(certFile)
	if err != nil {
		t.Logf("error reading certificate file %s - %v", certFile, err)
		return false
	}
	block, _ := pem.Decode(data) // ignore rest bytes, there is only one test certificate
	cert, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		t.Logf("error parsing certificate %s - %v", certFile, err)
		return false
	}
	subjects := certs.Subjects()
	for i := 0; i < len(subjects); i++ {
		if reflect.DeepEqual(subjects[i], cert.RawSubject) {
			return true
		}
	}
	return false
}

func setSSLCerts(t *testing.T) {
	t.Helper()

	// SystemCertPool does not work on Windows: https://github.com/golang/go/issues/16736
	// Fixed in 1.18
	if runtime.GOOS == "windows" || runtime.GOOS == "darwin" {
		t.Skip("this test does not run on windows and macOS")
	}
	sslCertFile = os.Getenv(sslCertFileEnv)
	err := os.Setenv(sslCertFileEnv, validCert)
	if err != nil {
		t.Skipf("cannot set %s environment variable", sslCertFileEnv)
	}
	if !isCertAddedToSystemPool(t, validCert) {
		t.Skipf("cannot setup test case by adding certificate %s to system certificate pool", validCert)
	}
}

func unsetSSLCerts(t *testing.T) {
	t.Helper()

	if len(sslCertFile) > 0 {
		if err := os.Setenv(sslCertFileEnv, sslCertFile); err != nil {
			t.Logf("cannot restore %s environment variable initial value - %s", sslCertFileEnv, sslCertFile)
		}
	} else {
		if err := os.Unsetenv(sslCertFileEnv); err != nil {
			t.Logf("cannot unset %s environment variable", sslCertFileEnv)
		}
	}
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
	setSSLCerts(t)
	defer unsetSSLCerts(t)
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

			// Start http(s) server
			srv := Host(":43234", art.FileName, int64(art.Size), false, isSecure(art.Link, t), validCert, validKey, t)
			defer srv.Close()
			name := filepath.Join(dir, art.FileName)

			// 1. Resume download of corrupted temporary file.
			WriteLn(filepath.Join(dir, prefix+art.FileName), "wrong start")
			if err := downloadArtifact(name, art, nil, certFile, 0, 0, make(chan struct{})); err == nil {
				t.Fatal("downlaod of corrupted temporary file must fail")
			}

			// 2. Cancel in the middle of the download operation.
			done := make(chan struct{})
			callback := func(bytes int64) {
				close(done)
			}
			if err := downloadArtifact(name, art, callback, certFile, 0, 0, done); err != ErrCancel {
				t.Fatalf("failed to cancel download operation: %v", err)
			}
			if _, err := os.Stat(filepath.Join(dir, prefix+art.FileName)); os.IsNotExist(err) {
				t.Fatal("missing partial download artifact")
			}

			// 3. Resume previous download operation.
			callback = func(bytes int64) { /* Do nothing. */ }
			if err := downloadArtifact(name, art, callback, certFile, 0, 0, make(chan struct{})); err != nil {
				t.Fatalf("failed to download artifact: %v", err)
			}
			check(name, art.Size, t)

			// 4. Download available file.
			if err := downloadArtifact(name, art, callback, certFile, 0, 0, make(chan struct{})); err != nil {
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
			if err := downloadArtifact(name, art, nil, certFile, 0, 0, make(chan struct{})); err == nil {
				t.Fatal("validate resume with file bigger than expected")
			}

			// 6. Try to resume from missing link.
			WriteLn(filepath.Join(dir, prefix+art.FileName), "1111111111111")
			art.Link = "http://localhost:43234/test-missing.txt"
			if err := downloadArtifact(name, art, nil, "", 0, 0, make(chan struct{})); err == nil {
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

	// Start http(s) server
	srv := Host(":43234", art.FileName, int64(art.Size), true, isSecure(art.Link, t), untrustedCert, untrustedKey, t)
	defer srv.Close()
	name := filepath.Join(dir, art.FileName)

	// 1. Resume is not supported.
	WriteLn(filepath.Join(dir, prefix+art.FileName), "1111")
	if err := downloadArtifact(name, art, nil, "", 0, 0, make(chan struct{})); err != nil {
		t.Fatalf("failed to download file artifact: %v", err)
	}
	check(name, art.Size, t)

	// 2. Try with missing checksum.
	art.HashValue = ""
	if err := downloadArtifact(name, art, nil, "", 0, 0, make(chan struct{})); err == nil {
		t.Fatal("validated with missing checksum")
	}

	// 3. Try with missing link.
	art.Link = "http://localhost:43234/test-missing.txt"
	if err := downloadArtifact(name, art, nil, "", 0, 0, make(chan struct{})); err == nil {
		t.Fatal("failed to validate with missing link")
	}

	// 4. Try with wrong checksum type.
	art.Link = "http://localhost:43234/test-simple.txt"
	art.HashType = ""
	if err := downloadArtifact(name, art, nil, "", 0, 0, make(chan struct{})); err == nil {
		t.Fatal("validate with wrong checksum type")
	}

	// 5. Try with wrong checksum format.
	art.HashValue = ";;"
	if err := downloadArtifact(name, art, nil, "", 0, 0, make(chan struct{})); err == nil {
		t.Fatal("validate with wrong checksum format")
	}

	// 6. Try to download file bigger than expected.
	art.HashType = "MD5"
	art.HashValue = "ab2ce340d36bbaafe17965a3a2c6ed5b"
	art.Size -= 10
	if err := downloadArtifact(name, art, nil, "", 0, 0, make(chan struct{})); err == nil {
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
		FileName: "test.txt", Size: 65536,
		HashType:  "MD5",
		HashValue: "ab2ce340d36bbaafe17965a3a2c6ed5b",
	}

	// Start https servers
	srvSecureInvalid := Host(":43234", art.FileName, int64(art.Size), true, true, expiredCert, expiredKey, t)
	defer srvSecureInvalid.Close()
	srvSecureUntrusted := Host(":43235", art.FileName, int64(art.Size), true, true, untrustedCert, untrustedKey, t)
	defer srvSecureUntrusted.Close()
	srvSecureValid := Host(":43236", art.FileName, int64(art.Size), true, true, validCert, validKey, t)
	defer srvSecureValid.Close()
	name := filepath.Join(dir, art.FileName)

	// 1. Server uses expired certificate
	art.Link = "https://localhost:43234/test.txt"
	if err := downloadArtifact(name, art, nil, "", 0, 0, make(chan struct{})); err == nil {
		t.Fatalf("download must fail(client uses no certificate, server uses expired): %v", err)
	}
	if err := downloadArtifact(name, art, nil, expiredCert, 0, 0, make(chan struct{})); err == nil {
		t.Fatalf("download must fail(client and server use expired certificate): %v", err)
	}

	// 2. Server uses untrusted certificate
	art.Link = "https://localhost:43235/test.txt"
	if err := downloadArtifact(name, art, nil, "", 0, 0, make(chan struct{})); err == nil {
		t.Fatalf("download must fail(client uses no certificate, server uses untrusted): %v", err)
	}

	// 3. Server uses valid certificate
	art.Link = "https://localhost:43236/test.txt"
	if err := downloadArtifact(name, art, nil, untrustedCert, 0, 0, make(chan struct{})); err == nil {
		t.Fatalf("download must fail(client uses untrusted certificate, server uses valid): %v", err)
	}
}

// check that file with this name exists and its size is the same.
func check(name string, expected int, t *testing.T) {
	if stat, err := os.Stat(name); os.IsNotExist(err) || stat.Size() != int64(expected) {
		t.Fatalf("corrupted download artifact: %v != %v", stat.Size(), expected)
	}
}
