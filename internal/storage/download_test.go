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
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

const (
	validCert   = "testdata/valid_cert.pem"
	validKey    = "testdata/valid_key.pem"
	expiredCert = "testdata/expired_cert.pem"
	expiredKey  = "testdata/expired_key.pem"
	extCert     = "testdata/ext_cert.pem"
	extKey      = "testdata/ext_key.pem"
)

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
	// SystemCertPool does not work on Windows: https://github.com/golang/go/issues/16736
	// Fixed in 1.18
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
			srv := Host(":43234", art.FileName, int64(art.Size), false, isSecure(art.Link, t), validCert, validKey, t)
			defer srv.Close()
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
	srv := Host(":43234", art.FileName, int64(art.Size), true, isSecure(art.Link, t), extCert, extKey, t)
	defer srv.Close()
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
		t.Fatal("validated with missing checksum")
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
		FileName: "test.txt", Size: 65536,
		HashType:  "MD5",
		HashValue: "ab2ce340d36bbaafe17965a3a2c6ed5b",
	}

	// Start Web servers secure
	srvSecureInv := Host(":43234", art.FileName, int64(art.Size), true, true, expiredCert, expiredKey, t)
	defer srvSecureInv.Close()
	srvSecureExt := Host(":43235", art.FileName, int64(art.Size), true, true, extCert, extKey, t)
	defer srvSecureExt.Close()
	srvSecureVal := Host(":43236", art.FileName, int64(art.Size), true, true, validCert, validKey, t)
	defer srvSecureVal.Close()
	name := filepath.Join(dir, art.FileName)

	// 1. Try download with expired certificate
	art.Link = "https://localhost:43234/test.txt"
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
		t.Fatalf("validated with expired certificate: %v", err)
	}
	if err := downloadArtifact(name, art, nil, expiredCert, make(chan struct{})); err == nil {
		t.Fatalf("validated with expired certificate: %v", err)
	}

	// 2. Try download with untrusted certificate
	art.Link = "https://localhost:43235/test.txt"
	if err := downloadArtifact(name, art, nil, "", make(chan struct{})); err == nil {
		t.Fatalf("validated with untrusted certificate: %v", err)
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
