// Copyright (c) 2023 Contributors to the Eclipse Foundation
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

package tls

import (
	"crypto/tls"
	"errors"
	"fmt"
	"path/filepath"
	"testing"
)

const (
	nonExisting  = "nonexisting.test"
	invalidFile  = "testdata/invalid.pem"
	caCertPath   = "testdata/ca.crt"
	certPath     = "testdata/certificate.pem"
	keyPath      = "testdata/key.pem"
	caError      = "failed to load CA: open %s: no such file or directory"
	keyPairError = "failed to load X509 key pair: open %s: no such file or directory"
	invalidError = "failed to load X509 key pair: tls: failed to find any PEM data in key input"
)

func TestNewTLSConfig(t *testing.T) {
	dirAbsPath, _ := filepath.Abs("./")

	tests := map[string]struct {
		CACert        string
		Cert          string
		Key           string
		ExpectedError error
	}{
		"valid_config_with_credentials": {CACert: caCertPath, Cert: certPath, Key: keyPath, ExpectedError: nil},
		"valid_config_no_credentials":   {CACert: caCertPath, Cert: "", Key: "", ExpectedError: nil},
		"no_files_provided":             {CACert: "", Cert: "", Key: "", ExpectedError: fmt.Errorf(caError, "")},
		"non_existing_ca_file":          {CACert: nonExisting, Cert: "", Key: "", ExpectedError: fmt.Errorf(caError, nonExisting)},
		"invalid_ca_file":               {CACert: invalidFile, Cert: certPath, Key: keyPath, ExpectedError: fmt.Errorf("failed to parse CA %s", invalidFile)},
		"invalid_ca_file_arg":           {CACert: "\\\000", Cert: certPath, Key: keyPath, ExpectedError: errors.New("failed to load CA: open \\\000: invalid argument")},
		"not_abs_cert_file_provided":    {CACert: caCertPath, Cert: nonExisting, Key: "", ExpectedError: fmt.Errorf(keyPairError, nonExisting)},
		"cert_is_directory":             {CACert: caCertPath, Cert: dirAbsPath, Key: "", ExpectedError: fmt.Errorf("failed to load X509 key pair: read %s: is a directory", dirAbsPath)},
		"no_key_file_provided":          {CACert: caCertPath, Cert: certPath, Key: "", ExpectedError: fmt.Errorf(keyPairError, "")},
		"not_abs_key_file_provided":     {CACert: caCertPath, Cert: certPath, Key: nonExisting, ExpectedError: fmt.Errorf(keyPairError, nonExisting)},
		"empty_key_file_provided":       {CACert: caCertPath, Cert: certPath, Key: "testdata/empty.crt", ExpectedError: errors.New(invalidError)},
		"cert_file_instead_key":         {CACert: caCertPath, Cert: certPath, Key: caCertPath, ExpectedError: fmt.Errorf("failed to load X509 key pair: tls: found a certificate rather than a key in the PEM for the private key")},
		"invalid_key_file_provided":     {CACert: caCertPath, Cert: certPath, Key: invalidFile, ExpectedError: errors.New(invalidError)},
		// "real_files": {CACert: "/home/antonia/tony/kanto/mosquitto_new_certs/ca.crt", Cert: "/home/antonia/tony/kanto/mosquitto_new_certs/client.crt", Key: "/home/antonia/tony/kanto/mosquitto_new_certs/client.key", ExpectedError: nil},
	}

	for testName, testCase := range tests {
		t.Run(testName, func(t *testing.T) {
			cfg, err := NewTLSConfig(testCase.CACert, testCase.Cert, testCase.Key)
			if testCase.ExpectedError != nil {
				if testCase.ExpectedError.Error() != err.Error() {
					t.Fatalf("expected error : %s, got: %s", testCase.ExpectedError, err)
				}
				if cfg != nil {
					t.Fatalf("expected nil, got: %v", cfg)
				}
			} else {
				if err != nil {
					t.Fatal(err)
				}
				if len(cfg.Certificates) == 0 && testCase.Cert != "" && testCase.Key != "" {
					t.Fatal("certificates length must not be 0")
				}
				if len(cfg.CipherSuites) == 0 {
					t.Fatal("cipher suites length must not be 0")
				}
				// assert that cipher suites identifiers are contained in tls.CipherSuites
				for _, csID := range cfg.CipherSuites {
					if !func() bool {
						for _, cs := range tls.CipherSuites() {
							if cs.ID == csID {
								return true
							}
						}
						return false
					}() {
						t.Fatalf("cipher suite %d is not implemented", csID)
					}
				}
				if cfg.InsecureSkipVerify {
					t.Fatal("skip verify is set to true")
				}
				if cfg.MinVersion != tls.VersionTLS12 {
					t.Fatalf("invalid min TLS version %d", cfg.MinVersion)
				}
				if cfg.MaxVersion != tls.VersionTLS13 {
					t.Fatalf("invalid max TLS version %d", cfg.MaxVersion)
				}
			}
		})
	}
}
