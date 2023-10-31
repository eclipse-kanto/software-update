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
	"crypto/x509"
	"fmt"
	"os"
)

// NewTLSConfig initializes the TLS.
func NewTLSConfig(rootCert, cert, key string) (*tls.Config, error) {
	caCert, err := os.ReadFile(rootCert)
	if err != nil {
		return nil, fmt.Errorf("failed to load CA: %s", err)
	}
	caCertPool := x509.NewCertPool()
	if !caCertPool.AppendCertsFromPEM(caCert) {
		return nil, fmt.Errorf("failed to parse CA %s", rootCert)
	}

	tlsConfig := &tls.Config{
		InsecureSkipVerify: false,
		RootCAs:            caCertPool,
		MinVersion:         tls.VersionTLS12,
		MaxVersion:         tls.VersionTLS13,
		CipherSuites:       supportedCipherSuites(),
	}

	if len(cert) > 0 || len(key) > 0 {
		cert, err := tls.LoadX509KeyPair(cert, key)
		if err != nil {
			return nil, fmt.Errorf("failed to load X509 key pair: %s", err)
		}
		tlsConfig.Certificates = []tls.Certificate{cert}
	}

	return tlsConfig, nil
}

func supportedCipherSuites() []uint16 {
	cs := tls.CipherSuites()
	cid := make([]uint16, len(cs))
	for i := range cs {
		cid[i] = cs[i].ID
	}
	return cid
}
