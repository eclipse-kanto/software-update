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

package hawkbit

type (
	// Logger interface allows plugging of a logger implementation that
	// fits best the needs of the application that is to use the Ditto library
	Logger interface {
		Println(v ...interface{})
		Printf(format string, v ...interface{})
	}

	// LoggerStub provides an empty default implementation
	LoggerStub struct{}
)

// Println writes log entry with spaces between operands and a newline at the end.
func (LoggerStub) Println(v ...interface{}) {
	// Default logger do not write any logs.
}

// Printf formats according to a format specifier and writes log entry.
func (LoggerStub) Printf(format string, v ...interface{}) {
	// Default logger do not write any logs.
}

// Levels of the library's output that can be configured during package initialization in init()
var (
	INFO  Logger = LoggerStub{}
	WARN  Logger = LoggerStub{}
	DEBUG Logger = LoggerStub{}
	ERROR Logger = LoggerStub{}
)
