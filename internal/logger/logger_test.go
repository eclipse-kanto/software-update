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

package logger

import (
	"bufio"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/eclipse-kanto/software-update/hawkbit"
)

// TestLogLevelError tests logger functions with log level set to ERROR.
func TestLogLevelError(t *testing.T) {
	validate("ERROR", true, false, false, false, false, t)
}

// TestLogLevelWarn tests logger functions with log level set to WARN.
func TestLogLevelWarn(t *testing.T) {
	validate("WARN", true, true, false, false, false, t)
}

// TestLogLevelInfo tests logger functions with log level set to INFO.
func TestLogLevelInfo(t *testing.T) {
	validate("INFO", true, true, true, false, false, t)
}

// TestLogLevelDebug tests logger functions with log level set to DEBUG.
func TestLogLevelDebug(t *testing.T) {
	validate("DEBUG", true, true, true, true, false, t)
}

// TestLogLevelTrace tests logger functions with log level set to TRACE.
func TestLogLevelTrace(t *testing.T) {
	validate("TRACE", true, true, true, true, true, t)
}

// TestNopWriter tests logger functions without writter.
func TestNopWriter(t *testing.T) {
	// Prepare
	dir := "_tmp-logger"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}
	defer os.RemoveAll(dir)

	// Prepare the logger without writter
	loggerOut := SetupLogger(&LogConfig{LogFile: "", LogLevel: "TRACE", LogFileSize: 2, LogFileCount: 5})
	defer loggerOut.Close()

	// Validate that temporary is empty
	Error("test error")
	f, err := os.Open(dir)
	if err != nil {
		t.Fatalf("cannot open temporary directory: %v", err)
	}
	defer f.Close()

	if _, err = f.Readdirnames(1); err != io.EOF {
		t.Errorf("temporary directory is not empty")
	}
}

func validate(lvl string, hasError bool, hasWarn bool, hasInfo bool, hasDebug bool, hasTrace bool, t *testing.T) {
	// Prepare
	dir := "_tmp-logger"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}
	defer os.RemoveAll(dir)

	// Prepare the logger
	log := filepath.Join(dir, lvl+".log")
	loggerOut := SetupLogger(&LogConfig{LogFile: log, LogLevel: lvl, LogFileSize: 2, LogFileCount: 5})
	defer loggerOut.Close()

	// 1. Validate for error logs.
	validateError(log, hasError, t)
	validateHawkbitWrapper(log, hawkbit.ERROR, ePrefix, hasError, t)

	// 2. Validate for warn logs.
	validateWarn(log, hasWarn, t)
	validateHawkbitWrapper(log, hawkbit.WARN, wPrefix, hasWarn, t)

	// 3. Validate for info logs.
	validateInfo(log, hasInfo, t)
	validateHawkbitWrapper(log, hawkbit.INFO, iPrefix, hasInfo, t)

	// 4. Validate for debug logs.
	validateDebug(log, hasDebug, t)
	validateHawkbitWrapper(log, hawkbit.DEBUG, dPrefix, hasDebug, t)

	// 5. Validate for trace logs.
	validateTrace(log, hasTrace, t)
}

// validateHawkbitWrapper validates with the Hawkbit logger.
func validateHawkbitWrapper(log string, logger hawkbit.Logger, prefix string, has bool, t *testing.T) {
	prefix = strings.ToLower(prefix)
	// 1. Validate for Println function.
	logger.Println(prefix + " hawkbit")
	if has != search(log, t, hawkbitPrefix, prefix, prefix+" hawkbit") {
		t.Errorf("[%s][Prefix: %s] Println entry mismatch [result: %v]", log, prefix, !has)
	}
	// 2. Validate for Printf function.
	logger.Printf("%s hawkbit [%v,%s]", prefix, "param1", "param2")
	if has != search(log, t, hawkbitPrefix, prefix, prefix+" hawkbit [param1,param2]") {
		t.Errorf("[%s][Prefix: %s] Printf entry mismatch: [result: %v]", log, prefix, !has)
	}
}

// validateError validates for error logs.
func validateError(log string, has bool, t *testing.T) {
	// 1. Validate for Error function.
	Error("error log")
	if has != search(log, t, suPrefix, ePrefix, "error log") {
		t.Errorf("error entry mismatch [result: %v]", !has)
	}
	// 2. Validate for Errorf function.
	Errorf("error log [%v,%s]", "param1", "param2")
	if has != search(log, t, suPrefix, ePrefix, "error log [param1,param2]") {
		t.Errorf("errorf entry mismatch: [result: %v]", !has)
	}
}

// validateError validates for warn logs.
func validateWarn(log string, has bool, t *testing.T) {
	// 1. Validate for Warn function.
	Warn("warn log")
	if has != search(log, t, suPrefix, wPrefix, "warn log") {
		t.Errorf("warn entry mismatch [result: %v]", !has)
	}
	// 2. Validate for Warnf function.
	Warnf("warn log [%v,%s]", "param1", "param2")
	if has != search(log, t, suPrefix, wPrefix, "warn log [param1,param2]") {
		t.Errorf("warnf entry mismatch: [result: %v]", !has)
	}
}

// validateError validates for info logs.
func validateInfo(log string, has bool, t *testing.T) {
	// 1. Validate for Info function.
	Info("info log")
	if has != search(log, t, suPrefix, iPrefix, "info log") {
		t.Errorf("info entry mismatch [result: %v]", !has)
	}
	// 2. Validate for Infof function.
	Infof("info log [%v,%s]", "param1", "param2")
	if has != search(log, t, suPrefix, iPrefix, "info log [param1,param2]") {
		t.Errorf("infof entry mismatch: [result: %v]", !has)
	}
}

// validateError validates for debug logs.
func validateDebug(log string, has bool, t *testing.T) {
	// 1. Validate for Debug function.
	Debug("debug log")
	if has != search(log, t, suPrefix, dPrefix, "debug log") {
		t.Errorf("debug entry mismatch [result: %v]", !has)
	}
	// 2. Validate for Debugf function.
	Debugf("debug log [%v,%s]", "param1", "param2")
	if has != search(log, t, suPrefix, dPrefix, "debug log [param1,param2]") {
		t.Errorf("debugf entry mismatch: [result: %v]", !has)
	}
}

// validateError validates for trace logs.
func validateTrace(log string, has bool, t *testing.T) {
	// 1. Validate for Trace function.
	Trace("trace log")
	if has != search(log, t, suPrefix, tPrefix, "trace log") {
		t.Errorf("trace entry mismatch [result: %v]", !has)
	}
	// 2. Validate for Tracef function.
	Tracef("trace log [%v,%s]", "param1", "param2")
	if has != search(log, t, suPrefix, tPrefix, "trace log [param1,param2]") {
		t.Errorf("tracef entry mismatch: [result: %v]", !has)
	}
}

// search strings in log file.
func search(fn string, t *testing.T, entries ...string) bool {
	file, err := os.Open(fn)
	if err != nil {
		t.Fatalf("fail to open log file: %v", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		if has(scanner.Text(), entries...) {
			return true
		}
	}

	if err := scanner.Err(); err != nil {
		t.Fatalf("fail to read log file: %v", err)
	}
	return false
}

// has checks if string has substrings
func has(s string, substrs ...string) bool {
	for _, substr := range substrs {
		if !strings.Contains(s, substr) {
			return false
		}
	}
	return true
}
