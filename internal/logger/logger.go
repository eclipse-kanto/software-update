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

package logger

import (
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/eclipse-kanto/software-update/hawkbit"

	"github.com/eclipse/ditto-clients-golang"
	"gopkg.in/natefinch/lumberjack.v2"
)

// LogConfig represents a log configuration.
type LogConfig struct {
	LogFile       string
	LogLevel      string
	LogFileSize   int
	LogFileCount  int
	LogFileMaxAge int
}

// LogLevel represents a log level.
type LogLevel int

const (
	// ERROR represents the error log level.
	ERROR LogLevel = 1 + iota
	// WARN represents the warn log level.
	WARN
	// INFO represents the info log level.
	INFO
	// DEBUG represents the debug log level.
	DEBUG
	// TRACE represents the trace log level.
	TRACE

	logFlags int = log.Ldate | log.Ltime | log.Lmicroseconds | log.Lmsgprefix

	ePrefix = "ERROR  "
	wPrefix = "WARN   "
	iPrefix = "INFO   "
	dPrefix = "DEBUG  "
	tPrefix = "TRACE  "

	prefix        = " %-10s"
	suPrefix      = "[SU]"
	hawkbitPrefix = "[Hawkbit]"
	dittoPrefix   = "[Ditto]"
)

var (
	logger *log.Logger
	level  LogLevel
)

// SetupLogger initialized the log besed on the provided log configuration.
func SetupLogger(logConfig *LogConfig) io.WriteCloser {
	loggerOut := io.WriteCloser(&nopWriterCloser{out: os.Stderr})
	if len(logConfig.LogFile) > 0 {
		if err := os.MkdirAll(filepath.Dir(logConfig.LogFile), 0755); err == nil {
			loggerOut = &lumberjack.Logger{
				Filename:   logConfig.LogFile,
				MaxSize:    logConfig.LogFileSize,
				MaxBackups: logConfig.LogFileCount,
				MaxAge:     logConfig.LogFileMaxAge,
				LocalTime:  true,
				Compress:   true,
			}
		}
	}

	log.SetOutput(loggerOut)
	log.SetFlags(logFlags)

	logger = log.New(loggerOut, fmt.Sprintf(prefix, suPrefix), logFlags)

	// Parse log level
	switch strings.ToUpper(logConfig.LogLevel) {
	case "INFO":
		level = INFO
	case "WARN":
		level = WARN
	case "DEBUG":
		level = DEBUG
	case "TRACE":
		level = TRACE
	default:
		level = ERROR
	}
	lHawkbit := log.New(loggerOut, fmt.Sprintf(prefix, hawkbitPrefix), logFlags)
	hawkbit.ERROR = &wrapper{logger: lHawkbit, level: ERROR, prefix: ePrefix}
	hawkbit.WARN = &wrapper{logger: lHawkbit, level: WARN, prefix: wPrefix}
	hawkbit.INFO = &wrapper{logger: lHawkbit, level: INFO, prefix: iPrefix}
	hawkbit.DEBUG = &wrapper{logger: lHawkbit, level: DEBUG, prefix: dPrefix}

	lDitto := log.New(loggerOut, fmt.Sprintf(prefix, dittoPrefix), logFlags)
	ditto.ERROR = &wrapper{logger: lDitto, level: ERROR, prefix: ePrefix}
	ditto.WARN = &wrapper{logger: lDitto, level: WARN, prefix: wPrefix}
	ditto.INFO = &wrapper{logger: lDitto, level: INFO, prefix: iPrefix}
	ditto.DEBUG = &wrapper{logger: lDitto, level: DEBUG, prefix: dPrefix}

	return loggerOut
}

// Error writes an error entry to the log.
func Error(v interface{}) {
	if level >= ERROR {
		logger.Println(ePrefix, v)
	}
}

// Errorf formats according to a format specifier and write the string as an
// error entry to the log.
func Errorf(format string, v ...interface{}) {
	if level >= ERROR {
		logger.Println(fmt.Errorf(fmt.Sprint(ePrefix, " ", format), v...))
	}
}

// Warn writes a warning entry to the log.
func Warn(v interface{}) {
	if level >= WARN {
		logger.Println(wPrefix, v)
	}
}

// Warnf formats according to a format specifier and write the string as a
// warning entry to the log.
func Warnf(format string, v ...interface{}) {
	if level >= WARN {
		logger.Printf(fmt.Sprint(wPrefix, " ", format), v...)
	}
}

// Info writes an info entry to the log.
func Info(v interface{}) {
	if level >= INFO {
		logger.Println(iPrefix, v)
	}
}

// Infof formats according to a format specifier and write the string as an
// info entry to the log.
func Infof(format string, v ...interface{}) {
	if level >= INFO {
		logger.Printf(fmt.Sprint(iPrefix, " ", format), v...)
	}
}

// Debug writes an debug entry to the log.
func Debug(v interface{}) {
	if IsDebugEnabled() {
		logger.Println(dPrefix, v)
	}
}

// Debugf formats according to a format specifier and write the string as an
// debug entry to the log.
func Debugf(format string, v ...interface{}) {
	if IsDebugEnabled() {
		logger.Printf(fmt.Sprint(dPrefix, " ", format), v...)
	}
}

// Trace writes an trace entry to the log.
func Trace(v ...interface{}) {
	if IsTraceEnabled() {
		logger.Println(tPrefix, fmt.Sprint(v...))
	}
}

// Tracef formats according to a format specifier and write the string as an
// trace entry to the log.
func Tracef(format string, v ...interface{}) {
	if IsTraceEnabled() {
		logger.Printf(fmt.Sprint(tPrefix, " ", format), v...)
	}
}

// IsDebugEnabled checks if debug log level is enabled.
func IsDebugEnabled() bool {
	return level >= DEBUG
}

// IsTraceEnabled checks if trace log level is enabled.
func IsTraceEnabled() bool {
	return level >= TRACE
}

type wrapper struct {
	logger *log.Logger
	level  LogLevel
	prefix string
}

func (w *wrapper) Println(v ...interface{}) {
	if level >= w.level {
		w.logger.Println(w.prefix, fmt.Sprint(v...))
	}
}

func (w *wrapper) Printf(format string, v ...interface{}) {
	if level >= w.level {
		w.logger.Printf(fmt.Sprint(w.prefix, " ", format), v...)
	}
}

type nopWriterCloser struct {
	out io.Writer
}

func (w *nopWriterCloser) Write(p []byte) (n int, err error) {
	return w.out.Write(p)
}

func (*nopWriterCloser) Close() error {
	return nil
}
