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

package feature

import (
	"bufio"
	"bytes"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/eclipse-kanto/software-update/hawkbit"
	"github.com/eclipse-kanto/software-update/internal/logger"
	"github.com/fsnotify/fsnotify"
)

const nStatus = "status"

type monitor struct {
	cid    string
	module *hawkbit.SoftwareModuleID
	status hawkbit.Status
	su     *hawkbit.SoftwareUpdatable

	oldProgress   int
	oldMessage    string
	oldStatusCode string
}

func (o *monitor) update(name string) {
	p, m, c, done := load(name)
	if done && (p > o.oldProgress || m != o.oldMessage || c != o.oldStatusCode) {
		o.oldProgress = p
		o.oldMessage = m
		o.oldStatusCode = c
		ops := hawkbit.NewOperationStatusUpdate(o.cid, o.status, o.module).
			WithProgress(p).WithMessage(m).WithStatusCode(c)
		if err := o.su.SetLastOperation(ops); err != nil {
			logger.Errorf("fail to send last operation status: %v", err)
		}
	}
}

func (o *monitor) waitFor(dir string) (chan bool, error) {
	o.oldProgress = -1
	file := filepath.Join(dir, nStatus)
	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		return nil, err
	}

	done := make(chan bool)
	go func() {
		defer watcher.Close()
		for {
			select {
			case event := <-watcher.Events:
				if event.Name == file && (event.Op&fsnotify.Write == fsnotify.Write ||
					event.Op&fsnotify.Chmod == fsnotify.Chmod) {
					o.update(file)
				}
			case err := <-watcher.Errors:
				logger.Debugf("fail to watch working directory: %v", err)
			case <-done:
				return
			}
		}
	}()

	logger.Debugf("Watch working directory: %s", dir)
	if err := watcher.Add(dir); err != nil {
		return nil, err
	}
	return done, nil
}

func load(name string) (p int, m string, c string, done bool) {
	logger.Debugf("load status file: %s", name)
	file, err := os.Open(name)
	if err != nil {
		logger.Warningf("failed to load status file: %s", name)
		return p, m, c, false
	}
	defer file.Close()

	trailingNewline := false

	// Create new scanner which can check for trailing newline.
	scanner := bufio.NewScanner(file)
	scanner.Split(func(data []byte, atEOF bool) (advance int, token []byte, err error) {
		if atEOF && len(data) == 0 {
			logger.Debug("EOF reached")
			return 0, nil, nil
		}
		if atEOF {
			logger.Debugf("EOF reached, treaming whats left: %v", len(data))
			return len(data), trim(data), nil
		}
		if i := bytes.IndexByte(data, '\n'); i >= 0 {
			logger.Debugf("found data with LF, trim and continue...")
			trailingNewline = true
			return i + 1, trim(data[0:i]), nil
		}
		logger.Warning("mandatory trailing LF not found at the end of this file")
		trailingNewline = false
		return 0, nil, nil
	})

	var found bool
	// Search for "message", "statusCode" and "progress" keys.
	var ln string
	for scanner.Scan() {
		ln = scanner.Text()
		logger.Debugf("Scanning :: %s", ln)
		ss := strings.SplitN(ln, "=", 2)
		if len(ss) == 2 {
			switch ss[0] {
			case "message":
				found = true
				m = ss[1]
			case "statusCode":
				found = true
				c = ss[1]
			case "progress":
				found = true
				p, err = strconv.Atoi(strings.TrimSpace(ss[1]))
				if err != nil || p < 0 || p > 100 {
					logger.Debugf("progress value [%v] must be a number with range [0, 100]: %v", p, err)
					return p, m, c, false
				}
			default:
				// Unknown key, skip line.
				logger.Debugf("unknown key, skip line: %s", ln)
			}
		}
	}

	// Trailing newline is a protection mechanism against not fully written files.
	if !trailingNewline {
		logger.Warning("trailing new line not found")
		return p, m, c, false
	}

	if found {
		logger.Debugf("file is valid, found at least one parameter: %v, %v, %v", p, m, c)
		return p, m, c, true
	}

	return p, m, c, false
}

func trim(data []byte) []byte {
	ln := len(data)
	if ln > 0 && data[ln-1] == '\r' {
		return data[0 : ln-1]
	}
	return data
}
