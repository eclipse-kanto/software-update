// Copyright (c) 2022 Contributors to the Eclipse Foundation
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
	"os"
	"path/filepath"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

func isFile(link string) bool {
	if fs, err := os.Stat(link); !os.IsNotExist(err) && !fs.IsDir() {
		return true
	}
	return false
}

func isDir(dir string) bool {
	if fs, err := os.Stat(dir); !os.IsNotExist(err) && fs.IsDir() {
		return true
	}
	return false
}

func (f *ScriptBasedSoftwareUpdatable) locateArtifact(link string) string {
	fileCheck := isFile(link)
	paths := f.installPath
	if len(paths) == 0 { // include the current dir
		paths = []string{"."}
	}
	var accessDenied string
	for _, path := range paths {
		if fileCheck && f.isAccessPermitted(link, path, &accessDenied) {
			return link
		}
		location := filepath.Join(path, link)
		if isFile(location) && f.isAccessPermitted(location, path, &accessDenied) {
			return location
		}
	}
	if accessDenied != "" {
		logger.Errorf(accessDenied, link)
	}
	return ""
}

func (f *ScriptBasedSoftwareUpdatable) isAccessPermitted(location, allowedPath string, accessDenied *string) bool {
	if f.accessMode == modeLax {
		return true
	}

	var targetAbs, allowedDir, targetDir string
	var err error

	if targetAbs, err = filepath.Abs(location); err != nil { // just in case
		logger.Debugf("cannot determine absolute path of [%s]", location)
		return false
	}
	targetDir = filepath.Dir(targetAbs)

	if allowedDir, err = filepath.Abs(allowedPath); err != nil {
		logger.Debugf("invalid install path directory - [%s]", allowedPath)
		return false
	}
	if !isDir(allowedDir) {
		logger.Debugf("invalid install path directory - [%s]", allowedDir)
		return false
	}

	if (f.accessMode == modeStrict && targetDir == allowedDir) || (f.accessMode == modeScoped && isSubDir(targetDir, allowedDir)) {
		return true
	}
	*accessDenied = "artifact %s found, but access to it was denied"
	return false
}

// dir and sub must be absolute paths
func isSubDir(sub string, dir string) bool {
	last := ""
	for p := sub; p != last; p = filepath.Dir(last) {
		if dir == p {
			return true
		}

		last = p
	}

	return false
}
