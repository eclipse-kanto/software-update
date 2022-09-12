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
	"fmt"
	"os"
	"path/filepath"
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

func (f *ScriptBasedSoftwareUpdatable) resolveLocalArtifacts(link string) (string, error) {
	if f.accessMode != modeLax && len(f.installDirs) == 0 {
		return link, fmt.Errorf("no install directories specified, while using `strict` access mode")
	}
	fileCheck := isFile(link)
	var accessDenied error
	for _, path := range f.installDirs {
		if fileCheck {
			if accessDenied = f.checkAccess(link, path); accessDenied == nil {
				return link, nil
			}
		}
		location := filepath.Join(path, link)
		if isFile(location) {
			if accessDenied = f.checkAccess(location, path); accessDenied == nil {
				return location, nil
			}
		}
	}
	return link, accessDenied
}

func (f *ScriptBasedSoftwareUpdatable) checkAccess(location, allowedPath string) error {
	if f.accessMode == modeLax {
		return nil
	}

	var targetAbs, allowedDir, targetDir string
	var err error

	if targetAbs, err = filepath.Abs(location); err != nil { // just in case
		return fmt.Errorf("cannot determine absolute path of [%s]", location)
	}
	targetDir = filepath.Dir(targetAbs)

	if allowedDir, err = filepath.Abs(allowedPath); err != nil {
		return fmt.Errorf("invalid install path directory(cannot determine absolute path) - [%s]", allowedPath)
	}
	if !isDir(allowedDir) {
		return fmt.Errorf("invalid install path location(not a directory) - [%s]", allowedDir)
	}

	if (f.accessMode == modeStrict && targetDir == allowedDir) || (f.accessMode == modeScoped && isSubDir(targetDir, allowedDir)) {
		return nil
	}
	return fmt.Errorf("artifact [%s] found, but access to it was denied", location)
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
