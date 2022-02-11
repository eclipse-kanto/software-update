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
	"bufio"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/eclipse-kanto/software-update/hawkbit"
	"github.com/eclipse-kanto/software-update/internal/logger"
)

// ReadLn reads reads first line from a file.
func ReadLn(fn string) (string, error) {
	file, err := os.Open(fn)
	if err != nil {
		logger.Debugf("fail to open file: %v", err)
		return "", err
	}
	defer file.Close()

	reader := bufio.NewReader(file)
	ln, _, err := reader.ReadLine()
	if err != nil {
		logger.Debugf("fail to read line from file: %v", err)
		return "", err
	}
	return strings.TrimSpace(string(ln)), nil
}

// WriteLn writes single line to a file.
func WriteLn(fn string, status string) error {
	file, err := os.OpenFile(fn, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	if err != nil {
		logger.Debugf("fail to open file: %v", err)
		return err
	}
	defer file.Close()

	writer := bufio.NewWriter(file)
	if _, err := writer.WriteString(status); err != nil {
		logger.Debugf("fail to write to file: %v", err)
		return err
	}
	return writer.Flush()
}

// SaveSoftwareUpdatable as JSON file to file system.
func SaveSoftwareUpdatable(operation string, cid string, to string,
	modules []*hawkbit.SoftwareModuleAction) (*Updatable, error) {
	logger.Debugf("Save software updatable [%s] to: %s", operation, to)
	logger.Tracef("Modules: %v", modules)
	action := &Updatable{
		Operation:     operation,
		CorrelationID: cid,
		Modules:       make([]*Module, len(modules)),
	}
	for i, module := range modules {
		tmp, err := toModule(*module)
		if err != nil {
			return nil, err
		}
		action.Modules[i] = tmp
	}

	file, err := json.Marshal(action)
	if err != nil {
		return nil, err
	}

	logger.Debugf("Create needed directories: %s", to)
	err = os.MkdirAll(filepath.Dir(to), 0755)
	if err != nil {
		return nil, err
	}
	logger.Tracef("Save software updatable file [%s] : %s", to, file)
	return action, ioutil.WriteFile(to, file, 0644)
}

// FindAvailableLocation search for available directory in provided directory.
func FindAvailableLocation(parent string) (string, error) {
	logger.Debugf("Find available location in: %s", parent)
	file, err := os.Open(parent)
	if err != nil {
		return "", err
	}
	defer file.Close()

	names, err := file.Readdirnames(0)
	if err != nil {
		return "", err
	}

	id := 0
	for _, name := range names {
		if i, err := strconv.Atoi(name); err == nil && id <= i {
			id = i + 1
		}
	}
	return filepath.Join(parent, strconv.Itoa(id)), nil
}

func loadInstalledDep(dep string) (*hawkbit.DependencyDescription, error) {
	logger.Debugf("Load installed dependency: %s", dep)
	file, err := os.Open(dep)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)
	info := &hawkbit.DependencyDescription{}

	for scanner.Scan() {
		ss := strings.SplitN(scanner.Text(), "=", 2)
		if len(ss) == 2 {
			switch ss[0] {
			case "group":
				info.Group = ss[1]
			case "name":
				info.Name = ss[1]
			case "version":
				info.Version = ss[1]
			case "type":
				info.Type = ss[1]
			default: // Skip unknown lines.
			}
		}
	}
	if info.Group == "" || info.Name == "" || info.Version == "" {
		return nil, fmt.Errorf("missing mandaroty field in installed dependency file: %s", dep)
	}
	logger.Infof("Installed Dependency [%s] -> %s", dep, info)
	return info, nil
}

func loadSoftwareUpdatable(from string) (*Updatable, error) {
	logger.Debugf("Load software updatable from: %s", from)
	jsonFile, err := os.Open(from)
	if err != nil {
		return nil, err
	}
	defer jsonFile.Close()

	buf, err := ioutil.ReadAll(jsonFile)
	if err != nil {
		return nil, err
	}
	action := &Updatable{}
	err = json.Unmarshal([]byte(buf), action)
	if err != nil {
		return nil, err
	}
	logger.Tracef("Software updatable [%s] loaded: %v", from, action)
	return action, nil
}

func toModule(sma hawkbit.SoftwareModuleAction) (*Module, error) {
	module := &Module{
		Name:      sma.SoftwareModule.Name,
		Version:   sma.SoftwareModule.Version,
		Artifacts: make([]*Artifact, len(sma.Artifacts)),
		Metadata:  sma.Metadata,
	}
	for i, artifact := range sma.Artifacts {
		tmp, err := toArtifact(artifact)
		if err != nil {
			return nil, err
		}
		module.Artifacts[i] = tmp
	}
	logger.Tracef("Convert module [%v] to [%v]", sma, module)
	return module, nil
}

func toArtifact(sa *hawkbit.SoftwareArtifactAction) (*Artifact, error) {
	artifact := &Artifact{
		FileName: sa.Filename,
		Size:     sa.Size,
	}

	// Set artifact link with following priority: HTTPS, HTTP
	if sa.Download[hawkbit.HTTPS] != nil {
		artifact.Link = sa.Download[hawkbit.HTTPS].URL
	} else if sa.Download[hawkbit.HTTP] != nil {
		artifact.Link = sa.Download[hawkbit.HTTP].URL
	} else {
		return nil, fmt.Errorf("unknown or missing link for artifact %s", sa.Filename)
	}

	// Set artifact checksum with following priority: SHA256, SHA1, MD5
	if sa.Checksums[hawkbit.SHA256] != "" {
		artifact.HashValue = sa.Checksums[hawkbit.SHA256]
		artifact.HashType = string(hawkbit.SHA256)
	} else if sa.Checksums[hawkbit.SHA1] != "" {
		artifact.HashValue = sa.Checksums[hawkbit.SHA1]
		artifact.HashType = string(hawkbit.SHA1)
	} else if sa.Checksums[hawkbit.MD5] != "" {
		artifact.HashValue = sa.Checksums[hawkbit.MD5]
		artifact.HashType = string(hawkbit.MD5)
	} else {
		return nil, fmt.Errorf("unknown or missing hash information for artifact %s", sa.Filename)
	}
	logger.Tracef("Convert artifact [%v] to [%v]", sa, artifact)
	return artifact, nil
}

func move(src string, dest string) error {
	logger.Debugf("Move directory [%s] to [%s]", src, dest)
	files, err := os.ReadDir(src)
	if err != nil {
		return err
	}

	for _, file := range files {
		nSrc := filepath.Join(src, file.Name())
		nDest := filepath.Join(dest, file.Name())
		if file.IsDir() {
			// Remove existing installed dependensies (directory).
			if _, err := os.Stat(nDest); !os.IsNotExist(err) {
				if err = os.RemoveAll(nDest); err != nil {
					return err
				}
				logger.Debugf("Removed existing directory: %s", nDest)
			}
			// Get source directory permissions and apply them to the destination directory.
			if err := os.MkdirAll(nDest, 0755); err != nil {
				return err
			}
			logger.Debugf("Created directory: %s", nDest)
			if err := move(nSrc, nDest); err != nil {
				return err
			}
		} else {
			logger.Debugf("Rename [%s] to [%s]", nSrc, nDest)
			if err = os.Rename(nSrc, nDest); err != nil {
				return err
			}
		}
	}
	return nil
}

func searchAndMove(inDir string, toDir string, module *Module) {
	logger.Infof("Search for module [%s:%s]", module.Name, module.Version)

	if _, err := os.Stat(inDir); os.IsNotExist(err) {
		return
	}
	paths, err := os.ReadDir(inDir)
	if err != nil {
		logger.Warningf("failed to get archived modules names: %v", err)
		return
	}

	id := module.Name + ":" + module.Version
	for _, path := range paths {
		status := filepath.Join(inDir, path.Name(), InternalStatusName)
		if _, err := os.Stat(status); !os.IsNotExist(err) {
			s, _ := ReadLn(status)
			if s == id {
				dir := filepath.Join(inDir, path.Name())
				logger.Infof("Move archived module [%s] to directory: %s", id, dir)
				if err := move(dir, toDir); err != nil {
					logger.Errorf("failed to moved archived module [%s] to directory [%s]: %v", id, dir, err)
				} else if err := os.RemoveAll(dir); err != nil {
					logger.Errorf("failed to remove archived module directory [%s]: %v", dir, err)
				} else if err := os.Remove(filepath.Join(toDir, InternalStatusName)); err != nil {
					logger.Errorf("failed to remove old module internal status: %v", err)
				}
				return
			}
		}
	}
}
