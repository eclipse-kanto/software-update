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
	"errors"
	"fmt"
	"io/fs"
	"math"
	"os"
	"path/filepath"
	"sort"
	"strconv"

	"github.com/eclipse-kanto/software-update/hawkbit"
	"github.com/eclipse-kanto/software-update/internal/logger"
)

var (
	// ErrCancel represents cancel operation error.
	ErrCancel = errors.New("cancel operation")
	// ErrFileSizeExceeded represents file size exceeded error.
	ErrFileSizeExceeded = errors.New("file size exceeded")
)

// Progress represents a callback handler that is called on written file chunk.
type Progress func(percent int)
type progressBytes func(bytes int64)

// A Updatable represents a simplified SoftwareUpdateAction.
type Updatable struct {
	Operation     string    `json:"operation"`
	CorrelationID string    `json:"correlationId"`
	Modules       []*Module `json:"softwareModules,omitempty"`
}

// Module represents a SoftwareModuleAction.
type Module struct {
	Name      string            `json:"name"`
	Version   string            `json:"version"`
	Artifacts []*Artifact       `json:"artifacts,omitempty"`
	Metadata  map[string]string `json:"metadata,omitempty"`
}

// A Artifact represents a SoftwareArtifactAction where checksums are simplified to hashType and hashValue.
// Hash priority is as follow:
//  1. checksums#SHA256
//  2. checksums#SHA1
//  3. checksums#SHA256
//  4. checksums#MD5
//  5. First downloadable download#links#md5url
// Links are simplified to simple list with download URIs without any links for MD5 hashes.
type Artifact struct {
	FileName  string `json:"fileName"`
	Size      int    `json:"size"`
	HashType  string `json:"hashType"`
	HashValue string `json:"hashValue"`
	Link      string `json:"link"`
}

// A Storage for Script-Based SoftwareUpdatable.
type Storage struct {
	// DownloadPath represents the download directory location.
	DownloadPath string
	// InstalledDepsPath represents the installed dependencies directory location.
	InstalledDepsPath string
	// ModulesPath represents the downloaded modules directory location.
	ModulesPath string
	// done is used to stop ongoing downloads.
	done chan struct{}
}

const (
	// InternalStatusName represents the name of the internal status file.
	InternalStatusName = "internal-status"
	// SoftwareUpdatableName represents the name of the software updatable file.
	SoftwareUpdatableName = "updatable.json"
)

// NewStorage for Script-Based SoftwareUpdatable is created.
func NewStorage(location string) (*Storage, error) {
	logger.Infof("New Storage with location: %s", location)
	this := &Storage{
		DownloadPath:      filepath.Join(location, "download"),
		InstalledDepsPath: filepath.Join(location, "installed-deps"),
		ModulesPath:       filepath.Join(location, "modules"),
		done:              make(chan struct{}),
	}
	// Create Downlaod directory.
	if err := os.MkdirAll(this.DownloadPath, 0755); err != nil {
		return nil, fmt.Errorf("failed to create download directory: %v", err)
	}
	// Create Installed Dependencies directory.
	if err := os.MkdirAll(this.InstalledDepsPath, 0755); err != nil {
		return nil, fmt.Errorf("failed to create installed dependencies directory: %v", err)
	}
	// Create Modules directory.
	if err := os.MkdirAll(this.ModulesPath, 0755); err != nil {
		return nil, fmt.Errorf("failed to create modules directory: %v", err)
	}
	return this, nil
}

// Close ongoing storage operations.
func (st *Storage) Close() {
	close(st.done)
}

// LoadInstalledDeps from file system.
func (st *Storage) LoadInstalledDeps() ([]*hawkbit.DependencyDescription, error) {
	logger.Debug("Load installed dependencies")
	ids := []*hawkbit.DependencyDescription{}
	cnt := 0

	// Search for installed dependencies.
	logger.Debugf("Search for installed dependencies in %s", st.InstalledDepsPath)
	err := filepath.WalkDir(st.InstalledDepsPath, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		// Search for *.prs files in root and first level child directories.
		if d.Type().IsRegular() && filepath.Ext(path) == ".prs" {
			logger.Trace("Found *.prs file: ", path)
			if parent := filepath.Dir(path); parent == st.InstalledDepsPath || filepath.Dir(parent) == st.InstalledDepsPath {
				cnt++
				id, err := loadInstalledDep(path)
				if err != nil {
					return err
				}
				ids = append(ids, id)
			}
		}
		return nil
	})
	logger.Tracef("Installed Dependencies: %v", ids)
	return ids, err
}

// LoadSoftwareUpdatables from unfinished operations.
func (st *Storage) LoadSoftwareUpdatables() map[string]*Updatable {
	logger.Debugf("Search for unfinished operations in %s", st.DownloadPath)
	// Get all directories in Download direcotry.
	paths, err := os.ReadDir(st.DownloadPath)
	if err != nil {
		logger.Errorf("failed to load operations: %v", err)
		return map[string]*Updatable{}
	}

	// Convert ditectory names in Download directory to indexes and sort them.
	ids := make([]int, 0, len(paths))
	for _, path := range paths {
		if i, err := strconv.Atoi(path.Name()); err == nil {
			ids = append(ids, i)
		}
	}
	sort.Ints(ids)

	updatables := map[string]*Updatable{}
	for _, name := range ids {
		dir := filepath.Join(st.DownloadPath, strconv.Itoa(name))
		to := filepath.Join(dir, SoftwareUpdatableName)
		if _, err := os.Stat(to); os.IsNotExist(err) {
			continue
		}
		updatable, err := loadSoftwareUpdatable(to)
		if err != nil || updatable == nil {
			logger.Debugf("fail to load [%s]: %v", to, err)
			continue
		}
		updatables[dir] = updatable
	}
	return updatables
}

// MoveInstalledDeps to local storage.
func (st *Storage) MoveInstalledDeps(dir string, metadata map[string]string) error {
	idp := "installed-deps"
	if metadata != nil && metadata["installed-deps"] != "" {
		idp = metadata["installed-deps"]
	}
	idp = filepath.Join(dir, idp)

	if _, err := os.Stat(idp); os.IsNotExist(err) {
		logger.Infof("No predefined installed dependencies.")
		return nil
	}
	logger.Infof("Move installed dependencies [%s] to [%s]", idp, st.InstalledDepsPath)
	return move(idp, st.InstalledDepsPath)
}

// ArchiveModule to modules directory.
func (st *Storage) ArchiveModule(dir string) error {
	logger.Debugf("Archive module from directory: %s", dir)
	path, err := FindAvailableLocation(st.ModulesPath)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(path, 0755); err != nil {
		return err
	}
	return move(dir, path)
}

// DownloadModule artifacts to local storage.
func (st *Storage) DownloadModule(toDir string, module *Module, progress Progress, serverCert string) (err error) {
	logger.Debugf("Download module to directory: [%s]", toDir)
	logger.Tracef("Module: %v", module)
	if err = os.MkdirAll(toDir, 0755); err != nil {
		return err
	}
	searchAndMove(st.ModulesPath, toDir, module)

	callback := func(bytes int64) { /* This is a wrapper function, do nothing by default. */ }
	if progress != nil {
		var totalSize int64
		for _, sa := range module.Artifacts {
			totalSize += int64(sa.Size)
		}
		logger.Debugf("Total module size: %v", totalSize)

		var totalWritten int64
		var lProgress int
		callback = func(bytes int64) {
			totalWritten += bytes
			cProgress := int(math.Round(float64(totalWritten) / float64(totalSize) * 100.0))
			if lProgress != cProgress {
				lProgress = cProgress
				progress(cProgress)
			}
		}
	}

	for _, sa := range module.Artifacts {
		if err = downloadArtifact(filepath.Join(toDir, sa.FileName), sa, callback, serverCert, st.done); err != nil {
			return err
		}
	}
	return err
}
