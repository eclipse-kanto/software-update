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

package feature

import (
	"os"
	"path/filepath"
	"strconv"

	"github.com/eclipse-kanto/software-update/hawkbit"
	"github.com/eclipse-kanto/software-update/internal/logger"
	"github.com/eclipse-kanto/software-update/internal/storage"
)

// downloadHandler adds the operation to the queue.
func (f *ScriptBasedSoftwareUpdatable) downloadHandler(
	update *hawkbit.SoftwareUpdateAction, su *hawkbit.SoftwareUpdatable) {
	// Create new operation wrapper.
	op := func(dir string, updatable *storage.Updatable) bool {
		return f.downloadModules(dir, updatable, su)
	}
	f.prepare("download", update.CorrelationID, update.SoftwareModules, op)
}

// downloadModule is called by download handler and after restart with remaining updatables.
// returns true if canceled!
// testing
func (f *ScriptBasedSoftwareUpdatable) downloadModules(
	toDir string, updatable *storage.Updatable, su *hawkbit.SoftwareUpdatable) bool {
	// Process download operation.
	logger.Debugf("Process download operation with id: %s", updatable.CorrelationID)

	// Download all modules.
	for i, module := range updatable.Modules {
		select {
		case <-done:
			return true // Cancel: application is closing!
		default:
			if f.downloadModule(updatable.CorrelationID, module, filepath.Join(toDir, strconv.Itoa(i)), su) {
				return true // Cancel: application is closing!
			}
		}
	}

	// Archive all modules.
	for i, module := range updatable.Modules {
		if err := f.store.ArchiveModule(filepath.Join(toDir, strconv.Itoa(i))); err != nil {
			logger.Errorf("failed to archive module [%s.%s]: %v", module.Name, module.Version, err)
		}
	}

	// Remove operation woring directory
	logger.Debugf("Remove download operation working directory: %s", toDir)
	if err := os.RemoveAll(toDir); err != nil {
		logger.Errorf("failed to remove directory [%s]: %v", toDir, err)
	}
	return false
}

// downloadModule returns true if canceled!
func (f *ScriptBasedSoftwareUpdatable) downloadModule(
	cid string, module *storage.Module, toDir string, su *hawkbit.SoftwareUpdatable) bool {
	// Download module to directory.
	logger.Infof("Download module [%s.%s] to directory: %s", module.Name, module.Version, toDir)
	// Create few useful variables.
	id := module.Name + ":" + module.Version
	s := filepath.Join(toDir, storage.InternalStatusName)
	var opError error
	opErrorMsg := errRuntime

	// Process final operation status in defer to also catch potential panic calls.
	defer func() {
		if opError == storage.ErrCancel {
			return // Cancel: application is closing!
		}
		storage.WriteLn(s, id)
		if err := recover(); err != nil { // In case of panic report FinishedError
			logger.Errorf("panic on module download [%s.%s] %v", module.Name, module.Version, err)
			setLastOS(su, newOS(cid, module, hawkbit.StatusFinishedError).WithMessage(errRuntime))
		} else if opError != nil { // In case of error report FinishedError
			logger.Errorf("failed to download module [%s.%s]: %v", module.Name, module.Version, opError)
			setLastOS(su, newOS(cid, module, hawkbit.StatusFinishedError).WithMessage(opErrorMsg))
		} else { // Success
			setLastOS(su, newOS(cid, module, hawkbit.StatusFinishedSuccess))
		}
	}()

	// Create module directory
	if opError = os.MkdirAll(toDir, 0755); opError != nil {
		return false
	}

	// Read previous module status
	status, _ := storage.ReadLn(s)
	switch status {
	case string(hawkbit.StatusStarted):
		goto Started
	case string(hawkbit.StatusDownloading):
		goto Downloading
	case id:
		return false
	default: // Unknown or missing internal state, do not jump to any labels.
	}

	// Started
	logger.Debugf("[%s.%s] Module download started", module.Name, module.Version)
	setLastOS(su, newOS(cid, module, hawkbit.StatusStarted))
	storage.WriteLn(s, string(hawkbit.StatusStarted))
Started:

	// Downloading
	logger.Debugf("[%s.%s] Downloading module", module.Name, module.Version)
	setLastOS(su, newOS(cid, module, hawkbit.StatusDownloading))
	storage.WriteLn(s, string(hawkbit.StatusDownloading))
Downloading:
	if opError = f.store.DownloadModule(toDir, module, func(percent int) {
		setLastOS(su, newOS(cid, module, hawkbit.StatusDownloading).WithProgress(percent))
	}, f.serverCert, f.downloadRetryCount, f.downloadRetryInterval, func() error {
		return f.validateLocalArtifacts(module)
	}); opError != nil {
		opErrorMsg = errDownload
		logger.Errorf("error downloading module [%s.%s] - %v", module.Name, module.Version, opError)
		return opError == storage.ErrCancel
	}

	// Downloaded
	logger.Debugf("[%s.%s] Module download finished", module.Name, module.Version)
	setLastOS(su, newOS(cid, module, hawkbit.StatusDownloaded).WithProgress(100))
	storage.WriteLn(s, string(hawkbit.StatusDownloaded))
	return false
}
