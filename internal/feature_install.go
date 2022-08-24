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
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"

	"github.com/eclipse-kanto/software-update/hawkbit"
	"github.com/eclipse-kanto/software-update/internal/logger"
	"github.com/eclipse-kanto/software-update/internal/storage"
)

// installHandler adds the operation to the queue.
func (f *ScriptBasedSoftwareUpdatable) installHandler(
	update *hawkbit.SoftwareUpdateAction, su *hawkbit.SoftwareUpdatable) {
	// Create new operation wrapper.
	op := func(dir string, updatable *storage.Updatable) bool {
		return f.installModules(dir, updatable, su)
	}
	f.prepare("install", update.CorrelationID, update.SoftwareModules, op)
}

// installModules is called by install handler and after restart with remaining updatables.
// returns true if canceled!
func (f *ScriptBasedSoftwareUpdatable) installModules(
	toDir string, updatable *storage.Updatable, su *hawkbit.SoftwareUpdatable) bool {
	// Process install operation.
	logger.Debugf("Process install operation with id: %s", updatable.CorrelationID)

	// Install all modules.
	for i, module := range updatable.Modules {
		select {
		case <-done:
			return true // Cancel: application is closing!
		default:
			if f.installModule(updatable.CorrelationID, module, filepath.Join(toDir, fmt.Sprint(i)), su) {
				return true // Cancel: application is closing!
			}
		}
	}

	// Remove operation woring directory
	logger.Debugf("Remove install operation working directory: %s", toDir)
	if err := os.RemoveAll(toDir); err != nil {
		logger.Errorf("failed to remove directory [%s]: %v", toDir, err)
	}
	return false
}

// installModule returns true if canceled!
func (f *ScriptBasedSoftwareUpdatable) installModule(
	cid string, module *storage.Module, dir string, su *hawkbit.SoftwareUpdatable) bool {
	// Install module to direcotry.
	logger.Infof("Install module [%s.%s] from directory: %s", module.Name, module.Version, dir)
	// Create few useful variables.
	id := module.Name + ":" + module.Version
	s := filepath.Join(dir, storage.InternalStatusName)
	var opError error
	opErrorMsg := errRuntime

	// Process final operation status in defer to also catch potential panic calls.
	defer func() {
		if opError == storage.ErrCancel {
			return // Cancel: application is closing!
		}
		storage.WriteLn(s, id)
		if err := recover(); err != nil { // In case of panic report FinishedError
			logger.Errorf("panic in module installation [%s.%s]: %v", module.Name, module.Version, err)
			setLastOS(su, newOS(cid, module, hawkbit.StatusFinishedError).WithMessage(errRuntime))
		} else if opError != nil { // In case of error report FinishedError
			if exiterr, ok := opError.(*exec.ExitError); ok {
				logger.Errorf("failed to install module [%s.%s][ExitCode: %v]: %v",
					module.Name, module.Version, exiterr.ExitCode(), opError)
				setLastOS(su, newFileOS(dir, cid, module, hawkbit.StatusFinishedError).
					WithStatusCode(strconv.Itoa(exiterr.ExitCode())).
					WithMessage(opErrorMsg))
			} else {
				logger.Errorf("failed to install module [%s.%s]: %v", module.Name, module.Version, opError)
				setLastOS(su, newOS(cid, module, hawkbit.StatusFinishedError).WithMessage(opErrorMsg))
			}
		} else { // Success
			setLastOS(su, newFileOS(dir, cid, module, hawkbit.StatusFinishedSuccess))
		}
	}()

	// Create module directory
	if opError = os.MkdirAll(dir, 0755); opError != nil {
		return false
	}

	// Read previous module status
	lStatus, _ := storage.ReadLn(s)
	switch lStatus {
	case string(hawkbit.StatusStarted):
		goto Started
	case string(hawkbit.StatusDownloading):
		goto Downloading
	case string(hawkbit.StatusDownloaded):
		goto Downloaded
	case string(hawkbit.StatusInstalling):
		goto Installing
	case id:
		return false
	default: // Unknown or missing internal state, do not jump to any labels.
	}

	// Started
	logger.Debugf("[%s.%s] Module instalation started", module.Name, module.Version)
	setLastOS(su, newOS(cid, module, hawkbit.StatusStarted))
	storage.WriteLn(s, string(hawkbit.StatusStarted))
Started:

	// Downloading
	logger.Debugf("[%s.%s] Downloading module", module.Name, module.Version)
	setLastOS(su, newOS(cid, module, hawkbit.StatusDownloading))
	storage.WriteLn(s, string(hawkbit.StatusDownloading))
Downloading:
	if opError = f.store.DownloadModule(dir, module, func(progress int) {
		setLastOS(su, newOS(cid, module, hawkbit.StatusDownloading).WithProgress(progress))
	}, f.serverCert, f.downloadRetryCount, f.downloadRetryInterval); opError != nil {
		opErrorMsg = errDownload
		return opError == storage.ErrCancel
	}

	// Downloaded
	logger.Debugf("[%s.%s] Module download finished", module.Name, module.Version)
	setLastOS(su, newOS(cid, module, hawkbit.StatusDownloaded).WithProgress(100))
	storage.WriteLn(s, string(hawkbit.StatusDownloaded))
Downloaded:

	// Installing
	logger.Debugf("[%s.%s] Installing module", module.Name, module.Version)
	setLastOS(su, newOS(cid, module, hawkbit.StatusInstalling).WithProgress(0))
	storage.WriteLn(s, string(hawkbit.StatusInstalling))
Installing:

	// Monitor install progress
	monitor, err := (&monitor{
		status: hawkbit.StatusInstalling,
		su:     su,
		cid:    cid,
		module: &hawkbit.SoftwareModuleID{Name: module.Name, Version: module.Version},
	}).waitFor(dir)
	if err != nil {
		logger.Debugf("fail to start progress monitor: %v", err)
	}

	// Get artifact type
	artifactType := f.artifactType
	if module.Metadata != nil && module.Metadata["artifact-type"] != "" {
		artifactType = module.Metadata["artifact-type"]
	}
	if artifactType == "archive" { // Extract if needed
		if len(module.Artifacts) > 1 { // Only one archive/artifact is allowed in archive modules
			opErrorMsg = errMultiArchives
			opError = fmt.Errorf("archive modules cannot have multiples artifacts")
			return false
		}
		logger.Debugf("[%s.%s] Extract module archive(s) to: ", module.Name, module.Version)
		if opError = storage.ExtractArchive(dir); opError != nil {
			opErrorMsg = errExtractArchive
			return false
		}
	}

	// Start install script
	logger.Debugf("[%s.%s] Run module install script", module.Name, module.Version)
	if opError = f.installCommand.run(dir, "install"); opError != nil {
		opErrorMsg = errInstallScript
		return false
	}

	// Stop progress monitoring
	if monitor != nil {
		close(monitor)
	}

	// Move the predefined installed dependencies
	if opError = f.store.MoveInstalledDeps(dir, module.Metadata); opError != nil {
		opErrorMsg = errInstalledDepsSsave
		return false
	}

	// Installed
	logger.Debugf("[%s.%s] Module installed", module.Name, module.Version)
	setLastOS(su, newFileOS(dir, cid, module, hawkbit.StatusInstalled))

	// Update installed dependencies
	deps, err := f.store.LoadInstalledDeps()
	if err != nil {
		opError = err
		opErrorMsg = errInstalledDepsRefresh
		return false
	}
	logger.Debugf("[%s.%s] Set module installed dependencies", module.Name, module.Version)
	f.su.SetInstalledDependencies(deps...)
	return false
}
