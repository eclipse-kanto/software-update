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
	"path/filepath"

	"github.com/eclipse-kanto/software-update/hawkbit"
	"github.com/eclipse-kanto/software-update/internal/logger"
	"github.com/eclipse-kanto/software-update/internal/storage"
	"github.com/eclipse/ditto-clients-golang"
	"github.com/eclipse/ditto-clients-golang/model"
)

const (
	errRuntime               = "internal runtime error"
	errMultiArchives         = "archive modules cannot have multiple artifacts"
	errDownload              = "fail to download module"
	errExtractArchive        = "fail to extract module archive"
	errInstallScript         = "fail to execute install script"
	errInstalledDepsSave     = "fail to save installed dependencies"
	errInstalledDepsRefresh  = "fail to refresh installed dependencies"
	errDetermineAbsolutePath = "fail to determine absolute path of install script %s - %v"
)

// opw is an operation wrapper function.
type opw func(dir string, u *storage.Updatable) bool

func (f *ScriptBasedSoftwareUpdatable) init(
	scriptSUPConfig *ScriptBasedSoftwareUpdatableConfig, edge *edgeConfiguration) (err error) {
	// Create Ditto client.
	config := ditto.NewConfiguration().
		WithDisconnectTimeout(defaultDisconnectTimeout).
		WithConnectHandler(func(dittoClient *ditto.Client) {
			logger.Infof("Connected to MQTT broker: %s", scriptSUPConfig.Broker)
			f.su.Activate()
			go f.process()
			f.load()
		})

	f.dittoClient, err = ditto.NewClientMqtt(f.mqttClient, config)
	if err != nil {
		return fmt.Errorf("failed to create Ditto client: %v", err)
	}

	// Load Installed-Dependencies from file system.
	ids, err := f.store.LoadInstalledDeps()
	if err != nil {
		return fmt.Errorf("failed to load installed dependencies: %v", err)
	}

	// Create Hawkbit SoftwareUpdatable configuration.
	cfg := (&hawkbit.Configuration{}).
		WithDittoClient(f.dittoClient).
		WithThingID(model.NewNamespacedIDFrom(edge.DeviceID)).
		WithFeatureID(scriptSUPConfig.FeatureID).
		WithSoftwareType(scriptSUPConfig.ModuleType).
		WithInstallHandler(f.installHandler).
		WithDownloadHandler(f.downloadHandler)

	// Create new Hawkbit SoftwareUpdatable.
	if f.su, err = hawkbit.NewSoftwareUpdatable(cfg); err != nil {
		return err
	}
	f.su.SetInstalledDependencies(ids...)
	return nil
}

func (f *ScriptBasedSoftwareUpdatable) load() {
	f.lock.Lock()
	defer f.lock.Unlock()

	// Load all previous operations and add them to the queue
	updatables := f.store.LoadSoftwareUpdatables()
	for dir, updatable := range updatables {
		f.queue <- func() bool {
			// Add install operation to the queue.
			if updatable.Operation == "install" {
				return f.installModules(dir, updatable, f.su)
			}
			// Add download operation to the queue.
			if updatable.Operation == "download" {
				return f.downloadModules(dir, updatable, f.su)
			}
			return false
		}
	}
}

func (f *ScriptBasedSoftwareUpdatable) process() {
	f.lock.Lock()
	if !featureAvailable {
		f.lock.Unlock()
		return
	}
	wg.Add(1)
	f.lock.Unlock()
	defer wg.Done()

	// Wait for operations or close signal.
	for {
		select {
		case <-done:
			return // Cancel: application is closing!
		case op := <-f.queue:
			if op() {
				return // Cancel: application is closing!
			}
		}
	}
}

// prepare find available directory for the operation and saved it.
func (f *ScriptBasedSoftwareUpdatable) prepare(name string, cid string,
	modules []*hawkbit.SoftwareModuleAction, w opw) {
	// Lock current goroute until file operation is added to the queue.
	f.lock.Lock()
	defer f.lock.Unlock()
	wg.Add(1)
	defer wg.Done()

	logger.Tracef("Process %s operation: %v", name, modules)
	// Skip operations without modules.
	if len(modules) == 0 {
		return
	}

	// Find available directory to store the operation.
	toDir, err := storage.FindAvailableLocation(f.store.DownloadPath)
	if err != nil {
		logger.Debugf("Fail to find available directory: %v", err)
		f.fail(cid, modules)
		return
	}
	logger.Debugf("%s operation working directory: %s", name, toDir)

	// Save the operation to its directory.
	to := filepath.Join(toDir, storage.SoftwareUpdatableName)
	updatable, err := storage.SaveSoftwareUpdatable(name, cid, to, modules)
	if err != nil {
		logger.Debugf("Fail to save [%s] operation: %v", name, err)
		f.fail(cid, modules)
		return
	}

	// Add operation to the queue.
	f.queue <- func() bool {
		return w(toDir, updatable)
	}
}

// fail all modules in the operation.
func (f *ScriptBasedSoftwareUpdatable) fail(cid string, modules []*hawkbit.SoftwareModuleAction) {
	for _, module := range modules {
		setLastOS(f.su, (&hawkbit.OperationStatus{}).
			WithCorrelationID(cid).
			WithSoftwareModule(module.SoftwareModule).
			WithStatus(hawkbit.StatusFinishedError).
			WithMessage("Fail to save operation data."))
	}
}

// newOS returns newly created OperationStatus pointer.
func newOS(cid string, module *storage.Module, status hawkbit.Status) *hawkbit.OperationStatus {
	return hawkbit.NewOperationStatusUpdate(cid, status,
		&hawkbit.SoftwareModuleID{Name: module.Name, Version: module.Version})
}

// newFileOS returns newly created OperationStatus pointer, filled with the status file data.
func newFileOS(dir string, cid string, module *storage.Module, status hawkbit.Status) *hawkbit.OperationStatus {
	// Create base OperationStatus object.
	ops := newOS(cid, module, status)

	// Fill the OperationStatus object with the data from the status file.
	file := filepath.Join(dir, nStatus)
	if _, err := os.Stat(file); !os.IsNotExist(err) {
		p, m, c, done := load(file)
		if done {
			if c != "" {
				ops.WithStatusCode(c)
			}
			if p >= 0 && p <= 100 {
				ops.WithProgress(p)
			}
			if m != "" {
				ops.WithMessage(m)
			}
		}
	}
	return ops
}

// setLastOS sets the last operation status and log an error on error.
func setLastOS(su *hawkbit.SoftwareUpdatable, os *hawkbit.OperationStatus) {
	if err := su.SetLastOperation(os); err != nil {
		logger.Errorf("fail to send last operation status: %v", err)
	}
}

func (f *ScriptBasedSoftwareUpdatable) validateLocalArtifacts(module *storage.Module) error {
	logger.Debugf("validating local artifacts of module - %v", module)
	for _, sa := range module.Artifacts {
		if !storage.IsFileLink(sa.Link) {
			continue
		}
		location := f.locateArtifact(sa.Link)
		if location == "" {
			msg := fmt.Sprintf("could not locate local artifact [%s]", sa.Link)
			return fmt.Errorf(msg)
		}
		logger.Infof("resolved local artifact location - %s", location)
		sa.Link = location
	}
	return nil
}
