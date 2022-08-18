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
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/eclipse-kanto/software-update/hawkbit"
	"github.com/eclipse-kanto/software-update/internal/storage"
)

const (
	testDirFeature        = "_tmp-feature"
	testDefaultHost       = ":12345"
	testDefaultHostSecure = ":12346"
	testCert              = "storage/testdata/valid_cert.pem"
	testKey               = "storage/testdata/valid_key.pem"
)

// TestScriptBasedConstructor tests NewScriptBasedSU with wrong broker URL.
func TestScriptBasedConstructor(t *testing.T) {
	// Prepare
	dir := assertPath(t, testDirFeature, true)
	// Remove temporary directory at the end.
	defer os.RemoveAll(dir)

	// Create simple ScriptBasedSoftwareUpdatableConfig.
	cfg := &ScriptBasedSoftwareUpdatableConfig{
		StorageLocation: dir,
		Username:        "admin",
		Password:        "admin",
		Broker:          "tcp://unknownhost:1883",
	}

	// 1. Try to create new ScriptBasedSoftwareUpdatable with wrong broker URL.
	if _, err := NewScriptBasedSU(cfg); err == nil {
		t.Fatalf("fail to validate with unknown broker host: %v", err)
	}
}

// TestNewScriptBasedInitHawkBitValidation tests the ScriptBasedSoftwareUpdatable initialization
// when invalid config field featureID is provided to HawkBit
func TestNewScriptBasedInitHawkBitValidation(t *testing.T) {
	// Prepare
	dir := assertPath(t, testDirFeature, false)
	// Remove temporary directory at the end.
	defer os.RemoveAll(dir)

	// 1. Try to init a new ScriptBasedSoftwareUpdatable with missing mandatory featureID
	_, _, err := mockScriptBasedSoftwareUpdatable(t, &testConfig{
		storageLocation: dir, featureID: "", clientConnected: true})
	if err == nil {
		t.Fatalf("expected to fail when mandatory field featureID is empty")
	}
}

// TestScriptBasedInitLoadDependencies tests the ScriptBasedSoftwareUpdatable initialization
// with invalid mandatory field in dependencies on load
func TestScriptBasedInitLoadDependencies(t *testing.T) {
	// Prepare
	dir := assertPath(t, testDirFeature, false)
	// Remove temporary directory at the end.
	defer os.RemoveAll(dir)

	// write initial file with group empty only
	if err := os.MkdirAll(filepath.Join(dir, "installed-deps"), 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}
	err2 := ioutil.WriteFile(filepath.Join(dir, "installed-deps", "test.prs"), []byte("group="), 0755)
	if err2 != nil {
		t.Errorf("unable to create or write to temporary file: %v, reason: %v", "test.prs", err2)
	}

	// 1. Try to init a new ScriptBasedSoftwareUpdatable with error for loading install dependencies
	_, _, err := mockScriptBasedSoftwareUpdatable(t, &testConfig{
		clientConnected: true, storageLocation: dir, featureID: getDefaultFlagValue(t, flagFeatureID)})
	if err == nil {
		t.Fatalf("expected to fail when mandatory field is missing in insalled dept file")
	}
}

// TestScriptBasedInit tests the ScriptBasedSoftwareUpdatable initialization when the client is not connected
func TestScriptBasedInit(t *testing.T) {
	// Prepare
	dir := assertPath(t, testDirFeature, false)
	// Remove temporary directory at the end.
	defer os.RemoveAll(dir)

	// 1. Try to init a new ScriptBasedSoftwareUpdatable with error for not connected client
	_, _, err := mockScriptBasedSoftwareUpdatable(t, &testConfig{
		clientConnected: false, storageLocation: dir, featureID: getDefaultFlagValue(t, flagFeatureID)})
	if err == nil {
		t.Fatal("ditto Client shall not be connected!")
	}

}

// TestScriptBasedCore tests ScriptBasedSoftwareUpdatable core functionality: init, install and download.
func TestScriptBasedCore(t *testing.T) {
	// Prepare
	dir := assertPath(t, testDirFeature, false)
	// Remove temporary directory at the end.
	defer os.RemoveAll(dir)

	// Prepare/Close simple HTTP server used to host testing artifacts
	w := storage.Host(testDefaultHost, "", 0, true, false, "", "", t).AddInstallScript()
	defer w.Close()

	wSecure := storage.Host(testDefaultHostSecure, "", 0, true, true, testCert, testKey, t).AddInstallScript()
	defer wSecure.Close()

	// 1. Try to init a new ScriptBasedSoftwareUpdatable.
	feature, mc, err := mockScriptBasedSoftwareUpdatable(t, &testConfig{
		clientConnected: true, featureID: getDefaultFlagValue(t, flagFeatureID), storageLocation: dir})
	if err != nil {
		t.Fatalf("failed to initialize ScriptBasedSoftwareUpdatable: %v", err)
	}
	defer feature.Disconnect()

	testDownloadInstall(feature, mc, w.GenerateSoftwareArtifacts(false, "install"), t)

	feature.serverCert = testCert
	testDownloadInstall(feature, mc, wSecure.GenerateSoftwareArtifacts(true, "install"), t)
}

func testDownloadInstall(feature *ScriptBasedSoftwareUpdatable, mc *mockedClient, artifacts []*hawkbit.SoftwareArtifactAction, t *testing.T) {
	// Preapare simple software update action.
	sua := &hawkbit.SoftwareUpdateAction{
		CorrelationID: "test-correlation-id",
		SoftwareModules: []*hawkbit.SoftwareModuleAction{{
			SoftwareModule: &hawkbit.SoftwareModuleID{Name: "test", Version: "1.0.0"},
			Artifacts:      artifacts,
			Metadata:       map[string]string{"artifact-type": "plane"},
		}},
	}

	// 2. Try to execute simple an download operation.
	feature.downloadHandler(sua, feature.su)

	// 2.1. Check for STARTED status.
	if lo := mc.lastOperation(t); lo["status"] != "STARTED" {
		t.Fatalf("received unexpected lastOperation status: %s != STARTED", lo["status"])
	}

	// 2.2. Check for DOWNLOADING status.
	if lo := mc.lastOperation(t); lo["status"] != "DOWNLOADING" {
		t.Fatalf("received unexpected lastOperation status: %s != DOWNLOADING", lo["status"])
	}

	// 2.3. Check for 100% DOWNLOADING status.
	lo := mc.lastOperation(t)
	if lo["status"] != "DOWNLOADING" {
		t.Fatalf("received unexpected lastOperation status: %s != DOWNLOADING", lo["status"])
	}
	if lo["progress"] != 100.0 {
		t.Fatalf("received unexpected lastOperation percent: %s != 100", lo["progress"])
	}

	// 2.4. Check for DOWNLOADED status.
	if lo := mc.lastOperation(t); lo["status"] != "DOWNLOADED" {
		t.Fatalf("received unexpected lastOperation status: %s != DOWNLOADED", lo["status"])
	}

	// 2.5. Check for FINISHED_SUCCESS status.
	if lo := mc.lastOperation(t); lo["status"] != "FINISHED_SUCCESS" {
		t.Fatalf("received unexpected lastOperation status: %s != FINISHED_SUCCESS", lo["status"])
	}

	// 3. Try to execute simple install operation.
	feature.installHandler(sua, feature.su)

	// 3.1. Check for STARTED status.
	if lo := mc.lastOperation(t); lo["status"] != "STARTED" {
		t.Fatalf("received unexpected lastOperation status: %s != STARTED", lo["status"])
	}

	// 3.2. Check for DOWNLOADING status.
	lo = mc.lastOperation(t)
	if lo["status"] != "DOWNLOADING" {
		t.Fatalf("received unexpected lastOperation status: %s != DOWNLOADING", lo["status"])
	}

	// 3.3. Check for 100% DOWNLOADING status.
	lo = mc.lastOperation(t)
	if lo["status"] != "DOWNLOADING" {
		t.Fatalf("received unexpected lastOperation status: %s != DOWNLOADING", lo["status"])
	}
	if lo["progress"] != 100.0 {
		t.Fatalf("received unexpected lastOperation percent: %s != 100", lo["progress"])
	}

	// 3.4. Check for DOWNLOADED status.
	if lo := mc.lastOperation(t); lo["status"] != "DOWNLOADED" {
		t.Fatalf("received unexpected lastOperation status: %s != DOWNLOADED", lo["status"])
	}

	// 3.5. Check for INSTALLING status.
	if lo := mc.lastOperation(t); lo["status"] != "INSTALLING" {
		t.Fatalf("received unexpected lastOperation status: %s != INSTALLING", lo["status"])
	}

	//3.6. Check for INSTALLING status with 100%.
	if lo := mc.lastOperation(t); lo["status"] != "INSTALLING" {
		t.Fatalf("received unexpected lastOperation status: %s != INSTALLING", lo["status"])
	}
	if lo["progress"] != 100.0 {
		t.Fatalf("received unexpected lastOperation percent: %s != 100", lo["progress"])
	}

	//3.7. Check for INSTALLED status with "My final message!" message.
	lo = mc.lastOperation(t)
	if lo["status"] != "INSTALLED" {
		t.Fatalf("received unexpected lastOperation status: %s != INSTALLED", lo["status"])
	}
	if lo["message"] != "My final message!" {
		t.Fatalf("received unexpected lastOperation message: %s != My final message!", lo["message"])
	}
	// 3.8. Check for FINISHED_SUCCESS status.
	if lo := mc.lastOperation(t); lo["status"] != "FINISHED_SUCCESS" {
		t.Fatalf("received unexpected lastOperation status: %s != FINISHED_SUCCESS", lo["status"])
	}
}
