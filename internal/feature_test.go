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
	"sync"
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

	noProgress = -12345
	noMessage  = "no message"
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
	if su, err := InitScriptBasedSU(cfg); err == nil {
		if su != nil {
			defer su.Close()
		}
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

// TestScriptBasedDownloadAndInstall tests ScriptBasedSoftwareUpdatable core functionality: init, install and download operations.
func TestScriptBasedDownloadAndInstall(t *testing.T) {
	testScriptBasedSoftwareUpdatableOperations(true, t)
}

// TestScriptBasedDownloadAndInstallResume tests ScriptBasedSoftwareUpdatable core functionality with operation resume
func TestScriptBasedDownloadAndInstallResume(t *testing.T) {
	testScriptBasedSoftwareUpdatableOperations(false, t)
}

func testScriptBasedSoftwareUpdatableOperations(noResume bool, t *testing.T) {
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
	defer feature.Disconnect(true)

	if noResume {
		testDownloadInstall(feature, mc, w.GenerateSoftwareArtifacts(false, "install"), t)
		feature.serverCert = testCert
		testDownloadInstall(feature, mc, wSecure.GenerateSoftwareArtifacts(true, "install"), t)
	} else {
		testResume(feature, mc, w.GenerateSoftwareArtifacts(false, "install"), t)
	}
}

func testResume(feature *ScriptBasedSoftwareUpdatable, mc *mockedClient, artifacts []*hawkbit.SoftwareArtifactAction, t *testing.T) {
	sua := prepareSoftwareUpdateAction(artifacts)
	testReconnectWhileRunningOperation(feature, mc, sua, false, t) // disconnect while downloading
	testReconnectWhileRunningOperation(feature, mc, sua, true, t)  // disconnect while downloading or installing
}

func testReconnectWhileRunningOperation(feature *ScriptBasedSoftwareUpdatable, mc *mockedClient,
	sua *hawkbit.SoftwareUpdateAction, install bool, t *testing.T) {
	var waitDisconnect sync.WaitGroup
	waitDisconnect.Add(1)
	if install {
		feature.installHandler(sua, feature.su)
	} else {
		feature.downloadHandler(sua, feature.su)
	}
	firstIterationEventCount, secondIterationEventCount := 2, 3
	if install {
		firstIterationEventCount = 5
	}
	statuses := pullStatusChanges(mc, firstIterationEventCount) // should go between DOWNLOADING/INSTALLING and next state

	go func() { // blocks until done
		feature.Disconnect(false)
		waitDisconnect.Done()
	}()

	statuses = append(statuses, pullStatusChanges(mc, secondIterationEventCount)...)
	waitDisconnect.Wait()
	defer feature.Connect(mc, supConfig, edgeCfg)
	if install {
		checkInstallStatusEvents(statuses, t)
	} else {
		checkDownloadStatusEvents(statuses, t)
	}
}

func pullStatusChanges(mc *mockedClient, expectedCount int) []interface{} {
	var statuses []interface{}
	for i := 0; i < expectedCount; i++ {
		lo := mc.pullLastOperationStatus()
		if _, ok := lo["status"]; !ok {
			break
		}
		statuses = append(statuses, lo)
	}
	return statuses
}

func testDownloadInstall(feature *ScriptBasedSoftwareUpdatable, mc *mockedClient, artifacts []*hawkbit.SoftwareArtifactAction, t *testing.T) {
	sua := prepareSoftwareUpdateAction(artifacts)

	// Try to execute a simple download operation.
	feature.downloadHandler(sua, feature.su)

	statuses := pullStatusChanges(mc, 5)
	checkDownloadStatusEvents(statuses, t)

	// Try to execute a simple install operation.
	feature.installHandler(sua, feature.su)

	statuses = pullStatusChanges(mc, 8)
	checkInstallStatusEvents(statuses, t)
}

func checkDownloadStatusEvents(statuses []interface{}, t *testing.T) {
	statusSeq := []string{
		string(hawkbit.StatusStarted), string(hawkbit.StatusDownloading), string(hawkbit.StatusDownloading),
		string(hawkbit.StatusDownloaded), string(hawkbit.StatusFinishedSuccess),
	}
	progressSeq := []float64{
		noProgress, noProgress, 100.0, 100.0, noProgress,
	}
	messageSeq := []string{
		noMessage, noMessage, noMessage, noMessage, noMessage,
	}
	checkStatusEvents(statusSeq, progressSeq, messageSeq, statuses, t)
}

func checkInstallStatusEvents(statuses []interface{}, t *testing.T) {
	statusSeq := []string{
		string(hawkbit.StatusStarted), string(hawkbit.StatusDownloading), string(hawkbit.StatusDownloading),
		string(hawkbit.StatusDownloaded), string(hawkbit.StatusInstalling), string(hawkbit.StatusInstalling),
		string(hawkbit.StatusInstalled), string(hawkbit.StatusFinishedSuccess),
	}
	progressSeq := []float64{
		noProgress, noProgress, 100.0, 100.0, noProgress, noProgress, noProgress, noProgress,
	}
	messageSeq := []string{
		noMessage, noMessage, noMessage, noMessage, noMessage, "My final message!", "My final message!", "My final message!",
	}
	checkStatusEvents(statusSeq, progressSeq, messageSeq, statuses, t)
}

func checkStatusEvents(statusSeq []string, progressSeq []float64, messageSeq []string, statuses []interface{}, t *testing.T) {
	if len(statusSeq) != len(statuses) {
		t.Fatalf("wrong number of operation status events, expected statuses - %v, received events(includes whole payload) - %v", statusSeq, statuses)
	}
	for i, el := range statusSeq {
		lo := statuses[i].(map[string]interface{})
		if el != lo["status"] {
			t.Fatalf("received unexpected lastOperation status: %s != %s", lo["status"], el)
		}
		checkStatusParameter("progress", progressSeq[i], progressSeq[i] == noProgress, lo, t)
		checkStatusParameter("message", messageSeq[i], messageSeq[i] == noMessage, lo, t)
	}
}

func checkStatusParameter(name string, expectedValue interface{}, noValue bool, lo map[string]interface{}, t *testing.T) {
	receivedValue, ok := lo[name]
	if ok {
		if noValue {
			t.Fatalf("no %s expected in payload: %v", name, lo)
		}
		if expectedValue != receivedValue {
			t.Fatalf("received unexpected lastOperation %s: %s != %s", name, receivedValue, expectedValue)
		}
	} else if !noValue {
		t.Fatalf("no %s found in payload: %v", name, lo)
	}
}

func prepareSoftwareUpdateAction(artifacts []*hawkbit.SoftwareArtifactAction) *hawkbit.SoftwareUpdateAction {
	// Prepare simple software update action.
	return &hawkbit.SoftwareUpdateAction{
		CorrelationID: "test-correlation-id",
		SoftwareModules: []*hawkbit.SoftwareModuleAction{{
			SoftwareModule: &hawkbit.SoftwareModuleID{Name: "test", Version: "1.0.0"},
			Artifacts:      artifacts,
			Metadata:       map[string]string{"artifact-type": "plane"},
		}},
	}
}
