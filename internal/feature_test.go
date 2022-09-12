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

	statusParam   = "status"
	progressParam = "progress"
	messageParam  = "message"
)

// TestScriptBasedConstructor tests NewScriptBasedSU with wrong broker URL.
func TestScriptBasedConstructor(t *testing.T) {
	// Prepare
	dir := assertDirs(t, testDirFeature, true)
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
	dir := assertDirs(t, testDirFeature, false)
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
	dir := assertDirs(t, testDirFeature, false)
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
	dir := assertDirs(t, testDirFeature, false)
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
	dir := assertDirs(t, testDirFeature, false)
	// Remove temporary directory at the end.
	defer os.RemoveAll(dir)

	// Prepare/Close simple HTTP server used to host testing artifacts
	w := storage.NewTestHTTPServer(testDefaultHost, "", 0, t)
	w.Host(true, false, "", "")
	w.AddInstallScript()
	defer w.Close()

	wSecure := storage.NewTestHTTPServer(testDefaultHostSecure, "", 0, t)
	wSecure.Host(true, true, testCert, testKey)
	wSecure.AddInstallScript()
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
		testDisconnect(feature, mc, w.GenerateSoftwareArtifacts(false, "install"), t)
	}
}

func testDisconnect(feature *ScriptBasedSoftwareUpdatable, mc *mockedClient, artifacts []*hawkbit.SoftwareArtifactAction, t *testing.T) {
	sua := prepareSoftwareUpdateAction(artifacts)
	testDisconnectWhileRunningOperation(feature, mc, sua, false, t) // disconnect while downloading
	testDisconnectWhileRunningOperation(feature, mc, sua, true, t)  // disconnect while installing
}

func testDisconnectWhileRunningOperation(feature *ScriptBasedSoftwareUpdatable, mc *mockedClient,
	sua *hawkbit.SoftwareUpdateAction, install bool, t *testing.T) {
	var waitDisconnect sync.WaitGroup
	waitDisconnect.Add(1)
	if install {
		feature.installHandler(sua, feature.su)
	} else {
		feature.downloadHandler(sua, feature.su)
	}
	preDisconnectEventCount := 2  // STARTED, DOWNLOADING
	postDisconnectEventCount := 3 // DOWNLOADING(100)/INSTALLING(100), DOWNLOADED/INSTALLED, FINISHED_SUCCESS
	if install {
		preDisconnectEventCount = 5 // STARTED, DOWNLOADING, DOWNLOADING(100), DOWNLOADED, INSTALLING
	}
	statuses := pullStatusChanges(mc, preDisconnectEventCount) // should go between DOWNLOADING/INSTALLING and next state

	go func() { // decrements count number with 1, when disconnected
		feature.Disconnect(false)
		waitDisconnect.Done()
	}()

	statuses = append(statuses, pullStatusChanges(mc, postDisconnectEventCount)...)
	waitDisconnect.Wait()
	defer connectFeature(t, mc, feature, getDefaultFlagValue(t, flagFeatureID))
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
		statuses = append(statuses, lo)
	}
	return statuses
}

func testDownloadInstall(feature *ScriptBasedSoftwareUpdatable, mc *mockedClient, artifacts []*hawkbit.SoftwareArtifactAction, t *testing.T) {
	sua := prepareSoftwareUpdateAction(artifacts)

	// Try to execute a simple download operation.
	feature.downloadHandler(sua, feature.su)

	statuses := pullStatusChanges(mc, 5) // STARTED, DOWNLOADING, DOWNLOADING(100), DOWNLOADED, FINISHED_SUCCESS
	checkDownloadStatusEvents(statuses, t)

	// Try to execute a simple install operation.
	feature.installHandler(sua, feature.su)

	statuses = pullStatusChanges(mc, 8) // STARTED, DOWNLOADING, DOWNLOADING(100), DOWNLOADED,
	// INSTALLING, INSTALLING(100), INSTALLED, FINISHED_SUCCESS
	checkInstallStatusEvents(statuses, t)
}

func createStatus(status hawkbit.Status, progress float64, message string) map[string]interface{} {
	return map[string]interface{}{
		statusParam:   string(status),
		progressParam: progress,
		messageParam:  message,
	}
}

func checkDownloadStatusEvents(actualStatuses []interface{}, t *testing.T) {
	var expectedStatuses []interface{}
	expectedStatuses = append(expectedStatuses,
		createStatus(hawkbit.StatusStarted, noProgress, noMessage),
		createStatus(hawkbit.StatusDownloading, noProgress, noMessage),
		createStatus(hawkbit.StatusDownloading, 100.0, noMessage),
		createStatus(hawkbit.StatusDownloaded, 100.0, noMessage),
		createStatus(hawkbit.StatusFinishedSuccess, noProgress, noMessage),
	)
	checkStatusEvents(expectedStatuses, actualStatuses, t)
}

func checkInstallStatusEvents(actualStatuses []interface{}, t *testing.T) {
	var expectedStatuses []interface{}
	expectedStatuses = append(expectedStatuses,
		createStatus(hawkbit.StatusStarted, noProgress, noMessage),
		createStatus(hawkbit.StatusDownloading, noProgress, noMessage),
		createStatus(hawkbit.StatusDownloading, 100.0, noMessage),
		createStatus(hawkbit.StatusDownloaded, 100.0, noMessage),
		createStatus(hawkbit.StatusInstalling, noProgress, noMessage),
		createStatus(hawkbit.StatusInstalling, noProgress, "My final message!"),
		createStatus(hawkbit.StatusInstalled, noProgress, "My final message!"),
		createStatus(hawkbit.StatusFinishedSuccess, noProgress, "My final message!"),
	)
	checkStatusEvents(expectedStatuses, actualStatuses, t)
}

func checkStatusEvents(expectedStatuses []interface{}, actualStatuses []interface{}, t *testing.T) {
	if len(expectedStatuses) != len(actualStatuses) {
		t.Fatalf("wrong number of operation status events, expected statuses - %v, received events(includes whole payload) - %v",
			expectedStatuses, actualStatuses)
	}
	for i, el := range expectedStatuses {
		expected := el.(map[string]interface{})
		actual := actualStatuses[i].(map[string]interface{})
		if expected[statusParam] != actual[statusParam] {
			t.Fatalf("received unexpected lastOperation status: %s != %s(actual)", expected[statusParam], actual[statusParam])
		}
		checkStatusParameter(expected[progressParam].(float64), actual, progressParam,
			expected[progressParam].(float64) == noProgress, t)
		checkStatusParameter(expected[messageParam], actual, messageParam, expected[messageParam] == noMessage, t)
	}
}

func checkStatusParameter(expectedParamValue interface{}, actualStatus map[string]interface{},
	name string, noValue bool, t *testing.T) {
	receivedValue, ok := actualStatus[name]
	if ok {
		if noValue {
			t.Fatalf("no %s expected in payload: %v", name, actualStatus)
		}
		if expectedParamValue != receivedValue {
			t.Fatalf("received unexpected lastOperation %s: %s != %s", name, receivedValue, expectedParamValue)
		}
	} else if !noValue {
		t.Fatalf("no %s found in payload: %v", name, actualStatus)
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
