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

	noMessage       = "no message"
	anyErrorMessage = "*"

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
		testDownloadInstall(feature, mc, w.GenerateSoftwareArtifacts(false, "install"), true, "*", t)
		feature.serverCert = testCert
		testDownloadInstall(feature, mc, wSecure.GenerateSoftwareArtifacts(true, "install"), true, "*", t)
	} else {
		testDisconnect(feature, mc, w.GenerateSoftwareArtifacts(false, "install"), t)
	}
}

func testDisconnect(feature *ScriptBasedSoftwareUpdatable, mc *mockedClient, artifacts []*hawkbit.SoftwareArtifactAction, t *testing.T) {
	sua := prepareSoftwareUpdateAction(artifacts, "")
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
	// only 1 artifact here
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
		checkInstallStatusEvents(0, statuses, t)
	} else {
		checkDownloadStatusEvents(0, statuses, t)
	}
}

// TestScriptBasedDownloadAndInstallMixedResources tests ScriptBasedSoftwareUpdatable core functionality: init, install and download operations,
// working with both downloadable and local resources
func TestScriptBasedDownloadAndInstallMixedResources(t *testing.T) {
	// Prepare
	storageDir := assertDirs(t, testDirFeature, false)
	// Remove temporary directory at the end.
	defer os.RemoveAll(storageDir)

	feature, mc, err := mockScriptBasedSoftwareUpdatable(t, &testConfig{
		clientConnected: true, featureID: getDefaultFlagValue(t, flagFeatureID), storageLocation: storageDir, mode: modeLax,
	})
	if err != nil {
		t.Fatalf("failed to initialize ScriptBasedSoftwareUpdatable: %v", err)
	}
	defer feature.Disconnect(true)

	a, aBody := "a.txt", "test"
	aPath, aHash := createLocalArtifact(t, storageDir, a, aBody)
	b, bBody := "b.txt", "test"
	bPath, bHash := createLocalArtifact(t, storageDir, b, bBody)

	// Prepare/Close simple HTTP server used to host testing artifacts
	w := storage.NewTestHTTPServer(testDefaultHost, "", 0, t)
	w.Host(true, false, "", "")
	w.AddInstallScript()
	defer w.Close()

	installScript := w.GenerateSoftwareArtifacts(false, "install")
	artifacts := []*hawkbit.SoftwareArtifactAction{
		convertLocalArtifact(aPath, a, aHash, len(aBody)),
		convertLocalArtifact(bPath, b, bHash, len(bBody)),
		installScript[0],
	}

	testDownloadInstall(feature, mc, artifacts, true, b, t)
}

// TestScriptBasedDownloadAndInstallLocalResources tests ScriptBasedSoftwareUpdatable core functionality: init, install and download operations,
// but working with local resources
func TestScriptBasedDownloadAndInstallLocalResources(t *testing.T) {
	tmpDir := "testdata"
	assertDirs(t, tmpDir, true)
	defer os.RemoveAll(tmpDir)

	installScriptAlias, installScriptBody := storage.GetTestInstallScript()
	installScriptPath, installScriptHash := createLocalArtifact(t, tmpDir, installScriptAlias, installScriptBody)
	localResourceAlias, localResourceBody := "local.txt", "test"
	localResourcePath, localResourceHash := createLocalArtifact(t, tmpDir, localResourceAlias, localResourceBody)

	t.Run("Test_without_copying_artifacts", func(t *testing.T) {
		artifacts := []*hawkbit.SoftwareArtifactAction{
			convertLocalArtifact(installScriptPath, installScriptAlias, installScriptHash, len(installScriptBody)),
			convertLocalArtifact(getAbsolutePath(t, localResourcePath), localResourceAlias, localResourceHash, len(localResourceBody)),
		}
		testScriptBasedSoftwareUpdatableOperationsLocal(t, []string{}, artifacts, modeLax, "", true)
		checkFileExistsWithContent(t, filepath.Join(tmpDir, "status"), "message=My final message!") // install file was executed in correct folder
	})

	t.Run("Test_with_correct_install_dirs", func(t *testing.T) {
		artifacts := []*hawkbit.SoftwareArtifactAction{
			convertLocalArtifact(installScriptAlias, installScriptAlias, installScriptHash, len(installScriptBody)),
			convertLocalArtifact(getAbsolutePath(t, localResourcePath), localResourceAlias, localResourceHash, len(localResourceBody)),
		}
		testScriptBasedSoftwareUpdatableOperationsLocal(t, []string{tmpDir}, artifacts, modeLax, "*", true)
		testScriptBasedSoftwareUpdatableOperationsLocal(t, []string{tmpDir}, artifacts, modeStrict, "*", true)
	})

	t.Run("Test_with_no_install_dirs", func(t *testing.T) {
		artifacts := []*hawkbit.SoftwareArtifactAction{
			convertLocalArtifact(installScriptPath, installScriptAlias, installScriptHash, len(installScriptBody)),
			convertLocalArtifact(getAbsolutePath(t, localResourcePath), localResourceAlias, localResourceHash, len(localResourceBody)),
		}
		testScriptBasedSoftwareUpdatableOperationsLocal(t, []string{}, artifacts, modeLax, "*", true)
		testScriptBasedSoftwareUpdatableOperationsLocal(t, []string{}, artifacts, modeStrict, "*", false)
		testScriptBasedSoftwareUpdatableOperationsLocal(t, []string{}, artifacts, modeScoped, "*", false)
	})

	t.Run("Test_with_incorrect_install_dirs", func(t *testing.T) {
		artifacts := []*hawkbit.SoftwareArtifactAction{
			convertLocalArtifact(getAbsolutePath(t, installScriptPath), installScriptAlias, installScriptHash, len(installScriptBody)),
		}

		installDirs := []string{filepath.Join(tmpDir, "test")}
		assertDirs(t, installDirs[0], true)
		testScriptBasedSoftwareUpdatableOperationsLocal(t, installDirs, artifacts, modeLax, "*", true)
		testScriptBasedSoftwareUpdatableOperationsLocal(t, installDirs, artifacts, modeScoped, "*", false)
	})
}

func testScriptBasedSoftwareUpdatableOperationsLocal(t *testing.T, installDirs []string,
	artifacts []*hawkbit.SoftwareArtifactAction, mode string, copyArtifacts string, expectedSuccess bool) {
	// Prepare
	dir := assertDirs(t, testDirFeature, false)
	// Remove temporary directory at the end.
	defer os.RemoveAll(dir)

	feature, mc, err := mockScriptBasedSoftwareUpdatable(t, &testConfig{
		clientConnected: true, featureID: getDefaultFlagValue(t, flagFeatureID), storageLocation: dir,
		installDirs: installDirs, mode: mode})
	if err != nil {
		t.Fatalf("failed to initialize ScriptBasedSoftwareUpdatable: %v", err)
	}
	defer feature.Disconnect(true)

	testDownloadInstall(feature, mc, artifacts, expectedSuccess, copyArtifacts, t)
}

func pullStatusChanges(mc *mockedClient, expectedCount int) []interface{} {
	var statuses []interface{}
	for i := 0; i < expectedCount; i++ {
		lo := mc.pullLastOperationStatus()
		statuses = append(statuses, lo)
		if lo["status"] == string(hawkbit.StatusFinishedSuccess) || lo["status"] == string(hawkbit.StatusFinishedError) {
			break
		}
	}
	return statuses
}

func testDownloadInstall(feature *ScriptBasedSoftwareUpdatable, mc *mockedClient, artifacts []*hawkbit.SoftwareArtifactAction,
	expectedSuccess bool, copyArtifacts string, t *testing.T) {
	sua := prepareSoftwareUpdateAction(artifacts, copyArtifacts)

	extraDownloadingEventsCount := getCopiedArtifactsCount(artifacts, copyArtifacts)
	if extraDownloadingEventsCount > 0 {
		extraDownloadingEventsCount-- // 1 artifact expected in default case
	}

	// Try to execute a simple download operation.
	feature.downloadHandler(sua, feature.su)

	statuses := pullStatusChanges(mc, 5+extraDownloadingEventsCount) // STARTED, DOWNLOADING, DOWNLOADING(x extraDownloadingEventsCount), DOWNLOADING(100), DOWNLOADED, FINISHED_SUCCESS
	if expectedSuccess {
		checkDownloadStatusEvents(extraDownloadingEventsCount, statuses, t)
		if copyArtifacts == "" {
			if !checkNoFilesCopied(t, filepath.Join(testDirFeature, "download", "0", "0"), true) {
				checkNoFilesCopied(t, filepath.Join(testDirFeature, "modules", "0"), false)
			}
		}
	} else {
		checkDownloadFailedStatusEvents(statuses, t)
	}

	// Try to execute a simple install operation.
	feature.installHandler(sua, feature.su)

	statuses = pullStatusChanges(mc, 8+extraDownloadingEventsCount) // STARTED, DOWNLOADING, DOWNLOADING(x extraDownloadingEventsCount), DOWNLOADING(100), DOWNLOADED,
	// INSTALLING, INSTALLING(100), INSTALLED, FINISHED_SUCCESS
	if expectedSuccess {
		checkInstallStatusEvents(extraDownloadingEventsCount, statuses, t)
	} else {
		checkDownloadFailedStatusEvents(statuses, t)
	}
}

func createStatus(status hawkbit.Status, progress func(float64) bool, message string) map[string]interface{} {
	result := make(map[string]interface{})
	result[statusParam] = string(status)
	result[messageParam] = message
	if progress != nil {
		result[progressParam] = progress
	}
	return result
}

func checkDownloadFailedStatusEvents(actualStatuses []interface{}, t *testing.T) {
	var expectedStatuses []interface{}
	expectedStatuses = append(expectedStatuses,
		createStatus(hawkbit.StatusStarted, nil, noMessage),
		createStatus(hawkbit.StatusDownloading, nil, noMessage),
		createStatus(hawkbit.StatusFinishedError, nil, anyErrorMessage),
	)
	checkStatusEvents(expectedStatuses, actualStatuses, t)
}

func checkDownloadStatusEvents(extraDownloadingEventsCount int, actualStatuses []interface{}, t *testing.T) {
	var expectedStatuses []interface{}
	expectedStatuses = append(expectedStatuses,
		createStatus(hawkbit.StatusStarted, nil, noMessage),
		createStatus(hawkbit.StatusDownloading, nil, noMessage),
	)
	for i := 0; i < extraDownloadingEventsCount; i++ {
		expectedStatuses = append(expectedStatuses, createStatus(hawkbit.StatusDownloading, partialDownload, noMessage))
	}
	expectedStatuses = append(expectedStatuses,
		createStatus(hawkbit.StatusDownloading, completeDownload, noMessage),
		createStatus(hawkbit.StatusDownloaded, completeDownload, noMessage),
		createStatus(hawkbit.StatusFinishedSuccess, nil, noMessage),
	)
	checkStatusEvents(expectedStatuses, actualStatuses, t)
}

func checkInstallStatusEvents(extraDownloadingEventsCount int, actualStatuses []interface{}, t *testing.T) {
	var expectedStatuses []interface{}
	expectedStatuses = append(expectedStatuses,
		createStatus(hawkbit.StatusStarted, nil, noMessage),
		createStatus(hawkbit.StatusDownloading, nil, noMessage),
	)
	for i := 0; i < extraDownloadingEventsCount; i++ {
		expectedStatuses = append(expectedStatuses, createStatus(hawkbit.StatusDownloading, partialDownload, noMessage))
	}
	expectedStatuses = append(expectedStatuses,
		createStatus(hawkbit.StatusDownloading, completeDownload, noMessage),
		createStatus(hawkbit.StatusDownloaded, completeDownload, noMessage),
		createStatus(hawkbit.StatusInstalling, nil, noMessage),
		createStatus(hawkbit.StatusInstalling, nil, "My final message!"),
		createStatus(hawkbit.StatusInstalled, nil, "My final message!"),
		createStatus(hawkbit.StatusFinishedSuccess, nil, "My final message!"),
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
			t.Fatalf("received unexpected lastOperation status: %v != %v(actual)", expected[statusParam], actual[statusParam])
		}
		checkStatusParameter(expected[progressParam], actual, progressParam,
			expected[progressParam] == nil, t)
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
		if checkProgressFunc, ok := expectedParamValue.(func(float64) bool); ok {
			if !checkProgressFunc(receivedValue.(float64)) {
				t.Fatalf("received unacceptable lastOperation %s: %v", name, receivedValue)
			}
		} else if expectedParamValue != anyErrorMessage && expectedParamValue != receivedValue {
			t.Fatalf("received unexpected lastOperation %s: %v != %v", name, receivedValue, expectedParamValue)
		}
	} else if !noValue {
		t.Fatalf("no %s found in payload: %v", name, actualStatus)
	}
}

func prepareSoftwareUpdateAction(artifacts []*hawkbit.SoftwareArtifactAction, copyArtifacts string) *hawkbit.SoftwareUpdateAction {
	// Prepare simple software update action.
	return &hawkbit.SoftwareUpdateAction{
		CorrelationID: "test-correlation-id",
		SoftwareModules: []*hawkbit.SoftwareModuleAction{{
			SoftwareModule: &hawkbit.SoftwareModuleID{Name: "test", Version: "1.0.0"},
			Artifacts:      artifacts,
			Metadata:       map[string]string{"artifact-type": typePlain, "copy-artifacts": copyArtifacts},
		}},
	}
}
