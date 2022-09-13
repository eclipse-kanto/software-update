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
	"testing"
	"time"

	"github.com/eclipse-kanto/software-update/hawkbit"
)

const (
	testAllValid                         = "All parameters valid"
	testMissingMessage                   = "Missing message only"
	testMissingStatusCode                = "Missing status code only"
	testMissingProgress                  = "Missing progress only"
	testProgressOutOfRange               = "Progress out of range"
	testProgressWithSpacesAtBegin        = "Progress with spaces at begin"
	testProgressWithSpacesBetween        = "Progress with spaces between numbers"
	testProgressWithSpacesLeadTrail      = "Progress with spaces at begin and end"
	testUnknownKey                       = "Unknown key with valid key"
	testEmptyFile                        = "Empty file"
	testMissingValidParameters           = "Missing valid parameters"
	testWrongParametersWithNewLines      = "Wrong parameters with new lines"
	testNoLineFeed                       = "No LF at the end of the file"
	testCariageReturnWithLineFeed        = "CR LF at the end of the file"
	testMultipleCariageReturnAndLineFeed = "Multiple CR LF at the end of the file"
	testEmptyRowsBetweenParameters       = "Empty rows between each parameter"

	testType          = "my-type"
	testCid           = "correlation-id"
	testModuleName    = "testName"
	testModuleVersion = "moduleVersion"

	testDirStatus  = "_tmp-status"
	testStatusFile = "status"
)

// TestUpdate function is used to test the functionality of the status
// file monitor watcher when valid file is created and updated.
func TestUpdate(t *testing.T) {
	su, _ := mockSoftwareUpdatable(t, hawkbit.NewConfiguration(), &testConfig{clientConnected: true})

	dir := assertDirs(t, testDirStatus, true)
	defer os.RemoveAll(dir)

	// write initial file with progress only
	err2 := ioutil.WriteFile(filepath.Join(dir, testStatusFile), []byte("progress=20"), 0755)
	if err2 != nil {
		t.Errorf("unable to create or write to temporary file: %v, reason: %v", testStatusFile, err2)
	}

	// create monitor object
	var monitor = &monitor{
		status: hawkbit.StatusInstalling,
		su:     su,
		cid:    testCid,
		module: &hawkbit.SoftwareModuleID{Name: testModuleName, Version: testModuleVersion},
	}

	// create channel to listen for status file changes events
	chanMon, err := monitor.waitFor(dir)
	if err != nil {
		t.Errorf("failed to start progress monitor: %v", err)
	}
	defer close(chanMon)

	// Write to file to generate file write event and complete the channel afterwards
	err3 := ioutil.WriteFile(filepath.Join(dir, testStatusFile), []byte("progress=35\nmessage=AlmostDone\nstatusCode=-1\n"), 0755)
	if err3 != nil {
		t.Errorf("unable to create or write to temporary file: %v, reason: %v", testStatusFile, err3)
	}

	// give a time to the watcher update routine
	time.Sleep(2 * time.Second)

	// Ensure the watcher routine finished
	chanMon <- true

	assertEqualsMonitor(35, "AlmostDone", "-1", monitor.oldProgress, monitor.oldMessage, monitor.oldStatusCode, t)
}

// TestUpdateInvalidFolder function assert that the error is returned when update is called with missing status file
func TestUpdateInvalidFolder(t *testing.T) {
	dir := assertDirs(t, testDirStatus, false)
	// statusFolder not created initially
	monitorChan, err := (&monitor{
		status: hawkbit.StatusInstalling,
		su:     &hawkbit.SoftwareUpdatable{},
		cid:    testCid,
		module: &hawkbit.SoftwareModuleID{Name: testModuleName, Version: testModuleVersion},
	}).waitFor(dir)
	if err == nil {
		t.Error("expecting error when status file is not found.")
	}

	if monitorChan != nil {
		t.Errorf("unexpected creation of monitor channel when status file is missing: %v", monitorChan)
	}
}

// TestLoadInvalidFile validate the values returned from the load function when there is status file missing
func TestLoadInvalidFile(t *testing.T) {
	// statusFile not created initially
	progress, message, statusCode, done := load(testStatusFile)
	assertEqualsLoadParameters(0, "", "", false, progress, message, statusCode, done, t, "Test load status with missing file")
}

// TestLoad is used to assert the values returned when a status file is updated with invalid parameters,
// wrong or inconsistent parameter values or if it not ends with a mandatory LF char
func TestLoad(t *testing.T) {
	// Test 1 - All parameters are valid
	assertLoad(testAllValid, "progress=21\r\nstatusCode=-1\r\nmessage=TestMessage\r\n", true, t)

	// Test 2 - One of properties is missing
	assertLoad(testMissingMessage, "progress=21\nstatusCode=-1\n", true, t)
	assertLoad(testMissingStatusCode, "progress=21\nmessage=TestMessage\n", true, t)
	assertLoad(testMissingProgress, "statusCode=-1\nmessage=TestMessage\n", true, t)

	// Test 3 - Progress is out of range
	assertLoad(testProgressOutOfRange, "progress=300\nmessage=TestMessage\nstatusCode=300\n", false, t)

	// Test 4 - Progress with spaces at begin values
	assertLoad(testProgressWithSpacesAtBegin, "progress=    4   5\nmessage=TestMessage\nstatusCode=100\n", false, t)
	assertLoad(testProgressWithSpacesBetween, "progress=4   5\nmessage=TestMessage\nstatusCode=110\n", false, t)
	assertLoad(testProgressWithSpacesLeadTrail, "progress=   45   \nmessage=TestMessage\nstatusCode=110\n", true, t)

	// Test 5 - Test ignore the wrong/unknown key
	assertLoad(testUnknownKey, "test=20\r\nstatusCode=200\n", true, t)
	assertLoad(testWrongParametersWithNewLines, "progres=45\r\n", false, t)

	// Test 6 - Test empty lines
	assertLoad(testEmptyFile, "", false, t)
	assertLoad(testNoLineFeed, "statusMessage=TestMessage\nstatusCode=24", false, t)
	assertLoad(testCariageReturnWithLineFeed, "message=TestMessage\nstatusCode=24\r\n", true, t)
	assertLoad(testMultipleCariageReturnAndLineFeed, "message=TestMessage\r\n\r\n\r\n\r\n", true, t)
	assertLoad(testEmptyRowsBetweenParameters, "message=TestMessage\r\n\r\nstatusCode=-1\r\n\r\nprogress=23\r\n", true, t)
}

// assertLoad is a util function used for file content assertion by each rule and file content provided
func assertLoad(rule, fileContent string, shallPass bool, t *testing.T) {
	statusFile := assertDirs(t, testStatusFile, false)
	if err := ioutil.WriteFile(statusFile, []byte(fileContent), 0755); err != nil {
		t.Errorf("unable to create or write to temporary file: %v, reason: %v", statusFile, err)
	}
	defer os.Remove(statusFile)

	progress, message, statusCode, done := load(statusFile)

	// parameters order: progress, message, statusCode, done
	switch rule {
	case testAllValid:
		assertEqualsLoadParameters(21, "TestMessage", "-1", shallPass, progress, message, statusCode, done, t, rule)
	case testMissingMessage:
		assertEqualsLoadParameters(21, "", "-1", shallPass, progress, message, statusCode, done, t, rule)
	case testMissingStatusCode:
		assertEqualsLoadParameters(21, "TestMessage", "", shallPass, progress, message, statusCode, done, t, rule)
	case testMissingProgress:
		assertEqualsLoadParameters(0, "TestMessage", "-1", shallPass, progress, message, statusCode, done, t, rule)
	case testProgressOutOfRange:
		assertEqualsLoadParameters(300, "", "", shallPass, progress, message, statusCode, done, t, rule)
	case testProgressWithSpacesAtBegin:
		assertEqualsLoadParameters(0, "", "", shallPass, progress, message, statusCode, done, t, rule)
	case testProgressWithSpacesBetween:
		assertEqualsLoadParameters(0, "", "", shallPass, progress, message, statusCode, done, t, rule)
	case testProgressWithSpacesLeadTrail:
		assertEqualsLoadParameters(45, "TestMessage", "110", shallPass, progress, message, statusCode, done, t, rule)
	case testUnknownKey:
		assertEqualsLoadParameters(0, "", "200", shallPass, progress, message, statusCode, done, t, rule)
	case testEmptyFile:
		assertEqualsLoadParameters(0, "", "", shallPass, progress, message, statusCode, done, t, rule)
	case testMissingValidParameters:
		assertEqualsLoadParameters(0, "", "", shallPass, progress, message, statusCode, done, t, rule)
	case testWrongParametersWithNewLines:
		assertEqualsLoadParameters(0, "", "", shallPass, progress, message, statusCode, done, t, rule)
	case testNoLineFeed:
		assertEqualsLoadParameters(0, "", "24", shallPass, progress, message, statusCode, done, t, rule)
	case testCariageReturnWithLineFeed:
		assertEqualsLoadParameters(0, "TestMessage", "24", shallPass, progress, message, statusCode, done, t, rule)
	case testMultipleCariageReturnAndLineFeed:
		assertEqualsLoadParameters(0, "TestMessage", "", shallPass, progress, message, statusCode, done, t, rule)
	case testEmptyRowsBetweenParameters:
		assertEqualsLoadParameters(23, "TestMessage", "-1", shallPass, progress, message, statusCode, done, t, rule)
	default:
		// omitted
	}
}

// assertEqualsLoadParameters function compares the expected and actual results and return meaningful information
// about the rules which are failing. Parameters:
// ep (expected progress), em (expected message), esc (espected status code), ed (expected done), r (rule)
// ap (actual progress), am (actual message), asc (actual status code), ad (actual done)
func assertEqualsLoadParameters(ep int, em string, esc string, ed bool, ap int,
	am string, asc string, ad bool, t *testing.T, r string) {
	if ep != ap || em != am || esc != asc || ed != ad {
		t.Errorf("unmatching expected vs actual result for rule (%v): %v, %v, %v, %v vs %v, %v, %v, %v",
			r, ep, em, esc, ed, ap, am, asc, ad)
	}
}

// assertEqualsMonitor function compares the expected and actual results for the monitor object
func assertEqualsMonitor(ep int, em string, esc string, ap int,
	am string, asc string, t *testing.T) {
	if ep != ap || em != am || esc != asc {
		t.Errorf("unmatching expected vs actual result for monitor: %v, %v, %v vs %v, %v, %v",
			ep, em, esc, ap, am, asc)
	}
}
