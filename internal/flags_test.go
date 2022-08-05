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
	"flag"
	"io/ioutil"
	"os"
	"strconv"
	"testing"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

const (
	// testFirstArg is used to specify the mandatory initial argument when setting manually the flags
	testFirstArg       = "cmd"
	testConfigFileName = "test_config.txt"
	testDirFlags       = "_tmp-flags"
	testConfigFilePath = testDirFlags + "/" + testConfigFileName
)

// TestInstallCommandFlag tests the initialization with flags when install command is provided
// with flag, but also in config file. Assert that the flag has higher priority.
func TestInstallCommandFlag(t *testing.T) {
	notExpectedInstallCMD := "/home/install"
	notExpectedInstallArgs := "-wrongArgument"

	expectedInstallCMD := "/test/test/install"
	expectedInstallArgs := "-testArgument"

	// Prepare test default dir
	dir := assertPath(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	content := "{\"install\": [\"" + notExpectedInstallCMD + "\",\"" + notExpectedInstallArgs + "\"]}"
	writeToConfigFile(t, content)

	setFlags([]string{c(flagConfigFile, testConfigFilePath),
		c(flagInstall, expectedInstallCMD),
		c(flagInstall, expectedInstallArgs)})

	assertInstallCommand(t, expectedInstallCMD, expectedInstallArgs)
}

// TestInstallCommandConfig tests the initialization with flags, when install command is provided only
// in config file.
func TestInstallCommandConfig(t *testing.T) {
	expectedInstallCMD := "/home/pi/install"
	expectedInstallArgs := "-exampleArgument"

	// Prepare test default dir
	dir := assertPath(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	// 1. Test with Install command not set with flag
	content := "{\"install\": [\"" + expectedInstallCMD + "\",\"" + expectedInstallArgs + "\"]}"
	writeToConfigFile(t, content)

	setFlags([]string{c(flagConfigFile, testConfigFilePath)})

	assertInstallCommand(t, expectedInstallCMD, expectedInstallArgs)
}

// TestFlagsHasHigherPriority tests initializing with flags when all properties except installCommand are
// provided via flags, when config file is used. Assert that the flags has higher priority
func TestFlagsHasHigherPriority(t *testing.T) {
	expectedResult := "DEBUG"

	// Prepare test default dir
	dir := assertPath(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	// 1. Test with log field
	writeToConfigFile(t, "{\""+flagLogLevel+"\": \"TRACE\"}")
	setFlags([]string{c(flagConfigFile, testConfigFilePath), c(flagLogLevel, expectedResult)})
	_, lc, err := InitFlags(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing flags with log level: %v", err)
	}

	if lc.LogLevel != expectedResult {
		t.Errorf("unmatching result for flags high priority, found %v instead of %v", lc.LogLevel, expectedResult)
	}

	expectedResult = "FeatureTestUpdatable"
	// 2. Test with software updatable config field
	writeToConfigFile(t, "{\""+flagFeatureID+"\": \"WrongFeatureID\"}")
	setFlags([]string{c(flagConfigFile, testConfigFilePath), c(flagFeatureID, expectedResult)})
	sc, _, err := InitFlags(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing flags with featureId: %v", err)
	}

	if sc.FeatureID != expectedResult {
		t.Errorf("unmatching result for flags high priority, found %v instead of %v", sc.FeatureID, expectedResult)
	}

	// 3. Test with all flags are applied instead of default values if no configuration JSON is provided
	expectedFlagBroker := "host:1234"
	expectedArtifact := "TestArchive"
	expectedFeatureID := "TestFeature"
	expectedInstall := "TestInstall"
	expectedCert := "TestCert"
	expectedKey := "TestKey"
	expectedLogFile := ""
	expectedLogFileCount := 4
	expectedLogFileMaxAge := 13
	expectedLogFileSize := 22
	expectedLogLevel := "TRACE"
	expectedModuleType := "TestSoftwareModule"
	expectedPassword := "TestPass"
	expectedStorageLocation := "TestLocation"
	expectedUsername := "TestUser"
	expectedPrintVersion := "false"

	setFlags([]string{
		c(flagBroker, expectedFlagBroker),
		c(flagArtifactType, expectedArtifact),
		c(flagFeatureID, expectedFeatureID),
		c(flagInstall, expectedInstall),
		c(flagCert, expectedCert),
		c(flagKey, expectedKey),
		c(flagLogFile, expectedLogFile),
		c(flagLogFileCount, strconv.Itoa(expectedLogFileCount)),
		c(flagLogFileMaxAge, strconv.Itoa(expectedLogFileMaxAge)),
		c(flagLogFileSize, strconv.Itoa(expectedLogFileSize)),
		c(flagLogLevel, expectedLogLevel),
		c(flagModuleType, expectedModuleType),
		c(flagPassword, expectedPassword),
		c(flagStorageLocation, expectedStorageLocation),
		c(flagUsername, expectedUsername),
		c(flagVersion, expectedPrintVersion),
	})

	sc, lc, err = InitFlags(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing flags with featureId: %v", err)
	}

	assertSuConfigEqual(sc, &ScriptBasedSoftwareUpdatableConfig{
		Broker:          expectedFlagBroker,
		Username:        expectedUsername,
		Password:        expectedPassword,
		Cert:            expectedCert,
		Key:             expectedKey,
		StorageLocation: expectedStorageLocation,
		FeatureID:       expectedFeatureID,
		ModuleType:      expectedModuleType,
		ArtifactType:    expectedArtifact,
	})

	assertLogConfigEqual(lc, &logger.LogConfig{
		LogFile:       expectedLogFile,
		LogLevel:      expectedLogLevel,
		LogFileSize:   expectedLogFileSize,
		LogFileCount:  expectedLogFileCount,
		LogFileMaxAge: expectedLogFileMaxAge,
	})
}

// TestInitFlagsWithPrintVersion tests that initialization with flags prints version and exit
// when flag version is specified. Since the program exits, this test expects the InitFlags func to panic.
func TestInitFlagsWithPrintVersion(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Errorf("expected to print the version and exit")
		}
	}()
	setFlags([]string{c(flagVersion, "true")})
	InitFlags(testVersion)
}

// TestInitFlagWithInvalidConfigFile tests the behaviour when wrong JSON config file is supplied.
func TestInitFlagWithInvalidConfigFile(t *testing.T) {
	// Prepare test default dir
	dir := assertPath(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	//1. Test with JSON which is not starting with leading "{" character
	writeToConfigFile(t, "\"Broker\": \"tcp://host:1234\"}")
	setFlags([]string{c(flagConfigFile, testConfigFilePath)})
	sc, lc, err := InitFlags(testVersion)
	assertInitFlagsFails(t, sc, lc, err, "expecting init flags to fail if JSON format of the config file is not valid")

	//2. Test with JSON using string instead of integer for config field value
	writeToConfigFile(t, "{\"Broker\": \"tcp://host:1234\", \"LogFileSize\": \"20\"}")
	setFlags([]string{c(flagConfigFile, testConfigFilePath)})
	sc, lc, err = InitFlags(testVersion)
	assertInitFlagsFails(t, sc, lc, err, "expecting init flags to fail if provide int value to an string field")

	//3. Test with JSON using integer instead of string for config field value
	writeToConfigFile(t, "{\"Broker\": \"tcp://host:1234\", \"Username\": 200}")
	setFlags([]string{c(flagConfigFile, testConfigFilePath)})
	sc, lc, err = InitFlags(testVersion)
	assertInitFlagsFails(t, sc, lc, err, "expecting init flags to fail if provide string value to an int field")
}

// TestInitFlagsWithMissingConfigFile tests that the error is returned when missing config file is
// specified with flag.
func TestInitFlagsWithMissingConfigFile(t *testing.T) {
	setFlags([]string{c(flagConfigFile, "TestLocation")})
	sc, lc, err := InitFlags(testVersion)
	assertInitFlagsFails(t, sc, lc, err, "expecting error when initializing with missing config file flag")
}

// TestInitFlagsConfigAllPropertiesProvided verifies that all of the properties from the configuration file are set
func TestInitFlagsConfigAllPropertiesProvided(t *testing.T) {
	// Prepare test default dir
	dir := assertPath(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	content := "{\"Broker\": \"tcp://host:1234\",\"Username\": \"TestUser\",\"Password\": \"TestPass\",\"StorageLocation\": \"_tmp-flags\",\"FeatureID\": \"SoftwareTestUpdatable\",\"ModuleType\": \"TestSoftware\",\"ArtifactType\": \"TestArchive\",\"LogFile\": \"TestLogFile.txt\",\"LogLevel\": \"TRACE\",\"LogFileSize\": 10,\"LogFileCount\": 20,\"LogFileMaxAge\": 30}"
	writeToConfigFile(t, content)

	setFlags([]string{c(flagConfigFile, testConfigFilePath)})

	expectedConfig := &ScriptBasedSoftwareUpdatableConfig{
		Broker:          "tcp://host:1234",
		FeatureID:       "SoftwareTestUpdatable",
		ArtifactType:    "TestArchive",
		ModuleType:      "TestSoftware",
		StorageLocation: dir,
		Username:        "TestUser",
		Password:        "TestPass",
	}

	expectedLogConfig := &logger.LogConfig{
		LogFile:       "TestLogFile.txt",
		LogLevel:      "TRACE",
		LogFileSize:   10,
		LogFileCount:  20,
		LogFileMaxAge: 30,
	}

	compareConfigResult(t, expectedConfig, expectedLogConfig)
}

// TestWithEmptyConfigFile verifies if empty JSON configuration file is provided.
// Assert that error is returned.
func TestWithEmptyConfigFile(t *testing.T) {
	// Prepare test default dir
	dir := assertPath(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	writeToConfigFile(t, "")
	setFlags([]string{c(flagConfigFile, testConfigFilePath)})

	_, _, err := InitFlags(testVersion)
	if err == nil {
		t.Errorf("expected to fail when empty config file is supplied")
	}
}

// TestInitFlagsWithConfigMixedContent verifies if some of the configuration properties are provided
// and those who are not, are used from default values
func TestInitFlagsWithConfigMixedContent(t *testing.T) {
	// Prepare test default dir
	dir := assertPath(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	content := "{\"broker\": \"tcp://host:12345\",\"logLevel\": \"TRACE\",\"logFile\": \"test_log.txt\",\"username\": \"test\"}"
	writeToConfigFile(t, content)
	setFlags([]string{c(flagConfigFile, testConfigFilePath)})

	expectedConfig := &ScriptBasedSoftwareUpdatableConfig{
		Broker:          "tcp://host:12345",
		FeatureID:       defaultFeatureID,
		ArtifactType:    defaultArtifactType,
		ModuleType:      defaultModuleType,
		StorageLocation: defaultStorageLocation,
		Username:        "test",
		Password:        defaultPassword,
	}

	expectedLogConfig := &logger.LogConfig{
		LogFile:       "test_log.txt",
		LogLevel:      "TRACE",
		LogFileSize:   defaultLogFileSize,
		LogFileCount:  defaultLogFileCount,
		LogFileMaxAge: defaultLogFileMaxAge,
	}

	compareConfigResult(t, expectedConfig, expectedLogConfig)
}

// compareConfigResult function verifies the content of the expected and actual configuration struct
func compareConfigResult(t *testing.T, expectedConfig *ScriptBasedSoftwareUpdatableConfig, expectedLogConfig *logger.LogConfig) {
	suConfig, logConfig, err := InitFlags(testVersion)

	if suConfig == nil || logConfig == nil || err != nil {
		t.Error("failed to init flags with valid configuration file: ", err)
	}

	if !assertSuConfigEqual(expectedConfig, suConfig) || !assertLogConfigEqual(expectedLogConfig, logConfig) {
		t.Errorf("configurations does not match, suConfig: %v != %v or logConfig: %v != %v ",
			expectedConfig, suConfig, expectedLogConfig, logConfig)
	}
}

func assertLogConfigEqual(e *logger.LogConfig, a *logger.LogConfig) bool {
	// Sonar: Reduce the number of conditional operators used in the expression (maximum allowed 3).
	c := e.LogFile == a.LogFile && e.LogFileCount == a.LogFileCount && e.LogFileMaxAge == a.LogFileMaxAge
	return c && e.LogFileSize == a.LogFileSize && e.LogLevel == a.LogLevel
}

func assertSuConfigEqual(e *ScriptBasedSoftwareUpdatableConfig, a *ScriptBasedSoftwareUpdatableConfig) bool {
	// Sonar: Reduce the number of conditional operators used in the expression (maximum allowed 3).
	c1 := e.Broker == a.Broker && a.Password == e.Password && a.Username == e.Username
	c2 := a.FeatureID == e.FeatureID && a.ModuleType == e.ModuleType && e.ArtifactType == a.ArtifactType
	return c1 && c2 && a.StorageLocation == e.StorageLocation
}

func assertInitFlagsFails(t *testing.T, suConfig *ScriptBasedSoftwareUpdatableConfig,
	logConfig *logger.LogConfig, err error, msg string) {
	if suConfig != nil || logConfig != nil || err == nil {
		t.Error(msg)
	}
}

// assertInstallCommand verifies the result when initializing flags with install command,
// which is specified with config file or flag
func assertInstallCommand(t *testing.T, expectedInstallCMD string, expectedInstallArgs string) {
	sc, _, err := InitFlags(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing with install config: %v", err)
	}
	if len(sc.InstallCommand.args) != 1 {
		t.Error("expecting install command to be set")
	}
	if sc.InstallCommand.args[0] != expectedInstallArgs {
		t.Errorf("unmatching install command args, expected %v, actual %v", expectedInstallArgs, sc.InstallCommand.args)
	}
	if sc.InstallCommand.cmd != expectedInstallCMD {
		t.Errorf("unmatching install command path, expected %v, actual %v", expectedInstallCMD, sc.InstallCommand.cmd)
	}
}

// c function is used to Construct the name and value in flag format
func c(flagName string, flagValue string) string {
	return "-" + flagName + "=" + flagValue
}

// writeToConfigFile is used to write content to the config file
func writeToConfigFile(t *testing.T, content string) {
	if err := ioutil.WriteFile(testConfigFilePath, []byte(content), 0755); err != nil {
		t.Errorf("unable to create or write to temporary file: %v, reason: %v", testStatusFile, err)
	}
}

// setFlags function is used to add the custom test flags to the os.Args global variable
func setFlags(args []string) {
	resetArgs()
	os.Args = append([]string{testFirstArg}, args...)
}

// resetArgs function saves the current state of the arguments passed to tests for future use.
// Must be invoked before each test which is providing test argumets
func resetArgs() {
	// save the state of args before the test starts
	oldArgs := os.Args
	suConfig = &ScriptBasedSoftwareUpdatableConfig{}
	logConfig = &logger.LogConfig{}

	defer func() {
		// return the state of args for the next tests
		os.Args = oldArgs
		// reset the flags before each test to avoid flag redefined panic
		flag.CommandLine = flag.NewFlagSet(os.Args[0], flag.ExitOnError)
	}()
}
