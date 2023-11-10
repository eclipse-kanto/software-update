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

//go:build unit

package feature

import (
	"flag"
	"io/ioutil"
	"os"
	"reflect"
	"strconv"
	"testing"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

const (
	// testFirstArg is used to specify the mandatory initial argument when setting manually the flags
	testFirstArg       = "cmd"
	testConfigFileName = "test_config.txt"
	testDirFlags       = "_tmp-flags"
	testConfigFileDirs = testDirFlags + "/" + testConfigFileName
)

// TestInstallCommandFlag tests the initialization with flags when install command is provided
// with flag, but also in config file. Assert that the flag has higher priority.
func TestInstallCommandFlag(t *testing.T) {
	notExpectedInstallCMD := "/home/install"
	notExpectedInstallArgs := "-wrongArgument"

	expectedInstallCMD := "/test/test/install"
	expectedInstallArgs := "-testArgument"

	// Prepare test default dir
	dir := assertDirs(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	content := "{\"install\": [\"" + notExpectedInstallCMD + "\",\"" + notExpectedInstallArgs + "\"]}"
	writeToConfigFile(t, content)

	setFlags([]string{c(flagConfigFile, testConfigFileDirs),
		c(flagInstall, expectedInstallCMD),
		c(flagInstall, expectedInstallArgs)})

	assertInstallCommand(t, expectedInstallCMD, expectedInstallArgs)
}

// TestInstallDirsCommandFlag tests the initialization with flags when install directories are provided
// with flag, but also in config file. Assert that the flag has higher priority.
func TestInstallDirsCommandFlag(t *testing.T) {
	notExpectedDir := "dir1"
	expectedDir := "dir2"

	// Prepare test default dir
	dir := assertDirs(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	content := "{\"installDirs\": [\"" + notExpectedDir + "\"]}"
	writeToConfigFile(t, content)

	setFlags([]string{c(flagConfigFile, testConfigFileDirs),
		c(flagInstallDirs, expectedDir)})

	assertInstallDirs(t, expectedDir)
}

// TestInstallCommandConfig tests the initialization with flags, when install command is provided only
// in config file.
func TestInstallCommandConfig(t *testing.T) {
	expectedInstallCMD := "/home/pi/install"
	expectedInstallArgs := "-exampleArgument"

	// Prepare test default dir
	dir := assertDirs(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	// 1. Test with Install command not set with flag
	content := "{\"install\": [\"" + expectedInstallCMD + "\",\"" + expectedInstallArgs + "\"]}"
	writeToConfigFile(t, content)

	setFlags([]string{c(flagConfigFile, testConfigFileDirs)})

	assertInstallCommand(t, expectedInstallCMD, expectedInstallArgs)
}

// TestFlagsHasHigherPriority tests initializing with flags when all properties except installCommand are
// provided via flags, when config file is used. Assert that the flags has higher priority
func TestFlagsHasHigherPriority(t *testing.T) {
	expectedResult := "DEBUG"

	// Prepare test default dir
	dir := assertDirs(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	// 1. Test with log field
	writeToConfigFile(t, "{\""+flagLogLevel+"\": \"TRACE\"}")
	setFlags([]string{c(flagConfigFile, testConfigFileDirs), c(flagLogLevel, expectedResult)})
	cfg, err := LoadConfig(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing flags with log level: %v", err)
	}

	if cfg.LogLevel != expectedResult {
		t.Errorf("unmatching result for flags high priority, found %v instead of %v", cfg.LogLevel, expectedResult)
	}

	expectedResult = "FeatureTestUpdatable"
	// 2. Test with software updatable config field
	writeToConfigFile(t, "{\""+flagFeatureID+"\": \"WrongFeatureID\"}")
	setFlags([]string{c(flagConfigFile, testConfigFileDirs), c(flagFeatureID, expectedResult)})
	cfg, err = LoadConfig(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing flags with featureId: %v", err)
	}

	if cfg.FeatureID != expectedResult {
		t.Errorf("unmatching result for flags high priority, found %v instead of %v", cfg.FeatureID, expectedResult)
	}

	// 3. Test with all flags are applied instead of default values if no configuration JSON is provided
	expectedFlagBroker := "host:1234"
	expectedArtifact := "TestArchive"
	expectedCACert := "TestCaCert.crt"
	expectedCert := "TestCert.cert"
	expectedKey := "TestKey.key"
	expectedFeatureID := "TestFeature"
	expectedInstall := "TestInstall"
	expectedServerCert := "TestCert"
	expectedDownloadRetryCount := 3
	expectedDownloadRetryInterval := "5s"
	expectedInstallDir := "/var/tmp/storage"
	expectedMode := "lax"
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
		c(flagCACert, expectedCACert),
		c(flagCert, expectedCert),
		c(flagKey, expectedKey),
		c(flagFeatureID, expectedFeatureID),
		c(flagInstall, expectedInstall),
		c(flagServerCert, expectedServerCert),
		c(flagRetryCount, strconv.Itoa(expectedDownloadRetryCount)),
		c(flagRetryInterval, expectedDownloadRetryInterval),
		c(flagInstallDirs, expectedInstallDir),
		c(flagMode, expectedMode),
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

	cfg, err = LoadConfig(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing flags with featureId: %v", err)
	}

	assertSoftwareUpdatable(t, cfg.ScriptBasedSoftwareUpdatableConfig, ScriptBasedSoftwareUpdatableConfig{
		Broker:                expectedFlagBroker,
		Username:              expectedUsername,
		Password:              expectedPassword,
		CACert:                expectedCACert,
		Cert:                  expectedCert,
		Key:                   expectedKey,
		ServerCert:            expectedServerCert,
		StorageLocation:       expectedStorageLocation,
		InstallCommand:        Command{cmd: expectedInstall},
		DownloadRetryCount:    expectedDownloadRetryCount,
		DownloadRetryInterval: getDurationTime(t, expectedDownloadRetryInterval),
		InstallDirs:           []string{expectedInstallDir},
		Mode:                  expectedMode,
		FeatureID:             expectedFeatureID,
		ModuleType:            expectedModuleType,
		ArtifactType:          expectedArtifact,
	})

	assertLogConfig(t, cfg.LogConfig, logger.LogConfig{
		LogFile:       expectedLogFile,
		LogLevel:      expectedLogLevel,
		LogFileSize:   expectedLogFileSize,
		LogFileCount:  expectedLogFileCount,
		LogFileMaxAge: expectedLogFileMaxAge,
	})
}

// TestLoadConfigWithPrintVersion tests that initialization with flags prints version and exit
// when flag version is specified. Since the program exits, this test expects the LoadConfig func to panic.
func TestLoadConfigWithPrintVersion(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Errorf("expected to print the version and exit")
		}
	}()
	setFlags([]string{c(flagVersion, "true")})
	LoadConfig(testVersion)
}

// TestLoadConfigWithInvalidConfigFile tests the behavior when wrong JSON config file is supplied.
func TestLoadConfigWithInvalidConfigFile(t *testing.T) {
	// Prepare test default dir
	dir := assertDirs(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	//1. Test with JSON which is not starting with leading "{" character
	writeToConfigFile(t, "\"Broker\": \"tcp://host:1234\"}")
	setFlags([]string{c(flagConfigFile, testConfigFileDirs)})
	assertLoadConfigFails(t, "expecting init flags to fail if JSON format of the config file is not valid")

	//2. Test with JSON using string instead of integer for config field value
	writeToConfigFile(t, "{\"Broker\": \"tcp://host:1234\", \"LogFileSize\": \"20\"}")
	setFlags([]string{c(flagConfigFile, testConfigFileDirs)})
	assertLoadConfigFails(t, "expecting init flags to fail if provide int value to an string field")

	//3. Test with JSON using integer instead of string for config field value
	writeToConfigFile(t, "{\"Broker\": \"tcp://host:1234\", \"Username\": 200}")
	setFlags([]string{c(flagConfigFile, testConfigFileDirs)})
	assertLoadConfigFails(t, "expecting init flags to fail if provide string value to an int field")
}

// TestLoadConfigWithMissingConfigFile tests that the error is returned when missing config file is
// specified with flag.
func TestLoadConfigWithMissingConfigFile(t *testing.T) {
	setFlags([]string{c(flagConfigFile, "TestLocation")})
	assertLoadConfigFails(t, "expecting error when initializing with missing config file flag")
}

// TestLoadConfigConfigAllPropertiesProvided verifies that all of the properties from the configuration file are set
func TestLoadConfigConfigAllPropertiesProvided(t *testing.T) {
	// Prepare test default dir
	dir := assertDirs(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	content := "{\"Broker\": \"tcp://host:1234\",\"Username\": \"TestUser\",\"Password\": \"TestPass\",\"StorageLocation\": \"_tmp-flags\",\"FeatureID\": \"SoftwareTestUpdatable\",\"ModuleType\": \"TestSoftware\",\"ArtifactType\": \"TestArchive\"," +
		"\"DownloadRetryInterval\":\"7s\", \"Mode\":\"Scoped\", \"LogFile\": \"TestLogFile.txt\",\"LogLevel\": \"TRACE\",\"LogFileSize\": 10,\"LogFileCount\": 20,\"LogFileMaxAge\": 30}"
	writeToConfigFile(t, content)

	setFlags([]string{c(flagConfigFile, testConfigFileDirs)})

	expectedConfig := NewDefaultConfig()
	expectedConfig.Broker = "tcp://host:1234"
	expectedConfig.FeatureID = "SoftwareTestUpdatable"
	expectedConfig.ArtifactType = "TestArchive"
	expectedConfig.ModuleType = "TestSoftware"
	expectedConfig.StorageLocation = dir
	expectedConfig.Username = "TestUser"
	expectedConfig.Password = "TestPass"
	expectedConfig.DownloadRetryInterval = getDurationTime(t, "7s")
	expectedConfig.Mode = "Scoped"

	expectedConfig.LogFile = "TestLogFile.txt"
	expectedConfig.LogLevel = "TRACE"
	expectedConfig.LogFileSize = 10
	expectedConfig.LogFileCount = 20
	expectedConfig.LogFileMaxAge = 30

	compareConfigResult(t, expectedConfig)
}

// TestWithEmptyConfigFile verifies if empty JSON configuration file is provided.
// Assert that error is returned.
func TestWithEmptyConfigFile(t *testing.T) {
	// Prepare test default dir
	dir := assertDirs(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	writeToConfigFile(t, "")
	setFlags([]string{c(flagConfigFile, testConfigFileDirs)})

	assertLoadConfigFails(t, "expected to fail when empty config file is supplied")
}

// TestLoadConfigWithConfigMixedContent verifies if some of the configuration properties are provided
// and those who are not, are used from default values
func TestLoadConfigWithConfigMixedContent(t *testing.T) {
	// Prepare test default dir
	dir := assertDirs(t, testDirFlags, true)
	defer os.RemoveAll(dir)

	content := "{\"broker\": \"tcp://host:12345\",\"logLevel\": \"TRACE\",\"logFile\": \"test_log.txt\",\"username\": \"test\"}"
	writeToConfigFile(t, content)
	setFlags([]string{c(flagConfigFile, testConfigFileDirs)})

	expectedConfig := NewDefaultConfig()
	expectedConfig.Broker = "tcp://host:12345"
	expectedConfig.Username = "test"

	expectedConfig.LogFile = "test_log.txt"
	expectedConfig.LogLevel = "TRACE"

	compareConfigResult(t, expectedConfig)
}

func TestInvalidAccessModeFlag(t *testing.T) {
	setFlags([]string{c(flagMode, "test"), c(flagFeatureID, "id")})
	cfg, err := LoadConfig(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing flags with invalid access mode: %v", err)
	}
	if err = cfg.Validate(); err == nil {
		t.Fatal("expecting error when validating configuration with invalid access mode flag")
	}
}

// compareConfigResult function verifies the content of the expected and actual configuration struct
func compareConfigResult(t *testing.T, expectedConfig *BasicConfig) {
	cfg, err := LoadConfig(testVersion)

	if err != nil {
		t.Error("failed to init flags with valid configuration file: ", err)
	}

	assertSoftwareUpdatable(t, cfg.ScriptBasedSoftwareUpdatableConfig, expectedConfig.ScriptBasedSoftwareUpdatableConfig)
	assertLogConfig(t, cfg.LogConfig, expectedConfig.LogConfig)
}

func assertString(t *testing.T, actual, expected string) {
	if expected != actual {
		t.Errorf("Expected string value %s, but received value %s", expected, actual)
	}
}

func assertInt(t *testing.T, actual, expected int) {
	if expected != actual {
		t.Errorf("Expected int value %d, but received value %d", expected, actual)
	}
}

func assertDeep(t *testing.T, actual, expected interface{}) {
	if !reflect.DeepEqual(expected, actual) {
		t.Errorf("Expected  %s, but received %s", expected, actual)
	}
}

func assertSoftwareUpdatable(t *testing.T, actual, expected ScriptBasedSoftwareUpdatableConfig) {
	assertString(t, actual.Broker, expected.Broker)
	assertString(t, actual.Username, expected.Username)
	assertString(t, actual.Password, expected.Password)
	assertString(t, actual.CACert, expected.CACert)
	assertString(t, actual.Cert, expected.Cert)
	assertString(t, actual.Key, expected.Key)
	assertString(t, actual.ServerCert, expected.ServerCert)
	assertString(t, actual.StorageLocation, expected.StorageLocation)
	assertInt(t, actual.DownloadRetryCount, expected.DownloadRetryCount)
	assertDeep(t, actual.DownloadRetryInterval, expected.DownloadRetryInterval)
	assertDeep(t, actual.InstallDirs, expected.InstallDirs)
	assertString(t, actual.Mode, expected.Mode)
	assertString(t, actual.FeatureID, expected.FeatureID)
	assertString(t, actual.ModuleType, expected.ModuleType)
	assertString(t, actual.ArtifactType, expected.ArtifactType)
}

func assertLogConfig(t *testing.T, actual, expected logger.LogConfig) {
	assertString(t, actual.LogFile, expected.LogFile)
	assertString(t, actual.LogLevel, expected.LogLevel)
	assertInt(t, actual.LogFileSize, expected.LogFileSize)
	assertInt(t, actual.LogFileCount, expected.LogFileCount)
	assertInt(t, actual.LogFileMaxAge, expected.LogFileMaxAge)
}

func assertLoadConfigFails(t *testing.T, msg string) {
	_, err := LoadConfig(testVersion)
	if err == nil {
		t.Error(msg)
	}
}

// assertInstallCommand verifies the result when initializing flags with install command,
// which is specified with config file or flag
func assertInstallCommand(t *testing.T, expectedInstallCMD string, expectedInstallArgs string) {
	cfg, err := LoadConfig(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing with install config: %v", err)
	}
	if len(cfg.InstallCommand.args) != 1 {
		t.Error("expecting install command to be set")
	}
	if cfg.InstallCommand.args[0] != expectedInstallArgs {
		t.Errorf("unmatching install command args, expected %v, actual %v", expectedInstallArgs, cfg.InstallCommand.args)
	}
	if cfg.InstallCommand.cmd != expectedInstallCMD {
		t.Errorf("unmatching install command path, expected %v, actual %v", expectedInstallCMD, cfg.InstallCommand.cmd)
	}
}

// assertInstallDirs verifies the result when initializing flags with install directories,
// which are specified with config file or flag
func assertInstallDirs(t *testing.T, expectedInstallDir string) {
	cfg, err := LoadConfig(testVersion)
	if err != nil {
		t.Errorf("not expecting error when initializing with install config: %v", err)
	}
	if len(cfg.InstallDirs) != 1 {
		t.Error("expecting install directories to be set")
	}
	if cfg.InstallDirs[0] != expectedInstallDir {
		t.Errorf("unmatching install directories args, expected %v, actual %v", expectedInstallDir, cfg.InstallCommand)
	}
}

// c function is used to Construct the name and value in flag format
func c(flagName string, flagValue string) string {
	return "-" + flagName + "=" + flagValue
}

// writeToConfigFile is used to write content to the config file
func writeToConfigFile(t *testing.T, content string) {
	if err := ioutil.WriteFile(testConfigFileDirs, []byte(content), 0755); err != nil {
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

	defer func() {
		// return the state of args for the next tests
		os.Args = oldArgs
		// reset the flags before each test to avoid flag redefined panic
		flag.CommandLine = flag.NewFlagSet(os.Args[0], flag.ExitOnError)
	}()
}

func getDurationTime(t *testing.T, defaultValue string) (result DurationTime) {
	err := result.Set(defaultValue)
	if err != nil {
		t.Fatalf("unable to get duration time from %s", defaultValue)
	}
	return
}
