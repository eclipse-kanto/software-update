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
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

const (
	flagConfigFile = "configFile"
	flagInstall    = "install"
)

var (
	modeDescription = "Artifact access mode. Restricts where local file system artifacts can be located.\nAllowed values are:" +
		"\n  'strict' - artifacts can only be located in directories, included in installDirs property value" +
		"\n  'scoped' - artifacts can only be located in directories or their subdirectories recursively, included in installDirs property value" +
		"\n  'lax' - artifacts can be located anywhere on local file system. Use with care!"
)

// InitFlags tries to initialize Script-Based SoftwareUpdatable and Log configurations.
// Returns true if version flag is specified for print version and exit. Returns error
// if JSON configuration file cannot be read properly or missing config file is specified with flag.
func InitFlags(flagSet *flag.FlagSet, cfg *BasicConfig) {
	// init log flags
	flagSet.StringVar(&cfg.LogLevel, "logLevel", cfg.LogLevel, "Log levels are ERROR, WARN, INFO, DEBUG, TRACE")
	flagSet.StringVar(&cfg.LogFile, "logFile", cfg.LogFile, "Log file location in storage directory")
	flagSet.IntVar(&cfg.LogFileSize, "logFileSize", cfg.LogFileSize, "Log file size in MB before it gets rotated")
	flagSet.IntVar(&cfg.LogFileCount, "logFileCount", cfg.LogFileCount, "Log file max rotations count")
	flagSet.IntVar(&cfg.LogFileMaxAge, "logFileMaxAge", cfg.LogFileMaxAge, "Log file rotations max age in days")

	// init connection flags
	flagSet.StringVar(&cfg.Broker, "broker", cfg.Broker, "Local MQTT broker address")
	flagSet.StringVar(&cfg.Username, "username", cfg.Username, "Username that is a part of the credentials")
	flagSet.StringVar(&cfg.Password, "password", cfg.Password, "Password that is a part of the credentials")
	flagSet.StringVar(&cfg.CACert, "caCert", cfg.CACert, "A PEM encoded CA certificates file for MQTT broker connection")
	flagSet.StringVar(&cfg.Cert, "cert", cfg.Cert, "A PEM encoded certificate file to authenticate to the MQTT server/broker")
	flagSet.StringVar(&cfg.Key, "key", cfg.Key, "A PEM encoded unencrypted private key file to authenticate to the MQTT server/broker")
	flagSet.StringVar(&cfg.StorageLocation, "storageLocation", cfg.StorageLocation, "Location of the storage")
	flagSet.StringVar(&cfg.FeatureID, "featureId", cfg.FeatureID, "Feature identifier of SoftwareUpdatable")
	flagSet.StringVar(&cfg.ModuleType, "moduleType", cfg.ModuleType, "Module type of SoftwareUpdatable")
	flagSet.StringVar(&cfg.ArtifactType, "artifactType", cfg.ArtifactType, "Defines the module artifact type: archive or plain")
	flagSet.StringVar(&cfg.ServerCert, "serverCert", cfg.ServerCert, "A PEM encoded certificate 'file' for secure artifact download")
	flagSet.IntVar(&cfg.DownloadRetryCount, "downloadRetryCount", cfg.DownloadRetryCount, "Number of retries, in case of a failed download. By default no retries are supported.")
	flagSet.DurationVar((*time.Duration)(&cfg.DownloadRetryInterval), "downloadRetryInterval", (time.Duration)(cfg.DownloadRetryInterval), "Interval between retries, in case of a failed download. Should be a sequence of decimal numbers, each with optional fraction and a unit suffix, such as '300ms', '1.5h', '10m30s', etc. Valid time units are 'ns', 'us' (or 'µs'), 'ms', 's', 'm', 'h'")

	flagSet.StringVar(&cfg.Mode, "mode", cfg.Mode, modeDescription)

	flagSet.Var(&cfg.InstallCommand, flagInstall, "Defines the absolute path to install script")
	flagSet.Var(newPathArgs(&cfg.InstallDirs), "installDirs", "Local file system directories, where to search for module artifacts")
	flagSet.StringVar(&cfg.ConfigFile, flagConfigFile, cfg.ConfigFile, "Defines the configuration file")
}

// ParseConfigFilePath returns the value for configuration file path if set.
func ParseConfigFilePath() string {
	var cfgFilePath string
	flagSet := flag.NewFlagSet("", flag.ContinueOnError)
	flagSet.SetOutput(io.Discard)
	flagSet.StringVar(&cfgFilePath, flagConfigFile, "", "Defines the configuration file")
	if err := flagSet.Parse(getFlagArgs(flagConfigFile)); err != nil {
		logger.Errorf("Cannot parse the configFile flag: %v", err)
	}
	return cfgFilePath
}

func getFlagArgs(flag string) []string {
	args := os.Args[1:]
	flag1 := "-" + flag
	flag2 := "--" + flag
	for index, arg := range args {
		if strings.HasPrefix(arg, flag1+"=") || strings.HasPrefix(arg, flag2+"=") {
			return []string{arg}
		}
		if (arg == flag1 || arg == flag2) && index < len(args)-1 {
			return args[index : index+2]
		}
	}
	return []string{}
}

func parseFlags(cfg *BasicConfig, version string) {
	flag.CommandLine = flag.NewFlagSet(os.Args[0], flag.ExitOnError)
	flagSet := flag.CommandLine

	InitFlags(flagSet, cfg)

	fVersion := flagSet.Bool("version", false, "Prints current version and exits")
	args := os.Args[1:]
	flag1 := "-" + flagInstall
	flag2 := "--" + flagInstall
	for _, arg := range args {
		if strings.HasPrefix(arg, flag1+"=") || strings.HasPrefix(arg, flag2+"=") {
			cfg.InstallCommand = command{}
			break
		}
	}
	if err := flagSet.Parse(args); err != nil {
		logger.Errorf("Cannot parse command flags: %v", err)
	}

	if *fVersion {
		fmt.Println(version)
		os.Exit(0)
	}
}

// LoadConfigFromFile reads the file contents and unmarshal them into the given config structure.
func LoadConfigFromFile(filePath string, config interface{}) error {
	if !isFile(filePath) {
		return fmt.Errorf("incorrect config file %s", filePath)
	}
	file, err := os.ReadFile(filePath)
	if err != nil {
		return err
	}
	return json.Unmarshal(file, config)
}

// LoadConfig loads a new configuration instance using flags and config file (if set).
func LoadConfig(version string) (*BasicConfig, error) {
	configFilePath := ParseConfigFilePath()
	config := NewDefaultConfig()
	if configFilePath != "" {
		if err := LoadConfigFromFile(configFilePath, config); err != nil {
			return nil, err
		}
	}
	parseFlags(config, version)
	return config, nil
}
