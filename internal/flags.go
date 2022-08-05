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
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

const (
	flagVersion         = "version"
	flagConfigFile      = "configFile"
	flagBroker          = "broker"
	flagUsername        = "username"
	flagPassword        = "password"
	flagStorageLocation = "storageLocation"
	flagFeatureID       = "featureId"
	flagModuleType      = "moduleType"
	flagArtifactType    = "artifactType"
	flagInstall         = "install"
	flagLogFile         = "logFile"
	flagLogLevel        = "logLevel"
	flagLogFileSize     = "logFileSize"
	flagLogFileCount    = "logFileCount"
	flagLogFileMaxAge   = "logFileMaxAge"
	flagCert            = "cert"
	flagKey             = "key"

	defaultBroker          = "tcp://localhost:1883"
	defaultUsername        = ""
	defaultPassword        = ""
	defaultStorageLocation = ""
	defaultFeatureID       = "SoftwareUpdatable"
	defaultModuleType      = "software"
	defaultArtifactType    = "archive"
	defaultLogFile         = "log/software-update.log"
	defaultLogLevel        = "INFO"
	defaultLogFileSize     = 2
	defaultLogFileCount    = 5
	defaultLogFileMaxAge   = 28
	defaultCertFile        = ""
	defaultCertKey         = ""
)

var (
	suConfig  = &ScriptBasedSoftwareUpdatableConfig{}
	logConfig = &logger.LogConfig{}
)

type cfg struct {
	Broker          string   `json:"broker"`
	Username        string   `json:"username"`
	Password        string   `json:"password"`
	StorageLocation string   `json:"storageLocation"`
	FeatureID       string   `json:"featureId"`
	ModuleType      string   `json:"moduleType"`
	ArtifactType    string   `json:"artifactType"`
	Install         []string `json:"install"`
	Cert            string   `json:"cert"`
	Key             string   `json:"key"`
	LogFile         string   `json:"logFile"`
	LogLevel        string   `json:"logLevel"`
	LogFileSize     int      `json:"logFileSize"`
	LogFileCount    int      `json:"logFileCount"`
	LogFileMaxAge   int      `json:"logFileMaxAge"`
}

// InitFlags tries to initialize Script-Based SoftwareUpdatable and Log configurations.
// Returns true if version flag is specified for print version and exit. Returns error
// if JSON configuration file cannot be read properly or missing config file is specified with flag.
func InitFlags(version string) (*ScriptBasedSoftwareUpdatableConfig, *logger.LogConfig, error) {
	flg := &cfg{}
	printVersion := flag.Bool(flagVersion, false, "Prints current version and exits")
	configFile := flag.String(flagConfigFile, "", "Defines the configuration file")

	// the install flag is set in the config object initially
	flag.Var(&suConfig.InstallCommand, flagInstall, "Defines the absolute path to install script")
	flag.StringVar(&flg.Broker, flagBroker, defaultBroker, "Local MQTT broker address")
	flag.StringVar(&flg.Username, flagUsername, defaultUsername, "Username for authorized local client")
	flag.StringVar(&flg.Password, flagPassword, defaultPassword, "Password for authorized local client")
	flag.StringVar(&flg.StorageLocation, flagStorageLocation, defaultStorageLocation, "Location of the storage")
	flag.StringVar(&flg.FeatureID, flagFeatureID, defaultFeatureID, "Feature identifier of SoftwareUpdatable")
	flag.StringVar(&flg.ModuleType, flagModuleType, defaultModuleType, "Module type of SoftwareUpdatable")
	flag.StringVar(&flg.ArtifactType, flagArtifactType, defaultArtifactType,
		"Defines the module artifact type: archive or plane")

	flag.StringVar(&flg.Cert, flagCert, defaultCertFile, "A PEM encoded certificate `file` for secure artifact download")
	flag.StringVar(&flg.Key, flagKey, defaultCertKey, "A PEM encoded unencrypted private key for secure artifact download")

	flag.StringVar(&flg.LogFile, flagLogFile, defaultLogFile, "Log file location in storage directory")
	flag.StringVar(&flg.LogLevel, flagLogLevel, defaultLogLevel, "Log levels are ERROR, WARNING, INFO, DEBUG, TRACE")
	flag.IntVar(&flg.LogFileSize, flagLogFileSize, defaultLogFileSize, "Log file size in MB before it gets rotated")
	flag.IntVar(&flg.LogFileCount, flagLogFileCount, defaultLogFileCount, "Log file max rotations count")
	flag.IntVar(&flg.LogFileMaxAge, flagLogFileMaxAge, defaultLogFileMaxAge, "Log file rotations max age in days")
	flag.Parse()

	if *printVersion {
		fmt.Println(version)
		os.Exit(0)
	}

	if err := applyConfigurationFile(*configFile); err != nil {
		return nil, nil, err
	}

	applyFlags(flg)
	return suConfig, logConfig, nil
}

func applyFlags(flg *cfg) {
	flag.Visit(func(f *flag.Flag) {
		switch name := f.Name; name {
		case flagBroker:
			suConfig.Broker = flg.Broker
		case flagUsername:
			suConfig.Username = flg.Username
		case flagPassword:
			suConfig.Password = flg.Password
		case flagStorageLocation:
			suConfig.StorageLocation = flg.StorageLocation
		case flagFeatureID:
			suConfig.FeatureID = flg.FeatureID
		case flagModuleType:
			suConfig.ModuleType = flg.ModuleType
		case flagArtifactType:
			suConfig.ArtifactType = flg.ArtifactType
		case flagCert:
			suConfig.Cert = flg.Cert
		case flagKey:
			suConfig.Key = flg.Key
		case flagLogFile:
			logConfig.LogFile = flg.LogFile
		case flagLogLevel:
			logConfig.LogLevel = flg.LogLevel
		case flagLogFileSize:
			logConfig.LogFileSize = flg.LogFileSize
		case flagLogFileCount:
			logConfig.LogFileCount = flg.LogFileCount
		case flagLogFileMaxAge:
			logConfig.LogFileMaxAge = flg.LogFileMaxAge
		default:
			// Unknown flag
		}
	})
}

func applyConfigurationFile(configFile string) error {
	def := &cfg{
		Broker:          defaultBroker,
		Username:        defaultUsername,
		Password:        defaultPassword,
		StorageLocation: defaultStorageLocation,
		FeatureID:       defaultFeatureID,
		ModuleType:      defaultModuleType,
		ArtifactType:    defaultArtifactType,
		LogFile:         defaultLogFile,
		LogLevel:        defaultLogLevel,
		LogFileSize:     defaultLogFileSize,
		LogFileCount:    defaultLogFileCount,
		LogFileMaxAge:   defaultLogFileMaxAge,
	}

	// Load configuration file (if posible)
	if len(configFile) > 0 {
		if jf, err := os.Open(configFile); err == nil {
			defer jf.Close()
			if buf, err := ioutil.ReadAll(jf); err == nil {
				if err := json.Unmarshal(buf, def); err != nil {
					return err
				}
			}
		} else {
			return fmt.Errorf("unable to locate or load the config file: %v", configFile)
		}
	}

	// Fulfil SoftwareUpdatable configuration with default/configuration values.
	suConfig.Broker = def.Broker
	suConfig.Username = def.Username
	suConfig.Password = def.Password
	suConfig.StorageLocation = def.StorageLocation
	suConfig.FeatureID = def.FeatureID
	suConfig.ModuleType = def.ModuleType
	suConfig.ArtifactType = def.ArtifactType

	// Set install command only if install flag was not visited.
	// If visited, the command is set initially in the config struct when it is defined.
	if len(def.Install) > 0 && isNotVisited(flagInstall) {
		for _, v := range def.Install {
			suConfig.InstallCommand.Set(v)
		}
	}

	// Fulfil Log configuration with default/configuration values.
	logConfig.LogFile = def.LogFile
	logConfig.LogLevel = def.LogLevel
	logConfig.LogFileSize = def.LogFileSize
	logConfig.LogFileCount = def.LogFileCount
	logConfig.LogFileMaxAge = def.LogFileMaxAge
	return nil
}

func isNotVisited(name string) bool {
	res := true
	flag.Visit(func(f *flag.Flag) {
		if f.Name == name {
			res = false
		}
	})
	return res
}
