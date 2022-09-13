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
	"io/ioutil"
	"log"
	"os"
	"reflect"
	"strconv"
	"strings"
	"unicode"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

const (
	flagVersion     = "version"
	flagConfigFile  = "configFile"
	flagInstall     = "install"
	flagInstallDirs = "installDirs"
)

var (
	suConfig  = &ScriptBasedSoftwareUpdatableConfig{}
	logConfig = &logger.LogConfig{}

	descriptions = map[string]string{
		"mode": "Artifact access mode. Restricts where local file system artifacts can be located.\nAllowed values are:" +
			"\n  'strict' - artifacts can only be located in directories, included in installDirs property value" +
			"\n  'scoped' - artifacts can only be located in directories or their subdirectories recursively, included in installDirs property value" +
			"\n  'lax' - artifacts can be located anywhere on local file system. Use with care!",
	}
)

type cfg struct {
	Broker                string       `json:"broker" def:"tcp://localhost:1883" descr:"Local MQTT broker address"`
	Username              string       `json:"username" descr:"Username for authorized local client"`
	Password              string       `json:"password" descr:"Password for authorized local client"`
	StorageLocation       string       `json:"storageLocation" def:"." descr:"Location of the storage"`
	FeatureID             string       `json:"featureId" def:"SoftwareUpdatable" descr:"Feature identifier of SoftwareUpdatable"`
	ModuleType            string       `json:"moduleType" def:"software" descr:"Module type of SoftwareUpdatable"`
	ArtifactType          string       `json:"artifactType" def:"archive" descr:"Defines the module artifact type: archive or plane"`
	Install               []string     `json:"install" descr:"Defines the absolute path to install script"`
	ServerCert            string       `json:"serverCert" descr:"A PEM encoded certificate \"file\" for secure artifact download"`
	DownloadRetryCount    int          `json:"downloadRetryCount" def:"0" descr:"Number of retries, in case of a failed download.\n By default no retries are supported."`
	DownloadRetryInterval durationTime `json:"downloadRetryInterval" def:"5s" descr:"Interval between retries, in case of a failed download.\n Should be a sequence of decimal numbers, each with optional fraction and a unit suffix, such as '300ms', '1.5h', '10m30s', etc. Valid time units are 'ns', 'us' (or 'µs'), 'ms', 's', 'm', 'h'."`
	InstallDirs           []string     `json:"installDirs" descr:"Local file system directories, where to search for module artifacts"`
	Mode                  string       `json:"mode" def:"strict" descr:"%s"`
	LogFile               string       `json:"logFile" def:"log/software-update.log" descr:"Log file location in storage directory"`
	LogLevel              string       `json:"logLevel" def:"INFO" descr:"Log levels are ERROR, WARN, INFO, DEBUG, TRACE"`
	LogFileSize           int          `json:"logFileSize" def:"2" descr:"Log file size in MB before it gets rotated"`
	LogFileCount          int          `json:"logFileCount" def:"5" descr:"Log file max rotations count"`
	LogFileMaxAge         int          `json:"logFileMaxAge" def:"28" descr:"Log file rotations max age in days"`
}

// InitFlags tries to initialize Script-Based SoftwareUpdatable and Log configurations.
// Returns true if version flag is specified for print version and exit. Returns error
// if JSON configuration file cannot be read properly or missing config file is specified with flag.
func InitFlags(version string) (*ScriptBasedSoftwareUpdatableConfig, *logger.LogConfig, error) {
	flgConfig := &cfg{}
	printVersion := flag.Bool(flagVersion, false, "Prints current version and exits")
	configFile := flag.String(flagConfigFile, "", "Defines the configuration file")

	// the install flag is set in the config object initially
	flag.Var(&suConfig.InstallCommand, flagInstall, "Defines the absolute path to install script")
	flag.Var(&suConfig.InstallDirs, flagInstallDirs, "Local file system directories, where to search for module artifacts")

	initFlagsWithDefaultValues(flgConfig)
	flag.Parse()
	if *printVersion {
		fmt.Println(version)
		os.Exit(0)
	}

	if err := applyConfigurationFile(*configFile); err != nil {
		return nil, nil, err
	}
	applyFlags(flgConfig)
	return suConfig, logConfig, nil
}

func initFlagsWithDefaultValues(config interface{}) {
	valueOf := reflect.ValueOf(config).Elem()
	typeOf := valueOf.Type()
	for i := 0; i < typeOf.NumField(); i++ {
		fieldType := typeOf.Field(i)
		defaultValue := fieldType.Tag.Get("def")
		description := fieldType.Tag.Get("descr")
		fieldValue := valueOf.FieldByName(fieldType.Name)
		pointer := fieldValue.Addr().Interface()
		flagName := toFlagName(fieldType.Name)
		if val, ok := descriptions[flagName]; ok {
			description = fmt.Sprintf(description, val)
		}
		switch val := fieldValue.Interface(); val.(type) {
		case string:
			flag.StringVar(pointer.(*string), flagName, defaultValue, description)
		case int:
			value, err := strconv.Atoi(defaultValue)
			if err != nil {
				log.Printf("error parsing integer argument %v with value %v", fieldType.Name, defaultValue)
			}
			flag.IntVar(pointer.(*int), flagName, value, description)
		case durationTime:
			v, ok := pointer.(flag.Value)
			if ok {
				flag.Var(v, flagName, description)
			} else {
				log.Println("custom type Duration must implement reflect.Value interface")
			}
		}
	}
}

func loadDefaultValues() *cfg {
	result := &cfg{}
	valueOf := reflect.ValueOf(result).Elem()
	typeOf := valueOf.Type()
	for i := 0; i < typeOf.NumField(); i++ {
		fieldType := typeOf.Field(i)
		defaultValue := fieldType.Tag.Get("def")
		fieldValue := valueOf.FieldByName(fieldType.Name)
		pointer := fieldValue.Addr().Interface()
		if len(defaultValue) > 0 {
			fieldValue := valueOf.FieldByName(fieldType.Name)
			switch fieldValue.Interface().(type) {
			case string:
				fieldValue.Set(reflect.ValueOf(defaultValue))
			case int:
				value, err := strconv.Atoi(defaultValue)
				if err != nil {
					log.Printf("error parsing integer argument %v with value %v", fieldType.Name, defaultValue)
				}
				fieldValue.Set(reflect.ValueOf(value))
			case durationTime:
				v, ok := pointer.(flag.Value)
				if ok {
					if err := v.Set(defaultValue); err == nil {
						fieldValue.Set(reflect.ValueOf(v).Elem())
					} else {
						log.Printf("error parsing argument %v with value %v - %v", fieldType.Name, defaultValue, err)
					}
				} else {
					log.Println("custom type Duration must implement reflect.Value interface")
				}
			}

		}
	}
	return result
}

func applyFlags(flagsConfig interface{}) {
	flagsConfigVal := reflect.ValueOf(flagsConfig).Elem()
	suConfigVal := reflect.ValueOf(suConfig).Elem()
	logConfigVal := reflect.ValueOf(logConfig).Elem()
	flag.Visit(func(f *flag.Flag) {
		name := toFieldName(f.Name)
		srcFieldVal := flagsConfigVal.FieldByName(name)
		if srcFieldVal.Kind() != reflect.Invalid && srcFieldVal.Kind() != reflect.Struct {
			dstFieldSu := suConfigVal.FieldByName(name)
			dstFieldLog := logConfigVal.FieldByName(name)
			if dstFieldSu.Kind() != reflect.Invalid && dstFieldSu.Kind() != reflect.Struct {
				dstFieldSu.Set(srcFieldVal)
			} else if dstFieldLog.Kind() != reflect.Invalid {
				dstFieldLog.Set(srcFieldVal)
			}
		}
	})
}

func applyConfigurationFile(configFile string) error {
	def := loadDefaultValues()

	// Load configuration file (if possible)
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
	copyConfigData(def, suConfig)

	// Set install command only if install flag was not visited.
	// If visited, the command is set initially in the config struct when it is defined.
	if len(def.Install) > 0 && isNotVisited(flagInstall) {
		for _, v := range def.Install {
			suConfig.InstallCommand.Set(v)
		}
	}

	if len(def.InstallDirs) > 0 && isNotVisited(flagInstallDirs) {
		for _, v := range def.InstallDirs {
			suConfig.InstallDirs.Set(v)
		}
	}

	// Fulfil Log configuration with default/configuration values.
	copyConfigData(def, logConfig)
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

func copyConfigData(sourceConfig interface{}, targetConfig interface{}) {
	sourceConfigVal := reflect.ValueOf(sourceConfig).Elem()
	targetConfigVal := reflect.ValueOf(targetConfig).Elem()
	typeOfSourceConfig := sourceConfigVal.Type()
	for i := 0; i < typeOfSourceConfig.NumField(); i++ {
		fieldType := typeOfSourceConfig.Field(i)
		fieldToSet := targetConfigVal.FieldByName(fieldType.Name)
		if fieldToSet.Kind() != reflect.Invalid && fieldToSet.Kind() != reflect.Struct {
			fieldToSet.Set(sourceConfigVal.FieldByName(fieldType.Name))
		}
	}
}

func toFieldName(s string) string {
	s = replaceSuffix(s, "Id", "ID")
	rn := []rune(s)
	rn[0] = unicode.ToUpper(rn[0])
	return string(rn)
}

func toFlagName(s string) string {
	s = replaceSuffix(s, "ID", "Id")
	rn := []rune(s)
	rn[0] = unicode.ToLower(rn[0])
	return string(rn)
}

func replaceSuffix(s, suff, replacement string) string {
	if strings.HasSuffix(s, suff) {
		return s[:len(s)-len(suff)] + replacement
	}
	return s
}
