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

package main

import (
	"fmt"
	"os"
	"os/signal"

	feature "github.com/eclipse-kanto/software-update/internal"
	"github.com/eclipse-kanto/software-update/internal/logger"
)

var version = "N/A"

func main() {
	// Initialize flags.
	cfg, err := feature.LoadConfig(version)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// Initialize logs.
	loggerOut := logger.SetupLogger(&cfg.LogConfig)
	defer loggerOut.Close()

	if err := cfg.Validate(); err != nil {
		logger.Errorf("failed to validate script-based software updatable configuration: %v\n", err)
		os.Exit(1)
	}

	// Create new Script-Based software updatable
	edgeCtr, err := feature.InitScriptBasedSU(&cfg.ScriptBasedSoftwareUpdatableConfig)
	if err != nil {
		logger.Errorf("failed to create script-based software updatable: %v", err)
		os.Exit(1)
	}
	defer edgeCtr.Close() // not nil

	chWaitCtrlC := make(chan os.Signal, 1)
	signal.Notify(chWaitCtrlC, os.Interrupt)
	<-chWaitCtrlC
}
