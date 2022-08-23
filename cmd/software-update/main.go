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
	suConfig, logConfig, err := feature.InitFlags(version)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// Initialize logs.
	loggerOut := logger.SetupLogger(logConfig)
	defer loggerOut.Close()

	// Create new Script-Based software updatable
	edgeCtr, err := feature.InitScriptBasedSU(suConfig)
	if err != nil {
		logger.Errorf("failed to create script-based software updatable: %v", err)
		os.Exit(1)
	}
	defer edgeCtr.Close() // not nil

	chWaitCtrlC := make(chan os.Signal, 1)
	signal.Notify(chWaitCtrlC, os.Interrupt)
	<-chWaitCtrlC
}
