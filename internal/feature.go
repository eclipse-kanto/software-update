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
	"sync"
	"time"

	"github.com/eclipse-kanto/software-update/hawkbit"
	"github.com/eclipse-kanto/software-update/internal/logger"
	"github.com/eclipse-kanto/software-update/internal/storage"

	"github.com/eclipse/ditto-clients-golang"
	MQTT "github.com/eclipse/paho.mqtt.golang"
)

const (
	defaultDisconnectTimeout = 250 * time.Millisecond
	defaultKeepAlive         = 20 * time.Second
)

var (
	wg   sync.WaitGroup
	done = make(chan struct{})
)

// operationFunc returns true if operation is canceled.
type operationFunc func() bool

// ScriptBasedSoftwareUpdatableConfig provides the Script-Based SoftwareUpdatable configuration.
type ScriptBasedSoftwareUpdatableConfig struct {
	Broker          string
	Username        string
	Password        string
	StorageLocation string
	FeatureID       string
	ModuleType      string
	ArtifactType    string
	ServerCert      string
	InstallCommand  command
}

// ScriptBasedSoftwareUpdatable is the Script-Based SoftwareUpdatable actual implementation.
type ScriptBasedSoftwareUpdatable struct {
	lock           sync.Mutex
	queue          chan operationFunc
	store          *storage.Storage
	su             *hawkbit.SoftwareUpdatable
	dittoClient    *ditto.Client
	mqttClient     MQTT.Client
	artifactType   string
	serverCert     string
	installCommand *command
}

// NewScriptBasedSU creates a new Script-Based SoftwareUpdatable instance with the provided configuration.
func NewScriptBasedSU(scriptSUPConfig *ScriptBasedSoftwareUpdatableConfig) (*ScriptBasedSoftwareUpdatable, error) {
	logger.Infof("New Script-Based SoftwareUpdatable [Broker: %s, Type: %s]",
		scriptSUPConfig.Broker, scriptSUPConfig.ModuleType)

	// Initialize local storage and load installed dependencies
	localStorage, err := storage.NewStorage(scriptSUPConfig.StorageLocation)
	if err != nil {
		return nil, err
	}
	feature := &ScriptBasedSoftwareUpdatable{
		// Initialize local storage and load installed dependencies
		store: localStorage,
		// Build install script command
		installCommand: &scriptSUPConfig.InstallCommand,
		serverCert:     scriptSUPConfig.ServerCert,
		// Define the module artifact(s) type: archive or plane
		artifactType: scriptSUPConfig.ArtifactType,
		// Create queue with size 10
		queue: make(chan operationFunc, 10),
	}

	// Get the local edge configuration.
	edge, mc, err := retrieveEdgeConfiguration(scriptSUPConfig.Broker, scriptSUPConfig.Username, scriptSUPConfig.Password)
	if err != nil {
		return nil, err
	}
	feature.mqttClient = mc
	return feature, feature.init(scriptSUPConfig, edge)
}

// Connect the client to the configured Ditto endpoint.
// If any error occurs during the connection's initiation - it's returned here.
func (f *ScriptBasedSoftwareUpdatable) Connect() error {
	return f.dittoClient.Connect()
}

// Disconnect the client from the configured Ditto endpoint.
func (f *ScriptBasedSoftwareUpdatable) Disconnect() {
	f.store.Close()
	close(done)
	wg.Wait()
	f.su.Deactivate()
	f.dittoClient.Disconnect()
	f.mqttClient.Disconnect(250)
}
