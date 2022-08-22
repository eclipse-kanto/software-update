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
	wg        sync.WaitGroup
	done      = make(chan struct{})
	connected bool
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

// InitScriptBasedSU creates a new Script-Based SoftwareUpdatable instance, listening for edge configuration.
func InitScriptBasedSU(scriptSUPConfig *ScriptBasedSoftwareUpdatableConfig) (*EdgeConnector, error) {
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
	edge, err := NewEdgeConnector(scriptSUPConfig, feature)
	if err != nil {
		return nil, err
	}
	return edge, nil
}

// Connect the client to the configured Ditto endpoint.
// If any error occurs during the connection's initiation - it's returned here.
func (f *ScriptBasedSoftwareUpdatable) Connect(client MQTT.Client, scriptSUPConfig *ScriptBasedSoftwareUpdatableConfig, edgeCfg *edgeConfiguration) error {
	logger.Infof("Connecting to ditto endpoint with configuration - %v", edgeCfg)
	f.mqttClient = client
	done = make(chan struct{})
	f.queue = make(chan operationFunc, 10)
	f.lock.Lock()
	connected = true
	f.lock.Unlock()
	err := f.init(scriptSUPConfig, edgeCfg)
	if err != nil {
		return err
	}
	return f.dittoClient.Connect()
}

// Disconnect the client from the configured Ditto endpoint.
func (f *ScriptBasedSoftwareUpdatable) Disconnect(closeStorage bool) {
	if closeStorage {
		f.store.Close()
	}
	f.lock.Lock()
	connected = false
	f.lock.Unlock()
	close(done)
	wg.Wait()
	f.su.Deactivate()
	logger.Info("ditto client unsubscribed")
	f.dittoClient.Unsubscribe()
	logger.Info("ditto client disconnected")
	f.dittoClient.Disconnect()
}
