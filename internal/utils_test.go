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
	"os"
	"strings"
	"testing"
	"time"

	"github.com/eclipse-kanto/software-update/hawkbit"
	"github.com/eclipse-kanto/software-update/internal/storage"
	"github.com/eclipse/ditto-clients-golang"
	"github.com/eclipse/ditto-clients-golang/model"
	"github.com/eclipse/ditto-clients-golang/protocol"
	mqtt "github.com/eclipse/paho.mqtt.golang"
)

const (
	testTopicEntryID   = "thing.id"
	testTopicNamespace = "my-namespace.id"
	testTenantID       = "test-tenant-id"
)

// testConfig is used to provide mock data
type testConfig struct {
	storageLocation string
	clientConnected bool
	featureID       string
}

var (
	supConfig *ScriptBasedSoftwareUpdatableConfig
	edgeCfg   *edgeConfiguration
)

var testVersion = "TestVersion"

func assertPath(t *testing.T, name string, create bool) string {
	if _, err := os.Stat(name); !os.IsNotExist(err) {
		if err = os.RemoveAll(name); err != nil {
			t.Fatalf("failed delete temporary directory [%s]: %v", name, err)
		}
	}
	if create {
		if err := os.MkdirAll(name, 0755); err != nil {
			t.Fatalf("failed to create temporary directory [%s]: %v", name, err)
		}
	}
	return name
}

// mockScriptBasedSoftwareUpdatable create new ScriptBasedSoftwareUpdatable with mocked MQTT clients.
func mockScriptBasedSoftwareUpdatable(t *testing.T, tc *testConfig) (*ScriptBasedSoftwareUpdatable, *mockedClient, error) {
	// Create default ScriptBasedSoftwareUpdatable with mocked MQTT Client
	mc := mockMqttClient(tc)

	// Initialize local storage and load installed dependencies
	localStorage, err := storage.NewStorage(tc.storageLocation)
	if err != nil {
		t.Fatalf("fail to initialize local storage: %v", err)
	}

	// Create new ScriptBasedSoftwareUpdatable struct
	feature := &ScriptBasedSoftwareUpdatable{
		store: localStorage,
		// Build install script command
		installCommand: &command{},
		// Define the module artifact(s) type: archive or plane
		artifactType: "plane",
		// Create queue with size 10
		queue: make(chan operationFunc, 10),
		// Create mocked MQTT Connection
		mqttClient: mc,
	}

	// Initialize mocked ScriptBasedSoftwareUpdatable
	supConfig = &ScriptBasedSoftwareUpdatableConfig{
		Broker:     getDefaultFlagValue(t, flagBroker),
		FeatureID:  tc.featureID,
		ModuleType: getDefaultFlagValue(t, flagModuleType),
	}
	edgeCfg = &edgeConfiguration{
		DeviceID: model.NewNamespacedID(testTopicNamespace, testTopicEntryID).String(),
		TenantID: testTenantID,
	}
	if err := feature.Connect(mc, supConfig, edgeCfg); err != nil { // calls feature.init(...)
		return nil, nil, err
	}

	// Wait for Ditto client to initialize.
	time.Sleep(5 * time.Second)
	return feature, mc, nil
}

// mockSoftwareUpdatable create new mocked SoftwareUpdatable with mocked MQTT client.
func mockSoftwareUpdatable(t *testing.T, cfg *hawkbit.Configuration, tc *testConfig) (*hawkbit.SoftwareUpdatable, *mockedClient) {
	// Create mocked Ditto and MQTT clients.
	mc := mockMqttClient(tc)
	dc, _ := ditto.NewClientMqtt(mc, nil)

	// Create hawkBit SoftwareUpdatable feature configuration.
	config := cfg.
		WithDittoClient(dc).
		WithThingID(model.NewNamespacedID(testTopicNamespace, testTopicEntryID)).
		WithSoftwareType(testType)

	// Create new SoftwareUpdatable with mocked Ditto and MQTT clients.
	su, err := hawkbit.NewSoftwareUpdatable(config)
	if err != nil {
		t.Fatalf("failed to create software updatable: %v", err)
	}

	// Return all structs.
	return su, mc
}

// mockMqttClient create new mocked MQTT client.
func mockMqttClient(tc *testConfig) *mockedClient {
	return &mockedClient{
		payload:   make(chan interface{}, 1),
		connected: tc.clientConnected,
	}
}

// mockedToken represents mocked mqtt.Token interface used for testing.
type mockedClient struct {
	err       error
	payload   chan interface{}
	connected bool
}

func (client *mockedClient) pullLastOperationStatus() map[string]interface{} {
	select {
	case payload := <-client.payload:
		// Get value map.
		if lo, ok := payload.(map[string]interface{}); ok {
			return lo
		}
	case <-time.After(10 * time.Second):
		// Fail after the timeout.
		return nil
	}
	return nil
}

// IsConnected returns true.
func (client *mockedClient) IsConnected() bool {
	return client.connected
}

// IsConnectionOpen returns true.
func (client *mockedClient) IsConnectionOpen() bool {
	return true
}

// Connect returns finished token.
func (client *mockedClient) Connect() mqtt.Token {
	return &mockedToken{err: client.err}
}

// Disconnect do nothing.
func (client *mockedClient) Disconnect(quiesce uint) {
	// Do nothing.
}

// Publish returns finished token and store lastOperation if found.
func (client *mockedClient) Publish(topic string, qos byte, retained bool, payload interface{}) (token mqtt.Token) {
	token = &mockedToken{err: client.err}
	// Convert the payload to ditto envelop.
	env := &protocol.Envelope{}
	if err := json.Unmarshal(payload.([]byte), env); err != nil {
		return token
	}
	// Validate its topic namespace and entry identifier.
	if env.Topic.Namespace != testTopicNamespace || env.Topic.EntityID != testTopicEntryID {
		return token
	}
	// Validate its starting path.
	if !strings.HasPrefix(env.Path, "/features/SoftwareUpdatable/properties/status/lastOperation") {
		return token
	}
	// Get value map.
	client.payload <- env.Value
	return token
}

// Subscribe returns finished token.
func (client *mockedClient) Subscribe(topic string, qos byte, callback mqtt.MessageHandler) mqtt.Token {
	return &mockedToken{err: client.err}
}

// SubscribeMultiple returns finished token.
func (client *mockedClient) SubscribeMultiple(filters map[string]byte, callback mqtt.MessageHandler) mqtt.Token {
	return &mockedToken{err: client.err}
}

// Unsubscribe returns finished token.
func (client *mockedClient) Unsubscribe(topics ...string) mqtt.Token {
	return &mockedToken{err: client.err}
}

// AddRoute do nothing.
func (client *mockedClient) AddRoute(topic string, callback mqtt.MessageHandler) {
	// Do nothing.
}

// OptionsReader returns an empty struct.
func (client *mockedClient) OptionsReader() mqtt.ClientOptionsReader {
	return mqtt.ClientOptionsReader{}
}

// mockedToken represents mocked mqtt.Token interface used for testing.
type mockedToken struct {
	err error
}

// Wait returns immediately with true.
func (token *mockedToken) Wait() bool {
	return true
}

// WaitTimeout returns immediately with true.
func (token *mockedToken) WaitTimeout(time.Duration) bool {
	return true
}

// Done returns immediately with nil channel.
func (token *mockedToken) Done() <-chan struct{} {
	return nil
}

// Error returns the error if set.
func (token *mockedToken) Error() error {
	return token.err
}
