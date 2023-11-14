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
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"
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

	flagBroker          = "broker"
	flagUsername        = "username"
	flagPassword        = "password"
	flagStorageLocation = "storageLocation"
	flagFeatureID       = "featureId"
	flagModuleType      = "moduleType"
	flagArtifactType    = "artifactType"
	flagMode            = "mode"
	flagLogFile         = "logFile"
	flagLogLevel        = "logLevel"
	flagLogFileSize     = "logFileSize"
	flagLogFileCount    = "logFileCount"
	flagLogFileMaxAge   = "logFileMaxAge"
	flagServerCert      = "serverCert"
	flagRetryCount      = "downloadRetryCount"
	flagRetryInterval   = "downloadRetryInterval"
	flagInstallDirs     = "installDirs"
	flagVersion         = "version"
)

// testConfig is used to provide mock data
type testConfig struct {
	storageLocation string
	clientConnected bool
	featureID       string
	installDirs     []string
	mode            string
}

var (
	testVersion = "TestVersion"

	partialDownload = func(progress float64) bool {
		return progress > 0 && progress < 100
	}
	completeDownload = func(progress float64) bool {
		return progress == 100
	}
)

func assertDirs(t *testing.T, name string, create bool) string {
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

func connectFeature(t *testing.T, mc *mockedClient, feature *ScriptBasedSoftwareUpdatable, featureID string) error {
	t.Helper()
	cfg := NewDefaultConfig()
	cfg.ScriptBasedSoftwareUpdatableConfig.FeatureID = featureID
	edgeCfg := &edgeConfiguration{
		DeviceID: model.NewNamespacedID(testTopicNamespace, testTopicEntryID).String(),
		TenantID: testTenantID,
	}
	return feature.Connect(mc, &cfg.ScriptBasedSoftwareUpdatableConfig, edgeCfg)
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
		installCommand: &Command{},
		// Define the module artifact(s) type: archive or plain
		artifactType: typePlain,
		// Define install location, where to search for artifacts
		installDirs: tc.installDirs,
		// Define the local file artifacts access restrictions
		accessMode: initAccessMode(tc.mode),
		// Create queue with size 10
		queue: make(chan operationFunc, 10),
		// Create mocked MQTT Connection
		mqttClient: mc,
	}

	if err := connectFeature(t, mc, feature, tc.featureID); err != nil { // calls feature.init(...)
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

func createLocalArtifact(t *testing.T, dir, alias, body string) (string, string) {
	t.Helper()

	file := filepath.Join(dir, alias)
	if err := storage.WriteLn(file, body); err != nil {
		t.Fatal("error writing to file", err)
	}
	hType := sha256.New()
	hType.Write([]byte(body))
	return file, hex.EncodeToString(hType.Sum(nil))
}

func convertLocalArtifact(path, name, hash string, len int) *hawkbit.SoftwareArtifactAction {
	return &hawkbit.SoftwareArtifactAction{
		Filename: name,
		Download: map[hawkbit.Protocol]*hawkbit.Links{
			storage.ProtocolFile: {URL: path},
		},
		Checksums: map[hawkbit.Hash]string{
			hawkbit.SHA256: hash,
		},
		Size: len,
	}
}

func getAbsolutePath(t *testing.T, path string) string {
	t.Helper()

	absolutePath, err := filepath.Abs(path)
	if err != nil {
		t.Fatalf("error getting absolute file path of %v - %v", path, err)
	}
	return absolutePath
}

func checkFileExistsWithContent(t *testing.T, filename, expectedContent string) {
	content, err := os.ReadFile(filename)
	if err != nil {
		t.Fatalf("expected file %s to be created from install script(error opening: %v)", filename, err)
	}
	message := strings.Trim(string(content), "\r\n")
	if expectedContent != message {
		t.Fatalf("wrong install script execution result, expected: %s, got: %s", expectedContent, message)
	}
}

func checkNoFilesCopied(t *testing.T, dir string, canBeMoved bool) bool {
	info, err := ioutil.ReadDir(dir)
	if err != nil {
		if canBeMoved {
			return false
		}
		t.Fatalf("error reading folder %s - %v", dir, err)
	}
	if canBeMoved && len(info) == 0 {
		return false
	}
	if len(info) != 1 {
		t.Fatalf("expected only internal status file to be located in storage, actual number of files: %v", len(info))
	}
	if storage.InternalStatusName != info[0].Name() {
		t.Fatalf("expected only internal status file to be located in storage, instead found: %s", info[0].Name())
	}
	return true
}

func getCopiedArtifactsCount(artifacts []*hawkbit.SoftwareArtifactAction, copy string) (count int) {
	if len(copy) == 0 {
		return 0
	}
	if copy == "*" {
		return len(artifacts)
	}
	toCopy := strings.FieldsFunc(copy, storage.SplitArtifacts)
	for _, artifact := range artifacts {
		_, local := artifact.Download[storage.ProtocolFile]
		if !local || contains(toCopy, artifact.Filename) {
			count++
		}
	}
	return count
}

func contains(s []string, str string) bool {
	for _, el := range s {
		if el == str {
			return true
		}
	}
	return false
}
