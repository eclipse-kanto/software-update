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

package hawkbit

import (
	"encoding/json"
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"

	"github.com/eclipse/ditto-clients-golang"
	"github.com/eclipse/ditto-clients-golang/model"
	"github.com/eclipse/ditto-clients-golang/protocol"
	"github.com/eclipse/ditto-clients-golang/protocol/things"
	mqtt "github.com/eclipse/paho.mqtt.golang"
)

const (
	topicEntryID   = "thing.id"
	topicNamespace = "my-namespace.id"
	swType         = "my-type"
	cid            = "correlation-id"
)

// TestSoftwareUpdatableWithWrongConfig tests SoftwareUpdatable creation with nil configuration.
func TestSoftwareUpdatableWithWrongConfig(t *testing.T) {
	if _, err := NewSoftwareUpdatable(nil); err == nil {
		t.Error("software updatable was created with nil configuration")
	}
}

// TestSoftwareUpdatable tests software updatable creation and modification.
func TestSoftwareUpdatable(t *testing.T) {
	su, mc := mock(t, NewConfiguration())
	sid := &SoftwareModuleID{Name: "name", Version: "1.0.0"}

	// 1. Test installed dependencies initialization.
	idd := &DependencyDescription{Name: "init", Version: "1.0.0", Group: "installed", Type: swType}
	if err := su.SetInstalledDependencies(idd); err != nil {
		t.Fatalf("unexpected error during installed dependencies initialization")
	}

	// 2. Test context dependencies initialization.
	cdd := &DependencyDescription{Name: "init", Version: "1.0.0", Group: "context", Type: swType}
	if err := su.SetContextDependencies(cdd); err != nil {
		t.Fatalf("unexpected error during context dependencies initialization")
	}

	// 3. Test last operation initialization.
	sop := NewOperationStatusUpdate(cid, StatusFinishedSuccess, sid)
	if err := su.SetLastOperation(sop); err != nil {
		t.Fatalf("unexpected error during last operation initialization")
	}

	// 4. Test last operation and last failed operation initialization.
	fop := NewOperationStatusUpdate(cid, StatusFinishedError, sid)
	if err := su.SetLastOperation(fop); err != nil {
		t.Fatalf("unexpected error during last operation and last failed operation initialization")
	}

	// 5. Test software updatable activation.
	if err := su.Activate(); err != nil {
		t.Fatalf("unexpected error during activation")
	}
	status := getFeatureStatus(t, mc.value(t))

	// 5.1 Validate installed dependencies.
	hasDependencyDescription(t, status.InstalledDependencies, idd)

	// 5.2 Validate context dependencies.
	hasDependencyDescription(t, status.ContextDependencies, cdd)

	// 5.3 Validate last operation.
	if !reflect.DeepEqual(status.LastOperation, fop) {
		t.Fatalf("last operation mishmash: %v != %v", status.LastOperation, fop)
	}

	// 5.4 Validate last failed operation.
	if !reflect.DeepEqual(status.LastFailedOperation, fop) {
		t.Fatalf("last failed operation mishmash: %v != %v", status.LastFailedOperation, fop)
	}

	// 6. Test software updatable second activation.
	if err := su.Activate(); err != nil {
		t.Fatalf("unexpected error during the second activation")
	}

	// 7. Test installed dependencies after the activation.
	if err := su.SetInstalledDependencies(); err != nil {
		t.Fatalf("unexpected error during installed dependencies modification")
	}

	// 7.1 Test that installed dependencies is set to empty map.
	dds := mc.value(t)
	if len(dds.(map[string]interface{})) > 0 {
		t.Fatalf("unexpected installed dependencies: %v", dds)
	}

	// 8. Test context dependencies after the activation.
	if err := su.SetContextDependencies(); err != nil {
		t.Fatalf("unexpected error during context dependencies modification")
	}

	// 8.1 Test that context dependencies is set to empty map.
	dds = mc.value(t)
	if len(dds.(map[string]interface{})) > 0 {
		t.Fatalf("unexpected context dependencies: %v", dds)
	}

	// 9. Test last operation and last failed operation after activation.
	fop = NewOperationStatusUpdate(cid, StatusFinishedRejected, sid)
	if err := su.SetLastOperation(fop); err != nil {
		t.Fatalf("unexpected error during last operation and last failed operation modification")
	}

	// 9.1 Validate last failed operation after activation.
	ops := getOperationStatus(t, mc.value(t))
	if !reflect.DeepEqual(ops, fop) {
		t.Fatalf("last failed operation mishmash: %v != %v", ops, fop)
	}

	// 9.2 Validate last operation after activation.
	ops = getOperationStatus(t, mc.value(t))
	if !reflect.DeepEqual(ops, fop) {
		t.Fatalf("last operation mishmash: %v != %v", ops, fop)
	}

	// 10. Test SetLastOperation for error.
	mc.err = errors.New("test")
	if err := su.SetLastOperation(fop); err == nil {
		t.Fatal("missing error on SetLastOperation")
	}

	// CleanUp
	su.Deactivate()

	// 11. Test Activate for error.
	if err := su.Activate(); err == nil {
		t.Fatal("missing error on Activate")
	}
}

// TestUpdateHandler tests the install, download and cancel handlers.
func TestUpdateHandler(t *testing.T) {
	// Prepare update handler data.
	actual := make(chan interface{}, 1)
	handler := func(update *SoftwareUpdateAction, softwareUpdatable *SoftwareUpdatable) {
		actual <- update
	}
	rid := "request-id"
	tid := model.NewNamespacedID(topicNamespace, topicEntryID)
	cid := protocol.WithCorrelationID("correlation-id")
	expected := &SoftwareUpdateAction{}
	msg := things.NewMessage(tid).Feature(suDefinitionName).WithPayload(expected)

	// 1. Test install handler.
	t.Run("install", func(t *testing.T) {
		su, _ := mock(t, NewConfiguration().WithInstallHandler(handler))
		su.messagesHandler(rid, msg.Inbox("install").Envelope(cid))
		validateHandler(t, actual, expected)
	})

	// 2. Test download handler.
	t.Run("download", func(t *testing.T) {
		su, _ := mock(t, NewConfiguration().WithDownloadHandler(handler))
		su.messagesHandler(rid, msg.Inbox("download").Envelope(cid))
		validateHandler(t, actual, expected)
	})

	// 3. Test cancel handler.
	t.Run("cancel", func(t *testing.T) {
		su, _ := mock(t, NewConfiguration().WithCancelHandler(handler))
		su.messagesHandler(rid, msg.Inbox("cancel").Envelope(cid))
		validateHandler(t, actual, expected)
	})
}

// TestRemoveHandler tests the remove and cancelRemove handlers.
func TestRemoveHandler(t *testing.T) {
	// Prepare remove handler data.
	actual := make(chan interface{}, 1)
	handler := func(remove *SoftwareRemoveAction, softwareUpdatable *SoftwareUpdatable) {
		actual <- remove
	}
	rid := "request-id"
	tid := model.NewNamespacedID(topicNamespace, topicEntryID)
	cid := protocol.WithCorrelationID("correlation-id")
	expected := &SoftwareRemoveAction{}
	msg := things.NewMessage(tid).Feature(suDefinitionName).WithPayload(expected)

	// 1. Test remove handler.
	t.Run("remove", func(t *testing.T) {
		su, _ := mock(t, NewConfiguration().WithRemoveHandler(handler))
		su.messagesHandler(rid, msg.Inbox("remove").Envelope(cid))
		validateHandler(t, actual, expected)
	})

	// 2. Test cancelRemove handler.
	t.Run("cancelRemove", func(t *testing.T) {
		su, _ := mock(t, NewConfiguration().WithCancelRemoveHandler(handler))
		su.messagesHandler(rid, msg.Inbox("cancelRemove").Envelope(cid))
		validateHandler(t, actual, expected)
	})

	// 3. Test with reply error.
	t.Run("replyError", func(t *testing.T) {
		su, mc := mock(t, NewConfiguration().WithRemoveHandler(handler))
		mc.err = errors.New("test")
		su.messagesHandler(rid, msg.Inbox("remove").Envelope(cid))
		validateHandler(t, actual, expected)
	})

	// 4. Test remove handler with wrong payload.
	t.Run("wrongPayload", func(t *testing.T) {
		su, _ := mock(t, NewConfiguration().WithRemoveHandler(handler))
		su.messagesHandler(rid, msg.Inbox("remove").WithPayload("wrong").Envelope(cid))
		validateHandlerTimeout(t, actual)
	})

	// 5. Test with wrong operation.
	t.Run("wrongOperation", func(t *testing.T) {
		su, _ := mock(t, NewConfiguration().WithRemoveHandler(handler))
		su.messagesHandler(rid, msg.Inbox("wrong").Envelope(cid))
		validateHandlerTimeout(t, actual)
	})

	// 6. Test with wrong operation.
	t.Run("wrongTopic", func(t *testing.T) {
		su, _ := mock(t, NewConfiguration().WithRemoveHandler(handler))
		msg := things.NewMessage(tid).Feature(suDefinitionName).WithPayload(expected)
		msg.Topic.Namespace = "wrong"
		su.messagesHandler(rid, msg.Envelope(cid))
		validateHandlerTimeout(t, actual)
	})
}

// validateHandlerTimeout validates a handler with expected timeout.
func validateHandlerTimeout(t *testing.T, value chan interface{}) {
	select {
	case v := <-value:
		t.Fatalf("unexpected hendler call with: %v", v)
	case <-time.After(2 * time.Second):
		// Expected behavior.
	}
}

// validateHandler validates a handler with expected valid value.
func validateHandler(t *testing.T, value chan interface{}, expected interface{}) {
	select {
	case actual := <-value:
		// Validate actual and expected values are equal.
		if !reflect.DeepEqual(actual, expected) {
			t.Fatalf("handler value mishmash: %v != %v", actual, expected)
		}
	case <-time.After(2 * time.Second):
		// Fail after the timeout.
		t.Fatal("failed to retrieve handler data")
	}
}

// hasDependencyDescription test that provided dependency description exists.
func hasDependencyDescription(t *testing.T, actuals map[string]*DependencyDescription, exp *DependencyDescription) {
	for _, dd := range actuals {
		if dd.Name == exp.Name && dd.Version == exp.Version && dd.Group == exp.Group && dd.Type == exp.Type {
			return
		}
	}
	t.Fatalf("missing dependency description: %v", exp)
}

// getOperationStatus returns the operation status from a published message value.
func getOperationStatus(t *testing.T, v interface{}) *OperationStatus {
	res := &OperationStatus{}
	convert(t, v, res)
	return res
}

// getFeatureStatus returns the status from a published message value.
func getFeatureStatus(t *testing.T, v interface{}) *softwareUpdatableStatus {
	res := &softwareUpdatableStatus{}
	convert(t, v.(map[string]interface{})["properties"].(map[string]interface{})["status"], res)
	return res
}

// convert from one interface to another.
func convert(t *testing.T, v interface{}, to interface{}) {
	bytes, err := json.Marshal(v)
	if err != nil {
		t.Fatalf("unexpected error during data marshal: %v", err)
	}
	if err := json.Unmarshal(bytes, to); err != nil {
		t.Fatalf("unexpected error during data unmarshal: %v", err)
	}
}

// mock create new SoftwareUpdatable with mocked MQTT clients.
func mock(t *testing.T, cfg *Configuration) (*SoftwareUpdatable, *mockedClient) {
	// Create mocked Ditto and MQTT clients.
	mc := &mockedClient{payload: make(chan interface{}, 2)}
	dc, _ := ditto.NewClientMqtt(mc, nil)

	// Create hawkBit SoftwareUpdatable feature configuration.
	config := cfg.
		WithDittoClient(dc).
		WithThingID(model.NewNamespacedID(topicNamespace, topicEntryID)).
		WithSoftwareType(swType)

	// Create new SoftwareUpdatable with mocked Ditto and MQTT clients.
	su, err := NewSoftwareUpdatable(config)
	if err != nil {
		t.Fatalf("failed to create software updatable: %v", err)
	}

	// Return all structs.
	return su, mc
}

// mockedToken represents mocked mqtt.Token interface used for testing.
type mockedClient struct {
	err     error
	payload chan interface{}
}

// value returns last payload value or waits 10sec for new payload.
func (client *mockedClient) value(t *testing.T) interface{} {
	select {
	case payload := <-client.payload:
		// Convert the payload to ditto envelop.
		env := &protocol.Envelope{}
		if err := json.Unmarshal(payload.([]byte), env); err != nil {
			t.Fatalf("unexpected error during data unmarshal: %v", err)
		}
		// Validate its topic namespace.
		if env.Topic.Namespace != topicNamespace {
			t.Fatalf("message topic namespace mishmash: %v != %v", env.Topic.Namespace, topicNamespace)
		}
		// Validate its topic entry identifier.
		if env.Topic.EntityID != topicEntryID {
			t.Fatalf("message topic entity identifier mishmash: %v != %v", env.Topic.EntityID, topicEntryID)
		}
		// Valdiate its starting path.
		if !strings.HasPrefix(env.Path, "/features/SoftwareUpdatable") {
			t.Fatalf("message path do not starts with [%v]: %v", "/features/SoftwareUpdatable", env.Path)
		}
		// Return its the value.
		return env.Value
	case <-time.After(5 * time.Second):
		// Fail after the timeout.
		t.Fatal("failed to retrieve published data")
	}
	return ""
}

// IsConnected returns true.
func (client *mockedClient) IsConnected() bool {
	return true
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

// Publish returns finished token and set client topic and payload.
func (client *mockedClient) Publish(topic string, qos byte, retained bool, payload interface{}) mqtt.Token {
	client.payload <- payload
	return &mockedToken{err: client.err}
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
