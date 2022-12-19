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

package hawkbit

import (
	"encoding/json"
	"fmt"
	"strconv"

	"github.com/eclipse/ditto-clients-golang/protocol"
	"github.com/eclipse/ditto-clients-golang/protocol/things"
)

const (
	suDefinitionNamespace = "org.eclipse.hawkbit.swupdatable"
	suDefinitionName      = "SoftwareUpdatable"
	suDefinitionVersion   = "2.0.0"

	suPropertyStatus                = "status"
	suPropertySoftwareModuleType    = suPropertyStatus + "/softwareModuleType"
	suPropertyLastOperation         = suPropertyStatus + "/lastOperation"
	suPropertyLastFailedOperation   = suPropertyStatus + "/lastFailedOperation"
	suPropertyInstalledDependencies = suPropertyStatus + "/installedDependencies"
	suPropertyContextDependencies   = suPropertyStatus + "/contextDependencies"
)

func (su *SoftwareUpdatable) messagesHandler(requestID string, msg *protocol.Envelope) {
	if requestID != "" {
		// Process incomming messages in new goroute to avoid deadlock in MQTT.Publish
		go su.messagesHandlerRoute(requestID, msg)
	}
}

func (su *SoftwareUpdatable) messagesHandlerRoute(requestID string, msg *protocol.Envelope) {
	DEBUG.Printf("[Received :: %s] %v\n", requestID, msg)
	if msg.Topic.Namespace == su.thingID.Namespace && msg.Topic.EntityID == su.thingID.Name {
		if su.installHandler != nil &&
			msg.Path == fmt.Sprintf("/features/%s/inbox/messages/install", su.featureID) {
			su.processInstall(requestID, msg)
		} else if su.downloadHandler != nil &&
			msg.Path == fmt.Sprintf("/features/%s/inbox/messages/download", su.featureID) {
			su.processDownload(requestID, msg)
		} else if su.cancelHandler != nil &&
			msg.Path == fmt.Sprintf("/features/%s/inbox/messages/cancel", su.featureID) {
			su.processCancel(requestID, msg)
		} else if su.removeHandler != nil &&
			msg.Path == fmt.Sprintf("/features/%s/inbox/messages/remove", su.featureID) {
			su.processRemove(requestID, msg)
		} else if su.cancelRemoveHandler != nil &&
			msg.Path == fmt.Sprintf("/features/%s/inbox/messages/cancelRemove", su.featureID) {
			su.processCancelRemove(requestID, msg)
		} else {
			DEBUG.Println("There is no handler for a message - skipping processing")
		}
	} else {
		DEBUG.Println("Message to unknown Thing - skipping processing")
	}
}

func (su *SoftwareUpdatable) processInstall(requestID string, msg *protocol.Envelope) {
	ua := &SoftwareUpdateAction{}
	if su.prepare(requestID, msg, "install", ua) {
		su.installHandler(ua, su)
	}
}

func (su *SoftwareUpdatable) processDownload(requestID string, msg *protocol.Envelope) {
	ua := &SoftwareUpdateAction{}
	if su.prepare(requestID, msg, "download", ua) {
		su.downloadHandler(ua, su)
	}
}

func (su *SoftwareUpdatable) processCancel(requestID string, msg *protocol.Envelope) {
	ua := &SoftwareUpdateAction{}
	if su.prepare(requestID, msg, "cancel", ua) {
		su.cancelHandler(ua, su)
	}
}

func (su *SoftwareUpdatable) processRemove(requestID string, msg *protocol.Envelope) {
	ra := &SoftwareRemoveAction{}
	if su.prepare(requestID, msg, "remove", ra) {
		su.removeHandler(ra, su)
	}
}

func (su *SoftwareUpdatable) processCancelRemove(requestID string, msg *protocol.Envelope) {
	ra := &SoftwareRemoveAction{}
	if su.prepare(requestID, msg, "cancelRemove", ra) {
		su.cancelRemoveHandler(ra, su)
	}
}

func (su *SoftwareUpdatable) prepare(requestID string, msg *protocol.Envelope, operation string, to interface{}) bool {
	DEBUG.Printf("Parse message value: %v", msg.Value)
	respReq := msg.Headers.IsResponseRequired()
	bytes, err := json.Marshal(msg.Value)
	if err == nil {
		err = json.Unmarshal(bytes, to)
	}
	if err == nil {
		if respReq {
			su.reply(requestID, msg.Headers.CorrelationID(), operation, 204, nil)
		}
		DEBUG.Printf("Start %s operation with id: %s", operation, msg.Headers.CorrelationID())
		return true
	}
	supErr := newMessagesParameterInvalidError(err.Error())
	ERROR.Println(fmt.Errorf("failed to parse message value: %v", supErr))
	if respReq {
		su.reply(requestID, msg.Headers.CorrelationID(), operation, supErr.Status, supErr)
	}
	return false
}

func (su *SoftwareUpdatable) reply(requestID string, cid string, cmd string, status int, payload interface{}) {
	bHeadersOpts := [3]protocol.HeaderOpt{protocol.WithCorrelationID(cid), protocol.WithResponseRequired(false)}
	headerOpts := bHeadersOpts[:2]
	response := things.NewMessage(su.thingID).Feature(su.featureID).Outbox(cmd)
	if payload != nil {
		response.WithPayload(payload)
		headerOpts = append(headerOpts, protocol.WithContentType("application/json"))
	}
	responseMsg := response.Envelope(headerOpts...)
	responseMsg.Status = status

	if err := su.dittoClient.Reply(requestID, responseMsg); err != nil {
		ERROR.Println(fmt.Errorf("failed to send error response to request Id %s: %v", requestID, err))
	} else {
		DEBUG.Printf("[Sent] %v\n", responseMsg)
	}
}

func (su *SoftwareUpdatable) setProperty(name string, value interface{}) error {
	if !su.active {
		return nil
	}
	DEBUG.Printf("Set %s property to: %v", name, value)
	cmd := things.NewCommand(su.thingID).FeatureProperty(su.featureID, name).Modify(value).Twin()
	return su.dittoClient.Send(cmd.Envelope(protocol.WithResponseRequired(false)))
}

func toDependencyDescriptionMap(deps ...*DependencyDescription) map[string]*DependencyDescription {
	to := map[string]*DependencyDescription{}
	for _, v := range deps {
		to[strconv.Itoa(len(to))] = v
	}
	return to
}
