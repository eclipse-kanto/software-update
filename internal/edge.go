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

	"github.com/eclipse-kanto/software-update/internal/logger"

	MQTT "github.com/eclipse/paho.mqtt.golang"
	"github.com/google/uuid"
)

const (
	topic = "edge/thing/response"
)

// edgeConfiguration represents local Edge Thing configuration. Its device, tenand and prolicy identifiers.
type edgeConfiguration struct {
	DeviceID string `json:"deviceId"`
	TenantID string `json:"tenantId"`
	PolicyID string `json:"policyId"`
}

// EdgeConnector listens for Edge Thing configuration changes and notifies the corresponding EdgeClient.
// It is used in the main package.
type EdgeConnector struct {
	mqttClient MQTT.Client
	cfg        *edgeConfiguration
	edgeClient edgeClient
}

// edgeClient receives notifications of Edge Thing configuration changes from EdgeConnector
type edgeClient interface {
	Connect(client MQTT.Client, scriptSUPConfig *ScriptBasedSoftwareUpdatableConfig, cfg *edgeConfiguration) error
	Disconnect(closeStorage bool)
}

// newEdgeConnector create EdgeConnector with the given server, username and password for the given EdgeClient
func newEdgeConnector(scriptSUPConfig *ScriptBasedSoftwareUpdatableConfig, ecl edgeClient) (*EdgeConnector, error) {
	logger.Infof("creating edge connector with configuration: %s", scriptSUPConfig)
	opts := MQTT.NewClientOptions().
		AddBroker(scriptSUPConfig.Broker).
		SetClientID(uuid.New().String()).
		SetKeepAlive(defaultKeepAlive).
		SetCleanSession(true).
		SetAutoReconnect(true)
	if len(scriptSUPConfig.Username) > 0 {
		opts = opts.SetUsername(scriptSUPConfig.Username).SetPassword(scriptSUPConfig.Password)
	}

	p := &EdgeConnector{mqttClient: MQTT.NewClient(opts), edgeClient: ecl}
	if token := p.mqttClient.Connect(); token.Wait() && token.Error() != nil {
		return nil, token.Error()
	}

	if token := p.mqttClient.Subscribe(topic, 1, func(client MQTT.Client, message MQTT.Message) {
		localCfg := &edgeConfiguration{}
		err := json.Unmarshal(message.Payload(), localCfg)
		if err != nil {
			logger.Errorf("could not unmarshal edge configuration: %v", err)
			return
		}

		if p.cfg == nil || *localCfg != *p.cfg {
			logger.Infof("apply edge configuration: %v", localCfg)
			if p.cfg != nil {
				p.edgeClient.Disconnect(false)
			}
			p.cfg = localCfg
			err = ecl.Connect(p.mqttClient, scriptSUPConfig, p.cfg)
			if err != nil {
				logger.Errorf("error connecting to broker: %v", err)
			} else {
				logger.Infof("edge [TenantID: %s, DeviceID: %s, PolicyID: %s]", p.cfg.TenantID, p.cfg.DeviceID, p.cfg.PolicyID)
			}
		}
	}); token.Wait() && token.Error() != nil {
		logger.Errorf("fail to subscribe for %s topic: %v", topic, token.Error())
		return nil, token.Error()
	}
	logger.Info("ditto client subscribed")

	if token := p.mqttClient.Publish("edge/thing/request", 1, false, ""); token.Wait() && token.Error() != nil {
		logger.Errorf("fail to publish a message with %s topic: %v", topic, token.Error())
		return nil, token.Error()
	}
	return p, nil
}

// Close the EdgeConnector
func (p *EdgeConnector) Close() {
	if p.cfg != nil {
		p.edgeClient.Disconnect(true)
	}

	p.mqttClient.Unsubscribe(topic)
	p.mqttClient.Disconnect(200)

	logger.Info("disconnected from MQTT broker")
}
