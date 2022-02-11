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
	"errors"

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

var edgeConfigurationChan = make(chan *edgeConfiguration, 1)

// retrieveEdgeConfiguration tries to connect to the local MQTT broker and retrieve its configuration.
func retrieveEdgeConfiguration(server string, username string, password string) (*edgeConfiguration, MQTT.Client, error) {
	logger.Infof("Retrieve edge configuration from: %s", server)
	opts := MQTT.NewClientOptions().
		AddBroker(server).
		SetClientID(uuid.New().String()).
		SetKeepAlive(defaultKeepAlive).
		SetCleanSession(true).
		SetAutoReconnect(true).
		SetOnConnectHandler(edgeConnectHandler)
	if len(username) > 0 {
		opts = opts.SetUsername(username).SetPassword(password)
	}

	mqttClient := MQTT.NewClient(opts)

	if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
		return nil, nil, token.Error()
	}

	defer mqttClient.Unsubscribe(topic)

	cfg := <-edgeConfigurationChan
	if cfg == nil {
		return nil, nil, errors.New("fail to retrieve edge configuration")
	}
	logger.Infof("Edge [TenantID: %s, DeviceID: %s, PolicyID: %s]", cfg.TenantID, cfg.DeviceID, cfg.PolicyID)
	return cfg, mqttClient, nil
}

func edgeConnectHandler(client MQTT.Client) {
	if token := client.Subscribe(topic, 1, func(client MQTT.Client, message MQTT.Message) {
		localCfg := &edgeConfiguration{}
		err := json.Unmarshal(message.Payload(), localCfg)
		if err != nil {
			logger.Errorf("could not unmarshal edge configuration: %v", err)
			edgeConfigurationChan <- nil
			return
		}
		edgeConfigurationChan <- localCfg
	}); token.Wait() && token.Error() != nil {
		logger.Errorf("fail to subscribe for %s topic: %v", topic, token.Error())
		edgeConfigurationChan <- nil
	}

	if token := client.Publish("edge/thing/request", 1, false, ""); token.Wait() && token.Error() != nil {
		logger.Errorf("fail to publish a message with %s topic: %v", topic, token.Error())
		edgeConfigurationChan <- nil
	}
}
