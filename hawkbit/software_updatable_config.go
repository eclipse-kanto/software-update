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

package hawkbit

import (
	"errors"

	"github.com/eclipse/ditto-clients-golang"
	"github.com/eclipse/ditto-clients-golang/model"
)

// Configuration provides the SoftwareUpdatable's configuration.
type Configuration struct {
	dittoClient  *ditto.Client
	thingID      *model.NamespacedID
	featureID    string
	softwareType string

	//handlers
	downloadHandler     DownloadHandler
	installHandler      InstallHandler
	cancelHandler       CancelHandler
	removeHandler       RemoveHandler
	cancelRemoveHandler CancelRemoveHandler
}

// NewConfiguration returns a SoftwareUpdatable Configuration with the default feature ID.
func NewConfiguration() *Configuration {
	return &Configuration{featureID: suDefinitionName}
}

// WithDittoClient configures the Ditto client to be used by the SoftwareUpdatable feature.
func (cfg *Configuration) WithDittoClient(dittoClient *ditto.Client) *Configuration {
	cfg.dittoClient = dittoClient
	return cfg
}

// WithThingID configures the identifier of the thing, where SoftwareUpdatable feature will be registered.
func (cfg *Configuration) WithThingID(thingID *model.NamespacedID) *Configuration {
	cfg.thingID = thingID
	return cfg
}

// WithFeatureID configures the SoftwareUpdatable feature identifier.
func (cfg *Configuration) WithFeatureID(featureID string) *Configuration {
	cfg.featureID = featureID
	return cfg
}

// WithSoftwareType configures the SoftwareUpdatable software type.
func (cfg *Configuration) WithSoftwareType(softwareType string) *Configuration {
	cfg.softwareType = softwareType
	return cfg
}

// WithInstallHandler configures the handler to be notified when the SoftwareUpdatable receive install request.
func (cfg *Configuration) WithInstallHandler(handler InstallHandler) *Configuration {
	cfg.installHandler = handler
	return cfg
}

// WithDownloadHandler configures the handler to be notified when the SoftwareUpdatable receive download request.
func (cfg *Configuration) WithDownloadHandler(handler DownloadHandler) *Configuration {
	cfg.downloadHandler = handler
	return cfg
}

// WithCancelHandler configures the handler to be notified when the SoftwareUpdatable receive cancel request.
func (cfg *Configuration) WithCancelHandler(handler CancelHandler) *Configuration {
	cfg.cancelHandler = handler
	return cfg
}

// WithRemoveHandler configures the handler to be notified when the SoftwareUpdatable receive remove request.
func (cfg *Configuration) WithRemoveHandler(handler RemoveHandler) *Configuration {
	cfg.removeHandler = handler
	return cfg
}

// WithCancelRemoveHandler configures the handler to be notified when the SoftwareUpdatable
// receive cancelRemove request.
func (cfg *Configuration) WithCancelRemoveHandler(handler CancelRemoveHandler) *Configuration {
	cfg.cancelRemoveHandler = handler
	return cfg
}

// validateConfiguration will return an error if provided configuration is not valid.
func validateConfiguration(cfg *Configuration) error {
	if cfg == nil {
		return errors.New("software updatable configuration cannot be nil")
	}
	if cfg.dittoClient == nil {
		return errors.New("ditto client is madatory for software updatable")
	}
	if cfg.thingID == nil {
		return errors.New("thing identifier is madatory for software updatable")
	}
	if cfg.featureID == "" {
		return errors.New("feature identifier is madatory for software updatable")
	}
	if cfg.softwareType == "" {
		return errors.New("software type is madatory for software updatable")
	}
	return nil
}
