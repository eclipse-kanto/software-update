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
	"testing"

	"github.com/eclipse/ditto-clients-golang"
	"github.com/eclipse/ditto-clients-golang/model"
)

const (
	thingNamespace = "my-namespace.id"
	thingName      = "my.thing"
	featureID      = "TestFeature"
	softwareType   = "test-type"
)

// TestConfiguration tests confguration creation.
func TestConfiguration(t *testing.T) {
	fUpdate := func(update *SoftwareUpdateAction, softwareUpdatable *SoftwareUpdatable) { /* Test */ }
	fRemove := func(software *SoftwareRemoveAction, softwareUpdatable *SoftwareUpdatable) { /* Test */ }
	cfg := NewConfiguration().
		WithThingID(model.NewNamespacedID(thingNamespace, thingName)).
		WithFeatureID(featureID).
		WithSoftwareType(softwareType).
		WithDittoClient(ditto.NewClient(nil)).
		WithInstallHandler(fUpdate).
		WithDownloadHandler(fUpdate).
		WithRemoveHandler(fRemove).
		WithCancelHandler(fUpdate).
		WithCancelRemoveHandler(fRemove)

	if cfg.thingID.Namespace != thingNamespace {
		t.Errorf("thing namespace mishmash: %v != %v", cfg.thingID.Namespace, thingNamespace)
	}
	if cfg.thingID.Name != thingName {
		t.Errorf("thing name mishmash: %v != %v", cfg.thingID.Name, thingName)
	}
	if cfg.featureID != featureID {
		t.Errorf("feature identifier mishmash: %v != %v", cfg.featureID, featureID)
	}
	if cfg.softwareType != softwareType {
		t.Errorf("software type mishmash: %v != %v", cfg.softwareType, softwareType)
	}
	if cfg.dittoClient == nil {
		t.Error("missing ditto client")
	}
	if cfg.installHandler == nil {
		t.Error("missing install handler")
	}
	if cfg.downloadHandler == nil {
		t.Error("missing download handler")
	}
	if cfg.removeHandler == nil {
		t.Error("missing remove handler")
	}
	if cfg.cancelHandler == nil {
		t.Error("missing cancel handler")
	}
	if cfg.cancelRemoveHandler == nil {
		t.Error("missing cancel remove handler")
	}
}

// TestConfigurationValidation tests the validation of the configuration.
func TestConfigurationValidation(t *testing.T) {
	// 1. Test for nil configuration.
	if err := validateConfiguration(nil); err == nil ||
		err.Error() != "software updatable configuration cannot be nil" {
		t.Error("nil configuration should not be accepted")
	}

	tid := model.NewNamespacedID(thingNamespace, thingName)

	// 2. Test for missing ditto client.
	cfg := &Configuration{thingID: tid, featureID: featureID, softwareType: softwareType}
	if err := validateConfiguration(cfg); err == nil ||
		err.Error() != "ditto client is madatory for software updatable" {
		t.Error("nil configuration should not be accepted")
	}

	dittoClient := ditto.NewClient(nil)

	// 3. Test for missing thing identifier.
	cfg = &Configuration{dittoClient: dittoClient, featureID: featureID, softwareType: softwareType}
	if err := validateConfiguration(cfg); err == nil ||
		err.Error() != "thing identifier is madatory for software updatable" {
		t.Error("configuration with missing thing identifier should not be accepted")
	}

	// 4. Test for missing feature identifier.
	cfg = &Configuration{dittoClient: dittoClient, thingID: tid, softwareType: softwareType}
	if err := validateConfiguration(cfg); err == nil ||
		err.Error() != "feature identifier is madatory for software updatable" {
		t.Error("configuration with missing feature identifier should not be accepted")
	}

	// 5. Test for missing software type.
	cfg = &Configuration{dittoClient: dittoClient, thingID: tid, featureID: featureID}
	if err := validateConfiguration(cfg); err == nil ||
		err.Error() != "software type is madatory for software updatable" {
		t.Error("configuration with missing software type should not be accepted")
	}

	// 6. Test with valid configuration.
	cfg = &Configuration{dittoClient: dittoClient, thingID: tid, featureID: featureID, softwareType: softwareType}
	if err := validateConfiguration(cfg); err != nil {
		t.Errorf("unexpected configuration error: %v", err)
	}
}
