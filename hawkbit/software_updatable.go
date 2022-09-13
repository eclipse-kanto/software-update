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
	"sync"

	"github.com/eclipse/ditto-clients-golang"
	"github.com/eclipse/ditto-clients-golang/model"
	"github.com/eclipse/ditto-clients-golang/protocol"
	"github.com/eclipse/ditto-clients-golang/protocol/things"
)

// SoftwareUpdatable is the HawkBit's library actual implementation.
// It provides SoftwareUpdatable capabilities to a specified Thing.
type SoftwareUpdatable struct {
	dittoClient *ditto.Client
	thingID     *model.NamespacedID
	featureID   string

	//handlers
	downloadHandler     DownloadHandler
	installHandler      InstallHandler
	cancelHandler       CancelHandler
	removeHandler       RemoveHandler
	cancelRemoveHandler CancelRemoveHandler

	statusLock sync.Mutex
	status     *softwareUpdatableStatus
	active     bool
}

// NewSoftwareUpdatable creates new SoftwareUpdatable instance, which will
// manage the SoftwareUdpate feature with the specified type to the provided
// Thing via Ditto client connection.
func NewSoftwareUpdatable(cfg *Configuration) (*SoftwareUpdatable, error) {
	// Check for missing mandatory configuration values.
	if err := validateConfiguration(cfg); err != nil {
		return nil, err
	}
	return &SoftwareUpdatable{
		dittoClient:         cfg.dittoClient,
		thingID:             cfg.thingID,
		featureID:           cfg.featureID,
		downloadHandler:     cfg.downloadHandler,
		installHandler:      cfg.installHandler,
		cancelHandler:       cfg.cancelHandler,
		removeHandler:       cfg.removeHandler,
		cancelRemoveHandler: cfg.cancelRemoveHandler,
		status: &softwareUpdatableStatus{
			SoftwareModuleType:    cfg.softwareType,
			InstalledDependencies: map[string]*DependencyDescription{},
		},
		active: false,
	}, nil
}

// Activate subscribes for incoming Ditto messages and send SoftwareUpdatable
// feature initial information. If any error occurs during the feature
// initiation - it's returned here.
func (su *SoftwareUpdatable) Activate() error {
	// Do not allow multiple goroutes to access SU status!
	su.statusLock.Lock()
	defer su.statusLock.Unlock()

	// Do not activate already activated feature.
	if su.active {
		return nil
	}

	// Create new SoftwareUpdatable feature event.
	INFO.Printf("Create new SoftwareUpdatable[%s] feature in: %s", su.status.SoftwareModuleType, su.thingID)
	feature := (&model.Feature{}).
		WithDefinition(model.NewDefinitionID(suDefinitionNamespace, suDefinitionName, suDefinitionVersion)).
		WithProperty(suPropertyStatus, su.status)
	event := things.NewCommand(su.thingID).Feature(su.featureID).Modify(feature).Twin()

	// Add the SoftwareUpdatable feature.
	su.dittoClient.Subscribe(su.messagesHandler)
	if err := su.dittoClient.Send(event.Envelope(protocol.WithResponseRequired(false))); err != nil {
		su.dittoClient.Unsubscribe()
		return err
	}
	su.active = true
	return nil
}

// Deactivate unsubscribes from incoming Ditto messages.
func (su *SoftwareUpdatable) Deactivate() {
	// Do not allow multiple goroutes to access SU status!
	su.statusLock.Lock()
	defer su.statusLock.Unlock()

	su.dittoClient.Unsubscribe()
	su.active = false
}

// SetInstalledDependencies set the entire set of installed dependencies of
// underlying SoftwareUpdatable feature.
// Note: Involking this function before the feature activation will change
// its initial installed dependencies value.
func (su *SoftwareUpdatable) SetInstalledDependencies(deps ...*DependencyDescription) error {
	// Do not allow multiple goroutes to access SU status!
	su.statusLock.Lock()
	defer su.statusLock.Unlock()

	DEBUG.Printf("Set installed dependencies: %v", deps)
	su.status.InstalledDependencies = toDependencyDescriptionMap(deps...)
	return su.setProperty(suPropertyInstalledDependencies, su.status.InstalledDependencies)
}

// SetContextDependencies set the entire set of context dependencies of
// underlying SoftwareUpdatable feature.
// Note: Involking this function before the feature activation will change
// its initial context dependencies value.
func (su *SoftwareUpdatable) SetContextDependencies(deps ...*DependencyDescription) error {
	// Do not allow multiple goroutes to access SU status!
	su.statusLock.Lock()
	defer su.statusLock.Unlock()

	su.status.ContextDependencies = toDependencyDescriptionMap(deps...)
	return su.setProperty(suPropertyContextDependencies, su.status.ContextDependencies)
}

// SetLastOperation set the last operation and last failed operation (if needed) of
// underlying SoftwareUpdatable feature.
// Note: Involking this function before the feature activation will change
// its initial last operation and last failed operation values.
func (su *SoftwareUpdatable) SetLastOperation(os *OperationStatus) error {
	// Do not allow multiple goroutes to access SU status!
	su.statusLock.Lock()
	defer su.statusLock.Unlock()

	// Check if OperationStatus has failed status, if so, update the LastFailedOperation.
	su.status.LastOperation = os
	if os != nil && (os.Status == StatusFinishedError || os.Status == StatusFinishedRejected ||
		os.Status == StatusCancelRejected) {
		su.status.LastFailedOperation = su.status.LastOperation
		if err := su.setProperty(suPropertyLastFailedOperation, su.status.LastFailedOperation); err != nil {
			return err
		}
	}
	return su.setProperty(suPropertyLastOperation, su.status.LastOperation)
}
