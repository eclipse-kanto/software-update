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

// SoftwareUpdateAction is used for instructing the device to install or download one or more software.
type SoftwareUpdateAction struct {
	// CorrelationID returns an identifier used to correlate the status updates sent from the device for this action.
	CorrelationID string `json:"correlationId"`
	// SoftwareModules that needs to be processed.
	SoftwareModules []*SoftwareModuleAction `json:"softwareModules,omitempty"`
	// Weight returns the priority in case of multiple, parallel installation instructions.
	Weight int `json:"weight,omitempty"`
	// Metadata returns any other information which should be passed to the device.
	Metadata map[string]string `json:"metadata,omitempty"`
	// Forced indicates the urgency of the update. When true, the device should install as soon as possible.
	Forced bool `json:"forced,omitempty"`
}
