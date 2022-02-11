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

// SoftwareRemoveAction is used for instructing the device to remove a defined set of software.
type SoftwareRemoveAction struct {
	// CorrelationID returns an identifier used to correlate the status updates sent from the device for this action.
	CorrelationID string `json:"correlationId"`
	// Software to be removed.
	Software []*DependencyDescription `json:"software,omitempty"`
	// Weight returns the priority in case of multiple, parallel instructions.
	Weight int `json:"weight,omitempty"`
	// Metadata returns any other information which should be passed to the device.
	Metadata map[string]string `json:"metadata,omitempty"`
	// Forced indicates the urgency of the update. When true, the device should install as soon as possible.
	Forced bool `json:"forced,omitempty"`
}
