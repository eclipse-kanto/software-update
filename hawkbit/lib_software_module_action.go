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

// SoftwareModuleAction representing a software module - a collection of artifacts to be downloaded and installed.
type SoftwareModuleAction struct {
	// SoftwareModule represents an unique indentifier for the software module.
	SoftwareModule *SoftwareModuleID `json:"softwareModule"`
	// Artifacts represents a list of software artifacts contained in the module.
	Artifacts []*SoftwareArtifactAction `json:"artifacts,omitempty"`
	// Metadata represents any other information which should be passed to the device.
	Metadata map[string]string `json:"metaData,omitempty"`
}
