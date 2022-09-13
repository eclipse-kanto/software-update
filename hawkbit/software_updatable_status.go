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

// softwareUpdatableStatus represents the ability of a device to install and manage a certain type of software.
type softwareUpdatableStatus struct {
	// SoftwareModuleType represents the type of the software managed by this feature.
	SoftwareModuleType string `json:"softwareModuleType"`
	// InstalledDependencies list of all installed software managed by this feature.
	// The key should be the concatenated group.name:version.
	InstalledDependencies map[string]*DependencyDescription `json:"installedDependencies,omitempty"`
	// ContextDependencies represents an additional dependencies relevant for software
	// installation, e.g. hardware parts or runtimes like OSGi container or JREs.
	ContextDependencies map[string]*DependencyDescription `json:"contextDependencies,omitempty"`
	// LastOperation holds the last operation's status response.
	LastOperation *OperationStatus `json:"lastOperation"`
	// LastFailedOperation holds the last operation status indicating a finished with error.
	LastFailedOperation *OperationStatus `json:"lastFailedOperation"`
}
