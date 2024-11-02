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

// Software DependencyDescription structure keys
const (
	softwareGroupParam   = "group"
	softwareNameParam    = "name"
	softwareVersionParam = "version"
	softwareTypeParam    = "type"
)

// DependencyDescription describes an installed software or other dependencies for a device.
type DependencyDescription struct {
	// Group represents an identifier which groups the dependency into a certain category.
	Group string `json:"group"`
	// Name represents the dependency name.
	Name string `json:"name"`
	// Version Name represents the dependency version.
	Version string `json:"version"`
	// Type represents a "category" classifier for the dependency.
	Type string `json:"type,omitempty"`
}
