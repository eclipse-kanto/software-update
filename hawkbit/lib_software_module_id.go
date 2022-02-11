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

// SoftwareModuleID represents an unique identifier for software modules.
type SoftwareModuleID struct {
	// Name for the software module.
	Name string `json:"name"`
	// Version of the software module.
	Version string `json:"version"`
}
