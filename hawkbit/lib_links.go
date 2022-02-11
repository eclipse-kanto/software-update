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

// Links represents the datatype for the artifact links.
type Links struct {
	// URL to download the artifact.
	URL string `json:"url"`
	// MD5URL to download the MD5SUM file.
	MD5URL string `json:"md5url,omitempty"`
}
