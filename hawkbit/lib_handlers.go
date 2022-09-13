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

// InstallHandler represents a callback handler that is called on each received install message.
type InstallHandler func(update *SoftwareUpdateAction, softwareUpdatable *SoftwareUpdatable)

// DownloadHandler represents a callback handler that is called on each received download message.
type DownloadHandler func(update *SoftwareUpdateAction, softwareUpdatable *SoftwareUpdatable)

// CancelHandler represents a callback handler that is called on each received cancel message.
type CancelHandler func(update *SoftwareUpdateAction, softwareUpdatable *SoftwareUpdatable)

// RemoveHandler represents a callback handler that is called on each received remove message.
type RemoveHandler func(software *SoftwareRemoveAction, softwareUpdatable *SoftwareUpdatable)

// CancelRemoveHandler represents a callback handler that is called on each received cancel remove message.
type CancelRemoveHandler func(software *SoftwareRemoveAction, softwareUpdatable *SoftwareUpdatable)
