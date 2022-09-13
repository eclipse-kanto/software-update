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

// Status is representing the failure, progress or sucess of the operation.
type Status string

// Supported operation statuses.
const (
	StatusStarted            Status = "STARTED"
	StatusDownloading        Status = "DOWNLOADING"
	StatusDownloadingWaiting Status = "DOWNLOADING_WAITING"
	StatusDownloaded         Status = "DOWNLOADED"
	StatusInstalling         Status = "INSTALLING"
	StatusInstallingWaiting  Status = "INSTALLING_WAITING"
	StatusInstalled          Status = "INSTALLED"
	StatusRemoving           Status = "REMOVING"
	StatusRemovingWaiting    Status = "REMOVING_WAITING"
	StatusRemoved            Status = "REMOVED"
	StatusCancelingWaiting   Status = "CANCELING_WAITING"
	StatusCancelRejected     Status = "CANCEL_REJECTED"
	StatusFinishedCanceled   Status = "FINISHED_CANCELED"
	StatusFinishedError      Status = "FINISHED_ERROR"
	StatusFinishedSuccess    Status = "FINISHED_SUCCESS"
	StatusFinishedWarning    Status = "FINISHED_WARNING"
	StatusFinishedRejected   Status = "FINISHED_REJECTED"
)
