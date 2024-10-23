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

// OperationStatus represents the status of an operation (install/remove) called on a device.
type OperationStatus struct {
	// CorrelationID is used for correlating the status-update with the operation called before.
	CorrelationID string `json:"correlationId"`
	// Status represents one of the predefined status, representing the failure, progress or sucess of the operation.
	Status Status `json:"status"`
	// SoftwareModule is required in the case of an install/download/cancel operation, absent in case of
	// remove or cancelRemove.
	SoftwareModule *SoftwareModuleID `json:"softwareModule,omitempty"`
	// Software is required for a remove or cancelRemove operation, absent in case of install/download/cancel.
	Software []*DependencyDescription `json:"software,omitempty"`
	// Progress represents the progress indicator in percentage.
	Progress *int `json:"progress,omitempty"`
	// Message from the device to give more context to the transmitted status.
	Message string `json:"message,omitempty"`
	// StatusCode represents a custom status code transmitted by the device.
	StatusCode string `json:"statusCode,omitempty"`
}

// NewOperationStatusUpdate returns an OperationStatus with the mandatory fields needed for software module update operation.
func NewOperationStatusUpdate(correlationID string, status Status, softwareModule *SoftwareModuleID) *OperationStatus {
	return &OperationStatus{CorrelationID: correlationID, Status: status, SoftwareModule: softwareModule}
}

// NewOperationStatusRemove returns an OperationStatus with the mandatory fields needed for software module remove operation.
func NewOperationStatusRemove(correlationID string, status Status,
	software ...*DependencyDescription) *OperationStatus {
	return &OperationStatus{CorrelationID: correlationID, Status: status, Software: software}
}

// WithCorrelationID sets the correlation id of the operation status.
func (os *OperationStatus) WithCorrelationID(correlationID string) *OperationStatus {
	os.CorrelationID = correlationID
	return os
}

// WithStatus sets the status of the operation status.
func (os *OperationStatus) WithStatus(status Status) *OperationStatus {
	os.Status = status
	return os
}

// WithSoftwareModule sets the software module id of the operation status.
func (os *OperationStatus) WithSoftwareModule(softwareModule *SoftwareModuleID) *OperationStatus {
	os.SoftwareModule = softwareModule
	return os
}

// WithSoftware sets the removed software to the operation status.
func (os *OperationStatus) WithSoftware(software ...*DependencyDescription) *OperationStatus {
	os.Software = software
	return os
}

// WithProgress sets the progress of the operation status.
func (os *OperationStatus) WithProgress(progress *int) *OperationStatus {
	os.Progress = progress
	return os
}

// WithMessage sets the message of the operation status.
func (os *OperationStatus) WithMessage(message string) *OperationStatus {
	os.Message = message
	return os
}

// WithStatusCode sets the status code of the operation status.
func (os *OperationStatus) WithStatusCode(statusCode string) *OperationStatus {
	os.StatusCode = statusCode
	return os
}
