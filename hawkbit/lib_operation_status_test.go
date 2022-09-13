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

import (
	"testing"
)

// TestOperationStatusUpdate tests the creation of OperationStatus for install, download and cancel operations.
func TestOperationStatusUpdate(t *testing.T) {
	cid := "correlation-id"
	sid := &SoftwareModuleID{Name: "name", Version: "1.0.0"}
	progress := 99
	msg := "test message"
	code := "status-code"

	// 1. Test correlation identifier initial and with values.
	ops := NewOperationStatusUpdate(cid, StatusFinishedSuccess, sid)
	if ops.CorrelationID != cid {
		t.Errorf("correlation identifier mishmash: %v != %v", ops.CorrelationID, cid)
	}
	if ops.WithCorrelationID("").CorrelationID != "" {
		t.Errorf("unexpected correlation identifier: %v", ops.CorrelationID)
	}

	// 2. Test status initial and with values.
	if ops.Status != StatusFinishedSuccess {
		t.Errorf("status mishmash: %v != %v", ops.Status, StatusFinishedSuccess)
	}
	if ops.WithStatus(StatusFinishedError).Status != StatusFinishedError {
		t.Errorf("status mishmash: %v != %v", ops.Status, StatusFinishedError)
	}

	// 3. Test software module identifier initial and with values.
	if ops.SoftwareModule.Name != sid.Name {
		t.Errorf("software module name mishmash: %v != %v", ops.SoftwareModule.Name, sid.Name)
	}
	if ops.SoftwareModule.Version != sid.Version {
		t.Errorf("software module version mishmash: %v != %v", ops.SoftwareModule.Version, sid.Version)
	}
	if ops.WithSoftwareModule(nil).SoftwareModule != nil {
		t.Errorf("unexpected software module identifier: %v", ops.SoftwareModule)
	}

	// 4. Test WithProgress value.
	if ops.WithProgress(progress).Progress != progress {
		t.Errorf("progress mishmash: %v != %v", ops.Progress, progress)
	}

	// 5. Test WithMessage value.
	if ops.WithMessage(msg).Message != msg {
		t.Errorf("message mishmash: %v != %v", ops.Message, msg)
	}

	// 6. Test WithStatusCode value.
	if ops.WithStatusCode(code).StatusCode != code {
		t.Errorf("status code mishmash: %v != %v", ops.StatusCode, code)
	}
}

// TestNewOperationStatusRemove tests the creation of OperationStatus for remove and cancel remove operations.
func TestNewOperationStatusRemove(t *testing.T) {
	cid := "correlation-id"
	sw := []*DependencyDescription{{Name: "name"}}

	// 1. Test correlation identifier initial and with values.
	ops := NewOperationStatusRemove(cid, StatusFinishedSuccess, sw...)
	if ops.CorrelationID != cid {
		t.Errorf("correlation identifier mishmash: %v != %v", ops.CorrelationID, cid)
	}
	// 1.1 Test the WithCorrelationID func.
	if ops.WithCorrelationID("").CorrelationID != "" {
		t.Errorf("unexpected correlation identifier: %v", ops.CorrelationID)
	}

	// 2. Test status initial and with values.
	if ops.Status != StatusFinishedSuccess {
		t.Errorf("status mishmash: %v != %v", ops.Status, StatusFinishedSuccess)
	}
	// 2.1 Test the WithStatus func.
	if ops.WithStatus(StatusFinishedError).Status != StatusFinishedError {
		t.Errorf("status mishmash: %v != %v", ops.Status, StatusFinishedError)
	}

	// 3. Test software initial and with values.
	if len(ops.Software) != 1 {
		t.Errorf("only one software was expected: %v", ops.Software)
	}
	// 3.1 Test the WithSoftware func.
	if len(ops.WithSoftware().Software) != 0 {
		t.Errorf("unexpected software: %v", ops.Software)
	}
}
