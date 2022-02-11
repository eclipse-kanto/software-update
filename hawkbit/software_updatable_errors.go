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

import "fmt"

type dittoError string

const (
	// ditto errors
	messagesParameterInvalid dittoError = "messages:parameter.invalid"

	// error codes
	responseStatusBadRequest = 400

	thingErrorStringFormat = "[%d][%s] %s"
)

// thingError defined Things related errors
type thingError struct {
	ErrorCode dittoError `json:"error"`
	Status    int        `json:"status"`
	Message   string     `json:"message"`
}

func (thErr *thingError) Error() string {
	return fmt.Sprintf(thingErrorStringFormat, thErr.Status, thErr.ErrorCode, thErr.Message)
}

// newMessagesParameterInvalidError define and error for invalid parameters
func newMessagesParameterInvalidError(messageFormat string, args ...interface{}) *thingError {
	return &thingError{
		ErrorCode: messagesParameterInvalid,
		Status:    responseStatusBadRequest,
		Message:   fmt.Sprintf(messageFormat, args...),
	}
}
