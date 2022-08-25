// Copyright (c) 2022 Contributors to the Eclipse Foundation
//
// See the NOTICE file(s) distributed with this work for additional
// information regarding copyright ownership.
//
// This program and the accompanying materials are made available under the
// terms of the Eclipse Public License 2.0 which is available at
// http://www.eclipse.org/legal/epl-2.0
//
// SPDX-License-Identifier: EPL-2.0

package feature

import (
	"encoding/json"
	"errors"
	"time"
)

//durationTime is custom type of type time.durationTime in order to add json unmarshal support
type durationTime time.Duration

//UnmarshalJSON unmarshal durationTime type
func (d *durationTime) UnmarshalJSON(b []byte) error {
	var v interface{}
	if err := json.Unmarshal(b, &v); err != nil {
		return err
	}
	switch value := v.(type) {

	case string:
		duration, err := time.ParseDuration(value)
		if err != nil {
			return err
		}
		*d = durationTime(duration)
	default:
		return errors.New("invalid duration")
	}
	return nil
}

//Set durationTime from string, used for flag set
func (d *durationTime) Set(s string) error {
	v, err := time.ParseDuration(s)
	if err != nil {
		err = errors.New("parse error")
	}
	*d = durationTime(v)
	return err
}

func (d durationTime) String() string {
	return time.Duration(d).String()
}
