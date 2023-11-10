// Copyright (c) 2022 Contributors to the Eclipse Foundation
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

package feature

import (
	"encoding/json"
	"errors"
	"time"
)

// DurationTime is custom type of type time.Duration in order to add json unmarshal support
type DurationTime struct {
	time.Duration
}

// UnmarshalJSON unmarshal DurationTime type
func (d *DurationTime) UnmarshalJSON(b []byte) error {
	var v interface{}
	if err := json.Unmarshal(b, &v); err != nil {
		return err
	}

	switch value := v.(type) {
	case float64:
		d.Duration = time.Duration(value) * time.Second
		return nil
	case string:
		var err error
		d.Duration, err = time.ParseDuration(value)
		if err != nil {
			return err
		}
	default:
		return errors.New("invalid duration")
	}
	return nil
}

// MarshalJSON supports marshalling to '50s' string format.
func (d DurationTime) MarshalJSON() ([]byte, error) {
	return json.Marshal(d.String())
}

// Set durationTime from string, used for flag set
func (d *DurationTime) Set(s string) error {
	v, err := time.ParseDuration(s)
	if err != nil {
		err = errors.New("parse error")
	}
	d.Duration = v
	return err
}

func (d DurationTime) String() string {
	return d.Duration.String()
}
