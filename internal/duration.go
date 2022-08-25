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

//Duration is custom type of type time.Duration in order to add json unmarshal support
type Duration time.Duration

//UnmarshalJSON unmarshal duration type
func (d *Duration) UnmarshalJSON(b []byte) error {
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
		*d = Duration(duration)
	default:
		return errors.New("invalid duration")
	}
	return nil
}

//Set duration set from string, used for flag set
func (d *Duration) Set(s string) error {
	v, err := time.ParseDuration(s)
	if err != nil {
		err = errors.New("parse error")
	}
	*d = Duration(v)
	return err
}

func (d Duration) String() string {
	return time.Duration(d).String()
}
