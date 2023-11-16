// Copyright (c) 2023 Contributors to the Eclipse Foundation
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
	"fmt"
	"strings"
)

// StringSliceV represents slice of strings flag value.
type StringSliceV struct {
	value *[]string
}

// NewStringSliceV creates new flag variable for slice of strings definition.
func NewStringSliceV(setter *[]string) *StringSliceV {
	return &StringSliceV{
		value: setter,
	}
}

// String returns the flag string value.
func (f *StringSliceV) String() string {
	if f.value == nil {
		return ""
	}
	return strings.Join(*f.value, " ")
}

// Set validates and applies the provided value if no error.
func (f *StringSliceV) Set(value string) error {
	if len(value) == 0 {
		return fmt.Errorf("value cannot be empty")
	}
	*f.value = strings.Fields(value)
	return nil
}
