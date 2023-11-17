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
	"fmt"
	"strings"
)

type pathArgs struct {
	args *[]string
}

func (a *pathArgs) String() string {
	if a.args == nil {
		return ""
	}
	return strings.Join(*a.args, " ")
}

func (a *pathArgs) Set(value string) error {
	if len(value) == 0 {
		return fmt.Errorf("value cannot be empty")
	}
	*a.args = strings.Fields(value)
	return nil
}

// NewPathArgs creates new flag variable for slice of strings definition.
func NewPathArgs(setter *[]string) *pathArgs {
	return &pathArgs{
		args: setter,
	}
}
