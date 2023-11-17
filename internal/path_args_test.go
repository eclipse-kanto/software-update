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
	"flag"
	"io"
	"reflect"
	"testing"
)

func TestPathArgsIsSet(t *testing.T) {
	f := flag.NewFlagSet("testing", flag.ContinueOnError)

	var s []string
	v := NewPathArgs(&s)
	f.Var(v, "S", "S")

	args := []string{"-S=a b"}
	err := f.Parse(args)
	if err != nil {
		t.Errorf("Expected no error, but received %s", err)
	}

	if "a b" != v.String() {
		t.Errorf("Expected string value %s, but received value %s", "a b", v.String())
	}

	expected := []string{"a", "b"}
	if !reflect.DeepEqual(expected, s) {
		t.Errorf("Expected  %s, but received %s", expected, s)
	}
}

func TestPathArgsInvalid(t *testing.T) {
	f := flag.NewFlagSet("testing", flag.ContinueOnError)
	f.SetOutput(io.Discard)

	var s []string
	v := NewPathArgs(&s)
	f.Var(v, "S", "S")

	args := []string{"-S="}
	err := f.Parse(args)
	if err == nil {
		t.Errorf("Expected error, but not received")
	}
}
