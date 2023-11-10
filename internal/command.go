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

package feature

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

// Command is custom type of command name and arguments of command in order to add json unmarshal support
type Command struct {
	cmd  string
	args []string
}

// String is representation of Command as combination of name and arguments of the command
func (i *Command) String() string {
	if len(i.args) == 0 {
		return i.cmd
	}
	return fmt.Sprint(i.cmd, " ", strings.Join(i.args, " "))
}

// Set Command from string, used for flag set
func (i *Command) Set(value string) error {
	if i.cmd == "" {
		i.setCommand(value)
	} else {
		i.args = append(i.args, value)
	}
	return nil
}

func (i *Command) setCommand(value string) {
	i.cmd = value
	i.args = []string{}
	if runtime.GOOS != "windows" && strings.HasSuffix(value, ".sh") {
		i.cmd = "/bin/sh"
		i.args = []string{value}
	}
}

func (i *Command) run(dir string, def string) (err error) {
	script := i.cmd
	args := i.args
	if script == "" {
		if runtime.GOOS == "windows" {
			if script, err = filepath.Abs(filepath.Join(dir, def+".bat")); err != nil {
				return err
			}
			args = []string{}
		} else {
			script = "/bin/sh"
			args = []string{def + ".sh"}
		}
	}

	c := exec.Command(script, args...)
	c.Dir, err = filepath.Abs(dir)
	if err == nil {
		logger.Infof("Execute [%s] in directory: %v\n", c.Args, c.Dir)
		err = c.Run()
	}
	return err
}

// UnmarshalJSON unmarshal Command type
func (i *Command) UnmarshalJSON(b []byte) error {
	var v []string
	if err := json.Unmarshal(b, &v); err != nil {
		return err
	}

	for num, elem := range v {
		if num == 0 {
			i.setCommand(elem)
		} else {
			i.args = append(i.args, elem)
		}
	}
	return nil
}

// MarshalJSON supports marshalling to string format.
func (i Command) MarshalJSON() ([]byte, error) {
	return json.Marshal(i.String())
}
