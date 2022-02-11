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

package feature

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

type command struct {
	cmd  string
	args []string
}

func (i *command) String() string {
	if len(i.args) == 0 {
		return i.cmd
	}
	return fmt.Sprint(i.cmd, " ", strings.Join(i.args, " "))
}

func (i *command) Set(value string) error {
	if i.cmd == "" {
		i.cmd = value
		i.args = []string{}
		if runtime.GOOS != "windows" && strings.HasSuffix(value, ".sh") {
			i.cmd = "/bin/sh"
			i.args = []string{value}
		}
	} else {
		i.args = append(i.args, value)
	}
	return nil
}

func (i *command) run(dir string, def string) (err error) {
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
