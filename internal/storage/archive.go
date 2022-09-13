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

package storage

import (
	"archive/tar"
	"archive/zip"
	"compress/gzip"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/eclipse-kanto/software-update/internal/logger"
)

type reader func() (io.Reader, error)

// ExtractArchive all artifacts to file system and remove the archive.
func ExtractArchive(dir string) error {
	logger.Debugf("Extract archive(s) in directory: %s", dir)
	files, err := os.ReadDir(dir)
	if err != nil {
		return err
	}

	for _, file := range files {
		if file.Type().IsRegular() {
			if err := extractAndRemove(dir, file.Name()); err != nil {
				return err
			}
		}
	}
	return nil
}

func extractAndRemove(dir string, name string) error {
	if strings.HasSuffix(name, ".zip") {
		if err := unzip(dir, name); err != nil {
			return err
		}
		logger.Debugf("Remove archive: %s", name)
		return os.Remove(filepath.Join(dir, name))
	}
	if strings.HasSuffix(name, ".tar.gz") {
		if err := untar(dir, name); err != nil {
			return err
		}
		logger.Debugf("Remove archive: %s", name)
		return os.Remove(filepath.Join(dir, name))
	}
	return nil
}

func unzip(dir string, name string) error {
	logger.Debugf("Unzip archive [%s] in directory: %s", name, dir)
	file, err := zip.OpenReader(filepath.Join(dir, name))
	if err != nil {
		return err
	}
	defer file.Close()

	for _, f := range file.File {
		if err := processEntry(dir, f.Name, f.FileInfo(), func() (io.Reader, error) {
			return f.Open()
		}); err != nil {
			return err
		}
	}
	return nil
}

func untar(dir string, name string) error {
	logger.Debugf("Untar archive [%s] in directory: %s", name, dir)
	r, err := os.Open(filepath.Join(dir, name))
	if err != nil {
		return err
	}
	defer r.Close()

	file, err := gzip.NewReader(r)
	if err != nil {
		return err
	}
	defer file.Close()

	tr := tar.NewReader(file)
	for {
		header, err := tr.Next()
		if err == io.EOF {
			return nil
		}
		if err != nil {
			return err
		}

		if err := processEntry(dir, header.Name, header.FileInfo(), func() (io.Reader, error) {
			return tr, nil
		}); err != nil {
			return err
		}
	}
}

func processEntry(dir string, name string, f fs.FileInfo, in reader) error {
	dest := filepath.Join(dir, name)
	if !strings.HasPrefix(dest, filepath.Clean(dir)+string(os.PathSeparator)) {
		return fmt.Errorf("illegal file path: %s", name)
	}

	if f.Mode().IsDir() {
		logger.Tracef("Create directory: %s", name)
		return os.MkdirAll(dest, f.Mode())
	}

	logger.Tracef("Extract: %s", name)
	input, err := in()
	if err != nil {
		return err
	}
	return saveToFile(input, dest, f.Mode())
}

func saveToFile(in io.Reader, dest string, perm os.FileMode) error {
	// Close the input in case of io.ReadCloser (Zip).
	if closer, ok := in.(io.ReadCloser); ok {
		defer closer.Close()
	}

	// Make parent directories if missing.
	if err := os.MkdirAll(filepath.Dir(dest), 0755); err != nil {
		return err
	}

	// Open system file.
	out, err := os.OpenFile(dest, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, perm)
	if err != nil {
		return err
	}
	defer out.Close()

	// Copy archive file to the system file.
	_, err = io.Copy(out, in)
	return err
}
