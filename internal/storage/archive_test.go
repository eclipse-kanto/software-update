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
	"io/fs"
	"os"
	"path/filepath"
	"testing"
)

type ae struct {
	Name, Body string
}

// TestExtractArchive tests ExtractArchive functions.
func TestExtractArchive(t *testing.T) {
	// Prepare
	dir := "_tmp-extract"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	// Create test zip file.
	aZip := "test.zip"
	fZip := filepath.Join(dir, aZip)
	eZip := []ae{{"zf.txt", "zf1"}, {filepath.Join("zd", "zf.txt"), "zf2"}}
	createZip(fZip, eZip, t)

	// Create test tag.gz file.
	aTar := "test.tar.gz"
	eTar := []ae{{"tf.txt", "tf1"}, {filepath.Join("td", "tf.txt"), "tf2"}}
	createTar(filepath.Join(dir, aTar), eTar, t)

	// 1. Try to extract all archives.
	if err := ExtractArchive(dir); err != nil {
		t.Errorf("fail to extract all archives: %v", err)
	}
	isExtracted(dir, eZip, t)
	isExtracted(dir, eTar, t)

	// 2. Try to extract corrupted archive.
	if err := WriteLn(fZip, "corrupted"); err != nil {
		t.Fatalf("fail to write file: %v", err)
	}
	if err := ExtractArchive(dir); err == nil {
		t.Errorf("fail to validate with corrupted archive")
	}

	// 3. Try to extract archives from file (not directory).
	if err := ExtractArchive(fZip); err == nil {
		t.Errorf("fail to validate with file as target directory")
	}
}

// TestUnzip tests unzip functions.
func TestUnzip(t *testing.T) {
	// Prepare
	dir := "_tmp-zip"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	// 1. Try to extract correct zip file.
	z := "test.zip"
	entries := []ae{{"f.txt", "f1"}, {filepath.Join("d", "f.txt"), "s2"}, {"e/", ""}}
	createZip(filepath.Join(dir, z), entries, t)
	if err := unzip(dir, z); err != nil {
		t.Errorf("failed to extract zip archive: %v", err)
	}
	isExtracted(dir, entries, t)

	// 2. Try to extract zip archive with illegal file path.
	z = "illegal.zip"
	createZip(filepath.Join(dir, z), []ae{{"../i.txt", "file with illegal path"}}, t)
	if err := unzip(dir, z); err == nil {
		t.Error("illegal paths should not be permitted")
	}

	// 3. Try to extract missing zip archive.
	if err := unzip(dir, "missing.zip"); err == nil {
		t.Error("missing zip should not be permitted")
	}

	// 4. Try to extract file to directory.
	if err := os.MkdirAll(filepath.Join(dir, "dir"), 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}
	z = "fail.zip"
	createZip(filepath.Join(dir, z), []ae{{"dir", "a"}}, t)
	if err := unzip(dir, z); err == nil {
		t.Error("extracting file to directory should not be permitted")
	}
}

// TestUntar tests untar functions.
func TestUntar(t *testing.T) {
	// Prepare
	dir := "_tmp-tar.gz"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	// 1. Try to extract correct tar.gz file.
	z := "test.tar.gz"
	entries := []ae{{"f.txt", "f1"}, {filepath.Join("d", "f.txt"), "s2"}, {"e/", ""}}
	createTar(filepath.Join(dir, z), entries, t)
	if err := untar(dir, z); err != nil {
		t.Errorf("failed to extract tar.gz archive: %v", err)
	}
	isExtracted(dir, entries, t)

	// 2. Try to extract tar.gz archive with illegal file path.
	z = "illegal.tar.gz"
	createTar(filepath.Join(dir, z), []ae{{"../i.txt", "file with illegal path"}}, t)
	if err := untar(dir, z); err == nil {
		t.Error("illegal paths should not be permitted")
	}

	// 3. Try to extract missing tar.gz archive.
	if err := untar(dir, "missing.tar.gz"); err == nil {
		t.Error("missing tar.gz should not be permitted")
	}
}

// TestExtractAndRemove tests extractAndRemove functions.
func TestExtractAndRemove(t *testing.T) {
	// Prepare
	dir := "_tmp-archives"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	// 1. Try to extract correct zip file.
	z := "test.zip"
	entries := []ae{{"fz.txt", "fz"}}
	createZip(filepath.Join(dir, z), entries, t)
	if err := extractAndRemove(dir, z); err != nil {
		t.Errorf("failed to extract zip archive: %v", err)
	}
	isExtracted(dir, entries, t)
	if _, err := os.Stat(filepath.Join(dir, z)); !os.IsNotExist(err) {
		t.Errorf("archive file should be deleted")
	}

	// 2. Try to extract correct tar.gz file.
	z = "test.tar.gz"
	entries = []ae{{"fgz.txt", "fgz"}}
	createTar(filepath.Join(dir, z), entries, t)
	if err := extractAndRemove(dir, z); err != nil {
		t.Errorf("failed to extract tar.gz archive: %v", err)
	}
	isExtracted(dir, entries, t)
	if _, err := os.Stat(filepath.Join(dir, z)); !os.IsNotExist(err) {
		t.Errorf("archive file should be deleted")
	}

	// 3. Try to extract file with unknown extension.
	if err := extractAndRemove(dir, "unknown"); err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	// 4. Try to extract missing zip archive.
	if err := extractAndRemove(dir, "missing.zip"); err == nil {
		t.Error("missing tar.gz should not be permitted")
	}

	// 5. Try to extract missing tar.gz archive.
	if err := extractAndRemove(dir, "missing.tar.gz"); err == nil {
		t.Error("missing tar.gz should not be permitted")
	}
}

// ----- utils ----- ----- ----- ----- ----- ----- ----- ----- -----

func isExtracted(dir string, files []ae, t *testing.T) {
	for _, file := range files {
		name := filepath.Join(dir, file.Name)
		if _, err := os.Stat(name); os.IsNotExist(err) {
			t.Errorf("entry not extracted: %s", file.Name)
		} else if file.Name[len(file.Name)-1] != '/' {
			s, err := ReadLn(name)
			if err != nil {
				t.Errorf("fail to read extracted file [%s]: %v", file.Name, err)
			} else if s != file.Body {
				t.Errorf("file extracted with wrong content: %s -> [%s] != [%s]", file.Name, s, file.Body)
			}
		}
	}
}

func createZip(name string, files []ae, t *testing.T) {
	f, err := os.Create(name)
	if err != nil {
		t.Fatalf("failed to create zip archive: %v", err)
	}
	defer f.Close()
	w := zip.NewWriter(f)
	defer w.Close()

	for _, file := range files {
		fh := &zip.FileHeader{Name: file.Name}
		if file.Name[len(file.Name)-1] == '/' {
			fh.SetMode(fs.ModeDir | 0755)
			if _, err := w.CreateHeader(fh); err != nil {
				t.Fatalf("failed to create zip archive directory entry: %v", err)
			}
		} else {
			fh.SetMode(0644)
			fh.Method = zip.Deflate
			f, err := w.CreateHeader(fh)
			if err != nil {
				t.Fatalf("failed to create zip archive file entry: %v", err)
			}
			if _, err = f.Write([]byte(file.Body)); err != nil {
				t.Fatalf("failed to write zip archive file entry: %v", err)
			}
		}
	}
}

func createTar(name string, files []ae, t *testing.T) {
	f, err := os.Create(name)
	if err != nil {
		t.Fatalf("failed to create tar.gz archive: %v", err)
	}
	defer f.Close()
	gw := gzip.NewWriter(f)
	defer gw.Close()
	tw := tar.NewWriter(gw)
	defer tw.Close()

	for _, file := range files {
		fh := &tar.Header{Name: file.Name}
		if file.Name[len(file.Name)-1] == '/' {
			fh.Typeflag = tar.TypeDir
			fh.Mode = 0755
			if err := tw.WriteHeader(fh); err != nil {
				t.Fatalf("failed to create tar archive directory entry: %v", err)
			}
		} else {
			fh.Mode = 0644
			fh.Size = int64(len(file.Body))
			if err := tw.WriteHeader(fh); err != nil {
				t.Fatalf("failed to create tar archive file entry: %v", err)
			}
			if _, err = tw.Write([]byte(file.Body)); err != nil {
				t.Fatalf("failed to write tar archive file entry: %v", err)
			}
			tw.Flush()
		}
	}
}
