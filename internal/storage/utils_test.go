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
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/eclipse-kanto/software-update/hawkbit"
)

type artifactData struct {
	hash     hawkbit.Hash
	protocol hawkbit.Protocol
	local    bool
	copy     bool
}

// ----- ReadLn & WriteLn ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestReadWriteLn tests both ReadLn and WriteLn functions.
func TestReadWriteLn(t *testing.T) {
	// Prepare
	dir := "_tmp-ln"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	// 1. Validate ReadLn for missing file.
	name := filepath.Join(dir, "test.me")
	if s, _ := ReadLn(name); s != "" {
		t.Errorf("read from missing file: %s", s)
	}

	// 2. Validate WriteLn and ReadLine.
	if err := WriteLn(name, "test-string-\n-with new line"); err != nil {
		t.Fatalf("failed to write to file: %v", err)
	}
	if s, _ := ReadLn(name); s != "test-string-" {
		t.Errorf("wrong text was read [%s]", s)
	}

	// 3. Validate WriteLn fail. Write to a missing directory.
	fake := filepath.Join(dir, "fake-dir", "test.me")
	WriteLn(fake, "test")
	if _, err := os.Stat(fake); !os.IsNotExist(err) {
		t.Error("write should not create missing parent directories")
	}

	// 4. Validate ReadLn with directory (not file).
	if s, _ := ReadLn(dir); s != "" {
		t.Errorf("cannot read direcotry: %s", s)
	}
}

// ----- FindAvailableLocation ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestFindAvailableLocation tests FindAvailableLocation functions.
func TestFindAvailableLocation(t *testing.T) {
	// Prepare
	dir := "_tmp-find"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	// 1. Search for available directory in empty directory.
	t.Run("InEmptyDirectory", func(t *testing.T) { validateFindAvailableLocation(dir, "test", "0", t) })

	// 2. Search for available directory in directory with reserved subdirectory.
	t.Run("WithExistingIndexes", func(t *testing.T) { validateFindAvailableLocation(dir, "0", "1", t) })

	// 3. Validate when some indexes are missing.
	t.Run("WithSkippedIndexes", func(t *testing.T) { validateFindAvailableLocation(dir, "3", "4", t) })

	// 4. Validate when searching for directory in a file.
	t.Run("InFile-Error", func(t *testing.T) {
		parent := filepath.Join(dir, "5")
		if err := WriteLn(parent, ""); err != nil {
			t.Fatalf("failed create temporary file [%s]: %v", parent, err)
		}
		if l, err := FindAvailableLocation(parent); err == nil {
			t.Errorf("location found in file [%s]: %s", parent, l)
		}
	})

	// 5. Search in missing directory.
	t.Run("InMissingDirectory-Error", func(t *testing.T) {
		parent := filepath.Join(dir, "missing")
		if l, err := FindAvailableLocation(parent); err == nil {
			t.Errorf("location found in missing directory [%s]: %s", parent, l)
		}
	})
}

func validateFindAvailableLocation(parent string, create string, expected string, t *testing.T) string {
	// Create test directory.
	if err := os.MkdirAll(filepath.Join(parent, create), 0755); err != nil {
		t.Fatalf("failed create temporary directory [%s]: %v", create, err)
	}
	// Try to find available directory
	actual, err := FindAvailableLocation(parent)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Validate that found directory is the expected one.
	if actual != filepath.Join(parent, expected) {
		t.Errorf("found path was incorrect: %s != %s", filepath.Join(parent, expected), actual)
	}
	// Validate that found directory to not exists.
	if _, err := os.Stat(actual); !os.IsNotExist(err) {
		t.Errorf("destination directory exists: %s", actual)
	}
	return actual
}

// ----- loadInstalledDep ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestLoadInstalledDep tests loadInstalledDep function.
func TestLoadInstalledDep(t *testing.T) {
	// Prepare
	dir := "_tmp"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}
	dep := filepath.Join(dir, "test.prs")

	// remove temporary directory at the end
	defer os.RemoveAll(dir)

	// 1. Read installed dependenciy from missing file.
	if _, err := loadInstalledDep(dep); err == nil {
		t.Error("read installed dependency from missing file")
	}

	// 2. Read installed dependenciy.
	expected := hawkbit.DependencyDescription{Group: "group", Name: "name", Version: "1.0.0", Type: "type"}
	WriteLn(dep, fmt.Sprintf("group=%s\nname=%s\nversion=%s\ntype=%s",
		expected.Group, expected.Name, expected.Version, expected.Type))
	actual, err := loadInstalledDep(dep)
	if err != nil {
		t.Errorf("failed to read installed dependency: %v", err)
	}
	validateInstalledDep(expected, *actual, t)

	// 3. Read installed dependenciy without group.
	WriteLn(dep, "name=name\nversion=1.0.0")
	if _, err = loadInstalledDep(dep); err == nil {
		t.Error("read installed dependency without group")
	}
}

func validateInstalledDep(expected hawkbit.DependencyDescription, actual hawkbit.DependencyDescription, t *testing.T) {
	if expected.Group != actual.Group {
		t.Errorf("wrong installed dependency group: %s != %s", expected.Group, actual.Group)
	}
	if expected.Name != actual.Name {
		t.Errorf("wrong installed dependency name: %s != %s", expected.Name, actual.Name)
	}
	if expected.Version != actual.Version {
		t.Errorf("wrong installed dependency version: %s != %s", expected.Version, actual.Version)
	}
	if expected.Type != actual.Type {
		t.Errorf("wrong installed dependency type: %s != %s", expected.Type, actual.Type)
	}
}

// ----- loadSoftwareUpdatable ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestLoadSoftwareUpdatable tests loadSoftwareUpdatable function.
func TestSaveLoadSoftwareUpdatable(t *testing.T) {
	// Prepare
	dir := "_tmp"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed create temporary directory: %v", err)
	}
	su := filepath.Join(dir, SoftwareUpdatableName)
	fake := filepath.Join(dir, "fake")
	WriteLn(fake, "fake")

	h1 := hawkbit.SoftwareModuleAction{
		SoftwareModule: &hawkbit.SoftwareModuleID{Name: "m1", Version: "1.0.0"},
		Artifacts:      []*hawkbit.SoftwareArtifactAction{}}
	m1, err := toModule(h1)
	if err != nil {
		t.Fatalf("fail to convert module [%s:%s]", m1.Name, m1.Version)
	}

	h2 := hawkbit.SoftwareModuleAction{
		SoftwareModule: &hawkbit.SoftwareModuleID{Name: "m2", Version: "2.0.0"},
		Artifacts:      []*hawkbit.SoftwareArtifactAction{}}
	m2, err := toModule(h2)
	if err != nil {
		t.Fatalf("fail to convert software updatable [%s:%s]", m1.Name, m1.Version)
	}
	expected := &Updatable{Operation: "test", CorrelationID: "test-correlation-id", Modules: []*Module{m1, m2}}

	// remove temporary directory at the end
	defer os.RemoveAll(dir)

	// 1. Read software updatable from missing file.
	if _, err := loadSoftwareUpdatable(su); err == nil {
		t.Error("read software updatable from missing file")
	}

	// 2. Read software updatable from directory.
	if _, err := loadSoftwareUpdatable(dir); err == nil {
		t.Error("read software updatable from directory")
	}

	// 3. Read software updatable from file with wrong format.
	if _, err := loadSoftwareUpdatable(fake); err == nil {
		t.Error("read software updatable from file with wrong format")
	}

	// 4. Save software updatable to wrong file path.
	if _, err := SaveSoftwareUpdatable("", "", filepath.Join(fake, "fake-file"), nil); err == nil {
		t.Error("save software updatable to file with wrong path")
	}

	// 5. Save software updatable.
	actual, err := SaveSoftwareUpdatable(expected.Operation, expected.CorrelationID, su,
		[]*hawkbit.SoftwareModuleAction{&h1, &h2})
	if err != nil {
		t.Fatalf("fail to save software updatable: %v", err)
	}
	validateSoftwareUpdatable(expected, actual, t)

	// 6. Load software updatable.
	actual, err = loadSoftwareUpdatable(su)
	if err != nil {
		t.Fatalf("fail to load software updatable: %v", err)
	}
	validateSoftwareUpdatable(expected, actual, t)

	// 7. Save software updatable with wrong artifact.
	a := &hawkbit.SoftwareArtifactAction{Checksums: make(map[hawkbit.Hash]string)}
	m := []*hawkbit.SoftwareModuleAction{{
		SoftwareModule: &hawkbit.SoftwareModuleID{Name: "m3", Version: "3"},
		Artifacts:      []*hawkbit.SoftwareArtifactAction{a}}}
	if _, err = SaveSoftwareUpdatable("", "", su, m); err == nil {
		t.Error("save software updatable with wrong artifact")
	}
}

func validateSoftwareUpdatable(expected *Updatable, actual *Updatable, t *testing.T) {
	if expected.Operation != actual.Operation {
		t.Errorf("wrong software updatable operation: %s != %s", expected.Operation, actual.Operation)
	}
	if expected.CorrelationID != actual.CorrelationID {
		t.Errorf("wrong software updatable correlation-id: %s != %s", expected.CorrelationID, actual.CorrelationID)
	}
	if len(expected.Modules) != len(actual.Modules) {
		t.Errorf("wrong number of modules: %v != %v", len(expected.Modules), len(actual.Modules))
	}
	for i, e := range expected.Modules {
		a := actual.Modules[i]
		if e.Name != a.Name {
			t.Errorf("wrong module name: %s != %s", e.Name, a.Name)
		}
		if e.Version != a.Version {
			t.Errorf("wrong module version: %v != %v", e.Version, a.Version)
		}
	}
}

// ----- toModule ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestToModule tests toModule function.
func TestToModule(t *testing.T) {
	a1 := &hawkbit.SoftwareArtifactAction{
		Filename:  "test1.txt",
		Size:      111,
		Checksums: make(map[hawkbit.Hash]string),
		Download:  make(map[hawkbit.Protocol]*hawkbit.Links),
	}
	a1.Checksums[hawkbit.MD5] = "md5-value"
	a1.Download[hawkbit.HTTPS] = &hawkbit.Links{URL: "https://test.me", MD5URL: ""}

	a2 := &hawkbit.SoftwareArtifactAction{
		Filename:  "test2.txt",
		Size:      222,
		Checksums: make(map[hawkbit.Hash]string),
		Download:  make(map[hawkbit.Protocol]*hawkbit.Links),
	}
	a2.Checksums[hawkbit.SHA1] = "sha1-value"
	a2.Download[hawkbit.HTTP] = &hawkbit.Links{URL: "http://test.me", MD5URL: ""}

	a3 := &hawkbit.SoftwareArtifactAction{
		Filename:  "test3.txt",
		Size:      333,
		Checksums: make(map[hawkbit.Hash]string),
		Download:  make(map[hawkbit.Protocol]*hawkbit.Links),
	}
	a3.Checksums[hawkbit.SHA1] = "sha1-value"
	a3.Download[ProtocolFile] = &hawkbit.Links{URL: "/var/tmp/test3.txt", MD5URL: ""}

	a4 := &hawkbit.SoftwareArtifactAction{
		Filename:  "test4.txt",
		Size:      444,
		Checksums: make(map[hawkbit.Hash]string),
		Download:  make(map[hawkbit.Protocol]*hawkbit.Links),
	}
	a4.Checksums[hawkbit.SHA1] = "sha1-value"
	a4.Download[ProtocolFile] = &hawkbit.Links{URL: "/var/tmp/test4.txt", MD5URL: ""}

	expected := hawkbit.SoftwareModuleAction{
		SoftwareModule: &hawkbit.SoftwareModuleID{Name: "name", Version: "1.0.0"},
		Artifacts:      []*hawkbit.SoftwareArtifactAction{a1, a2, a3, a4},
		Metadata:       map[string]string{"copy-artifacts": "test3.txt,invalid.txt"},
	}

	// 1. Validate with two correct artifacts
	actual, err := toModule(expected)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	eah := []artifactData{
		{hash: hawkbit.MD5, protocol: hawkbit.HTTPS, local: false, copy: false},
		{hash: hawkbit.SHA1, protocol: hawkbit.HTTP, local: false, copy: false},
		{hash: hawkbit.SHA1, protocol: ProtocolFile, local: true, copy: true},
		{hash: hawkbit.SHA1, protocol: ProtocolFile, local: true, copy: false},
	}
	validateModule(expected, actual, eah, t)

	// 2. Validate with two correct artifacts and one wrong artifact
	a5 := &hawkbit.SoftwareArtifactAction{
		Filename:  "test5.txt",
		Size:      555,
		Checksums: make(map[hawkbit.Hash]string),
		Download:  make(map[hawkbit.Protocol]*hawkbit.Links),
	}
	expected.Artifacts = append(expected.Artifacts, a5)
	if _, err = toModule(expected); err == nil {
		t.Errorf("an error was expected for wrong artifact")
	}
}

// ----- toArtifact ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestToArtifact tests toArtifact function.
func TestToArtifact(t *testing.T) {
	expected := &hawkbit.SoftwareArtifactAction{
		Filename:  "test.txt",
		Size:      123,
		Checksums: make(map[hawkbit.Hash]string),
		Download:  make(map[hawkbit.Protocol]*hawkbit.Links),
	}
	expected.Checksums[hawkbit.MD5] = "md5-value"
	expected.Download[hawkbit.FTP] = &hawkbit.Links{URL: "ftp://test.me", MD5URL: ""}
	expected.Download[hawkbit.SFTP] = &hawkbit.Links{URL: "sftp://test.me", MD5URL: ""}
	expected.Download[hawkbit.HTTP] = &hawkbit.Links{URL: "http://test.me", MD5URL: ""}

	// 1. Validate with MD5 and HTTP
	actual, err := toArtifact(expected, false)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	validateArtifact(expected, actual, artifactData{hash: hawkbit.MD5, protocol: hawkbit.HTTP}, t)

	// 2. Validate with SHA1 and HTTPS
	expected.Checksums[hawkbit.SHA1] = "sha1-value"
	expected.Download[hawkbit.HTTPS] = &hawkbit.Links{URL: "https://test.me", MD5URL: ""}
	actual, err = toArtifact(expected, false)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	validateArtifact(expected, actual, artifactData{hash: hawkbit.SHA1, protocol: hawkbit.HTTPS}, t)

	// 3. Validate with SHA256 and HTTPS
	expected.Checksums[hawkbit.SHA256] = "sha256-value"
	actual, err = toArtifact(expected, false)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	validateArtifact(expected, actual, artifactData{hash: hawkbit.SHA256, protocol: hawkbit.HTTPS}, t)

	// 4. Validate for unknown/missing Hash
	expected.Checksums = make(map[hawkbit.Hash]string)
	if _, err = toArtifact(expected, false); err == nil {
		t.Errorf("an error was expected for unknown or missing hash")
	}

	// 5. Validate for unknown/missing link
	expected.Download = make(map[hawkbit.Protocol]*hawkbit.Links)
	expected.Download[hawkbit.FTP] = &hawkbit.Links{URL: "ftp://test.me", MD5URL: ""}
	if _, err = toArtifact(expected, false); err == nil {
		t.Errorf("an error was expected for unknown or missing link")
	}
}

func validateModule(expected hawkbit.SoftwareModuleAction, actual *Module, ahs []artifactData, t *testing.T) {
	if expected.SoftwareModule.Name != actual.Name {
		t.Errorf("wrong module name: %s != %s", expected.SoftwareModule.Name, actual.Name)
	}
	if expected.SoftwareModule.Version != actual.Version {
		t.Errorf("wrong module version: %v != %v", expected.SoftwareModule.Version, actual.Version)
	}
	if len(expected.Artifacts) != len(actual.Artifacts) {
		t.Errorf("wrong number of artifacts in a module: %v != %v", len(expected.Artifacts), len(actual.Artifacts))
	}
	for i, v := range expected.Artifacts {
		validateArtifact(v, actual.Artifacts[i], ahs[i], t)
	}
}

func validateArtifact(expected *hawkbit.SoftwareArtifactAction, actual *Artifact, ah artifactData, t *testing.T) {
	if expected.Filename != actual.FileName {
		t.Errorf("wrong artifact file name: %s != %s", expected.Filename, actual.FileName)
	}
	if expected.Size != actual.Size {
		t.Errorf("wrong artifact size: %v != %v", expected.Size, actual.Size)
	}
	if string(ah.hash) != actual.HashType {
		t.Errorf("wrong artifact hash type: %v != %v", string(ah.hash), actual.HashType)
	}
	if expected.Checksums[ah.hash] != actual.HashValue {
		t.Errorf("wrong artifact hash value: %v != %v", expected.Checksums[ah.hash], actual.HashValue)
	}
	if expected.Download[ah.protocol].URL != actual.Link {
		t.Errorf("wrong artifact link: %v != %v", expected.Download[ah.protocol], actual.Link)
	}
	if ah.copy != actual.Copy {
		t.Errorf("wrong artifact copy policy: %v != %v", ah.copy, actual.Copy)
	}
}

// ----- move & searchAndMove ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestMove tests move function.
func TestMove(t *testing.T) {
	// Prepare one module in a temporary directory
	r := "_tmp"
	src := filepath.Join(r, "1")
	dest := filepath.Join(r, "2")
	createModule(src, "test", "1.0.0", t)

	// remove temporary directory at the end
	defer os.RemoveAll(r)

	// Create destination directory
	if err := os.MkdirAll(filepath.Join(dest, "installed-deps"), 0755); err != nil {
		t.Fatalf("failed create destination directory: %v", err)
	}

	// 1. Validate module moving.
	if err := move(src, dest); err != nil {
		t.Fatalf("failed to move directory: %v", err)
	}
	validateModuleDirectory(dest, "test", "1.0.0", true, t)

	// 2. Validate moving of missing directory
	if err := move("missing", dest); err == nil {
		t.Fatal("cannot move missing directory. error was expected")
	}
}

// TestSearchAndMove tests searchAndMove function.
func TestSearchAndMove(t *testing.T) {
	// Prepare one module in a temporary directory
	r := "_tmp"
	src := filepath.Join(r, "1")
	dest := filepath.Join(r, "2")
	module := &Module{Name: "test", Version: "1.0.0"}
	createModule(src, module.Name, module.Version, t)

	// remove temporary directory at the end
	defer os.RemoveAll(r)

	// 1. Try search in missing directory.
	searchAndMove(dest, dest, module)
	if _, err := os.Stat(dest); !os.IsNotExist(err) {
		t.Error("destination directory should not be created")
	}

	// 2. Try search for module in a file.
	file := filepath.Join(r, "file")
	WriteLn(file, "test")
	searchAndMove(file, dest, module)
	if s, err := os.Stat(dest); !os.IsNotExist(err) && s.IsDir() {
		t.Error("cannot search for module in a file")
	}

	// 3. Try to move existing module to missing directory.
	searchAndMove(r, dest, module)
	if _, err := os.Stat(dest); !os.IsNotExist(err) {
		t.Error("destination directory should not be created")
	}

	// 4. Move existing module.
	if err := os.MkdirAll(dest, 0755); err != nil {
		t.Fatalf("failed create destination directory: %v", err)
	}
	searchAndMove(r, dest, module)
	validateModuleDirectory(dest, module.Name, module.Version, false, t)
}

func createModule(dir string, name string, version string, t *testing.T) {
	if err := os.MkdirAll(filepath.Join(dir, "installed-deps"), 0755); err != nil {
		t.Fatalf("failed to create module directory: %v", err)
	}
	WriteLn(filepath.Join(dir, InternalStatusName), name+":"+version)
	WriteLn(filepath.Join(dir, "artifact.bin"), "artifact")
	WriteLn(filepath.Join(dir, "installed-deps", "test.prs"),
		fmt.Sprintf("group=test-gr\nname=%s\nversion=%s\ntype=test", name, version))
}

func validateModuleDirectory(dir string, name string, version string, hasInternalStatus bool, t *testing.T) {
	if hasInternalStatus {
		if s, _ := ReadLn(filepath.Join(dir, InternalStatusName)); s != name+":"+version {
			t.Fatalf("missing module internal status file")
		}
	}
	if s, _ := ReadLn(filepath.Join(dir, "artifact.bin")); s != "artifact" {
		t.Fatalf("wrong or missing artifact file: %s", s)
	}
	if s, _ := ReadLn(filepath.Join(dir, "installed-deps", "test.prs")); s != "group=test-gr" {
		t.Fatalf("wrong or missing installed-deps file: %s", s)
	}
}
