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
	"archive/zip"
	"bytes"
	"context"
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"testing"
	"time"

	"github.com/eclipse-kanto/software-update/hawkbit"
)

type sWeb struct {
	name string
	data []byte
	srv  *http.Server
	t    *testing.T
}

const DDF = "group=%s\nname=%s\nversion=%s\ntype=%s\n"

// ----- NewStorage ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestConstructorErrors tests NewStorage constructor for error cases.
func TestConstructorErrors(t *testing.T) {
	names := []string{"download", "installed-deps", "modules"}
	for _, name := range names {
		// Try to create a Storage without important directory.
		t.Run(name, func(t *testing.T) {
			// Prepare
			dir := "_tmp-constructor"
			if err := os.MkdirAll(dir, 0755); err != nil {
				t.Fatalf("fail create temporary directory: %v", err)
			}

			// Remove temporary directory at the end.
			defer os.RemoveAll(dir)

			// Occupy needed directory.
			path := filepath.Join(dir, name)
			file, err := os.Create(path)
			if err != nil {
				t.Fatalf("fail to occupy directory [%s]: %v", path, err)
			}
			defer file.Close()

			// Try to create new Storage.
			if _, err := NewStorage(dir); err == nil {
				t.Fatalf("initialize local storage without %s directory!", name)
			}
		})
	}
}

// ----- MoveInstalledDeps and LoadInstalledDeps ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestLoadSoftwareUpdatables tests MoveInstalledDeps and LoadInstalledDeps functions.
func TestMoveLoadInstalledDeps(t *testing.T) {
	// Prepare
	dir := "_tmp-installed-deps"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("fail create temporary directory: %v", err)
	}

	// Remove temporary directory at the end.
	defer os.RemoveAll(dir)

	// Create Storage object.
	store, err := NewStorage(dir)
	if err != nil {
		t.Fatalf("fail to initialize local storage: %v", err)
	}
	defer store.Close()

	// 1. Try load installed dependencies for empty directory.
	actual, err := store.LoadInstalledDeps()
	if err != nil || len(actual) != 0 {
		t.Fatalf("fail to load installed dependency from empty directory [size: %v]: %v", len(actual), err)
	}

	// Create first module installed dependencies directory.
	path := filepath.Join(store.DownloadPath, "10", "0")
	idPath := filepath.Join(path, "installed-deps")
	sid1 := saveInstalledDep(filepath.Join(idPath, "id11.prs"), "group1", "name1", "1", "test", t)
	cid1 := saveInstalledDep(filepath.Join(idPath, "dir1", "id12.prs"), "group1", "name1", "1", "test", t)

	// 2. Try to move installed dependencies from default directory.
	if err := store.MoveInstalledDeps(path, nil); err != nil {
		t.Fatalf("fail to move installed dependency [%s]: %v", path, err)
	}

	// Create second module installed dependencies directory.
	path = filepath.Join(store.DownloadPath, "10", "1")
	idPath = filepath.Join(path, "ideps")
	sid2 := saveInstalledDep(filepath.Join(idPath, "id21.prs"), "group2", "name1", "1", "test", t)
	cid2 := saveInstalledDep(filepath.Join(idPath, "dir2", "id22.prs"), "group2", "name1", "1", "test", t)

	// 3. Try to move installed dependencies from custom directory.
	metadata := map[string]string{"installed-deps": "ideps"}
	if err := store.MoveInstalledDeps(path, metadata); err != nil {
		t.Fatalf("fail to move installed dependency [%s]: %v", path, err)
	}

	// 4. Try to validate all moved installed dependencies.
	ids, err := store.LoadInstalledDeps()
	if err != nil {
		t.Fatalf("fail to load installed dependency: %v", err)
	}
	hasInstalledDep(ids, sid1, t)
	hasInstalledDep(ids, cid1, t)
	hasInstalledDep(ids, sid2, t)
	hasInstalledDep(ids, cid2, t)
}

func hasInstalledDep(actual []*hawkbit.DependencyDescription, exp *hawkbit.DependencyDescription, t *testing.T) {
	for _, a := range actual {
		if exp.Group == a.Group && exp.Name == a.Name && exp.Version == a.Version && exp.Type == a.Type {
			return
		}
	}
	t.Fatalf("missing installed dependency: %v", exp)
}

// saveInstalledDep write dependency to the provided path.
func saveInstalledDep(path, ddGroup, ddName, ddVersion, ddType string, t *testing.T) *hawkbit.DependencyDescription {
	save(path, fmt.Sprintf(DDF, ddGroup, ddName, ddVersion, ddType), t)
	return &hawkbit.DependencyDescription{Group: ddGroup, Name: ddName, Version: ddVersion, Type: ddType}
}

// ----- LoadSoftwareUpdatables, ArchiveModule and DownloadModule ----- ----- ----- ----- ----- ----- ----- ----- -----

// TestLoadSoftwareUpdatables tests LoadSoftwareUpdatables functions.
func TestLoadSoftwareUpdatables(t *testing.T) {
	// Prepare
	dir := "_tmp-updatable-load"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("fail create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	// Host dependency descriptions as module.zip
	srv, art, _ := ddHost("module1.zip", t)
	defer srv.close()

	// Create Storage object
	store, err := NewStorage(dir)
	if err != nil {
		t.Fatalf("fail to initialize local storage: %v", err)
	}
	defer store.Close()

	// 1. Search for updatables in empty directory.
	actual := store.LoadSoftwareUpdatables()
	if len(actual) > 0 {
		t.Fatalf("unexpected updatables: %v", actual)
	}

	// Create broken updatable.
	save(filepath.Join(store.DownloadPath, "1", SoftwareUpdatableName), "broken", t)

	// Create empty directory.
	if err := os.MkdirAll(filepath.Join(store.DownloadPath, "2"), 0755); err != nil {
		t.Fatalf("fail create temporary directory: %v", err)
	}

	// Save valid updatable.
	path := filepath.Join(store.DownloadPath, "0")
	name := filepath.Join(path, SoftwareUpdatableName)
	expected, err := SaveSoftwareUpdatable("install", "cid", name, hm(art))
	if err != nil {
		t.Fatalf("fail to save updatable to file: %v", err)
	}

	// 2. Search for single updatable.
	actual = store.LoadSoftwareUpdatables()
	if actual == nil || len(actual) != 1 || !reflect.DeepEqual(expected, actual[path]) {
		t.Errorf("fail to load updatables: [%v] != [%v]", expected, actual[path])
	}

	// 3. Try to load updatables from file (not directory)
	if err := os.RemoveAll(store.DownloadPath); err != nil {
		t.Fatalf("fail to delete download directory: %v", err)
	}
	save(store.DownloadPath, "", t)
	actual = store.LoadSoftwareUpdatables()
	if len(actual) > 0 {
		t.Fatalf("unexpected updatables loaded from file: %v", actual)
	}
}

// TestDownloadArchiveModule tests DownloadModule and ArchiveModule functions.
func TestDownloadArchiveModule(t *testing.T) {
	// Prepare
	dir := "_tmp-storage"
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("fail create temporary directory: %v", err)
	}

	// Remove temporary directory at the end
	defer os.RemoveAll(dir)

	// Host dependency descriptions as module.zip
	srv, art, dds := ddHost("module2.zip", t)
	defer srv.close()

	// Create Storage object
	store, err := NewStorage(dir)
	if err != nil {
		t.Fatalf("fail to initialize local storage: %v", err)
	}
	defer store.Close()

	// 1. Download module without progress.
	path := filepath.Join(store.DownloadPath, "0", "0")
	m := &Module{Name: "name1", Version: "1", Artifacts: []*Artifact{art}}
	if err := store.DownloadModule(path, m, nil, "", 0, 0, nil); err != nil {
		t.Fatalf("fail to download module [Hash: %s, File: %s]: %v", art.HashValue, hex.EncodeToString(srv.data), err)
	}
	existence(filepath.Join(path, art.FileName), true, "[initial download]", t)

	// 2. Archive module.
	if err := WriteLn(filepath.Join(path, InternalStatusName), m.Name+":"+m.Version); err != nil {
		t.Fatalf("fail to write module id: %v", err)
	}
	if err := store.ArchiveModule(path); err != nil {
		t.Fatalf("fail to archive module: %v", err)
	}
	existence(filepath.Join(store.ModulesPath, "0", art.FileName), true, "[archive]", t)

	// 3. Download previous module with progress.
	path = filepath.Join(store.DownloadPath, "0", "1")
	progress := func(percent int) { /* Do nothing. */ }

	// Download with validation error
	validationErr := fmt.Errorf("test validation error")
	validationFail := func() error {
		return validationErr
	}
	if err := store.DownloadModule(path, m, progress, "", 0, 0, validationFail); err != validationErr {
		t.Errorf("unexpected validation error")
	}

	if err := store.DownloadModule(path, m, progress, "", 0, 0, nil); err != nil {
		t.Errorf("fail to download module: %v", err)
	}
	existence(filepath.Join(store.ModulesPath, "0", art.FileName), false, "[archive]", t)
	existence(filepath.Join(path, art.FileName), true, "[second download]", t)

	// Extract module.
	if err := ExtractArchive(path); err != nil {
		t.Fatalf("fail to extract module [%s]: %v", path, err)
	}

	for k, v := range dds {
		dd, err := loadInstalledDep(filepath.Join(path, "installed-deps", k))
		if err != nil {
			t.Fatalf("fail to load installed dependency: %v", err)
		}
		if !reflect.DeepEqual(v, dd) {
			t.Errorf("fail to load installed dependency: [%v] != [%v]", v, dd)

		}
	}
}

func existence(path string, exists bool, prefix string, t *testing.T) {
	if _, err := os.Stat(path); os.IsNotExist(err) == exists {
		t.Fatalf("%s fail to check file existence [path: %s, isNotExists: %v]", prefix, path, exists)
	}
}

func hm(art *Artifact) []*hawkbit.SoftwareModuleAction {
	// Create hawkbit artifact.
	a := &hawkbit.SoftwareArtifactAction{
		Filename:  art.FileName,
		Size:      art.Size,
		Download:  map[hawkbit.Protocol]*hawkbit.Links{hawkbit.HTTP: {URL: art.Link, MD5URL: ""}},
		Checksums: map[hawkbit.Hash]string{hawkbit.MD5: art.HashValue},
	}

	// Create hawkbit modules.
	hm1 := &hawkbit.SoftwareModuleAction{
		SoftwareModule: &hawkbit.SoftwareModuleID{Name: "m1", Version: "1"},
		Artifacts:      []*hawkbit.SoftwareArtifactAction{a}}
	hm2 := &hawkbit.SoftwareModuleAction{
		SoftwareModule: &hawkbit.SoftwareModuleID{Name: "m2", Version: "1"},
		Artifacts:      []*hawkbit.SoftwareArtifactAction{a}}
	hm := []*hawkbit.SoftwareModuleAction{hm1, hm2}
	return hm
}

// ddHost create and start a HTTP server on port: 43234
func ddHost(name string, t *testing.T) (*sWeb, *Artifact, map[string]*hawkbit.DependencyDescription) {
	w := &sWeb{name: name, t: t}

	// Create dependency descriptions
	dds := map[string]*hawkbit.DependencyDescription{}
	dds["id1.prs"] = &hawkbit.DependencyDescription{Group: "group1", Name: "name1", Version: "1", Type: "test"}
	dds["id2.prs"] = &hawkbit.DependencyDescription{Group: "group2", Name: "name2", Version: "1", Type: "test"}

	// set dependency descriptions to the buffer.
	buf := new(bytes.Buffer)
	a := zip.NewWriter(buf)
	for name, dd := range dds {
		h := &zip.FileHeader{Name: filepath.Join("installed-deps", name), Method: zip.Deflate}
		h.SetMode(0644)
		f, err := a.CreateHeader(h)
		if err != nil {
			w.t.Fatalf("fail to create zip archive file entry: %v", err)
		}
		body := fmt.Sprintf(DDF, dd.Group, dd.Name, dd.Version, dd.Type)
		if _, err = f.Write([]byte(body)); err != nil {
			w.t.Fatalf("fail to write zip archive file entry: %v", err)
		}
		a.Flush()
	}
	a.Close()
	w.data = buf.Bytes()

	// Create new HTTP server.
	w.srv = &http.Server{Addr: ":43234"}
	http.HandleFunc(fmt.Sprintf("/%s", w.name), w.handler)

	// Start HTTP server in separate goroute.
	go func() {
		if err := w.srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			w.t.Errorf("fail to start web server: %v", err)
		}
	}()

	time.Sleep(1 * time.Second)

	// Create artifact.
	hType := md5.New()
	if _, err := hType.Write(w.data); err != nil {
		w.t.Fatalf("fail to calculate checksum: %v", err)
	}
	art := &Artifact{
		FileName:  name,
		HashType:  "MD5",
		Size:      len(w.data),
		HashValue: hex.EncodeToString(hType.Sum(nil)),
		Link:      fmt.Sprintf("http://localhost%s/%s", w.srv.Addr, name),
	}
	return w, art, dds
}

// handler handles incoming HTTP requests.
func (w *sWeb) handler(writer http.ResponseWriter, request *http.Request) {
	writer.Header().Set("Content-Type", "application/zip")
	writer.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, w.name))
	writer.Header().Set("Content-Length", strconv.Itoa(len(w.data)))
	writer.Write(w.data)
}

// close the http server.
func (w *sWeb) close() {
	if err := w.srv.Shutdown(context.Background()); err != nil {
		w.t.Errorf("fail shutdown web server: %v", err)
	}
}

func save(path string, body string, t *testing.T) {
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		t.Fatalf("fail create directory [%s]: %v", path, err)
	}
	if err := WriteLn(path, body); err != nil {
		t.Fatalf("fail to save file [%s]: %v", path, err)
	}
}
