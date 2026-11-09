// SuperBOL OSS Studio
//
// Copyright (c) 2022-2026 OCamlPro SAS
//
// All rights reserved.
// This source code is licensed under the GNU Affero General Public
// License version 3 found in the LICENSE.md file in the root directory
// of this source tree.

// Unix primitives that `Cache_store` needs, but that the runtime of
// js_of_ocaml does not provide (as of 6.0.1).  Without them, calling the
// corresponding `Unix` function raises a `Failure` (and not a `Unix_error`),
// which the store has no reason to expect.
//
// Each is implemented with node; where node is not available (or the path does
// not live on a node file-system), they raise `Unix_error (ENOSYS, ...)`, which
// callers handle like any other failure of the system call.
//
// Every primitive is provided under both its OCaml 5 (`caml_unix_*`) and its
// OCaml 4 (`unix_*`) name.  `//Alias` would not do: aliases are not exported
// from runtimes built for separate compilation, and libraries compiled before
// this file is known (like `unix.cma.js`) refer to primitives by their original
// name.

//Provides: superbol_node_path
//Requires: resolve_fs_device, caml_raise_system_error
function superbol_node_path(name, cmd) {
  var root = resolve_fs_device(name);
  if (!root.device.fs || !root.device.nm)
    caml_raise_system_error(/* raise Unix_error */ 1, "ENOSYS", cmd);
  return { fs: root.device.fs, path: root.device.nm(root.rest) };
}

//Provides: caml_unix_getpid
function caml_unix_getpid(unit) {
  var proc = globalThis.process;
  return proc && proc.pid ? proc.pid | 0 : 0;
}

//Provides: caml_unix_chmod
//Requires: superbol_node_path, caml_raise_nodejs_error
function caml_unix_chmod(name, perm) {
  var p = superbol_node_path(name, "chmod");
  try {
    p.fs.chmodSync(p.path, perm);
  } catch (err) {
    caml_raise_nodejs_error(err, /* raise Unix_error */ 1, "chmod");
  }
  return 0;
}

//Provides: caml_unix_link
//Requires: superbol_node_path, caml_raise_nodejs_error
//Requires: caml_raise_system_error
function caml_unix_link(follow, src, dst) {
  var s = superbol_node_path(src, "link");
  var d = superbol_node_path(dst, "link");
  if (s.fs !== d.fs)
    caml_raise_system_error(/* raise Unix_error */ 1, "EXDEV", "link");
  // `follow` is an option, `0` meaning `None`: node's `linkSync` behaves like
  // `link(2)`, which is what `None` asks for anyway.
  try {
    s.fs.linkSync(s.path, d.path);
  } catch (err) {
    caml_raise_nodejs_error(err, /* raise Unix_error */ 1, "link");
  }
  return 0;
}

//Provides: caml_unix_fsync
//Requires: caml_unix_lookup_file, caml_raise_nodejs_error
//Requires: caml_raise_system_error
function caml_unix_fsync(fd) {
  var file = caml_unix_lookup_file(fd, "fsync");
  if (!file.fs || file.fd === undefined)
    caml_raise_system_error(/* raise Unix_error */ 1, "ENOSYS", "fsync");
  try {
    file.fs.fsyncSync(file.fd);
  } catch (err) {
    caml_raise_nodejs_error(err, /* raise Unix_error */ 1, "fsync");
  }
  return 0;
}

//Provides: caml_unix_error_message
//Requires: unix_error, caml_string_of_jsstring
function caml_unix_error_message(err) {
  // Constant constructors are integers; `EUNKNOWNERR n` is a block.
  var msg =
    typeof err === "number"
      ? unix_error[err] || "Unknown error"
      : "Unknown error " + err[1];
  return caml_string_of_jsstring(msg);
}

// OCaml 4 names

//Provides: unix_getpid
//Requires: caml_unix_getpid
function unix_getpid(unit) {
  return caml_unix_getpid(unit);
}

//Provides: unix_chmod
//Requires: caml_unix_chmod
function unix_chmod(name, perm) {
  return caml_unix_chmod(name, perm);
}

//Provides: unix_link
//Requires: caml_unix_link
function unix_link(follow, src, dst) {
  return caml_unix_link(follow, src, dst);
}

//Provides: unix_fsync
//Requires: caml_unix_fsync
function unix_fsync(fd) {
  return caml_unix_fsync(fd);
}

//Provides: unix_error_message
//Requires: caml_unix_error_message
function unix_error_message(err) {
  return caml_unix_error_message(err);
}
