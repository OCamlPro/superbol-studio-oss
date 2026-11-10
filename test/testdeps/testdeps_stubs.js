// SuperBOL OSS Studio
//
// Copyright (c) 2022-2026 OCamlPro SAS
//
// All rights reserved.
// This source code is licensed under the GNU Affero General Public
// License version 3 found in the LICENSE.md file in the root directory
// of this source tree.

// `Unix.putenv` for tests run with node, which js_of_ocaml (as of 6.0.1) does
// not provide.  `Sys.getenv` reads `process.env`, so that is where values go.
//
// Provided under both its OCaml 5 and OCaml 4 names, as `//Alias` is not
// enough for libraries compiled separately (see `cache_store_stubs.js` in
// `superbol_platform`).

//Provides: caml_unix_putenv
//Requires: caml_jsstring_of_string
function caml_unix_putenv(name, value) {
  var proc = globalThis.process;
  if (proc && proc.env)
    proc.env[caml_jsstring_of_string(name)] = caml_jsstring_of_string(value);
  return 0;
}

//Provides: unix_putenv
//Requires: caml_unix_putenv
function unix_putenv(name, value) {
  return caml_unix_putenv(name, value);
}
