(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* Helpers to find the bundled superbol executable *)
let find_existing_in ~root_uri : (([<`exe | `js] * string) as 'a) list -> 'a =
  let actual_uri = function
    | `exe, exe_name when Node.Process.platform = "win32" ->
        Vscode.Uri.joinPath root_uri ~pathSegments:[exe_name ^ ".exe"]
    | `exe, exe_name ->
        Vscode.Uri.joinPath root_uri ~pathSegments:[exe_name]
    | `js, js_name ->
        Vscode.Uri.joinPath root_uri ~pathSegments:[js_name]
  in
  let rec aux = function
    | [] ->
        raise Not_found
    | (kind, _) as next_candidate :: remaining_candidates ->
        let uri = actual_uri next_candidate in
        let path = Vscode.Uri.fsPath uri in
        if Node.Fs.existsSync path
        then kind, path
        else aux remaining_candidates
  in
  aux

(* Look for the most specific `${prog_prefix}` executable amongst (in order):

   - `${prog_prefix}-${platform}-${arch}${suffix}`
   - `${prog_prefix}-${platform}${suffix}`
   - `${prog_prefix}${suffix}`
   - `${prog_prefix}.js` (as fallback)

   The `platform` and `arch` used are from the corresponding `process`
   attributes in node.js. The `suffix` is `".exe"` on Windows, and empty
   otherwise.

   https://nodejs.org/api/process.html#processplatform
   https://nodejs.org/api/process.html#processarch
*)
let find_executable ~prog_prefix root_uri =
  let platform = Node.Process.platform and arch = Node.Process.arch in
  let alt_arch_execs =
    if platform = "darwin" && arch = "arm64"
    then [ `exe, Format.asprintf "%s-%s-x64" prog_prefix platform ]
    else []
  in
  find_existing_in ~root_uri @@
  [
    `exe, Format.asprintf "%s-%s-%s" prog_prefix platform arch;
    `exe, Format.asprintf "%s-%s" prog_prefix platform;
  ] @ alt_arch_execs @ [
    `exe, prog_prefix;
    `js, Format.asprintf "%s.js" prog_prefix;
  ]
