(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023-2025 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module LSP = Vscode_languageclient

type server_access =
  | Sub_process of LSP.ServerOptions.t
  | TCP of { host: string; port: int }


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

(* Look for the most specific `superbol-free` executable amongst (in order):

   - `superbol-free-${platform}-${arch}${suffix}`
   - `superbol-free-${platform}${suffix}`
   - `superbol-free${suffix}`
   - `superbol-free.js` (as fallback)

   The `platform` and `arch` used are from the corresponding `process`
   attributes in node.js. The `suffix` is `".exe"` on Windows, and empty
   otherwise.

   https://nodejs.org/api/process.html#processplatform
   https://nodejs.org/api/process.html#processarch
*)
let find_superbol root_uri =
  let platform = Node.Process.platform and arch = Node.Process.arch in
  let prefix = "superbol-free" in
  let alt_arch_execs =
    if platform = "darwin" && arch = "arm64"
    then [ `exe, Format.asprintf "%s-%s-x64" prefix platform ]
    else []
  in
  find_existing_in ~root_uri @@
  [
    `exe, Format.asprintf "%s-%s-%s" prefix platform arch;
    `exe, Format.asprintf "%s-%s" prefix platform;
  ] @ alt_arch_execs @ [
    `exe, prefix;
    `js, "superbol-free.js";
  ]

let scan_host_and_port url =
  let fail () = Format.ksprintf failwith "Invalid %S" url in
  match String.split_on_char ':' url with
  | [host; port] ->
      (try TCP { host; port = int_of_string port }
       with Invalid_argument _ -> fail ())
  | _ ->
      fail ()

let scan_server_command cmd =
  let prefix = "tcp://" in
  if String.starts_with ~prefix cmd then
    let l = String.length prefix in
    let url = String.sub cmd l (String.length cmd - l) in
    Some (scan_host_and_port url)
  else
    None


let server_command ~context ?cmd () =
  let root_uri = Vscode.ExtensionContext.extensionUri context
  and storage_uri =
    (* Use the global state URI as the server stores caches on a per-project
       basis (ie. for each workspace directory), not on a per-workspace
       basis. *)
    if Superbol_workspace.bool "cacheInGlobalStorage"
    then Some (Vscode.ExtensionContext.globalStorageUri context)
    else None
  in
  let server_kind, command =
    match cmd with
    | Some cmd ->
        `exe, cmd
    | None ->
        try find_superbol (Vscode.Uri.joinPath root_uri ~pathSegments:["_dist"])
        with Not_found ->
          (* If there is no bundled executable for the current platform, fall
             back to looking for superbol-free in the PATH *)
          `exe, "superbol-free"
  and fallback_config_dir =
    Vscode.Uri.fsPath @@
    Vscode.Uri.joinPath root_uri ~pathSegments:["gnucobol-config"]
  in
  let env =
    Interop.Dict.add "COB_CONFIG_DIR_FALLBACK" fallback_config_dir
      Node.Process.Env.env
  in
  let args =
    let force_diagnostics = Superbol_workspace.bool "forceSyntaxDiagnostics" in
    "lsp" ::
    (if force_diagnostics then ["--force-syntax-diagnostics"] else []) @
    (match storage_uri with
     | None -> []
     | Some uri -> ["--storage-directory"; Vscode.Uri.fsPath uri])
  in
  let server_options =
    match server_kind with
    | `exe ->
        `Executable (LSP.Executable.create () ~command ~args
                       ~options:(LSP.ExecutableOptions.create ~env ()))
    | `js ->
        `NodeModule (LSP.NodeModule.create ()
                       ~module_:command ~transport:`stdio ~args
                       ~options:(LSP.ForkOptions.create ~env ()))
  in
  Sub_process server_options


let server_access ~context =
  match Superbol_workspace.superbol_exe () with
  | None ->
      server_command ~context ()
  | Some cmd ->
      match scan_server_command cmd with
      | Some access -> access
      | None -> server_command ~context ~cmd ()


let client_options () =
  LSP.ClientOptions.create ()
    ~documentSelector:[|
      `Filter (LSP.DocumentFilter.createLanguage ()
                 ~language:"cobol");
      `Filter (LSP.DocumentFilter.createLanguage ()
                 ~language:"COBOL_GNU_LISTFILE");
      `Filter (LSP.DocumentFilter.createLanguage ()
                 ~language:"COBOL_GNU_DUMPFILE");
    |]


let server_needs_restart_after ~config_change =
  let affects ?scope section : bool =
    Vscode.ConfigurationChangeEvent.affectsConfiguration config_change ()
      ~section ?scope
  in
  affects "superbol.lspPath"         (* machine setting: no need for a scope *)
  || affects "superbol.forceSyntaxDiagnostics"                       (* scope? *)
  || affects "superbol.cacheInGlobalStorage"
