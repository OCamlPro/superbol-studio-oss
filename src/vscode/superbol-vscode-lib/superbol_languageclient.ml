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

module LSP = Vscode_languageclient

type server_access =
  | Sub_process of LSP.ServerOptions.t
  | TCP of { host: string; port: int }

let find_superbol ~server_prefix ~root_uri =
  try
    Superbol_extension.find_executable ~prog_prefix:server_prefix
      (Vscode.Uri.joinPath root_uri ~pathSegments:["_dist"])
  with Not_found ->
    (* If there is no bundled executable for the current platform, fall back to
       looking for the server binary in the PATH *)
    `exe, server_prefix

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


(* What the server and the CLI commands that share its caches run with. *)
type superbol_command =
  {
    kind: [`exe | `js];
    command: string;
    env: string Interop.Dict.t;
    storage_args: string list;
  }

let superbol_command ~server_prefix ~context ?cmd () =
  let root_uri = Vscode.ExtensionContext.extensionUri context
  and storage_uri =
    (* Use the global state URI as the server stores caches on a per-project
       basis (ie. for each workspace directory), not on a per-workspace
       basis. *)
    if Superbol_config.bool "cacheInGlobalStorage"
    then Some (Vscode.ExtensionContext.globalStorageUri context)
    else None
  in
  let kind, command =
    match cmd with
    | Some cmd ->
        `exe, cmd
    | None ->
        find_superbol ~server_prefix ~root_uri
  and fallback_config_dir =
    Vscode.Uri.fsPath @@
    Vscode.Uri.joinPath root_uri ~pathSegments:["gnucobol-config"]
  in
  let env =
    Interop.Dict.add "COB_CONFIG_DIR_FALLBACK" fallback_config_dir
      Node.Process.Env.env
  and storage_args =
    match storage_uri with
    | None -> []
    | Some uri -> ["--storage-directory"; Vscode.Uri.fsPath uri]
  in
  { kind; command; env; storage_args }

let server_command ~server_prefix ~context ?cmd () =
  let { kind = server_kind; command; env; storage_args } =
    superbol_command ~server_prefix ~context ?cmd ()
  in
  let args =
    let force_diagnostics = Superbol_config.bool "forceSyntaxDiagnostics" in
    "lsp" ::
    (if force_diagnostics then ["--force-syntax-diagnostics"] else []) @
    storage_args
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


let server_access ~context ~server_prefix =
  match Superbol_config.superbol_exe () with
  | None ->
      server_command ~server_prefix ~context ()
  | Some cmd ->
      match scan_server_command cmd with
      | Some access -> access
      | None -> server_command ~server_prefix ~context ~cmd ()


type cli_invocation =
  {
    program: string;
    args_prefix: string list;
    cli_env: string Interop.Dict.t;
    cli_storage_args: string list;
  }

(* A bundled node module is run with the node runtime that runs VS Code
   itself, as the language client does when it forks the server. *)
let cli_invocation ~context ~server_prefix =
  let invocation { kind; command; env; storage_args } =
    match kind with
    | `exe ->
        Some { program = command; args_prefix = []; cli_env = env;
               cli_storage_args = storage_args }
    | `js ->
        Some { program = Node.Process.execPath; args_prefix = [command];
               cli_env = Interop.Dict.add "ELECTRON_RUN_AS_NODE" "1" env;
               cli_storage_args = storage_args }
  in
  match Superbol_config.superbol_exe () with
  | None ->
      invocation @@ superbol_command ~server_prefix ~context ()
  | Some cmd ->
      match scan_server_command cmd with
      | Some TCP _ | Some Sub_process _ -> None      (* not run by us *)
      | None -> invocation @@ superbol_command ~server_prefix ~context ~cmd ()

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
