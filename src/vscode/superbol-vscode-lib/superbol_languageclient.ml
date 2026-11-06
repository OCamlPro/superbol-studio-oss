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

let find_superbol ~server_prefix ~root_uri =
  try
    Superbol_extension.find_executable ~prog_prefix:server_prefix
      (Vscode.Uri.joinPath root_uri ~pathSegments:["_dist"])
  with Not_found ->
    (* If there is no bundled executable for the current platform, fall back to
       looking for the server binary in the PATH *)
    `exe, server_prefix

(* What the server and the CLI commands that share its caches run with. *)
type superbol_command =
  {
    kind: [`exe | `js];
    command: string;
    env: string Interop.Dict.t;
    storage_args: string list;
  }

let superbol_command ~server_prefix ~context =
  let root_uri = Vscode.ExtensionContext.extensionUri context
  and storage_uri =
    (* Use the global state URI as the server stores caches on a per-project
       basis (ie. for each workspace directory), not on a per-workspace
       basis. *)
    if Superbol_config.bool "cacheInGlobalStorage"
    then Some (Vscode.ExtensionContext.globalStorageUri context)
    else None
  in
  let kind, command = find_superbol ~server_prefix ~root_uri
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

(* Sizes as the server accepts them (see `Arg_utils.size_of_string'). *)
let is_size s =
  let digits =
    match s.[String.length s - 1] with
    | 'k' | 'K' | 'm' | 'M' | 'g' | 'G' -> String.sub s 0 (String.length s - 1)
    | _ -> s
  in
  digits <> "" && String.for_all (function '0'..'9' -> true | _ -> false) digits

(* An invalid size limit is ignored with a warning: the server would refuse to
   start otherwise. *)
let cache_pruning_args () =
  let size_limit =
    match String.trim @@ Superbol_config.string "cache.sizeLimit" with
    | "" ->
        []
    | s when is_size s ->
        ["--cache-size-limit"; s]
    | s ->
        let _ =
          Vscode.Window.showWarningMessage ()
            ~message:(Printf.sprintf
                        "SuperBOL: ignoring invalid cache size limit `%s' \
                         (expected a number of bytes, with an optional `k', \
                         `M' or `G' suffix)" s)
        in
        []
  and max_age =
    match Superbol_config.positive_int "cache.maxAge" ~default:0 with
    | 0 -> []
    | days -> ["--cache-max-age"; string_of_int days]
  in
  size_limit @ max_age

let server_options ~context ~server_prefix =
  let { kind; command; env; storage_args } =
    superbol_command ~server_prefix ~context
  in
  let args =
    let force_diagnostics = Superbol_config.bool "forceSyntaxDiagnostics" in
    "lsp" ::
    (if force_diagnostics then ["--force-syntax-diagnostics"] else []) @
    storage_args @
    cache_pruning_args ()
  in
  match kind with
  | `exe ->
      `Executable (LSP.Executable.create () ~command ~args
                     ~options:(LSP.ExecutableOptions.create ~env ()))
  | `js ->
      `NodeModule (LSP.NodeModule.create ()
                     ~module_:command ~transport:`stdio ~args
                     ~options:(LSP.ForkOptions.create ~env ()))

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
  let { kind; command; env; storage_args } =
    superbol_command ~server_prefix ~context
  in
  match kind with
  | `exe ->
      { program = command; args_prefix = []; cli_env = env;
        cli_storage_args = storage_args }
  | `js ->
      { program = Node.Process.execPath; args_prefix = [command];
        cli_env = Interop.Dict.add "ELECTRON_RUN_AS_NODE" "1" env;
        cli_storage_args = storage_args }

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
  affects "superbol.forceSyntaxDiagnostics"                          (* scope? *)
  || affects "superbol.cacheInGlobalStorage"
  || affects "superbol.cache.sizeLimit"
  || affects "superbol.cache.maxAge"
