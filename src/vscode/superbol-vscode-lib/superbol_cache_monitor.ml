(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)


type status =
  {
    project_root: string;
    cache: string option;
    entries: int;
    values: int;
    disk_usage: float;
  }

type entry =
  {
    key: string;
    label: string;
    dependencies: int;
    disk_usage: float;
    last_used: float;
    value: string;
    value_size: float;
  }

type listing =
  {
    totals: status;
    now: float;
    items: entry list;
  }

type trimmed =
  {
    reclaimed: int;
    remaining: status;
  }

let now () = Node.Date.now () /. 1000.

(* Length of the tail of the error output that is reported when a command
   fails. *)
let stderr_tail_length = 2048

let run (cli: Superbol_languageclient.cli_invocation) args =
  Promise.make @@ fun ~resolve ~reject:_ ->
  let args = cli.args_prefix @ args in
  let stdout = Buffer.create 1024 and stderr = ref "" in
  let settled = ref false in
  let settle result = if not !settled then (settled := true; resolve result) in
  let string_of_chunk = function
    | `String s -> s
    | `Buffer b -> Node.Buffer.toString b
  in
  match
    Node.ChildProcess.spawn_process cli.program (Array.of_list args) ()
      ~options:(Node.ChildProcess.Options.create ~env:cli.cli_env ())
  with
  | exception e ->
      settle (Error (Printexc.to_string e))
  | process ->
      Node.Stream.Readable.on (Node.ChildProcess.get_stdout process)
        (`Data (fun ~chunk ->
             Buffer.add_string stdout (string_of_chunk chunk)));
      Node.Stream.Readable.on (Node.ChildProcess.get_stderr process)
        (`Data begin fun ~chunk ->
            let s = !stderr ^ string_of_chunk chunk in
            let len = String.length s in
            stderr :=
              if len <= stderr_tail_length then s
              else String.sub s (len - stderr_tail_length) stderr_tail_length
          end);
      Node.ChildProcess.on process @@ `Error begin fun ~err ->
        settle (Error (Node.JsError.message err))
      end;
      Node.ChildProcess.on process @@ `Close begin fun ~code ?signal:_ () ->
        if code = 0
        then settle (Ok (Buffer.contents stdout))
        else settle (Error (Printf.sprintf "exit code %d\n%s" code !stderr))
      end

let decode_status json =
  let open Jsonoo.Decode in
  let count name = try_default 0 (field name int) json in
  { project_root = field "project_root" string json;
    cache = field "cache" (nullable string) json;
    entries = count "entries";
    values = count "values";
    disk_usage = try_default 0. (field "disk_usage" float) json }

let decode_entry json =
  let open Jsonoo.Decode in
  { key = field "key" string json;
    label = field "label" string json;
    dependencies = try_default 0 (field "dependencies" int) json;
    disk_usage = try_default 0. (field "disk_usage" float) json;
    last_used = try_default 0. (field "last_used" float) json;
    value = field "value" string json;
    value_size = try_default 0. (field "value_size" float) json }

let decode_listing json =
  let open Jsonoo.Decode in
  { totals = decode_status json;
    now = try_default (now ()) (field "now" float) json;
    items = try_default [] (field "items" (list decode_entry)) json }

let decode_trimmed json =
  let open Jsonoo.Decode in
  { reclaimed = try_default 0 (field "reclaimed" int) json;
    remaining = decode_status json }

let decode decoder output =
  match Jsonoo.try_parse_opt output with
  | None ->
      Error (Printf.sprintf "unexpected output: %s" output)
  | Some json ->
      try Ok (decoder json) with Jsonoo.Decode_error msg -> Error msg

let status cli ~project_root =
  let open Promise.Syntax in
  let+ output =
    run cli @@
    ["cache"; "status"; "--json"; "--project-root"; project_root] @
    cli.cli_storage_args
  in
  Result.bind output (decode decode_status)

let listing cli ~project_root =
  let open Promise.Syntax in
  let+ output =
    run cli @@
    ["cache"; "status"; "--json"; "--list-entries";
     "--project-root"; project_root] @
    cli.cli_storage_args
  in
  Result.bind output (decode decode_listing)

let trim cli ~project_root ?older_than ?max_size () =
  let open Promise.Syntax in
  let+ output =
    run cli @@
    ["cache"; "trim"; "--json"; "--project-root"; project_root] @
    Option.fold older_than ~none:[]
      ~some:(fun days -> ["--older-than"; Printf.sprintf "%g" days]) @
    Option.fold max_size ~none:[] ~some:(fun size -> ["--max-size"; size]) @
    cli.cli_storage_args
  in
  Result.bind output (decode decode_trimmed)

(* --- *)

let removed_by_age_bound ~now ~older_than items =
  let removed, kept =
    List.partition (fun e -> now -. e.last_used > older_than *. 86400.) items
  in
  let held = Hashtbl.create (List.length kept) in
  List.iter (fun e -> Hashtbl.replace held e.value ()) kept;
  let freed_values = Hashtbl.create 17 in
  let manifests =
    List.fold_left begin fun total e ->
      if not (Hashtbl.mem held e.value) then
        Hashtbl.replace freed_values e.value e.value_size;
      total +. (e.disk_usage -. e.value_size)
    end 0. removed
  in
  List.length removed,
  Hashtbl.fold (fun _ size total -> total +. size) freed_values manifests

let workspace_folders () =
  List.filter (fun f -> Vscode.(Uri.scheme (WorkspaceFolder.uri f)) = "file")
    (Vscode.Workspace.workspaceFolders ())

let pp_size ppf bytes =
  let rec pick n = function
    | _ :: (_ :: _ as units) when n >= 1024. -> pick (n /. 1024.) units
    | "B" :: _ -> Format.fprintf ppf "%.0f B" n
    | unit :: _ -> Format.fprintf ppf "%.1f %s" n unit
    | [] -> Format.fprintf ppf "%.0f B" n
  in
  pick bytes ["B"; "KiB"; "MiB"; "GiB"; "TiB"]

let string_of_size = Format.asprintf "%a" pp_size

