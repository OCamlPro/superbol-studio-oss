(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Vscode
open Interop

let type_ = "superbol"

let attributes debug =
  ["copybooks", ([%js.of: string list] []);
   "dialect", [%js.of: string] "";
   "sourceFormat", [%js.of: string] "";
   "forDebugging", [%js.of: bool] debug]

let definition_debug = TaskDefinition.create ~type_ ~attributes:(attributes true) ()
let definition = TaskDefinition.create ~type_ ~attributes:(attributes false) ()

let make_args attributes =
  ["-x"; "${relativeFile}"; "-ffold-copy=LOWER"] |>
  (fun args -> match List.assoc_opt "copybooks" attributes with
     | None -> args
     | Some books ->
       let books = ([%js.to: string list] books) in
       List.fold_left (fun acc book ->
           "-I"::book::acc)
         args
         books) |>
  (fun args -> match List.assoc_opt "dialect" attributes with
     | Some dialect when ([%js.to: string] dialect) <> "" ->
       let dialect = ([%js.to: string] dialect) in
       ("-std="^dialect)::args
     | _ -> args) |>
  (fun args -> match List.assoc_opt "sourceFormat" attributes with
     | Some sf when ([%js.to: string] sf) <> "" ->
       let sf = ([%js.to: string] sf) in
       ("-fformat="^sf)::args
     | _ -> args) |>
  (fun args -> match List.assoc_opt "extensions" attributes with
     | None -> args
     | Some exts ->
       let exts = ([%js.to: string list] exts) in
       List.fold_left (fun acc ext ->
           "-ext"::ext::acc)
         args
         exts) |>
  List.map (fun elt -> `String elt)

let provide_tasks ~token:_ =
  let scope = TaskScope.Workspace in
  let execution compiler =
    let args = make_args (attributes false) in
    let shell_execution = ShellExecution.makeCommandArgs ~command:(`String compiler) ~args () in
    `ShellExecution shell_execution
  in
  let build_task = Task.make ()
      ~definition
      ~scope
      ~name:"Build file"
      ~source:"superbol"
      ~execution:(execution "cobc")
  in
  let build_debug_task = Task.make ()
      ~definition:definition_debug
      ~scope
      ~name:"Build file for debug"
      ~source:"superbol"
      ~execution:(execution "cobcd")
  in
  `Value (Some [build_task; build_debug_task])

let resolve_cnt =
  let resolve = ref 0 in
  fun () ->
    incr resolve;
    !resolve

let resolve_task ~task ~token:_ =
  let oc = Window.createOutputChannel ~name:("Resolve"^(string_of_int (resolve_cnt ()))) in
  OutputChannel.appendLine oc ~value:"Starting resolve";
  let definition = Task.definition task in
  OutputChannel.appendLine oc ~value:"Got definition";
  let attributes, debug =
    [] |>
    (fun attributes ->
       match [%js.to: string list or_undefined] (TaskDefinition.get_attribute definition "copybooks") with
       | Some [] | None -> attributes
       | Some books -> ("copybooks", ([%js.of: string list] books))::attributes
       | exception _ -> attributes) |>
    (fun attributes ->
       match [%js.to: string or_undefined] (TaskDefinition.get_attribute definition "dialect") with
       | Some "" | None -> attributes
       | Some str -> ("dialect", [%js.of: string] str)::attributes
       | exception _ -> attributes) |>
    (fun attributes ->
       match [%js.to: string or_undefined] (TaskDefinition.get_attribute definition "sourceFormat") with
       | Some "" | None -> attributes
       | Some str -> ("sourceFormat", [%js.of: string] str)::attributes
       | exception _ -> attributes) |>
    (fun attributes ->
       match [%js.to: string list or_undefined] (TaskDefinition.get_attribute definition "extensions") with
       | Some [] | None -> attributes
       | Some exts -> ("extensions", [%js.of: string list] exts)::attributes
       | exception _ -> attributes) |>
    (fun attributes ->
       match [%js.to: bool or_undefined] (TaskDefinition.get_attribute definition "forDebugging") with
       | Some b -> attributes, b
       | None | exception _ -> attributes, false)
  in
  OutputChannel.appendLine oc ~value:"Got attributes";
  let args = make_args attributes in
  let execution =
    if debug then
      let shell_execution = ShellExecution.makeCommandArgs ~command:(`String "cobcd") ~args () in
      `ShellExecution shell_execution
    else
      let shell_execution = ShellExecution.makeCommandArgs ~command:(`String "cobc") ~args () in
      `ShellExecution shell_execution
  in
  let task =
    Task.make ()
      ~definition
      ~scope:(TaskScope.Workspace)
      ~name:(Task.name task)
      ~source:(Task.source task)
      ~execution
  in
  OutputChannel.appendLine oc ~value:"Task made";
  `Value (Some task)

let provider =
  TaskProvider.Default.create
    ~provideTasks:provide_tasks
    ~resolveTask:resolve_task
