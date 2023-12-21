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

(* --- *)

type attribute_spec =
  | C: (Ojs.t -> 'a option) * ('a -> Ojs.t) * 'a -> attribute_spec

let attributes_spec ~debug =
  [
    "copybooks", C ([%js.to: string list or_undefined],
                    [%js.of: string list], []);
    "dialect", C ([%js.to: string or_undefined],
                  [%js.of: string], "default");
    "source-format", C ([%js.to: string or_undefined],
                        [%js.of: string], "auto");
    "for-debug", C ([%js.to: bool or_undefined],
                    [%js.of: bool], debug);
    "cobc-path", C ([%js.to: string or_undefined],
                    [%js.of: string], "cobc");
    "extra-args", C ([%js.to: string list or_undefined],
                     [%js.of: string list], ["-ffold-copy=LOWER"]);
  ]

(* --- *)

let bool_flag_arg key ~ok ?(ko = Fun.id) ~attributes args =
  match List.assoc_opt key attributes with
  | Some flag when [%js.to: bool] flag -> ok args
  | _ -> ko args

let string_arg key ~mk ~attributes args =
  match List.assoc_opt key attributes with
  | Some s -> mk ([%js.to: string] s) :: args
  | None -> args

let string_args key ~append ~attributes args =
  match List.assoc_opt key attributes with
  | Some l -> append ([%js.to: string list] l) args
  | None -> args

let make_args ~attributes =
  ["-x"; "${relativeFile}"] |>
  string_args "copybooks" ~attributes
    ~append:(fun l args -> List.flatten (List.map (fun l -> ["-I"; l]) l) @ args) |>
  string_arg "dialect" ~attributes
    ~mk:((^) "-std=")|>
  string_arg "source-format" ~attributes
    ~mk:((^) "-fformat=") |>
  bool_flag_arg "for-debug" ~attributes
    ~ok:(fun args -> "-fsource-location" :: "-ftraceall" ::
                     "-g" ::
                     "-Q" :: "--coverage" ::
                     "-A" :: "--coverage" :: args) |>
  string_args "extra-args" ~attributes
    ~append:(fun args' args -> args @ args') |>
  List.map (fun elt -> `String elt)

let cobc_execution ~attributes =
  let cobc =
    match List.assoc_opt "cobc-path" attributes with
    | Some exe ->
        [%js.to: string] exe
    | None ->
        match Superbol_workspace.cobc_exe () with    (* fallback to WS config *)
        | None -> "cobc"
        | Some s -> s
  in
  `ShellExecution (ShellExecution.makeCommandArgs ()
                     ~command:(`String cobc)
                     ~args:(make_args ~attributes))

let cobc_build_task ?task name ~attributes =
  let definition = match task with
    | None -> TaskDefinition.create () ~type_ ~attributes
    | Some def -> Task.definition def
  in
  Task.make ()
    ~definition
    ~scope:TaskScope.Workspace
    ~name
    ~source:(Option.fold ~none:"SuperBOL" ~some:Task.source task)
    ~execution:(cobc_execution ~attributes)
    ~problemMatchers:[
      "$gnucobol";
      "$gnucobol-warning";
      "$gnucobol-error";
      "$gnucobol-note";
    ]

(* --- *)

let provide_tasks ~token:_ =
  let attributes =
    List.map (fun (a, C (_, f, d)) -> a, f d) (attributes_spec ~debug:false)
  and attributes_for_debug =
    List.map (fun (a, C (_, f, d)) -> a, f d) (attributes_spec ~debug:true)
  in
  `Value (Some [
      cobc_build_task "build"         ~attributes:attributes;
      cobc_build_task "build (debug)" ~attributes:attributes_for_debug;
    ])

let resolve_task =
  let resolve_cnt =
    let resolve = ref 0 in
    fun () ->
      incr resolve;
      !resolve
  in
  fun ~task ~token:_ ->
    let oc =
      Window.createOutputChannel
        ~name:("Resolve" ^ string_of_int (resolve_cnt ()))
    in
    OutputChannel.appendLine oc ~value:"Starting resolve";
    let definition = Task.definition task in
    OutputChannel.appendLine oc ~value:"Got definition";
    let attributes =
      List.filter_map begin fun (a, C (f, t, _)) ->
        match f (TaskDefinition.get_attribute definition a) with
        | Some x -> Some (a, t x)
        | _ | exception _ -> None
      end (attributes_spec ~debug:false)
    in
    OutputChannel.appendLine oc ~value:"Got attributes";
    let task = cobc_build_task ~task ~attributes (Task.name task) in
    OutputChannel.appendLine oc ~value:"Task made";
    `Value (Some task)

(* --- *)

let provider =
  TaskProvider.Default.create
    ~provideTasks:provide_tasks
    ~resolveTask:resolve_task
