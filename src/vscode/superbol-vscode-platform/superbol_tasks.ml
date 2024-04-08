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
    "for-debug", C ([%js.to: bool or_undefined],
                    [%js.of: bool], debug);
    "cobc-path", C ([%js.to: string or_undefined],
                    [%js.of: string], "cobc");
    "extra-args", C ([%js.to: string list or_undefined],
                     [%js.of: string list], []);
  ]

(* --- *)

let attr_bool_flag key ~ok ?(ko = Fun.id) ~attributes args =
  match List.assoc_opt key attributes with
  | Some flag when [%js.to: bool] flag -> ok args
  | None when Superbol_workspace.bool key -> ok args
  | _ -> ko args

let string_arg ?(allow_empty = false) ~mk s args =
  if s = "" && not allow_empty then args else mk s :: args

(* let attr_string ?allow_empty key ~mk ~attributes args = *)
(*   match List.assoc_opt key attributes with *)
(*   | Some s -> string_arg ([%js.to: string] s) ?allow_empty ~mk args *)
(*   | None -> args *)

let config_string key ~config =
  string_arg @@
  try Jsonoo.Decode.string @@ Hashtbl.find config key
  with Not_found -> Superbol_workspace.string key

let attr_strings key ~append ~attributes args =
  match List.assoc_opt key attributes with
  | Some l -> append ([%js.to: string list] l) args
  | None -> args

(* let config_strings key ~config:_ ~append args = *)
(*   append (Superbol_workspace.string_list key) args *)

type copybook_path =
  {
    dir: string;
    file_relative: bool;
  }

let copybook_path_of_jsonoo: Jsonoo.t -> copybook_path = fun j ->
  Jsonoo.Decode.{
    dir = field "dir" string j;
    file_relative = (try_default false @@ field "file-relative" bool) j;
  }

let config_copybook_paths key ~config ~append =
  append @@
  try
    Jsonoo.Decode.(list copybook_path_of_jsonoo) @@
    try Hashtbl.find config key
    with Not_found -> Jsonoo.t_of_js (Superbol_workspace.any key)
  with Jsonoo.Decode_error _ ->
    (* Warning: silenced decode errors for now *)
    []

let cobc_execution ?config attributes =
  let config = match config with Some t -> t | None -> Hashtbl.create 0 in
  let cobc =
    try [%js.to: string] @@ List.assoc "cobc-path" attributes
    with Not_found ->                                 (* fallback to WS config *)
      Option.value (Superbol_workspace.cobc_exe ()) ~default:"cobc"
  in
  let args =
    ["-x"; "${relativeFile}"] |>
    config_copybook_paths "cobol.copybooks" ~config
      ~append:begin fun l args ->
        List.flatten @@
        List.map begin fun { dir; file_relative } ->
          ["-I";
           (* Note: assumes `cobc` is called from the root folder of the
              project. *)
           if file_relative
           then "${relativeFileDirname}${pathSeparator}"^dir
           else dir]
        end l |>
        List.append args
      end |>
    config_string "cobol.dialect" ~config
      ~mk:(function "gnucobol" -> "-std=default" | s -> "-std=" ^ s) |>
    config_string "cobol.source-format" ~config
      ~mk:((^) "-fformat=") |>
    attr_bool_flag "for-debug" ~attributes
      ~ok:(fun args -> "-fsource-location" :: "-ftraceall" ::
                       "-g" ::
                       "-Q" :: "--coverage" ::
                       "-A" :: "--coverage" :: args) |>
    attr_strings "extra-args" ~attributes
      ~append:(fun args' args -> args @ args')
  in
  `ShellExecution (ShellExecution.makeCommandArgs ()
                     ~command:(`String cobc)
                     ~args:(List.map (fun elt -> `String elt) args))

let make_default_cobc_task ~name ~definition ~execution =
  let task =
    Task.make ~name ~definition ~execution ()
      ~source:"SuperBOL"
      ~scope:Workspace
      ~problemMatchers:[
        "$gnucobol";
        "$gnucobol-warning";
        "$gnucobol-error";
        "$gnucobol-note";
      ]
  in
  Task.set_group task TaskGroup.build;
  task

let cobc_build_task ~task ?config attributes =
  Promise.Option.return @@
  make_default_cobc_task
    ~name:(Task.name task)
    ~definition:(Task.definition task)
    ~execution:(cobc_execution ?config attributes)

let define_cobc_build_task ?config ~debug name =
  let map_attributes = List.map (fun (a, C (_, f, d)) -> a, f d) in
  let attributes = map_attributes @@ attributes_spec ~debug in
  make_default_cobc_task
    ~name
    ~definition:(TaskDefinition.create () ~type_ ~attributes)
    ~execution:(cobc_execution ?config attributes)

let provide_tasks instance ~token:_ =
  let open Promise.Syntax in
  `Promise begin
    let* config = Superbol_instance.get_project_config instance in
    let config = match config with
      | Error _ -> Hashtbl.create 0
      | Ok config -> config
    in
    Promise.Option.return [
      define_cobc_build_task "build"         ~config ~debug:false;
      define_cobc_build_task "build (debug)" ~config ~debug:true;
    ]
  end

let resolve_task =
  let oc =
    Window.createOutputChannel
      ~name:("SuperBOL Task Resolution")
  in
  let open Promise.Syntax in
  fun instance ~task ~token:_ ->
    `Promise begin
      OutputChannel.appendLine oc ~value:"Starting new resolution.";
      let definition = Task.definition task in
      let attributes =
        List.filter_map begin fun (a, C (f, t, _)) ->
          match f (TaskDefinition.get_attribute definition a) with
          | Some x -> Some (a, t x)
          | _ | exception _ -> None
        end (attributes_spec ~debug:false)
      in
      let* config = Superbol_instance.get_project_config instance in
      match config with
      | Error value ->
          OutputChannel.appendLine oc ~value;
          OutputChannel.appendLine oc ~value:"Aborting task resolution.";
          Promise.return None
      | Ok config ->
          OutputChannel.appendLine oc ~value:"Configuration: ";
          Hashtbl.iter begin fun k v ->
            OutputChannel.appendLine oc
              ~value:(Printf.sprintf "- %s: %s" k (Jsonoo.stringify v))
          end config;
          let task = cobc_build_task ~task ~config attributes in
          OutputChannel.appendLine oc ~value:"Resolution done.";
          task
    end

(* --- *)

let provider instance =
  TaskProvider.Default.create
    ~provideTasks:(provide_tasks instance)
    ~resolveTask:(resolve_task instance)
