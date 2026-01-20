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
  | O: (Ojs.t -> 'a option) * ('a -> Ojs.t) -> attribute_spec       (* w/o default *)

let attributes_spec ~debug ~coverage ~executable =
  [
    "forDebug", C ([%js.to: bool or_undefined],
                   [%js.of: bool], debug);
    "forCoverage", C ([%js.to: bool or_undefined],
                      [%js.of: bool], coverage);
    "executable", C ([%js.to: bool or_undefined],
                     [%js.of: bool], executable);
    "cobcPath", O ([%js.to: string or_undefined],
                   [%js.of: string]);
    "listingsTarget", C ([%js.to: string option or_undefined],
                         [%js.of: string option], None);
    "extraArgs", C ([%js.to: string list or_undefined],
                    [%js.of: string list], []);
  ]

let executable_spec = attributes_spec ~executable:true
let module_spec = attributes_spec ~executable:false

(* --- *)

let attr_bool_flag key ~ok ?(ko = Fun.id) ~attributes args =
  match List.assoc_opt key attributes with
  | Some flag when [%js.to: bool] flag -> ok args
  | None when Superbol_workspace.bool key -> ok args
  | _ -> ko args

let string_arg ?(allow_empty = false) ~append s args =
  if s = "" && not allow_empty then args else append s args

let config_string key ~config =
  string_arg @@
  try Jsonoo.Decode.string @@ Hashtbl.find config key
  with Not_found -> Superbol_workspace.string key

let config_strings key ~config ~append =
  append @@
  try Jsonoo.Decode.(list string) @@ Hashtbl.find config key
  with Not_found -> Superbol_workspace.strings key
     | Jsonoo.Decode_error _ -> []  (* Warning: silenced decode errors for now *)

let attr_strings key ?(append = List.append) ~attributes args =
  match List.assoc_opt key attributes with
  | Some l -> append ([%js.to: string list] l) args
  | None -> args

let attr_string_opt key ~append ~attributes args =
  match List.assoc_opt key attributes with
  | None -> args
  | Some s -> match [%js.to: string option] s with
    | None -> args
    | Some s -> string_arg s ~allow_empty:false ~append args

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

let cobc_path attributes =
  match [%js.to: string] @@ List.assoc "cobcPath" attributes with
  | exception Not_found | "" ->                       (* fallback to WS config *)
      Option.value (Superbol_workspace.cobc_exe ()) ~default:"cobc"
  | path ->
      path

let cobc_execution ?config attributes =
  let config = match config with Some t -> t | None -> Hashtbl.create 0 in
  let args =
    ["${relativeFile}"] |>
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
    config_strings "cobol.copyexts" ~config
      ~append:begin fun exts ->
        List.append @@ List.flatten @@ List.map (fun e -> ["-ext"; e]) exts
      end |>
    config_string "cobol.dialect" ~config
      ~append:begin function
        | "gnucobol" -> List.cons "-std=default"
        | s when s <> "" && not (Filename.is_relative s) ->
                  List.cons ("-conf=" ^ s)
        | s -> List.cons ("-std=" ^ s)
      end|>
    config_string "cobol.sourceFormat" ~config
      ~append:(fun f -> List.cons ("-fformat=" ^ f)) |>
    attr_bool_flag "forDebug" ~attributes
      ~ok:(List.append ["-ftraceall"; "-g"]) |>
    attr_bool_flag "forCoverage" ~attributes
      ~ok:(List.cons "--coverage") |>
    attr_bool_flag "executable" ~attributes
      ~ok:(List.cons "-x")
      ~ko:(List.cons "-m") |>
    attr_string_opt "listingsTarget" ~attributes
      ~append:(fun t -> List.append ["-P"; t]) |>
    attr_strings "extraArgs" ~attributes
  in
  `ShellExecution (ShellExecution.makeCommandArgs ()
                     ~command:(`String (cobc_path attributes))
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

let define_cobc_build_task ?config ~debug ?(coverage = false) ~spec name =
  let attributes =
    List.filter_map (function a, C (_, f, d) -> Some (a, f d) | _ -> None) @@
    spec ~debug ~coverage
  in
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
    let define_exec_build_task
      = define_cobc_build_task ~config ~spec:executable_spec
    and define_modl_build_task
      = define_cobc_build_task ~config ~spec:module_spec
    in
    Promise.Option.return [
      define_exec_build_task "build"          ~debug:false;
      define_exec_build_task "build (debug)"  ~debug:true;
      define_modl_build_task "build (module)" ~debug:false;
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
        let retrieve a f t =
          match f (TaskDefinition.get_attribute definition a) with
          | Some x -> Some (a, t x)
          | _ | exception _ -> None
        in
        List.filter_map begin function
          | a, C (f, t, _) -> retrieve a f t
          | a, O (f, t) -> retrieve a f t
        end (executable_spec ~debug:false ~coverage:false)
      in
      let* config = Superbol_instance.get_project_config instance in
      match config with
      | Error error ->
          OutputChannel.appendLine oc ~value:(Superbol_printer.show_error error);
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
