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

open Vscode

(* TODO: could use "${defaultBuildTask}" *)
module SuperBOLConsts = struct
  let taskType = "superbol"
  let taskSource = "SuperBOL"
  let workspaceTaskSource = "Workspace"
  let buildTaskName = "build (debug)"
  let moduleBuildTaskName = "build (module debug)"
  let workspaceBuildTaskName =
    Printf.sprintf "%s: %s" taskSource buildTaskName
  let workspaceModuleBuildTaskName =
    Printf.sprintf "%s: %s" taskSource moduleBuildTaskName
end

(* TODO: could look for debug tasks with "forDebug:true" *)
let findSuperBOLBuildTasks () =
  let find tasks source name =
    List.exists (fun t ->
        Task.source t = source && Task.name t = name
    ) tasks
  in
  let filter = TaskFilter.create ~type_:SuperBOLConsts.taskType () in
  Tasks.fetchTasks ~filter () |>
  Promise.then_ ~fulfilled:(fun tasks ->
      let buildTask =
        if find tasks SuperBOLConsts.workspaceTaskSource
            SuperBOLConsts.workspaceBuildTaskName then
          Some (SuperBOLConsts.workspaceBuildTaskName)
        else if find tasks SuperBOLConsts.taskSource
            SuperBOLConsts.buildTaskName then
          Some (SuperBOLConsts.workspaceBuildTaskName)
        else
          None
      in
      let moduleBuildTask =
        if find tasks SuperBOLConsts.workspaceTaskSource
            SuperBOLConsts.workspaceBuildTaskName then
          Some (SuperBOLConsts.workspaceModuleBuildTaskName)
        else if find tasks SuperBOLConsts.taskSource
            SuperBOLConsts.moduleBuildTaskName then
          Some (SuperBOLConsts.workspaceModuleBuildTaskName)
        else
          None
      in
      Promise.resolve (buildTask, moduleBuildTask)
    )

module GdbDebugConfiguration = struct
  open Interop
  include Interface.Extend (DebugConfiguration) ()
  include DebugConfiguration

  (* NOTE: name, type and request are mandatory in launch.json,
           but will be undefined if launch.json is absent *)
  include
    [%js:
    val name: t -> string or_undefined [@@js.get]
    val type_: t -> string or_undefined [@@js.get]
    val request: t -> string or_undefined [@@js.get]
    val preLaunchTask: t -> string or_undefined [@@js.get]
    val pid : t -> string or_undefined [@@js.get]
    val remoteDebugger : t -> string or_undefined [@@js.get]
    val target : t -> string or_undefined [@@js.get]
    val arguments : t -> string or_undefined [@@js.get]
    val cwd : t -> string or_undefined [@@js.get]
    val group : t -> string maybe_list [@@js.get]
    val env : t -> string Interop.Dict.t [@@js.get]
    val verbose : t -> bool or_undefined [@@js.get]
    val gdbtty : t -> Types.GdbTty.t or_undefined [@@js.get]
    val useCobcrun : t -> bool or_undefined [@@js.get]
    val cobcrunPath : t -> string or_undefined [@@js.get]
    val sourceDirs : t -> string maybe_list [@@js.get]

    val set_name: t -> string -> unit [@@js.set]
    val set_type: t -> string -> unit [@@js.set]
    val set_request: t -> string -> unit [@@js.set]
    val set_preLaunchTask: t -> string or_undefined -> unit [@@js.set]
    val set_pid : t -> string or_undefined -> unit [@@js.set]
    val set_remoteDebugger : t -> string or_undefined -> unit [@@js.set]
    val set_target : t -> string or_undefined -> unit [@@js.set]
    val set_arguments : t -> string or_undefined -> unit [@@js.set]
    val set_cwd : t -> string or_undefined -> unit [@@js.set]
    val set_group : t -> string maybe_list -> unit [@@js.set]
    val set_env : t -> string Interop.Dict.t -> unit [@@js.set]
    val set_verbose : t -> bool or_undefined -> unit [@@js.set]
    val set_gdbtty : t -> Types.GdbTty.t or_undefined -> unit [@@js.set]
    val set_useCobcrun : t -> bool or_undefined -> unit [@@js.set]
    val set_cobcrunPath : t -> string or_undefined -> unit [@@js.set]
    val set_sourceDirs : t -> string maybe_list -> unit [@@js.set]

    val create :
         name:string -> type_:string -> request:string
      -> ?preLaunchTask:string -> ?pid:string -> ?remoteDebugger:string
      -> ?target:string -> ?arguments:string -> ?cwd:string
      -> ?group:string list -> ?env:string Interop.Dict.t -> ?verbose:bool
      -> ?gdbtty:Types.GdbTty.t -> ?useCobcrun:bool -> ?cobcrunPath:string
      -> ?sourceDirs:string list -> unit -> t [@@js.builder]]
end

module GdbConfigurationProvider = struct

  let wrapPromise p = `Promise p

  (* This function is called only when creating a new launch.json.
     It is not called when adding configurations to launch.json:
     in this case, only snippets from package.json are used.
     Ideally, the code here should match the snippets in package.json. *)
  let provideDebugConfigurations ~folder:_ ?token:_ () :
    DebugConfiguration.t list ProviderResult.t =
    findSuperBOLBuildTasks () |>
    Promise.then_ ~fulfilled:(fun (buildTask, moduleBuildTask) ->
        let launchConfig = GdbDebugConfiguration.create
            ~name:"SuperBOL: debug (launch)"
            ~type_:"superbol-gdb"
            ~request:"launch"
            ?preLaunchTask:buildTask
            ~target:"${file}"
            ~arguments:""
            ~cwd:"${workspaceFolder}"
            ~gdbtty:(Bool true) ()
        in
        let launchModuleConfig = GdbDebugConfiguration.create
            ~name:"SuperBOL: debug (launch module)"
            ~type_:"superbol-gdb"
            ~request:"launch"
            ?preLaunchTask:moduleBuildTask
            ~target:"${file}"
            ~arguments:""
            ~cwd:"${workspaceFolder}"
            ~gdbtty:(Bool true)
            ~useCobcrun:true ()
        in
        let attachLocalConfig = GdbDebugConfiguration.create
            ~name:"SuperBOL: debug (attach local)"
            ~type_:"superbol-gdb"
            ~request:"attach"
            ~pid:"${input:pid}"
            ~target:"${file}"
            ~cwd: "${workspaceFolder}" ()
        in
        let attachLocalModuleConfig = GdbDebugConfiguration.create
            ~name:"SuperBOL: debug (attach local module)"
            ~type_:"superbol-gdb"
            ~request:"attach"
            ~pid:"${input:pid}"
            ~target:"${file}"
            ~cwd: "${workspaceFolder}"
            ~useCobcrun:true ()
        in
        let attachRemoteConfig = GdbDebugConfiguration.create
            ~name:"SuperBOL: debug (attach remote)"
            ~type_:"superbol-gdb"
            ~request:"attach"
            ~remoteDebugger:"${input:remoteDebugger}"
            ~target:"${file}"
            ~cwd: "${workspaceFolder}" ()
        in
        let attachRemoteModuleConfig = GdbDebugConfiguration.create
            ~name:"SuperBOL: debug (attach remote module)"
            ~type_:"superbol-gdb"
            ~request:"attach"
            ~remoteDebugger:"${input:remoteDebugger}"
            ~target:"${file}"
            ~cwd: "${workspaceFolder}"
            ~useCobcrun:true ()
        in
        Promise.return
          (Some [launchConfig; launchModuleConfig;
                 attachLocalConfig; attachLocalModuleConfig;
                 attachRemoteConfig; attachRemoteModuleConfig])) |>
    wrapPromise

  (* This function is called when lauching/attaching a program.
     The input config is populated with whatever task has been
     selected by the user (from his launch.json file), or has
     all its fields undefined if no launch.json file is present.
     IMPORTANT: this should replace any "None" with suitable defaults
     (except for preLaunchTask, pid, remoteDebugger and cwd), as this
     makes GdbLaunchArguments and GdbAttachArguments easier to
     define and use *)
  let resolveDebugConfiguration
      ~folder:_workspaceFolder ~debugConfiguration:config ?token:_ () :
    DebugConfiguration.t ProviderResult.t =
    findSuperBOLBuildTasks () |>
    Promise.then_ ~fulfilled:(fun (buildTask, _moduleBuildTask) ->
        begin
          let open GdbDebugConfiguration in
          let noLaunchJson =
            name config = None ||
            type_ config = None ||
            request config = None
          in
          if name config = None then
            set_name config "SuperBOL: default debug";
          if type_ config = None then
            set_type config "superbol-gdb";
          if request config = None then
            set_request config "launch";
          if preLaunchTask config = None && noLaunchJson then
            set_preLaunchTask config buildTask;
          if target config = None then
            set_target config (Some "${file}");
          if arguments config = None then
            set_arguments config (Some "");
          if verbose config = None then
            set_verbose config (Some (false));
          if gdbtty config = None &&
             request config = Some ("launch") then
            set_gdbtty config (Some (Bool true));
          if useCobcrun config = None then
            set_useCobcrun config (Some (false));
          if cobcrunPath config = None then
            set_cobcrunPath config (Some (Settings.cobcrunPath ()));
          let libcobpath = Settings.libcobPath () in
          if libcobpath <> "" then
            begin
              let env = env config in
              let libpath =
                match Interop.Dict.find_opt "LD_LIBRARY_PATH" env with
                | None -> libcobpath
                | Some (libpath) ->
                    Printf.sprintf "%s%c%s" libcobpath
                      Node.Path.delimiter libpath
              in
              let env = Interop.Dict.add "LD_LIBRARY_PATH" libpath env in
              set_env config env
            end
        end;
        Promise.return (Some (config))) |>
    wrapPromise

  let resolveDebugConfigurationWithSubstitutedVariables
      ~folder ~debugConfiguration ?token () :
    DebugConfiguration.t ProviderResult.t =
    resolveDebugConfiguration ~folder ~debugConfiguration ?token ()

  let create () =
    DebugConfigurationProvider.create
      ~provideDebugConfigurations
      ~resolveDebugConfiguration
      ~resolveDebugConfigurationWithSubstitutedVariables

end

module GdbAdapterDescriptorFactory = struct

  let createDebugAdapterDescriptor ~session:_ ~executable:_ :
    DebugAdapterDescriptor.t ProviderResult.t =
    let gdbSession = Gdb.GdbDebugSession.create () in
    let implementation =
      Gdb.GdbDebugSession.toVSCodeDebugAdapter gdbSession in
    `Value (Some (`DebugAdapterInlineImplementation
                    (DebugAdapterInlineImplementation.make ~implementation)))

  let create () =
    DebugAdapterDescriptorFactory.create ~createDebugAdapterDescriptor

end

module GnuCOBOLEvalExpressionProvider = struct

  let maxColumnIndex = 300

  let cobolReservedWords =
    Types.StrSet.of_list [
    "perform"; "move"; "to"; "set"; "add"; "subtract"; "call"; "inquire";
    "modify"; "invoke"; "if"; "not"; "end-if"; "until"; "varying";
    "evaluate"; "true"; "when"; "false"; "go"; "thru"; "zeros"; "spaces";
    "zero"; "space"; "inspect"; "tallying"; "exit"; "paragraph"; "method";
    "cycle"; "from"; "by"; "and"; "or"; "of"; "length"; "function";
    "program"; "synchronized"; "end-synchronized"; "string"; "end-string";
    "on"; "reference"; "value"; "returning"; "giving"; "replacing"; "goback";
    "all"; "open"; "i-o"; "input"; "output"; "close"; "compute"; "unstring";
    "using"; "delete"; "start"; "read"; "write"; "rewrite"; "with"; "lock";
    "else"; "upper-case"; "lower-case"; "display"; "accept"; "at";
    "clear-screen"; "initialize"; "line"; "col"; "key"; "is"; "self";
    "null"; "stop"; "run"; "upon"; "environment-name"; "environment-value"]

  let getDocumentLine document position =
    let line = Position.line position in
    let start = Position.make ~line ~character:0 in
    let end_ = Position.make ~line ~character:maxColumnIndex in
    let range = Range.makePositions ~start ~end_ in
    TextDocument.getText document ~range ()

  let getSelectionRangeInEditor () =
    Option.bind (Window.activeTextEditor ()) (fun textEditor ->
        let selection = TextEditor.selection textEditor in
        let start = Selection.start selection in
        let end_ = Selection.end_ selection in
        if Position.compareTo start ~other:end_ <> 0 then
          Some (Range.makePositions ~start ~end_)
        else
          None
      )

  let provideEvaluatableExpression ~document ~position ~token:_ :
    EvaluatableExpression.t ProviderResult.t =
    let txtLine = getDocumentLine document position in
    let result =
      if String.starts_with ~prefix:"      *" txtLine then
        None
      else
        match getSelectionRangeInEditor () with
        | Some (selectionRange) ->
            Some (EvaluatableExpression.create ~range:selectionRange ())
        | None ->
            let wordRange =
              TextDocument.getWordRangeAtPosition document ~position () |>
              Option.value
                ~default:(Range.makeCoordinates ~startLine:0
                            ~startCharacter:0 ~endLine:0 ~endCharacter:0)
            in
            let txtToEval =
              TextDocument.getText document ~range:wordRange () in
            let createExp =
              if Types.StrSet.mem (String.lowercase_ascii txtToEval)
                  cobolReservedWords then
                false
              else
                let txtRegex = Str.regexp_case_fold
                    ({|.*\*>.*|} ^ (Str.quote txtToEval) ^ {|.*$|}) in
                if Util.matches txtRegex txtLine then
                  let pos =
                    Position.make ~line:(Position.line position)
                      ~character:(Util.indexOf txtLine "*>") in
                  not (Position.isAfter (Range.end_ wordRange) ~other:pos)
                else
                  true
            in
            if createExp then
              Some (EvaluatableExpression.create ~range:wordRange ())
            else
              None
    in
    `Value (result)

  let create () =
    EvaluatableExpressionProvider.create ~provideEvaluatableExpression

end

let activate (extension: ExtensionContext.t) =
  let provider = GdbConfigurationProvider.create () in
  ExtensionContext.subscribe extension
    ~disposable:(Debug.registerDebugConfigurationProvider
                   ~debugType:"superbol-gdb" ~provider ());
  let factory = GdbAdapterDescriptorFactory.create () in
  ExtensionContext.subscribe extension
    ~disposable:(Debug.registerDebugAdapterDescriptorFactory
                   ~debugType:"superbol-gdb" ~factory ());
  (* TODO: reactivate and finish this feature *) (*
  let provider = GnuCOBOLEvalExpressionProvider.create () in
  List.iter (fun selector ->
      ExtensionContext.subscribe extension
        ~disposable:(Languages.registerEvaluatableExpressionProvider
                       ~selector:(`String selector) ~provider)
    ) [ "GnuCOBOL"; "GnuCOBOL31"; "GnuCOBOL3.1";
        "GnuCOBOL32"; "GnuCOBOL3.2"; "COBOL" ]; *)
  Promise.return ()

let deactivate () =
  Promise.return ()
