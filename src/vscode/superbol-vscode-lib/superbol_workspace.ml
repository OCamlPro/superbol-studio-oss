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

let plural n one many = if n = 1 then one else many

(** {2 Workspace-wide analysis} *)

(* Same extensions as the `cobol' language contribution. *)
let cobol_file_patterns =
  [
    "**/*.[cC]{ob,OB,bl,BL,py,PY,bx,BX,bsql}";
    "**/*.[pP]{co,CO}";
  ]

let exclude_dirs_setting = "cobol.excludeDirs"

(* Same treatment of entries as `excludes_dir' in
   `superbol_project/project_config.ml'. *)
let trim_dir s =
  let s =
    if String.starts_with ~prefix:"./" s
    then String.sub s 2 (String.length s - 2) else s
  in
  if String.length s > 1 && String.ends_with ~suffix:"/" s
  then String.sub s 0 (String.length s - 1) else s

(* [relative_path] is that of a file, with "/" as separator.  Entries that hold
   a "/" are directory paths relative to the root; others are directory names
   that match at any depth. *)
let excluded_by entries ~relative_path =
  let entries = List.map trim_dir entries in
  let paths, names = List.partition (fun e -> String.contains e '/') entries in
  let rec check prefix = function
    | [] | [_] ->                                     (* the file name itself *)
        false
    | dir :: rest ->
        let path = if prefix = "" then dir else prefix ^ "/" ^ dir in
        List.mem dir names || List.mem path paths || check path rest
  in
  check "" (String.split_on_char '/' relative_path)

(* Exclusions are taken from the setting of each workspace folder, which the
   server also uses for projects that have no `superbol.toml'.

   Files are filtered after the search, rather than via the `excludes' glob
   given to `findFiles': that glob would replace the `files.exclude' setting,
   that we want to keep honoring. *)
let not_excluded uris =
  let entries_by_folder = Hashtbl.create 1 in
  let entries_for folder =
    let key = Uri.toString (WorkspaceFolder.uri folder) () in
    match Hashtbl.find_opt entries_by_folder key with
    | Some entries -> entries
    | None ->
        let entries =
          Superbol_config.strings exclude_dirs_setting
            ~scope:(`WorkspaceFolder folder)
        in
        Hashtbl.add entries_by_folder key entries;
        entries
  in
  List.filter begin fun uri ->
    match Workspace.getWorkspaceFolder ~uri with
    | None ->
        true
    | Some folder ->
        not @@ excluded_by (entries_for folder) ~relative_path:
          (Workspace.asRelativePath () ~pathOrUri:(`Uri uri)
             ~includeWorkspaceFolder:false)
  end uris

let find_cobol_files ~token =
  let open Promise.Syntax in
  let rec aux acc = function
    | [] ->
        Promise.return (not_excluded @@ List.rev acc)
    | pattern :: patterns ->
        let* uris = Workspace.findFiles () ~includes:(`String pattern) ~token in
        aux (List.rev_append uris acc) patterns
  in
  aux [] cobol_file_patterns

(* Files are analyzed by `superbol cache analyze' processes (see
   {!Superbol_jobs}), that fill the caches of the server.  Documents are not
   opened in the server: it finds their workspace symbols in its caches, and
   restores their analyses once they are opened.  Once a batch of files is
   analyzed, the server is asked to publish their diagnostics, that it also
   finds in its caches. *)

(* [publish_cached_diagnostics client uris] asks the server to publish the
   diagnostics of [uris] it finds in its caches, and gives the number of those
   it does not find (all of them in case of failure, that is logged). *)
let publish_cached_diagnostics client uris =
  let data =
    Jsonoo.Encode.(object_ [
        "uris", list (fun uri -> string @@ Uri.toString uri ()) uris
      ])
  in
  Vscode_languageclient.LanguageClient.sendRequest client ()
    ~meth:"superbol/publishCachedDiagnostics" ~data |>
  Promise.then_
    ~fulfilled:begin fun reply ->
      Promise.return @@
      try List.length Jsonoo.Decode.(field "missed" (list id) reply)
      with Jsonoo.Decode_error _ -> 0
    end
    ~rejected:begin fun err ->
      let err_js = Promise.error_to_js err in
      Superbol_printer.log_error
        "SuperBOL: could not publish diagnostics of %u file%s: %s"
        (List.length uris) (plural (List.length uris) "" "s")
        (Ojs.string_of_js (Ojs.get_prop_ascii err_js "message"));
      Promise.return (List.length uris)
    end

let report_processes_completion ~analyzed ~failed ~missed ~undiagnosed =
  let total = analyzed + failed + missed in
  let outcome =
    if missed = 0 then
      Printf.sprintf "SuperBOL: analyzed %u file%s"
        analyzed (plural analyzed "" "s")
    else
      Printf.sprintf "SuperBOL: analysis interrupted after %u of %u file%s"
        (analyzed + failed) total (plural total "" "s")
  and failures =
    if failed = 0 then ""
    else Printf.sprintf "; %u file%s could not be analyzed (see the log)"
        failed (plural failed "" "s")
  and diagnostics =
    if undiagnosed = 0 then ""
    else Printf.sprintf "; diagnostics of %u file%s are reported upon \
                         opening %s"
        undiagnosed (plural undiagnosed "" "s")
        (plural undiagnosed "it" "them")
  in
  let _ =
    Window.showInformationMessage ()
      ~message:(outcome ^ failures ^ diagnostics) |>
    Promise.then_ ~fulfilled:(fun (_: unit option) -> Promise.return ())
  in
  Promise.return ()

let analyze_workspace_in_processes ~client cli ~progress ~token =
  let open Promise.Syntax in
  let* uris = find_cobol_files ~token in
  let total = List.length uris in
  let percent i = i * 100 / max 1 total in
  let analyzed = ref 0 and failed = ref 0 in
  let publications = ref [] in
  let on_start ~processes =
    Progress.report progress ~value:Progress.{
        message = Some (Printf.sprintf "analyzing %u file%s in %u process%s"
                          total (plural total "" "s")
                          processes (plural processes "" "es"));
        increment = None;
      }
  and on_completion (job: Superbol_jobs.job) ~ok =
    let before = !analyzed + !failed and n = List.length job.files in
    if ok then begin
      analyzed := !analyzed + n;
      publications :=
        publish_cached_diagnostics client job.files :: !publications
    end else
      failed := !failed + n;
    let after = before + n in
    Progress.report progress ~value:Progress.{
        message = Some (Printf.sprintf "%u/%u files" after total);
        increment = Some (percent after - percent before);
      }
  in
  let* () =
    Superbol_jobs.run cli ~token ~on_start ~on_completion
      (Superbol_jobs.make uris)
  in
  let* undiagnosed = Promise.all (Array.of_list !publications) in
  report_processes_completion ~analyzed:!analyzed ~failed:!failed
    ~missed:(total - !analyzed - !failed)
    ~undiagnosed:(Array.fold_left (+) 0 undiagnosed)

let with_analysis_progress task =
  Window.withProgress (module Interop.Js.Unit) ~task
    ~options:(ProgressOptions.create
                ~location:(`ProgressLocation ProgressLocation.Notification)
                ~title:"SuperBOL: analyzing COBOL files"
                ~cancellable:true ())

let run_analysis instance =
  match Superbol_instance.client instance with
  | None ->
      Superbol_printer.show_error_message @@
      Error Superbol_types.Client_not_running
  | Some client ->
      with_analysis_progress @@
      analyze_workspace_in_processes ~client
        (Superbol_instance.cli_invocation instance)

(** {2 Copybook directory retrieval} *)

(* Extensions used to look up copybooks.  Lowercased like the server does. *)
let copybook_extensions () =
  List.map String.lowercase_ascii @@
  Superbol_config.strings Superbol_tasks.copyexts_setting

let copybook_file_pattern exts =
  Printf.sprintf "**/*.{%s}" @@
  String.concat "," @@
  List.concat_map (fun ext -> [ext; String.uppercase_ascii ext]) exts

module Json_list = Interop.Js.List (Jsonoo)

(* Workspace value only: the default and the user-wide value must not be
   copied into the workspace settings.  Malformed entries are skipped. *)
let configured_copybook_dirs () =
  match
    WorkspaceConfiguration.inspect (module Json_list) (Superbol_config.full ())
      ~section:Superbol_tasks.copybooks_setting
  with
  | Some { WorkspaceConfiguration.workspaceValue = Some entries; _ } ->
      List.filter_map
        (Jsonoo.Decode.try_optional Superbol_tasks.copybook_path_of_jsonoo)
        entries
  | _ ->
      []

let find_copybook_files exts =
  Workspace.findFiles () ~includes:(`String (copybook_file_pattern exts))

(* `asRelativePath' always uses "/", on every platform.  It gives back the
   whole path for a file that is outside the workspace folder. *)
let relative_dir_of uri =
  Filename.dirname @@
  Workspace.asRelativePath ~pathOrUri:(`Uri uri) ~includeWorkspaceFolder:false ()

(* A file-relative entry covers any directory ending with it, like the
   server's `file_is_in_libpath'. *)
let covered_by existing dir =
  List.exists begin fun Superbol_tasks.{ dir = d; file_relative } ->
    if file_relative then String.ends_with ~suffix:d dir else d = dir
  end existing

let missing_dirs ~root_fs existing =
  List.filter begin fun Superbol_tasks.{ dir; file_relative } ->
    not file_relative &&
    not (Node.Fs.existsSync (Node.Path.join [root_fs; dir]))
  end existing

let report_retrieval ~added ~total ~missing =
  let outcome =
    if added = 0 then
      Printf.sprintf "SuperBOL: copybook paths are already up to date \
                      (%u director%s configured)"
        total (plural total "y" "ies")
    else
      Printf.sprintf "SuperBOL: added %u copybook director%s to the \
                      workspace settings"
        added (plural added "y" "ies")
  and stale =
    match List.length missing with
    | 0 -> ""
    | n -> Printf.sprintf "; %u configured director%s no longer exist%s"
             n (plural n "y" "ies") (plural n "s" "")
  in
  let message = outcome ^ stale in
  (* Do not wait for the answer: the message only closes when the user acts on
     it, and the progress notification would stay up until then. *)
  let _ =
    Window.showInformationMessage () ~message ~choices:["Show Settings", ()] |>
    Promise.then_ ~fulfilled:begin function
      | Some () ->
          let _ =
            Commands.executeCommand ~args:[]
              ~command:"workbench.action.openWorkspaceSettingsFile"
          in
          Promise.return ()
      | None ->
          Promise.return ()
    end
  in
  Promise.return ()

let retrieve_copybook_dirs ~exts ~root_fs =
  let open Promise.Syntax in
  let* uris = find_copybook_files exts in
  let existing = configured_copybook_dirs () in
  let missing = missing_dirs ~root_fs existing in
  let found =
    List.sort_uniq String.compare @@ List.map relative_dir_of uris
  in
  let added =
    List.filter_map begin fun dir ->
      if covered_by existing dir
      then None
      else Some Superbol_tasks.{ dir; file_relative = false }
    end found
  in
  if found = [] then begin
    let _ =
      Window.showInformationMessage ()
        ~message:(Printf.sprintf
                    "SuperBOL: no copybook found in the workspace (looking \
                     for %s files)" @@
                  String.concat ", " @@
                  List.map (fun ext -> "`." ^ ext ^ "'") exts)
    in
    Promise.return ()
  end else if added = [] then
    report_retrieval ~added:0 ~total:(List.length existing) ~missing
  else
    let* () =
      WorkspaceConfiguration.update (Superbol_config.full ())
        ~section:Superbol_tasks.copybooks_setting
        ~value:(Jsonoo.t_to_js @@
                Jsonoo.Encode.list Superbol_tasks.copybook_path_to_jsonoo
                  (existing @ added))
        ~configurationTarget:
          (`ConfigurationTarget ConfigurationTarget.Workspace) ()
    in
    report_retrieval ~added:(List.length added)
      ~total:(List.length existing + List.length added) ~missing

(* Relative paths resolve against the server's working directory, ie. the
   first workspace folder.  Only safe with a single folder. *)
let run_copybook_retrieval () =
  match Workspace.workspaceFolders () with
  | [] ->
      let _ =
        Window.showWarningMessage ()
          ~message:"SuperBOL: open a folder before retrieving copybook \
                    directories"
      in
      Promise.return ()
  | _ :: _ :: _ ->
      let _ =
        Window.showWarningMessage ()
          ~message:"SuperBOL: copybook retrieval is not supported in \
                    multi-root workspaces yet"
      in
      Promise.return ()
  | [folder] ->
      match copybook_extensions () with
      | [] ->
          let _ =
            Window.showWarningMessage ()
              ~message:"SuperBOL: no extension listed in \
                        `superbol.cobol.copyexts'"
          in
          Promise.return ()
      | exts ->
          retrieve_copybook_dirs ~exts
            ~root_fs:(Uri.fsPath @@ WorkspaceFolder.uri folder)
