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

(* Diagnostics stay in the Problems view once the document is closed. *)

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

type report = {
  file: string;              (* the report file, in [root]/_superbol *)
  root: string;              (* the workspace folder it was made for *)
}

(* Written while the analysis runs, so a crash keeps the results found so far. *)
let create_report () =
  match Workspace.workspaceFolders () with
  | [] -> None
  | folder :: _ ->
      let root = Uri.fsPath (WorkspaceFolder.uri folder) in
      let dir = Node.Path.join [root; "_superbol"] in
      let file = Node.Path.join [dir; "analysis-report.md"] in
      try
        Node.Fs.mkdirSync dir ~recursive:true;
        Node.Fs.writeFileSync file "";
        Some { file; root }
      with e ->
        Superbol_printer.log_error
          "SuperBOL: cannot create %s: %s" file (Printexc.to_string e);
        None

let append_to_report report text =
  match report with
  | None -> ()
  | Some { file; _ } ->
      try Node.Fs.appendFileSync file text with e ->
        Superbol_printer.log_error
          "SuperBOL: cannot write %s: %s" file (Printexc.to_string e)

let text_document_id uri =
  Jsonoo.Encode.(object_ ["uri", string @@ Uri.toString uri ()])

(* The reply only comes once the server has handled the `didOpen' for [uri], so
   waiting for it paces the loop on real work. *)
let await_analysis_of ~uri instance =
  Superbol_instance.lsp_request instance
    ~meth:"textDocument/documentSymbol"
    ~data:Jsonoo.Encode.(object_ ["textDocument", text_document_id uri]) |>
  Promise.then_
    ~fulfilled:(fun _ -> Promise.return ())
    ~rejected:(fun _ -> Promise.return ())

(* VS Code cannot close a document opened with `openTextDocument', so we
   notify the server ourselves and keep one document in memory at a time. *)
let notify_did_open ~uri ~text instance =
  Superbol_instance.lsp_notification instance
    ~meth:"textDocument/didOpen"
    ~data:Jsonoo.Encode.(object_ [
        "textDocument", object_ [
          "uri", string @@ Uri.toString uri ();
          "languageId", string "cobol";
          "version", int 1;
          "text", string text;
        ];
      ])

let notify_did_close ~uri instance =
  Superbol_instance.lsp_notification instance
    ~meth:"textDocument/didClose"
    ~data:Jsonoo.Encode.(object_ ["textDocument", text_document_id uri])

(* Documents already open are synced by the client: do not close them. *)
let is_already_open uri =
  let uri = Uri.toString uri () in
  List.exists
    (fun doc -> Uri.toString (TextDocument.uri doc) () = uri)
    (Workspace.textDocuments ())

(* [false] when the file could not be read, so it was never analyzed. *)
let analyze_document ~uri ~report instance =
  let open Promise.Syntax in
  Promise.catch
    ~rejected:begin fun error ->
      let value =
        Printf.sprintf "%s: skipped, %s"
          (Workspace.asRelativePath () ~pathOrUri:(`Uri uri))
          (Node.JsError.message error)
      in
      Superbol_printer.log_error "SuperBOL: %s" value;
      append_to_report report ("- " ^ value ^ "\n");
      Promise.return false
    end @@
  if is_already_open uri then
    let+ () = await_analysis_of ~uri instance in
    true
  else
    let* text = Node.Fs.readFile (Uri.fsPath uri) in
    notify_did_open ~uri ~text instance;
    let+ () = await_analysis_of ~uri instance in
    notify_did_close ~uri instance;
    true

let severity_name = function
  | DiagnosticSeverity.Error -> "error"
  | DiagnosticSeverity.Warning -> "warning"
  | DiagnosticSeverity.Information -> "note"
  | DiagnosticSeverity.Hint -> "hint"

(* The report is in [root]/_superbol, so a file in [root] is one "../" away.
   Other roots are outside [root]: link those by URI. *)
let report_target ~root uri =
  let path = Uri.fsPath uri in
  let prefix = root ^ String.make 1 Node.Path.sep in
  if String.starts_with ~prefix path then
    let rest =
      String.sub path (String.length prefix)
        (String.length path - String.length prefix)
    in
    (* Markdown wants "/", even on Windows. *)
    "../" ^ String.concat "/" (String.split_on_char Node.Path.sep rest)
  else
    Uri.toString uri ()

(* Markdown link, so that a click jumps to the reported line. *)
let report_link ~path ~target ~line ~char =
  Printf.sprintf "[%s:%u:%u](<%s#L%u>)" path line char target line

let report_line ~path ~target diag =
  let pos = Range.start @@ Diagnostic.range diag in
  Printf.sprintf "- %s: %s: %s\n"
    (report_link ~path ~target
       ~line:(succ @@ Position.line pos)
       ~char:(succ @@ Position.character pos))
    (severity_name @@ Diagnostic.severity diag)
    (Diagnostic.message diag)

let report_diagnostics_of ~uri report =
  match report, Languages.getDiagnostics uri with
  | None, _ | _, [] -> ()
  | Some { root; _ }, diags ->
      let path = Workspace.asRelativePath () ~pathOrUri:(`Uri uri) in
      let target = report_target ~root uri in
      append_to_report report @@
      String.concat "" @@ List.map (report_line ~path ~target) diags

let report_completion report ~analyzed ~skipped ~missed =
  let outcome =
    if missed = 0 then
      Printf.sprintf "SuperBOL: analyzed %u file%s"
        analyzed (plural analyzed "" "s")
    else
      let scanned = analyzed + skipped in
      let total = scanned + missed in
      Printf.sprintf "SuperBOL: analysis interrupted after %u of %u file%s"
        scanned total (plural total "" "s")
  and unread =
    if skipped = 0 then ""
    else Printf.sprintf "; %u file%s could not be read"
        skipped (plural skipped "" "s")
  and where =
    match report with
    | None ->
        "; diagnostics are listed in the Problems view"
    | Some { file; _ } ->
        Printf.sprintf "; diagnostics are listed in the Problems view and in %s"
          (Workspace.asRelativePath () ~pathOrUri:(`Uri (Uri.file file)))
  in
  let message = outcome ^ unread ^ where in
  append_to_report report (Printf.sprintf "\n%s%s\n" outcome unread);
  let _ =
    match report with
    | None ->
        Window.showInformationMessage () ~message |>
        Promise.then_ ~fulfilled:(fun (_: unit option) -> Promise.return ())
    | Some { file; _ } ->
        Window.showInformationMessage () ~message
          ~choices:["Show Report", ()] |>
        Promise.then_ ~fulfilled:begin function
          | Some () ->
              let _ =
                Window.showTextDocument ~document:(`Uri (Uri.file file)) ()
              in
              Promise.return ()
          | None ->
              Promise.return ()
        end
  in
  Promise.return ()

let analyze_workspace instance ~progress ~token =
  let open Promise.Syntax in
  let* uris = find_cobol_files ~token in
  let report = create_report () in
  let total = List.length uris in
  let percent i = i * 100 / max 1 total in
  let rec loop i skipped = function
    | remaining when CancellationToken.isCancellationRequested token ->
        report_completion report ~analyzed:(i - skipped) ~skipped
          ~missed:(List.length remaining)
    | [] ->
        report_completion report ~analyzed:(i - skipped) ~skipped ~missed:0
    | uri :: remaining ->
        Progress.report progress ~value:Progress.{
            message = Some (Printf.sprintf "%u/%u: %s" (succ i) total @@
                            Workspace.asRelativePath () ~pathOrUri:(`Uri uri));
            increment = Some (percent (succ i) - percent i);
          };
        let* analyzed = analyze_document ~uri ~report instance in
        if analyzed then report_diagnostics_of ~uri report;
        loop (succ i) (if analyzed then skipped else succ skipped) remaining
  in
  loop 0 0 uris

(** {3 Analysis in parallel processes}

    Files are analyzed by `superbol cache analyze' processes (see
    {!Superbol_jobs}), that fill the caches of the server.  Documents are not
    opened in the server: it finds their workspace symbols in its caches, and
    restores their analyses (and publishes their diagnostics) once they are
    opened. *)

let report_processes_completion ~analyzed ~failed ~missed =
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
  in
  let _ =
    Window.showInformationMessage ()
      ~message:(outcome ^ failures ^ "; diagnostics are reported upon \
                                       opening files") |>
    Promise.then_ ~fulfilled:(fun (_: unit option) -> Promise.return ())
  in
  Promise.return ()

let analyze_workspace_in_processes cli ~progress ~token =
  let open Promise.Syntax in
  let* uris = find_cobol_files ~token in
  let total = List.length uris in
  let percent i = i * 100 / max 1 total in
  let analyzed = ref 0 and failed = ref 0 in
  let on_start ~processes =
    Progress.report progress ~value:Progress.{
        message = Some (Printf.sprintf "analyzing %u file%s in %u process%s"
                          total (plural total "" "s")
                          processes (plural processes "" "es"));
        increment = None;
      }
  and on_completion (job: Superbol_jobs.job) ~ok =
    let before = !analyzed + !failed and n = List.length job.files in
    if ok then analyzed := !analyzed + n else failed := !failed + n;
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
  report_processes_completion ~analyzed:!analyzed ~failed:!failed
    ~missed:(total - !analyzed - !failed)

(* The server drops all diagnostics unless `forceSyntaxDiagnostics' is set or
   the dialect is COBOL85 (see `dispatch_diagnostics' in `lsp_server.ml').  The
   dialect is per project, so we warn instead of refusing. *)
let check_diagnostics_reported () =
  if Superbol_config.bool "forceSyntaxDiagnostics" ||
     Superbol_config.string "cobol.dialect" = "cobol85" then
    Promise.return `Scan
  else
    let open Promise.Syntax in
    let+ choice =
      Window.showWarningMessage ()
        ~message:"SuperBOL only reports diagnostics for projects that use the \
                  COBOL85 dialect, unless `superbol.forceSyntaxDiagnostics' is \
                  enabled.  The scan may find nothing to report."
        ~choices:["Enable and Restart Server", `Enable;
                  "Scan Anyway", `Scan]
    in
    Option.value choice ~default:`Abort

(* Writing the setting restarts the server.  We cannot await that, so we ask
   for a new run. *)
let enable_syntax_diagnostics () =
  let open Promise.Syntax in
  let target =
    if Workspace.workspaceFolders () = []
    then ConfigurationTarget.Global
    else ConfigurationTarget.Workspace
  in
  let+ () =
    WorkspaceConfiguration.update
      (Superbol_config.full ())
      ~section:"forceSyntaxDiagnostics"
      ~value:(Ojs.bool_to_js true)
      ~configurationTarget:(`ConfigurationTarget target) ()
  in
  let _ =
    Window.showInformationMessage ()
      ~message:"Diagnostics enabled.  The language server is restarting; \
                please run the analysis again."
  in
  ()

let with_analysis_progress task =
  Window.withProgress (module Interop.Js.Unit) ~task
    ~options:(ProgressOptions.create
                ~location:(`ProgressLocation ProgressLocation.Notification)
                ~title:"SuperBOL: analyzing COBOL files"
                ~cancellable:true ())

let scan_workspace instance =
  with_analysis_progress (analyze_workspace instance)

let run_analysis instance =
  match Superbol_instance.client instance with
  | None ->
      Superbol_printer.show_error_message @@
      Error Superbol_types.Client_not_running
  | Some _ ->
      (* A server we do not run ourselves may not share our file-system, so we
         leave the analysis to it, by opening every document. *)
      match Superbol_instance.cli_invocation instance with
      | Some cli ->
          with_analysis_progress (analyze_workspace_in_processes cli)
      | None ->
          let open Promise.Syntax in
          let* decision = check_diagnostics_reported () in
          match decision with
          | `Abort -> Promise.return ()
          | `Enable -> enable_syntax_diagnostics ()
          | `Scan -> scan_workspace instance

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
