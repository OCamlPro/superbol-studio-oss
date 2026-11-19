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

open Vscode_languageclient
open Superbol_types

open Promise.Syntax

(* --- *)

type t = {
  context: Vscode.ExtensionContext.t;
  lsp_server_prefix: string;
  mutable language_client: LanguageClient.t option;
  mutable start_hooks: (LanguageClient.t -> unit) list;
}
type client = LanguageClient.t

let id = "superbol-lsp"

let name = "SuperBOL Language Server"

let make ~lsp_server_prefix ~context = {
  lsp_server_prefix;
  context;
  language_client = None;
  start_hooks = [];
}

let client { language_client; _ } = language_client

let context { context; _ } = context

let cli_invocation { context; lsp_server_prefix = server_prefix; _ } =
  Superbol_languageclient.cli_invocation ~context ~server_prefix

let on_language_server_start t hook =
  t.start_hooks <- t.start_hooks @ [hook]

let subscribe_disposable { context; _ } disposable =
  Vscode.ExtensionContext.subscribe context ~disposable

(** Context key that holds while the language server runs. *)
let running_context_key = "superbol.languageServerRunning"

let set_running_context running =
  let _: Ojs.t option Promise.t =
    Vscode.Commands.executeCommand ~command:"setContext"
      ~args:[Ojs.string_to_js running_context_key; Ojs.bool_to_js running]
  in ()

let stop_language_server t =
  match t.language_client with
  | None ->
      Promise.return ()
  | Some client ->
      t.language_client <- None;
      set_running_context false;     (* in case [client] is already stopped *)
      if LanguageClient.isRunning client then
        LanguageClient.stop client
      else
        Promise.return ()

let start_language_server ({ lsp_server_prefix = server_prefix; context; _ } as t) =
  let open Promise.Syntax in
  let* () = stop_language_server t in
  let client =
    LanguageClient.make () ~id ~name
      ~serverOptions:(Superbol_languageclient.server_options
                        ~server_prefix ~context)
      ~clientOptions:(Superbol_languageclient.client_options ())
  in
  (* The client may also stop on its own, e.g. after repeated crashes. *)
  let _: Vscode.Disposable.t =
    LanguageClient.onDidChangeState client ()
      ~listener:begin fun event ->
        set_running_context @@
        match StateChangeEvent.newState event with
        | Running -> true
        | Stopped | Starting -> false
      end
  in
  let+ () = LanguageClient.start client in
  t.language_client <- Some client;
  List.iter (fun hook -> hook client) t.start_hooks


let start_autorestarter instance =
  subscribe_disposable instance @@
  Vscode.Workspace.onDidChangeConfiguration ()
    ~listener:begin fun config_change ->
      if Superbol_languageclient.server_needs_restart_after ~config_change
      then begin
        subscribe_disposable instance @@
        Vscode.Window.setStatusBarMessage ()
          ~text:"Restarting SuperBOL Language Server…"
          ~hide:(`AfterTimeout 2000);
        let _ : unit Promise.t = start_language_server instance in ()
      end;
    end


module JSON = struct

  let current_text_document_id text_editor =
    let document = Vscode.TextEditor.document text_editor in
    let uri = Vscode.TextDocument.uri document in
    Jsonoo.Encode.(object_ ["uri", string @@ Vscode.Uri.toString uri ()])

end

let active_text_editor ?text_editor () =
  match text_editor with None -> Vscode.Window.activeTextEditor () | e -> e

let current_document ?text_editor () =
  match active_text_editor ?text_editor () with
  | None -> None
  | Some e -> Some (Vscode.TextEditor.document e)

let current_document_uri ?text_editor () =
  match current_document ?text_editor () with
  | None -> None
  | Some doc -> Some (Vscode.TextDocument.uri doc)

let current_selections ?text_editor () =
  match active_text_editor ?text_editor () with
  | None -> None
  | Some e -> Some (Vscode.TextEditor.selections e)

let write_project_config_for ?uri client =
  let assoc = match uri with
    | Some uri ->
        ["uri", Jsonoo.Encode.string @@ Vscode.Uri.toString uri ()]
    | None ->
        [] (* send without URI: the server will consider every known folder *)
  in
  Vscode_languageclient.LanguageClient.sendRequest client ()
    ~meth:"superbol/writeProjectConfiguration"
    ~data:(Jsonoo.Encode.object_ assoc) |>
  Promise.(then_ ~fulfilled:(fun _ -> return ()))

let with_context_and_client ~f instance =
  match client instance with
  | Some client ->
      f ~context:(context instance) ~client
  | None ->
      (* TODO: is there a way to activate the extension from here?  Starting the
         client/instance seems to launch two distinct LSP server processes. *)
      Promise.return @@ Error Client_not_running

let write_project_config ?text_editor instance =
  with_context_and_client instance ~f:begin fun ~context:_ ~client ->
    let uri = current_document_uri ?text_editor () in
    write_project_config_for ?uri client >>=
    Promise.Result.return
  end >>= Superbol_printer.show_error_message

let lsp_request ~meth ~data instance =
  with_context_and_client instance ~f:begin fun ~context:_ ~client ->
    let* result =
      Vscode_languageclient.LanguageClient.sendRequest client ()
        ~meth ~data
    in
    Promise.Result.return result
  end

let lsp_notification ~meth ~data instance =
  match client instance with
  | None -> ()
  | Some client ->
      Vscode_languageclient.LanguageClient.sendNotification client meth
        (Jsonoo.t_to_js data)

let get_project_config ?text_editor instance =
  (* The active editor may show something else than a file, like an output
     channel (e.g, with URI [output:tasks]), that belongs to no project. *)
  match active_text_editor ?text_editor () with
  | Some text_editor
    when Vscode.(Uri.scheme @@ TextDocument.uri @@
                 TextEditor.document text_editor) = "file" ->
      lsp_request instance
        ~meth:"superbol/getProjectConfiguration"
        ~data:(JSON.current_text_document_id text_editor) |>
      Promise.Result.map Jsonoo.Decode.(dict id)
  | _ ->
      Promise.return @@ Error No_active_text_editor

(** Toggles comments on the selected lines, unless the document changes
    while waiting for the server. *)
let toggle_comments ?text_editor instance =
  match active_text_editor ?text_editor () with
  | None ->
      Promise.return @@ Error No_active_text_editor
  | Some text_editor ->
      let document = Vscode.TextEditor.document text_editor in
      let version = Vscode.TextDocument.version document in
      let open Jsonoo.Encode in
      let jsposition pos =
        object_ [
          "line", int (Vscode.Position.line pos);
          "character", int (Vscode.Position.character pos);
        ]
      in
      let jsselection s =
        object_ [
          "start", jsposition (Vscode.Selection.start s);
          "end", jsposition (Vscode.Selection.end_ s);
        ]
      in
      let data =
        object_ [
          "uri", string @@
          Vscode.Uri.toString (Vscode.TextDocument.uri document) ();
          "selectionRanges", list jsselection @@
          Vscode.TextEditor.selections text_editor;
        ]
      in
      let decode_edits =
        let open Jsonoo.Decode in
        let position json =
          Vscode.Position.make
            ~line:(field "line" int json)
            ~character:(field "character" int json)
        in
        let range json =
          Vscode.Range.makePositions
            ~start:(field "start" position json)
            ~end_:(field "end" position json)
        in
        let edit json = field "range" range json, field "newText" string json in
        fun json -> Option.value ~default:[] (nullable (list edit) json)
      in
      lsp_request instance ~meth:"superbol/editionsToToggleComments" ~data |>
      Promise.Result.map decode_edits >>= function
      | Ok edits
        when edits <> [] &&
             Vscode.TextDocument.version document = version ->
          let+ _applied =
            Vscode.TextEditor.edit text_editor () ~callback:begin
              fun ~editBuilder ->
                List.iter begin fun (range, value) ->
                  Vscode.TextEditorEdit.replace editBuilder
                    ~location:(`Range range) ~value
                end edits
            end
          in
          Ok ()
      | Ok _ ->
          Promise.Result.return ()
      | Error _ as e ->
          Promise.return e

(* --- *)

let compilation_unit_names ~document instance =
  let data =
    Jsonoo.Encode.(object_ [
        "uri", string @@ Vscode.Uri.toString (Vscode.TextDocument.uri document) ();
      ])
  in
  lsp_request instance ~meth:"superbol/compilationUnitNames" ~data |>
  Promise.Result.map Jsonoo.Decode.(fun json ->
      Option.value ~default:[] (nullable (list string) json))

(** Tells whether [name] may be used as a file name as is. *)
let valid_file_basename name =
  name <> "" && name <> "." && name <> ".." &&
  String.for_all
    (fun c -> Char.code c >= 32 && not (String.contains "/\\:*?\"<>|" c))
    name

let preferred_extension =
  List.hd Superbol_shared.Cobol_files.source_extensions

let cobol_file_filters =
  Interop.Dict.of_alist [
    "COBOL", Superbol_shared.Cobol_files.source_extensions;
    "All Files", ["*"];
  ]

(** Directory proposed for saving untitled documents. *)
let default_save_directory () =
  match Vscode.Workspace.workspaceFolders () with
  | folder :: _ -> Vscode.WorkspaceFolder.uri folder
  | [] -> Vscode.Uri.file (Node.Os.homedir ())

(** Replaces the (untitled) [text_editor] with an editor for the file at
    [uri], that is assumed to hold the same text. *)
let replace_untitled_editor text_editor ~uri =
  let selection = Vscode.TextEditor.selection text_editor
  and visible = Vscode.TextEditor.visibleRanges text_editor
  and column = Vscode.TextEditor.viewColumn text_editor in
  (* Discards the untitled document without prompting; it is the active
     editor as the save dialog was modal. *)
  let* _ = Vscode.Commands.executeCommand
      ~command:"workbench.action.revertAndCloseActiveEditor" ~args:[] in
  let+ editor =
    Vscode.Window.showTextDocument () ~document:(`Uri uri) ?column
  in
  Vscode.TextEditor.set_selection editor selection;
  match visible with
  | range :: _ ->
      Vscode.TextEditor.revealRange editor ~range ~revealType:AtTop ()
  | [] -> ()

(** Saves the untitled COBOL document of [text_editor], proposing a file name
    that is based on the name of its first compilation unit.  Falls back to
    the regular save command whenever no name can be proposed (including when
    the language server is not running). *)
let save_untitled_document ?text_editor instance =
  let default_save () =
    let+ _ = Vscode.Commands.executeCommand
        ~command:"workbench.action.files.save" ~args:[] in
    ()
  in
  match active_text_editor ?text_editor () with
  | Some text_editor
    when Vscode.TextDocument.isUntitled (Vscode.TextEditor.document
                                           text_editor) ->
      let document = Vscode.TextEditor.document text_editor in
      compilation_unit_names ~document instance >>= begin function
        | Ok (name :: _) when valid_file_basename name ->
            let defaultUri =
              Vscode.Uri.joinPath (default_save_directory ())
                ~pathSegments:[name ^ "." ^ preferred_extension]
            in
            let options =
              Vscode.SaveDialogOptions.create ()
                ~defaultUri ~filters:cobol_file_filters
            in
            Vscode.Window.showSaveDialog ~options () >>= begin function
              | None ->
                  Promise.return ()
              | Some uri when Vscode.Uri.scheme uri <> "file" ->
                  (* No support for other file systems for now *)
                  Superbol_printer.log_error
                    "Unable to save %s: unsupported URI scheme; reverting \
                     to the regular save command"
                    (Vscode.Uri.toString uri ());
                  default_save ()
              | Some uri ->
                  match
                    Node.Fs.writeFileSync (Vscode.Uri.fsPath uri)
                      (Vscode.TextDocument.getText document ())
                  with
                  | () ->
                      replace_untitled_editor text_editor ~uri
                  | exception e ->
                      let message =
                        Printf.sprintf "Unable to save %s: %s"
                          (Vscode.Uri.fsPath uri) (Printexc.to_string e)
                      in
                      let _ = Vscode.Window.showErrorMessage () ~message in
                      Promise.return ()
            end
        | Ok _ | Error _ ->
            default_save ()
      end
  | _ ->
      default_save ()
