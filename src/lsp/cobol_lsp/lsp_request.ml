(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_common.Srcloc.TYPES
open Lsp_imports
open Lsp_project.TYPES
open Lsp_server.TYPES
open Lsp_lookup.TYPES
open Lsp.Types
open Ez_file.V1


(** {2 Some preliminary utilities for manipulating source locations} *)

type loc_translator =
  {
    location_of_srcloc: srcloc -> Location.t;
    location_of: 'x. 'x with_loc -> Location.t;
  }

let loc_translator TextDocumentIdentifier.{ uri } =
  let filename = Lsp.Uri.to_path uri in
  let location_of_srcloc loc =
    let range = Lsp_position.range_of_srcloc_in ~filename loc in
    Location.create ~uri ~range
  in
  {
    location_of_srcloc;
    location_of = fun { loc; _ } -> location_of_srcloc loc;
  }

(** {2 Error handling} *)

(** Raises {!Jsonrpc.Response.Error.E} *)
let error ~code fmt =
  Pretty.string_to begin fun message ->
    Jsonrpc.Response.Error.(raise @@ make ~code ~message ())
  end fmt

let request_failed fmt =
  error ~code:Jsonrpc.Response.Error.Code.RequestFailed fmt
let internal_error fmt =
  error ~code:Jsonrpc.Response.Error.Code.InternalError fmt

(** Catch generic exception cases, and report errors using {!error} above *)
let try_doc ~f registry doc_id =
  let doc =
    try Lsp_server.find_document doc_id registry
    with Not_found ->
      request_failed
        "Received a request about a document that has not been opened yet (uri = \
         %s) --- possible cause is the client did not manage to send the didOpen \
         notification; this may happen due to unhandled character encodings.\
        " (DocumentUri.to_string doc_id.TextDocumentIdentifier.uri)
  in
  try f ~doc
  with e ->
    internal_error "Caught exception: %a" Fmt.exn e

(** Same as [try_doc], with some additional document data *)
let try_with_document_data ~f =
  try_doc ~f:(fun ~doc -> f ~doc @@ Lsp_document.retrieve_parsed_data doc)

(** {2 Handling requests} *)

let handle_initialize (params: InitializeParams.t) =
  InitializeResult.create ()
    ~capabilities:(Lsp_capabilities.reply params.capabilities)

let find_definitions { location_of; _ } cu_name qn defs =
  let location_of_item { item_definition; _ } =
    match item_definition.payload with
    | CondName {condition_name = name; _}
    | Renames {rename_to = name; _}
    | Constant {constant_name = name; _} ->
        location_of name
    | Data {data_name = Some name; _}
    | Screen {screen_data_name = Some name; _}
    | ReportGroup {report_data_name = Some name; _} ->
        location_of name
    | _ ->
        raise Not_found
  in
  try
    let _cu, cu_defs = CUMap.find_by_name cu_name defs in
    let { as_item; as_paragraph } = Cobol_data.Qualmap.find qn cu_defs in
    Option.((to_list @@ map location_of_item as_item) @
            (to_list @@ map location_of as_paragraph))
  with Not_found -> []

let lookup_definition_in_doc
    DefinitionParams.{ textDocument = doc; position; _ }
    Lsp_document.{ ptree; definitions = defs; _ }
  =
  match Lsp_lookup.names_at_position ~uri:doc.uri position ptree with
  | { qualname_at_position = None; _ }
  | { enclosing_compilation_unit_name = None; _ } ->
      None
  | { qualname_at_position = Some qn;
      enclosing_compilation_unit_name = Some cu_name } ->
      let defs = Lazy.force defs in
      let loc_translator = loc_translator doc in
      Some (`Location (find_definitions loc_translator cu_name qn defs))

let handle_definition registry (params: DefinitionParams.t) =
  try_with_document_data registry params.textDocument
    ~f:(fun ~doc:_ -> lookup_definition_in_doc params)

let lookup_references_in_doc
    ReferenceParams.{ textDocument = doc; position; context; _ }
    Lsp_document.{ ptree; definitions = defs; references = refs; _ }
  =
  match Lsp_lookup.names_at_position ~uri:doc.uri position ptree with
  | { enclosing_compilation_unit_name = None; _ } ->
      None
  | { qualname_at_position = qn;
      enclosing_compilation_unit_name = Some cu_name } ->
      let { location_of_srcloc; _ } as loc_translator = loc_translator doc in
      let def_locs =
        match qn with
        | Some qn when context.includeDeclaration ->
            find_definitions loc_translator cu_name qn (Lazy.force defs)
        | Some _ | None ->
            []
      in
      let ref_locs =
        try
          match qn with
          | None -> []
          | Some qn ->
              let refs = Lazy.force refs in
              let _cu, cu_refs = CUMap.find_by_name cu_name refs in
              List.map location_of_srcloc
                (Cobol_data.Qualmap.find qn cu_refs)
        with Not_found -> []
      in
      Some (def_locs @ ref_locs)

let handle_references state (params: ReferenceParams.t) =
  try_with_document_data state params.textDocument
    ~f:(fun ~doc:_ -> lookup_references_in_doc params)

let lsp_text_edit (ir : Cobol_indent.Type.indent_record) =
  match ir with { lnum; offset_orig; offset_modif } ->
  let delta = offset_modif - offset_orig in
  let position = Position.create ~line:(lnum - 1) ~character:offset_orig in
  let range = Range.create ~start:position ~end_:position in
  if delta > 0 then
    TextEdit.create ~newText:(String.make delta ' ') ~range
  else
    let start =
      Position.create ~line:(lnum - 1) ~character:(offset_orig + delta)
    in
    let range = Range.create ~start ~end_:position in
    TextEdit.create ~newText:"" ~range

(*Remark:
    The first line of the text selected to RangeFormatting must be
    the begin of statement/paragraph/section/division/01 level data declaration,
    and the text selected must terminate in the same scope.
    Otherwise, unexpected result.
*)
let handle_range_formatting registry params =
  let open DocumentRangeFormattingParams in
  let { textDocument = doc; range = {start; end_}; _ } = params in
  let Lsp_document.{ project; textdoc; _ } =
    Lsp_server.find_document doc registry
  in
  let range_to_indent =
    Cobol_indent.Type.{
      start_line = start.line + 1;
      end_line = end_.line + 1
    }
  in
  let edit_list =
    Cobol_indent.indent_range
      ~dialect:(Cobol_config.dialect project.config.cobol_config)
      ~source_format:project.config.source_format
      ~indent_config:(Some (Cobol_indent.config project.config.indent_config))
      ~filename:(Lsp.Uri.to_path doc.uri)
      ~contents:(Lsp.Text_document.text textdoc)
      ~range:(Some range_to_indent)
  in
  Some (List.map lsp_text_edit edit_list)

let handle_formatting registry params =
  let DocumentFormattingParams.{ textDocument = doc; _ } = params in
  let Lsp_document.{ project; textdoc; _ } =
    Lsp_server.find_document doc registry in
  try
    let editList =
      Cobol_indent.indent_range
        ~dialect:(Cobol_config.dialect project.config.cobol_config)
        ~source_format:project.config.source_format
        ~indent_config:(Some (Cobol_indent.config project.config.indent_config))
        ~filename:(Lsp.Uri.to_path doc.uri)
        ~contents:(Lsp.Text_document.text textdoc)
        ~range:None
    in
    Some (List.map lsp_text_edit editList)
  with Failure msg ->
    internal_error "Formatting error: %s" msg

let handle_semtoks_full,
    handle_semtoks_range =
  let handle registry ?range (doc: TextDocumentIdentifier.t) =
    try_with_document_data registry doc
      ~f:begin fun ~doc:{ artifacts = { pplog; tokens;
                                        rev_comments; rev_ignored; _ };
                          _ } Lsp_document.{ ptree; _ } ->
        let data =
          Lsp_semtoks.data ~filename:(Lsp.Uri.to_path doc.uri) ~range
            ~pplog ~rev_comments ~rev_ignored
            ~tokens:(Lazy.force tokens) ~ptree
        in
        Some (SemanticTokens.create ~data ())
      end
  in
  (fun registry (SemanticTokensParams.{ textDocument; _ }) ->
     handle registry textDocument),
  (fun registry (SemanticTokensRangeParams.{ textDocument; range; _ }) ->
     handle registry ~range textDocument)

let handle_hover registry (params: HoverParams.t) =
  let filename = Lsp.Uri.to_path params.textDocument.uri in
  let find_hovered_pplog_event pplog =
    List.find_opt begin function
      | Cobol_preproc.Trace.Replace _
      | CompilerDirective _ ->
          false
      | Replacement { matched_loc = loc; _ }
      | FileCopy { copyloc = loc; _ } ->
          Lsp_position.is_in_lexloc params.position
            (Cobol_common.Srcloc.lexloc_in ~filename loc)
    end (Cobol_preproc.Trace.events pplog)
  in
  let hover_markdown ~loc value =
    let content = MarkupContent.create ~kind:MarkupKind.Markdown ~value in
    let range = Lsp_position.range_of_srcloc_in ~filename loc in
    Some (Hover.create () ~contents:(`MarkupContent content) ~range)
  in
  try_doc registry params.textDocument
    ~f:begin fun ~doc:{ project; artifacts = { pplog; _ }; _ } ->
      match find_hovered_pplog_event pplog with
      | Some Replacement { matched_loc = loc;
                           replacement_text; _ } ->
          Pretty.string_to (hover_markdown ~loc) "``@[<h>%a@]``"
            Cobol_preproc.Text.pp_text replacement_text
      | Some FileCopy { copyloc = loc;
                        status = CopyDone lib | CyclicCopy lib } ->
          let text = EzFile.read_file lib in
          (* TODO: grab source-format from preprocessor state? *)
          let module Config = (val project.config.cobol_config) in
          let mdlang = match Config.format#value with
            | SF (SFFree | SFVariable | SFCOBOLX) -> "cobolfree"
            | SF _ | Auto -> "cobol"
          in
          Pretty.string_to (hover_markdown ~loc) "```%s\n%s\n```" mdlang text
      | Some FileCopy { status = MissingCopy _; _ }
      | Some Replace _
      | Some CompilerDirective _
      | None ->
          None
    end

let handle_completion registry (params: CompletionParams.t) =
  try_with_document_data registry params.textDocument
    ~f:begin fun ~doc:{ textdoc; _ } { ptree; _ } ->
      let items =
        Lsp_completion.completion_items textdoc params.position ptree in
      Some (`CompletionList (CompletionList.create ()
                               ~isIncomplete:false ~items))
    end

(*TODO(if necessary):
    Now, the request folding has the default perfomance (in VS Code)
    It only supports folding complete lines, and does
    not support FoldingRangeKind or CollapsedText
    (To support these features, need to change the client capability) *)
let handle_folding_range registry (params: FoldingRangeParams.t) =
  try_with_document_data registry params.textDocument
    ~f:begin fun ~doc:_ { ptree; cus; _ } ->
      let filename = Lsp.Uri.to_path params.textDocument.uri in
      Some (Lsp_folding.ranges_in ~filename ptree cus)
    end

let handle_shutdown registry =
  try Lsp_server.save_project_caches registry
  with e ->
    internal_error
      "Exception caught while saving project caches: %a@." Fmt.exn e

let on_request
  : type r. state -> r Lsp.Client_request.t ->
    id:Jsonrpc.Id.t -> (r * state, r error) result =
  fun state client_req ~id:_ ->
  match state, client_req with
  | NotInitialized config, Lsp.Client_request.Initialize init_params ->
      Ok (handle_initialize init_params, Initialized config)
  | NotInitialized _, _ ->
      Error (InvalidStatus state)
  | (ShuttingDown | Initialized _ | Exit _) as state, _ ->
      Error (InvalidStatus state)
  | Running registry, _ -> match client_req with
    | Initialize _ ->
        Error (InvalidStatus (Running registry))
    | TextDocumentDefinition def_params ->
        Ok (handle_definition registry def_params, state)
    | TextDocumentReferences ref_params ->
        Ok (handle_references registry ref_params, state)
    | TextDocumentRangeFormatting params ->
        Ok (handle_range_formatting registry params, state)
    | TextDocumentFormatting params ->
        Ok (handle_formatting registry params, state)
    | SemanticTokensFull params ->
        Ok (handle_semtoks_full registry params, state)
    | SemanticTokensRange params ->
        Ok (handle_semtoks_range registry params, state)
    | TextDocumentHover params ->
        Ok (handle_hover registry params, state)
    | TextDocumentCompletion params ->
        Ok (handle_completion registry params, state)
    | TextDocumentFoldingRange params ->
        Ok (handle_folding_range registry params, state)
    | Shutdown ->
        Ok (handle_shutdown registry, ShuttingDown)
    | TextDocumentDeclaration  (* TextDocumentPositionParams.t.t *) _
    | TextDocumentTypeDefinition  (* TypeDefinitionParams.t.t *) _
    | TextDocumentImplementation  (* ImplementationParams.t.t *) _
    | TextDocumentCodeLens  (* CodeLensParams.t.t *) _
    | TextDocumentCodeLensResolve  (* CodeLens.t.t *) _
    | TextDocumentPrepareCallHierarchy  (* CallHierarchyPrepareParams.t.t *) _
    | TextDocumentPrepareRename  (* PrepareRenameParams.t.t *) _
    | TextDocumentRename  (* RenameParams.t.t *) _
    | TextDocumentLink  (* DocumentLinkParams.t.t *) _
    | TextDocumentLinkResolve  (* DocumentLink.t.t *) _
    | TextDocumentMoniker  (* MonikerParams.t.t *) _
    | DocumentSymbol  (* DocumentSymbolParams.t.t *) _
    | WorkspaceSymbol  (* WorkspaceSymbolParams.t.t *) _
    | DebugEcho (* DebugEcho.Params.t *) _
    | DebugTextDocumentGet  (* DebugTextDocumentGet.Params.t *) _
    | TextDocumentHighlight  (* DocumentHighlightParams.t.t *) _
    | SignatureHelp  (* SignatureHelpParams.t.t *) _
    | CodeAction  (* CodeActionParams.t.t *) _
    | CodeActionResolve  (* CodeAction.t.t *) _
    | CompletionItemResolve  (* CompletionItem.t.t *) _
    | WillSaveWaitUntilTextDocument  (* WillSaveTextDocumentParams.t.t *) _
    | TextDocumentOnTypeFormatting  (* DocumentOnTypeFormattingParams.t.t *) _
    | TextDocumentColorPresentation  (* ColorPresentationParams.t.t *) _
    | TextDocumentColor  (* DocumentColorParams.t.t *) _
    | SelectionRange  (* SelectionRangeParams.t.t *) _
    | ExecuteCommand  (* ExecuteCommandParams.t.t *) _
    | SemanticTokensDelta  (* SemanticTokensDeltaParams.t.t *) _
    | LinkedEditingRange  (* LinkedEditingRangeParams.t.t *) _
    | CallHierarchyIncomingCalls  (* CallHierarchyIncomingCallsParams.t.t *) _
    | CallHierarchyOutgoingCalls  (* CallHierarchyOutgoingCallsParams.t.t *) _
    | WillCreateFiles  (* CreateFilesParams.t.t *) _
    | WillDeleteFiles  (* DeleteFilesParams.t.t *) _
    | WillRenameFiles  (* RenameFilesParams.t.t *) _
      ->
        Error (UnhandledRequest client_req)
    | UnknownRequest { meth; _ } ->
        Error (UnknownRequest meth)

let handle (Jsonrpc.Request.{ id; _ } as req) state =
  match Lsp.Client_request.of_jsonrpc req with
  | Error message ->
      let code = Jsonrpc.Response.Error.Code.InvalidRequest in
      let err = Jsonrpc.Response.Error.make ~message ~code () in
      state, Jsonrpc.Response.(error id err)
  | Ok (E r) ->
      match on_request state r ~id with
      | Ok (reply, state) ->
          let reply_json = Lsp.Client_request.yojson_of_result r reply in
          state, Jsonrpc.Response.ok id reply_json
      | Error server_error ->
          state,
          Jsonrpc.Response.error id @@
          Lsp_server.jsonrpc_of_error server_error req.method_
      | exception Jsonrpc.Response.Error.E e ->
          state, Jsonrpc.Response.error id e
      | exception e ->
          state, Jsonrpc.Response.(error id @@ Error.of_exn e)

module INTERNAL = struct
  let lookup_definition = handle_definition
  let lookup_definition_in_doc = lookup_definition_in_doc
  let lookup_references = handle_references
  let lookup_references_in_doc = lookup_references_in_doc
  let hover = handle_hover
  let formatting = handle_formatting
end
