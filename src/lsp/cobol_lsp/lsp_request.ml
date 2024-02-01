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
open Cobol_common.Srcloc.INFIX
open Lsp_imports
open Lsp_project.TYPES
open Lsp_server.TYPES
open Lsp_lookup.TYPES
open Lsp.Types
open Ez_file.V1

(** {2 Handling requests} *)

(** Catch generic exception cases, and report errors using {!error} above *)
let try_doc ~f registry doc_id =
  let doc =
    try Lsp_server.find_document doc_id registry
    with Not_found ->
      Lsp_error.request_failed
        "Received a request about a document that has not been opened yet (uri = \
         %s) --- possible cause is the client did not manage to send the didOpen \
         notification; this may happen due to unhandled character encodings.\
        " (DocumentUri.to_string doc_id.TextDocumentIdentifier.uri)
  in
  try f ~doc
  with e ->
    Lsp_error.internal "Caught exception: %a" Fmt.exn e

(** Same as [try_doc], with some additional document data *)
let try_with_document_data ~f =
  try_doc ~f:(fun ~doc -> f ~doc @@ Lsp_document.checked doc)

(** {3 Initialization} *)

let handle_initialize (params: InitializeParams.t) =
  InitializeResult.create ()
    ~capabilities:(Lsp_capabilities.reply params.capabilities)

(** {3 Shutdown} *)

let handle_shutdown registry =
  Lsp_server.save_project_caches registry

(** {3 Definitions} *)

let focus_on_name_in_defintions = true

let find_data_definition Lsp_position.{ location_of; location_of_srcloc }
    ?(allow_notifications = true)
    (qn: Cobol_ptree.Types.qualname) (cu: Cobol_unit.Types.cobol_unit) =
  match Cobol_unit.Qualmap.find qn cu.unit_data.data_items.named with
  | Data_field { def = { loc; _ }; _ }
  | Data_renaming { def = { loc; _ }; _ }
  | Data_condition { def = { loc; _ }; _ }
  | Table_index { table = { loc; _ }; _ }
    when not focus_on_name_in_defintions ->
      [location_of_srcloc loc]
  | Data_field { def; _ } ->
      Option.(to_list @@ map location_of ~&def.field_qualname)
  | Data_renaming { def; _ } ->
      [location_of ~&def.renaming_name]
  | Data_condition { def; _ } ->
      [location_of ~&def.condition_name_qualname]
  | Table_index { qualname; _ } ->
      [location_of qualname]
  | exception Not_found
  | exception Cobol_unit.Qualmap.Ambiguous _
    when not allow_notifications ->
      []
  | exception Not_found ->
      (* Note: we keep that for ourselves for now as not all of the DATA DIV. is
         analyzed. *)
      (* Lsp_notify.unknown "data-name" qn; *)
      []
  | exception Cobol_unit.Qualmap.Ambiguous (lazy matching_qualnames) ->
      Lsp_notify.ambiguous "data-name" qn ~matching_qualnames;
      []

let find_proc_definition
    Lsp_position.{ location_of; _ }
    ?(allow_notifications = true)
    ?(in_section: Cobol_unit.Types.procedure_section option)
    (qn: Cobol_ptree.Types.qualname) (cu: Cobol_unit.Types.cobol_unit) =
  match Cobol_unit.Procedure.find ?in_section qn cu.unit_procedure with
  | Paragraph { payload = { paragraph_name = Some qn; _ }; _ }
    when focus_on_name_in_defintions ->
      [location_of qn]
  | Section p
    when focus_on_name_in_defintions ->
      [location_of ~&p.section_name]
  | Paragraph p ->
      [location_of p]
  | Section p ->
      [location_of p]
  | exception Not_found
  | exception Cobol_unit.Qualmap.Ambiguous _
    when not allow_notifications ->
      []
  | exception Not_found ->
      Lsp_notify.unknown "procedure-name" qn;
      []
  | exception Cobol_unit.Qualmap.Ambiguous (lazy matching_qualnames) ->
      Lsp_notify.ambiguous "procedure-name" qn ~matching_qualnames;
      []

let find_definitions ?allow_notifications loc_translator
    cu_name element_at_pos group =
  try
    let { payload = cu; _ } = CUs.find_by_name cu_name group in
    match element_at_pos with
    | Data_item { full_qn = Some qn; _ } | Data_full_name qn | Data_name qn ->
        find_data_definition loc_translator ?allow_notifications qn cu
    | Data_item { full_qn = None; def_loc } ->
        [loc_translator.location_of_srcloc def_loc]
    | Proc_name { qn; in_section } ->
        find_proc_definition loc_translator ?allow_notifications ?in_section
          qn cu
  with Not_found -> []

let lookup_definition_in_doc
    DefinitionParams.{ textDocument = doc; position; _ }
    Cobol_typeck.Outputs.{ group; _ }
  =
  match Lsp_lookup.element_at_position ~uri:doc.uri position group with
  | { element_at_position = None; _ }
  | { enclosing_compilation_unit_name = None; _ } ->
      None
  | { element_at_position = Some qn;
      enclosing_compilation_unit_name = Some cu_name } ->
      let loc_translator = Lsp_position.loc_translator doc in
      Some (`Location (find_definitions loc_translator cu_name qn group))

let handle_definition registry (params: DefinitionParams.t) =
  try_with_document_data registry params.textDocument
    ~f:(fun ~doc:_ -> lookup_definition_in_doc params)

(** {3 References} *)

let lookup_qn ~kind ~lookup qn =
  try Some (lookup qn) with
  | Not_found ->
      Lsp_notify.unknown kind qn;
      None
  | Cobol_unit.Qualmap.Ambiguous (lazy matching_qualnames) ->
      Lsp_notify.ambiguous kind qn ~matching_qualnames;
      None

let find_full_qn ~kind qn qmap =
  lookup_qn ~kind qn
    ~lookup:(fun qn -> (Cobol_unit.Qualmap.find_binding qn qmap).full_qn)

let find_proc_qn ~kind qn ?in_section cu =
  lookup_qn ~kind qn
    ~lookup:begin fun qn ->
      Cobol_unit.Procedure.full_qn ?in_section qn
        cu.Cobol_unit.Types.unit_procedure
    end

let lookup_references_in_doc
    ReferenceParams.{ textDocument = doc; position; context; _ }
    Cobol_typeck.Outputs.{ group; artifacts = { references }; _ }
  =
  match Lsp_lookup.element_at_position ~uri:doc.uri position group with
  | { element_at_position = None; _ } ->
    Lsp_debug.message "Lsp_request.lookup_references_in_doc: element_at_position = None";
    None
  | { enclosing_compilation_unit_name = None; _ } ->
    Lsp_debug.message "Lsp_request.lookup_references_in_doc: enclosing_compilation_unit_name = None";
      None
  | { element_at_position = Some qn;
      enclosing_compilation_unit_name = Some cu_name } ->
      let Lsp_position.{ location_of_srcloc; _ } as loc_translator
        = Lsp_position.loc_translator doc in
      let def_locs =
        if context.includeDeclaration then
          find_definitions ~allow_notifications:false loc_translator
            cu_name qn group
        else []
      in
      let ref_locs =
        try
          let cu, cu_refs = CUMap.find_by_name cu_name references in
          let data_refs qn =
            List.rev_map location_of_srcloc
              (Cobol_unit.Qual.MAP.find qn cu_refs.data_refs)
          and proc_refs qn =
            List.rev_map location_of_srcloc
              (Cobol_unit.Qual.MAP.find qn cu_refs.proc_refs)
          in
          match qn with
          | Data_full_name qn
          | Data_item { full_qn = Some qn; _ } ->
            Lsp_debug.message "Lsp_request.lookup_references_in_doc: Data_full_name...";
              data_refs qn
          | Data_item { full_qn = None; _ } ->
            Lsp_debug.message "Lsp_request.lookup_references_in_doc: Data_item...";
              []
          | Data_name qn ->
            Lsp_debug.message "Lsp_request.lookup_references_in_doc: Data_name...";
              Option.fold ~none:[] ~some:data_refs @@
              find_full_qn qn ~&cu.unit_data.data_items.named ~kind:"data-name"
          | Proc_name { qn; in_section } ->
            Lsp_debug.message "Lsp_request.lookup_references_in_doc: Proc_name...";
              Option.fold ~none:[] ~some:proc_refs @@
              find_proc_qn qn ?in_section ~&cu ~kind:"procedure-name"
        with Not_found -> []
      in
      Some (def_locs @ ref_locs)

let handle_references state (params: ReferenceParams.t) =
  try_with_document_data state params.textDocument
    ~f:(fun ~doc:_ -> lookup_references_in_doc params)

(** {3 Formatting} *)

let lsp_text_edit Cobol_indent.Types.{ lnum; offset_orig; offset_modif } =
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
    Cobol_indent.Types.{
      start_line = start.line + 1;
      end_line = end_.line + 1
    }
  in
  let edit_list =
    Cobol_indent.Indent_main.indent_range
      ~dialect:(Cobol_config.Config.dialect project.config.cobol_config)
      ~source_format:project.config.source_format
      ~indent_config:(Some (Cobol_indent.Indent_main.config project.config.indent_config))
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
      Cobol_indent.Indent_main.indent_range
        ~dialect:(Cobol_config.Config.dialect project.config.cobol_config)
        ~source_format:project.config.source_format
        ~indent_config:(Some (Cobol_indent.Indent_main.config project.config.indent_config))
        ~filename:(Lsp.Uri.to_path doc.uri)
        ~contents:(Lsp.Text_document.text textdoc)
        ~range:None
    in
    Some (List.map lsp_text_edit editList)
  with Failure msg ->
    Lsp_error.internal "Formatting error: %s" msg

(** {3 Semantic tokens} *)

let handle_semtoks_full,
    handle_semtoks_range =
  let handle registry ?range (doc: TextDocumentIdentifier.t) =
    try_with_document_data registry doc
      ~f:begin fun ~doc:{ artifacts = { pplog; tokens;
                                        rev_comments; rev_ignored; _ };
                          _ } Cobol_typeck.Outputs.{ ptree; _ } ->
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

(** {3 Hover} *)

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

(** {3 Completion} *)

let handle_completion registry (params: CompletionParams.t) =
  try_with_document_data registry params.textDocument
    ~f:begin fun ~doc:{ textdoc; _ } { ptree; _ } ->
      let items =
        Lsp_completion.completion_items textdoc params.position ptree in
      Some (`CompletionList (CompletionList.create ()
                               ~isIncomplete:false ~items))
    end

(** {3 Folding} *)

(*TODO(if necessary):
    Now, the request folding has the default perfomance (in VS Code)
    It only supports folding complete lines, and does
    not support FoldingRangeKind or CollapsedText
    (To support these features, need to change the client capability) *)
let handle_folding_range registry (params: FoldingRangeParams.t) =
  try_with_document_data registry params.textDocument
    ~f:begin fun ~doc:_ { ptree; group; _ } ->
      let filename = Lsp.Uri.to_path params.textDocument.uri in
      Some (Lsp_folding.ranges_in ~filename ptree group)
    end

(** {3 Generic handling} *)

let shutdown: state -> unit = function
  | NotInitialized _
  | ShuttingDown
  | Initialized _
  | Exit _ ->
      ()                                                             (* no-op *)
  | Running registry ->
      handle_shutdown registry

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
      Lsp_debug.message "Lsp_request: unhandled request";
      Error (UnhandledRequest client_req)
    | UnknownRequest { meth; _ } ->
      Lsp_debug.message "Lsp_request. unknown request";
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

(** {2 Access to internal stuff} *)

module INTERNAL = struct
  let lookup_definition = handle_definition
  let lookup_definition_in_doc = lookup_definition_in_doc
  let lookup_references = handle_references
  let lookup_references_in_doc = lookup_references_in_doc
  let hover = handle_hover
  let formatting = handle_formatting
end
