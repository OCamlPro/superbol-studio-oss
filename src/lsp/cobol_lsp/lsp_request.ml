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


(** Some preliminary utilities for manipulating source locations *)

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


(** Catching cases where we miss some document data *)

let try_with_document_data ?(on_error = None) ~f registry document_id =
  try
    let Lsp_document.{ project; textdoc; pplog; tokens; _ } as doc =
      Lsp_server.find_document document_id registry in
    f ~project ~textdoc ~pplog ~tokens @@ Lsp_document.retrieve_parsed_data doc
  with e ->
    Lsp_io.pretty_notification ~type_:Warning "Caught exception: %a" Fmt.exn e;
    on_error

(** Handling requests *)

(* Client capabilities are to be used for special request response, for example
   a definition request can be answered with a LocationLink iff the client
   supports it.

   NOTE: For now we don't use them because we don't have any special
   response. *)
let make_capabilities _ =
  let sync =
    TextDocumentSyncOptions.create ()
      ~openClose:true
      ~change:Incremental
  in
  let semantic =
    let full = SemanticTokensOptions.create_full ~delta:false () in
    let legend = SemanticTokensLegend.create
        ~tokenTypes:Lsp_semantic.tokens_types
        ~tokenModifiers:Lsp_semantic.tokens_modifiers
    in
    SemanticTokensOptions.create ()
      ~full:(`Full full) ~legend
  in
  let hover = HoverOptions.create () in
  let completion_option = CompletionOptions.create () in

  ServerCapabilities.create ()
    ~textDocumentSync:(`TextDocumentSyncOptions sync)
    ~definitionProvider:(`Bool true)
    ~referencesProvider:(`Bool true)
    ~documentRangeFormattingProvider: (`Bool true)
    ~documentFormattingProvider: (`Bool true)
    ~semanticTokensProvider:(`SemanticTokensOptions semantic)
    ~hoverProvider:(`HoverOptions hover)
    ~completionProvider:(completion_option)

let handle_initialize (params: InitializeParams.t) =
  InitializeResult.create ()
    ~capabilities:(make_capabilities params.capabilities)

let find_definitions { location_of; _ } cu_name qn defs =
  let location_of_item { item_definition; _ } =
    match item_definition.payload with
    | CondName {condition_name = name; _}
    | Renames {rename_to = name; _}->
        location_of name
    | Constant {constant_name = Some name; _}
    | Data {data_name = Some name; _}
    | Screen {screen_data_name = Some name; _}
    | ReportGroup {report_data_name = Some name; _} ->
        location_of name
    | _ -> raise Not_found
  in
  try
    let _cu, cu_defs = CUMap.find_by_name cu_name defs in
    let { as_item; as_paragraph } = Cobol_data.Qualmap.find qn cu_defs in
    Option.((to_list @@ map location_of_item as_item) @
            (to_list @@ map location_of as_paragraph))
  with Not_found -> []

let lookup_definition_in_doc
    DefinitionParams.{ textDocument = doc; position; _ }
    Lsp_document.{ ast = ptree; definitions = defs; _ }
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
    ~f:(fun ~project:_ ~textdoc:_ ~pplog:_ ~tokens:_ ->
        lookup_definition_in_doc params)

let lookup_references_in_doc
    ReferenceParams.{ textDocument = doc; position; context; _ }
    Lsp_document.{ ast = ptree; definitions = defs; references = refs; _ }
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
    ~f:(fun ~project:_ ~textdoc:_ ~pplog:_ ~tokens:_ ->
        lookup_references_in_doc params)


(*Remark:
    The first line of the text selected to RangeFormatting must be
    the begin of statement/paragraph/section/division/01 level data declaration,
    and the text selected must terminate in the same scope.
    Otherwise, unexpected result.
*)
let handle_range_formatting registry params =
  let open DocumentRangeFormattingParams in
  let { textDocument = doc; range = {start; end_}; _ } = params in
  let Lsp_document.{ project; _ } = Lsp_server.find_document doc registry in
  let range_to_indent =
    Cobol_indent.Type.{
      start_line = start.line + 1;
      end_line = end_.line + 1
    }
  in
  (*the range must contain the whole lines*)
  let range =
    (*TODO:find a simple method to get the number of letters of one line
           (the code below use 1000 as the upperbound)                   *)
    Range.{
      start = Position.create ~line:start.line ~character:0;
      end_ = Position.create ~line:end_.line ~character:1000;
    }
  in
  let newText =
    Cobol_indent.indent_range'
      ~source_format:project.source_format
      ~indent_config:None
      ~file:(Lsp.Uri.to_path doc.uri)
      ~range:(Some range_to_indent)
  in
  Some [TextEdit.create ~newText ~range]

let handle_formatting registry params =
  let DocumentFormattingParams.{ textDocument = doc; _ } = params in
  let Lsp_document.{ project; textdoc; _ } =
    Lsp_server.find_document doc registry in
  let lines = String.split_on_char '\n' (Lsp.Text_document.text textdoc) in
  let length = List.length lines - 1 in
  (* TODO: formatting on empty files will break here *)
  let width = String.length @@ List.hd @@ List.rev lines in
  let edit_range =
    Range.create
      ~start:(Position.create ~character:0 ~line:0)
      ~end_:(Position.create ~character:width ~line:length)
  in
  let path = Lsp.Uri.to_path doc.uri in
  let newText =
    Cobol_indent.indent_range'
      ~source_format:project.source_format
      ~indent_config:None
      ~file:path
      ~range:None
  in
  Some [TextEdit.create ~newText ~range:edit_range]

let handle_semantic_tokens_full registry (params: SemanticTokensParams.t) =
  try_with_document_data registry params.textDocument
    ~f:begin fun ~project:_ ~textdoc:_ ~pplog:_ ~tokens Lsp_document.{ ast; _ } ->
      let filename = Lsp.Uri.to_path params.textDocument.uri in
      let data = Lsp_semantic.data ~filename (Lazy.force tokens) ast in
      Some (SemanticTokens.create ~data ())
    end

let handle_hover registry (params: HoverParams.t) =
  let filename = Lsp.Uri.to_path params.textDocument.uri in
  let find_hovered_replacement pplog =
    List.find_opt begin fun Cobol_preproc.{ matched_loc; _ } ->
      Lsp_position.is_in_lexloc params.position
        (Cobol_common.Srcloc.lexloc_in ~filename matched_loc)
    end pplog
  in
  let hover_markdown ~loc value =
    let content = MarkupContent.create ~kind:MarkupKind.Markdown ~value in
    let range = Lsp_position.range_of_srcloc_in ~filename loc in
    Some (Hover.create () ~contents:(`MarkupContent content) ~range)
  in
  try_with_document_data registry params.textDocument
    ~f:begin fun ~project ~textdoc:_ ~pplog ~tokens:_ { ast; _ } ->
      match Lsp_lookup.copy_at_pos ~filename params.position ast with
      | Some { payload = lib; loc } ->
          let text = EzFile.read_file lib in
          (* TODO: grab source-format from preprocessor state? *)
          let module Config = (val project.cobol_config) in
          let mdlang = match Config.format#value with
            | SF (SFFree | SFVariable | SFCOBOLX) -> "cobolfree"
            | SF _ | Auto -> "cobol"
          in
          Pretty.string_to (hover_markdown ~loc) "```%s\n%s\n```" mdlang text
      | None ->
          match find_hovered_replacement pplog with
          | None -> None
          | Some Cobol_preproc.{ matched_loc = loc; replacement_text; _ } ->
              Pretty.string_to (hover_markdown ~loc) "``@[<h>%a@]``"
                Cobol_preproc.Text.pp_text replacement_text
    end

let handle_completion registry (params:CompletionParams.t) =
  let open Lsp_completion in
  try_with_document_data registry params.textDocument
    ~f:begin fun ~project:_ ~textdoc ~pplog:_ ~tokens:_ { ast; _ } ->
      let items = completion_items textdoc params.position ast in
      let completionlist = CompletionList.create ~isIncomplete:false ~items () in
      Some (`CompletionList completionlist)
    end

let handle_shutdown registry =
  try Lsp_server.save_project_caches registry
  with e -> Pretty.error "Exception caught while saving project caches: %a@.\
                         " Fmt.exn e

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
        begin try Ok (handle_formatting registry params, state)
        with Failure msg -> Error (FormattingError msg) end
    | SemanticTokensFull semantic_params ->
        Ok (handle_semantic_tokens_full registry semantic_params, state)
    | TextDocumentHover hover_params ->
        Ok (handle_hover registry hover_params, state)
    | TextDocumentCompletion completion_params ->
        Ok (handle_completion registry completion_params, state)
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
    | TextDocumentFoldingRange  (* FoldingRangeParams.t.t *) _
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
    | SemanticTokensRange  (* SemanticTokensRangeParams.t.t *) _
    | LinkedEditingRange  (* LinkedEditingRangeParams.t.t *) _
    | CallHierarchyIncomingCalls  (* CallHierarchyIncomingCallsParams.t.t *) _
    | CallHierarchyOutgoingCalls  (* CallHierarchyOutgoingCallsParams.t.t *) _
    | WillCreateFiles  (* CreateFilesParams.t.t *) _
    | WillDeleteFiles  (* DeleteFilesParams.t.t *) _
    | WillRenameFiles  (* RenameFilesParams.t.t *) _
      ->
        Error (UnhandledRequest client_req)
    | UnknownRequest { meth ; params=_ } ->
        Error (UnknownRequest meth)

let handle req state =
  match Lsp.Client_request.of_jsonrpc req with
  | Ok (E r) ->
      let response, state =
        match on_request state r ~id:req.id with
        | Ok (reply, state) ->
            let reply_json = Lsp.Client_request.yojson_of_result r reply in
            Jsonrpc.Response.ok req.id reply_json, state
        | Error server_error ->
            let json_error =
              Lsp_server.jsonrpc_of_error server_error req.method_ in
            Jsonrpc.Response.error req.id json_error, state
        | exception e ->
            Jsonrpc.Response.error req.id @@ Jsonrpc.Response.Error.of_exn e, state
      in
      Lsp_io.send_response response;
      state
  | Error str ->
      Pretty.failwith "Could not read request: %s" str

module INTERNAL = struct
  let lookup_definition = handle_definition
  let lookup_definition_in_doc = lookup_definition_in_doc
  let lookup_references = handle_references
  let lookup_references_in_doc = lookup_references_in_doc
  let hover = handle_hover
  let formatting = handle_formatting
end
