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

(** Catch generic exception cases, and report errors using {!Lsp_error}.
    Returns [None] in case the document cannot be parsed (or is a copybook, for
    now).  [f] has to return an optional value. *)
let try_main_doc ~f registry doc_id =
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
  with Lsp_document.(Unparseable _ | Copybook _) -> None
     | e -> Lsp_error.internal "Caught exception: %a" Fmt.exn e

(** Same as {!try_main_doc}, with some additional document data. *)
let try_with_main_document_data ~f =
  try_main_doc ~f:(fun ~doc -> f ~doc @@ Lsp_document.checked doc)

(** {3 Initialization} *)

let initialize ~config (params: InitializeParams.t) =
  let root_uri = match params.rootUri with
    | None -> None
    | Some uri -> Some uri
  in
  let workspace_folders = match params.workspaceFolders with
    | Some Some (_ :: _ as l) -> List.map (fun x -> x.WorkspaceFolder.uri) l
    | _ -> Option.to_list root_uri
  in
  Lsp_io.log_info "Initializing@ for@ workspace@ folders:@ %a"
    Pretty.(list ~fopen:"@[" ~fclose:"@]" string)
    (List.map (fun x -> DocumentUri.to_path x) workspace_folders);
  let capabilities = Lsp_capabilities.reply params.capabilities in
  let with_semantic_tokens =
    capabilities.semanticTokensProvider <> None
  in
  let with_client_config_watcher = match params.capabilities.workspace with
    | Some { didChangeConfiguration = Some { dynamicRegistration }; _ } ->
        (* Note: for now we rely on the client's dynamic registration ability;
           for clients that do not support that it could just be simpler to
           trigger server restarts when relevant changes happen. *)
        Option.value ~default:false dynamicRegistration
    | _ ->
        false
  and with_client_file_watcher = match params.capabilities.workspace with
    | Some { didChangeWatchedFiles = Some { dynamicRegistration; _ }; _ } ->
        (* Note: for now we rely on the client's dynamic registration ability;
           for clients that do not support that it could just be simpler to
           trigger server restarts when relevant changes happen. *)
        Option.value ~default:false dynamicRegistration
    | _ ->
        false
  in
  Lsp_io.log_info "Negociated@ server@ parameters:@\n@[%t@]" @@
  Pretty.delayed_record [
    Fmt.(field "client_config_watcher" (fun _ -> with_client_config_watcher) bool);
    Fmt.(field "client_file_watcher" (fun _ -> with_client_file_watcher) bool);
  ];
  let result =
    InitializeResult.create ()
      ~serverInfo:(InitializeResult.create_serverInfo ()
                     ~name:"SuperBOL LSP Server"
                     ~version:Version.version)
      ~capabilities
  in
  Ok (result, Initialized { root_uri; workspace_folders; config;
                            with_semantic_tokens;
                            with_client_config_watcher;
                            with_client_file_watcher })


(** {3 Shutdown} *)

let handle_shutdown registry =
  Lsp_server.save_project_caches registry


(** {3 Custom commands for configuration management} *)


let assoc_of_jsonrpc_struct params =
  Yojson.Safe.Util.to_assoc @@ Jsonrpc.Structured.yojson_of_t params


let handle_write_project_config_command param registry =
  try
    let uri = match List.assoc_opt "uri" @@ assoc_of_jsonrpc_struct param with
      | Some uri -> Some (Lsp.Uri.t_of_yojson uri)
      | None -> None
    in
    let registry = Lsp_server.on_write_project_config_command ?uri registry in
    Ok (`Null, Running registry)
  with Yojson.Safe.Util.(Type_error _ | Undefined _) ->
    Lsp_error.invalid_params "param = %s (association list with \"uri\" key \
                              expected)" Yojson.Safe.(to_string (param :> t))


let handle_get_project_config_command param registry =
  try
    let assoc = assoc_of_jsonrpc_struct param in
    let uri = Lsp.Uri.t_of_yojson (List.assoc "uri" assoc) in
    let reply = Lsp_server.get_project_config_command uri registry in
    Lsp_io.log_debug "Reply: %a" (Yojson.Safe.pretty_print ~std:false) reply;
    Ok (reply, Running registry)
  with Yojson.Safe.Util.(Type_error _ | Undefined _) | Not_found ->
    Lsp_error.invalid_params "param = %s (association list with \"uri\" key \
                              expected)" Yojson.Safe.(to_string (param :> t))

let create_cfg_options o =
  let graph_name =
    try Some (List.assoc "graph_name" o |> Yojson.Safe.Util.to_string)
    with Not_found -> None in
  let hide_unreachable =
    try Some (List.assoc "hide_unreachable" o |> Yojson.Safe.Util.to_bool)
    with Not_found -> None in
  let collapse_fallthru =
    try Some (List.assoc "collapse_fallthru" o |> Yojson.Safe.Util.to_bool)
    with Not_found -> None in
  let shatter_hubs =
    try Some (List.assoc "shatter_hubs" o |> Yojson.Safe.Util.to_int)
    with Not_found -> None in
  Cobol_cfg.Options.create ~graph_name ?hide_unreachable ?collapse_fallthru ~shatter_hubs ()

let handle_open_cfg registry params =
  let params = Jsonrpc.Structured.yojson_of_t params in
  let uri, options = Yojson.Safe.Util.(
      to_string @@ member "uri" params,
      try to_assoc @@ member "render_options" params with Type_error _ -> [])
  in
  let textDoc = TextDocumentIdentifier.create ~uri:(DocumentUri.of_path uri) in
  try_with_main_document_data registry textDoc
    ~f:begin fun ~doc:_ checked_doc ->
      let open Cobol_cfg.Builder in
      let options = create_cfg_options options in
      let graphs = make ~options checked_doc in
      let yojsonify ({ string_repr_dot; string_repr_d3; name; nodes_pos } : graph) =
        let nodes_pos = List.map begin fun (n,loc) ->
            let range = Lsp_position.range_of_srcloc_in ~filename:uri loc in
            (string_of_int n, Range.yojson_of_t range)
          end nodes_pos in
        `Assoc [
          ("string_repr_d3", `String string_repr_d3);
          ("string_repr_dot", `String string_repr_dot);
          ("nodes_pos", `Assoc nodes_pos);
          ("name", `String name);]
      in
      Some (`Assoc ["graphviz_legend", `String graphviz_legend;
        "graphs", `List (List.map yojsonify graphs)])
    end
  |> Option.value ~default:(`Assoc [])

let handle_find_procedure registry params =
  let params = Jsonrpc.Structured.yojson_of_t params in
  let filename = Yojson.Safe.Util.to_string @@ Yojson.Safe.Util.member "uri" params in
  let line = Yojson.Safe.Util.to_int @@ Yojson.Safe.Util.member "line" params in
  let character = Yojson.Safe.Util.to_int @@ Yojson.Safe.Util.member "character" params in
  let textDoc = TextDocumentIdentifier.create ~uri:(DocumentUri.of_path filename) in
  try_with_main_document_data registry textDoc
    ~f:begin fun ~doc:_ checked_doc ->
      let pos = Position.create ~character ~line in
      let { cu; proc_name } =
        Lsp_lookup.proc_at_pos ~filename pos checked_doc.group in
      let proc = match proc_name, cu with
        | Some qn, _ -> Pretty.to_string "%a" Cobol_ptree.pp_qualname qn
                        |> Str.global_replace (Str.regexp "\n") " "
        | None, Some cu -> ~&(cu.unit_name)
        | _ -> "" in
      Some (`String proc)
    end
  |> Option.value ~default:(`String "")

(** {3 Definitions} *)


let focus_on_name_in_defintions = true

let find_data_definition Lsp_position.{ location_of; location_of_srcloc }
    ?(allow_notifications = true)
    (qn: Cobol_ptree.qualname) (cu: Cobol_unit.Types.cobol_unit) =
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
    (qn: Cobol_ptree.qualname) (cu: Cobol_unit.Types.cobol_unit) =
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

let lookup_definition_in_doc ~rootdir
    DefinitionParams.{ textDocument = doc; position; _ }
    Cobol_typeck.Outputs.{ group; _ }
  =
  match Lsp_lookup.element_at_position ~uri:doc.uri position group with
  | { element_at_position = None; _ }
  | { enclosing_compilation_unit_name = None; _ } ->
      None
  | { element_at_position = Some qn;
      enclosing_compilation_unit_name = Some cu_name } ->
      let loc_translator = Lsp_position.loc_translator ~rootdir doc in
      Some (`Location (find_definitions loc_translator cu_name qn group))

let handle_definition registry (params: DefinitionParams.t) =
  try_with_main_document_data registry params.textDocument
    ~f:begin fun ~doc:{ project; _ } ->
      let rootdir = Lsp_project.(string_of_rootdir @@ rootdir project) in
      lookup_definition_in_doc ~rootdir params
    end

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
    ~rootdir
    ReferenceParams.{ textDocument = doc; position; context; _ }
    Cobol_typeck.Outputs.{ group; artifacts = { references }; _ }
  =
  match Lsp_lookup.element_at_position ~uri:doc.uri position group with
  | { element_at_position = None; _ } ->
      Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                         element_at_position = None";
    None
  | { enclosing_compilation_unit_name = None; _ } ->
      Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                         enclosing_compilation_unit_name = None";
      None
  | { element_at_position = Some qn;
      enclosing_compilation_unit_name = Some cu_name } ->
      let Lsp_position.{ location_of_srcloc; _ } as loc_translator
        = Lsp_position.loc_translator ~rootdir doc in
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
              Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                                 Data_full_name...";
              data_refs qn
          | Data_item { full_qn = None; _ } ->
              Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                                 Data_item...";
              []
          | Data_name qn ->
              Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                                 Data_name...";
              Option.fold ~none:[] ~some:data_refs @@
              find_full_qn qn ~&cu.unit_data.data_items.named ~kind:"data-name"
          | Proc_name { qn; in_section } ->
              Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                                 Proc_name...";
              Option.fold ~none:[] ~some:proc_refs @@
              find_proc_qn qn ?in_section ~&cu ~kind:"procedure-name"
        with Not_found -> []
      in
      Some (def_locs @ ref_locs)

let handle_references state (params: ReferenceParams.t) =
  try_with_main_document_data state params.textDocument
    ~f:begin fun ~doc:{ project; _ } ->
      let rootdir = Lsp_project.(string_of_rootdir @@ rootdir project) in
      lookup_references_in_doc ~rootdir params
    end

(** {3 Formatting} *)

let to_textedits ( ops : Cobol_indent.Types.edit_space_operation list ) =

  let rec iter ops ~lnum ~delta rev =
    match ops with
    | [] -> List.rev rev

    (* two consecutive deletions *)
    | { Cobol_indent.Types.line = line1 ; char = char1 ; spaces = spaces1 } as op
      :: { line = line2 ; char = char2 ; spaces = spaces2 }
      :: ops
      when line1 = line2 && spaces1 < 0 && spaces2 < 0 &&
           char2 = char1 - spaces1 ->
      let ops = { op with spaces = spaces1 + spaces2 } :: ops in
      iter ops ~lnum ~delta rev

    (* two consecutive insertions *)
    | { line = line1 ; char = char1 ; spaces = spaces1 } as op
      :: { line = line2 ; char = char2 ; spaces = spaces2 }
      :: ops
      when line1 = line2 && spaces1 > 0 && spaces2 > 0 &&
           char2 = char1 + spaces1 ->
      let ops = { op with spaces = spaces1 + spaces2 } :: ops in
      iter ops ~lnum ~delta rev

    | { line ; char ; spaces } :: ops ->

      let line = line - 1 in
      let delta = if line = lnum then delta else 0 in
      let char = char + delta in
      let start = Position.create ~line ~character:char in
      if spaces > 0 then
        (* add spaces *)
        let range = Range.create ~start ~end_:start in
        let edit = TextEdit.create ~newText:(String.make spaces ' ') ~range in
        let delta = delta + spaces in
        iter ops ~lnum ~delta ( edit :: rev )
      else
        (* delete spaces *)
        let spaces = -spaces in
        let end_ = Position.create ~line ~character:(char + spaces) in
        let range = Range.create ~start ~end_ in
        let edit = TextEdit.create ~newText:"" ~range in
        let delta = delta - spaces in
        iter ops ~lnum:line ~delta ( edit :: rev )

  in
  iter ops [] ~lnum:0 ~delta:0

(*
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
*)

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
  let _edit_list, edit_ops =
    Cobol_indent.Main.indent
      ~dialect:(Cobol_config.dialect project.config.cobol_config)
      ~source_format:project.config.source_format
      ~config:project.config.indent_config
      ~filename:(Lsp.Uri.to_path doc.uri)
      ~contents:(Lsp.Text_document.text textdoc)
      ~range:range_to_indent
      ()
  in
  Some ( to_textedits edit_ops ) (* (List.map lsp_text_edit edit_list) *)

let handle_formatting registry params =
  let DocumentFormattingParams.{ textDocument = doc; _ } = params in
  let Lsp_document.{ project; textdoc; _ } =
    Lsp_server.find_document doc registry in
  try
    let _editList, edit_ops =
      Cobol_indent.Main.indent
        ~dialect:(Cobol_config.dialect project.config.cobol_config)
        ~source_format:project.config.source_format
        ~config:project.config.indent_config
        ~filename:(Lsp.Uri.to_path doc.uri)
        ~contents:(Lsp.Text_document.text textdoc)
        ()
    in
    Some ( to_textedits edit_ops ) (* List.map lsp_text_edit editList) *)
  with Failure msg ->
    Lsp_error.internal "Formatting error: %s" msg

(** {3 Semantic tokens} *)

let handle_semtoks_full,
    handle_semtoks_range =
  let handle registry ?range (doc: TextDocumentIdentifier.t) =
    try_with_main_document_data registry doc
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

let doc_of_datadef ~rev_comments ~filename data_def =
  let open Cobol_preproc.Text in
  let loc = Cobol_data.Item.def_loc data_def in
  let def_filename = (fst @@ Cobol_common.Srcloc.as_lexloc loc).pos_fname in
  if not (String.equal filename def_filename) (** def is in copybook *)
  then ""
  else
    let def_range = Lsp_position.range_of_srcloc_in ~filename loc in
    let (inline, full_line) =
      List.fold_left begin fun acc { comment_loc; comment_kind; comment_contents } ->
        let com_range = Lsp_position.range_of_lexloc comment_loc in
        if def_range.start.line = com_range.start.line
        then (Some comment_contents, snd acc)
        else if def_range.start.line = com_range.start.line + 1
             && comment_kind == `Line
        then (fst acc, Some comment_contents)
        else acc
      end (None, None) rev_comments
    in
    match inline, full_line with
    | Some comment, _ -> "\n---\n" ^ String.sub comment 2 (String.length comment - 2)
    | None, Some comment -> "\n---\n" ^ String.sub comment 1 (String.length comment - 1)
    | _ -> ""

let lookup_data_definition_for_hover cu_name element_at_pos group =
  let { payload = cu; _ } = CUs.find_by_name cu_name group in
  let named_data_defs = cu.unit_data.data_items.named in
  try match element_at_pos with
    | Data_item { full_qn = Some qn; def_loc } ->
        Cobol_unit.Qualmap.find qn named_data_defs, def_loc
    | Data_full_name qn | Data_name qn ->
        Cobol_unit.Qualmap.find qn named_data_defs, Lsp_lookup.baseloc_of_qualname qn
    | Data_item _ | Proc_name _ ->
        raise Not_found
  with Cobol_unit.Qualmap.Ambiguous _ -> raise Not_found

let data_definition_on_hover
    ?(always_show_hover_text_in_data_div = false) ~rev_comments
    ~uri position Cobol_typeck.Outputs.{ group; _ } =
  let filename = Lsp.Uri.to_path uri in
  match Lsp_lookup.element_at_position ~uri position group with
  | { element_at_position = None; _ }
  | { enclosing_compilation_unit_name = None; _ } ->
      None
  | { element_at_position = Some ele_at_pos;
      enclosing_compilation_unit_name = Some cu_name } ->
      try
        let data_def, hover_loc
          = lookup_data_definition_for_hover cu_name ele_at_pos group in
        let doc_comments = doc_of_datadef ~rev_comments ~filename data_def in
        if always_show_hover_text_in_data_div ||
           not (Lsp_position.is_in_srcloc ~filename position @@
                Cobol_data.Item.def_loc data_def)
        then Some (Pretty.to_string "%a%s"
                     Lsp_data_info_printer.pp_data_definition data_def doc_comments,
                   hover_loc)
        else None
      with Not_found ->
        None


let hover_markdown ~filename ~loc value =
  let content = MarkupContent.create ~kind:MarkupKind.Markdown ~value in
  let range = Lsp_position.range_of_srcloc_in ~filename loc in
  Some (Hover.create () ~contents:(`MarkupContent content) ~range)

let cobol_code fmt =                                   (* TODO: ensure no ``` *)
  Pretty.to_string ("```cobol\n" ^^ fmt ^^ "\n```")

let find_hovered_pplog_event ~filename position pplog =
  List.find_opt begin function
    | Cobol_preproc.Trace.Replace _
    | CompilerDirective _
    | Exec_block _
    | Ignored _ ->
        false
    | Replacement { matched_loc = loc; _ }
    | FileCopy { copyloc = loc; _ } ->
        try           (* Some locations in the pre-processor log may not involve
                         [filename], so we need to catch those cases. *)
          Lsp_position.is_in_lexloc position
            (Cobol_common.Srcloc.lexloc_in ~filename loc)
        with Invalid_argument _ -> false
  end (Cobol_preproc.Trace.events pplog)

let preproc_info_on_hover ~filename position pplog =
  match find_hovered_pplog_event ~filename position pplog with
  | Some Replacement { matched_loc = loc; replacement_text = []; _ } ->
      Some ("empty text", loc)
  | Some Replacement { matched_loc = loc; replacement_text; _ } ->
      Some (cobol_code "%a" Cobol_preproc.Text.pp_text replacement_text, loc)
  | Some FileCopy { copyloc = loc; status = CopyDone lib | CyclicCopy lib } ->
      (match EzFile.read_file lib with
       | "" -> None
       | text -> Some (cobol_code "%s" text, loc))
  | Some FileCopy { status = MissingCopy _; _ }
  | Some Replace _
  | Some CompilerDirective _
  | Some Exec_block _
  | Some Ignored _
  | None ->
      None

let handle_hover ?always_show_hover_text_in_data_div
    registry HoverParams.{ textDocument = doc; position; _ } =
  let filename = Lsp.Uri.to_path doc.uri in
  try_with_main_document_data registry doc
    ~f:begin fun ~doc:{ artifacts = { pplog; rev_comments; _ }; _ } checked_doc ->
      match data_definition_on_hover ~uri:doc.uri position checked_doc
              ?always_show_hover_text_in_data_div ~rev_comments,
            preproc_info_on_hover ~filename position pplog with
      | None, None ->
          None
      | None, Some (text, loc) | Some (text, loc), None ->
          hover_markdown ~filename ~loc text
      | Some(info_text, loc), Some(pp_text, _) ->
          hover_markdown ~filename ~loc @@
          Pretty.to_string "%s\n---\nAdditional pre-processing:\n%s"
            info_text pp_text
    end

(** {3 Completion} *)

let handle_completion ?(eager=true) registry (params: CompletionParams.t) =
  try_with_main_document_data registry params.textDocument
    ~f:begin fun ~doc checked_doc->
      let config = Lsp_completion.config ~eager () in
      let completion_list =
        Lsp_completion.contextual ~config
          doc checked_doc params.position
      in Some (`CompletionList completion_list)
    end

(** {3 Folding} *)

(*TODO(if necessary):
    Now, the request folding has the default perfomance (in VS Code)
    It only supports folding complete lines, and does
    not support FoldingRangeKind or CollapsedText
    (To support these features, need to change the client capability) *)
let handle_folding_range registry (params: FoldingRangeParams.t) =
  try_with_main_document_data registry params.textDocument
    ~f:begin fun ~doc:_ { ptree; group; _ } ->
      let filename = Lsp.Uri.to_path params.textDocument.uri in
      Some (Lsp_folding.ranges_in ~filename ptree group)
    end

(** { Document Symbol } *)

let handle_document_symbol registry (params: DocumentSymbolParams.t) =
  try_with_main_document_data registry params.textDocument
    ~f:begin fun ~doc { ptree; _ } ->
      let uri = Lsp.Text_document.documentUri doc.textdoc in
      let symbols = Lsp_document_symbol.from_ptree_at ~uri ptree in
      Some (`DocumentSymbol symbols)
    end

(** { Document Code Lens } *)

module Positions = Set.Make (struct
    type t = Position.t
    let compare (p1: t) (p2: t) =
      let c = p2.line - p1.line in
      if c <> 0 then c else p2.character - p1.character
  end)

let codelens_positions ~uri group =
  let filename = Lsp.Uri.to_path uri in
  let open struct
    include Cobol_common.Visitor
    include Cobol_data.Visitor
    type context =
      | ProcedureDiv
      | DataDiv
      | None
  end in
  let set_context context (old, acc) =
    do_children_and_then (context, acc) (fun (_, acc) -> (old, acc))
  in
  let take_when_in context { loc; _ } (current, acc) =
    if context <> current
    then skip (current, acc)
    else
      let range = Lsp_position.range_of_srcloc_in ~filename loc in
      skip (context, Positions.add range.start acc)
  in
  Cobol_unit.Visitor.fold_unit_group
    object (v)
      inherit [_] Cobol_unit.Visitor.folder
      method! fold_procedure _ = set_context ProcedureDiv
      method! fold_data_definitions _ = set_context DataDiv
      method! fold_paragraph' _ = skip
      method! fold_procedure_name' = take_when_in ProcedureDiv
      method! fold_qualname' = take_when_in DataDiv
      method! fold_record_renaming { renaming_name; _ } =
        take_when_in DataDiv renaming_name
      method! fold_field_definition { field_qualname; field_redefines;
                                      field_leading_ranges;
                                      field_offset; field_size; field_layout;
                                      field_conditions; field_redefinitions;
                                      field_length = _ } acc =
        ignore(field_redefines, field_leading_ranges, field_offset, field_size);
        skip @@ begin acc
          |> Cobol_ptree.Terms_visitor.fold_qualname'_opt v field_qualname
          |> fold_field_layout v field_layout
          |> fold_condition_names v field_conditions
          |> fold_item_redefinitions v field_redefinitions
        end
      method! fold_table_definition { table_field; table_offset; table_size;
                                      table_range; table_init_values;
                                      table_redefines; table_redefinitions } acc =
        ignore(table_offset, table_size, table_init_values, table_redefines);
        skip @@ begin acc
          |> fold_field_definition' v table_field
          |> fold_table_range v table_range
          |> fold_item_redefinitions v table_redefinitions
        end
    end group (None, Positions.empty)
  |> snd

let handle_codelens registry ({ textDocument; _ }: CodeLensParams.t) =
  try_with_main_document_data registry textDocument
    ~f:begin fun ~doc checked_doc ->
      let uri = Lsp.Text_document.documentUri doc.textdoc in
      let rootdir = Lsp_project.(string_of_rootdir @@ rootdir doc.project) in
      let context = ReferenceContext.create ~includeDeclaration:true in
      codelens_positions ~uri checked_doc.group
      |> Positions.to_seq
      |> Seq.map begin fun position ->
        let params =
          ReferenceParams.create ~context ~position ~textDocument () in
        let ref_count =
          lookup_references_in_doc ~rootdir params checked_doc
          |> Option.fold ~none:0 ~some:List.length in
        let title = string_of_int ref_count
                    ^ " reference"
                    ^ if ref_count > 1 then "s" else "" in
        let range = Range.create ~end_:position ~start:position in
        let uri = DocumentUri.yojson_of_t textDocument.uri in
        let command = Command.create () ~title
            ~command:"superbol.editor.action.findReferences"
            ~arguments:[uri; Position.yojson_of_t position] in
        CodeLens.create ~command ~range ()
      end
      |> List.of_seq |> Option.some
    end
  |> Option.value ~default:[]

(** { Rename } *)

let handle_rename ?(ignore_when_copybook=false)
    registry
    ({ textDocument; position; newName = newText; _ }: RenameParams.t) =
  try_with_main_document_data registry textDocument
    ~f:begin fun ~doc checked_doc ->
      let rootdir = Lsp_project.(string_of_rootdir @@ rootdir doc.project) in
      let locations = Option.value ~default:[] @@
        let context = ReferenceContext.create ~includeDeclaration:true in
        let params = ReferenceParams.create
            ~context ~position ~textDocument () in
        lookup_references_in_doc ~rootdir params checked_doc in
      let changes, is_copybook =
        List.fold_left begin fun (map, is_copybook) ({ range; uri }: Location.t) ->
          URIMap.add_to_list uri (TextEdit.create ~newText ~range) map,
          is_copybook || DocumentUri.compare uri textDocument.uri <> 0
        end (URIMap.empty, false) locations in
      let changes = List.of_seq @@ URIMap.to_seq changes in
      if is_copybook && ignore_when_copybook
      then begin Lsp_io.notify_info
          "Ignored renaming of a reference that occurs in a copybook";
        Some ( WorkspaceEdit.create () ) end
      else
        begin if is_copybook
          then Lsp_io.notify_warn
              "Proceeded to rename of a reference that occurs in a copybook";
          Some ( WorkspaceEdit.create ~changes () ) end
    end
  |> Option.get


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
  | NotInitialized config, Initialize init_params ->
      initialize ~config init_params
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
    | DocumentSymbol params ->
        Ok (handle_document_symbol registry params, state)
    | TextDocumentCodeLens (* CodeLensParams.t.t *) params ->
        Ok (handle_codelens registry params, state)
    | TextDocumentRename params ->
        Ok (handle_rename registry params, state)
    | TextDocumentDeclaration  (* TextDocumentPositionParams.t.t *) _
    | TextDocumentTypeDefinition  (* TypeDefinitionParams.t.t *) _
    | TextDocumentImplementation  (* ImplementationParams.t.t *) _
    | TextDocumentCodeLensResolve  (* CodeLens.t.t *) _
    | TextDocumentPrepareCallHierarchy  (* CallHierarchyPrepareParams.t.t *) _
    | TextDocumentPrepareRename  (* PrepareRenameParams.t.t *) _
    | TextDocumentLink  (* DocumentLinkParams.t.t *) _
    | TextDocumentLinkResolve  (* DocumentLink.t.t *) _
    | TextDocumentMoniker  (* MonikerParams.t.t *) _
    | WorkspaceSymbol  (* WorkspaceSymbolParams.t.t *) _
    | DebugEcho (* DebugEcho.Params.t *) _
    | DebugTextDocumentGet  (* DebugTextDocumentGet.Params.t *) _
    | TextDocumentHighlight  (* DocumentHighlightParams.t.t *) _
    | InlayHint (* InlayHintParams.t.t *) _
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
    | UnknownRequest { meth = "superbol/writeProjectConfiguration";
                       params = Some param } ->
        handle_write_project_config_command param registry
    | UnknownRequest { meth = "superbol/getProjectConfiguration";
                       params = Some param } ->
        handle_get_project_config_command param registry
    | UnknownRequest { meth = "superbol/getCFG";
                       params = Some param } ->
        Ok (handle_open_cfg registry param, state)
    | UnknownRequest { meth = "superbol/findProcedure";
                       params = Some param } ->
        Ok (handle_find_procedure registry param, state)
    | UnknownRequest { meth; _ } ->
        Lsp_debug.message "Lsp_request: unknown request (%s)" meth;
        Error (UnknownRequest meth)

let handle (Jsonrpc.Request.{ id; _ } as req) state =
  match Lsp.Client_request.of_jsonrpc req with
  | Error message ->
      let err = Jsonrpc.Response.Error.make ~message ~code:InvalidRequest () in
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
  let lookup_references = handle_references
  let hover = handle_hover
  let completion = handle_completion
  let codelens = handle_codelens
  let document_symbol = handle_document_symbol
  let formatting = handle_formatting
  let rename = handle_rename
end
