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

open EzCompat                                                    (* StringMap *)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Lsp_imports
open Lsp_project.TYPES
open Lsp_server.TYPES
open Lsp_lookup.TYPES
open Lsp.Types

module TYPES = struct
  type alternate_handler =
    {
      h: 'r. 'r Lsp.Client_request.t -> registry -> ('r * registry, 'r error) result;
    }
end
open TYPES

(** {2 Handling requests} *)

(** Catch generic exception cases, and report errors using {!Lsp_error}.
    Returns [None] in case the document cannot be parsed (or is a copybook, for
    now).  [f] has to return an optional value. *)
let try_with_doc ~f registry doc_id =
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

(** Same as {!try_with_doc}, with some additional document data. *)
let try_with_checked_doc ~f =
  try_with_doc ~f:(fun ~doc -> f ~doc @@ Lsp_document.checked doc)

(** {3 Initialization} *)

let log_version_info () =
  match Version.commit_hash, Version.commit_date with
  | None, _ | _, None ->
      Lsp_io.log_info "Version: %s" Version.version
  | Some h, Some d ->
      Lsp_io.log_info "Commit: %s (%s)" h d;
      Lsp_io.log_info "Version tag: %s" Version.version

let log_backend_info () =
  Lsp_io.log_info "OS type: %s" Sys.os_type;
  Lsp_io.log_info "Backend: %s"
    (match Sys.backend_type with
     | Native -> "native"
     | Bytecode -> "bytecode"
     | Other s -> s)

let log_initialization_info () =
  Lsp_io.log_info "Initializing SuperBOL LSP Server";
  log_version_info ();
  log_backend_info ()

let initialize ~config (params: InitializeParams.t) =
  let root_uri = match params.rootUri with
    | None -> None
    | Some uri -> Some uri
  in
  let workspace_folders = match params.workspaceFolders with
    | Some Some (_ :: _ as l) -> List.map (fun x -> x.WorkspaceFolder.uri) l
    | _ -> Option.to_list root_uri
  in
  log_initialization_info ();
  Lsp_io.log_info "Initial workspace folders: %a"
    Pretty.(list ~fopen:"" ~fclose:"" string)
    (List.map (fun x -> DocumentUri.to_path x) workspace_folders);
  let capabilities = Lsp_capabilities.reply params.capabilities in
  let with_semantic_tokens =
    capabilities.semanticTokensProvider <> None
  and position_encoding =
    match capabilities.positionEncoding with
    | Some UTF8 -> `UTF8
    | _ -> `UTF16                                    (* use a sensible default *)
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
    | Some { didChangeWatchedFiles = Some { dynamicRegistration = Some true;
                                            relativePatternSupport };
             _ } ->
        `yes (if relativePatternSupport = Some true then `any else `absolute)
    | _ ->
        `no
  in
  let watcher ppf = function
    | `no -> Fmt.string ppf "none"
    | `yes `absolute -> Fmt.string ppf "absolute patterns only"
    | `yes `any -> Fmt.string ppf "any pattern"
  in
  Lsp_io.log_info "Negociated@ server@ parameters:@\n@[%t@]" @@
  Pretty.delayed_record [
    Fmt.(field "client_config_watcher" (fun _ -> with_client_config_watcher) bool);
    Fmt.(field "client_file_watcher" (fun _ -> with_client_file_watcher) watcher);
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
                            with_client_file_watcher;
                            position_encoding })


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

let handle_get_cfg registry params =
  let params = Jsonrpc.Structured.yojson_of_t params in
  let uri, name, options = Yojson.Safe.Util.(
      DocumentUri.t_of_yojson @@ member "uri" params,
      to_string @@ member "name" params,
      try to_assoc @@ member "render_options" params with Type_error _ -> [])
  in
  let textDoc = TextDocumentIdentifier.create ~uri in
  try_with_checked_doc registry textDoc
    ~f:begin fun ~doc:_ checked_doc ->
      let jsoono =
        Lsp_cfg.doc_to_cfg_jsoono ~filename:(DocumentUri.to_path uri) ~name ~options checked_doc
      in Some jsoono
    end |>
  Option.get

let handle_get_possible_cfg registry params =
  let params = Jsonrpc.Structured.yojson_of_t params in
  let uri = Yojson.Safe.Util.(DocumentUri.t_of_yojson @@ member "uri" params) in
  let textDoc = TextDocumentIdentifier.create ~uri in
  try_with_checked_doc registry textDoc
    ~f:begin fun ~doc:_ checked_doc ->
      let open Cobol_cfg.Builder in
      let possibles = possible_cfgs_of_doc checked_doc in
      let yojsonify cfg_name = `String cfg_name in
      Some (`List (List.map yojsonify possibles))
    end |>
  Option.get


let handle_find_procedure registry params =
  let params = Jsonrpc.Structured.yojson_of_t params in
  let filename = Yojson.Safe.Util.to_string @@ Yojson.Safe.Util.member "uri" params in
  let line = Yojson.Safe.Util.to_int @@ Yojson.Safe.Util.member "line" params in
  let character = Yojson.Safe.Util.to_int @@ Yojson.Safe.Util.member "character" params in
  let textDoc = TextDocumentIdentifier.create ~uri:(DocumentUri.of_path filename) in
  try_with_checked_doc registry textDoc
    ~f:begin fun ~doc:_ checked_doc ->
      let pos = Position.create ~character ~line in
      let { cu; proc_name } =
        Lsp_lookup.proc_at_pos ~filename pos checked_doc.group in
      let proc = match proc_name, cu with
        | Some qn, _ -> Pretty.to_string "%a" Cobol_ptree.pp_qualname qn
                        |> Str.global_replace (Str.regexp "\n") " "
        | _ -> raise Not_found in
      Some (`String proc)
    end |>
  Option.get

(** {3 Definitions} *)


let focus_on_name_in_defintions = true

let find_cu_data_definition Lsp_position.{ location_of; location_of_srcloc }
    ?(allow_notifications = true)
    (qn: Cobol_ptree.qualname) (cu: Cobol_unit.Types.cobol_unit) =
  match Cobol_unit.Resolver_map.find qn cu.unit_data.data_items.named with
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
  | exception Cobol_unit.Resolver_map.Ambiguous _
    when not allow_notifications ->
      []
  | exception Not_found ->
      (* Note: we keep that for ourselves for now as not all of the DATA DIV. is
         analyzed. *)
      (* Lsp_notify.unknown "data-name" qn; *)
      []
  | exception Cobol_unit.Resolver_map.Ambiguous (lazy matching_qualnames) ->
      Lsp_notify.ambiguous "data-name" qn ~matching_qualnames;
      []

let find_cu_proc_definition
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
  | exception Cobol_unit.Resolver_map.Ambiguous _
    when not allow_notifications ->
      []
  | exception Not_found ->
      Lsp_notify.unknown "procedure-name" qn;
      []
  | exception Cobol_unit.Resolver_map.Ambiguous (lazy matching_qualnames) ->
      Lsp_notify.ambiguous "procedure-name" qn ~matching_qualnames;
      []

let find_definitions ?allow_notifications loc_translator
    ?cu_name element_at_pos group =
  let[@local] with_cu f =
    match cu_name with
    | None ->
        []
    | Some cu_name ->
        try f ~&(CUs.find_by_name cu_name group) with Not_found -> []
  in
  match element_at_pos with
  | Data_item { full_qn = Some qn; _ } | Data_full_name qn | Data_name qn ->
      with_cu @@
      find_cu_data_definition loc_translator ?allow_notifications qn
  | Data_item { full_qn = None; item_loc } ->
      [loc_translator.location_of_srcloc item_loc]
  | Proc_name { qn; in_section } ->
      with_cu @@
      find_cu_proc_definition loc_translator ?allow_notifications ?in_section qn
  | Preproc_or_compilation_variable_ref
      { def = Compilation_var { src = Source_location loc; _ }
            |     Preproc_var { src = Source_location loc; _ }; _ } ->
      [loc_translator.location_of_srcloc loc]
  | Preproc_or_compilation_variable_ref
      { def = Compilation_var { src = Process_parameter |
                                      Process_environment; _ }
            | Preproc_var     { src = Process_parameter |
                                      Process_environment; _ }; _ } ->
      []                    (* CHECKME: location of ref may be the definition *)

let lookup_definition_in_doc position ~(doc: Lsp_document.t)
    Cobol_typeck.Outputs.{ group; _ }
  =
  let rootdir = Lsp_project.(string_of_rootdir @@ rootdir doc.project)
  and uri = Lsp_document.uri doc
  and artifacts = doc.artifacts in
  let filename = Lsp.Uri.to_path uri in
  match Lsp_lookup.element_at_position ~filename position group artifacts with
  | { element_at_position = None; _ } ->
      None
  | { element_at_position = Some element;
      enclosing_compilation_unit_name = cu_name } ->
      let loc_translator = Lsp_position.loc_translator ~rootdir uri in
      Some (`Location (find_definitions loc_translator ?cu_name element group))

let handle_definition registry (params: DefinitionParams.t) =
  try_with_checked_doc registry params.textDocument
    ~f:(lookup_definition_in_doc params.position)

(** {3 References} *)

let lookup_qn ~kind ~lookup qn =
  try Some (lookup qn) with
  | Not_found ->
      Lsp_notify.unknown kind qn;
      None
  | Cobol_unit.Resolver_map.Ambiguous (lazy matching_qualnames) ->
      Lsp_notify.ambiguous kind qn ~matching_qualnames;
      None

let find_full_qn ~kind qn qmap =
  lookup_qn ~kind qn
    ~lookup:(fun qn -> (Cobol_unit.Resolver_map.find_binding qn qmap).full_qn)

let find_proc_qn ~kind qn ?in_section cu =
  lookup_qn ~kind qn
    ~lookup:begin fun qn ->
      Cobol_unit.Procedure.full_qn ?in_section qn
        cu.Cobol_unit.Types.unit_procedure
    end

let ppenv_var_reference_locs (loc_translator: Lsp_position.translator)
    ~(doc: Lsp_document.t)
    (ppvar_def: Cobol_preproc.Env.var_definition) =
  List.filter_map begin fun (event: Cobol_preproc.Trace.log_entry) ->
    match ppvar_def, event with
    | Compilation_var compvar_def,
      (* | Variable_definition _ / filter out definition locs *)
      (Variable_substitution { loc; def; _ } |
       Variable_evaluation { loc; def = Some Compilation_var def; _ }) ->
        if def == compvar_def (* Note: assumes sharing btw map and log entries *)
        then Some (loc_translator.location_of_srcloc loc)
        else None
    | Preproc_var ppvar_def,
      Variable_evaluation { loc; def = Some Preproc_var def; _ } ->
        if def == ppvar_def   (* Note: assumes sharing btw map and log entries *)
        then Some (loc_translator.location_of_srcloc loc)
        else None
    | _ ->
        None
  end (Cobol_preproc.Trace.events doc.artifacts.pplog)

let lookup_references_in_doc position ~with_declaration ~(doc: Lsp_document.t)
    Cobol_typeck.Outputs.{ group; artifacts = { references }; _ }
  =
  let rootdir = Lsp_project.(string_of_rootdir @@ rootdir doc.project)
  and uri = Lsp_document.uri doc
  and artifacts = doc.artifacts in
  let filename = Lsp.Uri.to_path uri in
  match Lsp_lookup.element_at_position ~filename position group artifacts with
  | { element_at_position = None; _ } ->
      Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                         element_at_position = None";
      None
  | { element_at_position = Some element;
      enclosing_compilation_unit_name = cu_name } ->
      let Lsp_position.{ location_of_srcloc; _ } as loc_translator
        = Lsp_position.loc_translator ~rootdir uri in
      let data_refs (cu_refs: Cobol_typeck.Outputs.references_in_unit) qn =
        List.rev_map location_of_srcloc
          (Cobol_unit.Qual.MAP.find qn cu_refs.data_refs)
      and proc_refs (cu_refs: Cobol_typeck.Outputs.references_in_unit) qn =
        List.rev_map location_of_srcloc
          (Cobol_unit.Qual.MAP.find qn cu_refs.proc_refs)
      and def_locs =
        if with_declaration then
          find_definitions ~allow_notifications:false loc_translator
            ?cu_name element group
        else []
      in
      let[@local] with_cu_n_refs f =
        match cu_name with
        | None ->
            []
        | Some cu_name ->
            try f @@ CUMap.find_by_name cu_name references
            with Not_found -> []
      in
      let ref_locs =
        match element with
        | Data_full_name qn
        | Data_item { full_qn = Some qn; _ } ->
            Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                               Data_full_name...";
            with_cu_n_refs @@ fun (_cu, cu_refs) ->
            data_refs cu_refs qn
        | Data_item { full_qn = None; _ } ->
            Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                               Data_item...";
            []
        | Data_name qn ->
            Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                               Data_name...";
            with_cu_n_refs @@ fun (cu, cu_refs) ->
            Option.fold ~none:[] ~some:(data_refs cu_refs) @@
            find_full_qn qn ~&cu.unit_data.data_items.named ~kind:"data-name"
        | Proc_name { qn; in_section } ->
            Lsp_debug.message "Lsp_request.lookup_references_in_doc: \
                               Proc_name...";
            with_cu_n_refs @@ fun (cu, cu_refs) ->
            Option.fold ~none:[] ~some:(proc_refs cu_refs) @@
            find_proc_qn qn ?in_section ~&cu ~kind:"procedure-name"
        | Preproc_or_compilation_variable_ref { def; _ } ->
            ppenv_var_reference_locs ~doc loc_translator def
      in
      Some (def_locs @ ref_locs)

let handle_references state (params: ReferenceParams.t) =
  try_with_checked_doc state params.textDocument
    ~f:(lookup_references_in_doc params.position
          ~with_declaration:params.context.includeDeclaration)

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
(* Note: the source format is resolved via [source_format_for], which checks
   for per-extension overrides before falling back to the global setting. *)
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
    let filename = Lsp.Uri.to_path doc.uri in
    Cobol_indent.Main.indent
      ~platform:Lsp_platform.record
      ~dialect:(Cobol_config.dialect project.config.cobol_config)
      ~source_format:(Superbol_project.Config.source_format_for
                        ~filename project.config)
      ~config:project.config.indent_config
      ~filename
      ~contents:(Lsp.Text_document.text textdoc)
      ~range:range_to_indent
      ()
  in
  Some ( to_textedits edit_ops ) (* (List.map lsp_text_edit edit_list) *)

(* Note: the source format is resolved via [source_format_for], which checks
   for per-extension overrides before falling back to the global setting. *)
let handle_formatting registry params =
  let DocumentFormattingParams.{ textDocument = doc; _ } = params in
  let Lsp_document.{ project; textdoc; _ } =
    Lsp_server.find_document doc registry in
  try
    let filename = Lsp.Uri.to_path doc.uri in
    let _editList, edit_ops =
      Cobol_indent.Main.indent
        ~platform:Lsp_platform.record
        ~dialect:(Cobol_config.dialect project.config.cobol_config)
        ~source_format:(Superbol_project.Config.source_format_for
                          ~filename project.config)
        ~config:project.config.indent_config
        ~filename
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
    try_with_checked_doc registry doc
      ~f:begin fun ~doc:{ artifacts = { pplog; tokens;
                                        rev_comments; rev_ignored; _ };
                          _ } Cobol_typeck.Outputs.{ ptree; _ } ->
        let data =
          let rev_comments = StringMap.find "" rev_comments in
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

type data_definition =
  | Regular of Cobol_data.Types.data_definition
  | Preproc of Cobol_preproc.Env.var_definition              (* or compil-var *)

let doc_of_datadef data_def (artifacts: Cobol_parser.Outputs.artifacts) =
  let definition_comment def_loc =
    let definition_lexloc = Cobol_common.Srcloc.as_lexloc def_loc in
    let definition_filename = (fst definition_lexloc).pos_fname in
    try
      let rev_comments = StringMap.find definition_filename artifacts.rev_comments in
      let definition_range = Lsp_position.range_of_lexloc definition_lexloc in
      let definition_line = definition_range.start.line in
      List.find_map begin fun Cobol_preproc.Text.{ comment_loc; comment_kind;
                                                   comment_contents = c } ->
        let comment_range = Lsp_position.range_of_lexloc comment_loc in
        let comment_line = comment_range.start.line in
        if definition_line = comment_line
        then Some (String.sub c 2 (String.length c - 2))
        else if definition_line = comment_line + 1 && comment_kind == `Line
        then Some (String.sub c 1 (String.length c - 1))
        else None
      end rev_comments |> function
      | Some c -> c
      | None -> ""
    with Invalid_argument _ | Not_found ->
      ""
  in
  match data_def with
  | Preproc Compilation_var { src = Process_parameter; _ }
  | Preproc Preproc_var     { src = Process_parameter; _ } ->
      "Given as process parameter"
  | Preproc Compilation_var { src = Process_environment; _ }
  | Preproc Preproc_var     { src = Process_environment; _ } ->
      "Defined in process environment"
  | Preproc Compilation_var { src = Source_location loc; _ }
  | Preproc Preproc_var     { src = Source_location loc; _ } ->
      definition_comment loc
  | Regular data_def ->
      definition_comment @@ Cobol_data.Item.def_loc data_def

let lookup_data_definition ?cu_name element_at_pos group =
  let lookup_qn qn =
    match cu_name with
    | None ->
        raise Not_found
    | Some cu_name ->
        let { payload = cu; _ } = CUs.find_by_name cu_name group in
        let named_data_defs = cu.unit_data.data_items.named in
        try
          Cobol_unit.Resolver_map.find qn named_data_defs
        with Cobol_unit.Resolver_map.Ambiguous _ ->
          raise Not_found
  in
  match element_at_pos with
  | Data_item { full_qn = Some qn; _ } | Data_full_name qn | Data_name qn ->
      Regular (lookup_qn qn)
  | Data_item _ | Proc_name _ ->
      raise Not_found
  | Preproc_or_compilation_variable_ref { def; _ } ->
      Preproc def

let element_defintion_location = function
  | Data_item { item_loc; _ } ->
      item_loc
  | Data_full_name qn | Data_name qn | Proc_name { qn; _ } ->
      Lsp_lookup.baseloc_of_qualname qn
  | Preproc_or_compilation_variable_ref { loc; _ } ->
      loc

let pp_data_definition_info ppf = function
  | Regular def ->
      Lsp_data_info_printer.pp_data_definition ppf def
  | Preproc def ->
      Lsp_data_info_printer.pp_compilation_var_definition ppf def

let describe_data_definition_for_element_at_pos
    ?(show_hover_text_on_definitions = false)
    ~(doc: Lsp_document.t) ~(checked_doc: Cobol_typeck.Outputs.t) position
  =
  let Cobol_typeck.Outputs.{ group; _ } = checked_doc in
  let filename = Lsp.Uri.to_path @@ Lsp_document.uri doc
  and artifacts = doc.artifacts in
  match Lsp_lookup.element_at_position ~filename position group artifacts with
  | { element_at_position = None; _ } ->
      None
  | { element_at_position = Some ele_at_pos;
      enclosing_compilation_unit_name = cu_name } ->
      try
        let data_def = lookup_data_definition ?cu_name ele_at_pos group
        and hover_loc = element_defintion_location ele_at_pos in
        let data_def_src = match data_def with
          | Regular def -> Source_location (Cobol_data.Item.def_loc def)
          | Preproc Preproc_var def -> def.src
          | Preproc Compilation_var def -> def.src
        in
        let pp_documentation ppf =
          let doc_comments = doc_of_datadef data_def artifacts in
          if doc_comments <> ""
          then Pretty.print ppf "\n---\n%s" doc_comments
        in
        let text =
          if show_hover_text_on_definitions ||
             not (Lsp_position.is_in_src ~filename position data_def_src)
          then Some (Pretty.to_string "%a%t"
                       pp_data_definition_info data_def
                       pp_documentation)
          else None
        in
        Some (text, hover_loc)
      with Not_found ->
        None

let data_references position ~(doc: Lsp_document.t) checked_doc =
  Option.map List.length @@
  lookup_references_in_doc position ~doc checked_doc
    ~with_declaration:true

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
    | FileCopy { copyloc = loc; _ }
    | Variable_definition { loc; _ }
    | Variable_substitution { loc; _ }
    | Variable_evaluation { loc; _ } ->
        try           (* Some locations in the pre-processor log may not involve
                         [filename], so we need to catch those cases. *)
          Lsp_position.is_in_srcloc ~filename position loc
        with Invalid_argument _ -> false
  end (Cobol_preproc.Trace.events pplog)

let preproc_info_on_hover ~filename position pplog =
  match find_hovered_pplog_event ~filename position pplog with
  | Some Replacement { matched_loc = loc; replacement_text = []; _ } ->
      Some ("empty text", loc)
  | Some Replacement { matched_loc = loc; replacement_text; _ } ->
      Some (cobol_code "%a" Cobol_preproc.Text.pp_text replacement_text, loc)
  | Some FileCopy { copyloc = loc; status = CopyDone lib | CyclicCopy lib } ->
      (match Lsp_platform.record.read_text_file lib with
       | "" -> None
       | text -> Some (cobol_code "%s" text, loc))
  | Some Variable_definition _
  | Some Variable_substitution _
  | Some Variable_evaluation _
  | Some FileCopy { status = MissingCopy _; _ }
  | Some Replace _
  | Some CompilerDirective _
  | Some Exec_block _
  | Some Ignored _
  | None ->
      None

let handle_hover ?show_hover_text_on_definitions
    registry HoverParams.{ textDocument; position; _ } =
  let filename = Lsp.Uri.to_path textDocument.uri in
  try_with_checked_doc registry textDocument
    ~f:begin fun ~doc checked_doc ->
      let ref_count () = data_references position ~doc checked_doc in
      match
        describe_data_definition_for_element_at_pos position ~doc ~checked_doc
          ?show_hover_text_on_definitions,
        preproc_info_on_hover ~filename position doc.artifacts.pplog
      with
      | None, None ->
          None
      | Some (None, loc), None ->
          Option.bind (ref_count ()) @@ fun n ->
          hover_markdown ~filename ~loc @@ Printf.sprintf "References: %d" n
      | None, Some (text, loc) ->
          hover_markdown ~filename ~loc text
      | Some (Some text, loc), None ->
          let ref_text =
            Option.fold ~none:"" ~some:(Printf.sprintf "\n\n---\nReferences: %d")
              (ref_count ()) in
          hover_markdown ~filename ~loc @@ text ^ ref_text
      | Some (def_text, loc), Some (pp_text, _) ->
          let ref_text =
            Option.fold ~none:"" ~some:(Printf.sprintf "\n\n---\nReferences: %d")
              (ref_count ()) in
          hover_markdown ~filename ~loc @@
          Pretty.to_string "%s%s\n---\nAdditional pre-processing:\n%s"
            (Option.value ~default:"" def_text)
            ref_text
            pp_text
    end

(** {3 Completion} *)

let handle_completion ?(eager=true) registry (params: CompletionParams.t) =
  try_with_checked_doc registry params.textDocument
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
  try_with_checked_doc registry params.textDocument
    ~f:begin fun ~doc:_ { ptree; group; _ } ->
      let filename = Lsp.Uri.to_path params.textDocument.uri in
      Some (Lsp_folding.ranges_in ~filename ptree group)
    end

(** { Document Symbol } *)

let handle_document_symbol registry (params: DocumentSymbolParams.t) =
  try_with_checked_doc registry params.textDocument
    ~f:begin fun ~doc { ptree; _ } ->
      let uri = Lsp.Text_document.documentUri doc.textdoc in
      let symbols = Lsp_document_symbol.from_ptree_at ~uri ptree in
      Some (`DocumentSymbol symbols)
    end

(** { Document Code Lens } *)

let handle_codelens registry ({ textDocument; _ }: CodeLensParams.t) =
  try_with_checked_doc registry textDocument
    ~f:begin fun ~doc checked_doc ->
      Lsp_lens.positions ~uri:textDocument.uri
        checked_doc.group doc.artifacts |>
      List.rev_map begin fun position ->
        let ref_count =
          Option.fold ~none:0 ~some:List.length @@
          lookup_references_in_doc position ~doc checked_doc
            ~with_declaration:false
        in
        let range = Range.create ~end_:position ~start:position in
        let uri = DocumentUri.yojson_of_t textDocument.uri in
        let command = Command.create ()
            ~title:(Pretty.to_string "%d reference%s"
                      ref_count (if ref_count > 1 then "s" else ""))
            ~command:"superbol.editor.action.findReferences"
            ~arguments:[uri; Position.yojson_of_t position] in
        CodeLens.create ~command ~range ()
      end |> Option.some
    end
  |> Option.value ~default:[]

(** { Rename } *)

let handle_rename
    ?(abort_when_in_copybook = true)
    registry
    ({ textDocument; position; newName = newText; _ }: RenameParams.t) =
  Option.value ~default:(WorkspaceEdit.create ()) @@
  try_with_checked_doc registry textDocument
    ~f:begin fun ~doc checked_doc ->
      let locations =
        Option.value ~default:[] @@
        lookup_references_in_doc ~doc position checked_doc
          ~with_declaration:true
      in
      let changes, in_copybook =
        List.fold_left begin fun (map, in_copybook) Location.{ range; uri } ->
          URIMap.add_to_list uri (TextEdit.create ~newText ~range) map,
          in_copybook || DocumentUri.compare uri textDocument.uri <> 0
        end (URIMap.empty, false) locations
      in
      if in_copybook && abort_when_in_copybook
      then begin
        Lsp_io.notify_error "Reference occurs in a copybook: not renaming";
        None
      end else begin
        if in_copybook then
          Lsp_io.notify_warn "Renamed reference that occurs in a copybook";
        let changes = List.of_seq @@ URIMap.to_seq changes in
        Some (WorkspaceEdit.create ~changes ())
      end
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

let fallback (type t) : t Lsp.Client_request.t -> _ = function
  | UnknownRequest { meth; _ } ->
      Lsp_debug.message "%s: unknown request (%s)" __MODULE__ meth;
      Error (UnknownRequest meth)
  | req ->
      Lsp_debug.message "%s: unhandled request" __MODULE__;
      Error (UnhandledRequest req)

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
    | UnknownRequest { meth = "superbol/writeProjectConfiguration";
                       params = Some param } ->
        handle_write_project_config_command param registry
    | UnknownRequest { meth = "superbol/getProjectConfiguration";
                       params = Some param } ->
        handle_get_project_config_command param registry
    | UnknownRequest { meth = "superbol/getCFG";
                       params = Some param } ->
        Ok (handle_get_cfg registry param, state)
    | UnknownRequest { meth = "superbol/getPossibleCFG";
                       params = Some param } ->
        Ok (handle_get_possible_cfg registry param, state)
    | UnknownRequest { meth = "superbol/findProcedure";
                       params = Some param } ->
        Ok (handle_find_procedure registry param, state)
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
    | _ ->
        fallback client_req

let handle ?(alternate_handlers = []) (Jsonrpc.Request.{ id; _ } as req) state =
  match Lsp.Client_request.of_jsonrpc req with
  | Error message ->
      let err = Jsonrpc.Response.Error.make ~message ~code:InvalidRequest () in
      state, Jsonrpc.Response.(error id err)
  | Ok E r ->
      let rec try_alternate_on_error status handlers =
        match status, state, handlers with
        | Ok (reply, state), _, _ ->
            state,
            Jsonrpc.Response.ok id @@ Lsp.Client_request.yojson_of_result r reply
        | Error (UnhandledRequest _ | UnknownRequest _), Running registry,
          handler :: remaining_handlers ->
            let repl = match handler.h r registry with
              | Ok (reply, registry) -> Ok (reply, Running registry)
              | Error e -> Error e
            in
            try_alternate_on_error repl remaining_handlers
        | Error server_error, _, _ ->
            state,
            Jsonrpc.Response.error id @@
            Lsp_server.jsonrpc_of_error server_error req.method_
      in
      try try_alternate_on_error (on_request state r ~id) alternate_handlers with
      | Jsonrpc.Response.Error.E e ->
          state, Jsonrpc.Response.error id e
      | e ->
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
