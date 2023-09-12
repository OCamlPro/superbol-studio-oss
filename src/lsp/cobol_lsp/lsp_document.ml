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

open Lsp_imports
open Lsp_lookup.TYPES
open Lsp_project.TYPES
open Ez_file.V1

module DIAGS = Cobol_common.Diagnostics

module TYPES = struct

  type document =
    {
      project: Lsp_project.t;
      textdoc: Lsp.Text_document.t;
      copybook: bool;
      pplog: Cobol_preproc.log;
      tokens: Cobol_parser.tokens_with_locs Lazy.t;
      parsed: parsed_data option;
      (* Used for caching, when loading a cache file as the file is not reparsed,
         then diagnostics are not sent. *)
      diags: DIAGS.Set.t;
    }
  and parsed_data =
    {
      ast: PTREE.compilation_group;
      cus: CUs.t;
      (* Extracted info: lazy to only ever retrieve what's relevant upon a first
         request. *)
      definitions: name_definitions_in_compilation_unit CUMap.t Lazy.t;
      references: name_references_in_compilation_unit CUMap.t Lazy.t;
    }

  (** Raised by {!retrieve_parsed_data}. *)
  exception Unparseable of Lsp.Types.DocumentUri.t
  exception Copybook of Lsp.Types.DocumentUri.t

  type cached =                   (** Persistent representation (for caching) *)
    {
      doc_cache_filename: string;   (* relative to project rootdir *)
      doc_cache_checksum: Digest.t; (* checked against file on disk on reload *)
      doc_cache_langid: string;
      doc_cache_version: int;
      doc_cache_pplog: Cobol_preproc.log;
      doc_cache_tokens: Cobol_parser.tokens_with_locs;
      doc_cache_parsed: (PTREE.compilation_group * CUs.t) option;
      doc_cache_diags: DIAGS.Set.serializable;
    }

end
include TYPES

type t = document
let uri { textdoc; _ } = Lsp.Text_document.documentUri textdoc

let parse ~project text =
  let uri = Lsp.Text_document.documentUri text in
  let libpath = Lsp_project.libpath_for ~uri project in
  Cobol_parser.parse_with_tokens
    (* Recovery policy for the parser: *)
    ~recovery:(EnableRecovery { silence_benign_recoveries = true })
    ~source_format:project.source_format
    ~config:project.cobol_config
    ~libpath
    (String { contents = Lsp.Text_document.text text;
              filename = Lsp.Uri.to_path uri })

let lazy_definitions ast cus =
  lazy begin cus |>
    CUs.assoc Lsp_lookup.definitions |>
    (*this piece for handling renames is temporary*)
    Lsp_lookup.add_rename_item_definitions ast |>
    Lsp_lookup.add_paragraph_definitions ast |>
    Lsp_lookup.add_redefine_definitions ast
  end

let lazy_references ast cus defs =
  lazy begin
    let defs = Lazy.force defs in
    try
      List.fold_left
        (fun map cu ->
           let cu_name = Lsp_lookup.name_of_compunit cu in
           let _, cu_defs = CUMap.find_by_name cu_name defs in
           CUMap.add
             (CUs.find_by_name cu_name cus)
             (Lsp_lookup.references cu_defs cu) map )
        CUMap.empty ast
    with Not_found -> CUMap.empty
  end

let analyze ({ project; textdoc; copybook; _ } as doc) =
  let pplog, tokens, (parsed, diags) =
    if copybook then
      Cobol_preproc.Trace.empty, lazy [], (None, DIAGS.Set.none)
    else
      let ptree = parse ~project textdoc in
      Cobol_parser.preproc_rev_log ptree,
      Cobol_parser.parsed_tokens ptree,
      match Cobol_typeck.analyze_compilation_group ptree with
      | Ok (cus, ast, diags) ->
          let definitions = lazy_definitions ast cus in
          let references = lazy_references ast cus definitions in
          Some { ast; cus; definitions; references}, diags
      | Error diags ->
          None, diags (* NB: no token if unrecoverable error (e.g, wrong
                         indicator) *)
  in
  { doc with pplog; tokens; diags; parsed }

(** Creates a record for a document that is not yet parsed or analyzed. *)
let blank ~project ?copybook textdoc =
  let copybook = match copybook with
    | Some p -> p
    | None -> Lsp_project.detect_copybook project
                ~uri:(Lsp.Text_document.documentUri textdoc)
  in
  {
    project;
    textdoc;
    pplog = Cobol_preproc.Trace.empty;
    tokens = lazy [];
    diags = DIAGS.Set.none;
    parsed = None;
    copybook;
  }

let position_encoding = `UTF8

let load ~project ?copybook doc =
  Lsp.Text_document.make ~position_encoding doc
  |> blank ~project ?copybook
  |> analyze

let update { project; textdoc; _ } changes =
  (* TODO: Make it not reparse everything when a change occurs. *)
  Lsp.Text_document.apply_content_changes textdoc changes
  |> blank ~project
  |> analyze

(** Raises {!Unparseable} in case the document cannot be parsed entierely. *)
let retrieve_parsed_data: document -> parsed_data = function
  | { parsed = Some p; _ } -> p
  | { copybook = false; _ } as doc -> raise @@ Unparseable (uri doc)
  | { copybook = true;  _ } as doc -> raise @@ Copybook (uri doc)

(** Caching utilities *)

let to_cache ({ project; textdoc; pplog; tokens; parsed; diags; _ } as doc) =
  {
    doc_cache_filename = Lsp_project.relative_path_for ~uri:(uri doc) project;
    doc_cache_checksum = Digest.string (Lsp.Text_document.text textdoc);
    doc_cache_langid = Lsp.Text_document.languageId textdoc;
    doc_cache_version = Lsp.Text_document.version textdoc;
    doc_cache_pplog = pplog;
    doc_cache_tokens = Lazy.force tokens;
    doc_cache_parsed = Option.map (fun { ast; cus; _ } -> ast, cus) parsed;
    doc_cache_diags = DIAGS.Set.apply_delayed_formatting diags;
  }

(* NB: Note this checks against the actual file on disk, which may be different
   from what a client sends upon opening. *)
(** Raises {!Failure} in case of bad checksum. *)
let of_cache ~project
    { doc_cache_filename = filename;
      doc_cache_checksum = checksum;
      doc_cache_langid = languageId;
      doc_cache_version = version;
      doc_cache_pplog = pplog;
      doc_cache_tokens = tokens;
      doc_cache_parsed = parsed;
      doc_cache_diags = diags } =
  let absolute_filename = Lsp_project.absolute_path_for ~filename project in
  if checksum <> Digest.file absolute_filename then
    failwith "Bad checksum"
  else
    let uri = Lsp.Uri.of_path absolute_filename
    and text = EzFile.read_file absolute_filename in
    let doc = Lsp.Types.DidOpenTextDocumentParams.create
        ~textDocument:(Lsp.Types.TextDocumentItem.create
                         ~languageId ~text ~uri ~version) in
    let doc = Lsp.Text_document.make ~position_encoding doc |> blank ~project in
    let parsed =
      Option.map
        (fun (ast, cus) ->
          let definitions = lazy_definitions ast cus in
          let references = lazy_references ast cus definitions in
          { ast; cus; definitions; references})
        parsed
    in
    let diags = DIAGS.Set.of_serializable diags in
    { doc with pplog; tokens = lazy tokens; parsed; diags }

(* --- *)

(** {2 Miscellaneous} *)

let () =
  Printexc.register_printer begin function
    | Unparseable uri ->
        Some (Pretty.to_string "Unable to parse document at %s" @@
              Lsp.Types.DocumentUri.to_string uri)
    | Copybook uri ->
        Some (Pretty.to_string "Not parsing copybook at %s" @@
              Lsp.Types.DocumentUri.to_string uri)
    | _ ->
        None
  end
