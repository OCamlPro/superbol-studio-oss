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

module Cobol_typeck = Cobol_typeck.OLD

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
      artifacts: Cobol_parser.Outputs.artifacts;
      parsed: parsed_data option;
      rewinder: rewinder option;
      (* Used for caching, when loading a cache file as the file is not reparsed,
         then diagnostics are not sent. *)
      diags: DIAGS.Set.t;
    }
  and parsed_data =
    {
      ptree: Cobol_ptree.compilation_group;
      cus: CUs.t;
      (* Extracted info: lazy to only ever retrieve what's relevant upon a first
         request. *)
      definitions: name_definitions_in_compilation_unit CUMap.t Lazy.t;
      references: name_references_in_compilation_unit CUMap.t Lazy.t;
    }
  and rewinder =
    (Cobol_ptree.compilation_group option,
     Cobol_common.Behaviors.eidetic) Cobol_parser.Outputs.output
      Cobol_parser.rewinder

  (** Raised by {!val:Document.retrieve_parsed_data}. *)
  exception Unparseable of Lsp.Types.DocumentUri.t
  exception Copybook of Lsp.Types.DocumentUri.t

  (** Raised by {!val:Document.load} and {!val:Document.update}; allows keeping
      consistent document contents. *)
  exception Internal_error of document * exn * Printexc.raw_backtrace

  type cached =                   (** Persistent representation (for caching) *)
    {
      doc_cache_filename: string;   (* relative to project rootdir *)
      doc_cache_checksum: Digest.t; (* checked against file on disk on reload *)
      doc_cache_langid: string;
      doc_cache_version: int;
      doc_cache_pplog: Cobol_preproc.Trace.log;
      doc_cache_tokens: Cobol_parser.Outputs.tokens_with_locs;
      doc_cache_comments: Cobol_preproc.Text.comments;
      doc_cache_ignored: Cobol_common.Srcloc.lexloc list;
      doc_cache_parsed: (Cobol_ptree.compilation_group * CUs.t) option;
      doc_cache_diags: DIAGS.Set.serializable;
    }

end
include TYPES

type t = document
let uri { textdoc; _ } = Lsp.Text_document.documentUri textdoc

let rewindable_parse ({ project; textdoc; _ } as doc) =
  Cobol_parser.rewindable_parse_with_artifacts
    ~options:Cobol_parser.Options.{
        default with
        recovery = EnableRecovery { silence_benign_recoveries = true };
        config = project.config.cobol_config;
      } @@
  Cobol_preproc.preprocessor
    ~options:Cobol_preproc.Options.{
        default with
        libpath = Lsp_project.libpath_for ~uri:(uri doc) project;
        config = project.config.cobol_config;
        source_format = project.config.source_format
      } @@
  String { contents = Lsp.Text_document.text textdoc;
           filename = Lsp.Uri.to_path (uri doc) }

let lazy_definitions ptree cus =
  lazy begin cus |>
    CUs.assoc Lsp_lookup.definitions |>
    (*this piece for handling renames is temporary*)
    Lsp_lookup.add_rename_item_definitions ptree |>
    Lsp_lookup.add_paragraph_definitions ptree |>
    Lsp_lookup.add_redefine_definitions ptree
  end

let lazy_references ptree cus defs =
  lazy begin
    let defs = Lazy.force defs in
    List.fold_left begin fun map cu ->
        let cu_name = Lsp_lookup.name_of_compunit cu in
        try
          let _, cu_defs = CUMap.find_by_name cu_name defs in
          CUMap.add
            (CUs.find_by_name cu_name cus)
          (Lsp_lookup.references cu_defs cu) map
        with Not_found -> map
    end CUMap.empty ptree.Cobol_ptree.compilation_units
  end

let no_artifacts =
  Cobol_parser.Outputs.{ tokens = lazy [];
                         pplog = Cobol_preproc.Trace.empty;
                         rev_comments = [];
                         rev_ignored = [] }

let gather_parsed_data ptree =
  Cobol_typeck.analyze_compilation_group ptree |>
  DIAGS.map_result ~f:begin function
    | cus, Some ptree ->
        let definitions = lazy_definitions ptree cus in
        let references = lazy_references ptree cus definitions in
        Some { ptree; cus; definitions; references}
    | _, None ->
        None
  end

let extract_parsed_infos doc ptree =
  let DIAGS.{ result = artifacts, rewinder, parsed; diags} =
    DIAGS.more_result ~f:begin fun (ptree, rewinder) ->
      gather_parsed_data ptree |>
      DIAGS.map_result ~f:begin fun parsed ->
        Cobol_parser.artifacts ptree, Some rewinder, parsed
      end
    end ptree
  in
  { doc with artifacts; rewinder; diags; parsed }

let parse_and_analyze ({ copybook; _ } as doc) =
  if copybook then                                                    (* skip *)
    { doc with artifacts = no_artifacts; rewinder = None; parsed = None }
  else
    extract_parsed_infos doc @@ rewindable_parse doc

let reparse_and_analyze ?position ({ copybook; rewinder; textdoc; _ } as doc) =
  match position, rewinder with
  | None, _ | _, None ->
      parse_and_analyze doc
  | _, Some _ when copybook ->                                         (* skip *)
      { doc with artifacts = no_artifacts; rewinder = None; parsed = None }
  | Some position, Some rewinder ->
      extract_parsed_infos doc @@
      Cobol_parser.rewind_and_parse rewinder ~position @@
      Cobol_preproc.reset_preprocessor_for_string @@
      Lsp.Text_document.text textdoc

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
    artifacts = no_artifacts;
    rewinder = None;
    diags = DIAGS.Set.none;
    parsed = None;
    copybook;
  }

let position_encoding = `UTF8

let load ~project ?copybook doc =
  let textdoc = Lsp.Text_document.make ~position_encoding doc in
  let doc = blank ~project ?copybook textdoc in
  try parse_and_analyze doc
  with e -> raise @@ Internal_error (doc, e, Printexc.get_raw_backtrace ())

let first_change_pos changes =
  let line, char =
    List.fold_left begin fun ((l, c) as acc) -> function
      | Lsp.Types.TextDocumentContentChangeEvent.{ range = None; _ } ->
          (0, 0)                                 (* meaning: full text change *)
      | { range = Some { start = { line; character }; _ }; _ }
        when line < l || line = l && character < c ->
          line, character
      | _ ->
          acc
    end Int.(max_int, max_int) changes      (* can |changes|=0 really happen? *)
  in
  Cobol_parser.Indexed { line; char }

let update ({ textdoc; _ } as doc) changes =
  let position = first_change_pos changes in
  let doc =
    { doc with
      textdoc = Lsp.Text_document.apply_content_changes textdoc changes }
  in
  try reparse_and_analyze ~position doc
  with e -> raise @@ Internal_error (doc, e, Printexc.get_raw_backtrace ())

(** Raises {!Unparseable} in case the document cannot be parsed entierely, or
    {!Copybook} in case the document is not a main program. *)
let retrieve_parsed_data: document -> parsed_data = function
  | { parsed = Some p; _ } -> p
  | { copybook = false; _ } as doc -> raise @@ Unparseable (uri doc)
  | { copybook = true;  _ } as doc -> raise @@ Copybook (uri doc)

(** Caching utilities *)

let to_cache ({ project; textdoc; parsed; diags;
                artifacts = { pplog; tokens;
                              rev_comments; rev_ignored; _ }; _ } as doc) =
  {
    doc_cache_filename = Lsp_project.relative_path_for ~uri:(uri doc) project;
    doc_cache_checksum = Digest.string (Lsp.Text_document.text textdoc);
    doc_cache_langid = Lsp.Text_document.languageId textdoc;
    doc_cache_version = Lsp.Text_document.version textdoc;
    doc_cache_pplog = pplog;
    doc_cache_tokens = Lazy.force tokens;
    doc_cache_comments = rev_comments;
    doc_cache_ignored = rev_ignored;
    doc_cache_parsed = Option.map (fun { ptree; cus; _ } -> ptree, cus) parsed;
    doc_cache_diags = DIAGS.Set.apply_delayed_formatting diags;
  }

(* NB: Note this checks against the actual file on disk, which may be different
   from what a client sends upon opening. *)
(** Raises [Failure] in case of bad checksum. *)
let of_cache ~project
    { doc_cache_filename = filename;
      doc_cache_checksum = checksum;
      doc_cache_langid = languageId;
      doc_cache_version = version;
      doc_cache_pplog = pplog;
      doc_cache_tokens = tokens;
      doc_cache_comments = rev_comments;
      doc_cache_ignored = rev_ignored;
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
        (fun (ptree, cus) ->
           let definitions = lazy_definitions ptree cus in
           let references = lazy_references ptree cus definitions in
           { ptree; cus; definitions; references })
        parsed
    in
    { doc with artifacts = { pplog; tokens = lazy tokens;
                             rev_comments; rev_ignored };
               diags = DIAGS.Set.of_serializable diags;
               parsed }

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
