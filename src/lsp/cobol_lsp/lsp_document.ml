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
      checked: checked_doc option;
      rewinder: rewinder option;
      (* Used for caching, when loading a cache file as the file is not
         reparsed, then diagnostics are not sent. *)
      parsing_diags: Cobol_parser.Diagnostics.ALL.t;
      typecking_diags: Cobol_typeck.Diagnostics.t;
    }
  and checked_doc = Cobol_typeck.Outputs.t
  and rewinder =
    (Cobol_ptree.compilation_group option,
     Cobol_common.Behaviors.eidetic) Cobol_parser.Outputs.output
      Cobol_parser.rewinder

  (** Raised by {!val:Document.checked}. *)
  exception Unparseable of Lsp.Types.DocumentUri.t
  exception Copybook of Lsp.Types.DocumentUri.t

  (** Raised by {!val:Document.load} and {!val:Document.update}; allows keeping
      consistent document contents. *)
  exception Internal_error of document * exn * Printexc.raw_backtrace

  (** Persistent representation (for caching) *)
  type cached =
    {
      doc_cache_filename: string;   (* relative to project rootdir *)
      doc_cache_checksum: Digest.t; (* checked against file on disk on reload *)
      doc_cache_langid: string;
      doc_cache_version: int;
      doc_cache_pplog: Cobol_preproc.Trace.log;
      doc_cache_tokens: Cobol_parser.Outputs.tokens_with_locs;
      doc_cache_comments: Cobol_preproc.Text.comments;
      doc_cache_ignored: Cobol_common.Srcloc.lexloc list;
      doc_cache_checked: checked_doc option;
      doc_cache_parsing_diags: Cobol_parser.Diagnostics.ALL.t;
      doc_cache_typecking_diags: Cobol_typeck.Diagnostics.t;
    }

end
include TYPES

type t = document
let uri { textdoc; _ } = Lsp.Text_document.documentUri textdoc
let language_id { textdoc; _ } = Lsp.Text_document.languageId textdoc

let rewindable_parse ({ project; textdoc; _ } as doc) =
  Cobol_parser.rewindable_parse_with_artifacts
    ~options:Cobol_parser.Options.{
        (default ~exec_scanners: Superbol_preprocs.exec_scanners) with
        recovery = EnableRecovery { silence_benign_recoveries = true };
        config = project.config.cobol_config;
      } @@
  Cobol_preproc.preprocessor
    ~options:Cobol_preproc.Options.{
        default with
        copybook_lookup_config =
          Lsp_project.copybook_lookup_config_for ~uri:(uri doc) project;
        config = project.config.cobol_config;
        source_format = match language_id doc with
          | "COBOL_GNU_LISTFILE"
          | "COBOL_GNU_DUMPFILE" -> SF SFVariable
          | _ -> project.config.source_format
      } @@
  String { contents = Lsp.Text_document.text textdoc;
           filename = Lsp.Uri.to_path (uri doc) }

let no_artifacts =
  Cobol_parser.Outputs.{ tokens = lazy [];
                         pplog = Cobol_preproc.Trace.empty;
                         rev_comments = [];
                         rev_ignored = [] }

let check doc ptree =
  let config = doc.project.config.cobol_config in
  let Cobol_parser.Outputs.{ result = ptree, rewinder;
                             diags = parsing_diags } = ptree in
  let Cobol_typeck.Results.{ result = checked;
                             diags = typecking_diags }
    = Cobol_typeck.compilation_group ~config ptree in
  let artifacts = Cobol_parser.artifacts ptree in
  let typecking_diags =
    (* Do not report typeck diagnostics in case of syntax or pre-processing
       errors: *)
    if Cobol_parser.Diagnostics.ALL.has_errors parsing_diags
    then Cobol_typeck.Diagnostics.none
    else typecking_diags
  in
  { doc with artifacts;
             parsing_diags;
             typecking_diags;
             rewinder = Some rewinder;
             checked = Some checked; }

let parse_and_analyze ({ copybook; _ } as doc) =
  if copybook then                                                    (* skip *)
    { doc with artifacts = no_artifacts; rewinder = None; checked = None }
  else
    check doc @@ rewindable_parse doc

let reparse_and_analyze ?position ({ copybook; rewinder; textdoc; _ } as doc) =
  match position, rewinder with
  | None, _ | _, None ->
      parse_and_analyze doc
  | _, Some _ when copybook ->                                         (* skip *)
      { doc with artifacts = no_artifacts; rewinder = None; checked = None }
  | Some position, Some rewinder ->
      check doc @@
      Cobol_parser.rewind_and_parse rewinder ~position @@ fun ~last_pp:_ ->
      Cobol_preproc.reset_preprocessor_for_string @@
      Lsp.Text_document.text textdoc

(** [inspect_at ~position ~f doc] passes to [f] the state that is reached by the
    parser at [position] in [doc].  Returns [None] on copybooks, or [Some r] for
    [r] the result of [f]. *)
let rec inspect_at ~position ~f ({ copybook; rewinder; textdoc; _ } as doc) =
  match rewinder with
  | None | Some _ when copybook ->                                     (* skip *)
      None
  | None ->
      inspect_at ~position ~f @@ parse_and_analyze doc
  | Some rewinder ->
      let Lsp.Types.Position.{ line; character = char } = position in
      let exception FAILURE in
      let preproc_rewind ~last_pp =
        (* cut parsed program at given position *)
        let pos_cnum =
          try (Cobol_preproc.position_at ~line ~char last_pp).pos_cnum
          with Not_found when line = 0 -> char
             | Not_found -> raise FAILURE
        in
        let input = Lsp.Text_document.text textdoc in
        Cobol_preproc.reset_preprocessor_for_string @@
        String.sub input 0 pos_cnum
      in
      try
        Option.some @@
        Cobol_parser.rewind_for_inspection rewinder preproc_rewind
          ~position:(Indexed { line; char })
          ~inspect:f
      with FAILURE ->
        None

(** Creates a record for a document that is not yet parsed or analyzed. *)
let blank ~project textdoc =
  let uri = Lsp.Text_document.documentUri textdoc in
  let copybook =
    Lsp_project.detect_copybook project ~uri
      ~contents:(Lsp.Text_document.text textdoc)
  in
  if copybook then
    Lsp_io.log_debug "%s appears to be a copybook" (Lsp.Uri.to_string uri);
  {
    project;
    textdoc;
    artifacts = no_artifacts;
    rewinder = None;
    parsing_diags = Cobol_parser.Diagnostics.ALL.none;
    typecking_diags = Cobol_typeck.Diagnostics.none;
    checked = None;
    copybook;
  }

let position_encoding = `UTF8

let load ~project doc =
  let textdoc = Lsp.Text_document.make ~position_encoding doc in
  let doc = blank ~project textdoc in
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

let reload doc =
  try reparse_and_analyze doc
  with e -> raise @@ Internal_error (doc, e, Printexc.get_raw_backtrace ())

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
let checked: document -> checked_doc = function
  | { checked = Some p; _ } -> p
  | { copybook = false; _ } as doc -> raise @@ Unparseable (uri doc)
  | { copybook = true;  _ } as doc -> raise @@ Copybook (uri doc)

let diagnostics { parsing_diags; typecking_diags; _ } =
  DIAGS.Set.union
    (Cobol_parser.Diagnostics.ALL.translate parsing_diags)
    (Cobol_typeck.Diagnostics.translate typecking_diags)

(** Caching utilities *)

let to_cache ({ project; textdoc; checked; parsing_diags; typecking_diags;
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
    doc_cache_checked = checked;
    doc_cache_parsing_diags = parsing_diags;
    doc_cache_typecking_diags = typecking_diags;
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
      doc_cache_checked = checked;
      doc_cache_parsing_diags = parsing_diags;
      doc_cache_typecking_diags = typecking_diags } =
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
    { doc with artifacts = { pplog; tokens = lazy tokens;
                             rev_comments; rev_ignored };
               parsing_diags;
               typecking_diags;
               checked }

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
