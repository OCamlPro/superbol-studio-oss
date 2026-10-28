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
open Lsp_types

module DIAGS = Cobol_common.Diagnostics
module ANALYSIS = Superbol_project.Analysis

module TYPES = struct

  type document =
    {
      project: Lsp_project.t;
      textdoc: Lsp.Text_document.t;
      copybook: bool;
      artifacts: Cobol_parser.Outputs.artifacts;
      checked: checked_doc option;
      rewinder: rewinder option;
      (* Records what preprocessing observed of the outside world -- copybooks
         read, directories searched, environment variables consulted -- so that
         caching this document can be made conditional on all of it, and not
         merely on the document's own contents.  [None] on a copybook, and on a
         document that was restored from cache rather than parsed: the analysis
         of the latter must not be cached again, or the entry would be written
         back depending on nothing at all and would then match whatever its
         copybooks had become. *)
      recorder: Superbol_cache.Recorder.t option;
      (* Used for caching, when loading a cache file as the file is not
         reparsed, then diagnostics are not sent. *)
      parsing_diags: Cobol_parser.Diagnostics.ALL.t;
      typecking_diags: Cobol_typeck.Diagnostics.t;
    }
  and checked_doc = Cobol_typeck.Outputs.t
  and rewinder =
    (Cobol_ptree.compilation_group,
     Cobol_common.Behaviors.eidetic) Cobol_parser.rewinder

  (** Raised by {!val:Document.checked}. *)
  exception Unparseable of Lsp.Types.DocumentUri.t
  exception Copybook of Lsp.Types.DocumentUri.t

  (** Raised by {!val:Document.load} and {!val:Document.update}; allows keeping
      consistent document contents. *)
  exception Internal_error of document * exn * Printexc.raw_backtrace

  (** Persistent representation of LSP-specific data (for caching).  The
      analysis results are cached separately, as a
      {!Superbol_project.Analysis.t}, so that they can be shared with other
      front-ends. *)
  type cached =
    {
      doc_cache_filename: string;   (* relative to project rootdir *)
      (* No checksum of the document itself: its contents take part in the key
         this record is stored under, so a change to them yields a different
         entry rather than one that has to be rejected. *)
    }

end
include TYPES

type t = document
let uri { textdoc; _ } = Lsp.Text_document.documentUri textdoc
let language_id { textdoc; _ } = Lsp.Text_document.languageId textdoc

(** [analysis_overrides ~language_id] are the departures from their project's
    configuration with which documents with the given language identifier are to
    be analyzed. *)
let analysis_overrides ~language_id =
  match language_id with
  | "COBOL_GNU_LISTFILE"
  | "COBOL_GNU_DUMPFILE" ->
      { ANALYSIS.no_overrides with
        override_source_format = Some (Cobol_config.SF SFVariable) }
  | _ ->
      ANALYSIS.no_overrides

let analysis_overrides_for doc =
  analysis_overrides ~language_id:(language_id doc)

let rewindable_parse ~platform ({ project; textdoc; _ } as doc) =
  let filename = Lsp.Uri.to_path (uri doc) in
  let overrides = analysis_overrides_for doc in
  Cobol_parser.rewindable_parse_with_artifacts
    ~options:(ANALYSIS.parser_options ~overrides project) @@
  Cobol_preproc.preprocessor
    ~options:(ANALYSIS.preproc_options ~platform ~overrides ~filename
                project) @@
  Cobol_preproc.Input.string ~filename @@
  Lsp.Text_document.text textdoc

let no_artifacts =
  Cobol_parser.Outputs.{ tokens = lazy [];
                         pplog = Cobol_preproc.Trace.empty;
                         rev_comments = StringMap.empty;
                         rev_ignored = [] }

let check doc ptree =
  let Cobol_parser.Outputs.{ result = ptree, rewinder;
                             diags = parsing_diags } = ptree in
  let checked, typecking_diags =
    ANALYSIS.typecheck ~parsing_diags doc.project ptree
      ~overrides:(analysis_overrides_for doc)
  in
  let artifacts = Cobol_parser.artifacts ptree in
  { doc with artifacts;
             parsing_diags;
             typecking_diags;
             rewinder = Some rewinder;
             checked = Some checked; }

let parse_and_analyze ({ copybook; _ } as doc) =
  if copybook then                                                    (* skip *)
    { doc with artifacts = no_artifacts; rewinder = None; checked = None;
               recorder = None }
  else
    (* The recorder outlives this parse: a later incremental reparse runs on the
       preprocessor state captured in the rewinder, and so keeps reporting to
       this same recorder.  Witnesses only ever accumulate, which costs a
       needless miss at worst and never a wrong hit. *)
    let recorder = Superbol_cache.Recorder.create ~platform:Lsp_platform.record in
    let doc =
      check doc @@
      rewindable_parse ~platform:(Superbol_cache.Recorder.platform recorder) doc
    in
    { doc with recorder = Some recorder }

let reparse_and_analyze ?position ({ copybook; rewinder; textdoc; _ } as doc) =
  match position, rewinder with
  | None, _ | _, None ->
      parse_and_analyze doc
  | _, Some _ when copybook ->                                         (* skip *)
      { doc with artifacts = no_artifacts; rewinder = None; checked = None;
                 recorder = None }
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
    ANALYSIS.detect_copybook project ~platform:Lsp_platform.record
      ~filename:(Lsp.Uri.to_path uri)
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
    recorder = None;
    copybook;
  }

let load ~project ~params doc =
  let textdoc =
    Lsp.Text_document.make doc ~position_encoding:params.position_encoding
  in
  let doc = blank ~project textdoc in
  try parse_and_analyze doc
  with e -> raise @@ Internal_error (doc, e, Printexc.get_raw_backtrace ())

let unload doc =
  Option.iter Cobol_parser.forget doc.rewinder

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

(** [to_cache doc] splits [doc] into its analysis results, the witnesses these
    were obtained against, and LSP-specific data.  The witnesses are [None] on a
    document whose analysis was restored from cache rather than performed: there
    is then nothing to store it against, and the entry it was restored from is
    there already anyway. *)
let to_cache ({ project; copybook; checked; parsing_diags; typecking_diags;
                recorder; artifacts; _ } as doc) =
  let analysis =
    if copybook
    then ANALYSIS.copybook
    else ANALYSIS.of_artifacts ~checked ~parsing_diags
        ~typeck_diags:typecking_diags ~parsing_artifacts:artifacts
  in
  let witnesses =
    match recorder with
    | Some recorder ->
        (* An incremental reparse keeps reporting to the recorder, so it holds
           strictly more than what was known when the parse first finished. *)
        Some (Superbol_cache.Recorder.witnesses recorder)
    | None when copybook ->
        Some []        (* detection depends on nothing beyond the key's inputs *)
    | None ->
        None
  in
  analysis, witnesses,
  { doc_cache_filename = Lsp_project.relative_path_for ~uri:(uri doc) project }

(** [of_cache ~project ~params ~doc analysis cached] rebuilds a document from
    the client's [doc], and its cached analysis [analysis].

    The caller has already established that the contents of [doc] are what
    [analysis] was recorded against: the contents take part in the key the entry
    is stored under.  Everything {e else} the analysis depended upon is checked
    by the cache, against the witnesses it was stored with. *)
let of_cache ~project ~params ~doc (analysis: ANALYSIS.t)
    (_: cached option) =                      (* nothing to restore from it yet *)
  let textdoc =
    Lsp.Text_document.make doc ~position_encoding:params.position_encoding
  in
  {
    project;
    textdoc;
    copybook = analysis.copybook;
    artifacts = ANALYSIS.artifacts analysis;
    rewinder = None;
    parsing_diags = analysis.parsing_diags;
    typecking_diags = analysis.typeck_diags;
    checked = analysis.checked;
    recorder = None;
  }

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
