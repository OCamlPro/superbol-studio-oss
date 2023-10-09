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
open Cobol_common.Diagnostics.TYPES
open Preproc_options

module DIAGS = Cobol_common.Diagnostics

(* --- *)

type input =
  | Filename of string
  | String of { contents: string; filename: string }
  | Channel of { contents: in_channel; filename: string }

let decide_source_format _input
  : Cobol_config.source_format_spec ->
    Src_format.any with_diags = function
  | SF result ->
      { result = Src_format.from_config result; diags = DIAGS.Set.none }
  | Auto ->
      { result = Src_format.from_config SFFixed;
        diags = DIAGS.(Acc.warn Set.none) "Source format `auto` is not supported \
                                           yet, using `fixed`" }

(* --- *)

type preprocessor =
  {
    buff: Text.t;
    reader: Src_reader.t;
    ppstate: Preproc_state.t;
    pplog: Preproc_trace.log;
    diags: DIAGS.diagnostics;
    persist: preprocessor_persist;
  }
and preprocessor_persist =
  (** the preprocessor state that does not change very often *)
  {
    pparser: (module Text_processor.PPPARSER);
    overlay_manager: (module Src_overlay.MANAGER);
    replacing: Preproc_directives.replacing with_loc list list;
    copybooks: Cobol_common.Srcloc.copylocs;              (* opened copybooks *)
    libpath: string list;
    verbose: bool;
    show_if_verbose: [`Txt | `Src] list;
  }

let diags { diags; reader; _ } = DIAGS.Set.union diags @@ Src_reader.diags reader
let add_diag lp d = { lp with diags = DIAGS.Set.cons d lp.diags }
let add_diags lp d = { lp with diags = DIAGS.Set.union d lp.diags }
let log { pplog; _ } = pplog
let source_format { reader; _ } = Src_reader.source_format reader
let position { reader; _ } = Src_reader.position reader
let comments { reader; _ } = Src_reader.comments reader
let newline_cnums { reader; _ } = Src_reader.newline_cnums reader

let with_reader lp reader =
  if lp.reader == reader then lp else { lp with reader }
(* let with_diags lp diags = *)
(*   if lp.diags == diags then lp else { lp with diags } *)
let with_buff lp buff =
  if lp.buff == buff then lp else { lp with buff }
let with_pplog lp pplog =
  if lp.pplog == pplog then lp else { lp with pplog }
let with_diags_n_pplog lp diags pplog =
  if lp.diags == diags && lp.pplog == pplog then lp else { lp with diags; pplog }
let with_buff_n_pplog lp buff pplog =
  if lp.buff == buff && lp.pplog == pplog then lp else { lp with buff; pplog }
let with_replacing lp replacing =
  { lp with persist = { lp.persist with replacing } }

let show tag { persist = { verbose; show_if_verbose; _ }; _ } =
  verbose && List.mem tag show_if_verbose

let make_reader ~source_format = function
  | Filename filename ->
      Src_reader.from_file ~source_format filename
  | String { contents; filename } ->
      Src_reader.from_string ~filename ~source_format contents
  | Channel { contents; filename } ->
      Src_reader.from_channel ~filename ~source_format contents

let preprocessor input = function
  | `WithOptions { libpath; verbose; source_format;
                   config = (module Config) } ->
      let module Om_name = struct let name = __MODULE__ end in
      let module Om = Src_overlay.New_manager (Om_name) in
      let module Pp = Preproc_grammar.Make (Config) (Om) in
      let { result = source_format; diags }
        = decide_source_format input source_format in
      {
        buff = [];
        reader = make_reader ~source_format input;
        ppstate = Preproc_state.initial;
        pplog = Preproc_trace.empty;
        diags;
        persist =
          {
            pparser = (module Pp);
            overlay_manager = (module Om);
            replacing = [];
            copybooks = Cobol_common.Srcloc.no_copy;
            libpath;
            verbose;
            show_if_verbose = [`Src];
          };
      }
  | `Fork ({ persist; _ } as from, copyloc, copybook) ->
      let source_format = Src_reader.source_format from.reader in
      {
        from with
        buff = [];
        reader = make_reader ~source_format input;
        persist =
          {
            persist with
            copybooks =
              Cobol_common.Srcloc.new_copy ~copyloc copybook persist.copybooks;
          };
      }

let reset_preprocessor ~restart ?new_position ({ reader; _ } as pp) input =
  {
    pp with reader = restart ?position:new_position input reader;
  }

(* --- *)

let apply_active_replacing { pplog; persist; _ } = match persist with
  | { replacing = r :: _; _ } -> Text_processor.apply_replacing OnPartText r pplog
  | _ -> fun text -> Ok (text, pplog)

let apply_active_replacing_full { pplog; persist; _ } = match persist with
  | { replacing = r :: _; _ } -> Text_processor.apply_replacing OnFullText r pplog
  | _ -> fun text -> text, pplog

(** [lookup_compiler_directive chunk] searches for a compiler-directive
    text-word (that starts with either `{v >> v}' or `{v $ v}') in the given
    chunk of source text.

    Returns [Ok (prefix, cdir_text)] if a compiler directive is recognised,
    where [cdir_text] is guaranteed to start with a compiler-directive word on a
    line [l] and terminates at the end [l]. *)
(* Note: {!Text_processor.next_source_chunk} never outputs compiler-directive
   text-words in positions other than the first two.  Such a chunk also
   terminates at the end of the source line as it cannot be continued (contrary
   to normal source lines). *)
let lookup_compiler_directive: Text.text -> _ = function
  |      t :: _ as text  when Text.cdirp t -> Ok ([ ], text)
  | p :: (t :: _ as text) when Text.cdirp t -> Ok ([p], text)
  | _ -> Error `NotCDir

(* --- *)

(** [next_chunk lp] reads the next chunk from [lp], handling lexical and
    compiler directives along the way.  It never returns an empty result: the
    output text always containts at least {!Eof}. *)
let rec next_chunk ({ reader; buff; _ } as lp) =
  match Src_reader.next_chunk reader with
  | reader, ([{ payload = Eof; _}] as eof) ->
      let text, pplog = apply_active_replacing_full lp (buff @ eof) in
      text, { lp with reader; pplog; buff = [] }
  | reader, text ->
      if show `Src lp then
        Pretty.error "Src: %a@." Text.pp_text text;
      match lookup_compiler_directive text with
      | Error `NotCDir ->
          preprocess_line { lp with reader; buff = [] } (buff @ text)
      | Ok ([], lexdir_text) ->
          next_chunk @@ on_lexing_directive { lp with reader } lexdir_text
      | Ok (text, lexdir_text) ->
          let lp = { lp with reader; buff = [] } in
          preprocess_line (on_lexing_directive lp lexdir_text) (buff @ text)

and on_lexing_directive ({ persist = { pparser = (module Pp);
                                       overlay_manager = om; _ };
                           reader; _ } as lp) lexdir_text =
  (* Here, [lexdir_text] is never empty as it's known to start with a compiler
     directive marker `>>` (or `$` for MF-style directives), so we should always
     have a loc: *)
  let supplier = Text_supplier.pptoks_of_text_supplier om lexdir_text in
  let loc = Option.get @@ Cobol_common.Srcloc.concat_locs lexdir_text in
  let parser = Pp.Incremental.lexing_directive (position lp) in
  match ~&(Pp.MenhirInterpreter.loop supplier parser) with
  | { result = Some Preproc_directives.LexDirSource sf as lexdir; diags } ->
      let pplog = Preproc_trace.new_lexdir ~loc ?lexdir lp.pplog in
      let lp = add_diags lp diags in
      let lp = with_pplog lp pplog in
      with_reader lp (Src_reader.with_source_format sf reader)
  | { result = None; diags } ->       (* valid lexdir with erroneous semantics *)
      let pplog = Preproc_trace.new_lexdir ~loc lp.pplog in
      let lp = with_pplog lp pplog in
      add_diags lp diags
  | exception Pp.Error ->
      DIAGS.Cont.kerror (add_diag lp) ~loc
        "Malformed@ or@ unknown@ compiler@ directive"

and preprocess_line lp srctext =
  match try_preproc lp srctext with
  | Ok (`CDirNone (lp, [])) ->    (* Never return empty: skip to next sentence *)
      next_chunk lp
  | Ok (`CDirNone (lp, text)) ->
      do_replacing lp text
  | Ok (`CopyDone (lp, srctext))
  | Ok (`ReplaceDone (lp, [], srctext))
  | Ok (`CDirDone (lp, srctext)) ->  (* Continue with next phrase, which may also
                                        be a compiler directive. *)
      preprocess_line lp srctext
  | Ok (`ReplaceDone (lp, text, srctext)) ->
      text, with_buff lp @@ Text.strip_eof srctext
  | Error (`MissingPeriod | `MissingText) ->
      next_chunk (with_buff lp srctext)

and do_replacing lp text =
  match apply_active_replacing lp text with
  | Ok (text, pplog) ->
      text, with_pplog lp pplog
  | Error (`MissingText ([], pplog, buff)) ->
      next_chunk (with_buff_n_pplog lp buff pplog)
  | Error (`MissingText (text, pplog, buff)) ->
      text, with_buff_n_pplog lp buff pplog

and try_preproc lp srctext =
  match Preproc_state.find_preproc_phrase ~prefix:`Rev lp.ppstate srctext with
  | Error (`MissingPeriod | `MissingText) as e -> e
  | Error `NoneFound -> Ok (`CDirNone (lp, srctext))
  | Ok (cdir, ppstate) -> Ok (process_preproc_phrase { lp with ppstate } cdir)

and process_preproc_phrase ({ persist = { pparser = (module Pp);
                                          overlay_manager = (module Om); _ };
                              _ } as lp) =
  let parse ~stmt parser phrase : _ result =
    Pp.MenhirInterpreter.loop_handle
      Result.ok
      (function
        | HandlingError env ->
            let loc = Om.join_limits @@ Pp.MenhirInterpreter.positions env in
            Error DIAGS.(Set.one @@
                         One.error ~loc "Malformed@ %s@ statement" stmt)
        | _ ->
            Pretty.failwith
              "Unexpected@ state@ of@ parser@ for@ %s@ statement" stmt)
      (Text_supplier.pptoks_of_text_supplier (module Om) phrase)
      (parser @@ position lp)
  in
  function
  | Copy { prefix = rev_prefix; phrase; suffix } ->
      Result.fold (parse ~stmt:"COPY" Pp.Incremental.copy_statement phrase)
        ~ok:(fun copy -> do_copy lp rev_prefix copy suffix)
        ~error:(fun diags -> `CopyDone (add_diags lp diags,
                                        List.rev_append rev_prefix suffix))
  | Replace { prefix = rev_prefix; phrase; suffix } ->
      Result.fold (parse ~stmt:"REPLACE" Pp.Incremental.replace_statement phrase)
        ~ok:(fun repl -> do_replace lp rev_prefix repl suffix)
        ~error:(fun diags -> `ReplaceDone (add_diags lp diags,
                                           List.rev rev_prefix, suffix))
  | Header (header, { prefix = rev_prefix; phrase; suffix }) ->
      let prefix = match header with
        | ControlDivision
        | IdentificationDivision ->
            (* keep phrases that are further syntax-checked by the parser, and
               used to perform dialect-related checks there. *)
            List.rev_append rev_prefix phrase
        | SubstitutionSection ->
            (* discard this phrase, which is not checked by the parser *)
            List.rev rev_prefix
      in
      `ReplaceDone (lp, prefix, suffix)

and do_copy lp rev_prefix copy suffix =
  let { result = CDirCopy { library; replacing; _ }; diags } = ~&copy in
  let lp = add_diags lp diags in
  let libtext, lp = read_lib lp ~@copy library in
  let libtext, pplog =
    Text_processor.apply_replacing OnFullText replacing lp.pplog libtext
  in
  let lp = with_pplog lp pplog in
  (* eprintf "Library text: %a@." pp_text libtext; *)
  let text = List.rev_append rev_prefix libtext @ suffix in
  `CopyDone (lp, text)

and do_replace lp rev_prefix repl suffix =
  let { payload = { result = repl; diags }; loc } = repl in
  let lp = add_diags lp diags in
  let prefix, pplog =
    (* NB: this applies the current replacing on all remaining text leading to
       the current replacing phrase ([rev_prefix]), so this assumes no replacing
       may be performed on text that starts before and terminates after the
       replacing phrase. *)
    apply_active_replacing_full lp @@ List.rev rev_prefix
  in
  let lp = with_pplog lp @@ Preproc_trace.new_replace ~loc pplog in
  let lp = match repl, lp.persist.replacing with
    | CDirReplace { replacing = repl; _ }, ([] as replacing)
    | CDirReplace { replacing = repl; also = false }, replacing ->
        with_replacing lp (repl :: replacing)
    | CDirReplace { replacing = repl; also = true }, (r :: _ as replacing) ->
        with_replacing lp ((repl @ r) :: replacing)
    | CDirReplaceOff _, []
    | CDirReplaceOff { last = false }, _ ->
        with_replacing lp []
    | CDirReplaceOff { last = true }, (_ :: replacing) ->
        with_replacing lp replacing
  in
  `ReplaceDone (lp, prefix, suffix)


and read_lib ({ persist = { libpath; copybooks; verbose; _ }; _ } as lp)
    loc { libname; cbkname } =
  let libpath = match ~&?cbkname with None -> libpath | Some (_, d) -> [d] in
  let text, diags, pplog = match Copybook.find_lib ~libpath ~&libname with
    | Ok filename when Cobol_common.Srcloc.mem_copy filename copybooks ->
        (* TODO: `note addendum *)
        [],
        DIAGS.Acc.error lp.diags ~loc "@[Cyclic@ COPY@ of@ `%s'@]" filename,
        Preproc_trace.cyclic_copy ~loc ~filename lp.pplog
    | Ok filename ->
        if verbose then
          Pretty.error "Reading library `%s'@." filename;
        let text, lp =             (* note: [lp] holds all prev and new diags *)
          full_text                (* likewise for pplog *)
            (preprocessor (Filename filename) (`Fork (lp, loc, filename)))
            ~postproc:(Cobol_common.Srcloc.copy_from ~filename ~copyloc:loc)
        in
        text, lp.diags, Preproc_trace.copy_done ~loc ~filename lp.pplog
    | Error lnf ->
        [],
        Copybook.lib_not_found_error (DIAGS.Acc.error lp.diags ~loc "%t") lnf,
        Preproc_trace.missing_copy ~loc ~info:lnf lp.pplog
  in
  text, with_diags_n_pplog lp diags pplog

and full_text ?(item = "library") ?postproc lp : Text.text * preprocessor =
  let eofp p = ~&p = Text.Eof in
  let rec aux acc lp =
    let text, lp = next_chunk lp in
    let text = match postproc with
      | None -> text
      | Some p -> List.(rev @@ rev_map p text)
    in
    if not (List.exists eofp text)
    then aux (text :: acc) lp
    else begin
      if lp.persist.verbose then
        Pretty.error "Reached end of %s@." item;
      List.(concat (rev (filter (fun p -> not(eofp p)) text :: acc))), lp
    end
  in
  aux [] lp

let next_chunk lp =
  let text, lp = next_chunk lp in
  if show `Txt lp then
    Pretty.error "Txt: %a@." Text.pp_text text;
  text, lp

(* Pre-processing *)

(** For now, pre-processor tokens are essentially the same tokens as the general
    compilation group tokens since we reuse the same parser module. *)
type pptoken = Preproc_tokens.token with_loc

[@@@warning "-34"]
type pptokens = pptoken list

let pp_pptoken ppf (t: pptoken) =
  let t = ~&t in
  try Pretty.string ppf (Hashtbl.find Src_lexer.keyword_of_pptoken t)
  with Not_found -> match t with
    | TEXT_WORD w -> Pretty.print ppf "TEXT_WORD[%s]" w
    | PSEUDO_TEXT t -> Text.pp_pseudotext ppf t
    | ALPHANUM (s, q) -> Pretty.print ppf "%a%s%a" Text.pp_quote q s Text.pp_quote q
    | NATLIT s -> Pretty.print ppf "N\"%s\"" s
    | BOOLIT s -> Pretty.print ppf "B\"%s\"" s
    | HEXLIT s -> Pretty.print ppf "X\"%s\"" s
    | NULLIT s -> Pretty.print ppf "Z\"%s\"" s
    | LPAR -> Pretty.char ppf '('
    | RPAR -> Pretty.char ppf ')'
    | PERIOD -> Pretty.char ppf '.'
    | EOL -> Pretty.string ppf "EOL"
(*
Here is an example of a case that is not matched:
(TRAILING|SUPPRESS|SOURCEFORMAT|REPLACING|REPLACE|PRINTING|OFF|OF|LEADING|
LAST|IS|IN|FREE|FORMAT|COPY|CDIR_SOURCE|CDIR_SET|BY|ALSO|ALPHANUM_PREFIX _)
*)
    | _ -> Pretty.string ppf "<unknown preprocessor token>"

[@@@warning "-32"]                                                  (* unused *)
let pp_pptokens: pptokens Pretty.printer =
  Pretty.list ~fopen:"@[" ~fclose:"@]" pp_pptoken

(* --- *)

let reset_preprocessor_for_string string ?new_position pp =
  let contents = match new_position with
    | Some Lexing.{ pos_cnum; _ } -> EzString.after string (pos_cnum - 1)
    | None -> string
  in
  reset_preprocessor ?new_position pp contents
    ~restart:Src_reader.restart_on_string

(* --- *)

let preprocessor ?(options = Preproc_options.default) input =
  preprocessor input (`WithOptions options)

(** Default pretty-printing formatter for {!lex_file}, {!lex_lib}, and
    {!preprocess_file}. *)
let default_oppf = Fmt.stdout

let lex_file ~source_format ?(ppf = default_oppf) =
  Cobol_common.do_unit begin fun (module DIAGS) input ->
    let source_format =
      DIAGS.grab_diags @@ decide_source_format input source_format in
    Src_reader.print_lines ppf (make_reader ~source_format input)
  end

let lex_lib ~source_format ~libpath ?(ppf = default_oppf) =
  Cobol_common.do_unit begin fun (module DIAGS) libname ->
    match Copybook.find_lib ~libpath libname with
    | Ok filename ->
        let source_format =
          DIAGS.grab_diags @@
          decide_source_format (Filename filename) source_format in
        Src_reader.print_lines ppf @@
        Src_reader.from_file ~source_format filename
    | Error lnf ->
        Copybook.lib_not_found_error (DIAGS.error "%t") lnf
  end

let fold_source_lines ~source_format ?epf ~f =
  Cobol_common.do_any ?epf begin fun (module DIAGS) input ->
    let source_format =
      DIAGS.grab_diags @@ decide_source_format input source_format in
    Src_reader.fold_lines (make_reader ~source_format input) ~f
  end

let pp_preprocessed ppf lp =
  Pretty.print ppf "%a@." Text.pp_text (fst @@ full_text ~item:"file" lp)

let preprocess_file ?options ?(ppf = default_oppf) =
  Cobol_common.do_unit begin fun _init_diags filename ->
    pp_preprocessed ppf @@ preprocessor ?options (Filename filename)
  end

let text_of_input ?options ?epf a =
  Cobol_common.do_any begin fun _init_diags input ->
    fst @@
    full_text ~item:"file" @@
    preprocessor ?options input
  end ?epf a

let text_of_file ?options ?epf filename =
  text_of_input ?options ?epf (Filename filename)
