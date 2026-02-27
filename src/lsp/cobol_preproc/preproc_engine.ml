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
open Cobol_common.Platform.TYPES
open Cobol_common.Srcloc.INFIX
open Preproc_outputs.TYPES
open Preproc_options

module OUT = Preproc_outputs
module ENV = Preproc_env

(* --- *)

type preprocessor =
  {
    buff: Text.t;
    reader: Src_reader.t;
    ppstate: Preproc_state.t;
    pplog: Preproc_trace.log;
    diags: Preproc_diagnostics.t;
    env: Preproc_env.t;
    context: Preproc_logic.context;
    rev_ignored: Text.t;     (* text accumulated when not emitting (reversed) *)
    persist: preprocessor_persist;
  }

(** the preprocessor state that does not change very often *)
and preprocessor_persist =
  {
    pparser: (module Text_processor.PPPARSER);
    overlay_manager: (module Src_overlay.MANAGER);
    replacing: Preproc_directives.replacing with_loc list list;
    copybooks: Cobol_common.Srcloc.copylocs;              (* opened copybooks *)
    dialect: Cobol_config.dialect;
    source_format: Src_format.any option;  (* to keep auto-detecting on reset *)
    exec_preprocs: exec_preprocessor EXEC_MAP.t;
    copybook_lookup_config: Cobol_common.Copybook.lookup_config;
    platform: Cobol_common.Platform.TYPES.platform;
    show_if_verbose: [`Txt | `Src] list;
  }

let diags { diags; reader; _ } =
  Preproc_diagnostics.add_src_diagnostics (Src_reader.diags reader) diags

let add_diags lp d =
  if d == Preproc_diagnostics.none
  then lp
  else { lp with diags = Preproc_diagnostics.union d lp.diags }
let add_error lp e =
  { lp with diags = Preproc_diagnostics.add_error e lp.diags }
let add_warn lp w =
  { lp with diags = Preproc_diagnostics.add_warning w lp.diags }

let position { reader; _ } = Src_reader.position reader
let input_file { reader; _ } = Src_reader.input_file reader
let source_format { reader; _ } = Src_reader.source_format reader
let rev_log { pplog; _ } = pplog
let rev_comments { reader; _ } = Src_reader.rev_comments reader
let rev_ignored { reader; _ } = Src_reader.rev_ignored reader

(** [position_at ~line ~char pp] computes a lexing position that corresponds to
    the given line and character indexes (all starting at 0) in the input
    already read by [pp].  Raises [Not_found] if no complete line was processed
    yet, or the current position if the given line index does not correspond to
    an already processed line. *)
let position_at ~line ~char { reader; _ } =
  let rev_newline_cnums = Src_reader.rev_newline_cnums reader in
  if rev_newline_cnums = []                                (* no newline seen *)
  then raise Not_found
  else     (* |rev_newline_cnums| is the number of newline chars seen upto... *)
    let lexpos = Src_reader.position reader in       (* ... current position. *)
    if line <= lexpos.pos_lnum - 1                   (* = |rev_newline_cnums| *)
    then
      let pos_bol =
        if line <= 0
        then 0
        else List.nth rev_newline_cnums (lexpos.pos_lnum - 1 - line)
      in
      { lexpos with pos_bol;
                    pos_cnum = pos_bol + char;
                    pos_lnum = line + 1 }
    else
      lexpos                                           (* (line not seen yet) *)

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

let show tag { persist = { platform; show_if_verbose; _ }; _ } =
  platform.verbosity>0 && List.mem tag show_if_verbose

let source_format_config = function
  | Cobol_config.SF sf -> Some (Src_format.from_config sf)
  | Auto -> None

let preprocessor input = function
  | `WithOptions { platform; source_format; env;
                   exec_preprocs; config = (module Config);
                   copybook_lookup_config } ->
      let module Om_name = struct let name = __MODULE__ end in
      let module Om = Src_overlay.New_manager (Om_name) () in
      let module Pp = Preproc_grammar.Make (Config) (Om) in
      let source_format = source_format_config source_format in
      {
        buff = [];
        reader = Src_reader.from input ?source_format;
        ppstate = Preproc_state.initial;
        pplog = Preproc_trace.empty;
        diags = Preproc_diagnostics.none;
        env;
        context = Preproc_logic.empty_context;
        rev_ignored = [];
        persist =
          {
            pparser = (module Pp);
            overlay_manager = (module Om);
            replacing = [];
            copybooks = Cobol_common.Srcloc.no_copy;
            dialect = Config.dialect;
            source_format;
            exec_preprocs;
            copybook_lookup_config;
            platform;
            show_if_verbose = [`Src];
          };
      }
  | `Fork ({ persist; _ } as from, copyloc, copybook) ->
      let source_format = Src_reader.source_format from.reader in
      {
        from with
        buff = [];
        reader = Src_reader.from input ~source_format;
        rev_ignored = [];
        (* CHECKME: context and ignored? *)
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

(* --- *)

(** [next_chunk lp] reads the next chunk from [lp], handling lexical and
    compiler directives along the way.  It never returns an empty result: the
    output text always containts at least {!Eof}. *)
let rec next_chunk ({ reader; buff; persist = { dialect; _ }; _ } as lp) =
  match Src_reader.next_chunk reader with
  | reader, ([{ payload = Eof; loc }] as eof) ->
      let context, diags = Preproc_logic.flush_contexts ~loc lp.context in
      let lp = add_diags { lp with context } diags in
      let text, pplog = apply_active_replacing_full lp (buff @ eof) in
      text, { lp with reader; pplog; buff = [] }
  | reader, text ->
      if show `Src lp then
        Pretty.error "Src: %a@." Text.pp_text text;
      let emitting = Preproc_logic.emitting lp.context in
      match Src_reader.try_compiler_directive ~dialect text with
      | None when not emitting ->                              (* ignore text *)
          let rev_ignored = List.rev_append text lp.rev_ignored in
          next_chunk { lp with reader; rev_ignored }
      | None ->
          preprocess_line { lp with reader; buff = [] } (buff @ text)
      | Some ([], compdirs, _compdir_text, diags) ->
          let lp = add_diags { lp with reader } diags in
          next_chunk (apply_compiler_directives lp compdirs)
      | Some (text, compdirs, _compdir_text, diags) when not emitting ->
          let rev_ignored = List.rev_append text lp.rev_ignored in
          let lp = add_diags { lp with reader; rev_ignored } diags in
          next_chunk (apply_compiler_directives lp compdirs)     (* ignore text *)
      | Some (text, compdirs, _compdir_text, diags) ->
          let lp = add_diags { lp with reader; buff = [] } diags in
          preprocess_line (apply_compiler_directives lp compdirs) (buff @ text)

and apply_compiler_directives lp compdirs =
  List.fold_left apply_compiler_directive lp compdirs

and apply_compiler_directive ({ reader; pplog; _ } as lp)
    { payload = compdir; loc } =
  let lp = with_pplog lp @@ Preproc_trace.new_compdir ~loc ~compdir pplog in
  match compdir with
  | Preproc_directives.CDir_source_format sf ->
      (match Src_reader.with_source_format sf reader with
       | Ok reader -> with_reader lp reader
       | Error e -> add_error lp e)
  | CDir_control_section ->                              (* nothing to do here *)
      lp
  | CDir_preproc preproc_directive ->
      apply_preproc_directive lp (preproc_directive &@ loc)

and apply_preproc_directive ({ env; context; _ } as lp)
    { payload = ppdir; loc } =
  let new_env lp { result = env; diags } =
    if env != lp.env || diags != Preproc_diagnostics.none
    then { lp with env; diags = Preproc_diagnostics.union diags lp.diags }
    else lp
  and new_context lp { result = context; diags } =
    if context != lp.context || diags != Preproc_diagnostics.none
    then { lp with context; diags = Preproc_diagnostics.union diags lp.diags }
    else lp
  in
  match ppdir with
  | Define _ | Define_off _ | Set _
    when not (Preproc_logic.emitting lp.context) ->
      lp                                                            (* ignore *)
  | Define def ->
      let platform = lp.persist.platform in
      new_env lp @@ Preproc_logic.on_define ~platform ~loc def ~env
  | Define_off var ->
      new_env lp @@ Preproc_logic.on_define_off ~loc var ~env
  | If condition ->
      new_context lp @@ Preproc_logic.on_if ~loc ~condition ~env context
  | Elif condition ->
      new_context lp @@ Preproc_logic.on_elif ~loc ~condition ~env context
  | Else ->
      new_context lp @@ Preproc_logic.on_else ~loc context
  | End
  | End_if ->
      let lp = new_context lp @@ Preproc_logic.on_endif ~loc context in
      if Preproc_logic.emitting lp.context && lp.rev_ignored <> []
      then { lp with
             rev_ignored = [];
             pplog = Preproc_trace.ignored (List.rev lp.rev_ignored) lp.pplog }
      else lp
  | Set _ ->
      add_warn lp @@ Ignored { loc; item = Compiler_directive }

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
            Error (Preproc_diagnostics.Malformed
                     { loc; stuff = Preproc_statement stmt })
        | _ ->
            Pretty.failwith
              "Unexpected@ state@ of@ parser@ for@ %a@ statement"
              Preproc_diagnostics.pp_preproc_statement stmt)
      (Src_tokenizer.pptoks_of_text_supplier (module Om) phrase)
      (parser @@ position lp)
  in
  function
  | Copy { prefix = rev_prefix; phrase; suffix } ->
      Result.fold (parse ~stmt:`COPY Pp.Incremental.copy_statement phrase)
        ~ok:(fun copy -> do_copy lp rev_prefix copy suffix)
        ~error:(fun e -> `CopyDone (add_error lp e,
                                    List.rev_append rev_prefix suffix))
  | Replace { prefix = rev_prefix; phrase; suffix } ->
      Result.fold (parse ~stmt:`REPLACE Pp.Incremental.replace_statement phrase)
        ~ok:(fun repl -> do_replace lp rev_prefix repl suffix)
        ~error:(fun e -> `ReplaceDone (add_error lp e,
                                       List.rev rev_prefix, suffix))
  | Header (header, { prefix = rev_prefix; phrase; suffix }) ->
      let prefix, lp = match header with
        | ControlDivision
        | IdentificationDivision ->
            (* keep phrases that are further syntax-checked by the parser, and
               used to perform dialect-related checks there. *)
            List.rev_append rev_prefix phrase, lp
        | SubstitutionSection ->
            (* discard this phrase, which is not checked by the parser; keep it
               in pplog anyways, so as to keep its location for later use. *)
            let loc = Option.get @@ Cobol_common.Srcloc.concat_locs phrase in
            let section = Preproc_directives.CDir_control_section &@ loc in
            List.rev rev_prefix, apply_compiler_directive lp section
      in
      `ReplaceDone (lp, prefix, suffix)
  | ExecBlock { prefix = rev_prefix; phrase; suffix } ->
      do_exec lp rev_prefix phrase suffix
  | ExecBlockPrefix { prefix = rev_prefix; phrase; suffix } ->
      do_exec ~partial:true lp rev_prefix phrase suffix


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


and do_exec ?(partial = false) lp rev_prefix exec_block suffix =
  (* Note: `exec_block` must be non-empty; it should at least start with an
     `EXEC(UTE)` text word (not checked here). *)
  (* CHECKME: Assumes pre-processed EXEC blocks are NOT subject to
     replacement... *)
  let loc = Option.get @@ Cobol_common.Srcloc.concat_locs exec_block in
  let lp =
    if partial
    then add_error lp @@ Unterminated { loc; stuff = Exec_block }
    else lp
  in
  let emit lp exec_block =
    let lp =
      with_pplog lp @@ Preproc_trace.exec_block exec_block lp.pplog
        ~preamble_loc: ~@(List.hd exec_block)
        ?postamble_loc: (if partial then None
                         else Some ~@(EzList.last exec_block))
    in
    let block = Text.ExecBlock exec_block &@ loc in
    `ReplaceDone (lp, List.rev (block :: rev_prefix), suffix)
  in
  let error e =
    let lp = add_error lp e in
    (* Emit anyways (maybe we'll need to add a vailidity flag in `ExecBlock`) *)
    emit lp exec_block
  in
  match List.tl exec_block with                             (* skip EXEC(UTE) *)
  | { payload = Text.TextWord lang; _ } :: _ :: _ ->         (* avoid empty tail *)
      (match EXEC_MAP.find_opt lang lp.persist.exec_preprocs with
       | Some Text_preprocessor f ->
           (* TODO: check whether the relevant compiler directive/option has
              been set? *)
           emit lp (f exec_block)
       | None ->
           emit lp exec_block)           (* would a warning be relevant here? *)
  | { payload = Alphanum _ | AlphanumPrefix _; loc } :: _ ->
      error @@ Unexpected { loc; stuff = Alphanumeric_literal }
  | { payload = Pseudo _; loc } :: _ ->
      error @@ Unexpected { loc; stuff = Pseudotext }
  | _ when not partial ->
      error @@ Malformed { loc; stuff = Preproc_statement `EXEC_BLOCK }
  | _ ->
      emit lp exec_block                                  (* already reported *)


and read_lib ({ persist = { copybook_lookup_config;
                            copybooks; platform; _ }; _ } as lp)
    loc { txtname; libname } =
  let text, diags, pplog =
    match
      platform.find_lib ~&txtname ?libname:~&?libname
        ?fromfile:(input_file lp) ~lookup_config:copybook_lookup_config
    with
    | Ok filename when Cobol_common.Srcloc.mem_copy filename copybooks ->
        [],
        Preproc_diagnostics.add_error
          (Cyclic_copy { copyloc = loc; filename }) lp.diags,
        Preproc_trace.cyclic_copy ~loc ~filename lp.pplog
    | Ok filename ->
        if platform.verbosity>0 then
          platform.error "Reading library `%s'@." filename;
        let text, lp =             (* note: [lp] holds all prev and new diags *)
          Src_input.from ~platform ~filename ~f:begin fun input ->
            full_text                                   (* likewise for pplog *)
              (preprocessor input (`Fork (lp, loc, filename)))
              ~postproc:(Cobol_common.Srcloc.copy_from ~filename ~copyloc:loc)
          end
        in
        text, lp.diags, Preproc_trace.copy_done ~loc ~filename lp.pplog
    | Error lnf ->
        [],
        Preproc_diagnostics.add_error
          (Copybook_lookup_error { copyloc = Some loc; lnf }) lp.diags,
        Preproc_trace.missing_copy ~loc ~error:lnf lp.pplog
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
      if lp.persist.platform.verbosity>0 then
        lp.persist.platform.error "Reached end of %s@." item;
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
  and source_format = pp.persist.source_format in
  reset_preprocessor ?new_position pp contents
    ~restart:(Src_reader.restart_on_string ?source_format)

(* --- *)

let preprocessor ~options input =
  preprocessor input (`WithOptions options)

(** Default pretty-printing formatter for {!lex_file}, {!lex_lib}, and
    {!preprocess_file}. *)
let default_oppf = Fmt.stdout

let lex_input ~dialect ~source_format ?(ppf = default_oppf) input =
  OUT.result @@
  Src_reader.print_lines ~dialect ~skip_compiler_directives_text:true ppf @@
  Src_reader.from input ?source_format:(source_format_config source_format)

let lex_file ~platform ~dialect ~source_format ?ppf filename =
  Src_input.from ~platform ~filename ~f:(lex_input ~dialect ~source_format ?ppf)

let lex_lib ~platform ~dialect ~source_format ~lookup_config
    ?(ppf = default_oppf) lib =
  match platform.find_lib ~lookup_config lib with
  | Ok filename ->
      Src_input.from ~platform ~filename ~f:begin fun input ->
        OUT.result @@
        Src_reader.print_lines ~dialect ~skip_compiler_directives_text:true ppf @@
        Src_reader.from input ?source_format:(source_format_config source_format)
      end
  | Error lnf ->
      OUT.error_result () @@ Copybook_lookup_error { lnf; copyloc = None }

let fold_source_lines ~dialect ~source_format ?on_change_of_source_format
    ?skip_compiler_directives_text ?on_compiler_directive
    ~f input acc =
  let reader =
    Src_reader.from input ?source_format:(source_format_config source_format)
  in
  let acc, on_compiler_directive = match on_change_of_source_format with
    | Some f ->
      let check_source_format_compdir _ { payload = cdir; _ } acc =
        match cdir with
        | Preproc_directives.CDir_source_format { payload = sf; _ } -> f sf acc
        | _ -> acc
      in
      let f_compdir = match on_compiler_directive with
        | Some f -> fun l cdir acc -> f l cdir @@ check_source_format_compdir l cdir acc
        | None -> check_source_format_compdir
      in
      f (Src_reader.source_format reader) acc, Some f_compdir
    | None ->
       acc, on_compiler_directive
  in
  Src_reader.fold_lines ~dialect ~f reader
    ?skip_compiler_directives_text ?on_compiler_directive acc

let fold_source_words ~dialect ~source_format ~f input acc =
  fold_source_lines ~dialect ~source_format input acc
    ~skip_compiler_directives_text:true
    ~f:begin fun _ line acc ->
      ListLabels.fold_left line ~init:acc ~f:(fun acc word -> f word acc)
    end

let scan_prefix_for_copybook ~dialect ~source_format input =
  let open struct
    exception Res of [`Program | `Copybook]
    type copybook_prefix_state =
      | Expect_first_digits
      | Expect_word
    let digit_chars s =
      let rec aux i =
        i < 0 || match s.[i] with '0'..'9' -> aux (pred i) | _ -> false
      in
      aux (String.length s - 1)
    let is_digits = function
      | Text.TextWord s -> digit_chars s
      | _ -> false
    let is_word = function
      | Text.TextWord _ as w -> not (is_digits w)
      | _ -> false
  end in
  match
    fold_source_words ~dialect ~source_format input Expect_first_digits
      ~f:begin fun word -> function
        | Expect_first_digits when is_digits ~&word ->
            Expect_word
        | Expect_word when is_word ~&word ->
            raise @@ Res `Copybook
        | _ ->
            raise @@ Res `Program
      end
  with
  | exception Res res -> res
  | Expect_first_digits -> `Program                                  (* maybe? *)
  | Expect_word -> `Copybook                                         (* maybe? *)

let text_of_input ~options input =
  let text, pp =
    full_text ~item:"file" @@ preprocessor ~options input in
  OUT.result text ~diags:(diags pp)

let text_of_file ~platform ~options filename =
  Src_input.from ~platform ~filename ~f:(text_of_input ~options)

let preprocess_input ~options ?(ppf = default_oppf) input =
  text_of_input ~options input |>
  OUT.map_result ~f:(Pretty.print ppf "%a@." Text.pp_text)

let preprocess_file ~platform ~options ?ppf filename =
  Src_input.from ~platform ~filename ~f:(preprocess_input ~options ?ppf)
