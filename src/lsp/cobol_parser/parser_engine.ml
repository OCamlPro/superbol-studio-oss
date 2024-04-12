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

open Cobol_common.Types
open Cobol_common.Srcloc.INFIX
open Parser_options                               (* import types for options *)
open Parser_outputs                               (* import types for outputs *)

module OUT = Parser_outputs

module Tokzr = Text_tokenizer
module Overlay_manager = Grammar_utils.Overlay_manager
module Grammar_interpr = Grammar.MenhirInterpreter
module Grammar_recovery =
  Recovery.Make (Grammar_interpr) (struct
    include Grammar_recover
    include Grammar_printer
    let benign_assumption: Grammar_tokens.token -> bool = function
      | PERIOD -> true
      | _ -> false
  end)

(* Main type definitions *)

type 'x rewinder =
  {
    rewind_n_parse: preprocessor_rewind -> position: position ->
      ('x * ('x rewinder)) with_diags;
  }
and preprocessor_rewind =
  ?new_position: Lexing.position -> (Cobol_preproc.preprocessor as 'r) -> 'r
and position =
  | Lexing of Lexing.position
  | Indexed of { line: int; char: int }                  (* all starting at 0 *)

type 'm simple_parsing
  = options: parser_options
  -> Cobol_preproc.preprocessor
  -> (Cobol_ptree.compilation_group option, 'm) output with_diags

type 'm rewindable_parsing
  = options: parser_options
  -> Cobol_preproc.preprocessor
  -> (((Cobol_ptree.compilation_group option, 'm) output as 'x) *
      'x rewinder) with_diags

(* --- *)

(** State of the parser.

    In ['m state], the ['m] parameter denotes the ability of the parser to
    memorize and provide every token that has been parsed so far, along with a
    log of operations performed by the pre-processor.  An
    {!Cobol_common.Behaviors.amnesic} parser does not provide this ability,
    contrary to an {!Cobol_common.Behaviors.eidetic} parser.  This is reflected
    in the final result (see {!parsed_result}), as {!parsing_artifacts} may only
    be called on results of eidetic parsers. *)
type 'm state =
  {
    prev_limit: Cobol_preproc.Src_overlay.limit option; (* last right-limit *)
    (* `prev_limit'` is required to deal with the single-step backtracking
       upon recovery: *)
    prev_limit': Cobol_preproc.Src_overlay.limit option;  (* second-to-last *)
    preproc: 'm preproc;
  }

(** Part of the parser state that typically changes on a sentence-by-sentence
    basis (mostly the pre-processor and tokenizer's states). *)
and 'm preproc =
  {
    pp: Cobol_preproc.preprocessor;
    diags: Parser_diagnostics.t;
    tokzr: 'm Tokzr.state;
    persist: 'm persist;
  }

(** Part of the parser state that changes very rarely, if at all. *)
and 'm persist =
  {
    tokenizer_memory: 'm memory;
    recovery: recovery;
    verbose: bool;
    show: [`Pending] list;
  }

(** Initializes a parser state, given a preprocessor. *)
let make_parser
    (type m) Parser_options.{ verbose; show; recovery; config; exec_scanners }
    ?show_if_verbose ~(tokenizer_memory: m memory) pp =
  let tokzr: m Tokzr.state =
    let memory: m Tokzr.memory = match tokenizer_memory with
      | Parser_options.Amnesic -> Tokzr.amnesic
      | Parser_options.Eidetic -> Tokzr.eidetic
    in
    let module Config = (val config) in
    Tokzr.init ~verbose ?show_if_verbose ~exec_scanners ~memory Config.words
  in
  {
    prev_limit = None;
    prev_limit' = None;
    preproc =
      {
        pp;
        diags = Parser_diagnostics.none;
        tokzr;
        persist =
          {
            tokenizer_memory;
            recovery;
            verbose;
            show;
          }
      }
  }

(* Helpers to avoid unnecessary copies of parser state *)
let update_pp ps pp =
  if pp == ps.preproc.pp then ps
  else { ps with preproc = { ps.preproc with pp } }
and update_tokzr ps tokzr =
  if tokzr == ps.preproc.tokzr then ps
  else { ps with preproc = { ps.preproc with tokzr } }

let add_diag ({ preproc = ({ diags; _ } as pp); _ } as ps) severity ?loc diag =
  let diags = Parser_diagnostics.add_diag ~severity ?loc diag diags in
  { ps with preproc = { pp with diags } }

let add_exn ({ preproc = ({ diags; _ } as pp); _ } as ps) e =
  { ps with preproc = { pp with diags = Parser_diagnostics.add_exn e diags } }

let all_diags { preproc = { pp; diags; tokzr; _ }; _ } =
  Parser_diagnostics.ALL.{
    parser_diags = Parser_diagnostics.union diags @@ Tokzr.diagnostics tokzr;
    preproc_diags = Cobol_preproc.diags pp;
  }

(* --- *)

let rec produce_tokens (ps: _ state as 's) : 's * Text_tokenizer.tokens =
  let text, pp = Cobol_preproc.next_chunk ps.preproc.pp in
  let { preproc = { pp; tokzr; _ }; _ } as ps = update_pp ps pp in
  assert (text <> []);
  (* Note: this is the source format in use at the end of the sentence. *)
  let source_format = Cobol_preproc.source_format pp in
  match Tokzr.tokenize_text ~source_format tokzr text with
  | Error `MissingInputs, tokzr ->
      produce_tokens (update_tokzr ps tokzr)
  | Error `ReachedEOF tokens, tokzr
  | Ok tokens, tokzr ->
      update_tokzr ps tokzr, tokens

let rec next_token ({ preproc = { tokzr; _ }; _ } as ps) tokens =
  match Tokzr.next_token tokzr tokens with
  | Some (tokzr, token, tokens) ->
      (update_tokzr ps tokzr, token, tokens)
  | None ->
      let ps, tokens = produce_tokens ps in
      next_token ps tokens

let token_n_srcloc_limits ?prev_limit token =
  let s, e = Overlay_manager.limits ~@token in
  Option.iter (fun e -> Overlay_manager.link_limits e s) prev_limit;
  ~&token, s, e

let put_token_back ({ preproc; _ } as ps) token tokens =
  let tokzr, tokens = Tokzr.put_token_back preproc.tokzr token tokens in
  (* The limits of the re-submitted token will be re-constructed in
     `token_n_srcloc_limits`, so `prev_limit` needs to be re-adjusted to the
     second-to-last right-limit. *)
  { ps with prev_limit = ps.prev_limit';
            preproc = { preproc with tokzr } }, tokens

(* --- *)

(** Use recovery trace (assumptions on missing tokens) to generate syntax hints
    and report on an invalid syntax error. *)
let report_syntax_hints_n_error ps
    (assumed: Grammar_recovery.assumption list)
    ~(report_invalid_syntax:
        Cobol_common.Diagnostics.severity -> (_ state as 's) -> 's)
    ~recovery_options
  =
  (* Gather one hint per source position of recovery assumptions, and determine
     global benignness *)
  let hints, globally_benign =
    let concat_reports r1 r2 = match r1, r2 with
      | Some r, Some s -> Some (Pretty.delayed "%t@ %t" r s)
      | r, None | None, r -> r
    in
    List.fold_left begin fun (reports, benign') assumption ->
      let Grammar_recovery.{ show; pos; benign } = assumption in
      let show, prev_reports = match reports with
        | (report, prev_pos) :: tl when prev_pos == pos ->      (* same position *)
            concat_reports report show, tl
        | tl ->
            show, tl                                          (* new position *)
      in
      (* Note: Consider not benign if nothing is to be reported (show =
         None). *)
      (show, pos) :: prev_reports, benign && benign' && show <> None
    end ([], assumed <> []) assumed                 (* initially benign unless no
                                                      assumption was involved *)
  in
  (* Accumulate hints about missing tokens *)
  let ps =
    List.fold_left begin fun ps -> function
      | None, _ ->                               (* nothing relevant to report *)
          ps
      | Some pp_assumed, raw_pos ->
          let loc = Overlay_manager.join_limits (raw_pos, raw_pos) in
          add_diag ps Hint ~loc (Missing_tokens pp_assumed)
    end ps (List.rev hints)
  in
  (* Generate a global error or warning if necessary *)
  if globally_benign && recovery_options.silence_benign_recoveries
  then ps
  else if globally_benign
  then report_invalid_syntax Warn ps
  else report_invalid_syntax Error ps

(* --- *)

let leaving_context_on_reduction { preproc = { tokzr; _ }; _ } prod =
  match Tokzr.top_context tokzr with
  | None -> false
  | Some top_ctx ->
      match Grammar_interpr.lhs prod with
      | X T _ -> false
      | X N nt -> match Grammar_context.nonterminal_context nt with
        | Some ctx -> ctx = top_ctx
        | _ -> false

let leaving_context_on_shift { preproc = { tokzr; _ }; _ } e1 e2 =
  match Tokzr.top_context tokzr with
  | None -> false
  | Some top_ctx ->
      Grammar_context.context_sinks_on_shift' top_ctx
        (Grammar_interpr.current_state_number e1)
        (Grammar_interpr.current_state_number e2)

let pop_context ({ preproc = { tokzr; _ }; _ } as ps) tokens =
  let tokzr, tokens = Tokzr.pop_context tokzr tokens in
  update_tokzr ps tokzr, tokens


let on_shift ps tokens e1 e2 =
  let ps, tokens =
    if leaving_context_on_shift ps e1 e2
    then pop_context ps tokens
    else ps, tokens
  in
  let tokzr, tokens =
    Tokzr.push_contexts ps.preproc.tokzr tokens @@
    Grammar_context.contexts_for_state_num @@
    Grammar_interpr.current_state_number e2
  in
  update_tokzr ps tokzr, tokens

let on_reduction ps tokens prod =
  if leaving_context_on_reduction ps prod
  then pop_context ps tokens
  else ps, tokens

(** Traverses a path (sequence of parser states or productions) that starts with
    the state that matches the current context stack, and applies the induced
    changes to the context stack. *)
let seesaw_context_stack ps tokens recovery_operations =
  List.fold_left begin fun (ps, tokens) -> function
    | Grammar_recovery.Shift (e1, e2) -> on_shift ps tokens e1 e2
    | Grammar_recovery.Reduce p -> on_reduction ps tokens p
  end (ps, tokens) recovery_operations

(* --- *)

let env_loc env =
  match Grammar_interpr.top env with
  | None -> None
  | Some (Element (_, _, s, e)) -> Some (Overlay_manager.join_limits (s, e))

let pending ?(severity = Cobol_common.Diagnostics.Warn) descr ps env =
  if List.mem `Pending ps.preproc.persist.show
  then add_diag ps severity ?loc:(env_loc env) (Implementation_pending descr)
  else ps

let post_production ({ preproc = { tokzr; _ }; _ } as ps)
    token tokens prod env =
  match Grammar_post_actions.post_production prod env with
  | Post_special_names DecimalPointIsComma ->
      let tokzr, token, tokens
        = Tokzr.decimal_point_is_comma tokzr token tokens in
      update_tokzr ps tokzr, token, tokens
  | Post_pending descr ->
      pending descr ps env, token, tokens
  | Post_special_names _
  | NoPost ->
      ps, token, tokens

(** To be called {e after} a reduction of production [prod]. *)
let after_reduction ps token tokens prod = function
  | Grammar_interpr.HandlingError env
  | AboutToReduce (env, _)
  | Shifting (_, env, _) ->
      post_production ps token tokens prod env
  | _ ->
      ps, token, tokens

(* Main code for driving the parser with recovery and lexical contexts: *)

(** We call "stage" a high(er)-level parsing state (than {!type:state}). *)
type ('a, 'm) stage =
  | Trans of ('a, 'm) interim_stage
  | Final of ('a option * 'm state)

(** Interim stage, at which the parser may be stopped, restarted or
    rewound. *)
and ('a, 'm) interim_stage =
  'm state *
  Text_tokenizer.tokens *
  'a Grammar_interpr.env                     (* Always valid input_needed env. *)

let rec normal ps tokens = function
  | Grammar_interpr.InputNeeded env ->
      Trans (ps, tokens, env)
  | Shifting (e1, e2, _) as c ->
      let ps, tokens = on_shift ps tokens e1 e2 in
      normal ps tokens @@ Grammar_interpr.resume c
  | Accepted v ->
      accept ps v
  | AboutToReduce _       (* may only happen upon `check` (or empty language) *)
  | Rejected | HandlingError _ ->
      assert false                                     (* should never happen *)

and on_interim_stage ({ prev_limit; _ } as ps, tokens, env) =
  let c = Grammar_interpr.input_needed env in
  let ps, token, tokens = next_token ps tokens in
  let _t, _, e as tok = token_n_srcloc_limits ?prev_limit token in
  let ps = { ps with prev_limit = Some e; prev_limit' = prev_limit } in
  check ps token tokens env @@ Grammar_interpr.offer c @@ match tok with
    | Grammar_tokens.INTERVENING_ '.', l, r -> Grammar_tokens.PERIOD, l, r
    | tok -> tok

and check ps token tokens env = function
  | Grammar_interpr.HandlingError env ->
      error ps token tokens env
  | AboutToReduce (_, prod) when leaving_context_on_reduction ps prod ->
      (* Reoffer token *)
      let ps, tokens = put_token_back ps token tokens in
      let ps, tokens = pop_context ps tokens in
      normal ps tokens @@ Grammar_interpr.input_needed env
  (* CHECKME: whether retokenizing is needed on shifts. *)
  (* | Shifting (e1, e2, _) when leaving_context_on_shift ps e1 e2 -> *)
  (*     let ps, tokens = put_token_back ps token tokens in *)
  (*     let ps, tokens = pop_context ps tokens in *)
  (*     normal ps tokens @@ Grammar_interpr.input_needed env *)
  | AboutToReduce (_, prod) as c ->
      (* NB: Here, we assume semantic actions do not raise any exception; maybe
         that's a tad too optimistic; if they did we may need to report that. *)
      let c = Grammar_interpr.resume c in
      let ps, token, tokens = after_reduction ps token tokens prod c in
      check ps token tokens env c
  | Shifting (e1, e2, _) as c ->
      let ps, tokens = on_shift ps tokens e1 e2 in
      check ps token tokens env @@ Grammar_interpr.resume c
  | c ->
      normal ps tokens c

and error ps token tokens env =
  let report_invalid_syntax =
    let loc_limits = Grammar_interpr.positions env in
    let loc = Overlay_manager.join_limits loc_limits in
    fun severity ps -> add_diag ps severity ~loc Invalid_syntax
  in
  match ps.preproc.persist.recovery with
  | EnableRecovery recovery_options ->
      let ps, tokens = put_token_back ps token tokens in
      recover ps tokens (Grammar_recovery.generate env)
        ~report_syntax_hints_n_error:(report_syntax_hints_n_error
                                        ~report_invalid_syntax
                                        ~recovery_options)
  | DisableRecovery ->
      Final (None, report_invalid_syntax Error ps)

and recover ps tokens candidates ~report_syntax_hints_n_error =
  let { prev_limit; _ } as ps, token, tokens = next_token ps tokens in
  let _, _, e as tok = token_n_srcloc_limits ?prev_limit token in
  let ps = { ps with prev_limit = Some e; prev_limit' = prev_limit } in
  match Grammar_recovery.attempt candidates tok with
  | `Fail when ~&token <> Grammar_tokens.EOF ->             (* ignore one token *)
      recover ps tokens candidates ~report_syntax_hints_n_error
  | `Fail when Option.is_none candidates.final ->
      Final (None, report_syntax_hints_n_error ps [])    (* unable to recover *)
  | `Fail ->
      let v, assumed = Option.get candidates.final in
      accept (report_syntax_hints_n_error ps assumed) v
  | `Accept (v, assumed) ->
      accept (report_syntax_hints_n_error ps assumed) v
  | `Ok (c, _, visited, assumed) ->
      let ps, tokens = seesaw_context_stack ps tokens visited in
      normal (report_syntax_hints_n_error ps assumed) tokens c

and accept ps v =
  Final (Some v, ps)

let on_exn ps e =
  Final (None, add_exn ps e)

(* --- *)

(** [first_stage ps ~make_checkpoint] is the first stage for parsing a ['a] out
    of a parser in state [ps]. *)
let first_stage (ps: 'm state) ~make_checkpoint : ('a, 'm) stage =
  let ps, tokens = produce_tokens ps in
  let first_pos = match tokens with
    | [] -> Cobol_preproc.position ps.preproc.pp
    | t :: _ -> Cobol_common.Srcloc.start_pos ~@t
  in
  normal ps tokens (make_checkpoint first_pos)

(** [full_parse stage] completes parsing from the given stage [stage]. *)
let rec full_parse: ('a, 'm) stage -> 'a option * 'm state = function
  | Final (res, ps) ->
      res, ps
  | Trans ((ps, _, _) as state) ->
      full_parse @@ try on_interim_stage state with e -> on_exn ps e

(* --- *)

(** Gathers outputs that depend on the memorization behavior of the parser. *)
let aggregate_output (type m) (ps: m state) res
  : ('a option, m) output =
  match ps.preproc.persist.tokenizer_memory with
  | Amnesic ->
      Only res
  | Eidetic ->
      let artifacts =
        { tokens = Tokzr.parsed_tokens ps.preproc.tokzr;
          pplog = Cobol_preproc.rev_log ps.preproc.pp;
          rev_comments = Cobol_preproc.rev_comments ps.preproc.pp;
          rev_ignored = Cobol_preproc.rev_ignored ps.preproc.pp } in
      WithArtifacts (res, artifacts)

(** Simple parsing *)
let parse_once ~options (type m) ~(memory: m memory) ~make_checkpoint pp
  : (('a option, m) output) with_diags =
  let ps = make_parser options ~tokenizer_memory:memory pp in
  let res, ps = full_parse @@ first_stage ~make_checkpoint ps in
  OUT.with_diags (aggregate_output ps res) (all_diags ps)

(* --- *)

(* Rewindable parsing *)

(** The state of a rewindable parser combines a current stage [stage], and a
    store [store] that represent a rewindable history.  The initial state is
    kept in case parsing needs to restart at the very beginning of the input. *)
type ('a, 'm) rewindable_parsing_state =
  {
    init: 'm state;
    stage: ('a, 'm) stage;
    store: ('a, 'm) rewindable_history;
  }

(** The rewindable history is a list of events... *)
and ('a, 'm) rewindable_history = ('a, 'm) rewindable_history_event list

(** ... that associate pre-processor lexing positions ([preproc_position])
    with intermediate parsing stages [event_stage]. *)
and ('a, 'm) rewindable_history_event =
  {
    preproc_position: Lexing.position;
    event_stage: ('a, 'm) interim_stage_without_tokens;
  }

and ('a, 'm) interim_stage_without_tokens =
  'm state * 'a Grammar_interpr.env           (* Always valid input_needed env. *)

let init_rewindable_parse ps ~make_checkpoint =
  {
    init = ps;
    stage = first_stage ps ~make_checkpoint;
    store = [];
  }

(** Stores a stage as part of the memorized rewindable history events. *)
let save_interim_stage (ps, _, env) (store: _ rewindable_history) =
  let preproc_position = Cobol_preproc.position ps.preproc.pp in
  match store with
  | store'
    when preproc_position.pos_cnum <> preproc_position.pos_bol ->
      (* We must only save positions that correspond to beginning of lines; this
         should only make us skip recording events at the end of inputs. *)
      store'
  | { preproc_position = prev_pos; _ } :: store'
    when prev_pos.pos_cnum  = preproc_position.pos_cnum  &&
         prev_pos.pos_fname = preproc_position.pos_fname ->
      (* Preprocessor did not advance further since last save: replace event
         with new parser state: *)
      { preproc_position; event_stage = (ps, env) } :: store'
  | store' ->
      { preproc_position; event_stage = (ps, env) } :: store'

let rewindable_parser_state = function
  | { stage = Final (_, ps) | Trans (ps, _, _); _ } -> ps

(** Parses all the input, saving some rewindable history along the way. *)
(* TODO: configurable [save_stage] *)
let parse_with_history ?(save_stage = 10) rwps =
  let rec loop count ({ store; stage; _ } as rwps) = match stage with
    | Final (res, _) ->
        res, rwps
    | Trans ((ps, _, _) as state) ->
        let store, count =
          if count = save_stage then store, succ count
          else save_interim_stage state store, 0
        and stage =
          try on_interim_stage state with e -> on_exn ps e
        in
        loop count { rwps with store; stage }
  in
  Tokzr.enable_context_sensitive_tokens
    (rewindable_parser_state rwps).preproc.tokzr;
  let res, rwps = loop 0 rwps in
  Tokzr.disable_context_sensitive_tokens
    (rewindable_parser_state rwps).preproc.tokzr;
  res, rwps

let find_history_event_preceding ~position ({ store; _ } as rwps) =
  let lexpos = match position with
    | Lexing pos ->
        pos
    | Indexed { line; char } ->
        let ps = rewindable_parser_state rwps in
        Cobol_preproc.position_at ~line ~char ps.preproc.pp
  in
  let rec aux = function
    | [] ->
        raise Not_found
    | { preproc_position; _ } as event :: store
      when preproc_position.pos_cnum <= lexpos.pos_cnum &&
           preproc_position.pos_fname = lexpos.pos_fname ->
        event, store
    | _ :: store ->
        aux store
  in
  aux store

(* --- *)

let rec rewind_n_parse
  : type m. ('a, m) rewindable_parsing_state -> make_checkpoint:_
    -> preprocessor_rewind -> position: position
    -> ((('a option, m) output as 'x) * 'x rewinder) with_diags =
  fun rwps ~make_checkpoint pp_rewind ~position ->
  let rwps =
    try
      let event, store = find_history_event_preceding ~position rwps in
      let ps, env = event.event_stage in
      let pp = ps.preproc.pp in
      let pp = pp_rewind ~new_position:event.preproc_position pp in
      let ps = { ps with preproc = { ps.preproc with pp } } in
      Overlay_manager.restart ();
      let ps, tokens = produce_tokens ps in
      { rwps with stage = Trans (ps, tokens, env); store }
    with Not_found ->                     (* rewinding before first checkpoint *)
      let pp = pp_rewind rwps.init.preproc.pp in
      let ps = { rwps.init with preproc = { rwps.init.preproc with pp } } in
      init_rewindable_parse ~make_checkpoint ps
  in
  let res, rwps = parse_with_history rwps in
  let ps = rewindable_parser_state rwps in
  let output = aggregate_output ps res in
  let rewind_n_parse = rewind_n_parse rwps ~make_checkpoint in
  OUT.with_diags (output, { rewind_n_parse }) (all_diags ps)

let rewindable_parse
  : options:_ -> memory:'m memory -> make_checkpoint:_
    -> Cobol_preproc.preprocessor
    -> ((('a option, 'm) output as 'x) * 'x rewinder) with_diags =
  fun ~options ~memory ~make_checkpoint pp ->
  let res, rwps =
    make_parser options ~tokenizer_memory:memory pp |>
    init_rewindable_parse ~make_checkpoint |>
    parse_with_history
  in
  let ps = rewindable_parser_state rwps in
  let output = aggregate_output ps res in
  let rewind_n_parse = rewind_n_parse rwps ~make_checkpoint in
  OUT.with_diags (output, { rewind_n_parse }) (all_diags ps)

(* --- *)

(* Main exported functions *)

let parse
    (type m)
    ~(memory: m memory)
    ~(options: parser_options)
  : Cobol_preproc.preprocessor ->
    (Cobol_ptree.compilation_group option, m) output with_diags =
  parse_once ~options ~memory
    ~make_checkpoint:Grammar.Incremental.compilation_group

let parse_simple = parse ~memory:Amnesic
let parse_with_artifacts = parse ~memory:Eidetic

let rewindable_parse
    (type m)
    ~(memory: m memory)
    ~(options: parser_options)
  : Cobol_preproc.preprocessor ->
    (((Cobol_ptree.compilation_group option, m) output as 'x) * 'x rewinder)
      with_diags =
  rewindable_parse ~options ~memory
    ~make_checkpoint:Grammar.Incremental.compilation_group

let rewindable_parse_simple = rewindable_parse ~memory:Amnesic
let rewindable_parse_with_artifacts = rewindable_parse ~memory:Eidetic

let rewind_and_parse { rewind_n_parse } rewind_preproc ~position =
  rewind_n_parse rewind_preproc ~position

let artifacts
  : (_, Cobol_common.Behaviors.eidetic) output -> _ = function
  | WithArtifacts (_, artifacts) -> artifacts
