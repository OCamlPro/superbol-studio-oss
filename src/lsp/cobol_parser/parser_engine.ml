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

module DIAGS = Cobol_common.Diagnostics

open Cobol_common.Types
open Cobol_common.Srcloc.INFIX
open Parser_options                               (* import types for options *)
open Parser_outputs                               (* import types for outputs *)

(* Main type definitions *)

type 'x rewinder =
  {
    rewind_n_parse: preprocessor_rewind -> position: Lexing.position ->
      ('x * ('x rewinder)) with_diags;
  }
and preprocessor_rewind =
  ?new_position:Lexing.position -> (Cobol_preproc.preprocessor as 'r) -> 'r

type 'm simple_parsing
  = ?options:parser_options
  -> ?config:Cobol_config.t
  -> Cobol_preproc.preprocessor
  -> (PTree.compilation_group option, 'm) output DIAGS.with_diags

type 'm rewindable_parsing
  = ?options:parser_options
  -> ?config:Cobol_config.t
  -> Cobol_preproc.preprocessor
  -> (((PTree.compilation_group option, 'm) output as 'x) *
      'x rewinder) DIAGS.with_diags

(* --- *)

(** Parser configuration is mostly to deal with reserved tokens and grammar
    post-actions. *)
module Make (Config: Cobol_config.T) = struct

  module Tokzr = Text_tokenizer.Make (Config)
  module Post = Grammar_post_actions.Make (Config)
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


  (** State of the parser.

      In ['m state], the ['m] parameter denotes the ability of the parser to
      memorize and provide every token that has been parsed so far, along with a
      log of operations performed by the pre-processor.  An
      {!Cobol_common.Behaviors.amnesic} parser does not provide this ability,
      contrary to an {!Cobol_common.Behaviors.eidetic} parser.  This is
      reflected in the final result (see {!parsed_result}), as
      {!parsing_artifacts} may only be called on results of eidetic parsers. *)
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
      pp: Cobol_preproc.preprocessor;               (* also holds diagnistics *)
      tokzr: 'm Tokzr.state;
      context_stack: Context.stack;
      persist: 'm persist;
    }

  (** Part of the parser state that changes very rarely, if at all. *)
  and 'm persist =
    {
      tokenizer_memory: 'm memory;
      recovery: recovery;
      verbose: bool;
      show_if_verbose: [`Tks | `Ctx] list;
      show: [`Pending] list;
    }

  (* TODO: reset/restore text lexer's state w.r.t reserved/alias and
     context-stack will be needed when we want a persistent parser state.  Best
     place for this is probaly in the tokenizer.*)

  let make_parser
      (type m) Parser_options.{ verbose; show; recovery }
      ?(show_if_verbose = [`Tks; `Ctx]) ~(tokenizer_memory: m memory) pp =
    let tokzr: m Tokzr.state =
      let memory: m Tokzr.memory = match tokenizer_memory with
        | Parser_options.Amnesic -> Tokzr.amnesic
        | Parser_options.Eidetic -> Tokzr.eidetic
      in
      Tokzr.init memory
        ~context_sensitive_tokens:Grammar_contexts.sensitive_tokens
    in
    {
      prev_limit = None;
      prev_limit' = None;
      preproc =
        {
          pp;
          tokzr;
          context_stack = Context.empty_stack;
          persist =
            {
              tokenizer_memory;
              recovery;
              verbose;
              show_if_verbose;
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

  let show tag
      { preproc = { persist = { verbose; show_if_verbose; _ }; _ }; _ } =
    verbose && List.mem tag show_if_verbose

  let add_diag diag ({ preproc = { pp; _ }; _ } as ps) =
    update_pp ps (Cobol_preproc.add_diag pp diag)
  let add_diags diags ({ preproc = { pp; _ }; _ } as ps) =
    update_pp ps (Cobol_preproc.add_diags pp diags)

  let all_diags { preproc = { pp; tokzr; _ }; _ } =
    DIAGS.Set.union (Cobol_preproc.diags pp) @@ Tokzr.diagnostics tokzr

  let rec produce_tokens (ps: _ state as 's) : 's * Text_tokenizer.tokens =
    let text, pp = Cobol_preproc.next_sentence ps.preproc.pp in
    let { preproc = { pp; tokzr; _ }; _ } as ps = update_pp ps pp in
    assert (text <> []);
    (* Note: this is the source format in use at the end of the sentence. *)
    let Plx (pl, _) = Cobol_preproc.srclexer pp in
    let source_format = Cobol_preproc.Src_lexing.source_format pl in
    match Tokzr.tokenize_text ~source_format tokzr text with
    | Error `MissingInputs, tokzr ->
        produce_tokens (update_tokzr ps tokzr)
    | Error `ReachedEOF tokens, tokzr
    | Ok tokens, tokzr ->
        if show `Tks ps then
          Pretty.error "Tks: %a@." Text_tokenizer.pp_tokens tokens;
        update_tokzr ps tokzr, tokens

  let update_context_stack ~stack_update ~tokenizer_update
      ({ preproc; _ } as ps) tokens : Context.t list -> 's * 'a =
    function
    | [] ->
        ps, tokens
    | contexts ->
        let context_stack, tokens_set =
          List.fold_left begin fun (context_stack, set) ctx ->
            let context_stack, set' = stack_update context_stack ctx in
            context_stack, Text_lexer.TokenHandles.union set set'
          end (preproc.context_stack, Text_lexer.TokenHandles.empty) contexts
        in

        (* Update tokenizer state *)
        let tokzr, tokens = tokenizer_update preproc.tokzr tokens tokens_set in
        if show `Tks ps then
          Pretty.error "Tks': %a@." Text_tokenizer.pp_tokens tokens;

        (if context_stack == preproc.context_stack && tokzr == preproc.tokzr
         then ps
         else { ps with preproc = { preproc with tokzr; context_stack }}),
        tokens

  (** Use recovery trace (assumptions on missing tokens) to generate syntax
      hints and report on an invalid syntax error. *)
  let report_syntax_hints_n_error ps
      (assumed: Grammar_recovery.assumption list)
      ~(report_invalid_syntax: DIAGS.severity -> (_ state as 's) -> 's)
      ~recovery_options
    =
    (* Gather one hint per source position of recovery assumptions, and
       determine global benignness *)
    let hints, globally_benign =
      let concat_reports r1 r2 = match r1, r2 with
        | Some r, Some s -> Some (Pretty.delayed "%t@ %t" r s)
        | r, None | None, r -> r
      in
      List.fold_left begin fun (reports, benign') assumption ->
        let Grammar_recovery.{ show; pos; benign } = assumption in
        let show, prev_reports = match reports with
          | (report, prev_pos) :: tl when prev_pos == pos ->    (* same position *)
              concat_reports report show, tl
          | tl ->
              show, tl                                        (* new position *)
        in
        (* Note: Consider not benign if nothing is to be reported (show =
           None). *)
        (show, pos) :: prev_reports, benign && benign' && show <> None
      end ([], assumed <> []) assumed               (* initially benign unless no
                                                       assumption was involved *)
    in
    (* Accumulate hints about missing tokens *)
    let diags =
      List.fold_left begin fun diags -> function
        | None, _ ->                             (* nothing relevant to report *)
            diags
        | Some pp_assumed, raw_pos ->
            let loc = Overlay_manager.join_limits (raw_pos, raw_pos) in
            DIAGS.Acc.hint diags ~loc "Missing@ %t" pp_assumed
      end Cobol_common.Diagnostics.Set.none (List.rev hints)
    in
    let ps = add_diags diags ps in
    (* Generate a global error or warning if necessary *)
    if globally_benign && recovery_options.silence_benign_recoveries
    then ps
    else if globally_benign
    then report_invalid_syntax DIAGS.Warn ps
    else report_invalid_syntax DIAGS.Error ps

  (* --- *)

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
    { ps with prev_limit = ps.prev_limit';
              preproc = { ps.preproc with tokzr } }, tokens

  let leaving_context ps prod =
    match Context.top ps.preproc.context_stack with
    | None -> false                                            (* first filter *)
    | Some top_ctx ->
        match Grammar_interpr.lhs prod with
        | X T _ -> false
        | X N nt -> match Grammar_context.nonterminal_context nt with
          | Some ctx -> ctx == top_ctx
          | _ -> false

  let pop_context ({ preproc = { tokzr; context_stack; _ }; _ } as ps)
      tokens =
    let context_stack, tokens_set = Context.pop context_stack in
    if show `Ctx ps then
      Pretty.error "Outgoing: %a@." Context.pp_context tokens_set;
    let tokzr, tokens = Tokzr.disable_tokens tokzr tokens tokens_set in
    { ps with preproc = { ps.preproc with tokzr; context_stack }},
    tokens

  let push_incoming_contexts ps tokens env =

    let push context_stack ctx =
      if show `Ctx ps then
        Pretty.error "Incoming: %a@." Context.pp_context ctx;

      (* Push the new context on top of the stack *)
      let context_stack = Context.push ctx context_stack in

      (* ... and retrieve newly reserved tokens *)
      context_stack, Context.top_tokens context_stack
    in

    (* Retrieve and enable all incoming contexts *)
    update_context_stack ps tokens
      (Grammar_context.contexts_for_state_num @@
       Grammar_interpr.current_state_number env)
      ~stack_update:push
      ~tokenizer_update:Tokzr.enable_tokens

  let pop_outgoing_context ps tokens prod =
    if leaving_context ps prod
    then pop_context ps tokens
    else ps, tokens

  (** Traverses a path (sequence of parser states or productions) that starts
      with the state that matches the current context stack, and applies the
      induced changes to the context stack. *)
  let seesaw_context_stack ps tokens operations =
    List.fold_left begin fun (ps, tokens) -> function
      | Grammar_recovery.Env e -> push_incoming_contexts ps tokens e
      | Grammar_recovery.Prod p -> pop_outgoing_context ps tokens p
    end (ps, tokens) operations

  let env_loc env =
    match Grammar_interpr.top env with
    | None -> None
    | Some (Element (_, _, s, e)) -> Some (Overlay_manager.join_limits (s, e))

  let pending ?(severity = DIAGS.Warn) descr ps env =
    if List.mem `Pending ps.preproc.persist.show then
      let diag =
        DIAGS.One.diag severity "Ignored@ %a@ (implementation@ pending)"
          Pretty.text descr ?loc:(env_loc env)
      in
      add_diag diag ps
    else ps

  let post_production ({ preproc = { tokzr; _ }; _ } as ps)
      token tokens prod env =
    match Post.post_production prod env with
    | Post_diagnostic action ->
        let ps = match action ~loc:(env_loc env) with
          | Ok ((), Some diag) | Error Some diag -> add_diag diag ps
          | Ok ((), None) | Error None -> ps
        in
        ps, token, tokens
    | Post_special_names DecimalPointIsComma ->
        let tokzr, token, tokens =
          Tokzr.decimal_point_is_comma tokzr token tokens in
        if show `Tks ps then
          Pretty.error "Tks': %a@." Text_tokenizer.pp_tokens tokens;
        update_tokzr ps tokzr, token, tokens
    | Post_pending descr ->
        pending descr ps env, token, tokens
    | Post_special_names _
    | NoPost ->
        ps, token, tokens

  let on_reduction ps token tokens prod = function
    | Grammar_interpr.HandlingError env
    | AboutToReduce (env, _)
    | Shifting (_, env, _) ->
        post_production ps token tokens prod env
    | _ ->
        ps, token, tokens

  (* Main code for driving the parser with recovery and lexical contexts: *)

  type ('a, 'm) step =
    | OnTok of ('a, 'm) new_token_step
    | Final of ('a option * 'm state)
  and ('a, 'm) new_token_step =
    (('m state * Text_tokenizer.token * Text_tokenizer.tokens) *
     'a Grammar_interpr.env)                (* Always valid input_needed  env. *)

  let rec normal ps tokens = function
    | Grammar_interpr.InputNeeded env ->
        OnTok (next_token ps tokens, env)
    | Shifting (_e1, e2, _) as c ->
        let ps, tokens = push_incoming_contexts ps tokens e2 in
        normal ps tokens @@ Grammar_interpr.resume c
    | Accepted v ->
        accept ps v
    | AboutToReduce _     (* may only happen upon `check` (or empty language) *)
    | Rejected | HandlingError _ ->
        assert false                                   (* should never happen *)

  and on_new_token (({ prev_limit; _ } as ps, token, tokens), env) =
    let c = Grammar_interpr.input_needed env in
    let _t, _, e as tok = token_n_srcloc_limits ?prev_limit token in
    let ps = { ps with prev_limit = Some e; prev_limit' = prev_limit } in
    check ps token tokens env @@ Grammar_interpr.offer c tok

  and check ps token tokens env = function
    | Grammar_interpr.HandlingError env ->
        error ps token tokens env
    | AboutToReduce (_, prod) when leaving_context ps prod ->
        (* Reoffer token *)
        let ps, tokens = put_token_back ps token tokens in
        let ps, tokens = pop_context ps tokens in
        normal ps tokens @@ Grammar_interpr.input_needed env
    | AboutToReduce (_, prod) as c ->
        (* NB: Here, we assume semantic actions do not raise any exception;
           maybe that's a tad too optimistic; if they did we may need to report
           that. *)
        let c = Grammar_interpr.resume c in
        let ps, token, tokens = on_reduction ps token tokens prod c in
        check ps token tokens env c
    | Shifting (_e1, e2, _) as c ->
        let ps, tokens = push_incoming_contexts ps tokens e2 in
        check ps token tokens env @@ Grammar_interpr.resume c
    | c ->
        normal ps tokens c

  and error ps token tokens env =
    let report_invalid_syntax =
      let loc_limits = Grammar_interpr.positions env in
      let loc = Overlay_manager.join_limits loc_limits in
      fun severity -> add_diag (DIAGS.One.diag severity ~loc "Invalid@ syntax")
    in
    match ps.preproc.persist.recovery with
    | EnableRecovery recovery_options ->
        (* The limits of the re-submitted token will be re-constructed in
           `token_n_srcloc_limits`, so `prev_limit` needs to be re-adjusted to
           the second-to-last right-limit. *)
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
    | `Fail when ~&token <> Grammar_tokens.EOF ->           (* ignore one token *)
        recover ps tokens candidates ~report_syntax_hints_n_error
    | `Fail when Option.is_none candidates.final ->
        Final (None, report_syntax_hints_n_error ps [])  (* unable to recover *)
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
    Final (None, add_diag (DIAGS.of_exn e) ps)

  (* --- *)

  let init_parse ps ~make_checkpoint =
    let ps, tokens = produce_tokens ps in
    let first_pos = match tokens with
      | [] -> Cobol_preproc.position ps.preproc.pp
      | t :: _ -> Cobol_common.Srcloc.start_pos ~@t
    in
    normal ps tokens (make_checkpoint first_pos)

  let rec full_parse = function
    | Final (res, ps) ->
        res, ps
    | OnTok (((ps, _, _), _) as state) ->
        full_parse @@ try on_new_token state with e -> on_exn ps e

  (* --- *)

  type ('a, 'm) rewindable_parsing_state =
    {
      init: 'm state;
      step: ('a, 'm) step;
      store: ('a, 'm) rewindable_history;
    }
  and ('a, 'm) rewindable_history = ('a, 'm) rewindable_history_event list
  and ('a, 'm) rewindable_history_event =
    {
      preproc_position: Lexing.position;
      event_step: ('a, 'm) new_token_step;
    }

  let save_history_event
      (((ps, _, _), _) as state) (store: _ rewindable_history) =
    let preproc_position = Cobol_preproc.position ps.preproc.pp in
    match store with
    | { preproc_position = prev_pos; _ } :: store'
      when prev_pos.pos_cnum  = preproc_position.pos_cnum  &&
           prev_pos.pos_fname = preproc_position.pos_fname ->
        (* Preprocessor did not advance further since last save: replace event
           with new parser state: *)
        { preproc_position; event_step = state } :: store'
    | store' ->
        { preproc_position; event_step = state } :: store'

  let init_rewindable_parse ps ~make_checkpoint =
    {
      init = ps;
      step = init_parse ps ~make_checkpoint;
      store = [];
    }

  let rewindable_parser_state = function
    | { step = Final (_, ps) | OnTok ((ps, _, _), _); _ } -> ps

  let with_context_sensitive_tokens ~f = function
    | { step = Final (_, ps) | OnTok ((ps, _, _), _); _ } ->
        f (Context.all_tokens ps.preproc.context_stack)

  let parse_with_trace ?(save_step = 10) rwps =
    let rec loop count ({ store; step; _ } as rwps) = match step with
      | Final (res, _ps) ->
          with_context_sensitive_tokens rwps ~f:Text_lexer.disable_tokens;
          res, rwps
      | OnTok (((ps, _, _), _) as state) ->
          let store, count =
            if count = save_step then store, succ count
            else save_history_event state store, 0
          and step =
            try on_new_token state with e -> on_exn ps e
          in
          loop count { rwps with store; step }
    in
    with_context_sensitive_tokens rwps ~f:Text_lexer.enable_tokens;
    loop 0 rwps

  (* --- *)

  let aggregate_output (type m) res (ps: m state) : ('a option, m) output =
    match ps.preproc.persist.tokenizer_memory with
    | Amnesic ->
        Only res
    | Eidetic ->
        let artifacts =
          { tokens = Tokzr.parsed_tokens ps.preproc.tokzr;
            pplog = Cobol_preproc.log ps.preproc.pp;
            comments = Cobol_preproc.comments ps.preproc.pp;
            newline_cnums = Cobol_preproc.newline_cnums ps.preproc.pp } in
        WithArtifacts (res, artifacts)

  let parse_once
      ~options (type m) ~(memory: m memory) ~make_checkpoint pp
    : (('a option, m) output) with_diags =
    let ps = make_parser options ~tokenizer_memory:memory pp in
    let res, ps = full_parse @@ init_parse ~make_checkpoint ps in
    DIAGS.with_diags (aggregate_output res ps) (all_diags ps)

  let find_history_event_preceding ~(position: Lexing.position) store =
    let rec aux = function
      | [] ->
          raise Not_found
      | { preproc_position; _ } as event :: store
        when preproc_position.pos_cnum  <= position.pos_cnum &&
             preproc_position.pos_fname = position.pos_fname ->
          event, store
      | _ :: store ->
          aux store
    in
    aux store

  let rec rewind_n_parse
    : type m. ('a, m) rewindable_parsing_state -> make_checkpoint:_
      -> preprocessor_rewind -> position:Lexing.position
      -> ((('a option, m) output as 'x) * 'x rewinder) with_diags =
    fun rwps ~make_checkpoint pp_rewind ~position ->
    let rwps =
      try
        let event, store = find_history_event_preceding ~position rwps.store in
        let (ps, token, tokens), env = event.event_step in
        let pp = ps.preproc.pp in
        let pp = pp_rewind ?new_position:(Some event.preproc_position) pp in
        let ps = { ps with preproc = { ps.preproc with pp } } in
        { rwps with step = OnTok ((ps, token, tokens), env); store }
      with Not_found ->                   (* rewinding before first checkpoint *)
        let pp = pp_rewind rwps.init.preproc.pp in
        let ps = { rwps.init with preproc = { rwps.init.preproc with pp } } in
        init_rewindable_parse ~make_checkpoint ps
    in
    let res, rwps = parse_with_trace rwps in
    let ps = rewindable_parser_state rwps in
    let output = aggregate_output res ps in
    let rewind_n_parse = rewind_n_parse rwps ~make_checkpoint in
    DIAGS.with_diags (output, { rewind_n_parse }) (all_diags ps)

  let rewindable_parse
    : options:_ -> memory:'m memory -> make_checkpoint:_
      -> Cobol_preproc.preprocessor
      -> ((('a option, 'm) output as 'x) * 'x rewinder) with_diags =
    fun ~options ~memory ~make_checkpoint pp ->
    let res, rwps =
      make_parser options ~tokenizer_memory:memory pp |>
      init_rewindable_parse ~make_checkpoint |>
      parse_with_trace
    in
    let ps = rewindable_parser_state rwps in
    let output = aggregate_output res ps in
    let rewind_n_parse = rewind_n_parse rwps ~make_checkpoint in
    DIAGS.with_diags (output, { rewind_n_parse }) (all_diags ps)

end

(* --- *)

(* Main exported functions *)

let parse
    (type m)
    ~(memory: m memory)
    ?(options = Parser_options.default)
    ?(config = Cobol_config.default)
  : Cobol_preproc.preprocessor ->
    (PTree.compilation_group option, m) output with_diags =
  let module P = Make (val config) in
  P.parse_once ~options ~memory
    ~make_checkpoint:Grammar.Incremental.compilation_group

let parse_simple = parse ~memory:Amnesic
let parse_with_artifacts = parse ~memory:Eidetic

let rewindable_parse
    (type m)
    ~(memory: m memory)
    ?(options = Parser_options.default)
    ?(config = Cobol_config.default)
  : Cobol_preproc.preprocessor ->
    (((PTree.compilation_group option, m) output as 'x) * 'x rewinder)
      with_diags =
  let module P = Make (val config) in
  P.rewindable_parse ~options ~memory
    ~make_checkpoint:Grammar.Incremental.compilation_group

let rewindable_parse_simple = rewindable_parse ~memory:Amnesic
let rewindable_parse_with_artifacts = rewindable_parse ~memory:Eidetic

let rewind_and_parse { rewind_n_parse } rewind_preproc ~position =
  rewind_n_parse rewind_preproc ~position

let artifacts
  : (_, Cobol_common.Behaviors.eidetic) output -> _ = function
  | WithArtifacts (_, artifacts) -> artifacts