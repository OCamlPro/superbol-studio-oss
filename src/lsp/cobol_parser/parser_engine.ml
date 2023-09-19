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
include Parser_options                            (* import types for options *)

module Make (Config: Cobol_config.T) = struct

  module Tokzr = Text_tokenizer.Make (Config)
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

  module Post = Grammar_post_actions.Make (Config)

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
      recovery: recovery;
      tokenizer_memory: 'm memory;
      verbose: bool;
      show_if_verbose: [`Tks | `Ctx] list;
      show: [`Pending] list;
    }

  (* TODO: reset/restore text lexer's state w.r.t reserved/alias and
     context-stack will be needed when we want a persistent parser state.  Best
     place for this is probaly in the tokenizer.*)

  let init_parser
      ?(verbose = false)
      ?(show_if_verbose = [`Tks; `Ctx])
      ?(show = [`Pending])
      (type m) ~(tokenizer_memory: m memory)
      ~(recovery: recovery)
      pp =
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
              recovery;
              tokenizer_memory;
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

  let state_num env = Grammar_interpr.current_state_number env

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

  let do_parse: type m. m state -> _ -> _ -> _ * m state =

    let rec next_tokens ({ preproc = { tokzr; _ }; _ } as ps) tokens =
      match Tokzr.next_token tokzr tokens with
      | Some (tokzr, token, tokens) ->
          update_tokzr ps tokzr, (token, tokens)
      | None ->
          let ps, tokens = produce_tokens ps in
          next_tokens ps tokens
    in

    let token_n_srcloc_limits ?prev_limit token =
      let s, e = Overlay_manager.limits ~@token in
      Option.iter (fun e -> Overlay_manager.link_limits e s) prev_limit;
      ~&token, s, e
    in

    let put_token_back ({ preproc; _ } as ps) token tokens =
      let tokzr, tokens = Tokzr.put_token_back preproc.tokzr token tokens in
      { ps with prev_limit = ps.prev_limit';
                preproc = { ps.preproc with tokzr } }, tokens
    in

    let leaving_context ps prod =
      match Context.top ps.preproc.context_stack with
      | None -> false                                          (* first filter *)
      | Some top_ctx ->
          match Grammar_interpr.lhs prod with
          | X T _ -> false
          | X N nt -> match Grammar_context.nonterminal_context nt with
            | Some ctx -> ctx == top_ctx
            | _ -> false
    in

    let pop_context ({ preproc = { tokzr; context_stack; _ }; _ } as ps)
        tokens =
      let context_stack, tokens_set = Context.pop context_stack in
      if show `Ctx ps then
        Pretty.error "Outgoing: %a@." Context.pp_context tokens_set;
      let tokzr, tokens = Tokzr.disable_tokens tokzr tokens tokens_set in
      { ps with preproc = { ps.preproc with tokzr; context_stack }},
      tokens
    in

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
        (Grammar_context.contexts_for_state_num (state_num env))
        ~stack_update:push
        ~tokenizer_update:Tokzr.enable_tokens

    and pop_outgoing_context ps tokens prod =
      if leaving_context ps prod
      then pop_context ps tokens
      else ps, tokens
    in

    (** Traverses a path (sequence of parser states or productions) that starts
        with the state that matches the current context stack, and applies the
        induced changes to the context stack. *)
    let seesaw_context_stack ps tokens =
      List.fold_left begin fun (ps, tokens) -> function
        | Grammar_recovery.Env e -> push_incoming_contexts ps tokens e
        | Grammar_recovery.Prod p -> pop_outgoing_context ps tokens p
      end (ps, tokens)
    in

    let env_loc env =
      match Grammar_interpr.top env with
      | None -> None
      | Some (Element (_, _, s, e)) -> Some (Overlay_manager.join_limits (s, e))
    in

    let pending ?(severity = DIAGS.Warn) descr ps env =
      if List.mem `Pending ps.preproc.persist.show then
        let diag =
          DIAGS.One.diag severity "Ignored@ %a@ (implementation@ pending)"
            Pretty.text descr ?loc:(env_loc env)
        in
        add_diag diag ps
      else ps
    in

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
    in

    let rec normal ({ prev_limit; _ } as ps) tokens = function
      | Grammar_interpr.InputNeeded env as c ->
          let ps, (token, tokens) = next_tokens ps tokens in
          let _t, _, e as tok = token_n_srcloc_limits ?prev_limit token in
          let ps = { ps with prev_limit = Some e; prev_limit' = prev_limit } in
          check ps token tokens env (Grammar_interpr.offer c tok)
      | Shifting (_e1, e2, _) as c ->
          let ps, tokens = push_incoming_contexts ps tokens e2 in
          normal ps tokens @@ Grammar_interpr.resume c
      | Accepted v ->
          accept ps v
      | AboutToReduce _   (* may only happen upon `check` (or empty language) *)
      | Rejected | HandlingError _ ->
          assert false                                 (* should never happen *)

    and on_production ps token tokens prod = function
      | Grammar_interpr.HandlingError env
      | AboutToReduce (env, _)
      | Shifting (_, env, _) ->
          post_production ps token tokens prod env
      | _ ->
          ps, token, tokens

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
             maybe that's a tad too optimistic; if they did we may need to
             report that. *)
          let c = Grammar_interpr.resume c in
          let ps, token, tokens = on_production ps token tokens prod c in
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
          None, report_invalid_syntax Error ps

    and recover ({ prev_limit; _ } as ps) tokens candidates
        ~report_syntax_hints_n_error =
      let ps, (token, tokens) = next_tokens ps tokens in
      let _, _, e as tok = token_n_srcloc_limits ?prev_limit token in
      let ps = { ps with prev_limit = Some e; prev_limit' = prev_limit } in
      match Grammar_recovery.attempt candidates tok with
      | `Fail when ~&token <> Grammar_tokens.EOF ->         (* ignore one token *)
          recover ps tokens candidates ~report_syntax_hints_n_error
      | `Fail when Option.is_none candidates.final ->
          None, report_syntax_hints_n_error ps []        (* unable to recover *)
      | `Fail ->
          let v, assumed = Option.get candidates.final in
          accept (report_syntax_hints_n_error ps assumed) v
      | `Accept (v, assumed) ->
          accept (report_syntax_hints_n_error ps assumed) v
      | `Ok (c, _, visited, assumed) ->
          let ps, tokens = seesaw_context_stack ps tokens visited in
          normal (report_syntax_hints_n_error ps assumed) tokens c

    and accept ps v =
      Some v, ps

    in
    fun ps tokens c -> normal ps tokens c

  let parse ?verbose ?show ~recovery
      (type m) ~(memory: m memory) pp make_checkpoint
    : ('a option, m) output * _ =
    let ps = init_parser ?verbose ?show ~recovery
        ~tokenizer_memory:memory pp in
    let res, ps =
      (* TODO: catch in a deeper context to grab parsed tokens *)
      let ps, tokens = produce_tokens ps in
      let first_pos = match tokens with
        | [] -> Cobol_preproc.position ps.preproc.pp
        | t :: _ -> Cobol_common.Srcloc.start_pos ~@t
      in
      try do_parse ps tokens (make_checkpoint first_pos)
      with e -> None, add_diag (DIAGS.of_exn e) ps
    in
    match memory with
    | Amnesic ->
        Only res, all_diags ps
    | Eidetic ->
        let artifacts = {
          tokens = Tokzr.parsed_tokens ps.preproc.tokzr;
          pplog = Cobol_preproc.log ps.preproc.pp;
          comments = Cobol_preproc.comments ps.preproc.pp;
        } in
        WithArtifacts (res, artifacts), all_diags ps

end

let default_recovery =
  EnableRecovery { silence_benign_recoveries = false }

(* TODO: accept a record instead of many labeled arguments? *)
type 'm parsing_function
  = ?source_format:Cobol_config.source_format_spec
  -> ?config:Cobol_config.t
  -> ?recovery:recovery
  -> ?verbose:bool
  -> ?show:[`Pending] list
  -> libpath:string list
  -> Cobol_preproc.input
  -> (PTree.compilation_group option, 'm) parsed_result

let parse
    (type m)
    ~(memory: m memory)
    ?(source_format = Cobol_config.Auto)
    ?(config = Cobol_config.default)
    ?(recovery = default_recovery)
    ?verbose
    ?show
    ~libpath
  : Cobol_preproc.input -> (PTree.compilation_group option, m) parsed_result =
  let preprocessor = Cobol_preproc.preprocessor ?verbose in
  fun input ->
    let { result = output, parsed_diags; diags = other_diags } =
      Cobol_common.with_stateful_diagnostics input
        ~f:begin fun _init_diags input ->
          let pp = preprocessor input @@
            `WithLibpath Cobol_preproc.{ init_libpath = libpath;
                                         init_config = config;
                                         init_source_format = source_format}
          in
          let module P = Make (val config) in
          P.parse ?verbose ?show ~memory ~recovery pp
          Grammar.Incremental.compilation_group
        end
    in
    {
      parsed_input = input;
      parsed_output = output;
      parsed_diags = DIAGS.Set.union parsed_diags other_diags
    }

let parse_simple = parse ~memory:Amnesic
let parse_with_tokens = parse ~memory:Eidetic

let parsing_artifacts
  : (_, Cobol_common.Behaviors.eidetic) parsed_result -> _ = function
  | { parsed_output = WithArtifacts (_, artifacts); _ } -> artifacts
