(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_common.Srcloc.TYPES
open Cobol_procedure.Types                                  (* for statements *)
open Cobol_data.Types
open Cir_types
open Types

open Syntax

let ok x = Ok x
let errors e = Error e
let error e = errors (NEL.one e)


(** Basic blocks under construction, indexed by labels that may be reserved
    before the blocks they designate are complete. *)
type 'f code =
  {
    next_label: label;
    next_counter: counter;
    blocks: (label * 'f basic_block) list;
  }

(** Errors are kept in the open block only, so that labels may still be reserved
    once an error has been encountered, and the remaining statements checked. *)
type 'f acc =
  {
    code: 'f code;
    current: ('f open_block option, errors) result;
    (* Note: no open block if unreachable (after GO TO) *)
  }

and 'f open_block =
  {
    open_label: label;
    open_rev_stmts: 'f straight_statement with_loc list;
  }

(* --- *)

type ('f, 'r, 'm) build_context =
  {
    env: ('f, 'r, 'm) env;
    procedure: Cobol_unit.Types.procedure;
    enclosing_section: Cobol_unit.Types.section option;
    targets: procedure_name_target Cobol_unit.Qual.MAP.t;
    inline_loop: loop_exits option;
    paragraph_exit: label option;
    section_exit: label option;
    next_sentence: label option;
  }

and procedure_name_target =
  {
    target_label: label;
    target_exit: exit_point;
  }

(** Targets of [EXIT PERFORM] statements. *)
and loop_exits =
  {
    loop_exit: label;
    loop_cycle: label;
  }

(** Elements of a loop level, with optional initialization and step
    statements. *)
type 'f loop_level =
  {
    level_init: ('f straight_statement with_loc, errors) result option;
    level_until: ('f condition, errors) result;
    level_step: ('f straight_statement with_loc, errors) result option;
  }

(** Loop bodies are given their exit label; they must close the current
    block. *)
type 'f loop_body = continue_at:label -> 'f acc -> 'f acc

(* --- *)

let open_block open_label =
  Ok (Some { open_label; open_rev_stmts = [] })

let reserve_label ({ code; _ } as acc) =
  let label = code.next_label in
  label, { acc with code = { code with next_label = succ label } }

let reserve_counter ({ code; _ } as acc) =
  let counter = code.next_counter in
  counter, { acc with code = { code with next_counter = succ counter } }

let append_statement stmt acc =
  let append stmt b = { b with open_rev_stmts = stmt :: b.open_rev_stmts } in
  { acc with current = (let* stmt and* b = acc.current in
                        Ok (Option.map (append stmt) b)) }

let add_error e acc =
  { acc with current = (let+ () = error e and+ b = acc.current in b) }

let close_block ~terminator { code; current } =
  let* terminator and* b = current in
  match b with
  | None ->
      Ok code
  | Some b ->
      let block = { block_stmts = Array.of_list @@ List.rev b.open_rev_stmts;
                    block_term = terminator } in
      Ok { code with blocks = (b.open_label, block) :: code.blocks }

let close_and_continue ~terminator ~current acc =
  match close_block ~terminator acc with
  | Ok code -> { code; current }
  | Error e -> { acc with current = Error e }

let switch_block ~terminator ~next =
  close_and_continue ~terminator ~current:(open_block next)

let end_block ~terminator =
  close_and_continue ~terminator ~current:(Ok None)

(** Opens a block once the current one has been closed. *)
let open_at label acc =
  { acc with current = (let* _ = acc.current in open_block label) }

let append_option stmt acc =
  match stmt with
  | None -> acc
  | Some stmt -> append_statement stmt acc

let goto ~loc label =
  Ok (IR_goto label &@ loc)

let branch ~loc condition ~if_true ~if_false =
  let* condition in
  Ok (IR_branch { condition; if_true; if_false } &@ loc)

let implicit_stop ~loc =
  Ok (IR_stop { optional_status = None } &@ loc)

let blocks code =
  let blocks = Array.make code.next_label None in
  List.iter (fun (label, block) -> blocks.(label) <- Some block) code.blocks;
  Array.map Option.get blocks


let resolve_procedure_name { procedure; enclosing_section; targets; _ }
    (qn: Cobol_ptree.procedure_name with_loc) =
  try
    let full_qn =
      Cobol_unit.Procedure.full_qn ?enclosing_section ~&qn procedure in
    Ok (Cobol_unit.Qual.MAP.find full_qn targets)
  with                  (* Note: shouldn't happen now that typeck checks that *)
  | Not_found ->
      error @@ Undefined { stuff = Procedure_name ~&qn; loc = ~@qn }
  | Cobol_unit.Resolver_map.Ambiguous candidates ->
      error @@ Ambiguous { stuff = Procedure_name ~&qn; loc = ~@qn;
                           candidates = Lazy.force candidates }

let map_nel ~f l =
  let+ rev_l =
    NEL.fold_left ~f:(fun acc x -> let+ y = f x and+ acc in y :: acc) (Ok []) l
  in
  NEL.of_rev_list rev_l

let resolve_procedure_labels ctx names =
  map_nel names ~f:begin fun qn ->
    let+ { target_label; _ } = resolve_procedure_name ctx qn in
    target_label
  end

(** Gives the entry label and exit point of a procedure range. *)
let resolve_procedure_range ctx
    ({ procedure_start; procedure_end }: Cobol_ptree.procedure_range) =
  let+ start = resolve_procedure_name ctx procedure_start
  and+ stop = match procedure_end with
    | None -> Ok None
    | Some qn -> Result.map Option.some @@ resolve_procedure_name ctx qn
  in
  start.target_label, (Option.value stop ~default:start).target_exit


type 'f block_builder = 'f acc Cobol_unit.Visitor.folder

let translate_statements (visitor: _ block_builder) statements acc =
  Cobol_procedure.Visitor.fold_statements visitor statements acc

let translate_if_statement { env; _ } visitor stmt acc =
  let if_true, acc = reserve_label acc in
  let if_false, acc = reserve_label acc in
  let join, acc = reserve_label acc in
  let branch =
    let+ condition = Expr.resolve_condition env ~&stmt.condition in
    IR_branch { condition; if_true; if_false } &@<- stmt
  in
  let goto_join = Ok (IR_goto join &@<- stmt) in
  switch_block ~terminator:branch ~next:if_true acc |>
  translate_statements visitor ~&stmt.then_branch |>
  switch_block ~terminator:goto_join ~next:if_false |>
  translate_statements visitor ~&stmt.else_branch |>
  switch_block ~terminator:goto_join ~next:join

let translate_goto_statement ({ env; _ } as ctx) stmt acc =
  match ~&stmt with
  | GoToSimple { target } ->
      let terminator =
        let+ { target_label; _ } = resolve_procedure_name ctx target in
        IR_goto target_label &@<- stmt
      in
      end_block ~terminator acc
  | GoToDepending { targets; depending_on } ->
      let default, acc = reserve_label acc in
      let terminator =
        let+ targets = resolve_procedure_labels ctx targets
        and+ selector = Expr.resolve_ident env (depending_on &@<- stmt) in
        IR_goto_depending { selector; targets; default } &@<- stmt
      in
      switch_block ~terminator ~next:default acc
  | GoToEntry _ ->
      add_error (Unsupported { stuff = Statement (GoTo ~&stmt);
                               loc = ~@stmt }) acc

let reserve_step_label level ~test acc =
  match level.level_step with
  | None -> test, acc
  | Some _ -> reserve_label acc

let emit_step ?reinit ~loc level ~label ~next acc =
  match level.level_step with
  | None ->
      acc
  | Some step ->
      open_at label acc |>
      append_statement step |>
      append_option (Option.join reinit) |>
      end_block ~terminator:(goto ~loc next)

(** Inner levels are initialized upon entry, and then re-initialized right after
    every step of their outer level. *)
let loop_with_test_before ~loc ~(body: _ loop_body) ~exit levels acc =
  let rec aux ~exit (level, inner_levels) acc =
    let test, acc = reserve_label acc in
    let enter, acc = reserve_label acc in
    let step, acc = reserve_step_label level ~test acc in
    let until = branch ~loc level.level_until ~if_true:exit ~if_false:enter in
    let acc =
      switch_block ~terminator:(goto ~loc test) ~next:test acc |>
      switch_block ~terminator:until ~next:enter
    in
    match inner_levels with
    | [] ->
        body ~continue_at:step acc |>
        emit_step ~loc level ~label:step ~next:test
    | l :: ls ->
        aux ~exit:step (l, ls) acc |>
        emit_step ~reinit:l.level_init ~loc level ~label:step ~next:test
  in
  let level, inner_levels = levels in
  List.fold_left begin fun acc l ->
    append_option l.level_init acc
  end acc (level :: inner_levels) |>
  aux ~exit levels

let rec loop_with_test_after ~loc ~(body: _ loop_body) ~exit
    (level, inner_levels) acc =
  let start, acc = reserve_label acc in
  let test, acc = reserve_label acc in
  let acc =
    append_option level.level_init acc |>
    switch_block ~terminator:(goto ~loc start) ~next:start
  in
  let acc =
    match inner_levels with
    | [] ->
        body ~continue_at:test acc
    | l :: ls ->
        loop_with_test_after ~loc ~body ~exit:test (l, ls) acc
  in
  let step, acc = reserve_step_label level ~test:start acc in
  let until = branch ~loc level.level_until ~if_true:exit ~if_false:step in
  open_at test acc |> end_block ~terminator:until |>
  emit_step ~loc level ~label:step ~next:start

let loop_times { env; _ } ~loc ~(body: _ loop_body) ~exit count acc =
  let counter, acc = reserve_counter acc in
  let test, acc = reserve_label acc in
  let enter, acc = reserve_label acc in
  let set_counter =
    let+ count =
      match count with
      | Ident i ->
          Expr.resolve_ident env (i &@ loc)
      | Literal Ok i ->
          Literal.create ~builder:env.builder (Integer_value i &@ loc)
      | Literal Error () ->               (* invalid literal, already reported *)
          error @@ Unsupported { stuff = Scalar (ScalarLiteral (Error ()));
                                 loc }
    in
    IR_set_counter { counter; count; next = test } &@ loc
  in
  let count_down =
    ok (IR_count_down { counter; if_positive = enter; if_zero = exit } &@ loc)
  in
  switch_block ~terminator:set_counter ~next:test acc |>
  switch_block ~terminator:count_down ~next:enter |>
  body ~continue_at:test

let loop_forever ~loc ~(body: _ loop_body) acc =
  let start, acc = reserve_label acc in
  switch_block ~terminator:(goto ~loc start) ~next:start acc |>
  body ~continue_at:start

let varying_level { env; _ } (v: varying_phrase with_loc) =
  let { varying_ident; varying_from; varying_by; varying_until } = ~&v in
  let var = Expr.resolve_ident env (varying_ident &@<- v) in
  let init =
    let+ { field_ref; field_ref_loc } = var
    and+ source =
      Expr.resolve_scalar_data_reference env (varying_from &@<- v)
    in
    let target = { data_field = field_ref;
                   data_ref_loc = field_ref_loc;
                   data_refmod = None } in
    IR_move { source; targets = NEL.one target } &@<- v
  in
  let step =
    match var with
    | Error _ ->                            (* errors are reported with [init] *)
        None
    | Ok target ->
        Option.some @@
        let+ operand =
          match varying_by with
          | Some by ->
              Expr.resolve_scalar env (by &@<- v)
          | None ->
              Literal.create ~builder:env.builder (Integer_value Z.one &@<- v)
        in
        let targets = NEL.one { target; rounded = false } in
        IR_arithmetic { operator = Add; operand; targets } &@<- v
  in
  { level_init = Some init;
    level_until = Expr.resolve_condition env varying_until;
    level_step = step }

(** Translates a PERFORM statement with the given [mode]. [body] is given the
    label of the statements that follow the loop. *)
let translate_loop ({ env; _ } as ctx) ~loc ~mode
    ~(body: exit:label -> _ loop_body) acc =
  let exit, acc = reserve_label acc in
  let body = body ~exit in
  let loop ~with_test levels =
    match with_test with
    | None | Some Cobol_ptree.Before ->
        loop_with_test_before ~loc ~body ~exit levels
    | Some After ->
        loop_with_test_after ~loc ~body ~exit levels
  in
  open_at exit @@ match mode with
  | None ->
      body ~continue_at:exit acc
  | Some (PerformForever | PerformUntil { until = None; _ }) ->
      loop_forever ~loc ~body acc
  | Some PerformNTimes count ->
      loop_times ctx ~loc ~body ~exit count acc
  | Some PerformUntil { with_test; until = Some condition } ->
      let level = { level_init = None;
                    level_until = Expr.resolve_condition env condition;
                    level_step = None } in
      loop ~with_test (level, []) acc
  | Some PerformVarying { with_test; varying; after } ->
      let levels = varying_level ctx varying,
                   List.map (varying_level ctx) after in
      loop ~with_test levels acc

let performed_body ctx ~loc range =
  let range = resolve_procedure_range ctx range in
  fun ~exit:_ ~continue_at acc ->
    let terminator =
      let+ entry, exit_point = range in
      IR_perform { entry; exit_point; return_to = continue_at } &@ loc
    in
    end_block ~terminator acc

let inline_body ~loc ~visitor statements ~exit ~continue_at acc =
  let visitor = visitor { loop_exit = exit; loop_cycle = continue_at } in
  Cobol_procedure.Visitor.fold_statements visitor statements acc |>
  end_block ~terminator:(goto ~loc continue_at)

let translate_jump ~loc ~unexpected target acc =
  match target with
  | Some label ->
      end_block ~terminator:(goto ~loc label) acc
  | None ->
      add_error (Unexpected { stuff = unexpected; loc }) acc

let translate_exit_statement ctx stmt acc =
  let loc = ~@stmt in
  let[@local] jump target =
    translate_jump ~loc ~unexpected:Exit_perform_outside_inline_perform
      target acc
  in
  match ~&stmt with
  | ExitSimple ->
      acc
  | ExitPerform cycle ->
      jump @@ Option.map begin fun { loop_exit; loop_cycle } ->
        if cycle then loop_cycle else loop_exit
      end ctx.inline_loop
  | ExitParagraph ->
      end_block ~terminator:(goto ~loc (Option.get ctx.paragraph_exit)) acc
  | ExitSection ->
      end_block ~terminator:(goto ~loc (Option.get ctx.section_exit)) acc
  | ExitProgram ExitDefault ->
      let fallthrough, acc = reserve_label acc in
      switch_block acc ~next:fallthrough
        ~terminator:(Ok (IR_exit_program { fallthrough } &@ loc))
  | ExitProgram (ExitReturning _ | ExitRaising _)
  | ExitMethod _
  | ExitFunction _ ->
      add_error (Unsupported { stuff = Statement (Exit ~&stmt); loc }) acc

let translate_display_statement env stmt =
  let+ rev_fields =
    List.fold_left begin fun acc { display_items; _ } ->
      List.fold_left begin fun acc terminator ->
        let+ field = Expr.resolve_data_reference env terminator and+ acc in
        field :: acc
      end acc display_items
    end (Ok []) ~&stmt.display_items_clauses
  in
  IR_display { data_refs = Array.of_list @@ List.rev rev_fields;
               advancing = not ~&stmt.no_advancing } &@<- stmt

let translate_move_statement env stmt =
  match ~&stmt with
  | MoveSimple { from; to_ } ->
      let+ source = Expr.resolve_scalar_data_reference env (from &@<- stmt)
      and+ targets =
        map_nel (NEL.of_list to_) ~f:begin fun i ->
          Expr.resolve_data_reference env (Ident i &@<- stmt)
        end
      in
      IR_move { source; targets } &@<- stmt
  | MoveCorresponding _ ->
      error @@ Unsupported { stuff = Statement (Move ~&stmt); loc = ~@stmt }

let translate_arithmetic env ~operator ~statement stmt =
  let unsupported =
    Unsupported { stuff = Statement (statement ~&stmt); loc = ~@stmt }
  in
  let translate_target (t: ident Cobol_ptree.rounded) =
    let+ target = Expr.resolve_ident env (t.rounded &@<- stmt)
    and+ rounded = match t.rounded_rounding with
      | RoundingNotAny -> Ok false
      | RoundingDefault -> Ok true
      | RoundingMode _ -> error unsupported
    in
    { target; rounded }
  in
  match ~&stmt.basic_arith_operands with
  | ArithSimple { operands = [operand]; targets } ->
      let+ operand = Expr.resolve_scalar env (operand &@<- stmt)
      and+ targets = map_nel (NEL.of_list targets) ~f:translate_target in
      { operator; operand; targets }
  | _ ->
      error unsupported

let translate_arithmetic_statement { env; _ } visitor ~operator ~statement
    stmt acc =
  let loc = ~@stmt in
  let arithmetic = translate_arithmetic env ~operator ~statement stmt in
  match ~&stmt.basic_arith_on_size_error with
  | { dual_handler_pos = []; dual_handler_neg = [] } ->
      append_statement (let+ a = arithmetic in IR_arithmetic a &@ loc) acc
  | { dual_handler_pos; dual_handler_neg } ->
      let join, acc = reserve_label acc in
      let handler_label statements acc =
        match statements with
        | [] -> join, acc
        | _ -> reserve_label acc
      in
      let on_size_error, acc = handler_label dual_handler_pos acc in
      let not_on_size_error, acc = handler_label dual_handler_neg acc in
      let terminator =
        let+ arithmetic in
        IR_checked_arithmetic { arithmetic; on_size_error;
                                not_on_size_error } &@ loc
      in
      let translate_handler label statements acc =
        match statements with
        | [] -> acc
        | _ -> open_at label acc |> translate_statements visitor statements |>
               end_block ~terminator:(goto ~loc join)
      in
      end_block ~terminator acc |>
      translate_handler on_size_error dual_handler_pos |>
      translate_handler not_on_size_error dual_handler_neg |>
      open_at join

let translate_stop_statement env stmt =
  match ~&stmt with
  | StopRun None ->
      Ok (IR_stop { optional_status = None } &@<- stmt)
  | StopArg Some StopWithQualIdent qi ->
      let+ { field_ref; field_ref_loc } = Expr.resolve_qualident env qi in
      let f = { data_field = field_ref;
                data_ref_loc = field_ref_loc;
                data_refmod = None } in
      IR_stop { optional_status = Some f } &@<- stmt
  | StopRun Some _
  | StopArg _
  | StopError
  | StopThread _ ->
      error @@ Unsupported { stuff = Statement (Stop ~&stmt); loc = ~@stmt }

let rec block_builder ({ env; _ } as ctx) : _ block_builder =
  object (visitor)
    inherit [_] Cobol_unit.Visitor.folder
    (* Supported statements are translated by the methods below, that must not
       visit their children. *)
    method! fold_statement' s acc =
      match ~&s with
      | Continue ->
          Cobol_common.Visitor.skip acc
      | NextSentence ->
          Cobol_common.Visitor.skip @@
          end_block ~terminator:(goto ~loc:~@s (Option.get ctx.next_sentence)) acc
      | Add _ | Display _ | Exit _ | GoBack _ | GoTo _ | If _ | Move _
      | PerformInline _ | PerformTarget _ | Stop _ | Subtract _ ->
          Cobol_common.Visitor.do_children acc
      | _ ->                                         (* explicitly unsupported *)
          Cobol_common.Visitor.skip @@
          add_error (Unsupported { stuff = Statement ~&s; loc = ~@s }) acc

    method! fold_if' s acc =
      Cobol_common.Visitor.skip @@
      translate_if_statement ctx visitor s acc

    method! fold_display' s acc =
      Cobol_common.Visitor.skip @@
      append_statement (translate_display_statement env s) acc

    method! fold_move' s acc =
      Cobol_common.Visitor.skip @@
      append_statement (translate_move_statement env s) acc

    method! fold_add' s acc =
      Cobol_common.Visitor.skip @@
      translate_arithmetic_statement ctx visitor s acc ~operator:Add
        ~statement:(fun s -> Cobol_procedure.Types.Add s)

    method! fold_subtract' s acc =
      Cobol_common.Visitor.skip @@
      translate_arithmetic_statement ctx visitor s acc ~operator:Subtract
        ~statement:(fun s -> Cobol_procedure.Types.Subtract s)

    method! fold_goback' s acc =
      Cobol_common.Visitor.skip @@
      match ~&s with
      | { goback_raising = None; goback_returning = None } ->
          end_block ~terminator:(Ok (IR_goback &@<- s)) acc
      | _ ->
          add_error (Unsupported { stuff = Statement (GoBack ~&s);
                                   loc = ~@s }) acc

    method! fold_goto' s acc =
      Cobol_common.Visitor.skip @@
      translate_goto_statement ctx s acc

    method! fold_perform_target' s acc =
      let loc = ~@s in
      Cobol_common.Visitor.skip @@
      translate_loop ctx ~loc ~mode:~&s.perform_mode acc
        ~body:(performed_body ctx ~loc ~&s.perform_target)

    method! fold_perform_inline' s acc =
      let loc = ~@s in
      let visitor inline_loop =
        block_builder { ctx with inline_loop = Some inline_loop }
      in
      Cobol_common.Visitor.skip @@
      translate_loop ctx ~loc ~mode:~&s.perform_inline_mode acc
        ~body:(inline_body ~loc ~visitor ~&s.perform_statements)

    method! fold_exit' s acc =
      Cobol_common.Visitor.skip @@
      translate_exit_statement ctx s acc

    method! fold_stop' s acc =
      Cobol_common.Visitor.skip @@
      end_block ~terminator:(translate_stop_statement env s) acc
  end

type paragraph_info =
  {
    section: Cobol_unit.Types.section with_loc;
    section_label: label;
    paragraph: Cobol_unit.Types.paragraph with_loc;
    label: label;
    last_in_section: bool;
  }

(** Assigns labels to paragraphs in source order, and gives every section the
    label of its first paragraph. *)
let label_paragraphs (procedure: Cobol_unit.Types.procedure) =
  let add_target qn target targets =
    match qn with
    | Some qn -> Cobol_unit.Qual.MAP.add qn target targets
    | None -> targets
  in
  let label_section (next_label, targets, rev_infos)
      (section: Cobol_unit.Types.section with_loc) =
    let section_label = next_label in
    let name =
      Option.map (fun n -> Cobol_ptree.Name n) ~&section.section_name
    in
    let targets =
      add_target name
        { target_label = section_label;
          target_exit = Section_end section_label } targets
    in
    let rec label_paragraphs (label, targets, rev_infos) = function
      | [] ->
          label, targets, rev_infos
      | (paragraph: Cobol_unit.Types.paragraph with_loc) :: tl ->
          let targets =
            add_target ~&?(~&paragraph.paragraph_name)
              { target_label = label;
                target_exit = Paragraph_end label } targets
          in
          let info = { section; section_label; paragraph; label;
                       last_in_section = tl = [] } in
          label_paragraphs (succ label, targets, info :: rev_infos) tl
    in
    label_paragraphs (next_label, targets, rev_infos)
      ~&section.section_paragraphs.list
  in
  let next_label, targets, rev_infos =
    List.fold_left label_section (0, Cobol_unit.Qual.MAP.empty, [])
      procedure.procedure_sections
  in
  next_label, targets, List.rev rev_infos

let paragraph_context ~env ~procedure ~targets { section; _ } =
  { env; procedure; targets; enclosing_section = Some ~&section;
    inline_loop = None; paragraph_exit = None; section_exit = None;
    next_sentence = None }

let fold_sentences visitor
    (paragraph: Cobol_unit.Types.paragraph with_loc) acc =
  List.fold_left begin fun acc sentence ->
    Cobol_procedure.Visitor.fold_statements' visitor sentence acc
  end acc ~&paragraph.paragraph_sentences

(** Exit points of every PERFORM, ignoring resolution errors that are reported
    upon translation. *)
let performed_exit_points ~env ~procedure ~targets paragraphs =
  List.fold_left begin fun exit_points info ->
    let ctx = paragraph_context ~env ~procedure ~targets info in
    let visitor = object
      inherit [_] Cobol_unit.Visitor.folder
      method! fold_perform_target' s exit_points =
        Cobol_common.Visitor.skip @@
        match resolve_procedure_range ctx ~&s.perform_target with
        | Ok (_, exit_point) -> exit_point :: exit_points
        | Error _ -> exit_points
    end in
    fold_sentences visitor info.paragraph exit_points
  end [] paragraphs

(** Statements that jump to the end of an enclosing sentence, paragraph, or
    section. *)
type jumps =
  {
    exit_paragraph: bool;
    exit_section: bool;
    next_sentence: bool;
  }

let no_jumps =
  { exit_paragraph = false; exit_section = false; next_sentence = false }

let jumps_finder = object
  inherit [_] Cobol_unit.Visitor.folder
  method! fold_statement' s jumps =
    match ~&s with
    | NextSentence ->
        Cobol_common.Visitor.skip { jumps with next_sentence = true }
    | Exit ExitParagraph ->
        Cobol_common.Visitor.skip { jumps with exit_paragraph = true }
    | Exit ExitSection ->
        Cobol_common.Visitor.skip { jumps with exit_section = true }
    | _ ->
        Cobol_common.Visitor.do_children jumps
end

let sentence_jumps sentence =
  Cobol_procedure.Visitor.fold_statements' jumps_finder sentence no_jumps

let paragraph_jumps paragraph =
  fold_sentences jumps_finder paragraph no_jumps

let section_jumps (section: Cobol_unit.Types.section with_loc) =
  List.fold_left (Fun.flip @@ fold_sentences jumps_finder) no_jumps
    ~&section.section_paragraphs.list

let reserve_label_if used acc =
  if used
  then let label, acc = reserve_label acc in Some label, acc
  else None, acc

let translate_sentence ctx sentence acc =
  let next_sentence, acc =
    reserve_label_if (sentence_jumps sentence).next_sentence acc
  in
  let acc =
    Cobol_procedure.Visitor.fold_statements'
      (block_builder { ctx with next_sentence }) sentence acc
  in
  match next_sentence with
  | None -> acc
  | Some label ->
      switch_block ~terminator:(goto ~loc:~@sentence label) ~next:label acc

(** What follows the end of a paragraph. *)
type continuation =
  | Fall_into of label
  | End_of_procedure of srcloc

(** Closes the current block, with an exit check for the given [exit_points],
    if any. *)
let close_with_exit_check ~loc ~exit_points continuation acc =
  let exit_check ~fallthrough =
    let exit_points = NEL.of_list exit_points in
    Ok (IR_perform_exit { exit_points; fallthrough } &@ loc)
  in
  match exit_points, continuation with
  | [], Fall_into label ->
      end_block ~terminator:(goto ~loc label) acc
  | [], End_of_procedure loc ->
      end_block ~terminator:(implicit_stop ~loc) acc
  | _, Fall_into fallthrough ->
      end_block ~terminator:(exit_check ~fallthrough) acc
  | _, End_of_procedure stop_loc ->
      let final, acc = reserve_label acc in
      switch_block ~terminator:(exit_check ~fallthrough:final) ~next:final acc |>
      end_block ~terminator:(implicit_stop ~loc:stop_loc)

(** Note: EXIT SECTION skips the exit check of the last paragraph. *)
let translate_paragraph ctx ~performed ~continuation info acc =
  let loc = ~@(info.paragraph) in
  let paragraph_exit, acc =
    reserve_label_if (paragraph_jumps info.paragraph).exit_paragraph acc
  in
  let ctx = { ctx with paragraph_exit } in
  let acc =
    List.fold_left (Fun.flip @@ translate_sentence ctx) (open_at info.label acc)
      ~&(info.paragraph).paragraph_sentences
  in
  let acc =
    match paragraph_exit with
    | None -> acc
    | Some label -> switch_block ~terminator:(goto ~loc label) ~next:label acc
  in
  let performed = List.filter (fun e -> List.mem e performed) in
  let paragraph_exits = performed [Paragraph_end info.label]
  and section_exits =
    if info.last_in_section
    then performed [Section_end info.section_label]
    else []
  in
  match ctx.section_exit with
  | Some section_exit when info.last_in_section ->
      close_with_exit_check ~loc ~exit_points:paragraph_exits
        (Fall_into section_exit) acc |>
      open_at section_exit |>
      close_with_exit_check ~loc ~exit_points:section_exits continuation
  | _ ->
      close_with_exit_check ~loc ~exit_points:(paragraph_exits @ section_exits)
        continuation acc

let translate env ~loc (procedure: Cobol_unit.Types.procedure)
  : (_ Cir_types.procedure, _) result =
  let proc_entry = 0 in
  let next_label, targets, paragraphs = label_paragraphs procedure in
  let performed = performed_exit_points ~env ~procedure ~targets paragraphs in
  let acc =
    match paragraphs with
    | [] ->
        { code = { next_label = succ proc_entry; next_counter = 0;
                   blocks = [] };
          current = open_block proc_entry }
    | _ ->
        { code = { next_label; next_counter = 0; blocks = [] };
          current = Ok None }
  in
  let rec translate_paragraphs ~section_exit acc = function
    | [] ->
        acc
    | info :: tl ->
        let section_exit, acc =
          if info.label = info.section_label
          then reserve_label_if (section_jumps info.section).exit_section acc
          else section_exit, acc
        in
        let continuation = match tl with
          | [] -> End_of_procedure loc
          | next :: _ -> Fall_into next.label
        in
        let ctx = paragraph_context ~env ~procedure ~targets info in
        let ctx = { ctx with section_exit } in
        let acc = translate_paragraph ctx ~performed ~continuation info acc in
        translate_paragraphs ~section_exit acc tl
  in
  let* code =
    close_block ~terminator:(implicit_stop ~loc) @@
    translate_paragraphs ~section_exit:None acc paragraphs
  in
  Ok { proc_blocks = blocks code; proc_entry }
