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

(** Typing of PROCEDURE DIVISION statements. Typing errors are recovered on the
    nearest enclosing {!Cobol_procedure.Types.or_error} node. *)

open Cobol_procedure.Types
open Typeck_procedure_monad
open Typeck_terms
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

(* --- Operands *)

let typeck_rounded_ident (r : Cobol_ptree.rounded_ident with_loc)
  : rounded_ident Typeck_procedure_monad.t =
  let { Cobol_ptree.rounded; rounded_rounding } = ~&r in
  let+ rounded = typeck_ident (rounded &@<- r) in
  Cobol_ptree.{ rounded; rounded_rounding }

let typeck_rounded_idents ~loc (l : Cobol_ptree.rounded_idents) =
  map_list (fun r -> typeck_rounded_ident (r &@ loc)) l

let typeck_position (p : Cobol_ptree.position with_loc)
  : position Typeck_procedure_monad.t =
  match ~&p with
  | LinePosition i ->
      let+ i = typeck_ident_or_intlit (i &@<- p) in
      LinePosition i
  | ColumnPosition i ->
      let+ i = typeck_ident_or_intlit (i &@<- p) in
      ColumnPosition i
  | LineColumnPosition (i, j) ->
      let+ i = typeck_ident_or_intlit (i &@<- p)
      and+ j = typeck_ident_or_intlit (j &@<- p) in
      LineColumnPosition (i, j)
  | CompoundPosition i ->
      let+ i = typeck_ident_or_intlit (i &@<- p) in
      CompoundPosition i

let typeck_using_arg_default' (a : Cobol_ptree.using_arg_default with_loc)
  : using_arg_default with_loc Typeck_procedure_monad.t =
  let+ a' =
    match ~&a with
    | ArgDefaultOmitted -> return ArgDefaultOmitted
    | ArgDefault e ->
        let+ e = typeck_expr' e in
        ArgDefault e
  in
  a' &@<- a

let typeck_using_reference_arg' (a : Cobol_ptree.using_reference_arg with_loc)
  : using_reference_arg with_loc Typeck_procedure_monad.t =
  let+ a' =
    match ~&a with
    | ArgRefOmitted -> return ArgRefOmitted
    | ArgRef s ->
        let+ s' = typeck_scalar s in
        ArgRef (s' &@<- s)
  in
  a' &@<- a

let typeck_call_using_clause (c : Cobol_ptree.call_using_clause)
  : call_using_clause Typeck_procedure_monad.t =
  match c with
  | CallUsingDefault args ->
      let+ args = map_nel typeck_using_arg_default' args in
      CallUsingDefault args
  | CallUsingByReference args ->
      let+ args = map_nel typeck_using_reference_arg' args in
      CallUsingByReference args
  | CallUsingByContent args ->
      let+ args = map_nel typeck_expr' args in
      CallUsingByContent args
  | CallUsingByValue args ->
      let+ args = map_nel typeck_expr' args in
      CallUsingByValue args

let typeck_retry_clause (r : Cobol_ptree.retry_clause)
  : retry_clause Typeck_procedure_monad.t =
  match r with
  | RetryNTimes e ->
      let+ e = typeck_expr' e in
      RetryNTimes e
  | RetryForNSeconds e ->
      let+ e = typeck_expr' e in
      RetryForNSeconds e
  | RetryForever -> return RetryForever

let typeck_mcs_command_operands (m : Cobol_ptree.mcs_command_operands with_loc)
  : mcs_command_operands Typeck_procedure_monad.t =
  let { mcs_command_kind; mcs_command_target; mcs_command_key }
    : Cobol_ptree.mcs_command_operands =
    ~&m
  in
  let typeck_ident_or_alphanum x = typeck_ident_or_alphanum (x &@<- m) in
  let+ mcs_command_key = map_option typeck_ident_or_alphanum mcs_command_key in
  { mcs_command_kind; mcs_command_target; mcs_command_key }

let typeck_write_target (t : Cobol_ptree.write_target)
  : write_target Typeck_procedure_monad.t =
  match t with
  | WriteTargetName qn ->
      let+ r = typeck_data_ref qn in
      WriteTargetName r
  | WriteTargetFile n -> return (WriteTargetFile n)

let typeck_basic_arithmetic_operands
    (o : Cobol_ptree.basic_arithmetic_operands with_loc)
  : basic_arithmetic_operands Typeck_procedure_monad.t =
  let typeck_scalar x = typeck_scalar (x &@<- o) in
  match ~&o with
  | ArithSimple { operands; targets } ->
      let+ operands = map_list typeck_scalar operands
      and+ targets = typeck_rounded_idents ~loc:~@o targets in
      ArithSimple { operands; targets }
  | ArithGiving { leading_operands; base_operand; targets } ->
      let+ leading_operands = map_list typeck_scalar leading_operands
      and+ base_operand = typeck_scalar base_operand
      and+ targets = typeck_rounded_idents ~loc:~@o targets in
      ArithGiving { leading_operands; base_operand; targets }
  | ArithCorresponding { source; target } ->
      let+ source = typeck_data_ref source
      and+ target = typeck_rounded_ident (target &@<- o) in
      ArithCorresponding { source; target }

let typeck_divide_operands (o : Cobol_ptree.divide_operands with_loc)
  : divide_operands Typeck_procedure_monad.t =
  let typeck_ident x = typeck_ident (x &@<- o) in
  match ~&o with
  | DivideInto { divisor; dividends } ->
      let+ divisor = typeck_scalar (divisor &@<- o)
      and+ dividends = typeck_rounded_idents ~loc:~@o dividends in
      DivideInto { divisor; dividends }
  | DivideGiving { divisor; dividend; giving; into; remainder } ->
      let+ divisor = typeck_scalar (divisor &@<- o)
      and+ dividend = typeck_scalar (dividend &@<- o)
      and+ giving = typeck_rounded_idents ~loc:~@o giving
      and+ remainder = map_option typeck_ident remainder in
      DivideGiving { divisor; dividend; giving; into; remainder }

let typeck_multiply_operands (o : Cobol_ptree.multiply_operands with_loc)
  : multiply_operands Typeck_procedure_monad.t =
  match ~&o with
  | MultiplyBy { multiplier; multiplicand } ->
      let+ multiplier = typeck_scalar (multiplier &@<- o)
      and+ multiplicand = typeck_rounded_idents ~loc:~@o multiplicand in
      MultiplyBy { multiplier; multiplicand }
  | MultiplyGiving { multiplier; multiplicand; targets } ->
      let+ multiplier = typeck_scalar (multiplier &@<- o)
      and+ multiplicand = typeck_scalar (multiplicand &@<- o)
      and+ targets = typeck_rounded_idents ~loc:~@o targets in
      MultiplyGiving { multiplier; multiplicand; targets }

let typeck_raise_operand (r : Cobol_ptree.raise_operand with_loc)
  : raise_operand Typeck_procedure_monad.t =
  match ~&r with
  | RaiseIdent i ->
      let+ i = typeck_ident (i &@<- r) in
      RaiseIdent i
  | RaiseException n -> return (RaiseException n)

let typeck_raising (r : Cobol_ptree.raising with_loc)
  : raising Typeck_procedure_monad.t =
  match ~&r with
  | RaisingIdent i ->
      let+ i = typeck_ident (i &@<- r) in
      RaisingIdent i
  | RaisingException n -> return (RaisingException n)
  | RaisingLastException -> return RaisingLastException

let typeck_read_lock_behavior (b : Cobol_ptree.read_lock_behavior)
  : read_lock_behavior Typeck_procedure_monad.t =
  match b with
  | ReadAdvancingOnLock -> return ReadAdvancingOnLock
  | ReadIgnoringLock -> return ReadIgnoringLock
  | ReadRetry r ->
      let+ r = typeck_retry_clause r in
      ReadRetry r

let typeck_search_condition (c : Cobol_ptree.search_condition)
  : search_condition Typeck_procedure_monad.t =
  match c with
  | IsEqual { data_item; condition } ->
      let+ data_item = typeck_qualident data_item
      and+ condition = typeck_expr' condition in
      IsEqual { data_item; condition }
  | Cond q ->
      let+ q = typeck_qualident q in
      Cond q

let typeck_advancing_phrase (a : Cobol_ptree.advancing_phrase with_loc)
  : advancing_phrase Typeck_procedure_monad.t =
  match ~&a with
  | AdvancingLines { stage; lines; ambiguous } ->
      let+ lines = typeck_ident_or_intlit (lines &@<- a) in
      AdvancingLines { stage; lines; ambiguous }
  | AdvancingPage { stage } -> return (AdvancingPage { stage })

let typeck_set_locale_source (s : Cobol_ptree.set_locale_source with_loc)
  : set_locale_source Typeck_procedure_monad.t =
  match ~&s with
  | SetLocaleSource i ->
      let+ i = typeck_ident (i &@<- s) in
      SetLocaleSource i
  | SetLocaleSourceUserDefault -> return SetLocaleSourceUserDefault
  | SetLocaleSourceSystemDefault -> return SetLocaleSourceSystemDefault

let typeck_start_position (p : Cobol_ptree.start_position)
  : start_position Typeck_procedure_monad.t =
  match p with
  | StartPositionFirst -> return StartPositionFirst
  | StartPositionLast -> return StartPositionLast
  | StartPositionKey { operator; name; length } ->
      let+ name = typeck_data_ref name
      and+ length = map_option typeck_expr' length in
      StartPositionKey { operator; name; length }

let typeck_string_source (s : Cobol_ptree.string_source with_loc)
  : string_source Typeck_procedure_monad.t =
  let { string_source; string_delimiter } : Cobol_ptree.string_source = ~&s in
  let+ string_source = typeck_ident_or_nonnum (string_source &@<- s)
  and+ string_delimiter =
    map_option
      (function
        | Cobol_ptree.StringDelimiter d ->
            let+ d = typeck_ident_or_nonnum (d &@<- s) in
            StringDelimiter d
        | StringDelimiterSize -> return StringDelimiterSize
      )
      string_delimiter
  in
  { string_source; string_delimiter }

let typeck_unstring_delimiter (d : Cobol_ptree.unstring_delimiter with_loc)
  : unstring_delimiter Typeck_procedure_monad.t =
  let { unstring_delimiter; unstring_delimiter_by_all }
    : Cobol_ptree.unstring_delimiter =
    ~&d
  in
  let+ unstring_delimiter =
    typeck_ident_or_strlit (unstring_delimiter &@<- d)
  in
  { unstring_delimiter; unstring_delimiter_by_all }

let typeck_unstring_target (t : Cobol_ptree.unstring_target with_loc)
  : unstring_target Typeck_procedure_monad.t =
  let { unstring_target; unstring_target_delimiter; unstring_target_count }
    : Cobol_ptree.unstring_target =
    ~&t
  in
  let typeck_ident x = typeck_ident (x &@<- t) in
  let+ unstring_target = typeck_ident unstring_target
  and+ unstring_target_delimiter =
    map_option typeck_ident unstring_target_delimiter
  and+ unstring_target_count = map_option typeck_ident unstring_target_count in
  { unstring_target; unstring_target_delimiter; unstring_target_count }

(* --- Statement parts *)

let typeck_screen_attribute_clause
    (c : Cobol_ptree.screen_attribute_clause with_loc)
  : screen_attribute_clause Typeck_procedure_monad.t =
  match ~&c with
  | Bell -> return Bell
  | Blink -> return Blink
  | Highlight -> return Highlight
  | Lowlight -> return Lowlight
  | ReverseVideo -> return ReverseVideo
  | Underline -> return Underline
  | ForegroundColor i ->
      let+ i = typeck_ident_or_intlit (i &@<- c) in
      ForegroundColor i
  | BackgroundColor i ->
      let+ i = typeck_ident_or_intlit (i &@<- c) in
      BackgroundColor i

let typeck_sort_spec
    ({ sort_key_direction; sort_key_names } : Cobol_ptree.sort_spec)
  : sort_spec Typeck_procedure_monad.t =
  let+ sort_key_names = map_list typeck_data_ref sort_key_names in
  { sort_key_direction; sort_key_names }

let typeck_allocate_stmt (a : Cobol_ptree.allocate_stmt with_loc)
  : allocate_stmt Typeck_procedure_monad.t =
  let { allocate_kind; allocate_initialized; allocate_returning }
    : Cobol_ptree.allocate_stmt =
    ~&a
  in
  let+ allocate_kind =
    match allocate_kind with
    | AllocateCharacters e ->
        let+ e = typeck_expr' e in
        AllocateCharacters e
    | AllocateDataItem n ->
        let+ n = typeck_data_name_ref n in
        AllocateDataItem n
  and+ allocate_returning = map_option typeck_ident' allocate_returning in
  { allocate_kind; allocate_initialized; allocate_returning }

let typeck_entry_stmt (e : Cobol_ptree.entry_stmt)
  : entry_stmt Typeck_procedure_monad.t =
  match e with
  | EntrySimple n ->
      let+ n = typeck_entry_name n in
      EntrySimple n
  | EntryUsing { entry_name; entry_by_clauses } ->
      let+ entry_name = typeck_entry_name entry_name
      and+ entry_by_clauses =
        map_list
          (function
            | Cobol_ptree.EntryByReference l ->
                let+ l = map_list typeck_data_name_ref l in
                EntryByReference l
            | EntryByValue l ->
                let+ l = map_list typeck_data_name_ref l in
                EntryByValue l
          )
          entry_by_clauses
      in
      EntryUsing { entry_name; entry_by_clauses }
  | EntryForGoTo n ->
      let+ n = typeck_entry_name n in
      EntryForGoTo n

let typeck_returning (r : Cobol_ptree.returning with_loc)
  : returning Typeck_procedure_monad.t =
  let typeck_literal x = typeck_literal (x &@<- r) in
  match ~&r with
  | ReturningScalar s ->
      let+ s = typeck_scalar (s &@<- r) in
      ReturningScalar s
  | ReturningAddress q ->
      let+ q = typeck_qualident q in
      ReturningAddress q
  | ReturningInt { value; size } ->
      let+ value = typeck_literal value
      and+ size = map_option typeck_literal size in
      ReturningInt { value; size }

let typeck_exit_stmt (e : Cobol_ptree.exit_stmt with_loc)
  : exit_stmt Typeck_procedure_monad.t =
  let typeck_raising x = typeck_raising (x &@<- e) in
  match ~&e with
  | ExitSimple -> return ExitSimple
  | ExitProgram ExitDefault -> return (ExitProgram ExitDefault)
  | ExitProgram (ExitReturning r) ->
      let+ r = typeck_returning (r &@<- e) in
      ExitProgram (ExitReturning r)
  | ExitProgram (ExitRaising r) ->
      let+ r = typeck_raising r in
      ExitProgram (ExitRaising r)
  | ExitMethod r ->
      let+ r = map_option typeck_raising r in
      ExitMethod r
  | ExitFunction r ->
      let+ r = map_option typeck_raising r in
      ExitFunction r
  | ExitPerform b -> return (ExitPerform b)
  | ExitParagraph -> return ExitParagraph
  | ExitSection -> return ExitSection

let typeck_initialize_stmt (i : Cobol_ptree.initialize_stmt with_loc)
  : initialize_stmt Typeck_procedure_monad.t =
  let {
    init_items;
    init_filler;
    init_category;
    init_replacings;
    init_to_default;
  } : Cobol_ptree.initialize_stmt =
    ~&i
  in
  let typeck_ident x = typeck_ident (x &@<- i) in
  let+ init_items = map_list typeck_ident init_items
  and+ init_replacings =
    map_list
      (fun ({ init_replacing_category; init_replacing_replacement_item }
            : Cobol_ptree.init_replacing) ->
        let+ init_replacing_replacement_item =
          typeck_ident_or_literal (init_replacing_replacement_item &@<- i)
        in
        { init_replacing_category; init_replacing_replacement_item }
      )
      init_replacings
  in
  { init_items; init_filler; init_category; init_replacings; init_to_default }

let typeck_invoke_stmt (i : Cobol_ptree.invoke_stmt with_loc)
  : invoke_stmt Typeck_procedure_monad.t =
  let { invoke_target; invoke_method; invoke_using; invoke_returning }
    : Cobol_ptree.invoke_stmt =
    ~&i
  in
  let+ invoke_target = typeck_ident (invoke_target &@<- i)
  and+ invoke_method = typeck_ident_or_strlit (invoke_method &@<- i)
  and+ invoke_using = map_list typeck_call_using_clause invoke_using
  and+ invoke_returning = map_option typeck_ident' invoke_returning in
  { invoke_target; invoke_method; invoke_using; invoke_returning }

let typeck_inspect_stmt (i : Cobol_ptree.inspect_stmt with_loc)
  : inspect_stmt Typeck_procedure_monad.t =
  let { inspect_item; inspect_spec } : Cobol_ptree.inspect_stmt = ~&i in
  let typeck_ident_or_nonnum x = typeck_ident_or_nonnum (x &@<- i) in
  let inspect_where ((direction, reference) : Cobol_ptree.inspect_where) =
    let+ reference = map_nel typeck_ident_or_nonnum reference in
    direction, reference
  in
  let tallying ({ tallying_target; tallying_clauses } : Cobol_ptree.tallying) =
    let+ tallying_target = typeck_qualident tallying_target
    and+ tallying_clauses =
      map_list (fun (c : Cobol_ptree.tallying_clause with_loc) ->
          let+ c' =
            match ~&c with
            | TallyingCharacters wheres ->
                let+ wheres = map_list inspect_where wheres in
                TallyingCharacters wheres
            | TallyingRange (range, specs) ->
                let+ specs =
                  map_list
                    (fun ({ tallying_item; tallying_where }
                          : Cobol_ptree.tallying_spec) ->
                      let+ tallying_item = typeck_ident_or_nonnum tallying_item
                      and+ tallying_where =
                        map_list inspect_where tallying_where
                      in
                      { tallying_item; tallying_where }
                    )
                    specs
                in
                TallyingRange (range, specs)
          in
          c' &@<- c
        )
        tallying_clauses
    in
    { tallying_target; tallying_clauses }
  in
  let replacing (r : Cobol_ptree.replacing) =
    let+ r' =
      match ~&r with
      | ReplacingCharacters { replacement; where } ->
          let+ replacement = typeck_ident_or_nonnum replacement
          and+ where = map_list inspect_where where in
          ReplacingCharacters { replacement; where }
      | ReplacingRange (range, specs) ->
          let+ specs =
            map_list
              (fun ({ replacing_item; replacing_by; replacing_where }
                    : Cobol_ptree.replacing_range_spec) ->
                let+ replacing_item = typeck_ident_or_nonnum replacing_item
                and+ replacing_by = typeck_ident_or_nonnum replacing_by
                and+ replacing_where = map_list inspect_where replacing_where in
                { replacing_item; replacing_by; replacing_where }
              )
              specs
          in
          ReplacingRange (range, specs)
    in
    r' &@<- r
  in
  let+ inspect_item = typeck_ident (inspect_item &@<- i)
  and+ inspect_spec =
    match inspect_spec with
    | InspectTallying tl ->
        let+ tl = map_list tallying tl in
        InspectTallying tl
    | InspectReplacing rl ->
        let+ rl = map_list replacing rl in
        InspectReplacing rl
    | InspectBoth (tl, rl) ->
        let+ tl = map_list tallying tl
        and+ rl = map_list replacing rl in
        InspectBoth (tl, rl)
    | InspectConverting { converting_from; converting_to; converting_where } ->
        let+ converting_from = typeck_ident_or_nonnum converting_from
        and+ converting_to = typeck_ident_or_nonnum converting_to
        and+ converting_where = map_list inspect_where converting_where in
        InspectConverting { converting_from; converting_to; converting_where }
  in
  { inspect_item; inspect_spec }

let typeck_merge_stmt
    ({ merge_file; merge_keys; merge_collating; merge_using; merge_target }
     : Cobol_ptree.merge_stmt)
  : merge_stmt Typeck_procedure_monad.t =
  let+ merge_keys = map_list typeck_sort_spec merge_keys in
  { merge_file; merge_keys; merge_collating; merge_using; merge_target }

let typeck_move_stmt (m : Cobol_ptree.move_stmt with_loc)
  : move_stmt Typeck_procedure_monad.t =
  let typeck_ident x = typeck_ident (x &@<- m) in
  match ~&m with
  | MoveSimple { from; to_ } ->
      let+ from = typeck_scalar (from &@<- m)
      and+ to_ = map_list typeck_ident to_ in
      MoveSimple { from; to_ }
  | MoveCorresponding { from; to_ } ->
      let+ from = typeck_ident from
      and+ to_ = map_list typeck_ident to_ in
      MoveCorresponding { from; to_ }

let typeck_open_stmt (o : Cobol_ptree.open_stmt)
  : open_stmt Typeck_procedure_monad.t =
  map_list
    (fun ({ open_mode; open_sharing; open_retry; open_files }
          : Cobol_ptree.open_phrase) ->
      let+ open_retry = map_option typeck_retry_clause open_retry in
      { open_mode; open_sharing; open_retry; open_files }
    )
    o

let typeck_release_stmt (r : Cobol_ptree.release_stmt with_loc)
  : release_stmt Typeck_procedure_monad.t =
  let { release_item; release_from } : Cobol_ptree.release_stmt = ~&r in
  let typeck_ident_or_literal x = typeck_ident_or_literal (x &@<- r) in
  let+ release_item = typeck_data_ref release_item
  and+ release_from = map_option typeck_ident_or_literal release_from in
  { release_item; release_from }

let typeck_send_stmt (s : Cobol_ptree.send_stmt with_loc)
  : send_stmt Typeck_procedure_monad.t =
  let { send_name; send_operands } : Cobol_ptree.send_stmt = ~&s in
  let typeck_ident x = typeck_ident (x &@<- s) in
  let typeck_advancing_phrase x = typeck_advancing_phrase (x &@<- s) in
  let+ send_operands =
    match send_operands with
    | SendSimple { from } ->
        let+ from = typeck_ident from in
        SendSimple { from }
    | SendWith { from; ending_indicator; advancing; replace } ->
        let+ from = map_option typeck_ident from
        and+ ending_indicator =
          match ending_indicator with
          | EndingIndicator i ->
              let+ i = typeck_ident i in
              EndingIndicator i
          | EndingIndicatorESI -> return EndingIndicatorESI
          | EndingIndicatorEMI -> return EndingIndicatorEMI
          | EndingIndicatorEGI -> return EndingIndicatorEGI
        and+ advancing = map_option typeck_advancing_phrase advancing in
        SendWith { from; ending_indicator; advancing; replace }
  in
  { send_name; send_operands }

let typeck_set_stmt (s : Cobol_ptree.set_stmt with_loc)
  : set_stmt Typeck_procedure_monad.t =
  let typeck_ident x = typeck_ident (x &@<- s) in
  match ~&s with
  | SetAmbiguous { targets; set_method; value } ->
      let+ targets = map_list typeck_ident targets
      and+ value = typeck_expr' value in
      SetAmbiguous { targets; set_method; value }
  | SetSwitch specs ->
      let+ specs =
        map_list
          (fun ({ set_switch_targets; set_switch_value }
                : Cobol_ptree.set_switch_spec) ->
            let+ set_switch_targets = map_list typeck_ident set_switch_targets in
            { set_switch_targets; set_switch_value }
          )
          specs
      in
      SetSwitch specs
  | SetCondition specs ->
      let+ specs =
        map_list
          (fun ({ set_condition_targets; set_condition_value }
                : Cobol_ptree.set_condition_spec) ->
            let+ set_condition_targets =
              map_list typeck_ident set_condition_targets
            in
            { set_condition_targets; set_condition_value }
          )
          specs
      in
      SetCondition specs
  | SetEntry { targets; value } ->
      let+ targets = map_list typeck_ident targets
      and+ value = typeck_ident_or_nonnum (value &@<- s) in
      SetEntry { targets; value }
  | SetAttribute { name; attribute_switches } ->
      return (SetAttribute { name; attribute_switches })
  | SetSaveLocale { target; locale } ->
      let+ target = typeck_ident target in
      SetSaveLocale { target; locale }
  | SetLocale { target; source } ->
      let+ source = typeck_set_locale_source (source &@<- s) in
      SetLocale { target; source }
  | SetSavedException -> return SetSavedException
  | SetFloatContent { targets; content; sign } ->
      let+ targets = map_list typeck_ident targets in
      SetFloatContent { targets; content; sign }
  | SetEnvironment { variable; value } ->
      let+ variable = typeck_ident_or_literal (variable &@<- s)
      and+ value = typeck_ident_or_literal (value &@<- s) in
      SetEnvironment { variable; value }

let typeck_sort_stmt (s : Cobol_ptree.sort_stmt)
  : sort_stmt Typeck_procedure_monad.t =
  match s with
  | SortFile { file; keys; duplicate_in_order; collating; source; target } ->
      let+ keys = map_list typeck_sort_spec keys in
      SortFile { file; keys; duplicate_in_order; collating; source; target }
  | SortTable { table; keys; duplicate_in_order; collating } ->
      let+ table = typeck_qualident table
      and+ keys = map_list typeck_sort_spec keys in
      SortTable { table; keys; duplicate_in_order; collating }

let typeck_stop_stmt (s : Cobol_ptree.stop_stmt with_loc)
  : stop_stmt Typeck_procedure_monad.t =
  let typeck_scalar x = typeck_scalar (x &@<- s) in
  match ~&s with
  | StopArg a ->
      let+ a =
        map_option
          (function
            | Cobol_ptree.StopWithQualIdent ({ payload = QualIdent q; _ } as i) ->
                let+ q = typeck_qualident q in
                StopWithQualIdent (q &@<- i)
            | StopWithLiteral l ->
                let+ l' = typeck_literal l in
                StopWithLiteral (l' &@<- l)
          )
          a
      in
      StopArg a
  | StopRun r ->
      let+ r =
        map_option
          (function
            | Cobol_ptree.StopReturning r ->
                let+ r = typeck_returning (r &@<- s) in
                StopReturning r
            | StopWithStatus { status_kind; status_value } ->
                let+ status_value = map_option typeck_scalar status_value in
                StopWithStatus { status_kind; status_value }
          )
          r
      in
      StopRun r
  | StopError -> return StopError
  | StopThread q ->
      let+ q = map_option typeck_qualident q in
      StopThread q

let typeck_transform_stmt
    ({ transform_ident; transform_from; transform_to }
     : Cobol_ptree.transform_stmt)
  : transform_stmt Typeck_procedure_monad.t =
  let ident_or_nonnum' x =
    let+ x' = typeck_ident_or_nonnum x in
    x' &@<- x
  in
  let+ transform_ident = typeck_ident' transform_ident
  and+ transform_from = ident_or_nonnum' transform_from
  and+ transform_to = ident_or_nonnum' transform_to in
  { transform_ident; transform_from; transform_to }

let typeck_goto_stmt (g : Cobol_ptree.goto_stmt with_loc)
  : goto_stmt Typeck_procedure_monad.t =
  let typeck_ident x = typeck_ident (x &@<- g) in
  match ~&g with
  | GoToSimple { target } -> return (GoToSimple { target })
  | GoToDepending { targets; depending_on } ->
      let+ depending_on = typeck_ident depending_on in
      GoToDepending { targets; depending_on }
  | GoToEntry { targets; depending_on } ->
      let+ targets = map_nel typeck_entry_name targets
      and+ depending_on = map_option typeck_ident depending_on in
      GoToEntry { targets; depending_on }

let typeck_accept_clause' (c : Cobol_ptree.accept_clause with_loc)
  : accept_clause with_loc Typeck_procedure_monad.t =
  let accept_with_clause' (w : Cobol_ptree.accept_with_clause with_loc) =
    let typeck_ident_or_nonnum x = typeck_ident_or_nonnum (x &@<- w) in
    let+ w' =
      match ~&w with
      | AcceptAttribute a ->
          let+ a = typeck_screen_attribute_clause (a &@<- w) in
          AcceptAttribute a
      | AcceptPromptCharacter p ->
          let+ p = map_option typeck_ident_or_nonnum p in
          AcceptPromptCharacter p
      | AcceptSize i ->
          let+ i = typeck_ident_or_intlit (i &@<- w) in
          AcceptSize i
      | AcceptControl i ->
          let+ i = typeck_ident (i &@<- w) in
          AcceptControl i
      | AcceptAuto -> return AcceptAuto
      | AcceptFull -> return AcceptFull
      | AcceptGrid -> return AcceptGrid
      | AcceptLeftLine -> return AcceptLeftLine
      | AcceptOverLine -> return AcceptOverLine
      | AcceptRequired -> return AcceptRequired
      | AcceptSecure -> return AcceptSecure
      | AcceptTimeout -> return AcceptTimeout
      | AcceptLeftJustify -> return AcceptLeftJustify
      | AcceptRightJustify -> return AcceptRightJustify
      | AcceptSpaceFill -> return AcceptSpaceFill
      | AcceptTrailingSign -> return AcceptTrailingSign
      | AcceptUpdate -> return AcceptUpdate
      | AcceptUpper -> return AcceptUpper
      | AcceptLower -> return AcceptLower
      | AcceptZeroFill -> return AcceptZeroFill
    in
    w' &@<- w
  in
  let+ c' =
    match ~&c with
    | AcceptAt p ->
        let+ p = typeck_position (p &@<- c) in
        AcceptAt p
    | AcceptFromCRT -> return AcceptFromCRT
    | AcceptModeBlock -> return AcceptModeBlock
    | AcceptWith clauses ->
        let+ clauses = map_list accept_with_clause' clauses in
        AcceptWith clauses
  in
  c' &@<- c

let typeck_display_clause' (c : Cobol_ptree.display_clause with_loc)
  : display_clause with_loc Typeck_procedure_monad.t =
  let display_with_clause' (w : Cobol_ptree.display_with_clause with_loc) =
    let+ w' =
      match ~&w with
      | DisplayAttribute a ->
          let+ a = typeck_screen_attribute_clause (a &@<- w) in
          DisplayAttribute a
      | DisplaySize i ->
          let+ i = typeck_ident_or_intlit (i &@<- w) in
          DisplaySize i
      | DisplayControl i ->
          let+ i = typeck_ident (i &@<- w) in
          DisplayControl i
      | DisplayErase e -> return (DisplayErase e)
      | DisplayBlank b -> return (DisplayBlank b)
      | DisplayGrid -> return DisplayGrid
      | DisplayLeftLine -> return DisplayLeftLine
      | DisplayOverLine -> return DisplayOverLine
    in
    w' &@<- w
  in
  let+ c' =
    match ~&c with
    | DisplayAt p ->
        let+ p = typeck_position (p &@<- c) in
        DisplayAt p
    | DisplayUpon t -> return (DisplayUpon t)
    | DisplayModeIsBlock -> return DisplayModeIsBlock
    | DisplayWith clauses ->
        let+ clauses = map_list display_with_clause' clauses in
        DisplayWith clauses
  in
  c' &@<- c

(* --- Statements, possibly with nested statements *)

let rec typeck_statement (s : Cobol_ptree.statement with_loc)
  : statement with_loc Typeck_procedure_monad.t =
  let+ s' = typeck_statement_payload s in
  s' &@<- s

and typeck_statements (l : Cobol_ptree.statements)
  : statements Typeck_procedure_monad.t =
  map_list typeck_statement l

and typeck_statement_payload (s : Cobol_ptree.statement with_loc)
  : statement Typeck_procedure_monad.t =
  let typeck_ident_or_strlit x = typeck_ident_or_strlit (x &@<- s) in
  let typeck_ident x = typeck_ident (x &@<- s) in
  match ~&s with
  | Accept a ->
      let+ a = typeck_accept_stmt (a &@<- s) in
      Accept a
  | Add a ->
      let+ a = typeck_basic_arithmetic_stmt (a &@<- s) in
      Add a
  | Allocate a ->
      let+ a = typeck_allocate_stmt (a &@<- s) in
      Allocate a
  | Alter a -> return (Alter a)
  | Call c ->
      let+ c = typeck_call_stmt (c &@<- s) in
      Call c
  | Cancel l ->
      let+ l = map_list typeck_ident_or_strlit l in
      Cancel l
  | Close c -> return (Close c)
  | Compute c ->
      let+ c = typeck_compute_stmt (c &@<- s) in
      Compute c
  | Continue -> return Continue
  | Delete d ->
      let+ d = typeck_delete_stmt d in
      Delete d
  | Disable m ->
      let+ m = typeck_mcs_command_operands (m &@<- s) in
      Disable m
  | Display d ->
      let+ d = typeck_display_stmt d in
      Display d
  | Divide d ->
      let+ d = typeck_divide_stmt (d &@<- s) in
      Divide d
  | Enable m ->
      let+ m = typeck_mcs_command_operands (m &@<- s) in
      Enable m
  | Enter e -> return (Enter e)
  | Entry e ->
      let+ e = typeck_entry_stmt e in
      Entry e
  | Evaluate e ->
      let+ e = typeck_evaluate_stmt e in
      Evaluate e
  | ExecBlock e -> return (ExecBlock e)
  | Exit e ->
      let+ e = typeck_exit_stmt (e &@<- s) in
      Exit e
  | Free l ->
      let+ l = map_list typeck_data_ref l in
      Free l
  | Generate n -> return (Generate n)
  | GoTo g ->
      let+ g = typeck_goto_stmt (g &@<- s) in
      GoTo g
  | GoBack g ->
      let+ g = typeck_goback_stmt (g &@<- s) in
      GoBack g
  | If i ->
      let+ i = typeck_if_stmt i in
      If i
  | Initialize i ->
      let+ i = typeck_initialize_stmt (i &@<- s) in
      Initialize i
  | Initiate l -> return (Initiate l)
  | Inspect i ->
      let+ i = typeck_inspect_stmt (i &@<- s) in
      Inspect i
  | Invoke i ->
      let+ i = typeck_invoke_stmt (i &@<- s) in
      Invoke i
  | LoneGoTo -> return LoneGoTo
  | Merge m ->
      let+ m = typeck_merge_stmt m in
      Merge m
  | Move m ->
      let+ m = typeck_move_stmt (m &@<- s) in
      Move m
  | Multiply m ->
      let+ m = typeck_multiply_stmt (m &@<- s) in
      Multiply m
  | NextSentence -> return NextSentence
  | Open o ->
      let+ o = typeck_open_stmt o in
      Open o
  | PerformTarget p ->
      let+ p = typeck_perform_target_stmt (p &@<- s) in
      PerformTarget p
  | PerformInline p ->
      let+ p = typeck_perform_inline_stmt (p &@<- s) in
      PerformInline p
  | Purge n -> return (Purge n)
  | Raise r ->
      let+ r = typeck_raise_operand (r &@<- s) in
      Raise r
  | Read r ->
      let+ r = typeck_read_stmt (r &@<- s) in
      Read r
  | Receive r ->
      let+ r = typeck_receive_stmt (r &@<- s) in
      Receive r
  | Release r ->
      let+ r = typeck_release_stmt (r &@<- s) in
      Release r
  | Resume r -> return (Resume r)
  | Return r ->
      let+ r = typeck_return_stmt r in
      Return r
  | Rewrite r ->
      let+ r = typeck_rewrite_stmt (r &@<- s) in
      Rewrite r
  | Search r ->
      let+ r = typeck_search_stmt (r &@<- s) in
      Search r
  | SearchAll r ->
      let+ r = typeck_search_all_stmt (r &@<- s) in
      SearchAll r
  | Send r ->
      let+ r = typeck_send_stmt (r &@<- s) in
      Send r
  | Set r ->
      let+ r = typeck_set_stmt (r &@<- s) in
      Set r
  | Sort r ->
      let+ r = typeck_sort_stmt r in
      Sort r
  | Start r ->
      let+ r = typeck_start_stmt r in
      Start r
  | Stop r ->
      let+ r = typeck_stop_stmt (r &@<- s) in
      Stop r
  | String r ->
      let+ r = typeck_string_stmt (r &@<- s) in
      String r
  | Subtract r ->
      let+ r = typeck_basic_arithmetic_stmt (r &@<- s) in
      Subtract r
  | Suppress -> return Suppress
  | Terminate l -> return (Terminate l)
  | Transform t ->
      let+ t = typeck_transform_stmt t in
      Transform t
  | Unlock u -> return (Unlock u)
  | Unstring u ->
      let+ u = typeck_unstring_stmt (u &@<- s) in
      Unstring u
  | Validate l ->
      let+ l = map_list typeck_ident l in
      Validate l
  | Write w ->
      let+ w = typeck_write_stmt (w &@<- s) in
      Write w

and typeck_dual_handler
    ({ dual_handler_pos; dual_handler_neg } : Cobol_ptree.dual_handler) =
  let+ dual_handler_pos = typeck_statements dual_handler_pos
  and+ dual_handler_neg = typeck_statements dual_handler_neg in
  { dual_handler_pos; dual_handler_neg }

and typeck_accept_stmt (a : Cobol_ptree.accept_stmt with_loc)
  : accept_stmt Typeck_procedure_monad.t =
  match ~&a with
  | AcceptGeneric item ->
      let+ item = typeck_ident' item in
      AcceptGeneric item
  | AcceptFromDevice { item; device_item } ->
      let+ item = typeck_ident' item in
      AcceptFromDevice { item; device_item }
  | AcceptTemporal { item; date_time } ->
      let+ item = typeck_ident' item in
      AcceptTemporal { item; date_time }
  | AcceptMisc { item; misc } ->
      let+ item = typeck_ident' item in
      AcceptMisc { item; misc }
  | AcceptMsgCount n -> return (AcceptMsgCount n)
  | AcceptScreen { item; clauses; on_exception } ->
      let+ item = typeck_ident' item
      and+ clauses = map_list typeck_accept_clause' clauses
      and+ on_exception = typeck_dual_handler on_exception in
      AcceptScreen { item; clauses; on_exception }
  | AcceptFromEnv { item; env_item; on_exception } ->
      let+ item = typeck_ident' item
      and+ env_item =
        map_option (fun e ->
            let+ e' = typeck_ident_or_nonnum e in
            e' &@<- e
          )
          env_item
      and+ on_exception = typeck_dual_handler on_exception in
      AcceptFromEnv { item; env_item; on_exception }
  | AcceptFromArg { item; on_exception } ->
      let+ item = typeck_ident' item
      and+ on_exception = typeck_dual_handler on_exception in
      AcceptFromArg { item; on_exception }
  | AcceptFromCmdLine { item; on_exception } ->
      let+ item = typeck_ident' item
      and+ on_exception = typeck_dual_handler on_exception in
      AcceptFromCmdLine { item; on_exception }

and typeck_basic_arithmetic_stmt (a : Cobol_ptree.basic_arithmetic_stmt with_loc)
  : basic_arithmetic_stmt Typeck_procedure_monad.t =
  let { basic_arith_operands; basic_arith_on_size_error }
    : Cobol_ptree.basic_arithmetic_stmt =
    ~&a
  in
  let+ basic_arith_operands =
    typeck_basic_arithmetic_operands (basic_arith_operands &@<- a)
  and+ basic_arith_on_size_error =
    typeck_dual_handler basic_arith_on_size_error
  in
  { basic_arith_operands; basic_arith_on_size_error }

and typeck_call_stmt (c : Cobol_ptree.call_stmt with_loc)
  : call_stmt Typeck_procedure_monad.t =
  let {
    call_static;
    call_target;
    call_using;
    call_returning;
    call_error_handler;
  } : Cobol_ptree.call_stmt =
    ~&c
  in
  let typeck_ident_or_strlit x = typeck_ident_or_strlit (x &@<- c) in
  let+ call_target =
    match call_target with
    | CallGeneral i ->
        let+ i = typeck_ident_or_strlit i in
        CallGeneral i
    | CallProto { called; prototype } ->
        let+ called = map_option typeck_ident_or_strlit called
        and+ prototype =
          match prototype with
          | CallProtoIdent i ->
              let+ i = typeck_ident (i &@<- c) in
              CallProtoIdent i
          | CallProtoNested -> return CallProtoNested
        in
        CallProto { called; prototype }
  and+ call_using = map_list typeck_call_using_clause call_using
  and+ call_returning = map_option typeck_ident' call_returning
  and+ call_error_handler =
    map_option
      (function
        | Cobol_ptree.CallOnOverflow h ->
            let+ h = typeck_statements h in
            CallOnOverflow h
        | CallOnException h ->
            let+ h = typeck_dual_handler h in
            CallOnException h
      )
      call_error_handler
  in
  { call_static; call_target; call_using; call_returning; call_error_handler }

and typeck_compute_stmt (c : Cobol_ptree.compute_stmt with_loc)
  : compute_stmt Typeck_procedure_monad.t =
  let { compute_targets; compute_expr; compute_on_size_error }
    : Cobol_ptree.compute_stmt =
    ~&c
  in
  let+ compute_targets = typeck_rounded_idents ~loc:~@c compute_targets
  and+ compute_expr = typeck_expr' compute_expr
  and+ compute_on_size_error = typeck_dual_handler compute_on_size_error in
  { compute_targets; compute_expr; compute_on_size_error }

and typeck_delete_stmt
    ({ delete_targets; delete_retry; delete_on_invalid_key }
     : Cobol_ptree.delete_stmt)
  : delete_stmt Typeck_procedure_monad.t =
  let+ delete_retry = map_option typeck_retry_clause delete_retry
  and+ delete_on_invalid_key = typeck_dual_handler delete_on_invalid_key in
  { delete_targets; delete_retry; delete_on_invalid_key }

and typeck_display_stmt
    ({ display_items_clauses; no_advancing; on_exception }
     : Cobol_ptree.display_stmt)
  : display_stmt Typeck_procedure_monad.t =
  let+ display_items_clauses =
    map_list
      (fun ({ display_items; display_clauses }
            : Cobol_ptree.display_items_clauses) ->
        let+ display_items =
          map_list (fun i ->
              let+ i' = typeck_ident_or_literal i in
              i' &@<- i
            )
            display_items
        and+ display_clauses =
          map_list typeck_display_clause' display_clauses
        in
        { display_items; display_clauses }
      )
      display_items_clauses
  and+ on_exception = typeck_dual_handler on_exception in
  { display_items_clauses; no_advancing; on_exception }

and typeck_divide_stmt (d : Cobol_ptree.divide_stmt with_loc)
  : divide_stmt Typeck_procedure_monad.t =
  let { divide_operands = operands; divide_on_size_error }
    : Cobol_ptree.divide_stmt =
    ~&d
  in
  let+ divide_operands = typeck_divide_operands (operands &@<- d)
  and+ divide_on_size_error = typeck_dual_handler divide_on_size_error in
  { divide_operands; divide_on_size_error }

and typeck_evaluate_stmt
    ({ eval_subjects; eval_branches; eval_otherwise }
     : Cobol_ptree.evaluate_stmt)
  : evaluate_stmt Typeck_procedure_monad.t =
  let* eval_subjects =
    Typeck_condition.expand_selection_subjects eval_subjects
  in
  let+ eval_branches =
    map_list
      (fun ({ eval_selection; eval_actions }
            : Cobol_ptree.evaluate_branch) ->
        let+ eval_selection =
          map_list
            (Typeck_condition.expand_selection_objects eval_subjects)
            eval_selection
        and+ eval_actions = typeck_statements eval_actions in
        { eval_selection; eval_actions }
      )
      eval_branches
  and+ eval_otherwise = typeck_statements eval_otherwise in
  { eval_subjects; eval_branches; eval_otherwise }

and typeck_goback_stmt (g : Cobol_ptree.goback_stmt with_loc)
  : goback_stmt Typeck_procedure_monad.t =
  let { goback_raising; goback_returning } : Cobol_ptree.goback_stmt = ~&g in
  let typeck_raising x = typeck_raising (x &@<- g) in
  let+ goback_raising = map_option typeck_raising goback_raising
  and+ goback_returning =
    map_option (fun r ->
        let+ r' = typeck_ident_or_intlit r in
        r' &@<- r
      )
      goback_returning
  in
  { goback_raising; goback_returning }

and typeck_if_stmt
    ({ condition; then_branch; else_branch } : Cobol_ptree.if_stmt)
  : if_stmt Typeck_procedure_monad.t =
  let+ condition = Typeck_condition.expand_condition condition
  and+ then_branch = typeck_statements then_branch
  and+ else_branch = typeck_statements else_branch in
  { condition; then_branch; else_branch }

and typeck_multiply_stmt (m : Cobol_ptree.multiply_stmt with_loc)
  : multiply_stmt Typeck_procedure_monad.t =
  let { multiply_operands = operands; multiply_on_size_error }
    : Cobol_ptree.multiply_stmt =
    ~&m
  in
  let+ multiply_operands = typeck_multiply_operands (operands &@<- m)
  and+ multiply_on_size_error = typeck_dual_handler multiply_on_size_error in
  { multiply_operands; multiply_on_size_error }

and typeck_perform_mode (m : Cobol_ptree.perform_mode with_loc)
  : perform_mode Typeck_procedure_monad.t =
  let varying_phrase (v : Cobol_ptree.varying_phrase with_loc) =
    let { varying_ident; varying_from; varying_by; varying_until }
      : Cobol_ptree.varying_phrase =
      ~&v
    in
    let typeck_scalar x = typeck_scalar (x &@<- v) in
    let+ varying_ident = typeck_ident (varying_ident &@<- v)
    and+ varying_from = typeck_scalar varying_from
    and+ varying_by = map_option typeck_scalar varying_by
    and+ varying_until = Typeck_condition.expand_condition varying_until in
    { varying_ident; varying_from; varying_by; varying_until } &@<- v
  in
  match ~&m with
  | PerformNTimes i ->
      let+ i = typeck_ident_or_intlit (i &@<- m) in
      PerformNTimes i
  | PerformUntil { with_test; until } ->
      let+ until = map_option Typeck_condition.expand_condition until in
      PerformUntil { with_test; until }
  | PerformVarying { with_test; varying; after } ->
      let+ varying = varying_phrase varying
      and+ after = map_list varying_phrase after in
      PerformVarying { with_test; varying; after }
  | PerformForever -> return PerformForever

and typeck_perform_target_stmt (p : Cobol_ptree.perform_target_stmt with_loc)
  : perform_target_stmt Typeck_procedure_monad.t =
  let { perform_target; perform_mode = mode }
    : Cobol_ptree.perform_target_stmt =
    ~&p
  in
  let typeck_perform_mode x = typeck_perform_mode (x &@<- p) in
  let+ mode = map_option typeck_perform_mode mode in
  { perform_target; perform_mode = mode }

and typeck_perform_inline_stmt (p : Cobol_ptree.perform_inline_stmt with_loc)
  : perform_inline_stmt Typeck_procedure_monad.t =
  let { perform_inline_mode; perform_statements }
    : Cobol_ptree.perform_inline_stmt =
    ~&p
  in
  let typeck_perform_mode x = typeck_perform_mode (x &@<- p) in
  let+ perform_inline_mode = map_option typeck_perform_mode perform_inline_mode
  and+ perform_statements = typeck_statements perform_statements in
  { perform_inline_mode; perform_statements }

and typeck_read_stmt (r : Cobol_ptree.read_stmt with_loc)
  : read_stmt Typeck_procedure_monad.t =
  let {
    read_file;
    read_direction;
    read_into;
    read_lock_behavior = lock_behavior;
    read_lock;
    read_key;
    read_error_handler;
  } : Cobol_ptree.read_stmt =
    ~&r
  in
  let typeck_ident x = typeck_ident (x &@<- r) in
  let+ read_into = map_option typeck_ident read_into
  and+ read_lock_behavior = map_option typeck_read_lock_behavior lock_behavior
  and+ read_key = map_option typeck_data_ref read_key
  and+ read_error_handler =
    map_option (fun (e, h) ->
        let+ h = typeck_dual_handler h in
        e, h
      )
      read_error_handler
  in
  {
    read_file;
    read_direction;
    read_into;
    read_lock_behavior;
    read_lock;
    read_key;
    read_error_handler;
  }

and typeck_receive_stmt (r : Cobol_ptree.receive_stmt with_loc)
  : receive_stmt Typeck_procedure_monad.t =
  let { receive_name; receive_kind; receive_into; receive_on_no_data }
    : Cobol_ptree.receive_stmt =
    ~&r
  in
  let+ receive_into = typeck_ident (receive_into &@<- r)
  and+ receive_on_no_data = typeck_dual_handler receive_on_no_data in
  { receive_name; receive_kind; receive_into; receive_on_no_data }

and typeck_return_stmt
    ({ return_file; return_into; return_at_end } : Cobol_ptree.return_stmt)
  : return_stmt Typeck_procedure_monad.t =
  let+ return_into = map_option typeck_ident' return_into
  and+ return_at_end = typeck_dual_handler return_at_end in
  { return_file; return_into; return_at_end }

and typeck_rewrite_stmt (r : Cobol_ptree.rewrite_stmt with_loc)
  : rewrite_stmt Typeck_procedure_monad.t =
  let {
    rewrite_to;
    rewrite_from;
    rewrite_retry;
    rewrite_lock;
    rewrite_invalid_key_handler;
  } : Cobol_ptree.rewrite_stmt =
    ~&r
  in
  let typeck_ident_or_literal x = typeck_ident_or_literal (x &@<- r) in
  let+ rewrite_to = typeck_write_target rewrite_to
  and+ rewrite_from = map_option typeck_ident_or_literal rewrite_from
  and+ rewrite_retry = map_option typeck_retry_clause rewrite_retry
  and+ rewrite_invalid_key_handler =
    typeck_dual_handler rewrite_invalid_key_handler
  in
  {
    rewrite_to;
    rewrite_from;
    rewrite_retry;
    rewrite_lock;
    rewrite_invalid_key_handler;
  }

and typeck_search_stmt (s : Cobol_ptree.search_stmt with_loc)
  : search_stmt Typeck_procedure_monad.t =
  let { search_item; search_varying; search_at_end; search_when_clauses }
    : Cobol_ptree.search_stmt =
    ~&s
  in
  let typeck_ident x = typeck_ident (x &@<- s) in
  let+ search_item = typeck_data_ref search_item
  and+ search_varying = map_option typeck_ident search_varying
  and+ search_at_end = typeck_statements search_at_end
  and+ search_when_clauses =
    map_list (fun (w : Cobol_ptree.search_when_clause with_loc) ->
        let { search_when_cond; search_when_stmts }
          : Cobol_ptree.search_when_clause =
          ~&w
        in
        let+ search_when_cond =
          Typeck_condition.expand_condition search_when_cond
        and+ search_when_stmts = typeck_statements search_when_stmts in
        { search_when_cond; search_when_stmts } &@<- w
      )
      search_when_clauses
  in
  { search_item; search_varying; search_at_end; search_when_clauses }

and typeck_search_all_stmt (s : Cobol_ptree.search_all_stmt with_loc)
  : search_all_stmt Typeck_procedure_monad.t =
  let {
    search_all_item;
    search_all_at_end;
    search_all_conditions;
    search_all_action;
  } : Cobol_ptree.search_all_stmt =
    ~&s
  in
  let+ search_all_item = typeck_data_ref search_all_item
  and+ search_all_at_end = typeck_statements search_all_at_end
  and+ search_all_conditions =
    map_list typeck_search_condition search_all_conditions
  and+ search_all_action = typeck_statements search_all_action in
  {
    search_all_item;
    search_all_at_end;
    search_all_conditions;
    search_all_action;
  }

and typeck_start_stmt
    ({ start_file; start_position = position; start_on_invalid_key }
     : Cobol_ptree.start_stmt)
  : start_stmt Typeck_procedure_monad.t =
  let+ position = map_option typeck_start_position position
  and+ start_on_invalid_key = typeck_dual_handler start_on_invalid_key in
  { start_file; start_position = position; start_on_invalid_key }

and typeck_string_stmt (s : Cobol_ptree.string_stmt with_loc)
  : string_stmt Typeck_procedure_monad.t =
  let { string_sources; string_target; string_pointer; string_on_overflow }
    : Cobol_ptree.string_stmt =
    ~&s
  in
  let typeck_string_source x = typeck_string_source (x &@<- s) in
  let typeck_ident x = typeck_ident (x &@<- s) in
  let+ string_sources = map_list typeck_string_source string_sources
  and+ string_target = typeck_ident string_target
  and+ string_pointer = map_option typeck_ident string_pointer
  and+ string_on_overflow = typeck_dual_handler string_on_overflow in
  { string_sources; string_target; string_pointer; string_on_overflow }

and typeck_unstring_stmt (u : Cobol_ptree.unstring_stmt with_loc)
  : unstring_stmt Typeck_procedure_monad.t =
  let {
    unstring_source;
    unstring_delimiters;
    unstring_targets;
    unstring_pointer;
    unstring_tallying;
    unstring_on_overflow;
  } : Cobol_ptree.unstring_stmt =
    ~&u
  in
  let typeck_unstring_target x = typeck_unstring_target (x &@<- u) in
  let typeck_unstring_delimiter x = typeck_unstring_delimiter (x &@<- u) in
  let typeck_ident i = typeck_ident (i &@<- u) in
  let+ unstring_source = typeck_ident unstring_source
  and+ unstring_delimiters =
    map_list typeck_unstring_delimiter unstring_delimiters
  and+ unstring_targets = map_list typeck_unstring_target unstring_targets
  and+ unstring_pointer = map_option typeck_ident unstring_pointer
  and+ unstring_tallying = map_option typeck_ident unstring_tallying
  and+ unstring_on_overflow = typeck_dual_handler unstring_on_overflow in
  {
    unstring_source;
    unstring_delimiters;
    unstring_targets;
    unstring_pointer;
    unstring_tallying;
    unstring_on_overflow;
  }

and typeck_write_stmt (w : Cobol_ptree.write_stmt with_loc)
  : write_stmt Typeck_procedure_monad.t =
  let {
    write_to;
    write_from;
    write_advancing;
    write_retry;
    write_lock;
    write_error_handler;
  } : Cobol_ptree.write_stmt =
    ~&w
  in
  let typeck_ident_or_literal x = typeck_ident_or_literal (x &@<- w) in
  let typeck_advancing_phrase x = typeck_advancing_phrase (x &@<- w) in
  let+ write_to = typeck_write_target write_to
  and+ write_from = map_option typeck_ident_or_literal write_from
  and+ write_advancing = map_option typeck_advancing_phrase write_advancing
  and+ write_retry = map_option typeck_retry_clause write_retry
  and+ write_error_handler =
    map_option (fun (e, h) ->
        let+ h = typeck_dual_handler h in
        e, h
      )
      write_error_handler
  in
  {
    write_to;
    write_from;
    write_advancing;
    write_retry;
    write_lock;
    write_error_handler;
  }

let typeck_sentences (paragraph_sentences: Cobol_ptree.statements with_loc list)
  : statements with_loc list Typeck_procedure_monad.t =
  map_list (fun s ->
      let+ s' = typeck_statements ~&s in
      s' &@<- s
    )
    paragraph_sentences

