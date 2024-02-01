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

open Types

open Cobol_common.Srcloc.INFIX
open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Terms_visitor
open Operands_visitor

let todo    x = Cobol_common.Visitor.todo    __FILE__ x
let partial x = Cobol_common.Visitor.partial __FILE__ x

(* --- *)

class virtual ['a] folder = object
  inherit ['a] Operands_visitor.folder
  method fold_statement'          : (statement with_loc      , 'a) fold = default
  method fold_statements'         : (statements with_loc     , 'a) fold = default

  (* Statement-specific operands; shared ones should be in
     Operands_visitor. *)
  method fold_perform_mode        : (perform_mode            , 'a) fold = default
  method fold_varying_phrase      : (varying_phrase          , 'a) fold = default
  method fold_varying_phrase'     : (varying_phrase with_loc , 'a) fold = default
  method fold_allocate_kind       : (allocate_kind           , 'a) fold = default
  method fold_alter_operands'     : (alter_operands with_loc , 'a) fold = default
  method fold_call_prefix         : (call_prefix             , 'a) fold = default
  method fold_call_proto          : (call_proto              , 'a) fold = default
  method fold_close_format        : (close_format            , 'a) fold = default
  method fold_close_phrase        : (close_phrase            , 'a) fold = default
  (* method fold_display_target'     : (display_target with_loc , 'a) fold = default *)
  method fold_init_data_category  : (init_data_category      , 'a) fold = default
  method fold_init_replacing      : (init_replacing          , 'a) fold = default
  method fold_named_file_option   : (named_file_option       , 'a) fold = default
  (*for inspect*)
  method fold_inspect_spec        : (inspect_spec            , 'a) fold = default
  method fold_tallying            : (tallying                , 'a) fold = default
  method fold_tallying_clause'    : (tallying_clause with_loc, 'a) fold = default
  method fold_tallying_spec       : (tallying_spec           , 'a) fold = default
  method fold_replacing           : (replacing               , 'a) fold = default
  method fold_replacing_clause    : (replacing_clause        , 'a) fold = default
  method fold_replacing_range_spec: (replacing_range_spec    , 'a) fold = default
  method fold_converting          : (converting              , 'a) fold = default
  method fold_inspect_where       : (inspect_where           , 'a) fold = default
  method fold_merge_or_sort_target: (merge_or_sort_target    , 'a) fold = default
  method fold_open_phrase         : (open_phrase             , 'a) fold = default
  method fold_read_error          : (read_error              , 'a) fold = default
  method fold_stop_run            : (stop_run                , 'a) fold = default
  method fold_stop_kind           : (stop_kind               , 'a) fold = default
  method fold_string_source       : (string_source           , 'a) fold = default
  method fold_string_delimiter    : (string_delimiter        , 'a) fold = default
  method fold_evaluate_branch     : (evaluate_branch         , 'a) fold = default
  method fold_selection_subject   : (selection_subject       , 'a) fold = default
  method fold_selection_object    : (selection_object        , 'a) fold = default
  method fold_set_switch_spec     : (set_switch_spec         , 'a) fold = default
  method fold_set_condition_spec  : (set_condition_spec      , 'a) fold = default
  method fold_unstring_delimiter  : (unstring_delimiter      , 'a) fold = default
  method fold_unstring_target     : (unstring_target         , 'a) fold = default
  method fold_write_error         : (write_error             , 'a) fold = default

  (* high-level structures and branches *)
  method fold_handler'            : (handler with_loc           , 'a) fold = default
  method fold_dual_handler        : (dual_handler               , 'a) fold = default
  method fold_call_error_handler  : (call_error_handler         , 'a) fold = default
  method fold_search_when_clause' : (search_when_clause with_loc, 'a) fold = default
  method fold_read_error_handler  : (read_error * dual_handler  , 'a) fold = default
  method fold_write_error_handler : (write_error * dual_handler , 'a) fold = default

  (* Individial statements with locations *)
  method fold_accept'        : (accept_stmt with_loc          , 'a) fold = default
  method fold_allocate'      : (allocate_stmt with_loc        , 'a) fold = default
  method fold_alter'         : (alter_stmt with_loc           , 'a) fold = default
  method fold_add'           : (basic_arithmetic_stmt with_loc, 'a) fold = default
  method fold_call'          : (call_stmt with_loc            , 'a) fold = default
  method fold_cancel'        : (ident_or_strlit list with_loc , 'a) fold = default
  method fold_close'         : (close_stmt with_loc           , 'a) fold = default
  method fold_compute'       : (compute_stmt with_loc         , 'a) fold = default
  method fold_delete'        : (delete_stmt with_loc          , 'a) fold = default
  method fold_display'       : (display_stmt with_loc         , 'a) fold = default
  method fold_divide'        : (divide_stmt with_loc          , 'a) fold = default
  method fold_exit'          : (exit_stmt with_loc            , 'a) fold = default
  method fold_evaluate'      : (evaluate_stmt with_loc        , 'a) fold = default
  method fold_free'          : (name with_loc list with_loc   , 'a) fold = default
  method fold_generate'      : (name with_loc with_loc        , 'a) fold = default
  method fold_goback'        : (raising option with_loc       , 'a) fold = default
  method fold_goto'          : (goto_stmt with_loc            , 'a) fold = default
  method fold_goto_depending': (goto_depending_stmt with_loc  , 'a) fold = default
  method fold_if'            : (if_stmt with_loc              , 'a) fold = default
  method fold_initialize'    : (initialize_stmt with_loc      , 'a) fold = default
  method fold_initiate'      : (name with_loc list with_loc   , 'a) fold = default
  method fold_inspect'       : (inspect_stmt with_loc         , 'a) fold = default
  method fold_invoke'        : (invoke_stmt with_loc          , 'a) fold = default
  method fold_move'          : (move_stmt with_loc            , 'a) fold = default
  method fold_multiply'      : (multiply_stmt with_loc        , 'a) fold = default
  method fold_open'          : (open_stmt with_loc            , 'a) fold = default
  method fold_perform_target': (perform_target_stmt with_loc  , 'a) fold = default
  method fold_perform_inline': (perform_inline_stmt with_loc  , 'a) fold = default
  method fold_raise'         : (raise_operand with_loc        , 'a) fold = default
  method fold_read'          : (read_stmt with_loc            , 'a) fold = default
  method fold_release'       : (release_stmt with_loc         , 'a) fold = default
  method fold_resume'        : (resume_stmt with_loc          , 'a) fold = default
  method fold_return'        : (return_stmt with_loc          , 'a) fold = default
  method fold_rewrite'       : (rewrite_stmt with_loc         , 'a) fold = default
  method fold_search'        : (search_stmt with_loc          , 'a) fold = default
  method fold_search_all'    : (search_all_stmt with_loc      , 'a) fold = default
  method fold_set'           : (set_stmt with_loc             , 'a) fold = default
  method fold_start'         : (start_stmt with_loc           , 'a) fold = default
  method fold_stop'          : (stop_stmt with_loc            , 'a) fold = default
  method fold_string_stmt'   : (string_stmt with_loc          , 'a) fold = default
  method fold_subtract'      : (basic_arithmetic_stmt with_loc, 'a) fold = default
  method fold_terminate'     : (terminate_stmt with_loc       , 'a) fold = default
  method fold_transform'     : (transform_stmt with_loc       , 'a) fold = default
  method fold_unlock'        : (unlock_stmt with_loc          , 'a) fold = default
  method fold_unstring'      : (unstring_stmt with_loc        , 'a) fold = default
  method fold_validate'      : (ident list with_loc           , 'a) fold = default
  method fold_write'         : (write_stmt with_loc           , 'a) fold = default
end

let todo    x = todo    __MODULE__ x
and partial x = partial __MODULE__ x


let fold_varying_phrase (v: _ #folder) =
  handle v#fold_varying_phrase
    ~continue:begin fun { varying_ident; varying_from;
                          varying_by; varying_until } x -> x
      >> fold_ident v varying_ident
      >> fold_ident_or_numlit v varying_from
      >> fold_option ~fold:fold_ident_or_numlit v varying_by
      >> fold_condition v varying_until
    end

let fold_varying_phrase' (v: _ #folder) =
  handle' v#fold_varying_phrase' ~fold:fold_varying_phrase v

let fold_perform_mode (v: _ #folder) =
  handle v#fold_perform_mode
    ~continue:begin fun m x -> match m with
      | PerformForever -> x
      | PerformNTimes i -> x
          >> fold_ident_or_intlit v i
      | PerformUntil { with_test; until } -> x
          >> fold_option ~fold:fold_stage v with_test
          >> fold_condition v until
      | PerformVarying { with_test; varying; after } -> x
          >> fold_option ~fold:fold_stage v with_test
          >> fold_varying_phrase' v varying
          >> fold_list ~fold:fold_varying_phrase' v after
    end

let fold_allocate_kind (v: _ #folder) =
  handle v#fold_allocate_kind
    ~continue:begin function
      | AllocateCharacters e -> fold_expression v e
      | AllocateDataItem n -> fold_name' v n
    end

let fold_alter_operands' (v: _ #folder) =
  handle' v#fold_alter_operands' v
    ~fold:begin fun v { alter_source; alter_target } x -> x
      >> fold_procedure_name' v alter_source
      >> fold_procedure_name' v alter_target
    end

let fold_call_proto (v: _ #folder) =
  handle v#fold_call_proto
    ~continue:begin function
      | CallProtoIdent i -> fold_ident v i
      | CallProtoNested -> Fun.id
    end

let fold_call_prefix (v: _ #folder) =
  handle v#fold_call_prefix
    ~continue:begin fun p x -> match p with
      | CallGeneral i -> x
          >> fold_ident_or_strlit v i
      | CallProto { called; prototype } -> x
          >> fold_option ~fold:fold_ident_or_strlit v called
          >> fold_call_proto v prototype
    end

let fold_close_format (v: _ #folder) =
  handle v#fold_close_format
    ~continue:begin function
      | CloseUnitReel b -> fold_bool v b
      | CloseWithLock
      | CloseWithNoRewind -> Fun.id
    end

let fold_close_phrase (v: _ #folder) =
  handle v#fold_close_phrase
    ~continue:begin fun { close_item; close_format} x -> x
      >> fold_name' v close_item
      >> fold_option ~fold:fold_close_format v close_format
    end

let fold_init_data_category (v: _ #folder) =
  leaf v#fold_init_data_category

let fold_init_category (v: _ #folder) = function
  | InitAll -> Fun.id
  | InitCategory c -> fold_init_data_category v c

let fold_init_replacing (v: _ #folder) =
  handle v#fold_init_replacing
    ~continue:begin fun { init_replacing_category;
                          init_replacing_replacement_item } x -> x
      >> fold_init_data_category v init_replacing_category
      >> fold_ident_or_literal v init_replacing_replacement_item
    end

let fold_inspect_where (v: _ #folder) =
  handle v#fold_inspect_where
    ~continue:begin fun (_, reference) ->
      fold_ident_or_nonnum v reference
    end

let fold_tallying_spec (v: _ #folder) =
  handle v#fold_tallying_spec
    ~continue:begin fun { tallying_item; tallying_where } x -> x
      >> fold_ident_or_nonnum v tallying_item
      >> fold_list ~fold:fold_inspect_where v tallying_where
    end

let fold_tallying_clause' (v: _ #folder) =
  handle' v#fold_tallying_clause' v
    ~fold:begin fun v clause x -> match clause with
      | TallyingCharacters wheres -> x
          >> fold_list ~fold:fold_inspect_where v wheres
      | TallyingRange (_, spec) -> x
          >> fold_list ~fold:fold_tallying_spec v spec
    end

let fold_tallying (v: _ #folder) =
  handle v#fold_tallying
    ~continue:begin fun { tallying_target; tallying_clauses } x -> x
      >> fold_qualident v tallying_target
      >> fold_list ~fold:fold_tallying_clause' v tallying_clauses
    end

let fold_replacing_range_spec (v: _ #folder) =
  handle v#fold_replacing_range_spec
    ~continue:begin fun { replacing_item;
                          replacing_by;
                          replacing_where} x -> x
      >> fold_ident_or_nonnum v replacing_item
      >> fold_ident_or_nonnum v replacing_by
      >> fold_list ~fold:fold_inspect_where v replacing_where
    end

let fold_replacing_clause (v: _ #folder) =
  handle v#fold_replacing_clause
    ~continue:begin fun clause x -> match clause with
      | ReplacingCharacters { replacement; where } -> x
          >> fold_ident_or_nonnum v replacement
          >> fold_list ~fold:fold_inspect_where v where
      | ReplacingRange (_, specs) -> x
          >> fold_list ~fold:fold_replacing_range_spec v specs
    end

let fold_replacing (v:_ #folder) =
  handle' v#fold_replacing v ~fold:fold_replacing_clause

let fold_converting (v: _ #folder) =
  handle v#fold_converting
    ~continue:begin fun { converting_from;
                          converting_to;
                          converting_where} x -> x
      >> fold_ident_or_nonnum v converting_from
      >> fold_ident_or_nonnum v converting_to
      >> fold_list ~fold:fold_inspect_where v converting_where
    end

let fold_inspect_spec (v: _ #folder) =
  handle v#fold_inspect_spec
    ~continue:begin fun spec x -> match spec with
      | InspectTallying tl -> x
          >> fold_list ~fold:fold_tallying v tl
      | InspectReplacing rl -> x
          >> fold_list ~fold:fold_replacing v rl
      | InspectBoth (tl, rl) -> x
          >> fold_list ~fold:fold_tallying v tl
          >> fold_list ~fold:fold_replacing v rl
      | InspectConverting converting -> x
          >> fold_converting v converting
    end

let fold_merge_or_sort_target (v : _ #folder) =
  handle v#fold_merge_or_sort_target
    ~continue:begin function
      | OutputProcedure name_procedure_range ->
          fold_procedure_range v name_procedure_range
            ~fold:fold_procedure_name'
      | Giving names ->
          fold_list ~fold:fold_name' v names
    end

(*for open*)
let fold_named_file_option (v: _ #folder) =
  handle v#fold_named_file_option
    ~continue:begin fun { named_file_name;
                          named_file_option} x -> x
      >> fold_name' v named_file_name
      >> fold_option ~fold:fold_file_option v named_file_option
    end

let fold_open_phrase (v: _ #folder) =
  handle v#fold_open_phrase
    ~continue:begin fun { open_mode; open_sharing;
                          open_retry; open_files } x -> x
      >> fold_open_mode v open_mode
      >> fold_option ~fold:fold_sharing_mode v open_sharing
      >> fold_option ~fold:fold_retry_clause v open_retry
      >> fold_list ~fold:fold_named_file_option v open_files
    end

let fold_read_error (v: _ #folder) =
  leaf v#fold_read_error

let fold_set_switch_spec (v: _ #folder) =
  handle v#fold_set_switch_spec
    ~continue:begin fun { set_switch_targets;
                          set_switch_value } x -> x
      >> fold_list ~fold:fold_ident v set_switch_targets
      >> fold_on_off v set_switch_value
    end

let fold_set_condition_spec (v: _ #folder) =
  handle v#fold_set_condition_spec
    ~continue:begin fun { set_condition_targets;
                          set_condition_value } x -> x
      >> fold_list ~fold:fold_ident v set_condition_targets
      >> fold_bool v set_condition_value
    end

let fold_stop_kind (v: _ #folder) =
  leaf v#fold_stop_kind

let fold_stop_run (v: _ #folder) =
  handle v#fold_stop_run
    ~continue:begin fun { stop_kind; stop_status } x -> x
      >> fold_stop_kind v stop_kind
      >> fold_ident_or_literal v stop_status
    end

let fold_string_delimiter (v: _ #folder) =
  handle v#fold_string_delimiter
    ~continue:begin function
      | StringDelimiter ident_or_nonnum ->
          fold_ident_or_nonnum v ident_or_nonnum
      | StringDelimiterSize -> Fun.id
    end

let fold_string_source (v: _ #folder) =
  handle v#fold_string_source
    ~continue:begin fun {string_source; string_delimiter} x -> x
      >> fold_ident_or_nonnum v string_source
      >> fold_option ~fold:fold_string_delimiter v string_delimiter
    end

let fold_unstring_delimiter (v: _ #folder) =
  handle v#fold_unstring_delimiter
    ~continue:begin fun { unstring_delimiter;
                          unstring_delimiter_by_all } x -> x
      >> fold_ident_or_strlit v unstring_delimiter
      >> fold_bool v unstring_delimiter_by_all
    end

let fold_unstring_target (v: _ #folder) =
  handle v#fold_unstring_target
    ~continue:begin fun { unstring_target;
                          unstring_target_delimiter;
                          unstring_target_count} x -> x
      >> fold_ident v unstring_target
      >> fold_option ~fold:fold_ident v unstring_target_delimiter
      >> fold_option ~fold:fold_ident v unstring_target_count
    end

let fold_write_error (v: _ #folder) =
  leaf v#fold_write_error


(* Statements that do not need recursion (not high-level control structure,
   and no inline handler) *)

let fold_allocate' (v: _ #folder) =
  handle' v#fold_allocate' v
    ~fold:begin fun v { allocate_kind;
                        allocate_initialized;
                        allocate_returning } x -> x
      >> fold_allocate_kind v allocate_kind
      >> fold_bool v allocate_initialized
      >> fold_ident'_opt v allocate_returning
    end

let fold_alter' (v: _ #folder) =
  handle' v#fold_alter' v ~fold:(fold_list ~fold:fold_alter_operands')

let fold_cancel' (v: _ #folder) =
  handle' v#fold_cancel' v ~fold:(fold_list ~fold:fold_ident_or_strlit)

let fold_close' (v: _ #folder) =
  handle' v#fold_close' v ~fold:(fold_list ~fold:fold_close_phrase)

let fold_exit' (v: _ #folder) =
  handle' v#fold_exit' v
    ~fold:begin fun v -> function
      | ExitSimple
      | ExitParagraph
      | ExitSection -> Fun.id
      | ExitPerform b -> fold_bool v b
      | ExitProgram r
      | ExitMethod r
      | ExitFunction r -> fold_option ~fold:fold_raising v r
    end

let fold_free' (v: _ #folder) =
  handle' v#fold_free' v ~fold:(fold_list ~fold:fold_name')

let fold_generate' (v: _ #folder) =
  handle' v#fold_generate' v ~fold:fold_name'

let fold_goback' (v: _ #folder) =
  handle' v#fold_goback' v ~fold:(fold_option ~fold:fold_raising)

let fold_goto' (v: _ #folder) =
  handle' v#fold_goto' v
    ~fold:begin fun v { goto_target } x -> x
      >> fold_procedure_name' v goto_target
    end

let fold_goto_depending' (v: _ #folder) =
  handle' v#fold_goto_depending' v
    ~fold:begin fun v { goto_depending_targets; goto_depending_on } x -> x
      >> fold_nel ~fold:fold_procedure_name' v goto_depending_targets
      >> fold_ident v goto_depending_on
    end

let fold_initialize' (v: _ #folder) =
  handle' v#fold_initialize' v
    ~fold:begin fun v { init_items; init_filler; init_category;
                        init_replacings; init_to_default } x -> x
      >> fold_list ~fold:fold_ident v init_items
      >> fold_bool v init_filler
      >> fold_option ~fold:fold_init_category v init_category
      >> fold_list ~fold:fold_init_replacing v init_replacings
      >> fold_bool v init_to_default
    end

let fold_initiate' (v: _ #folder) =
  handle' v#fold_initiate' v ~fold:(fold_list ~fold:fold_name')

let fold_inspect' (v: _ #folder) =
  handle' v#fold_inspect' v
    ~fold:begin fun v { inspect_item; inspect_spec } x -> x
      >> fold_ident v inspect_item
      >> fold_inspect_spec v inspect_spec
    end

let fold_invoke' (v: _ #folder) =
  handle' v#fold_invoke' v
    ~fold:begin fun v { invoke_target; invoke_method;
                        invoke_using; invoke_returning} x -> x
      >> fold_ident v invoke_target
      >> fold_ident_or_strlit v invoke_method
      >> fold_list ~fold:fold_call_using_clause' v invoke_using
      >> fold_ident'_opt v invoke_returning
    end

let fold_move' (v: _ #folder) =
  handle' v#fold_move' v
    ~fold:begin fun v m x -> match m with
      | MoveSimple { from; to_ } -> x
          >> fold_ident_or_literal v from
          >> fold_list ~fold:fold_ident v to_
      | MoveCorresponding { from; to_ } -> x
          >> fold_ident v from
          >> fold_list ~fold:fold_ident v to_
    end

let fold_open' (v: _ #folder) =
  handle' v#fold_open' v ~fold:(fold_list ~fold:fold_open_phrase)

let fold_raise' (v: _ #folder) =
  handle' v#fold_raise' v
    ~fold:begin fun v -> function
      | RaiseIdent id -> fold_ident v id
      | RaiseException name -> fold_name' v name
    end

let fold_release' (v: _ #folder) =
  handle' v#fold_release' v
    ~fold:begin fun v {release_item; release_from} x -> x
      >> fold_name' v release_item
      >> fold_option ~fold:fold_ident_or_literal v release_from
    end

let fold_resume' (v: _ #folder) =
  handle' v#fold_resume' v
    ~fold:begin fun v stmt x -> match stmt with
      | ResumeNextStatement -> x
      | ResumeTarget t -> fold_procedure_name' v t x
    end

let fold_set' (v: _ #folder) =
  handle' v#fold_set' v
    ~fold:begin fun v stmt x -> match stmt with
      | SetAmbiguous { targets; set_method; value} -> x
          >> fold_list ~fold:fold_ident v targets
          >> fold_set_ambiguous_method v set_method
          >> fold_expression v value
      | SetSwitch specs -> x
          >> fold_list ~fold:fold_set_switch_spec v specs
      | SetCondition specs -> x
          >> fold_list ~fold:fold_set_condition_spec v specs
      | SetAttribute { name; attribute_switches } -> x
          >> fold_name' v name
          >> fold_list ~fold:fold_set_attribute_switch v attribute_switches
      | SetSaveLocale { target; locale } -> x
          >> fold_ident v target
          >> fold_set_save_locale v locale
      | SetLocale { target; source } -> x
          >> fold_set_locale_target v target
          >> fold_set_locale_source v source
      | SetSavedException -> x
      | SetFloatContent { targets; content; sign } -> x
          >> fold_list ~fold:fold_ident v targets
          >> fold_float_content v content
          >> fold_option ~fold:fold_sign v sign
    end

let fold_terminate' (v: _ #folder) =
  handle' v#fold_terminate' v
    ~fold:(fold_list ~fold:fold_name')

let fold_transform' (v: _ #folder) =
  handle' v#fold_transform' v
    ~fold:begin fun v { transform_ident; transform_from; transform_to } x -> x
      >> fold_ident' v transform_ident
      >> fold' ~fold:fold_ident_or_nonnum v transform_from
      >> fold' ~fold:fold_ident_or_nonnum v transform_to
    end

let fold_unlock' (v: _ #folder) =
  handle' v#fold_unlock' v
    ~fold:begin fun v { unlock_file; unlock_record } x -> x
      >> fold_name' v unlock_file
      >> fold_bool v unlock_record
    end

let fold_validate' (v: _ #folder) =
  handle' v#fold_validate' v ~fold:(fold_list ~fold:fold_ident)

let fold_stop' (v: _ #folder) =
  handle' v#fold_stop' v
    ~fold:begin fun v -> function
      | StopRun o -> fold_option ~fold:fold_stop_run v o
      | StopLiteral l -> fold_literal v l
    end

let fold_selection_subject (v: _ #folder) =
  handle v#fold_selection_subject
    ~continue:begin function
      | Subject c -> fold_condition v c
      | SubjectConst b -> fold_bool v b
    end

let fold_selection_object (v: _ #folder) =
  handle v#fold_selection_object
    ~continue:begin fun selection_object x ->
      match selection_object with
      | SelCond c -> x
          >> fold_condition v c
      | SelRange { negated; start; stop; alphabet} -> x
          >> fold_bool v negated
          >> fold_expression v start
          >> fold_expression v stop
          >> fold_option ~fold:fold_name' v alphabet
      | SelRelation { relation; expr } -> x
          >> fold_relop v relation
          >> fold_expression v expr
      | SelClassCond { negated; class_specifier } -> x
          >> fold_bool v negated
          >> fold_class v class_specifier
      | SelSignCond { negated; sign_specifier } -> x
          >> fold_bool v negated
          >> fold_signz v sign_specifier
      | SelOmitted { negated } -> x
          >> fold_bool v negated
      | SelConst b -> x
          >> fold_bool v b
      | SelAny -> x
    end

(* Statements with high-level control structure and/or (inline) handlers *)
let rec fold_statement' (v: _ #folder) =
  handle v#fold_statement'
    ~continue:begin fun { payload; loc } -> match payload with
      | Accept        s -> fold_accept'         v (s &@ loc)
      | Allocate      s -> fold_allocate'       v (s &@ loc)
      | Add           s -> fold_add'            v (s &@ loc)
      | Alter         s -> fold_alter'          v (s &@ loc)
      | Call          s -> fold_call'           v (s &@ loc)
      | Cancel        s -> fold_cancel'         v (s &@ loc)
      | Close         s -> fold_close'          v (s &@ loc)
      | Compute       s -> fold_compute'        v (s &@ loc)
      | Delete        s -> fold_delete'         v (s &@ loc)
      | Display       s -> fold_display'        v (s &@ loc)
      | Divide        s -> fold_divide'         v (s &@ loc)
      | Evaluate      s -> fold_evaluate'       v (s &@ loc)
      | Exit          s -> fold_exit'           v (s &@ loc)
      | Free          s -> fold_free'           v (s &@ loc)
      | Generate      s -> fold_generate'       v (s &@ loc)
      | GoBack        s -> fold_goback'         v (s &@ loc)
      | GoTo          s -> fold_goto'           v (s &@ loc)
      | GoToDepending s -> fold_goto_depending' v (s &@ loc)
      | If            s -> fold_if'             v (s &@ loc)
      | Initialize    s -> fold_initialize'     v (s &@ loc)
      | Initiate      s -> fold_initiate'       v (s &@ loc)
      | Inspect       s -> fold_inspect'        v (s &@ loc)
      | Invoke        s -> fold_invoke'         v (s &@ loc)
      | Move          s -> fold_move'           v (s &@ loc)
      | Multiply      s -> fold_multiply'       v (s &@ loc)
      | Open          s -> fold_open'           v (s &@ loc)
      | PerformInline s -> fold_perform_inline' v (s &@ loc)
      | PerformTarget s -> fold_perform_target' v (s &@ loc)
      | Raise         s -> fold_raise'          v (s &@ loc)
      | Read          s -> fold_read'           v (s &@ loc)
      | Release       s -> fold_release'        v (s &@ loc)
      | Resume        s -> fold_resume'         v (s &@ loc)
      | Return        s -> fold_return'         v (s &@ loc)
      | Rewrite       s -> fold_rewrite'        v (s &@ loc)
      | Search        s -> fold_search'         v (s &@ loc)
      | SearchAll     s -> fold_search_all'     v (s &@ loc)
      | Set           s -> fold_set'            v (s &@ loc)
      | Start         s -> fold_start'          v (s &@ loc)
      | Stop          s -> fold_stop'           v (s &@ loc)
      | String        s -> fold_string_stmt'    v (s &@ loc)
      | Subtract      s -> fold_subtract'       v (s &@ loc)
      | Terminate     s -> fold_terminate'      v (s &@ loc)
      | Transform     s -> fold_transform'      v (s &@ loc)
      | Unlock        s -> fold_unlock'         v (s &@ loc)
      | Unstring      s -> fold_unstring'       v (s &@ loc)
      | Validate      s -> fold_validate'       v (s &@ loc)
      | Write         s -> fold_write'          v (s &@ loc)
      | Continue
      | LoneGoTo
      | Suppress -> Fun.id
      | _ -> partial __LINE__ "fold_statement"
      (* | Disable of mcs_command_operands *)
      (* | Enable of mcs_command_operands *)
      (* | Enter of enter_stmt *)
      (* | Merge of merge_stmt *)
      (* | Purge of name with_loc *)
      (* | Receive of receive_stmt *)
      (* | Send of send_stmt *)
      (* | Sort of sort_stmt *)
    end

and fold_statements (v: _ #folder) =
  (* handle v#fold_statements ~continue: *)(fold_list ~fold:fold_statement' v)

and fold_statements' (v: _ #folder) =
  handle' v#fold_statements' ~fold:fold_statements v

and fold_handler v = fold_statements v
and fold_dual_handler (v: _ #folder) { dual_handler_pos;
                                       dual_handler_neg } x = x
  >> fold_handler v dual_handler_pos
  >> fold_handler v dual_handler_neg

and fold_branch (v: _ #folder) : branch -> 'a -> 'a = function
  | Statements stmts -> fold_statements v stmts
  | NextSentence -> Fun.id

and fold_basic_arith_stmt (v: _ #folder) : basic_arithmetic_stmt -> 'a -> 'a =
  fun { basic_arith_operands; basic_arith_on_size_error } x -> x
    >> fold_basic_arithmetic_operands v basic_arith_operands
    >> fold_dual_handler v basic_arith_on_size_error

and fold_accept' (v: _ #folder) : accept_stmt with_loc -> 'a -> 'a =
  handle' v#fold_accept' v
    ~fold:begin fun v stmt x -> match stmt with
      | AcceptGeneric id -> x
          >> fold_ident' v id
      | AcceptFromDevice { item; device_item } -> x
          >> fold_ident' v item
          >> fold_name' v device_item
      | AcceptTemporal { item; date_time } -> x
          >> fold_ident' v item
          >> fold_date_time v date_time
      | AcceptMsgCount name' -> x
          >> fold_name' v name'
      | AcceptAtScreen { item; position; on_exception } -> x
          >> fold_name' v item
          >> fold_option ~fold:fold_position v position
          >> fold_dual_handler v on_exception
      | AcceptFromEnv { item; env_item; on_exception } -> x
          >> fold_ident' v item
          >> fold' ~fold:fold_ident_or_nonnum v env_item
          >> fold_dual_handler v on_exception
    end

and fold_add' (v: _ #folder) : basic_arithmetic_stmt with_loc -> 'a -> 'a =
  handle' v#fold_add' v ~fold:fold_basic_arith_stmt

and fold_call' (v: _ #folder) : call_stmt with_loc -> 'a -> 'a =
  handle' v#fold_call' v
    ~fold:begin fun v { call_prefix; call_using; call_returning;
                        call_error_handler } x -> x
      >> fold_call_prefix v call_prefix
      >> fold_list ~fold:fold_call_using_clause' v call_using
      >> fold_ident'_opt v call_returning
      >> fold_option ~fold:fold_call_error_handler v call_error_handler
    end

and fold_call_error_handler (v: _ #folder) =
  handle v#fold_call_error_handler
    ~continue:begin function
      | CallOnOverflow h -> fold_handler v h
      | CallOnException h -> fold_dual_handler v h
    end

and fold_compute' (v: _ #folder) : compute_stmt with_loc -> 'a -> 'a =
  handle' v#fold_compute' v
    ~fold:begin fun v { compute_targets; compute_expr;
                        compute_on_size_error } x -> x
      >> fold_rounded_idents v compute_targets
      >> fold_expr v compute_expr
      >> fold_dual_handler v compute_on_size_error
    end

and fold_delete' (v: _ #folder) : delete_stmt with_loc -> 'a -> 'a =
  handle' v#fold_delete' v
    ~fold:begin fun v { delete_targets; delete_retry;
                        delete_on_invalid_key } x -> x
      >> fold_name' v delete_targets
      >> fold_option ~fold:fold_retry_clause v delete_retry
      >> fold_dual_handler v delete_on_invalid_key
    end

and fold_display' (v: _ #folder) : display_stmt with_loc -> 'a -> 'a =
  handle' v#fold_display' v
    ~fold:begin fun v d x -> match d with
      | DisplayDefault i -> x
          >> fold_ident_or_literal v i
      | DisplayDevice { displayed_items; upon; advancing } -> x
          >> fold_list ~fold:fold_ident_or_literal v displayed_items
          >> fold_option ~fold:fold_display_target' v upon
          >> fold_bool v advancing
      | DisplayScreen { screen_item; position; on_exception } -> x
          >> fold_name' v screen_item
          >> fold_option ~fold:fold_position v position
          >> fold_dual_handler v on_exception
    end

and fold_display_target' (v: _ #folder) : display_target with_loc -> 'a -> 'a =
  (* handle' v#fold_display_target' *)fold' v
    ~fold:begin fun v -> function
      | DisplayUponName n -> fold_name' v n
      | DisplayUponDeviceViaMnemonic _ -> Fun.id
    end

and fold_divide' (v: _ #folder) : divide_stmt with_loc -> 'a -> 'a =
  handle' v#fold_divide' v
    ~fold:begin fun v { divide_operands; divide_on_size_error } x -> x
      >> fold_divide_operands v divide_operands
      >> fold_dual_handler v divide_on_size_error
    end

and fold_evaluate' (v: _ #folder) : evaluate_stmt with_loc -> 'a -> 'a =
  handle' v#fold_evaluate' v
    ~fold:begin fun v { eval_subjects; eval_branches; eval_otherwise } x -> x
      >> fold_list ~fold:fold_selection_subject v eval_subjects
      >> fold_list ~fold:fold_evaluate_branch v eval_branches
      >> fold_statements v eval_otherwise
    end

and fold_evaluate_branch (v: _#folder) =
  handle v#fold_evaluate_branch
    ~continue:begin fun { eval_selection; eval_actions } x -> x
      >> fold_list v eval_selection
        ~fold:(fold_list ~fold:fold_selection_object)
      >> fold_statements v eval_actions
    end

and fold_if' (v: _ #folder) : if_stmt with_loc -> 'a -> 'a =
  handle' v#fold_if' v
    ~fold:begin fun v { condition; then_branch; else_branch } x -> x
      >> fold_condition v condition
      >> fold_branch v then_branch
      >> fold_option ~fold:fold_branch v else_branch
    end

and fold_multiply' (v: _ #folder) : multiply_stmt with_loc -> 'a -> 'a =
  handle' v#fold_multiply' v
    ~fold:begin fun v { multiply_operands; multiply_on_size_error } x -> x
      >> fold_multiply_operands v multiply_operands
      >> fold_dual_handler v multiply_on_size_error
    end

and fold_perform_inline' (v: _ #folder) : perform_inline_stmt with_loc -> 'a -> 'a =
  handle' v#fold_perform_inline' v
    ~fold:begin fun v { perform_inline_mode; perform_statements } x -> x
      >> fold_option ~fold:fold_perform_mode v perform_inline_mode
      >> fold_statements v perform_statements
    end

and fold_perform_target' (v: _ #folder) : perform_target_stmt with_loc -> 'a -> 'a =
  handle' v#fold_perform_target' v
    ~fold:begin fun v { perform_target = proc_range; perform_mode } x -> x
      >> fold_procedure_range ~fold:fold_procedure_name' v proc_range
      >> fold_option ~fold:fold_perform_mode v perform_mode
    end

and fold_read_error_handler (v: _ #folder) =
  handle v#fold_read_error_handler
    ~continue:begin fun (read_error, dual_handler) x -> x
      >> fold_read_error v read_error
      >> fold_dual_handler v dual_handler
    end

and fold_read' (v: _#folder) =
  handle' v#fold_read' v
    ~fold:begin fun v { read_file; read_direction;
                        read_into; read_lock_behavior;
                        read_lock; read_key;
                        read_error_handler } x -> x
      >> fold_name' v read_file
      >> fold_option ~fold:fold_read_direction v read_direction
      >> fold_option ~fold:fold_ident v read_into
      >> fold_option ~fold:fold_read_lock_behavior v read_lock_behavior
      >> fold_option ~fold:fold_bool v read_lock
      >> fold_option ~fold:fold_qualname v read_key
      >> fold_option ~fold:fold_read_error_handler v read_error_handler
    end

and fold_return' (v: _ #folder) =
  handle' v#fold_return' v
    ~fold:begin fun v {return_file; return_into; return_at_end} x -> x
      >> fold_name' v return_file
      >> fold_ident'_opt v return_into
      >> fold_dual_handler v return_at_end
    end

and fold_rewrite' (v: _ #folder) =
  handle' v#fold_rewrite' v
    ~fold:begin fun v { rewrite_to; rewrite_from;
                        rewrite_retry; rewrite_lock;
                        rewrite_invalid_key_handler } x -> x
      >> fold_write_target v rewrite_to
      >> fold_option ~fold:fold_ident_or_literal v rewrite_from
      >> fold_option ~fold:fold_retry_clause v rewrite_retry
      >> fold_option ~fold:fold_bool v rewrite_lock
      >> fold_dual_handler v rewrite_invalid_key_handler
    end

and fold_search_when_clause' (v: _#folder) =
  handle' v#fold_search_when_clause' v
    ~fold:begin fun v {search_when_cond; search_when_stmts} x -> x
      >> fold_condition v search_when_cond
      >> fold_branch v search_when_stmts
    end

and fold_search' (v: _ #folder) =
  handle' v#fold_search' v
    ~fold:begin fun v { search_item; search_at_end;
                        search_varying; search_when_clauses } x -> x
      >> fold_qualname v search_item
      >> fold_handler v search_at_end
      >> fold_option ~fold:fold_ident v search_varying
      >> fold_list ~fold:fold_search_when_clause' v search_when_clauses
    end

and fold_search_all' (v: _ #folder) =
  handle' v#fold_search_all' v
    ~fold:begin fun v { search_all_item; search_all_at_end;
                        search_all_conditions; search_all_action } x -> x
      >> fold_qualname v search_all_item
      >> fold_handler v search_all_at_end
      >> fold_list ~fold:fold_search_condition v search_all_conditions
      >> fold_branch v search_all_action
    end

and fold_start' (v: _ #folder) =
  handle' v#fold_start' v
    ~fold:begin fun v { start_file; start_position;
                        start_on_invalid_key } x -> x
      >> fold_name' v start_file
      >> fold_option ~fold:fold_start_position v start_position
      >> fold_dual_handler v start_on_invalid_key
    end

and fold_string_stmt' (v: _ #folder) =
  handle' v#fold_string_stmt' v
    ~fold:begin fun v { string_sources;
                        string_target;
                        string_pointer;
                        string_on_overflow} x -> x
      >> fold_list ~fold:fold_string_source v string_sources
      >> fold_ident v string_target
      >> fold_option ~fold:fold_ident v string_pointer
      >> fold_dual_handler v string_on_overflow
    end

and fold_subtract' (v: _ #folder) : basic_arithmetic_stmt with_loc -> 'a -> 'a =
  handle' v#fold_subtract' v ~fold:fold_basic_arith_stmt

and fold_unstring' (v: _ #folder) =
  handle' v#fold_unstring' v
    ~fold:begin fun v { unstring_source;
                        unstring_delimiters;
                        unstring_targets;
                        unstring_pointer;
                        unstring_tallying;
                        unstring_on_overflow} x -> x
      >> fold_ident v unstring_source
      >> fold_list ~fold:fold_unstring_delimiter v unstring_delimiters
      >> fold_list ~fold:fold_unstring_target v unstring_targets
      >> fold_option ~fold:fold_ident v unstring_pointer
      >> fold_option ~fold:fold_ident v unstring_tallying
      >> fold_dual_handler v unstring_on_overflow
    end

and fold_write_error_handler (v: _ #folder) =
  handle v#fold_write_error_handler
    ~continue:begin fun (write_error, dual_handler) x -> x
      >> fold_write_error v write_error
      >> fold_dual_handler v dual_handler
    end

and fold_write' (v: _ #folder) =
  handle' v#fold_write' v
    ~fold:begin fun v { write_to; write_from;
                        write_advancing; write_retry;
                        write_lock; write_error_handler } x -> x
      >> fold_write_target v write_to
      >> fold_option ~fold:fold_ident_or_literal v write_from
      >> fold_option ~fold:fold_advancing_phrase v write_advancing
      >> fold_option ~fold:fold_retry_clause v write_retry
      >> fold_option ~fold:fold_bool v write_lock
      >> fold_option ~fold:fold_write_error_handler v write_error_handler
    end
