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

(** Folding visitor for the typed procedure tree.

    Methods for the terms that are shared with the parse tree (names, literals,
    procedure names...) are inherited from
    {!Cobol_ptree.Terms_visitor.shared_folder}. Other methods have the same name
    as in the parse tree visitors.

    Ill-typed statements are leaves: their parse tree is never visited. *)

open Procedure_types
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX (* for `>>` (== `|>`) *)
module P = Cobol_ptree.Terms_visitor

class ['a] folder =
  object
    inherit ['a] Cobol_ptree.Terms_visitor.shared_folder

    (* Terms *)
    method fold_resolved_name : 'x. ('x resolved_name, 'a) fold = default
    method fold_data_ref : (data_ref, 'a) fold = default
    method fold_data_name_ref : (data_name_ref, 'a) fold = default
    method fold_literal_value : (Cobol_data.Types.literal_value, 'a) fold = default
    method fold_integer_value : (Cobol_data.Types.integer_value, 'a) fold = default
    method fold_alphanum_value : (Cobol_data.Types.alphanum_value, 'a) fold = default
    method fold_ident : (ident, 'a) fold = default
    method fold_base_ident : (base_ident, 'a) fold = default
    method fold_qualident : (qualident, 'a) fold = default
    method fold_subscript : (subscript, 'a) fold = default

    method fold_subscript' : (subscript with_loc, 'a) fold = default
    method fold_refmod : (refmod, 'a) fold = default
    method fold_address : (address, 'a) fold = default
    method fold_length_of : (ident_or_literal, 'a) fold = default
    method fold_inline_call : (inline_call, 'a) fold = default
    method fold_inline_invocation : (inline_invocation, 'a) fold = default
    method fold_effective_arg : (effective_arg, 'a) fold = default
    method fold_object_view : (object_view, 'a) fold = default
    method fold_expr : (expr, 'a) fold = default
    method fold_expr' : (expr with_loc, 'a) fold = default
    method fold_condition : (condition, 'a) fold = default
    method fold_condition' : (condition with_loc, 'a) fold = default
    method fold_condition_or_error' : (condition or_error with_loc, 'a) fold = default

    (* Operands *)
    method fold_alphabet_specification : (Cobol_ptree.alphabet_specification, 'a) fold = default
    method fold_rounded_ident : (rounded_ident, 'a) fold = default
    method fold_basic_arithmetic_operands : (basic_arithmetic_operands, 'a) fold = default
    method fold_using_arg_default' : (using_arg_default with_loc, 'a) fold = default
    method fold_using_reference_arg' : (using_reference_arg with_loc, 'a) fold = default
    method fold_call_using_clause : (call_using_clause, 'a) fold = default
    method fold_date_time : (Cobol_ptree.date_time, 'a) fold = default
    method fold_divide_operands : (divide_operands, 'a) fold = default
    method fold_file_option : (Cobol_ptree.file_option, 'a) fold = default
    method fold_multiply_operands : (multiply_operands, 'a) fold = default
    method fold_mcs_awaiting_item : (Cobol_ptree.mcs_awaiting_item, 'a) fold = default
    method fold_mcs_kind : (Cobol_ptree.mcs_kind, 'a) fold = default
    method fold_mcs_command_operands : (mcs_command_operands, 'a) fold = default
    method fold_open_mode : (Cobol_ptree.open_mode, 'a) fold = default
    method fold_position : (position, 'a) fold = default
    method fold_raising : (raising, 'a) fold = default
    method fold_read_direction : (Cobol_ptree.read_direction, 'a) fold = default
    method fold_read_lock_behavior : (read_lock_behavior, 'a) fold = default
    method fold_retry_clause : (retry_clause, 'a) fold = default
    method fold_search_condition : (search_condition, 'a) fold = default
    method fold_sharing_mode : (Cobol_ptree.sharing_mode, 'a) fold = default
    method fold_stage : (Cobol_ptree.stage, 'a) fold = default
    method fold_advancing_phrase : (advancing_phrase, 'a) fold = default
    method fold_write_target : (write_target, 'a) fold = default
    method fold_procedure_range : (Cobol_ptree.procedure_range, 'a) fold = default
    method fold_selection_subject : (selection_subject, 'a) fold = default
    method fold_selection_subject' : (selection_subject with_loc, 'a) fold = default
    method fold_selection_object : (selection_object, 'a) fold = default
    method fold_selection_object' : (selection_object with_loc, 'a) fold = default

    (* SET *)
    method fold_set_attribute_switch : (Cobol_ptree.set_attribute_switch, 'a) fold = default
    method fold_screen_attribute : (Cobol_ptree.screen_attribute, 'a) fold = default
    method fold_set_ambiguous_method : (Cobol_ptree.set_ambiguous_method, 'a) fold = default
    method fold_on_off : (Cobol_ptree.on_off, 'a) fold = default
    method fold_locale_category : (Cobol_ptree.locale_category, 'a) fold = default
    method fold_set_save_locale : (Cobol_ptree.set_save_locale, 'a) fold = default
    method fold_set_locale_target : (Cobol_ptree.set_locale_target, 'a) fold = default
    method fold_set_locale_source : (set_locale_source, 'a) fold = default
    method fold_start_position : (start_position, 'a) fold = default
    method fold_float_content : (Cobol_ptree.float_content, 'a) fold = default

    (* Clauses shared with data descriptions *)
    method fold_screen_attribute_clause : (screen_attribute_clause, 'a) fold = default
    method fold_erase_clause : (Cobol_ptree.erase_clause, 'a) fold = default
    method fold_blank_clause : (Cobol_ptree.blank_clause, 'a) fold = default
    method fold_sort_direction : (Cobol_ptree.sort_direction, 'a) fold = default
    method fold_sort_spec : (sort_spec, 'a) fold = default

    (* Statements *)
    method fold_statement' : (statement with_loc, 'a) fold = default
    method fold_statements' : (statements with_loc, 'a) fold = default
    method fold_accept_misc : (Cobol_ptree.accept_misc, 'a) fold = default
    method fold_accept_clause' : (accept_clause with_loc, 'a) fold = default
    method fold_accept_with_clause' : (accept_with_clause with_loc, 'a) fold = default
    method fold_allocate_kind : (allocate_kind, 'a) fold = default
    method fold_alter_operands' : (Cobol_ptree.alter_operands with_loc, 'a) fold = default
    method fold_call_target : (call_target, 'a) fold = default
    method fold_call_proto : (call_proto, 'a) fold = default
    method fold_close_format : (Cobol_ptree.close_format, 'a) fold = default
    method fold_close_phrase : (Cobol_ptree.close_phrase, 'a) fold = default
    method fold_converting : (converting, 'a) fold = default
    method fold_display_items_clauses : (display_items_clauses, 'a) fold = default
    method fold_display_clause' : (display_clause with_loc, 'a) fold = default
    method fold_display_target' : (Cobol_ptree.display_target with_loc, 'a) fold = default
    method fold_display_with_clause' : (display_with_clause with_loc, 'a) fold = default
    method fold_entry_by_clause : (entry_by_clause, 'a) fold = default
    method fold_evaluate_branch : (evaluate_branch, 'a) fold = default
    method fold_init_data_category : (Cobol_ptree.init_data_category, 'a) fold = default
    method fold_init_replacing : (init_replacing, 'a) fold = default
    method fold_inspect_spec : (inspect_spec, 'a) fold = default
    method fold_inspect_where : (inspect_where, 'a) fold = default
    method fold_merge_or_sort_target : (Cobol_ptree.merge_or_sort_target, 'a) fold = default
    method fold_named_file_option : (Cobol_ptree.named_file_option, 'a) fold = default
    method fold_open_phrase : (open_phrase, 'a) fold = default
    method fold_perform_mode : (perform_mode, 'a) fold = default
    method fold_read_error : (Cobol_ptree.read_error, 'a) fold = default
    method fold_replacing : (replacing, 'a) fold = default
    method fold_replacing_clause : (replacing_clause, 'a) fold = default
    method fold_replacing_range_spec : (replacing_range_spec, 'a) fold = default
    method fold_returning : (returning, 'a) fold = default
    method fold_send_operands : (send_operands, 'a) fold = default
    method fold_set_condition_spec : (set_condition_spec, 'a) fold = default
    method fold_set_switch_spec : (set_switch_spec, 'a) fold = default
    method fold_status_kind : (Cobol_ptree.status_kind, 'a) fold = default
    method fold_stop_run_return : (stop_run_return, 'a) fold = default
    method fold_stop_arg : (stop_arg, 'a) fold = default
    method fold_string_delimiter : (string_delimiter, 'a) fold = default
    method fold_string_source : (string_source, 'a) fold = default
    method fold_tallying : (tallying, 'a) fold = default
    method fold_tallying_clause' : (tallying_clause with_loc, 'a) fold = default
    method fold_tallying_spec : (tallying_spec, 'a) fold = default
    method fold_unstring_delimiter : (unstring_delimiter, 'a) fold = default
    method fold_unstring_target : (unstring_target, 'a) fold = default
    method fold_varying_phrase : (varying_phrase, 'a) fold = default
    method fold_varying_phrase' : (varying_phrase with_loc, 'a) fold = default
    method fold_write_error : (Cobol_ptree.write_error, 'a) fold = default
    method fold_sort_source : (Cobol_ptree.sort_source, 'a) fold = default
    method fold_message_ending_indicator : (message_ending_indicator, 'a) fold = default

    (* high-level structures and branches *)
    method fold_dual_handler : (dual_handler, 'a) fold = default
    method fold_call_error_handler : (call_error_handler, 'a) fold = default
    method fold_search_when_clause' : (search_when_clause with_loc, 'a) fold = default
    method fold_read_error_handler : (Cobol_ptree.read_error * dual_handler, 'a) fold = default
    method fold_write_error_handler : (Cobol_ptree.write_error * dual_handler, 'a) fold = default

    (* Individual statements with locations *)
    method fold_accept' : (accept_stmt with_loc, 'a) fold = default
    method fold_add' : (basic_arithmetic_stmt with_loc, 'a) fold = default
    method fold_allocate' : (allocate_stmt with_loc, 'a) fold = default
    method fold_alter' : (Cobol_ptree.alter_stmt with_loc, 'a) fold = default
    method fold_call' : (call_stmt with_loc, 'a) fold = default
    method fold_cancel' : (ident_or_strlit list with_loc, 'a) fold = default
    method fold_close' : (Cobol_ptree.close_stmt with_loc, 'a) fold = default
    method fold_compute' : (compute_stmt with_loc, 'a) fold = default
    method fold_delete' : (delete_stmt with_loc, 'a) fold = default
    method fold_disable' : (mcs_command_operands with_loc, 'a) fold = default
    method fold_display' : (display_stmt with_loc, 'a) fold = default
    method fold_divide' : (divide_stmt with_loc, 'a) fold = default
    method fold_enable' : (mcs_command_operands with_loc, 'a) fold = default
    method fold_enter' : (Cobol_ptree.enter_stmt with_loc, 'a) fold = default
    method fold_entry' : (entry_stmt with_loc, 'a) fold = default
    method fold_evaluate' : (evaluate_stmt with_loc, 'a) fold = default
    method fold_exec_block' : (Cobol_common.Exec_block.t with_loc, 'a) fold = default
    method fold_exit' : (exit_stmt with_loc, 'a) fold = default
    method fold_free' : (data_ref list with_loc, 'a) fold = default
    method fold_generate' : (Cobol_ptree.qualname with_loc with_loc, 'a) fold = default
    method fold_goback' : (goback_stmt with_loc, 'a) fold = default
    method fold_goto' : (goto_stmt with_loc, 'a) fold = default
    method fold_if' : (if_stmt with_loc, 'a) fold = default
    method fold_initialize' : (initialize_stmt with_loc, 'a) fold = default
    method fold_initiate' : (Cobol_ptree.name with_loc list with_loc, 'a) fold = default
    method fold_inspect' : (inspect_stmt with_loc, 'a) fold = default
    method fold_invoke' : (invoke_stmt with_loc, 'a) fold = default
    method fold_merge' : (merge_stmt with_loc, 'a) fold = default
    method fold_move' : (move_stmt with_loc, 'a) fold = default
    method fold_multiply' : (multiply_stmt with_loc, 'a) fold = default
    method fold_open' : (open_stmt with_loc, 'a) fold = default
    method fold_perform_inline' : (perform_inline_stmt with_loc, 'a) fold = default
    method fold_perform_target' : (perform_target_stmt with_loc, 'a) fold = default
    method fold_purge' : (Cobol_ptree.name with_loc with_loc, 'a) fold = default
    method fold_raise' : (raise_operand with_loc, 'a) fold = default
    method fold_read' : (read_stmt with_loc, 'a) fold = default
    method fold_receive' : (receive_stmt with_loc, 'a) fold = default
    method fold_release' : (release_stmt with_loc, 'a) fold = default
    method fold_resume' : (Cobol_ptree.resume_stmt with_loc, 'a) fold = default
    method fold_return' : (return_stmt with_loc, 'a) fold = default
    method fold_rewrite' : (rewrite_stmt with_loc, 'a) fold = default
    method fold_search' : (search_stmt with_loc, 'a) fold = default
    method fold_search_all' : (search_all_stmt with_loc, 'a) fold = default
    method fold_send' : (send_stmt with_loc, 'a) fold = default
    method fold_set' : (set_stmt with_loc, 'a) fold = default
    method fold_sort' : (sort_stmt with_loc, 'a) fold = default
    method fold_start' : (start_stmt with_loc, 'a) fold = default
    method fold_stop' : (stop_stmt with_loc, 'a) fold = default
    method fold_string_stmt' : (string_stmt with_loc, 'a) fold = default
    method fold_subtract' : (basic_arithmetic_stmt with_loc, 'a) fold = default
    method fold_terminate' : (Cobol_ptree.terminate_stmt with_loc, 'a) fold = default
    method fold_transform' : (transform_stmt with_loc, 'a) fold = default
    method fold_unlock' : (Cobol_ptree.unlock_stmt with_loc, 'a) fold = default
    method fold_unstring' : (unstring_stmt with_loc, 'a) fold = default
    method fold_validate' : (ident list with_loc, 'a) fold = default
    method fold_write' : (write_stmt with_loc, 'a) fold = default
  end

(* --- *)

(** Ill-typed parts of the tree are not visited. *)
let fold_or_error ~fold v = function
  | Ok x -> fold v x
  | Error () -> Fun.id

let fold_or_error' ~fold v { payload; loc } =
  fold_or_error v payload ~fold:(fun v x -> fold v (x &@ loc))

let fold_resolved_name (v: _ #folder) =
  handle v#fold_resolved_name
    ~continue:(fun { resolved_name; _ } -> P.fold_name' v resolved_name)

let fold_data_ref (v: _ #folder) =
  handle v#fold_data_ref
    ~continue:(fun { resolved_name; _ } -> P.fold_qualname' v resolved_name)

let fold_data_name_ref (v: _ #folder) =
  handle v#fold_data_name_ref
    ~continue:(fun { resolved_name; _ } -> P.fold_name' v resolved_name)

let fold_literal_value (v: _ #folder) = leaf v#fold_literal_value

let fold_literal_value' (v: _ #folder) = fold' ~fold:fold_literal_value v

let fold_integer_value (v: _ #folder) = leaf v#fold_integer_value

let fold_alphanum_value (v: _ #folder) = leaf v#fold_alphanum_value

let rec fold_ident (v: _ #folder) =
  handle v#fold_ident
    ~continue:(fun { ident_base; ident_refmod } x -> x
                >> fold_base_ident v ident_base
                >> fold_option ~fold:fold_refmod v ident_refmod)

and fold_base_ident (v: _ #folder) =
  handle v#fold_base_ident
    ~continue:(fun i x -> match i with
        | Address a -> x
            >> fold_address v a
        | Counter c -> x
            >> P.fold_counter v c
        | InlineCall c -> x
            >> fold_inline_call v c
        | InlineInvoke i -> x
            >> fold_inline_invocation v i
        | ObjectView o -> x
            >> fold_object_view v o
        | ObjectRef o -> x
            >> P.fold_object_ref v o
        | QualIdent q -> x
            >> fold_qualident v q)

and fold_qualident (v: _ #folder) =
  handle v#fold_qualident
    ~continue:(fun { ident_data_ref; ident_subscripts } x -> x
                >> fold_data_ref v ident_data_ref
                >> fold_list ~fold:fold_subscript' v ident_subscripts)

and fold_subscript (v: _ #folder) =
  handle v#fold_subscript
    ~continue:(fun s x -> match s with
        | SubSAll -> x
        | SubSExpr e -> x
            >> fold_expr' v e
        | SubSIdx (n, i) -> x
            >> fold_data_name_ref v n
            >> fold_integer_value v i)

and fold_subscript' (v: _ #folder) =
  handle' v#fold_subscript' ~fold:fold_subscript v

and fold_refmod (v: _ #folder) =
  handle v#fold_refmod
    ~continue:(fun { refmod_left; refmod_length } x -> x
                >> fold_expr' v refmod_left
                >> fold_option ~fold:fold_expr' v refmod_length)

and fold_address (v: _ #folder) =
  handle v#fold_address
    ~continue:(function
        | DataAddress i -> fold_ident v i
        | ProgAddress i -> fold_ident_or_literal v i)

and fold_inline_call (v: _ #folder) =
  handle v#fold_inline_call
    ~continue:(fun c x -> match c with
        | CallFunc { func; args } -> x
            >> P.fold_name' v func
            >> fold_list ~fold:fold_effective_arg v args
        | CallGenericIntrinsic { func; args } -> x
            >> P.fold_intrinsic_name' v func
            >> fold_list ~fold:fold_effective_arg v args
        | CallTrim { arg; tip } -> x
            >> fold_effective_arg v arg
            >> fold_option ~fold:P.fold_trimming_tip v tip
        | CallLength { arg; physical } -> x
            >> fold_ident_or_nonnum v arg
            >> fold_bool v physical
        | CallNumvalC args -> x
            >> fold_list ~fold:fold_ident_or_nonnum v args
        | CallLocaleDate args
        | CallLocaleTime args
        | CallLocaleTimeFromSeconds args -> x
            >> fold_effective_arg v args.locale_func_args
            >> fold_option ~fold:P.fold_qualname v args.locale_func_locale
        | CallFormattedDatetime args
        | CallFormattedTime args -> x
            >> fold_list ~fold:fold_effective_arg v args.formatted_func_args
            >> fold_bool v args.formatted_func_system_offset)

and fold_inline_invocation (v: _ #folder) =
  handle v#fold_inline_invocation
    ~continue:(fun { invoke_class; invoke_meth; invoke_args } x -> x
                >> fold_ident v invoke_class
                >> fold_or_error ~fold:fold_literal_value v invoke_meth
                >> fold_list ~fold:fold_effective_arg v invoke_args)

and fold_effective_arg (v: _ #folder) =
  handle v#fold_effective_arg
    ~continue:(function
        | ArgExpr e -> fold_expr' v e
        | ArgOmitted -> Fun.id)

and fold_object_view (v: _ #folder) =
  handle v#fold_object_view
    ~continue:(fun { object_view_ident; object_view_spec } x -> x
                >> fold_ident v object_view_ident
                >> P.fold_object_view_spec v object_view_spec)

and fold_expr (v: _ #folder) =
  handle v#fold_expr
    ~continue:(fun e x -> match e with
        | Atom a -> x
            >> fold_scalar v a
        | Unop (o, e) -> x
            >> P.fold_unop v o
            >> fold_expr' v e
        | Binop (e, o, e') -> x
            >> fold_expr' v e
            >> P.fold_binop v o
            >> fold_expr' v e')

and fold_expr' (v: _ #folder) = handle' v#fold_expr' ~fold:fold_expr v

and fold_ident_or_literal (v: _ #folder) : ident_or_literal -> 'a -> 'a =
  function
  | Ident i -> fold_ident v i
  | Literal l -> fold_or_error ~fold:fold_literal_value v l

and fold_ident_or_nonnum (v: _ #folder) : ident_or_nonnum -> 'a -> 'a =
  function
  | Ident i -> fold_ident v i
  | Literal l -> fold_or_error ~fold:fold_literal_value v l

and fold_scalar (v: _ #folder) : scalar -> 'a -> 'a = function
  | ScalarIdent i -> fold_ident v i
  | ScalarLiteral l -> fold_or_error ~fold:fold_literal_value v l
  | LengthOf i -> fold_length_of v i

and fold_length_of (v: _ #folder) =
  handle v#fold_length_of ~continue:(fold_ident_or_literal v)

let fold_ident_or_alphanum (v: _ #folder) : ident_or_alphanum -> 'a -> 'a =
  function
  | Ident i -> fold_ident v i
  | Literal a -> fold_or_error ~fold:fold_alphanum_value v a

let fold_ident_or_intlit (v: _ #folder) : ident_or_intlit -> 'a -> 'a =
  function
  | Ident i -> fold_ident v i
  | Literal i -> fold_or_error ~fold:fold_integer_value v i

let fold_ident_or_strlit (v: _ #folder) : ident_or_strlit -> 'a -> 'a =
  function
  | Ident i -> fold_ident v i
  | Literal l -> fold_or_error ~fold:fold_literal_value v l

let fold_ident' (v: _ #folder) = fold' ~fold:fold_ident v

let fold_ident'_opt (v: _ #folder) = fold_option ~fold:fold_ident' v

let fold_scalar' (v: _ #folder) = fold' ~fold:fold_scalar v

let fold_ident_or_literal' (v: _ #folder) = fold' ~fold:fold_ident_or_literal v

let fold_ident_or_nonnum' (v: _ #folder) = fold' ~fold:fold_ident_or_nonnum v

let fold_ident_or_intlit' (v: _ #folder) = fold' ~fold:fold_ident_or_intlit v

let rec fold_condition (v: _ #folder) =
  handle v#fold_condition
    ~continue:(fun c x -> match c with
        | Expr e -> x
            >> fold_expr' v e
        | Relation (e, r, e') -> x
            >> fold_expr' v e
            >> P.fold_relop v r
            >> fold_expr' v e'
        | ClassCond (e, c) -> x
            >> fold_expr' v e
            >> P.fold_class v c
        | SignCond (e, s) -> x
            >> fold_expr' v e
            >> P.fold_signz v s
        | Omitted e -> x
            >> fold_expr' v e
        | Not c -> x
            >> fold_condition_or_error' v c
        | Combined (c, o, c') -> x
            >> fold_condition_or_error' v c
            >> P.fold_logop v o
            >> fold_condition_or_error' v c')

and fold_condition' (v: _ #folder) =
  handle' v#fold_condition' ~fold:fold_condition v

and fold_condition_or_error' (v: _ #folder) =
  handle v#fold_condition_or_error'
    ~continue:(fold_or_error' ~fold:fold_condition' v)

(* --- Operands *)

let fold_alphabet_specification (v: _ #folder) =
  handle v#fold_alphabet_specification
    ~continue:(fun Cobol_ptree.{ alphanumeric; national } x -> x
                >> P.fold_name'_opt v alphanumeric
                >> P.fold_name'_opt v national)

let fold_rounded_ident (v: _ #folder) =
  handle v#fold_rounded_ident
    ~continue:(fun Cobol_ptree.{ rounded; rounded_rounding } x -> x
                >> fold_ident v rounded
                >> P.fold_rounding v rounded_rounding)

let fold_rounded_idents (v: _ #folder) = fold_list ~fold:fold_rounded_ident v

let fold_basic_arithmetic_operands (v: _ #folder) =
  handle v#fold_basic_arithmetic_operands
    ~continue:(fun o x -> match o with
        | ArithSimple { operands; targets } -> x
            >> fold_list ~fold:fold_scalar v operands
            >> fold_rounded_idents v targets
        | ArithGiving { leading_operands; base_operand; targets } -> x
            >> fold_list ~fold:fold_scalar v leading_operands
            >> fold_scalar v base_operand
            >> fold_rounded_idents v targets
        | ArithCorresponding { source; target } -> x
            >> fold_data_ref v source
            >> fold_rounded_ident v target)

let fold_using_arg_default' (v: _ #folder) =
  handle' v#fold_using_arg_default' v
    ~fold:(fun v a -> match a with
        | ArgDefaultOmitted -> Fun.id
        | ArgDefault e -> fold_expr' v e)

let fold_using_reference_arg' (v: _ #folder) =
  handle' v#fold_using_reference_arg' v
    ~fold:(fun v a -> match a with
        | ArgRefOmitted -> Fun.id
        | ArgRef a -> fold_scalar' v a)

let fold_call_using_clause (v: _ #folder) =
  handle v#fold_call_using_clause
    ~continue:(function
        | CallUsingDefault args -> fold_nel ~fold:fold_using_arg_default' v args
        | CallUsingByReference args ->
            fold_nel ~fold:fold_using_reference_arg' v args
        | CallUsingByContent args
        | CallUsingByValue args -> fold_nel ~fold:fold_expr' v args)

let fold_date_time (v: _ #folder) = leaf v#fold_date_time

let fold_divide_operands (v: _ #folder) =
  handle v#fold_divide_operands
    ~continue:(fun o x -> match o with
        | DivideInto { divisor; dividends } -> x
            >> fold_scalar v divisor
            >> fold_rounded_idents v dividends
        | DivideGiving { divisor; dividend; giving; into; remainder } -> x
            >> fold_scalar v divisor
            >> fold_scalar v dividend
            >> fold_rounded_idents v giving
            >> fold_bool v into
            >> fold_option ~fold:fold_ident v remainder)

let fold_file_option (v: _ #folder) = leaf v#fold_file_option

let fold_multiply_operands (v: _ #folder) =
  handle v#fold_multiply_operands
    ~continue:(fun o x -> match o with
        | MultiplyBy { multiplier; multiplicand } -> x
            >> fold_scalar v multiplier
            >> fold_rounded_idents v multiplicand
        | MultiplyGiving { multiplier; multiplicand; targets } -> x
            >> fold_scalar v multiplier
            >> fold_scalar v multiplicand
            >> fold_rounded_idents v targets)

let fold_mcs_awaiting_item (v: _ #folder) = leaf v#fold_mcs_awaiting_item

let fold_mcs_kind (v: _ #folder) = leaf v#fold_mcs_kind

let fold_mcs_command_operands (v: _ #folder) =
  handle v#fold_mcs_command_operands
    ~continue:(fun { mcs_command_kind; mcs_command_target;
                     mcs_command_key } x -> x
                >> fold_mcs_kind v mcs_command_kind
                >> P.fold_name' v mcs_command_target
                >> fold_option ~fold:fold_ident_or_alphanum v mcs_command_key)

let fold_open_mode (v: _ #folder) = leaf v#fold_open_mode

let fold_position (v: _ #folder) =
  handle v#fold_position
    ~continue:(fun p x -> match p with
        | LinePosition i
        | ColumnPosition i
        | CompoundPosition i -> x
            >> fold_ident_or_intlit v i
        | LineColumnPosition (i, j) -> x
            >> fold_ident_or_intlit v i
            >> fold_ident_or_intlit v j)

let fold_raising (v: _ #folder) =
  handle v#fold_raising
    ~continue:(function
        | RaisingIdent i -> fold_ident v i
        | RaisingException n -> P.fold_name' v n
        | RaisingLastException -> Fun.id)

let fold_read_direction (v: _ #folder) = leaf v#fold_read_direction

let fold_retry_clause (v: _ #folder) =
  handle v#fold_retry_clause
    ~continue:(function
        | RetryNTimes e
        | RetryForNSeconds e -> fold_expr' v e
        | RetryForever -> Fun.id)

let fold_read_lock_behavior (v: _ #folder) =
  handle v#fold_read_lock_behavior
    ~continue:(function
        | ReadAdvancingOnLock
        | ReadIgnoringLock -> Fun.id
        | ReadRetry retry_clause -> fold_retry_clause v retry_clause)

let fold_search_condition (v: _ #folder) =
  handle v#fold_search_condition
    ~continue:(fun cond x -> match cond with
        | IsEqual { data_item; condition } -> x
            >> fold_qualident v data_item
            >> fold_expr' v condition
        | Cond q -> x
            >> fold_qualident v q)

let fold_sharing_mode (v: _ #folder) = leaf v#fold_sharing_mode

let fold_stage (v: _ #folder) = leaf v#fold_stage

let fold_advancing_phrase (v: _ #folder) =
  handle v#fold_advancing_phrase
    ~continue:(fun phrase x -> match phrase with
        | AdvancingLines { stage; lines; ambiguous } -> x
            >> fold_stage v stage
            >> fold_ident_or_intlit v lines
            >> fold_bool v ambiguous
        | AdvancingPage { stage } -> x
            >> fold_stage v stage)

let fold_start_position (v: _ #folder) =
  handle v#fold_start_position
    ~continue:(fun pos x -> match pos with
        | StartPositionFirst
        | StartPositionLast -> x
        | StartPositionKey { operator; name; length } -> x
            >> P.fold_relop v operator
            >> fold_data_ref v name
            >> fold_option ~fold:fold_expr' v length)

let fold_write_target (v: _ #folder) =
  handle v#fold_write_target
    ~continue:(function
        | WriteTargetName r -> fold_data_ref v r
        | WriteTargetFile n -> P.fold_name' v n)

let fold_procedure_range (v: _ #folder) =
  handle v#fold_procedure_range
    ~continue:(fun Cobol_ptree.{ procedure_start; procedure_end } x -> x
                >> P.fold_procedure_name' v procedure_start
                >> P.fold_procedure_name'_opt v procedure_end)

let fold_selection_subject (v: _ #folder) =
  handle v#fold_selection_subject
    ~continue:(function
        | SubjectValue e -> fold_expr' v e
        | SubjectCond c -> fold_condition v c
        | SubjectConst b -> fold_bool v b)

let fold_selection_subject' (v: _ #folder) =
  handle' v#fold_selection_subject' ~fold:fold_selection_subject v

let fold_selection_object (v: _ #folder) =
  handle v#fold_selection_object
    ~continue:(fun o x -> match o with
        | SelCond c -> x
            >> fold_condition v c
        | SelValue { negated; value } -> x
            >> fold_bool v negated
            >> fold_expr' v value
        | SelRange { sel_range_negated; sel_range_start; sel_range_stop;
                     sel_range_alphabet } -> x
            >> fold_bool v sel_range_negated
            >> fold_expr' v sel_range_start
            >> fold_expr' v sel_range_stop
            >> P.fold_name'_opt v sel_range_alphabet
        | SelConst b -> x
            >> fold_bool v b
        | SelAny -> x)

let fold_selection_object' (v: _ #folder) =
  handle' v#fold_selection_object' ~fold:fold_selection_object v

(* SET *)

let fold_screen_attribute (v: _ #folder) = leaf v#fold_screen_attribute

let fold_set_ambiguous_method (v: _ #folder) = leaf v#fold_set_ambiguous_method

let fold_on_off (v: _ #folder) = leaf v#fold_on_off

let fold_locale_category (v: _ #folder) = leaf v#fold_locale_category

let fold_set_save_locale (v: _ #folder) = leaf v#fold_set_save_locale

let fold_set_attribute_switch (v: _ #folder) =
  handle v#fold_set_attribute_switch
    ~continue:(fun Cobol_ptree.{ set_attribute;
                                 set_attribute_switch_value } x -> x
                >> fold_screen_attribute v set_attribute
                >> fold_on_off v set_attribute_switch_value)

let fold_set_locale_target (v: _ #folder) =
  handle v#fold_set_locale_target
    ~continue:(function
        | Cobol_ptree.SetLocaleTarget c -> fold_locale_category v c
        | SetLocaleTargetUserDefault -> Fun.id)

let fold_set_locale_source (v: _ #folder) =
  handle v#fold_set_locale_source
    ~continue:(function
        | SetLocaleSource i -> fold_ident v i
        | SetLocaleSourceUserDefault
        | SetLocaleSourceSystemDefault -> Fun.id)

let fold_float_content (v: _ #folder) =
  handle v#fold_float_content
    ~continue:(function
        | Cobol_ptree.FarthestFromZero b
        | NearestToZero b -> fold_bool v b
        | FloatInfinity
        | FloatNotANumber
        | FloatNotANumberSignaling -> Fun.id)

(* Clauses shared with data descriptions *)

let fold_screen_attribute_clause (v: _ #folder) =
  handle v#fold_screen_attribute_clause
    ~continue:(function
        | Bell
        | Blink
        | Highlight
        | Lowlight
        | ReverseVideo
        | Underline -> Fun.id
        | ForegroundColor i
        | BackgroundColor i -> fold_ident_or_intlit v i)

let fold_erase_clause (v: _ #folder) = leaf v#fold_erase_clause

let fold_blank_clause (v: _ #folder) = leaf v#fold_blank_clause

let fold_sort_direction (v: _ #folder) = leaf v#fold_sort_direction

let fold_sort_spec (v: _ #folder) =
  handle v#fold_sort_spec
    ~continue:(fun { sort_key_direction; sort_key_names } x -> x
                >> fold_sort_direction v sort_key_direction
                >> fold_list ~fold:fold_data_ref v sort_key_names)

(* --- Statement parts *)

let fold_varying_phrase (v: _ #folder) =
  handle v#fold_varying_phrase
    ~continue:(fun { varying_ident; varying_from; varying_by;
                     varying_until } x -> x
                >> fold_ident v varying_ident
                >> fold_scalar v varying_from
                >> fold_option ~fold:fold_scalar v varying_by
                >> fold_condition_or_error' v varying_until)

let fold_varying_phrase' (v: _ #folder) =
  handle' v#fold_varying_phrase' ~fold:fold_varying_phrase v

let fold_perform_mode (v: _ #folder) =
  handle v#fold_perform_mode
    ~continue:(fun m x -> match m with
        | PerformForever -> x
        | PerformNTimes i -> x
            >> fold_ident_or_intlit v i
        | PerformUntil { with_test; until } -> x
            >> fold_option ~fold:fold_stage v with_test
            >> fold_option ~fold:fold_condition_or_error' v until
        | PerformVarying { with_test; varying; after } -> x
            >> fold_option ~fold:fold_stage v with_test
            >> fold_varying_phrase' v varying
            >> fold_list ~fold:fold_varying_phrase' v after)

let fold_allocate_kind (v: _ #folder) =
  handle v#fold_allocate_kind
    ~continue:(function
        | AllocateCharacters e -> fold_expr' v e
        | AllocateDataItem n -> fold_data_name_ref v n)

let fold_alter_operands' (v: _ #folder) =
  handle' v#fold_alter_operands' v
    ~fold:(fun v Cobol_ptree.{ alter_source; alter_target } x -> x
            >> P.fold_procedure_name' v alter_source
            >> P.fold_procedure_name' v alter_target)

let fold_call_proto (v: _ #folder) =
  handle v#fold_call_proto
    ~continue:(function
        | CallProtoIdent i -> fold_ident v i
        | CallProtoNested -> Fun.id)

let fold_call_target (v: _ #folder) =
  handle v#fold_call_target
    ~continue:(fun p x -> match p with
        | CallGeneral i -> x
            >> fold_ident_or_strlit v i
        | CallProto { called; prototype } -> x
            >> fold_option ~fold:fold_ident_or_strlit v called
            >> fold_call_proto v prototype)

let fold_close_format (v: _ #folder) =
  handle v#fold_close_format
    ~continue:(function
        | Cobol_ptree.CloseUnitReel b -> fold_bool v b
        | CloseWithLock
        | CloseWithNoRewind -> Fun.id)

let fold_close_phrase (v: _ #folder) =
  handle v#fold_close_phrase
    ~continue:(fun Cobol_ptree.{ close_item; close_format } x -> x
                >> P.fold_name' v close_item
                >> fold_option ~fold:fold_close_format v close_format)

let fold_init_data_category (v: _ #folder) = leaf v#fold_init_data_category

let fold_init_category (v: _ #folder) = function
  | Cobol_ptree.InitAll -> Fun.id
  | InitCategory c -> fold_init_data_category v c

let fold_init_replacing (v: _ #folder) =
  handle v#fold_init_replacing
    ~continue:(fun { init_replacing_category;
                     init_replacing_replacement_item } x -> x
                >> fold_init_data_category v init_replacing_category
                >> fold_ident_or_literal v init_replacing_replacement_item)

let fold_inspect_where (v: _ #folder) =
  handle v#fold_inspect_where
    ~continue:(fun (_, reference) ->
        fold_nel ~fold:fold_ident_or_nonnum v reference)

let fold_tallying_spec (v: _ #folder) =
  handle v#fold_tallying_spec
    ~continue:(fun { tallying_item; tallying_where } x -> x
                >> fold_ident_or_nonnum v tallying_item
                >> fold_list ~fold:fold_inspect_where v tallying_where)

let fold_tallying_clause' (v: _ #folder) =
  handle' v#fold_tallying_clause' v
    ~fold:(fun v clause x -> match clause with
        | TallyingCharacters wheres -> x
            >> fold_list ~fold:fold_inspect_where v wheres
        | TallyingRange (_, specs) -> x
            >> fold_list ~fold:fold_tallying_spec v specs)

let fold_tallying (v: _ #folder) =
  handle v#fold_tallying
    ~continue:(fun { tallying_target; tallying_clauses } x -> x
                >> fold_qualident v tallying_target
                >> fold_list ~fold:fold_tallying_clause' v tallying_clauses)

let fold_replacing_range_spec (v: _ #folder) =
  handle v#fold_replacing_range_spec
    ~continue:(fun { replacing_item; replacing_by; replacing_where } x -> x
                >> fold_ident_or_nonnum v replacing_item
                >> fold_ident_or_nonnum v replacing_by
                >> fold_list ~fold:fold_inspect_where v replacing_where)

let fold_replacing_clause (v: _ #folder) =
  handle v#fold_replacing_clause
    ~continue:(fun clause x -> match clause with
        | ReplacingCharacters { replacement; where } -> x
            >> fold_ident_or_nonnum v replacement
            >> fold_list ~fold:fold_inspect_where v where
        | ReplacingRange (_, specs) -> x
            >> fold_list ~fold:fold_replacing_range_spec v specs)

let fold_replacing (v: _ #folder) =
  handle' v#fold_replacing v ~fold:fold_replacing_clause

let fold_converting (v: _ #folder) =
  handle v#fold_converting
    ~continue:(fun { converting_from; converting_to; converting_where } x -> x
                >> fold_ident_or_nonnum v converting_from
                >> fold_ident_or_nonnum v converting_to
                >> fold_list ~fold:fold_inspect_where v converting_where)

let fold_inspect_spec (v: _ #folder) =
  handle v#fold_inspect_spec
    ~continue:(fun spec x -> match spec with
        | InspectTallying tl -> x
            >> fold_list ~fold:fold_tallying v tl
        | InspectReplacing rl -> x
            >> fold_list ~fold:fold_replacing v rl
        | InspectBoth (tl, rl) -> x
            >> fold_list ~fold:fold_tallying v tl
            >> fold_list ~fold:fold_replacing v rl
        | InspectConverting c -> x
            >> fold_converting v c)

let fold_merge_or_sort_target (v: _ #folder) =
  handle v#fold_merge_or_sort_target
    ~continue:(function
        | Cobol_ptree.OutputProcedure range -> fold_procedure_range v range
        | Giving names -> P.fold_name'_list v names)

let fold_named_file_option (v: _ #folder) =
  handle v#fold_named_file_option
    ~continue:(fun Cobol_ptree.{ named_file_name; named_file_option } x -> x
                >> P.fold_name' v named_file_name
                >> fold_option ~fold:fold_file_option v named_file_option)

let fold_open_phrase (v: _ #folder) =
  handle v#fold_open_phrase
    ~continue:(fun { open_mode; open_sharing; open_retry; open_files } x -> x
                >> fold_open_mode v open_mode
                >> fold_option ~fold:fold_sharing_mode v open_sharing
                >> fold_option ~fold:fold_retry_clause v open_retry
                >> fold_list ~fold:fold_named_file_option v open_files)

let fold_read_error (v: _ #folder) = leaf v#fold_read_error

let fold_set_switch_spec (v: _ #folder) =
  handle v#fold_set_switch_spec
    ~continue:(fun { set_switch_targets; set_switch_value } x -> x
                >> fold_list ~fold:fold_ident v set_switch_targets
                >> fold_on_off v set_switch_value)

let fold_set_condition_spec (v: _ #folder) =
  handle v#fold_set_condition_spec
    ~continue:(fun { set_condition_targets; set_condition_value } x -> x
                >> fold_list ~fold:fold_ident v set_condition_targets
                >> fold_bool v set_condition_value)

let fold_status_kind (v: _ #folder) = leaf v#fold_status_kind

let fold_string_delimiter (v: _ #folder) =
  handle v#fold_string_delimiter
    ~continue:(function
        | StringDelimiter i -> fold_ident_or_nonnum v i
        | StringDelimiterSize -> Fun.id)

let fold_string_source (v: _ #folder) =
  handle v#fold_string_source
    ~continue:(fun { string_source; string_delimiter } x -> x
                >> fold_ident_or_nonnum v string_source
                >> fold_option ~fold:fold_string_delimiter v string_delimiter)

let fold_unstring_delimiter (v: _ #folder) =
  handle v#fold_unstring_delimiter
    ~continue:(fun { unstring_delimiter; unstring_delimiter_by_all } x -> x
                >> fold_ident_or_strlit v unstring_delimiter
                >> fold_bool v unstring_delimiter_by_all)

let fold_unstring_target (v: _ #folder) =
  handle v#fold_unstring_target
    ~continue:(fun { unstring_target; unstring_target_delimiter;
                     unstring_target_count } x -> x
                >> fold_ident v unstring_target
                >> fold_option ~fold:fold_ident v unstring_target_delimiter
                >> fold_option ~fold:fold_ident v unstring_target_count)

let fold_write_error (v: _ #folder) = leaf v#fold_write_error

let fold_message_ending_indicator (v: _ #folder) =
  handle v#fold_message_ending_indicator
    ~continue:(function
        | EndingIndicator i -> fold_ident v i
        | EndingIndicatorESI
        | EndingIndicatorEMI
        | EndingIndicatorEGI -> Fun.id)

let fold_send_operands (v: _ #folder) =
  handle v#fold_send_operands
    ~continue:(fun o x -> match o with
        | SendSimple { from } -> x
            >> fold_ident v from
        | SendWith { from; ending_indicator; advancing; replace } -> x
            >> fold_option ~fold:fold_ident v from
            >> fold_message_ending_indicator v ending_indicator
            >> fold_option ~fold:fold_advancing_phrase v advancing
            >> fold_bool v replace)

let fold_sort_source (v: _ #folder) =
  handle v#fold_sort_source
    ~continue:(function
        | Cobol_ptree.SortInputProcedure range -> fold_procedure_range v range
        | SortUsing names -> P.fold_name'_list v names)

let fold_returning (v: _ #folder) =
  handle v#fold_returning
    ~continue:(fun r x -> match r with
        | ReturningScalar s -> x
            >> fold_scalar v s
        | ReturningAddress a -> x
            >> fold_qualident v a
        | ReturningInt { value; size } -> x
            >> fold_or_error ~fold:fold_literal_value v value
            >> fold_option ~fold:(fold_or_error ~fold:fold_literal_value) v size)

let fold_stop_run_return (v: _ #folder) =
  handle v#fold_stop_run_return
    ~continue:(fun r x -> match r with
        | StopReturning r -> x
            >> fold_returning v r
        | StopWithStatus { status_kind; status_value } -> x
            >> fold_status_kind v status_kind
            >> fold_option ~fold:fold_scalar v status_value)

let fold_stop_arg (v: _ #folder) =
  handle v#fold_stop_arg
    ~continue:(fun a x -> match a with
        | StopWithQualIdent i -> x
            >> fold' ~fold:fold_qualident v i
        | StopWithLiteral l -> x
            >> fold_or_error' ~fold:fold_literal_value' v l)

(* --- Statements that do not need recursion *)

let fold_allocate' (v: _ #folder) =
  handle' v#fold_allocate' v
    ~fold:(fun v { allocate_kind; allocate_initialized;
                   allocate_returning } x -> x
            >> fold_allocate_kind v allocate_kind
            >> fold_bool v allocate_initialized
            >> fold_ident'_opt v allocate_returning)

let fold_alter' (v: _ #folder) =
  handle' v#fold_alter' v ~fold:(fold_list ~fold:fold_alter_operands')

let fold_cancel' (v: _ #folder) =
  handle' v#fold_cancel' v ~fold:(fold_list ~fold:fold_ident_or_strlit)

let fold_close' (v: _ #folder) =
  handle' v#fold_close' v ~fold:(fold_list ~fold:fold_close_phrase)

let fold_disable' (v: _ #folder) =
  handle' v#fold_disable' v ~fold:fold_mcs_command_operands

let fold_enable' (v: _ #folder) =
  handle' v#fold_enable' v ~fold:fold_mcs_command_operands

let fold_enter' (v: _ #folder) =
  handle' v#fold_enter' v
    ~fold:(fun v Cobol_ptree.{ enter_language; enter_routine } x -> x
            >> P.fold_name' v enter_language
            >> P.fold_name'_opt v enter_routine)

let fold_entry_by_clause (v: _ #folder) =
  handle v#fold_entry_by_clause
    ~continue:(function
        | EntryByReference l
        | EntryByValue l -> fold_list ~fold:fold_data_name_ref v l)

let fold_entry' (v: _ #folder) =
  handle' v#fold_entry' v
    ~fold:(fun v stmt x -> match stmt with
        | EntrySimple name
        | EntryForGoTo name -> x
            >> fold_string' v name
        | EntryUsing { entry_name; entry_by_clauses } -> x
            >> fold_string' v entry_name
            >> fold_list ~fold:fold_entry_by_clause v entry_by_clauses)

let fold_exec_block' (v: _ #folder) =
  leaf' v#fold_exec_block' v (* Note: leaf *)

let fold_exit' (v: _ #folder) =
  handle' v#fold_exit' v
    ~fold:(fun v -> function
        | ExitSimple
        | ExitProgram ExitDefault
        | ExitParagraph
        | ExitSection -> Fun.id
        | ExitPerform b -> fold_bool v b
        | ExitProgram (ExitReturning r) -> fold_returning v r
        | ExitProgram (ExitRaising r) -> fold_raising v r
        | ExitMethod r
        | ExitFunction r -> fold_option ~fold:fold_raising v r)

let fold_free' (v: _ #folder) =
  handle' v#fold_free' v ~fold:(fold_list ~fold:fold_data_ref)

let fold_generate' (v: _ #folder) =
  handle' v#fold_generate' v ~fold:P.fold_qualname'

let fold_goback' (v: _ #folder) =
  handle' v#fold_goback' v
    ~fold:(fun v { goback_raising; goback_returning } x -> x
            >> fold_option ~fold:fold_raising v goback_raising
            >> fold_option ~fold:fold_ident_or_intlit' v goback_returning)

let fold_goto' (v: _ #folder) =
  handle' v#fold_goto' v
    ~fold:(fun v s x -> match s with
        | GoToSimple { target } -> x
            >> P.fold_procedure_name' v target
        | GoToDepending { targets; depending_on } -> x
            >> fold_nel ~fold:P.fold_procedure_name' v targets
            >> fold_ident v depending_on
        | GoToEntry { targets; depending_on } -> x
            >> fold_nel ~fold:fold_string' v targets
            >> fold_option ~fold:fold_ident v depending_on)

let fold_initialize' (v: _ #folder) =
  handle' v#fold_initialize' v
    ~fold:(fun v { init_items; init_filler; init_category; init_replacings;
                   init_to_default } x -> x
            >> fold_list ~fold:fold_ident v init_items
            >> fold_bool v init_filler
            >> fold_option ~fold:fold_init_category v init_category
            >> fold_list ~fold:fold_init_replacing v init_replacings
            >> fold_bool v init_to_default)

let fold_initiate' (v: _ #folder) =
  handle' v#fold_initiate' v ~fold:P.fold_name'_list

let fold_inspect' (v: _ #folder) =
  handle' v#fold_inspect' v
    ~fold:(fun v { inspect_item; inspect_spec } x -> x
            >> fold_ident v inspect_item
            >> fold_inspect_spec v inspect_spec)

let fold_invoke' (v: _ #folder) =
  handle' v#fold_invoke' v
    ~fold:(fun v { invoke_target; invoke_method; invoke_using;
                   invoke_returning } x -> x
            >> fold_ident v invoke_target
            >> fold_ident_or_strlit v invoke_method
            >> fold_list ~fold:fold_call_using_clause v invoke_using
            >> fold_ident'_opt v invoke_returning)

let fold_merge' (v: _ #folder) =
  handle' v#fold_merge' v
    ~fold:(fun v { merge_file; merge_keys; merge_collating; merge_using;
                   merge_target } x -> x
            >> P.fold_name' v merge_file
            >> fold_list ~fold:fold_sort_spec v merge_keys
            >> fold_option ~fold:fold_alphabet_specification v merge_collating
            >> P.fold_name'_list v merge_using
            >> fold_merge_or_sort_target v merge_target)

let fold_move' (v: _ #folder) =
  handle' v#fold_move' v
    ~fold:(fun v m x -> match m with
        | MoveSimple { from; to_ } -> x
            >> fold_scalar v from
            >> fold_list ~fold:fold_ident v to_
        | MoveCorresponding { from; to_ } -> x
            >> fold_ident v from
            >> fold_list ~fold:fold_ident v to_)

let fold_open' (v: _ #folder) =
  handle' v#fold_open' v ~fold:(fold_list ~fold:fold_open_phrase)

let fold_purge' (v: _ #folder) = handle' v#fold_purge' v ~fold:P.fold_name'

let fold_raise' (v: _ #folder) =
  handle' v#fold_raise' v
    ~fold:(fun v -> function
        | RaiseIdent i -> fold_ident v i
        | RaiseException n -> P.fold_name' v n)

let fold_release' (v: _ #folder) =
  handle' v#fold_release' v
    ~fold:(fun v { release_item; release_from } x -> x
            >> fold_data_ref v release_item
            >> fold_option ~fold:fold_ident_or_literal v release_from)

let fold_resume' (v: _ #folder) =
  handle' v#fold_resume' v
    ~fold:(fun v -> function
        | Cobol_ptree.ResumeNextStatement -> Fun.id
        | ResumeTarget t -> P.fold_procedure_name' v t)

let fold_send' (v: _ #folder) =
  handle' v#fold_send' v
    ~fold:(fun v { send_name; send_operands } x -> x
            >> P.fold_name' v send_name
            >> fold_send_operands v send_operands)

let fold_set' (v: _ #folder) =
  handle' v#fold_set' v
    ~fold:(fun v stmt x -> match stmt with
        | SetAmbiguous { targets; set_method; value } -> x
            >> fold_list ~fold:fold_ident v targets
            >> fold_set_ambiguous_method v set_method
            >> fold_expr' v value
        | SetSwitch specs -> x
            >> fold_list ~fold:fold_set_switch_spec v specs
        | SetCondition specs -> x
            >> fold_list ~fold:fold_set_condition_spec v specs
        | SetEntry { targets; value } -> x
            >> fold_list ~fold:fold_ident v targets
            >> fold_ident_or_nonnum v value
        | SetAttribute { name; attribute_switches } -> x
            >> P.fold_name' v name
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
            >> fold_option ~fold:P.fold_sign v sign
        | SetEnvironment { variable; value } -> x
            >> fold_ident_or_literal v variable
            >> fold_ident_or_literal v value)

let fold_sort' (v: _ #folder) =
  handle' v#fold_sort' v
    ~fold:(fun v s x -> match s with
        | SortFile { file; keys; duplicate_in_order; collating; source;
                     target } -> x
            >> P.fold_qualname' v file.ident_name
            >> fold_list ~fold:fold_sort_spec v keys
            >> fold_bool v duplicate_in_order
            >> fold_option ~fold:fold_alphabet_specification v collating
            >> fold_sort_source v source
            >> fold_merge_or_sort_target v target
        | SortTable { table; keys; duplicate_in_order; collating } -> x
            >> fold_qualident v table
            >> fold_list ~fold:fold_sort_spec v keys
            >> fold_bool v duplicate_in_order
            >> fold_option ~fold:fold_alphabet_specification v collating)

let fold_stop' (v: _ #folder) =
  handle' v#fold_stop' v
    ~fold:(fun v -> function
        | StopArg a -> fold_option ~fold:fold_stop_arg v a
        | StopRun r -> fold_option ~fold:fold_stop_run_return v r
        | StopError -> Fun.id
        | StopThread i -> fold_option ~fold:fold_qualident v i)

let fold_terminate' (v: _ #folder) =
  handle' v#fold_terminate' v ~fold:P.fold_name'_list

let fold_transform' (v: _ #folder) =
  handle' v#fold_transform' v
    ~fold:(fun v { transform_ident; transform_from; transform_to } x -> x
            >> fold_ident' v transform_ident
            >> fold_ident_or_nonnum' v transform_from
            >> fold_ident_or_nonnum' v transform_to)

let fold_unlock' (v: _ #folder) =
  handle' v#fold_unlock' v
    ~fold:(fun v Cobol_ptree.{ unlock_file; unlock_record } x -> x
            >> P.fold_name' v unlock_file
            >> fold_bool v unlock_record)

let fold_validate' (v: _ #folder) =
  handle' v#fold_validate' v ~fold:(fold_list ~fold:fold_ident)

(* --- Statements with high-level control structure and/or (inline)
   handlers *)

let rec fold_statement' (v: _ #folder) =
  handle v#fold_statement'
    ~continue:(fun { payload; loc } -> match payload with
        | Accept s -> fold_accept' v (s &@ loc)
        | Allocate s -> fold_allocate' v (s &@ loc)
        | Add s -> fold_add' v (s &@ loc)
        | Alter s -> fold_alter' v (s &@ loc)
        | Call s -> fold_call' v (s &@ loc)
        | Cancel s -> fold_cancel' v (s &@ loc)
        | Close s -> fold_close' v (s &@ loc)
        | Compute s -> fold_compute' v (s &@ loc)
        | Delete s -> fold_delete' v (s &@ loc)
        | Disable s -> fold_disable' v (s &@ loc)
        | Display s -> fold_display' v (s &@ loc)
        | Divide s -> fold_divide' v (s &@ loc)
        | Enable s -> fold_enable' v (s &@ loc)
        | Enter s -> fold_enter' v (s &@ loc)
        | Entry s -> fold_entry' v (s &@ loc)
        | Evaluate s -> fold_evaluate' v (s &@ loc)
        | ExecBlock s -> fold_exec_block' v (s &@ loc)
        | Exit s -> fold_exit' v (s &@ loc)
        | Free s -> fold_free' v (s &@ loc)
        | Generate s -> fold_generate' v (s &@ loc)
        | GoBack s -> fold_goback' v (s &@ loc)
        | GoTo s -> fold_goto' v (s &@ loc)
        | If s -> fold_if' v (s &@ loc)
        | Initialize s -> fold_initialize' v (s &@ loc)
        | Initiate s -> fold_initiate' v (s &@ loc)
        | Inspect s -> fold_inspect' v (s &@ loc)
        | Invoke s -> fold_invoke' v (s &@ loc)
        | Merge s -> fold_merge' v (s &@ loc)
        | Move s -> fold_move' v (s &@ loc)
        | Multiply s -> fold_multiply' v (s &@ loc)
        | Open s -> fold_open' v (s &@ loc)
        | PerformInline s -> fold_perform_inline' v (s &@ loc)
        | PerformTarget s -> fold_perform_target' v (s &@ loc)
        | Purge s -> fold_purge' v (s &@ loc)
        | Raise s -> fold_raise' v (s &@ loc)
        | Read s -> fold_read' v (s &@ loc)
        | Receive s -> fold_receive' v (s &@ loc)
        | Release s -> fold_release' v (s &@ loc)
        | Resume s -> fold_resume' v (s &@ loc)
        | Return s -> fold_return' v (s &@ loc)
        | Rewrite s -> fold_rewrite' v (s &@ loc)
        | Search s -> fold_search' v (s &@ loc)
        | SearchAll s -> fold_search_all' v (s &@ loc)
        | Send s -> fold_send' v (s &@ loc)
        | Set s -> fold_set' v (s &@ loc)
        | Sort s -> fold_sort' v (s &@ loc)
        | Start s -> fold_start' v (s &@ loc)
        | Stop s -> fold_stop' v (s &@ loc)
        | String s -> fold_string_stmt' v (s &@ loc)
        | Subtract s -> fold_subtract' v (s &@ loc)
        | Terminate s -> fold_terminate' v (s &@ loc)
        | Transform s -> fold_transform' v (s &@ loc)
        | Unlock s -> fold_unlock' v (s &@ loc)
        | Unstring s -> fold_unstring' v (s &@ loc)
        | Validate s -> fold_validate' v (s &@ loc)
        | Write s -> fold_write' v (s &@ loc)
        | Continue
        | LoneGoTo
        | NextSentence
        | Suppress -> Fun.id)

and fold_statements (v: _ #folder) = fold_list ~fold:fold_statement' v

and fold_statements' (v: _ #folder) =
  handle' v#fold_statements' ~fold:fold_statements v

and fold_handler v = fold_statements v

and fold_dual_handler (v: _ #folder) =
  handle v#fold_dual_handler
    ~continue:(fun { dual_handler_pos; dual_handler_neg } x -> x
                >> fold_handler v dual_handler_pos
                >> fold_handler v dual_handler_neg)

and fold_basic_arith_stmt (v: _ #folder) : basic_arithmetic_stmt -> 'a -> 'a =
  fun { basic_arith_operands; basic_arith_on_size_error } x -> x
    >> fold_basic_arithmetic_operands v basic_arith_operands
    >> fold_dual_handler v basic_arith_on_size_error

and fold_accept' (v: _ #folder) : accept_stmt with_loc -> 'a -> 'a =
  handle' v#fold_accept' v
    ~fold:(fun v stmt x -> match stmt with
        | AcceptGeneric i -> x
            >> fold_ident' v i
        | AcceptFromDevice { item; device_item } -> x
            >> fold_ident' v item
            >> P.fold_name' v device_item
        | AcceptTemporal { item; date_time } -> x
            >> fold_ident' v item
            >> fold_date_time v date_time
        | AcceptMisc { item; misc } -> x
            >> fold_ident' v item
            >> fold_accept_misc v misc
        | AcceptMsgCount n -> x
            >> P.fold_name' v n
        | AcceptScreen { item; clauses; on_exception } -> x
            >> fold_ident' v item
            >> fold_list ~fold:fold_accept_clause' v clauses
            >> fold_dual_handler v on_exception
        | AcceptFromEnv { item; env_item; on_exception } -> x
            >> fold_ident' v item
            >> fold_option ~fold:fold_ident_or_nonnum' v env_item
            >> fold_dual_handler v on_exception
        | AcceptFromArg { item; on_exception }
        | AcceptFromCmdLine { item; on_exception } -> x
            >> fold_ident' v item
            >> fold_dual_handler v on_exception)

and fold_accept_misc (v: _ #folder) : Cobol_ptree.accept_misc -> 'a -> 'a =
  leaf v#fold_accept_misc

and fold_accept_clause' (v: _ #folder) : accept_clause with_loc -> 'a -> 'a =
  handle' v#fold_accept_clause' v
    ~fold:(fun v clause x -> match clause with
        | AcceptAt p -> x
            >> fold_position v p
        | AcceptFromCRT
        | AcceptModeBlock -> x
        | AcceptWith clauses -> x
            >> fold_list ~fold:fold_accept_with_clause' v clauses)

and fold_accept_with_clause' (v: _ #folder) :
  accept_with_clause with_loc -> 'a -> 'a =
  handle' v#fold_accept_with_clause' v
    ~fold:(fun v clause x -> match clause with
        | AcceptAttribute a -> x
            >> fold_screen_attribute_clause v a
        | AcceptSize i -> x
            >> fold_ident_or_intlit v i
        | AcceptControl i -> x
            >> fold_ident v i
        | AcceptPromptCharacter i -> x
            >> fold_option ~fold:fold_ident_or_nonnum v i
        | AcceptAuto
        | AcceptFull
        | AcceptGrid
        | AcceptLeftLine
        | AcceptOverLine
        | AcceptRequired
        | AcceptSecure
        | AcceptTimeout
        | AcceptLeftJustify
        | AcceptRightJustify
        | AcceptSpaceFill
        | AcceptTrailingSign
        | AcceptUpdate
        | AcceptUpper
        | AcceptLower
        | AcceptZeroFill -> x)

and fold_add' (v: _ #folder) : basic_arithmetic_stmt with_loc -> 'a -> 'a =
  handle' v#fold_add' v ~fold:fold_basic_arith_stmt

and fold_call' (v: _ #folder) : call_stmt with_loc -> 'a -> 'a =
  handle' v#fold_call' v
    ~fold:(fun v { call_static; call_target; call_using; call_returning;
                   call_error_handler } x -> x
            >> fold_bool v call_static
            >> fold_call_target v call_target
            >> fold_list ~fold:fold_call_using_clause v call_using
            >> fold_ident'_opt v call_returning
            >> fold_option ~fold:fold_call_error_handler v call_error_handler)

and fold_call_error_handler (v: _ #folder) =
  handle v#fold_call_error_handler
    ~continue:(function
        | CallOnOverflow h -> fold_handler v h
        | CallOnException h -> fold_dual_handler v h)

and fold_compute' (v: _ #folder) : compute_stmt with_loc -> 'a -> 'a =
  handle' v#fold_compute' v
    ~fold:(fun v { compute_targets; compute_expr; compute_on_size_error } x -> x
            >> fold_rounded_idents v compute_targets
            >> fold_expr' v compute_expr
            >> fold_dual_handler v compute_on_size_error)

and fold_delete' (v: _ #folder) : delete_stmt with_loc -> 'a -> 'a =
  handle' v#fold_delete' v
    ~fold:(fun v { delete_targets; delete_retry; delete_on_invalid_key } x -> x
            >> P.fold_name' v delete_targets
            >> fold_option ~fold:fold_retry_clause v delete_retry
            >> fold_dual_handler v delete_on_invalid_key)

and fold_display' (v: _ #folder) : display_stmt with_loc -> 'a -> 'a =
  handle' v#fold_display' v
    ~fold:(fun v { display_items_clauses; no_advancing; on_exception } x -> x
            >> fold_list ~fold:fold_display_items_clauses v display_items_clauses
            >> fold_bool v no_advancing
            >> fold_dual_handler v on_exception)

and fold_display_items_clauses (v: _ #folder) =
  handle v#fold_display_items_clauses
    ~continue:(fun { display_items; display_clauses } x -> x
                >> fold_list ~fold:fold_ident_or_literal' v display_items
                >> fold_list ~fold:fold_display_clause' v display_clauses)

and fold_display_clause' (v: _ #folder) : display_clause with_loc -> 'a -> 'a =
  handle' v#fold_display_clause' v
    ~fold:(fun v clause x -> match clause with
        | DisplayAt p -> x
            >> fold_position v p
        | DisplayUpon t -> x
            >> fold_display_target' v t
        | DisplayModeIsBlock -> x
        | DisplayWith clauses -> x
            >> fold_list ~fold:fold_display_with_clause' v clauses)

and fold_display_target' (v: _ #folder) :
  Cobol_ptree.display_target with_loc -> 'a -> 'a =
  handle' v#fold_display_target' v
    ~fold:(fun v -> function
        | Cobol_ptree.DisplayUponName n -> P.fold_name' v n
        | DisplayUponDeviceViaMnemonic _ -> Fun.id)

and fold_display_with_clause' (v: _ #folder) :
  display_with_clause with_loc -> 'a -> 'a =
  handle' v#fold_display_with_clause' v
    ~fold:(fun v clause x -> match clause with
        | DisplayAttribute a -> x
            >> fold_screen_attribute_clause v a
        | DisplayBlank b -> x
            >> fold_blank_clause v b
        | DisplayErase e -> x
            >> fold_erase_clause v e
        | DisplaySize i -> x
            >> fold_ident_or_intlit v i
        | DisplayControl i -> x
            >> fold_ident v i
        | DisplayGrid
        | DisplayLeftLine
        | DisplayOverLine -> x)

and fold_divide' (v: _ #folder) : divide_stmt with_loc -> 'a -> 'a =
  handle' v#fold_divide' v
    ~fold:(fun v { divide_operands; divide_on_size_error } x -> x
            >> fold_divide_operands v divide_operands
            >> fold_dual_handler v divide_on_size_error)

and fold_evaluate' (v: _ #folder) : evaluate_stmt with_loc -> 'a -> 'a =
  handle' v#fold_evaluate' v
    ~fold:(fun v { eval_subjects; eval_branches; eval_otherwise } x -> x
            >> fold_list v eval_subjects
              ~fold:(fold_or_error' ~fold:fold_selection_subject')
            >> fold_list ~fold:fold_evaluate_branch v eval_branches
            >> fold_statements v eval_otherwise)

and fold_evaluate_branch (v: _ #folder) =
  handle v#fold_evaluate_branch
    ~continue:(fun { eval_selection; eval_actions } x -> x
                >> fold_list v eval_selection
                  ~fold:(fold_list ~fold:(fold_or_error' ~fold:fold_selection_object'))
                >> fold_statements v eval_actions)

and fold_if' (v: _ #folder) : if_stmt with_loc -> 'a -> 'a =
  handle' v#fold_if' v
    ~fold:(fun v { condition; then_branch; else_branch } x -> x
            >> fold_condition_or_error' v condition
            >> fold_statements v then_branch
            >> fold_statements v else_branch)

and fold_multiply' (v: _ #folder) : multiply_stmt with_loc -> 'a -> 'a =
  handle' v#fold_multiply' v
    ~fold:(fun v { multiply_operands; multiply_on_size_error } x -> x
            >> fold_multiply_operands v multiply_operands
            >> fold_dual_handler v multiply_on_size_error)

and fold_perform_inline' (v: _ #folder) :
  perform_inline_stmt with_loc -> 'a -> 'a =
  handle' v#fold_perform_inline' v
    ~fold:(fun v { perform_inline_mode; perform_statements } x -> x
            >> fold_option ~fold:fold_perform_mode v perform_inline_mode
            >> fold_statements v perform_statements)

and fold_perform_target' (v: _ #folder) :
  perform_target_stmt with_loc -> 'a -> 'a =
  handle' v#fold_perform_target' v
    ~fold:(fun v { perform_target; perform_mode } x -> x
            >> fold_procedure_range v perform_target
            >> fold_option ~fold:fold_perform_mode v perform_mode)

and fold_read_error_handler (v: _ #folder) =
  handle v#fold_read_error_handler
    ~continue:(fun (read_error, dual_handler) x -> x
                >> fold_read_error v read_error
                >> fold_dual_handler v dual_handler)

and fold_read' (v: _ #folder) : read_stmt with_loc -> 'a -> 'a =
  handle' v#fold_read' v
    ~fold:(fun v { read_file; read_direction; read_into; read_lock_behavior;
                   read_lock; read_key; read_error_handler } x -> x
            >> P.fold_name' v read_file
            >> fold_option ~fold:fold_read_direction v read_direction
            >> fold_option ~fold:fold_ident v read_into
            >> fold_option ~fold:fold_read_lock_behavior v read_lock_behavior
            >> fold_option ~fold:fold_bool v read_lock
            >> fold_option ~fold:fold_data_ref v read_key
            >> fold_option ~fold:fold_read_error_handler v read_error_handler)

and fold_receive' (v: _ #folder) : receive_stmt with_loc -> 'a -> 'a =
  handle' v#fold_receive' v
    ~fold:(fun v { receive_name; receive_kind; receive_into;
                   receive_on_no_data } x -> x
            >> P.fold_name' v receive_name
            >> fold_mcs_awaiting_item v receive_kind
            >> fold_ident v receive_into
            >> fold_dual_handler v receive_on_no_data)

and fold_return' (v: _ #folder) : return_stmt with_loc -> 'a -> 'a =
  handle' v#fold_return' v
    ~fold:(fun v { return_file; return_into; return_at_end } x -> x
            >> P.fold_name' v return_file
            >> fold_ident'_opt v return_into
            >> fold_dual_handler v return_at_end)

and fold_rewrite' (v: _ #folder) : rewrite_stmt with_loc -> 'a -> 'a =
  handle' v#fold_rewrite' v
    ~fold:(fun v { rewrite_to; rewrite_from; rewrite_retry; rewrite_lock;
                   rewrite_invalid_key_handler } x -> x
            >> fold_write_target v rewrite_to
            >> fold_option ~fold:fold_ident_or_literal v rewrite_from
            >> fold_option ~fold:fold_retry_clause v rewrite_retry
            >> fold_option ~fold:fold_bool v rewrite_lock
            >> fold_dual_handler v rewrite_invalid_key_handler)

and fold_search_when_clause' (v: _ #folder) :
  search_when_clause with_loc -> 'a -> 'a =
  handle' v#fold_search_when_clause' v
    ~fold:(fun v { search_when_cond; search_when_stmts } x -> x
            >> fold_condition_or_error' v search_when_cond
            >> fold_statements v search_when_stmts)

and fold_search' (v: _ #folder) : search_stmt with_loc -> 'a -> 'a =
  handle' v#fold_search' v
    ~fold:(fun v { search_item; search_varying; search_at_end;
                   search_when_clauses } x -> x
            >> fold_data_ref v search_item
            >> fold_handler v search_at_end
            >> fold_option ~fold:fold_ident v search_varying
            >> fold_list ~fold:fold_search_when_clause' v search_when_clauses)

and fold_search_all' (v: _ #folder) : search_all_stmt with_loc -> 'a -> 'a =
  handle' v#fold_search_all' v
    ~fold:(fun v { search_all_item; search_all_at_end; search_all_conditions;
                   search_all_action } x -> x
            >> fold_data_ref v search_all_item
            >> fold_handler v search_all_at_end
            >> fold_list ~fold:fold_search_condition v search_all_conditions
            >> fold_statements v search_all_action)

and fold_start' (v: _ #folder) : start_stmt with_loc -> 'a -> 'a =
  handle' v#fold_start' v
    ~fold:(fun v { start_file; start_position; start_on_invalid_key } x -> x
            >> P.fold_name' v start_file
            >> fold_option ~fold:fold_start_position v start_position
            >> fold_dual_handler v start_on_invalid_key)

and fold_string_stmt' (v: _ #folder) : string_stmt with_loc -> 'a -> 'a =
  handle' v#fold_string_stmt' v
    ~fold:(fun v { string_sources; string_target; string_pointer;
                   string_on_overflow } x -> x
            >> fold_list ~fold:fold_string_source v string_sources
            >> fold_ident v string_target
            >> fold_option ~fold:fold_ident v string_pointer
            >> fold_dual_handler v string_on_overflow)

and fold_subtract' (v: _ #folder) :
  basic_arithmetic_stmt with_loc -> 'a -> 'a =
  handle' v#fold_subtract' v ~fold:fold_basic_arith_stmt

and fold_unstring' (v: _ #folder) : unstring_stmt with_loc -> 'a -> 'a =
  handle' v#fold_unstring' v
    ~fold:(fun v { unstring_source; unstring_delimiters; unstring_targets;
                   unstring_pointer; unstring_tallying;
                   unstring_on_overflow } x -> x
            >> fold_ident v unstring_source
            >> fold_list ~fold:fold_unstring_delimiter v unstring_delimiters
            >> fold_list ~fold:fold_unstring_target v unstring_targets
            >> fold_option ~fold:fold_ident v unstring_pointer
            >> fold_option ~fold:fold_ident v unstring_tallying
            >> fold_dual_handler v unstring_on_overflow)

and fold_write_error_handler (v: _ #folder) =
  handle v#fold_write_error_handler
    ~continue:(fun (write_error, dual_handler) x -> x
                >> fold_write_error v write_error
                >> fold_dual_handler v dual_handler)

and fold_write' (v: _ #folder) : write_stmt with_loc -> 'a -> 'a =
  handle' v#fold_write' v
    ~fold:(fun v { write_to; write_from; write_advancing; write_retry;
                   write_lock; write_error_handler } x -> x
            >> fold_write_target v write_to
            >> fold_option ~fold:fold_ident_or_literal v write_from
            >> fold_option ~fold:fold_advancing_phrase v write_advancing
            >> fold_option ~fold:fold_retry_clause v write_retry
            >> fold_option ~fold:fold_bool v write_lock
            >> fold_option ~fold:fold_write_error_handler v write_error_handler)
