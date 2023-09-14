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

open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Terms_visitor
open Ast

let todo    x = Cobol_common.Visitor.todo    __FILE__ __MODULE__ x
let partial x = Cobol_common.Visitor.partial __FILE__ __MODULE__ x

(* --- *)

class ['a] folder = object
  inherit ['a] Terms_visitor.folder
  method fold_basic_arithmetic_operands: (basic_arithmetic_operands , 'a) fold = default
  method fold_call_using_clause        : (call_using_clause         , 'a) fold = default
  method fold_call_using_clause'       : (call_using_clause with_loc, 'a) fold = default
  method fold_call_using_by            : (call_using_by             , 'a) fold = default
  method fold_date_time                : (date_time                 , 'a) fold = default
  method fold_divide_operands          : (divide_operands           , 'a) fold = default
  method fold_file_option              : (file_option               , 'a) fold = default
  method fold_multiply_operands        : (multiply_operands         , 'a) fold = default
  method fold_open_mode                : (open_mode                 , 'a) fold = default
  method fold_position                 : (position                  , 'a) fold = default
  method fold_raising                  : (raising                   , 'a) fold = default
  method fold_read_direction           : (read_direction            , 'a) fold = default
  method fold_read_lock_behavior       : (read_lock_behavior        , 'a) fold = default
  method fold_retry_clause             : (retry_clause              , 'a) fold = default
  method fold_search_condition         : (search_condition          , 'a) fold = default
  method fold_sharing_mode             : (sharing_mode              , 'a) fold = default
  method fold_stage                    : (stage                     , 'a) fold = default
  method fold_advancing_phrase         : (advancing_phrase          , 'a) fold = default
  method fold_write_target             : (write_target              , 'a) fold = default
  method fold_procedure_range: 'x. ('x procedure_range, 'a) fold = default
  (* SET *)
  method fold_set_attribute_switch     : (set_attribute_switch      , 'a) fold = default
  method fold_screen_attribute         : (screen_attribute          , 'a) fold = default
  method fold_set_ambiguous_method     : (set_ambiguous_method      , 'a) fold = default
  method fold_on_off                   : (on_off                    , 'a) fold = default
  method fold_locale_category          : (locale_category           , 'a) fold = default
  method fold_set_save_locale          : (set_save_locale           , 'a) fold = default
  method fold_set_locale_target        : (set_locale_target         , 'a) fold = default
  method fold_set_locale_source        : (set_locale_source         , 'a) fold = default
  method fold_start_position           : (start_position            , 'a) fold = default
  method fold_float_content            : (float_content             , 'a) fold = default

end

let fold_basic_arithmetic_operands (v: _ #folder) =
  handle v#fold_basic_arithmetic_operands
    ~continue:begin fun o x -> match o with
      | ArithSimple { sources; targets } -> x
          >> fold_list ~fold:fold_ident_or_numlit v sources
          >> fold_rounded_idents v targets
      | ArithGiving { sources; to_or_from_item; targets } -> x
          >> fold_list ~fold:fold_ident_or_numlit v sources
          >> fold_ident_or_numlit v to_or_from_item
          >> fold_rounded_idents v targets
      | ArithCorresponding { source; target } -> x
          >> fold_qualname v source
          >> fold_rounded_ident v target
    end

let fold_call_using_by (v: _ #folder) =
  leaf v#fold_call_using_by

let fold_call_using_clause (v: _ #folder) =
  handle v#fold_call_using_clause
    ~continue:begin fun { call_using_by; call_using_expr } x -> x
      >> fold_option ~fold:fold_call_using_by v call_using_by
      >> fold_option ~fold:fold_expr v call_using_expr
    end

let fold_call_using_clause' (v: _ #folder) =
  handle' v#fold_call_using_clause' v ~fold:fold_call_using_clause

let fold_date_time (v: _ #folder) =
  leaf v#fold_date_time       (* NB: only `bool` children: consider as a leaf *)

let fold_divide_operands (v: _ #folder) =
  handle v#fold_divide_operands
    ~continue:begin fun o x -> match o with
      | DivideInto { divisor; dividends } -> x
          >> fold_ident_or_numlit v divisor
          >> fold_rounded_idents v dividends
      | DivideGiving { divisor; dividend; giving; into; remainder } -> x
          >> fold_ident_or_numlit v divisor
          >> fold_ident_or_numlit v dividend
          >> fold_rounded_idents v giving
          >> fold_bool v into
          >> fold_option ~fold:fold_ident v remainder
    end

let fold_file_option (v: _ #folder) =
  leaf v#fold_file_option

let fold_multiply_operands (v: _ #folder) =
  handle v#fold_multiply_operands
    ~continue:begin fun o x -> match o with
      | MultiplyBy { multiplier; multiplicand } -> x
          >> fold_ident_or_numlit v multiplier
          >> fold_rounded_idents v multiplicand
      | MultiplyGiving { multiplier; multiplicand; targets } -> x
          >> fold_ident_or_numlit v multiplier
          >> fold_ident_or_numlit v multiplicand
          >> fold_rounded_idents v targets
    end

let fold_open_mode (v: _ #folder) =
  leaf v#fold_open_mode

let fold_position (v: _ #folder) =
  handle v#fold_position
    ~continue:begin fun p x -> match p with
      | LinePosition i
      | ColumnPosition i -> x
          >> fold_ident_or_intlit v i
      | LineColumnPosition (i, j) -> x
          >> fold_ident_or_intlit v i
          >> fold_ident_or_intlit v j
    end

let fold_raising (v: _ #folder) =
  handle v#fold_raising
    ~continue:begin function
      | RaisingIdent i -> fold_ident v i
      | RaisingException n -> fold_name' v n
      | RaisingLastException -> Fun.id
    end

let fold_read_direction (v: _ #folder) =
  leaf v#fold_read_direction

let fold_retry_clause (v: _ #folder) =
  handle v#fold_retry_clause
    ~continue:begin function
      | RetryNTimes e
      | RetryForNSeconds e -> fold_expr v e
      | RetryForever -> Fun.id
    end

let fold_read_lock_behavior (v: _ #folder) =
  handle v#fold_read_lock_behavior
    ~continue:begin function
      | ReadAdvancingOnLock
      | ReadIgnoringLock -> Fun.id
      | ReadRetry retry_clause -> fold_retry_clause v retry_clause
    end

let fold_search_condition (v: _ #folder) =
  handle v#fold_search_condition
    ~continue:begin fun cond x -> match cond with
      | IsEqual { data_item; condition } -> x
          >> fold_qualident v data_item
          >> fold_expression v condition
      | Cond ql -> x
          >> fold_qualident v ql
    end

let fold_sharing_mode (v: _ #folder) =
  leaf v#fold_sharing_mode

let fold_stage (v: _ #folder) =
  leaf v#fold_stage

let fold_advancing_phrase (v: _ #folder) =
  handle v#fold_advancing_phrase
    ~continue:begin fun phrase x -> match phrase with
      | AdvancingLines { stage; lines; ambiguous } -> x
          >> fold_stage v stage
          >> fold_ident_or_intlit v lines
          >> fold_bool v ambiguous
      | AdvancingPage { stage } -> x
          >> fold_stage v stage
    end

let fold_start_position (v: _#folder) =
  handle v#fold_start_position
    ~continue:begin fun pos x -> match pos with
      | StartPositionFirst
      | StartPositionLast -> x
      | StartPositionKey { operator; name; length } -> x
          >> fold_relop v operator
          >> fold_qualname v name
          >> fold_option ~fold:fold_expression v length
    end

let fold_write_target (v: _ #folder) =
  handle v#fold_write_target
    ~continue:begin function
      | WriteTargetName qn -> fold_qualname v qn
      | WriteTargetFile name' -> fold_name' v name'
    end


(* --- *)

let fold_procedure_range (v: _ #folder) ~fold =
  handle v#fold_procedure_range
    ~continue:begin fun { procedure_start; procedure_end } x -> x
      >> fold v procedure_start
      >> fold_option ~fold v procedure_end
    end

(* SET *)

let fold_screen_attribute (v: _ #folder) =
  leaf v#fold_screen_attribute

let fold_set_ambiguous_method (v: _ #folder) =
  leaf v#fold_set_ambiguous_method

let fold_on_off (v: _ #folder) =
  leaf v#fold_on_off

let fold_locale_category (v: _ #folder) =
  leaf v#fold_locale_category

let fold_set_save_locale (v: _ #folder) =
  leaf v#fold_set_save_locale

let fold_set_attribute_switch (v: _ #folder) =
  handle v#fold_set_attribute_switch
    ~continue:begin fun { set_attribute; set_attribute_switch_value } x -> x
      >> fold_screen_attribute v set_attribute
      >> fold_on_off v set_attribute_switch_value
    end

let fold_set_locale_target (v: _ #folder) =
  handle v#fold_set_locale_target
    ~continue:begin function
      | SetLocaleTarget locale_category ->
          fold_locale_category v locale_category
      | SetLocaleTargetUserDefault -> Fun.id
    end

let fold_set_locale_source (v: _ #folder) =
  handle v#fold_set_locale_source
    ~continue:begin function
      | SetLocaleSource id -> fold_ident v id
      | SetLocaleSourceUserDefault
      | SetLocaleSourceSystemDefault -> Fun.id
    end

let fold_float_content (v: _ #folder) =
  handle v#fold_float_content
    ~continue:begin function
      | FarthestFromZero b
      | NearestToZero b -> fold_bool v b
      | FloatInfinity
      | FloatNotANumber
      | FloatNotANumberSignaling -> Fun.id
    end
