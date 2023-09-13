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

(** Non-branching statements *)

open Terms
open Operands


(*
ALLOCATE exp CHARACTERS INIT? (RET id)?
ALLOCATE id INIT? (RET id)?
*)

(* ALLOCATE *)
type allocate_stmt =
  {
    allocate_kind: allocate_kind;
    allocate_initialized: bool;
    allocate_returning: ident with_loc option;
  }
[@@deriving show, ord]

and allocate_kind =
  | AllocateCharacters of expression (*arith exp that evaluates to int (may be rounded)*)
  | AllocateDataItem of name with_loc
[@@deriving show, ord]


(*
ALTER id TO PROCEED? id
*)

(* ALTER *)
type alter_stmt =
  alter_operands with_loc list
[@@deriving show, ord]

and alter_operands =
  {
    alter_source: qualname;
    alter_target: qualname;
  }
[@@deriving show, ord]


(* CLOSE *)
type close_stmt = close_phrase list [@@deriving show, ord]
and close_phrase =
  {
    close_item: name with_loc;
    close_format: close_format option;
  }
[@@deriving show, ord]

and close_format =
  | CloseUnitReel of bool (* for removal *)
  | CloseWithLock
  | CloseWithNoRewind
[@@deriving show, ord]


(* ENTER *)
type enter_stmt =
  {
    enter_language: name with_loc;
    enter_routine: name with_loc option;
  }
[@@deriving show, ord]


(* EXIT *)
type exit_stmt =
  | ExitSimple
  | ExitProgram of raising option
  | ExitMethod of raising option
  | ExitFunction of raising option
  | ExitPerform of bool
  | ExitParagraph
  | ExitSection
[@@deriving show, ord]


(* INITIALIZE *)
type initialize_stmt =
  {
    init_items: ident list;
    init_filler: bool;
    init_category: init_category option;
    init_replacings: init_replacing list;
    init_to_default: bool;
  }
[@@deriving show, ord]

and init_category =
  | InitAll
  | InitCategory of init_data_category
[@@deriving show, ord]

and init_replacing =
  {
    init_replacing_category: init_data_category;
    init_replacing_replacement_item: ident_or_literal;
  }
[@@deriving show, ord]

and init_data_category =
  | InitCategoryAlphabetic
  | InitCategoryAlphanumeric
  | InitCategoryAlphanumericEdited
  | InitCategoryBoolean
  | InitCategoryDataPointer
  | InitCategoryFunctionPointer
  | InitCategoryNational
  | InitCategoryNationalEdited
  | InitCategoryNumeric
  | InitCategoryNumericEdited
  | InitCategoryObjectReference
  | InitCategoryProgramPointer
[@@deriving show, ord]


(* INVOKE *)
type invoke_stmt =
  {
    invoke_target: ident;
    invoke_method: ident_or_strlit;
    invoke_using: call_using_clause with_loc list;
    invoke_returning: ident with_loc option;
    (* error_htypeler: error_htypeler;  (* seen on IBM Cobol *)  *)
  }
[@@deriving show, ord]


(* INSPECT *)
type inspect_stmt =
  {
    inspect_item: ident;
    inspect_spec: inspect_spec;
  }
[@@deriving show, ord]

and inspect_spec =
  | InspectTallying of tallying list
  | InspectReplacing of replacing list
  | InspectBoth of tallying list * replacing list
  | InspectConverting of converting
[@@deriving show, ord]

and tallying =
  {
    tallying_target: qualident;
    tallying_clauses: tallying_clause with_loc list;
  }
[@@deriving show, ord]

and tallying_clause =
  | TallyingCharacters of inspect_where list
  | TallyingRange of tallying_range * tallying_spec list
[@@deriving show, ord]

and tallying_range =
  | TallyAll
  | TallyLeading
[@@deriving show, ord]

and tallying_spec =
  {
    tallying_item: ident_or_nonnum;
    tallying_where: inspect_where list;
  }
[@@deriving show, ord]

and replacing = replacing_clause with_loc
[@@deriving show ,ord]

and replacing_clause =
  | ReplacingCharacters of
      {
        replacement: ident_or_nonnum;
        where: inspect_where list;
      }
  | ReplacingRange of replacing_range * replacing_range_spec list
[@@deriving show, ord]

and replacing_range =
  | ReplaceAll
  | ReplaceLeading
  | ReplaceFirst
[@@deriving show, ord]

and replacing_range_spec =
  {
    replacing_item: ident_or_nonnum;
    replacing_by: ident_or_nonnum;
    replacing_where: inspect_where list;
  }
[@@deriving show, ord]

and converting =
  {
    converting_from: ident_or_nonnum;
    converting_to: ident_or_nonnum;
    converting_where: inspect_where list;
  }
[@@deriving show, ord]

and inspect_where = inspect_direction * inspect_reference
[@@deriving show, ord]

and inspect_direction =
  | InspectAfter
  | InspectBefore
[@@deriving show, ord]

and inspect_reference = ident_or_nonnum
[@@deriving show, ord]


(* MERGE *)
type merge_stmt =
  {
    merge_file: name with_loc;
    merge_keys: Data_descr.sort_spec list;
    merge_collating: Misc_descr.alphabet_specification option;
    merge_using: name with_loc list;
    merge_target: merge_or_sort_target;
  }
[@@deriving show, ord]

and merge_or_sort_target =
  | OutputProcedure of name with_loc procedure_range
  | Giving of name with_loc list
[@@deriving show, ord]


(* MOVE *)
type move_stmt =              (* TODO: maybe split in two distinct statements *)
  | MoveSimple of
      {
        from: ident_or_literal;
        to_: ident list;
      }
  | MoveCorresponding of
      {
        from: ident;
        to_: ident list;
      }
[@@deriving show, ord]


(* OPEN *)
type open_stmt = open_phrase list [@@deriving show, ord]
and open_phrase =
  {
    open_mode: open_mode;
    open_sharing: sharing_mode option;
    open_retry: retry_clause option;
    open_files: named_file_option list;
  }
[@@deriving show, ord]

and named_file_option =
  {
    named_file_name: name with_loc;
    named_file_option: file_option option;
  }
[@@deriving show, ord]


(* RELEASE *)
type release_stmt =
  {
    release_item: name with_loc;
    release_from: ident_or_literal option;
  }
[@@deriving show, ord]


(* SEND *)
type send_stmt =
  {
    send_name: name with_loc;
    send_operands: send_operands;
  }
[@@deriving show, ord]

and send_operands =
  | SendSimple of
      {
        from: ident;
      }
  | SendWith of
      {
        from: ident option;
        ending_indicator: message_ending_indicator;
        advancing: advancing_phrase option;
        replace: bool;
      }
[@@deriving show, ord]

and message_ending_indicator =
  | EndingIndicator of ident
  | EndingIndicatorESI
  | EndingIndicatorEMI
  | EndingIndicatorEGI
[@@deriving show, ord]


(* SET *)

  (*
 SET ADDR/id... UPBY/DOWNBY/TO id/exp/int

 SET (id... TO ON/OFF)...
 SET (id... TO TRUE/FALSE)...

 SET id ATTRIBUTE
 SET id TO LOCALE

 SET LOCALE
 SET LAST EXCEPTION
 SET CONTENT
 *)
type set_stmt =
  | SetAmbiguous of
      {
        targets: ident list;
        set_method: set_ambiguous_method;
        value: expression;
      }
  | SetSwitch of
      set_switch_spec list
  | SetCondition of
      set_condition_spec list
  | SetAttribute of
      {
        name: name with_loc;
        attribute_switches: set_attribute_switch list;
      }
  | SetSaveLocale of
      {
        target: ident;
        locale: set_save_locale;
      }
  | SetLocale of
      {
        target: set_locale_target;
        source: set_locale_source;
      }
  | SetSavedException
  | SetFloatContent of
      {
        targets: ident list;
        content: float_content;
        sign: sign option;
      }
[@@deriving show, ord]

and set_switch_spec =
  {
    set_switch_targets: ident list;
    set_switch_value: on_off;
  }
[@@deriving show, ord]

and set_condition_spec =
  {
    set_condition_targets: ident list;
    set_condition_value: bool;
  }
[@@deriving show, ord]



(* SORT *)
type sort_stmt =
  | SortFile of
      {
        file: qualident;
        keys: Data_descr.sort_spec list;                         (* Not empty *)
        duplicate_in_order: bool;
        collating: Misc_descr.alphabet_specification option;
        source: sort_source;
        target: merge_or_sort_target;
      }
  | SortTable of
      {
        table: qualident;
        keys: Data_descr.sort_spec list;                      (* Can be empty *)
        duplicate_in_order: bool;
        collating: Misc_descr.alphabet_specification option;
      }
[@@deriving show, ord]

and sort_source =                                                (* SORT only *)
  | SortInputProcedure of name with_loc procedure_range
  | SortUsing of name with_loc list
[@@deriving show, ord]


(* STOP *)
type stop_stmt =
  | StopRun of stop_run option
  | StopLiteral of literal
[@@deriving show, ord]

and stop_run =
  {
    stop_kind: stop_kind;
    stop_status: ident_or_literal;
  }
[@@deriving show, ord]

and stop_kind =
  | StopRunError
  | StopRunNormal
[@@deriving show, ord]


type terminate_stmt =
  name with_loc list
[@@deriving show, ord]


(* TRANSFORM *)

type transform_stmt =
  {
    transform_ident: ident with_loc;
    transform_from: ident_or_nonnum with_loc;
    transform_to: ident_or_nonnum with_loc;
  }
[@@deriving show, ord]


(* UNLOCK *)
type unlock_stmt =
  {
    unlock_file: name with_loc;
    unlock_record: bool;
  }
[@@deriving show, ord]
