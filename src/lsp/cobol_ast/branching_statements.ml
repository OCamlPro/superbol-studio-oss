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

(** Potentially branching statements *)

open Terms
open Operands
open Simple_statements


(* GOTO DEPENDING *)
type goto_depending_stmt =
  {
    goto_depending_targets: qualname list;                      (* procedures *)
    goto_depending_on: ident;
  }
[@@deriving show, ord]


(* Error/exception handlers *)
type handler =
  statements
[@@deriving show, ord]

and dual_handler =
  {
    dual_handler_pos: handler;                              (** positive case *)
    dual_handler_neg: handler;                              (** negative case *)
  }
[@@deriving show, ord]


(* EVAL *)
and evaluate_stmt =
  {
    eval_subjects: selection_subject list;
    eval_branches: evaluate_branch list;
    eval_otherwise: statements;
  }
[@@deriving show, ord]

and evaluate_branch =
  {
    eval_selection: selection_object list list;
    eval_actions: statements;
  }
[@@deriving show, ord]


(* PERFORM *)
and perform_stmt =
  {
    perform_target: perform_target;
    perform_mode: perform_mode option;
  }
[@@deriving show, ord]

and perform_target =
  | PerformOutOfLine of qualname procedure_range
  | PerformInline of statements
[@@deriving show, ord]

and perform_mode =
  | PerformNTimes of ident_or_intlit
  | PerformUntil of
      {
        with_test: stage option;
        until: condition;
      }
  | PerformVarying of
      {
        with_test: stage option;
        varying: varying_phrase with_loc;
        after: varying_phrase with_loc list;
      }
[@@deriving show, ord]

and varying_phrase =
  {
    varying_ident: ident;
    varying_from: ident_or_numlit;
    varying_by: ident_or_numlit option;
    varying_until: condition;
  }
[@@deriving show, ord]


(* SEARCH *)
and search_stmt =
  {
    search_item: qualname;
    search_at_end: handler;
    search_spec: search_spec;
  }
[@@deriving show, ord]

and search_spec =
  | SearchSerial of
      {
        varying: ident option;
        when_clauses: search_when_clause with_loc list;
      }
  | SearchAll of
      {
        conditions: search_condition list;
        action: branch;
      }
[@@deriving show, ord]

and search_when_clause =
  {
    search_when_cond: condition;
    search_when_stmts: branch;
  }
[@@deriving show, ord]


(* IF *)
and if_stmt =
  {
    condition: condition;
    then_branch: branch;
    else_branch: branch option;
  }
[@@deriving show, ord]


(* ACCEPT *)
and accept_stmt =
  | AcceptGeneric of ident with_loc
  | AcceptFromDevice of
      {
        item: ident with_loc;
        device_item: name with_loc;
      }
  | AcceptTemporal of
      {
        item: ident with_loc;
        date_time: date_time;
      }
  | AcceptMsgCount of name with_loc
  | AcceptAtScreen of
      {
        item: name with_loc;
        position: position option;
        on_exception: dual_handler;
      }
  | AcceptFromEnv of                                                    (* MF *)
      {
        item: ident with_loc;
        env_item: ident_or_nonnum with_loc;
        on_exception: dual_handler;
      }
[@@deriving show, ord]


(*
DISPLAY id/lit+ UPON...? WITH...? END_DISP
DISPLAY id AT...? ON EXCEPT?
*)

and display_stmt =
  | DisplayDefault of ident_or_literal
  | DisplayDevice of
      {
        displayed_items: ident_or_literal list; (* non-empty *)
        upon: display_target with_loc option;
        advancing: bool;
    }
  | DisplayScreen of
      {
        screen_item: name with_loc;
        position: position option;
        on_exception: dual_handler;
      }
[@@deriving show, ord]


and display_target =
  | DisplayUponName of name with_loc
  | DisplayUponDeviceViaMnemonic of display_device_mnemonic with_loc

and display_device_mnemonic =
 | DisplayDeviceEnvName
 | DisplayDeviceEnvValue
 | DisplayDeviceArgNumber
 | DisplayDeviceCommandLine


(* ADD & SUBTRACT *)
and basic_arithmetic_stmt =
  {
    basic_arith_operands: basic_arithmetic_operands;
    basic_arith_on_size_error: dual_handler;
  }
[@@deriving show, ord]


(* COMPUTE *)
and compute_stmt =
  {
    compute_targets: rounded_idents;
    compute_expr: expression;
    compute_on_size_error: dual_handler;
  }
[@@deriving show, ord]


(* DELETE *)
and delete_stmt =
  {
    delete_targets: name with_loc;
    delete_retry: retry_clause option;
    delete_on_invalid_key: dual_handler;
  }
[@@deriving show, ord]


(* DIVIDE *)
and divide_stmt =
  {
    divide_operands: divide_operands;
    divide_on_size_error: dual_handler;
  }
[@@deriving show, ord]


(* MULTIPLY *)
and multiply_stmt =
  {
    multiply_operands: multiply_operands;
    multiply_on_size_error: dual_handler;
  }
[@@deriving show, ord]


(* RECEIVE *)
and receive_stmt =
  {
    receive_name: name with_loc;                                        (* CD *)
    receive_kind: mcs_awaiting_item;
    receive_into: ident;
    receive_on_no_data: dual_handler;
  }
[@@deriving show, ord]


(* RETURN *)
and return_stmt =
  {
    return_file: name with_loc;
    return_into: ident with_loc option;
    return_at_end: dual_handler;
  }
[@@deriving show, ord]


(* REWRITE *)
and rewrite_stmt =
  {
    rewrite_to: write_target;
    rewrite_from: ident_or_literal option;
    rewrite_retry: retry_clause option;
    rewrite_lock: bool option;
    rewrite_invalid_key_handler: dual_handler;
  }
[@@deriving show, ord]


(* START *)
and start_stmt =
  {
    start_file: name with_loc;
    start_position: start_position option;
    start_on_invalid_key: dual_handler;
  }
[@@deriving show, ord]


(* STRING *)
and string_stmt =
  {
    string_sources: string_source list;
    string_target: ident;
    string_pointer: ident option;
    string_on_overflow: dual_handler;
  }
[@@deriving show, ord]


(* UNSTRING *)
and unstring_stmt =
  {
    unstring_source: ident;
    unstring_delimiters: unstring_delimiter list;
    unstring_targets: unstring_target list;
    unstring_pointer: ident option;
    unstring_tallying: ident option;
    unstring_on_overflow: dual_handler;
  }
[@@deriving show, ord]


(* WRITE *)
and write_stmt =
  {
    write_to: write_target;
    write_from: ident_or_literal option;
    write_advancing: advancing_phrase option;
    write_retry: retry_clause option;
    write_lock: bool option;
    write_error_handler: (write_error * dual_handler) option;
  }
[@@deriving show, ord]

and write_error =
  | WriteAtEndOfPage
  | WriteInvalidKey
[@@deriving show, ord]



(*
CALL id/lit USING...? RETURNING...? ON OVERFLOW (on-overflow - archaic)
CALL id/lit USING...? RETURNING...? ON EXCEPT (on-exception)

CALL (id/lit AS)? NESTED/id USING...? RETURNING...? (program-prototype)
*)

(* CALL *)
and call_stmt =
  {
    call_prefix: call_prefix;
    call_using: call_using_clause with_loc list;
    call_returning: ident with_loc option;
    call_error_handler: call_error_handler option;
  }
[@@deriving show, ord]

and call_prefix =
  | CallGeneral of ident_or_strlit
  | CallProto of
      {
        called: ident_or_strlit option;
        prototype: call_proto;
      }
[@@deriving show, ord]

and call_proto =
  | CallProtoIdent of ident
  | CallProtoNested
[@@deriving show, ord]

and call_error_handler =
  | CallOnOverflow of handler
  | CallOnException of dual_handler
[@@deriving show, ord]


(* READ *)
and read_stmt =
  {
    read_file: name with_loc;
    read_direction: read_direction option;
    read_into: ident option;
    read_lock_behavior: read_lock_behavior option;
    read_lock: bool option;
    read_key: qualname option;
    read_error_handler: (read_error * dual_handler) option;
  }
[@@deriving show, ord]

and read_error =
  | ReadAtEnd
  | ReadInvalidKey
[@@deriving show, ord]



and statement =
  (* TODO: split composed high-level (that depend on statement), and basic
     statements *)
  (* TODO: term-like unification (long-term) *)
  | Accept of accept_stmt
  | Add of basic_arithmetic_stmt
  | Allocate of allocate_stmt
  | Alter of alter_stmt
  | Call of call_stmt
  | Cancel of ident_or_strlit list (* non-empty *)
  | Close of close_stmt
  | Compute of compute_stmt
  | Continue
  | Delete of delete_stmt
  | Disable of mcs_command_operands
  | Display of display_stmt
  | Divide of divide_stmt
  | Enable of mcs_command_operands
  | Enter of enter_stmt
  | Evaluate of evaluate_stmt
  | Exit of exit_stmt
  | Free of name with_loc list
  | Generate of name with_loc
  | GoTo of qualname
  | GoToDepending of goto_depending_stmt
  | GoBack of raising option
  | If of if_stmt
  | Initialize of initialize_stmt
  | Initiate of name with_loc list
  | Inspect of inspect_stmt
  | Invoke of invoke_stmt
  | LoneGoTo                                               (* COB85, obsolete *)
  | Merge of merge_stmt
  | Move of move_stmt
  | Multiply of multiply_stmt
  | Open of open_stmt
  | Perform of perform_stmt
  | Purge of name with_loc
  | Raise of raise_operand
  | Read of read_stmt
  | Receive of receive_stmt
  | Release of release_stmt
  | Resume of qualname
  | ResumeNextStatement
  | Return of return_stmt
  | Rewrite of rewrite_stmt
  | Search of search_stmt
  | Send of send_stmt
  | Set of set_stmt
  | Sort of sort_stmt
  | Start of start_stmt
  | Stop of stop_stmt
  | String of string_stmt
  | Subtract of basic_arithmetic_stmt
  | Suppress
  | Terminate of terminate_stmt
  | Transform of transform_stmt
  | Unlock of unlock_stmt
  | Unstring of unstring_stmt
  | Validate of ident list
  | Write of write_stmt
[@@deriving show, ord]

and statements = statement with_loc list [@@deriving show, ord]

and branch =
  | Statements of statements
  | NextSentence
[@@deriving show, ord]
