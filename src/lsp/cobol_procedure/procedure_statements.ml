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

(** Typed statements: mirror of {!Cobol_ptree} statements. As for operands,
    statement parts that never refer to data items, expressions, conditions, or
    statements use their parse tree definition directly. *)

open Cobol_common.Srcloc.TYPES
open Procedure_terms
open Procedure_operands

type screen_attribute_clause =
  | Bell
  | Blink
  | Highlight
  | Lowlight
  | ReverseVideo
  | Underline
  | ForegroundColor of ident_or_intlit
  | BackgroundColor of ident_or_intlit
[@@deriving ord]

type sort_spec = {
  sort_key_direction: Cobol_ptree.sort_direction;
  sort_key_names: data_ref list;
}
[@@deriving ord]

(* ALLOCATE *)
type allocate_kind =
  | AllocateCharacters of expr with_loc
  | AllocateDataItem of data_ref
[@@deriving ord]

type allocate_stmt = {
  allocate_kind: allocate_kind;
  allocate_initialized: bool;
  allocate_returning: ident with_loc option;
}
[@@deriving ord]

(* ENTRY *)
type entry_by_clause =
  | EntryByReference of data_name_ref list
  | EntryByValue of data_name_ref list
[@@deriving ord]

type entry_stmt =
  | EntrySimple of string with_loc
  | EntryUsing of {
      entry_name: string with_loc;
      entry_by_clauses: entry_by_clause list;
    }
  | EntryForGoTo of string with_loc
[@@deriving ord]

(* EXIT *)
type returning =
  | ReturningScalar of scalar
  | ReturningAddress of qualident
  | ReturningInt of {
      value: literal or_error;
      size: literal or_error option;
    }
[@@deriving ord]

type program_exit_status =
  | ExitDefault
  | ExitReturning of returning
  | ExitRaising of raising
[@@deriving ord]

type exit_stmt =
  | ExitSimple
  | ExitProgram of program_exit_status
  | ExitMethod of raising option
  | ExitFunction of raising option
  | ExitPerform of bool
  | ExitParagraph
  | ExitSection
[@@deriving ord]

(* INITIALIZE *)
type init_replacing = {
  init_replacing_category: Cobol_ptree.init_data_category;
  init_replacing_replacement_item: ident_or_literal;
}
[@@deriving ord]

type initialize_stmt = {
  init_items: ident list;
  init_filler: bool;
  init_category: Cobol_ptree.init_category option;
  init_replacings: init_replacing list;
  init_to_default: bool;
}
[@@deriving ord]

(* INVOKE *)
type invoke_stmt = {
  invoke_target: ident;
  invoke_method: ident_or_strlit;
  invoke_using: call_using_clause list;
  invoke_returning: ident with_loc option;
}
[@@deriving ord]

(* INSPECT *)
type inspect_reference = ident_or_nonnum Cobol_ptree.nel
[@@deriving ord]

type inspect_where = Cobol_ptree.inspect_direction * inspect_reference
[@@deriving ord]

type tallying_spec = {
  tallying_item: ident_or_nonnum;
  tallying_where: inspect_where list;
}
[@@deriving ord]

type tallying_clause =
  | TallyingCharacters of inspect_where list
  | TallyingRange of Cobol_ptree.tallying_range * tallying_spec list
[@@deriving ord]

type tallying = {
  tallying_target: qualident;
  tallying_clauses: tallying_clause with_loc list;
}
[@@deriving ord]

type replacing_range_spec = {
  replacing_item: ident_or_nonnum;
  replacing_by: ident_or_nonnum;
  replacing_where: inspect_where list;
}
[@@deriving ord]

type replacing_clause =
  | ReplacingCharacters of {
      replacement: ident_or_nonnum;
      where: inspect_where list;
    }
  | ReplacingRange of Cobol_ptree.replacing_range * replacing_range_spec list
[@@deriving ord]

type replacing = replacing_clause with_loc
[@@deriving ord]

type converting = {
  converting_from: ident_or_nonnum;
  converting_to: ident_or_nonnum;
  converting_where: inspect_where list;
}
[@@deriving ord]

type inspect_spec =
  | InspectTallying of tallying list
  | InspectReplacing of replacing list
  | InspectBoth of tallying list * replacing list
  | InspectConverting of converting
[@@deriving ord]

type inspect_stmt = {
  inspect_item: ident;
  inspect_spec: inspect_spec;
}
[@@deriving ord]

(* MERGE *)
type merge_stmt = {
  merge_file: Cobol_ptree.name with_loc;
  merge_keys: sort_spec list;
  merge_collating: Cobol_ptree.alphabet_specification option;
  merge_using: Cobol_ptree.name with_loc list;
  merge_target: Cobol_ptree.merge_or_sort_target;
}
[@@deriving ord]

(* MOVE *)
type move_stmt =
  | MoveSimple of {
      from: scalar;
      to_: ident list;
    }
  | MoveCorresponding of {
      from: ident;
      to_: ident list;
    }
[@@deriving ord]

(* OPEN *)
type open_phrase = {
  open_mode: Cobol_ptree.open_mode;
  open_sharing: Cobol_ptree.sharing_mode option;
  open_retry: retry_clause option;
  open_files: Cobol_ptree.named_file_option list;
}
[@@deriving ord]

type open_stmt = open_phrase list
[@@deriving ord]

(* RELEASE *)
type release_stmt = {
  release_item: data_ref;
  release_from: ident_or_literal option;
}
[@@deriving ord]

(* SEND *)
type message_ending_indicator =
  | EndingIndicator of ident
  | EndingIndicatorESI
  | EndingIndicatorEMI
  | EndingIndicatorEGI
[@@deriving ord]

type send_operands =
  | SendSimple of { from: ident }
  | SendWith of {
      from: ident option;
      ending_indicator: message_ending_indicator;
      advancing: advancing_phrase option;
      replace: bool;
    }
[@@deriving ord]

type send_stmt = {
  send_name: Cobol_ptree.name with_loc;
  send_operands: send_operands;
}
[@@deriving ord]

(* SET *)
type set_switch_spec = {
  set_switch_targets: ident list;
  set_switch_value: Cobol_ptree.on_off;
}
[@@deriving ord]

type set_condition_spec = {
  set_condition_targets: ident list;
  set_condition_value: bool;
}
[@@deriving ord]

type set_stmt =
  | SetAmbiguous of {
      targets: ident list;
      set_method: Cobol_ptree.set_ambiguous_method;
      value: expr with_loc;
    }
  | SetSwitch of set_switch_spec list
  | SetCondition of set_condition_spec list
  | SetEntry of {
      targets: ident list;
      value: ident_or_nonnum;
    }
  | SetAttribute of {
      name: Cobol_ptree.name with_loc;
      attribute_switches: Cobol_ptree.set_attribute_switch list;
    }
  | SetSaveLocale of {
      target: ident;
      locale: Cobol_ptree.set_save_locale;
    }
  | SetLocale of {
      target: Cobol_ptree.set_locale_target;
      source: set_locale_source;
    }
  | SetSavedException
  | SetFloatContent of {
      targets: ident list;
      content: Cobol_ptree.float_content;
      sign: Cobol_ptree.sign option;
    }
  | SetEnvironment of {
      variable: ident_or_literal;
      value: ident_or_literal;
    }
[@@deriving ord]

(* SORT *)
type sort_stmt =
  | SortFile of {
      file: Cobol_ptree.qualident; (* sort file, not data *)
      keys: sort_spec list;
      duplicate_in_order: bool;
      collating: Cobol_ptree.alphabet_specification option;
      source: Cobol_ptree.sort_source;
      target: Cobol_ptree.merge_or_sort_target;
    }
  | SortTable of {
      table: qualident;
      keys: sort_spec list;
      duplicate_in_order: bool;
      collating: Cobol_ptree.alphabet_specification option;
    }
[@@deriving ord]

(* STOP *)
type stop_arg =
  | StopWithQualIdent of qualident with_loc
  | StopWithLiteral of literal or_error with_loc
[@@deriving ord]

type stop_run_status = {
  status_kind: Cobol_ptree.status_kind;
  status_value: scalar option;
}
[@@deriving ord]

type stop_run_return =
  | StopReturning of returning
  | StopWithStatus of stop_run_status
[@@deriving ord]

type stop_stmt =
  | StopArg of stop_arg option
  | StopRun of stop_run_return option
  | StopError
  | StopThread of qualident option
[@@deriving ord]

(* TRANSFORM *)
type transform_stmt = {
  transform_ident: ident with_loc;
  transform_from: ident_or_nonnum with_loc;
  transform_to: ident_or_nonnum with_loc;
}
[@@deriving ord]

(* GO TO *)
type goto_stmt =
  | GoToSimple of { target: Cobol_ptree.procedure_name with_loc }
  | GoToDepending of {
      targets: Cobol_ptree.procedure_name with_loc Cobol_ptree.nel;
      depending_on: ident;
    }
  | GoToEntry of {
      targets: string with_loc Cobol_ptree.nel;
      depending_on: ident option;
    }
[@@deriving ord]

(* Error/exception handlers *)
type handler = statements

and dual_handler = {
  dual_handler_pos: handler;  (** positive case *)
  dual_handler_neg: handler;  (** negative case *)
}

(* EVALUATE *)
and evaluate_stmt = {
  eval_subjects: selection_subject or_error with_loc list;
  eval_branches: evaluate_branch list;
  eval_otherwise: statements;
}

and evaluate_branch = {
  eval_selection: selection_object or_error with_loc list list;
  eval_actions: statements;
}

(* PERFORM *)
and perform_target_stmt = {
  perform_target: Cobol_ptree.procedure_range;
  perform_mode: perform_mode option;
}

and perform_inline_stmt = {
  perform_inline_mode: perform_mode option;
  perform_statements: statements;
}

and perform_mode =
  | PerformNTimes of ident_or_intlit
  | PerformUntil of {
      with_test: Cobol_ptree.stage option;
      until: condition or_error with_loc option (* None = UNTIL EXIT *);
    }
  | PerformVarying of {
      with_test: Cobol_ptree.stage option;
      varying: varying_phrase with_loc;
      after: varying_phrase with_loc list;
    }
  | PerformForever

and varying_phrase = {
  varying_ident: ident;
  varying_from: scalar;
  varying_by: scalar option;
  varying_until: condition or_error with_loc;
}

(* SEARCH *)
and search_stmt = {
  search_item: data_ref;
  search_varying: ident option;
  search_at_end: handler;
  search_when_clauses: search_when_clause with_loc list;
}

and search_when_clause = {
  search_when_cond: condition or_error with_loc;
  search_when_stmts: statements;
}

(* SEARCH ALL *)
and search_all_stmt = {
  search_all_item: data_ref;
  search_all_at_end: handler;
  search_all_conditions: search_condition list;
  search_all_action: statements;
}

(* IF *)
and if_stmt = {
  condition: condition or_error with_loc;
  then_branch: statements;
  else_branch: statements;
}

(* ACCEPT *)
and accept_stmt =
  | AcceptGeneric of ident with_loc
  | AcceptFromDevice of {
      item: ident with_loc;
      device_item: Cobol_ptree.name with_loc;
    }
  | AcceptTemporal of {
      item: ident with_loc;
      date_time: Cobol_ptree.date_time;
    }
  | AcceptMisc of {
      item: ident with_loc;
      misc: Cobol_ptree.accept_misc;
    }
  | AcceptMsgCount of Cobol_ptree.name with_loc
  | AcceptScreen of {
      item: ident with_loc;
      clauses: accept_clause with_loc list;
      on_exception: dual_handler;
    }
  | AcceptFromEnv of {
      item: ident with_loc;
      env_item: ident_or_nonnum with_loc option;
      on_exception: dual_handler;
    }
  | AcceptFromArg of {
      item: ident with_loc;
      on_exception: dual_handler;
    }
  | AcceptFromCmdLine of {
      item: ident with_loc;
      on_exception: dual_handler;
    }

and accept_clause =
  | AcceptAt of position
  | AcceptFromCRT
  | AcceptModeBlock
  | AcceptWith of accept_with_clause with_loc list

and accept_with_clause =
  | AcceptAttribute of screen_attribute_clause
  | AcceptAuto
  | AcceptFull
  | AcceptGrid
  | AcceptLeftLine
  | AcceptOverLine
  | AcceptPromptCharacter of ident_or_nonnum option
  | AcceptRequired
  | AcceptSecure
  | AcceptSize of ident_or_intlit
  | AcceptControl of ident
  | AcceptTimeout
  | AcceptLeftJustify
  | AcceptRightJustify
  | AcceptSpaceFill
  | AcceptTrailingSign
  | AcceptUpdate
  | AcceptUpper
  | AcceptLower
  | AcceptZeroFill

(* DISPLAY *)
and display_stmt = {
  display_items_clauses: display_items_clauses list;
  no_advancing: bool;
  on_exception: dual_handler;
}

and display_items_clauses = {
  display_items: ident_or_literal with_loc list;
  display_clauses: display_clause with_loc list;
}

and display_clause =
  | DisplayAt of position
  | DisplayUpon of Cobol_ptree.display_target with_loc
  | DisplayModeIsBlock
  | DisplayWith of display_with_clause with_loc list

and display_with_clause =
  | DisplayAttribute of screen_attribute_clause
  | DisplayGrid
  | DisplayErase of Cobol_ptree.erase_clause
  | DisplayLeftLine
  | DisplayOverLine
  | DisplaySize of ident_or_intlit
  | DisplayControl of ident
  | DisplayBlank of Cobol_ptree.blank_clause

(* ADD & SUBTRACT *)
and basic_arithmetic_stmt = {
  basic_arith_operands: basic_arithmetic_operands;
  basic_arith_on_size_error: dual_handler;
}

(* COMPUTE *)
and compute_stmt = {
  compute_targets: rounded_idents;
  compute_expr: expr with_loc;
  compute_on_size_error: dual_handler;
}

(* DELETE *)
and delete_stmt = {
  delete_targets: Cobol_ptree.name with_loc;
  delete_retry: retry_clause option;
  delete_on_invalid_key: dual_handler;
}

(* DIVIDE *)
and divide_stmt = {
  divide_operands: divide_operands;
  divide_on_size_error: dual_handler;
}

(* MULTIPLY *)
and multiply_stmt = {
  multiply_operands: multiply_operands;
  multiply_on_size_error: dual_handler;
}

(* RECEIVE *)
and receive_stmt = {
  receive_name: Cobol_ptree.name with_loc; (* CD *)
  receive_kind: Cobol_ptree.mcs_awaiting_item;
  receive_into: ident;
  receive_on_no_data: dual_handler;
}

(* RETURN *)
and return_stmt = {
  return_file: Cobol_ptree.name with_loc;
  return_into: ident with_loc option;
  return_at_end: dual_handler;
}

(* REWRITE *)
and rewrite_stmt = {
  rewrite_to: write_target;
  rewrite_from: ident_or_literal option;
  rewrite_retry: retry_clause option;
  rewrite_lock: bool option;
  rewrite_invalid_key_handler: dual_handler;
}

(* START *)
and start_stmt = {
  start_file: Cobol_ptree.name with_loc;
  start_position: start_position option;
  start_on_invalid_key: dual_handler;
}

(* STRING *)
and string_stmt = {
  string_sources: string_source list;
  string_target: ident;
  string_pointer: ident option;
  string_on_overflow: dual_handler;
}

(* UNSTRING *)
and unstring_stmt = {
  unstring_source: ident;
  unstring_delimiters: unstring_delimiter list;
  unstring_targets: unstring_target list;
  unstring_pointer: ident option;
  unstring_tallying: ident option;
  unstring_on_overflow: dual_handler;
}

(* WRITE *)
and write_stmt = {
  write_to: write_target;
  write_from: ident_or_literal option;
  write_advancing: advancing_phrase option;
  write_retry: retry_clause option;
  write_lock: bool option;
  write_error_handler: (Cobol_ptree.write_error * dual_handler) option;
}

(* CALL *)
and call_stmt = {
  call_static: bool;
  call_target: call_target;
  call_using: call_using_clause list;
  call_returning: ident with_loc option;
  call_error_handler: call_error_handler option;
}

and call_target =
  | CallGeneral of ident_or_strlit
  | CallProto of {
      called: ident_or_strlit option;
      prototype: call_proto;
    }

and call_proto =
  | CallProtoIdent of ident
  | CallProtoNested

and call_error_handler =
  | CallOnOverflow of handler
  | CallOnException of dual_handler

(* READ *)
and read_stmt = {
  read_file: Cobol_ptree.name with_loc;
  read_direction: Cobol_ptree.read_direction option;
  read_into: ident option;
  read_lock_behavior: read_lock_behavior option;
  read_lock: bool option;
  read_key: data_ref option;
  read_error_handler: (Cobol_ptree.read_error * dual_handler) option;
}

(* GOBACK *)
and goback_stmt = {
  goback_raising: raising option;
  goback_returning: ident_or_intlit with_loc option;
}

and statement =
  | Accept of accept_stmt
  | Add of basic_arithmetic_stmt
  | Allocate of allocate_stmt
  | Alter of Cobol_ptree.alter_stmt
  | Call of call_stmt
  | Cancel of ident_or_strlit list (* non-empty *)
  | Close of Cobol_ptree.close_stmt
  | Compute of compute_stmt
  | Continue
  | Delete of delete_stmt
  | Disable of mcs_command_operands
  | Display of display_stmt
  | Divide of divide_stmt
  | Enable of mcs_command_operands
  | Enter of Cobol_ptree.enter_stmt
  | Entry of entry_stmt
  | Evaluate of evaluate_stmt
  | ExecBlock of Cobol_common.Exec_block.t
  | Exit of exit_stmt
  | Free of data_ref list
  | Generate of Cobol_ptree.qualname with_loc
  | GoTo of goto_stmt
  | GoBack of goback_stmt
  | If of if_stmt
  | Initialize of initialize_stmt
  | Initiate of Cobol_ptree.name with_loc list
  | Inspect of inspect_stmt
  | Invoke of invoke_stmt
  | LoneGoTo (* COB85, obsolete *)
  | Merge of merge_stmt
  | Move of move_stmt
  | Multiply of multiply_stmt
  | NextSentence
  | Open of open_stmt
  | PerformTarget of perform_target_stmt
  | PerformInline of perform_inline_stmt
  | Purge of Cobol_ptree.name with_loc
  | Raise of raise_operand
  | Read of read_stmt
  | Receive of receive_stmt
  | Release of release_stmt
  | Resume of Cobol_ptree.resume_stmt
  | Return of return_stmt
  | Rewrite of rewrite_stmt
  | Search of search_stmt
  | SearchAll of search_all_stmt
  | Send of send_stmt
  | Set of set_stmt
  | Sort of sort_stmt
  | Start of start_stmt
  | Stop of stop_stmt
  | String of string_stmt
  | Subtract of basic_arithmetic_stmt
  | Suppress
  | Terminate of Cobol_ptree.terminate_stmt
  | Transform of transform_stmt
  | Unlock of Cobol_ptree.unlock_stmt
  | Unstring of unstring_stmt
  | Validate of ident list
  | Write of write_stmt

and statements = statement with_loc list [@@deriving ord]
