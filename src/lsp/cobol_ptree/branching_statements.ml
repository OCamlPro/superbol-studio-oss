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

open Common
open Numericals
open Terms
open Operands
open Simple_statements

(* --- *)

(* GOTO *)
type goto_stmt =
  | GoToSimple of
      {
        target: procedure_name with_loc
      }
  | GoToDepending of
      {
        targets: procedure_name with_loc nel;
        depending_on: ident;
      }
  | GoToEntry of
      {
        targets: alphanum with_loc nel;
        depending_on: ident option;
      }
[@@deriving ord]

let pp_goto_stmt ppf = function
  | GoToSimple { target } ->
      Fmt.pf ppf "@[GO TO %a@]" pp_procedure_name' target
  | GoToDepending { targets; depending_on } ->
      Fmt.pf ppf "@[GO TO @[%a@]@ DEPENDING ON %a@]"
        (pp_nel pp_procedure_name') targets
        pp_ident depending_on
  | GoToEntry { targets; depending_on = None } ->
      Fmt.pf ppf "@[GO TO ENTRY @[%a@]@]"
        (pp_nel @@ pp_with_loc pp_alphanum)
        targets
  | GoToEntry { targets; depending_on = Some i } ->
      Fmt.pf ppf "@[GO TO ENTRY @[%a@]@ DEPENDING ON %a@]"
        (pp_nel @@ pp_with_loc pp_alphanum)
        targets
        pp_ident i


(* RESUME *)

type resume_stmt =
  | ResumeNextStatement
  | ResumeTarget of procedure_name with_loc
[@@deriving ord]

let pp_resume_stmt ppf = function
  | ResumeNextStatement ->
      Fmt.pf ppf "RESUME AT NEXT STATEMENT"
  | ResumeTarget p ->
      Fmt.pf ppf "RESUME AT %a" pp_procedure_name' p

(* --- *)

(* Error/exception handlers *)
type handler =
  statements

and dual_handler =
  {
    dual_handler_pos: handler;                              (** positive case *)
    dual_handler_neg: handler;                              (** negative case *)
  }


(* EVAL *)
and evaluate_stmt =
  {
    eval_subjects: selection_subject list;
    eval_branches: evaluate_branch list;
    eval_otherwise: statements;
  }

and evaluate_branch =
  {
    eval_selection: selection_object list list;
    eval_actions: statements;
  }


(* PERFORM *)
and perform_target_stmt =
  {
    perform_target: procedure_name with_loc procedure_range;
    perform_mode: perform_mode option;
  }

and perform_inline_stmt =
  {
    perform_inline_mode: perform_mode option;
    perform_statements: statements;
  }

and perform_mode =
  | PerformNTimes of ident_or_intlit
  | PerformUntil of
      {
        with_test: stage option;
        until: condition option; (* None = UNTIL EXIT *)
      }
  | PerformVarying of
      {
        with_test: stage option;
        varying: varying_phrase with_loc;
        after: varying_phrase with_loc list;
      }
  | PerformForever (* GC/COBOL-IT extension *)

and varying_phrase =
  {
    varying_ident: ident;
    varying_from: scalar;
    varying_by: scalar option;                  (* XXX: more specialized type *)
    varying_until: condition;
  }


(* SEARCH *)
and search_stmt =
  {
    search_item: qualname;
    search_varying: ident option;
    search_at_end: handler;
    search_when_clauses: search_when_clause with_loc list;
  }

and search_when_clause =
  {
    search_when_cond: condition;
    search_when_stmts: statements;
  }

(* SEARCH ALL *)
and search_all_stmt =
  {
    search_all_item: qualname;
    search_all_at_end: handler;
    search_all_conditions: search_condition list;
    search_all_action: statements;
  }


(* IF *)
and if_stmt =
  {
    condition: condition;
    then_branch: statements;
    else_branch: statements;
  }


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
  | AcceptMisc of                                                       (* MF *)
      {
        item: ident with_loc;
        misc: accept_misc;
      }
  | AcceptMsgCount of name with_loc
  | AcceptScreen of
      {
        item: ident with_loc;
        clauses: accept_clause with_loc list;
        on_exception: dual_handler;
      }
  | AcceptFromEnv of                                                    (* MF *)
      {
        item: ident with_loc;
        env_item: ident_or_nonnum with_loc option;
        on_exception: dual_handler;
      }
  | AcceptFromArg of                                                    (* MF *)
      {
        item: ident with_loc;
        on_exception: dual_handler;
      }
  | AcceptFromCmdLine of                                          (* GnuCOBOL *)
      {
        item: ident with_loc;
        on_exception: dual_handler;
      }

and accept_misc =
  | AcceptLineNumber
  | AcceptLines
  | AcceptColumns
  | AcceptUserName
  | AcceptEscapeKey
  | AcceptExceptionStatus

and accept_clause =
  | AcceptAt of position
  | AcceptFromCRT                                                       (* MF *)
  | AcceptModeBlock                                                     (* MF *)
  | AcceptWith of accept_with_clause with_loc list                      (* MF *)

and accept_with_clause =
  | AcceptAttribute of Data_descr.screen_attribute_clause
  | AcceptAuto
  | AcceptFull
  | AcceptGrid
  | AcceptLeftLine
  | AcceptOverLine
  | AcceptPromptCharacter of ident_or_nonnum option (* or figurative *)
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


(*
DISPLAY id/lit+ UPON...? WITH...? END_DISP
DISPLAY id AT...? ON EXCEPT?
*)

and display_stmt =
  {
    display_items_clauses: display_items_clauses list; (* non-empty *)
    no_advancing: bool;
    on_exception: dual_handler;
  }

and display_items_clauses =
  {
    display_items: ident_or_literal list; (* non-empty *)
    display_clauses: display_clause with_loc list;
  }

and display_clause =
  | DisplayAt of position
  | DisplayUpon of display_target with_loc
  | DisplayModeIsBlock                                                   (* MF *)
  | DisplayWith of display_with_clause with_loc list                     (* MF *)

and display_target =
  | DisplayUponName of name with_loc
  | DisplayUponDeviceViaMnemonic of display_device_mnemonic with_loc

and display_device_mnemonic =
 | DisplayDeviceEnvName
 | DisplayDeviceEnvValue
 | DisplayDeviceArgNumber
 | DisplayDeviceCommandLine
 | DisplayDevicePrinter
 | DisplayDeviceTerminal

and display_with_clause =
  | DisplayAttribute of Data_descr.screen_attribute_clause
  | DisplayGrid
  | DisplayErase of Data_descr.erase_clause
  | DisplayLeftLine
  | DisplayOverLine
  | DisplaySize of ident_or_intlit
  | DisplayControl of ident
  | DisplayBlank of Data_descr.blank_clause


(* ADD & SUBTRACT *)
and basic_arithmetic_stmt =
  {
    basic_arith_operands: basic_arithmetic_operands;
    basic_arith_on_size_error: dual_handler;
  }


(* COMPUTE *)
and compute_stmt =
  {
    compute_targets: rounded_idents;
    compute_expr: expression;
    compute_on_size_error: dual_handler;
  }


(* DELETE *)
and delete_stmt =
  {
    delete_targets: name with_loc;
    delete_retry: retry_clause option;
    delete_on_invalid_key: dual_handler;
  }


(* DIVIDE *)
and divide_stmt =
  {
    divide_operands: divide_operands;
    divide_on_size_error: dual_handler;
  }


(* MULTIPLY *)
and multiply_stmt =
  {
    multiply_operands: multiply_operands;
    multiply_on_size_error: dual_handler;
  }


(* RECEIVE *)
and receive_stmt =
  {
    receive_name: name with_loc;                                        (* CD *)
    receive_kind: mcs_awaiting_item;
    receive_into: ident;
    receive_on_no_data: dual_handler;
  }


(* RETURN *)
and return_stmt =
  {
    return_file: name with_loc;
    return_into: ident with_loc option;
    return_at_end: dual_handler;
  }


(* REWRITE *)
and rewrite_stmt =
  {
    rewrite_to: write_target;
    rewrite_from: ident_or_literal option;
    rewrite_retry: retry_clause option;
    rewrite_lock: bool option;
    rewrite_invalid_key_handler: dual_handler;
  }


(* START *)
and start_stmt =
  {
    start_file: name with_loc;
    start_position: start_position option;
    start_on_invalid_key: dual_handler;
  }


(* STRING *)
and string_stmt =
  {
    string_sources: string_source list;
    string_target: ident;
    string_pointer: ident option;
    string_on_overflow: dual_handler;
  }


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

and write_error =
  | WriteAtEndOfPage
  | WriteInvalidKey



(*
CALL id/lit USING...? RETURNING...? ON OVERFLOW (on-overflow - archaic)
CALL id/lit USING...? RETURNING...? ON EXCEPT (on-exception)

CALL (id/lit AS)? NESTED/id USING...? RETURNING...? (program-prototype)
*)

(* CALL *)
and call_stmt =
  {
    call_static: bool;                                            (* GnuCOBOL *)
    call_prefix: call_prefix;
    call_using: call_using_clause with_loc list;
    call_returning: ident with_loc option;
    call_error_handler: call_error_handler option;
  }

and call_prefix =
  | CallGeneral of ident_or_strlit
  | CallProto of
      {
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

and read_error =
  | ReadAtEnd
  | ReadInvalidKey

and goback_stmt =
  {
    goback_raising : raising option;
    goback_returning: ident_or_intlit with_loc option;
  }

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
  | Entry of entry_stmt
  | Evaluate of evaluate_stmt
  | ExecBlock of Cobol_common.Exec_block.t
  | Exit of exit_stmt
  | Free of name with_loc list
  | Generate of name with_loc
  | GoTo of goto_stmt
  | GoBack of goback_stmt
  | If of if_stmt
  | Initialize of initialize_stmt
  | Initiate of name with_loc list
  | Inspect of inspect_stmt
  | Invoke of invoke_stmt
  | LoneGoTo                                               (* COB85, obsolete *)
  | Merge of merge_stmt
  | Move of move_stmt
  | Multiply of multiply_stmt
  | NextSentence
  | Open of open_stmt
  | PerformTarget of perform_target_stmt
  | PerformInline of perform_inline_stmt
  | Purge of name with_loc
  | Raise of raise_operand
  | Read of read_stmt
  | Receive of receive_stmt
  | Release of release_stmt
  | Resume of resume_stmt
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
  | Terminate of terminate_stmt
  | Transform of transform_stmt
  | Unlock of unlock_stmt
  | Unstring of unstring_stmt
  | Validate of ident list
  | Write of write_stmt

and statements = statement with_loc list
[@@deriving ord, show]

let pp_display_device_mnemonic ppf = function
  | DisplayDeviceEnvName -> Fmt.pf ppf "ENVIRONMENT-NAME"
  | DisplayDeviceEnvValue -> Fmt.pf ppf "ENVIRONMENT-VALUE"
  | DisplayDeviceArgNumber -> Fmt.pf ppf "ARGUMENT-NUMBER"
  | DisplayDeviceCommandLine -> Fmt.pf ppf "COMMAND-LINE"
  | DisplayDevicePrinter -> Fmt.pf ppf "PRINTER"
  | DisplayDeviceTerminal -> Fmt.pf ppf "TERMINAL"

let pp_display_target ppf = function
  | DisplayUponName nwl -> Fmt.pf ppf "UPON@ %a" (pp_with_loc pp_name) nwl
  | DisplayUponDeviceViaMnemonic mn ->
    Fmt.pf ppf "UPON@ %a" (pp_with_loc pp_display_device_mnemonic) mn

let pp_read_error ppf = function
  | ReadAtEnd -> Fmt.pf ppf "AT END"
  | ReadInvalidKey -> Fmt.pf ppf "INVALID KEY"

let pp_write_error ppf = function
  | WriteAtEndOfPage -> Fmt.pf ppf "AT EOP"
  | WriteInvalidKey -> Fmt.pf ppf "INVALID KEY"

let pp_call_proto ppf = function
  | CallProtoIdent i -> pp_ident ppf i
  | CallProtoNested -> Fmt.pf ppf "NESTED"

let pp_call_prefix ppf = function
  | CallGeneral i -> pp_ident_or_strlit ppf i
  | CallProto { called; prototype } ->
    Fmt.(option (pp_ident_or_strlit ++ any "@ AS@ ")) ppf called;
    pp_call_proto ppf prototype

let pp_dual_handler pp ?close ?(on = Fmt.any "ON EXCEPTION") ?off ppf
  { dual_handler_pos = p; dual_handler_neg = n }
=
  let off = Option.value ~default:Fmt.(any "NOT " ++ on) off in
  if p != [] then
    Fmt.(
      pf ppf "@ %a@ %a" on () (list ~sep:sp (pp_with_loc pp)) p
    );
  if n != [] then
    Fmt.(
      pf ppf "@ %a@ %a" off () (list ~sep:sp (pp_with_loc pp)) n
    );
  (* NOTE: We need the END-XXX even if there are no handler because otherwise
     there could be confusion. *)
  match close with
  | None -> ()
  | Some pp -> Fmt.sp ppf (); pp ppf ()

let pp_branching_stmt op ?pos ?neg pp_args pp_stmt ppf (args, clauses, dh) =
  Fmt.pf ppf "@[%s@;<1 2>%a" op (Fmt.box pp_args) args;
  Fmt.(list ~sep:nop (sp ++ box ~indent:2 (fun ppf pf -> pf ppf ())))
    ppf clauses;
  Option.iter (fun dh ->
    pp_dual_handler pp_stmt
      ?on:pos ?off:neg ~close:Fmt.(any "END-" ++ const string op)
      ppf dh) dh;
  Fmt.pf ppf "@]"

let opt_clause fmtt = function
  | Some opt -> [ Fmt.(const (sp ++ box fmtt) opt) ]
  | None -> []

let list_clause fmtt = function
  | [] -> []
  | xs -> [ Fmt.(const (sp ++ box fmtt)) xs ]

let pp_arithmetic_stmt op pp_operands pp_statement ppf (operands, on_size_error) =
  Fmt.pf ppf "@[%s%a" op pp_operands operands;
  pp_dual_handler pp_statement
    ~on:Fmt.(any "ON SIZE ERROR") ~close:Fmt.(any "END-" ++ const string op)
    ppf on_size_error;
  Fmt.pf ppf "@]"

let rec pp_handler ppf h = pp_statements ppf h

(* EVALUATE *)

and pp_evaluate_stmt ppf { eval_subjects; eval_branches; eval_otherwise } =
  Fmt.pf ppf "EVALUATE@ %a@ %a"
    Fmt.(list ~sep:(any "@ ALSO@ ") pp_selection_subject) eval_subjects
    Fmt.(list ~sep:sp pp_evaluate_branch) eval_branches;
  if eval_otherwise != [] then
    Fmt.pf ppf "@ WHEN@ OTHER@ %a" pp_statements eval_otherwise;
  Fmt.pf ppf "@ END-EVALUATE"

and pp_evaluate_branch ppf { eval_selection; eval_actions } =
  Fmt.pf ppf "WHEN@ %a@ %a"
    Fmt.(
      list ~sep:(any "@ WHEN@ ")
        (list ~sep:(any "@ ALSO@ ") pp_selection_object)
    ) eval_selection
    pp_statements eval_actions

(* PERFORM *)

and pp_perform_target_stmt ppf { perform_target; perform_mode } =
  Fmt.pf ppf "@[<hv>PERFORM@;<1 2>%a%a@]"
    (pp_procedure_range pp_procedure_name') perform_target
    Fmt.(option (sp ++ pp_perform_mode)) perform_mode

and pp_perform_inline_stmt ppf { perform_inline_mode; perform_statements } =
  Fmt.pf ppf "@[<v>@[PERFORM%a@]@;<1 2>%a@ END-PERFORM@]"
    Fmt.(option (sp ++ pp_perform_mode)) perform_inline_mode
    pp_statements perform_statements

and pp_perform_mode ppf = function
  | PerformForever -> Fmt.pf ppf "FOREVER"
  | PerformNTimes i -> Fmt.pf ppf "%a TIMES" pp_ident_or_intlit i
  | PerformUntil { with_test = _; until = None } ->
    Fmt.pf ppf "UNTIL EXIT"
  | PerformUntil { with_test; until = Some until } ->
    Fmt.(option (any " TEST " ++ pp_stage ++ sp)) ppf with_test;
    Fmt.pf ppf "UNTIL %a" pp_condition until
  | PerformVarying { with_test; varying; after } ->
    Fmt.(option (any " TEST " ++ pp_stage ++ sp)) ppf with_test;
    Fmt.pf ppf "VARYING %a"
      Fmt.(list ~sep:(any "@ AFTER ") (pp_with_loc pp_varying_phrase))
      (varying :: after);

and pp_varying_phrase ppf
  { varying_ident = vi; varying_from = vf; varying_by = vb; varying_until = vu }
=
  Fmt.pf ppf "%a FROM %a%a UNTIL %a"
    pp_ident vi
    pp_scalar vf
    Fmt.(option (any " BY " ++ pp_scalar)) vb
    pp_condition vu

(* SEARCH *)

and pp_search_stmt ppf { search_item = si; search_varying = sv;
                         search_at_end = h; search_when_clauses = swc } =
  Fmt.pf ppf "SEARCH %a" pp_qualname si;
  Fmt.(option (any "@ VARYING " ++ pp_ident)) ppf sv;
  List.iter (fun pf -> pf ppf ()) @@
  list_clause Fmt.(any "@ AT END " ++ box pp_handler) h;
  Fmt.(sp ++ list ~sep:sp (pp_with_loc pp_search_when_clause)) ppf swc;
  Fmt.pf ppf "@ END-SEARCH"

and pp_search_all_stmt ppf { search_all_item = si;
                             search_all_at_end = h;
                             search_all_conditions = c;
                             search_all_action = a } =
  Fmt.pf ppf "SEARCH ALL %a" pp_qualname si;
  List.iter (fun pf -> pf ppf ()) @@
  list_clause Fmt.(any "@ AT END " ++ box pp_handler) h;
  Fmt.(any "@ WHEN " ++ list ~sep:(any " AND@ ") pp_search_condition) ppf c;
  Fmt.(sp ++ pp_statements) ppf a;
  Fmt.pf ppf "@ END-SEARCH"

and pp_search_when_clause ppf { search_when_cond = c; search_when_stmts = w } =
  Fmt.pf ppf "WHEN %a@ %a" pp_condition c pp_statements w

(* IF *)

and pp_if_stmt ppf { condition = c; then_branch = t; else_branch = e } =
  let pp_then =
    Format.pp_print_custom_break
      ~fits:("", 0, "")
      ~breaks:("", -2, "THEN")
  in
  let e = match e with [] -> None | _ -> Some e in
  Fmt.pf ppf "@[<v>IF@[<hv>@ %a%t@]@;<1 2>%a%a@ END-IF@]"
    (Fmt.box pp_condition) c pp_then
    (Fmt.vbox pp_statements) t
    Fmt.(option (any "@ ELSE@;<1 2>" ++ vbox pp_statements)) e

(* ACCEPT *)

and pp_accept_stmt ppf = function
  | AcceptGeneric item -> Fmt.pf ppf "ACCEPT@ %a" (pp_with_loc pp_ident) item
  | AcceptFromDevice { item; device_item = di } ->
      Fmt.pf ppf "ACCEPT@ %a@ FROM@ %a"
        (pp_with_loc pp_ident) item
        (pp_with_loc pp_integer) di
  | AcceptTemporal { item; date_time = dt } ->
      Fmt.pf ppf "ACCEPT@ %a@ FROM@ %a"
        (pp_with_loc pp_ident) item
        pp_date_time dt
  | AcceptMisc { item; misc = m } ->
      Fmt.pf ppf "ACCEPT@ %a@ FROM@ %a"
        (pp_with_loc pp_ident) item
        pp_accept_misc m
  | AcceptMsgCount cnt ->
      Fmt.pf ppf "ACCEPT@ %a@ MESSAGE@ COUNT" (pp_with_loc pp_integer) cnt
  | AcceptScreen { item; clauses; on_exception } ->
      Fmt.pf ppf "@[ACCEPT@ %a" (pp_with_loc pp_ident) item;
      Fmt.pf ppf "@ %a"
        Fmt.(list (pp_with_loc pp_accept_clause)) clauses;
      pp_dual_handler pp_statement ~close:Fmt.(any "END-ACCEPT")
        ppf on_exception;
      Fmt.pf ppf "@]";
  | AcceptFromEnv { item; env_item; on_exception } ->
      Fmt.pf ppf "@[ACCEPT@;<1 2>%a@ @[FROM "
        Fmt.(box (pp_with_loc pp_ident)) item;
      begin
        match env_item with
        | Some ei ->
            Fmt.pf ppf "ENVIRONMENT@;<1 2>%a"
              Fmt.(box (pp_with_loc pp_ident_or_nonnum)) ei
        | None ->
            Fmt.pf ppf "ENVIRONMENT-VALUE"
      end;
      pp_dual_handler pp_statement ~close:Fmt.(any "END-ACCEPT")
        ppf on_exception;
      Fmt.pf ppf "@]"
  | AcceptFromArg { item; on_exception } ->
      Fmt.pf ppf "@[ACCEPT@;<1 2>%a@ "
        Fmt.(box @@ any "FROM ARGUMENT-VALUE@ " ++ pp_with_loc pp_ident) item;
      pp_dual_handler pp_statement ~close:Fmt.(any "END-ACCEPT")
        ppf on_exception;
      Fmt.pf ppf "@]"
  | AcceptFromCmdLine { item; on_exception } ->
      Fmt.pf ppf "@[ACCEPT@;<1 2>%a@ "
        Fmt.(box @@ any "FROM COMMAND-LINE@ " ++ pp_with_loc pp_ident) item;
      pp_dual_handler pp_statement ~close:Fmt.(any "END-ACCEPT")
        ppf on_exception;
      Fmt.pf ppf "@]"

and pp_accept_misc ppf = function
  | AcceptLineNumber -> Fmt.pf ppf "LINE NUMBER"
  | AcceptLines -> Fmt.pf ppf "LINES"
  | AcceptColumns -> Fmt.pf ppf "COLUMNS"
  | AcceptUserName -> Fmt.pf ppf "USER NAME"
  | AcceptEscapeKey -> Fmt.pf ppf "ESCAPE KEY"
  | AcceptExceptionStatus -> Fmt.pf ppf "EXCEPTION STATUS"

and pp_accept_clause ppf = function
  | AcceptAt position ->
    Fmt.pf ppf "AT@ %a" pp_position position
  | AcceptFromCRT ->
    Fmt.pf ppf "FROM@ CRT@"
  | AcceptModeBlock ->
    Fmt.pf ppf "MODE@ IS@ BLOCK@"
  | AcceptWith with_ ->
    Fmt.pf ppf "WITH@ %a"
      Fmt.(list ~sep:sp (pp_with_loc pp_accept_with_clause)) with_

and pp_accept_with_clause ppf = function
  | AcceptAttribute ac -> Data_descr.pp_screen_attribute_clause ppf ac
  | AcceptAuto -> Fmt.pf ppf "AUTO"
  | AcceptFull -> Fmt.pf ppf "FULL"
  | AcceptGrid -> Fmt.pf ppf "GRID"
  | AcceptLeftLine -> Fmt.pf ppf "LEFTLINE"
  | AcceptOverLine -> Fmt.pf ppf "OVERLINE"
  | AcceptPromptCharacter (None) -> Fmt.pf ppf "PROMPT"
  | AcceptPromptCharacter (Some inn) ->
      Fmt.pf ppf "PROMPT CHARACTER IS %a" pp_ident_or_nonnum inn
  | AcceptRequired -> Fmt.pf ppf "REQUIRED"
  | AcceptSecure -> Fmt.pf ppf "SECURE" (* MF doc says "SELECT", likely a typo *)
  | AcceptSize ii -> Fmt.pf ppf "SIZE IS %a" pp_ident_or_intlit ii
  | AcceptControl i -> Fmt.pf ppf "CONTROL IS %a" pp_ident i
  | AcceptTimeout -> Fmt.pf ppf "TIMEOUT"
  | AcceptLeftJustify -> Fmt.pf ppf "LEFT-JUSTIFY"
  | AcceptRightJustify -> Fmt.pf ppf "RIGHT-JUSTIFY"
  | AcceptSpaceFill -> Fmt.pf ppf "SPACE-FILL"
  | AcceptTrailingSign -> Fmt.pf ppf "TRAILING-SIGN"
  | AcceptUpdate -> Fmt.pf ppf "UPDATE"
  | AcceptUpper -> Fmt.pf ppf "UPPER"
  | AcceptLower -> Fmt.pf ppf "LOWER"
  | AcceptZeroFill -> Fmt.pf ppf "ZERO-FILL"

(* DISPLAY *)

and pp_display_stmt ppf
  { display_items_clauses; no_advancing; on_exception }
=
  Fmt.pf ppf "DISPLAY@ %a"
    Fmt.(list ~sep:sp pp_display_items_clauses) display_items_clauses;
  if no_advancing then Fmt.pf ppf "@ NO@ ADVANCING";
  pp_dual_handler pp_statement ~close:Fmt.(any "END-DISPLAY") ppf on_exception

and pp_display_items_clauses ppf
  { display_items; display_clauses }
=
  Fmt.pf ppf "%a%a"
    Fmt.(list ~sep:sp pp_ident_or_literal) display_items
    Fmt.(list ~sep:sp (pp_with_loc pp_display_clause)) display_clauses

and pp_display_clause ppf = function
 | DisplayAt p ->
   Fmt.pf ppf "AT@ %a" pp_position p
 | DisplayUpon dt ->
   Fmt.pf ppf "%a" (pp_with_loc pp_display_target) dt
 | DisplayModeIsBlock ->
   Fmt.pf ppf "MODE@ BLOCK@"
 | DisplayWith sc ->
   Fmt.(list ~sep:sp (pp_with_loc pp_display_with_clause)) ppf sc

and pp_display_with_clause ppf = function
  | DisplayAttribute ac -> Data_descr.pp_screen_attribute_clause ppf ac
  | DisplayGrid -> Fmt.pf ppf "GRID@"
  | DisplayErase ec -> Data_descr.pp_erase_clause ppf ec
  | DisplayLeftLine -> Fmt.pf ppf "LEFTLINE@"
  | DisplayOverLine -> Fmt.pf ppf "OVERLINE@"
  | DisplaySize ii -> Fmt.pf ppf "SIZE@ IS@ %a" pp_ident_or_intlit ii
  | DisplayControl i -> Fmt.pf ppf "CONTROL@ IS@ %a" pp_ident i
  | DisplayBlank bc -> Data_descr.pp_blank_clause ppf bc

(* ADD & SUBTRACT *)

and pp_basic_arithmetic_stmt ~sep op ppf
  { basic_arith_operands = ops; basic_arith_on_size_error = dh }
=
  pp_arithmetic_stmt op (pp_basic_arithmetic_operands ~sep) pp_statement
    ppf (ops, dh)

(* CALL *)

and pp_call_stmt ppf
  { call_static = cs
  ; call_prefix = cp
  ; call_using = cu
  ; call_returning = returning
  ; call_error_handler = cho }
=
  let on, dh =
    match cho with
    | Some CallOnOverflow h ->
      Some (Fmt.any "ON OVERFLOW"), { dual_handler_pos = h; dual_handler_neg = [] }
    | Some CallOnException dh ->
      Some (Fmt.any "ON EXCEPTION"), dh
    | None ->
      None, { dual_handler_pos = []; dual_handler_neg  = [] }
  in
  Fmt.pf ppf "@[CALL %s"
    (if cs then "STATIC " else "");
  pp_call_prefix ppf cp;
  if cu != [] then
    Fmt.pf ppf "@ USING@ %a"
      Fmt.(list ~sep:sp (pp_with_loc pp_call_using_clause)) cu;
  Option.iter (Fmt.pf ppf "@ RETURNING@ %a" (pp_with_loc pp_ident)) returning;
  pp_dual_handler pp_statement
    ?on ~close:Fmt.(any "END-CALL") ppf dh;
  Fmt.pf ppf "@]"

(* COMPUTE *)

and pp_compute_stmt ppf
  { compute_targets = ts; compute_expr = e; compute_on_size_error = dh}
=
  Fmt.pf ppf "@[COMPUTE@;<1 2>@[@[%a@] = @[%a@]@]"
    pp_rounded_idents ts pp_expression e;
  pp_dual_handler pp_statement
    ~on:Fmt.(any "ON SIZE ERROR") ~close:Fmt.(any "END-COMPUTE")
    ppf dh;
  Fmt.pf ppf "@]"

(* DELETE *)

and pp_delete_stmt ppf { delete_targets; delete_retry; delete_on_invalid_key} =
  Fmt.pf ppf "@[DELETE@;<1 2>@[%a%a@]"
    pp_name' delete_targets
    Fmt.(option (sp ++ pp_retry_clause)) delete_retry;
  pp_dual_handler pp_statement
    ~on:Fmt.(any "INVALID KEY") ~close:Fmt.(any "END-DELETE")
    ppf delete_on_invalid_key;
  Fmt.pf ppf "@]";

(* DIVIDE *)

and pp_divide_stmt ppf { divide_operands; divide_on_size_error = dh } =
  pp_arithmetic_stmt "DIVIDE" pp_divide_operands pp_statement
    ppf (divide_operands, dh)

(* MULTIPLY *)

and pp_multiply_stmt ppf { multiply_operands; multiply_on_size_error = dh } =
  pp_arithmetic_stmt "MULTIPLY" pp_multiply_operands pp_statement
    ppf (multiply_operands, dh)

(* READ *)

and pp_read_stmt ppf {
  read_file = rf; read_direction = rd; read_into = ri;
  read_lock_behavior = rlb; read_lock = rl; read_key = rk;
  read_error_handler = reh }
=
  let pp_with_lock ppf b =
    if b then Fmt.pf ppf "LOCK" else Fmt.pf ppf "NO LOCK"
  in
  let pp_read_header ppf (rf, rd, ri, rlb, b, k) =
    pp_name' ppf rf;
    Fmt.(option (sp ++ pp_read_direction)) ppf rd;
    Fmt.(option (any "@ INTO " ++ pp_ident)) ppf ri;
    Fmt.(option (sp ++ pp_read_lock_behavior)) ppf rlb;
    Fmt.(option (sp ++ pp_with_lock)) ppf b;
    Fmt.(option (sp ++ any "KEY " ++ pp_qualname)) ppf k
  in
  let pos = Option.map (fun (re, _) -> Fmt.const pp_read_error re) reh in
  let dh = Option.map snd reh in
  pp_branching_stmt "READ" ?pos pp_read_header pp_statement ppf
    ((rf, rd, ri, rlb, rl, rk), [], dh)

(* RECEIVE *)

and pp_receive_stmt ppf {
  receive_name = rn; receive_kind = rk; receive_into = ri;
  receive_on_no_data = dh }
=
  let pp_receive_header ppf (rn, rk, ri) =
    Fmt.pf ppf "%a@ %a@ INTO %a"
      pp_name' rn pp_mcs_awaiting_item rk pp_ident ri
  in
  pp_branching_stmt "RECEIVE" pp_receive_header pp_statement ppf
    ~pos:Fmt.(any "NO DATA") ~neg:Fmt.(any "DATA")
    ((rn, rk, ri), [], Some dh)

(* RETURN *)

and pp_return_stmt ppf
  { return_file = rf; return_into = ri; return_at_end = dh }
=
  let pp_return_header ppf (rn, ri) =
    pp_name' ppf rn;
    Fmt.(option (sp ++ any "INTO " ++ pp_with_loc pp_ident)) ppf ri
  in
  pp_branching_stmt "RETURN" pp_return_header pp_statement ppf
    ~pos:Fmt.(any "AT END") ((rf, ri), [], Some dh)

(* REWRITE *)

and pp_rewrite_stmt ppf {
  rewrite_to = rt; rewrite_from = rf; rewrite_retry = rr;
  rewrite_lock = rl; rewrite_invalid_key_handler = dh }
=
  let pp_with_lock ppf b =
    if b then Fmt.pf ppf "LOCK" else Fmt.pf ppf "NO LOCK"
  in
  let pp_rewrite_header ppf (rt, rf, rr, rl) =
    pp_write_target ppf rt;
    Fmt.(option (sp ++ any "FROM " ++ pp_ident_or_literal)) ppf rf;
    Fmt.(option (sp ++ pp_retry_clause)) ppf rr;
    Fmt.(option (sp ++ pp_with_lock)) ppf rl
  in
  pp_branching_stmt "REWRITE" pp_rewrite_header pp_statement ppf
    ~pos:Fmt.(any "INVALID KEY") ((rt, rf, rr, rl), [], Some dh)


(* START *)

and pp_start_stmt ppf
  { start_file = sf; start_position = sp; start_on_invalid_key = dh }
=
  let pp_start_header ppf (sf, sp) =
    pp_name' ppf sf;
    Fmt.(option (sp ++ pp_start_position)) ppf sp
  in
  pp_branching_stmt "START" pp_start_header pp_statement ppf
    ~pos:Fmt.(any "INVALID KEY") ((sf, sp), [], Some dh)


(* STRING *)

and pp_string_stmt ppf {
  string_sources = ss; string_target = st; string_pointer = sp;
  string_on_overflow = dh }
=
  let pp_string_header ppf (ss, st) =
    Fmt.pf ppf "%a@ INTO@ %a"
      Fmt.(list ~sep:sp pp_string_source) ss
      pp_ident st
  in
  pp_branching_stmt "STRING" pp_string_header pp_statement ppf
    ~pos:Fmt.(any "ON OVERFLOW") ((ss, st), (
      opt_clause Fmt.(any "POINTER " ++ pp_ident) sp
    ), Some dh)

(* UNSTRING *)

and pp_unstring_stmt ppf {
  unstring_source = us; unstring_delimiters = uds; unstring_targets = uts;
  unstring_pointer = up; unstring_tallying = ut; unstring_on_overflow = dh }
=
  pp_branching_stmt "UNSTRING" pp_ident pp_statement ppf
  ~pos:Fmt.(any "ON OVERFLOW") (us, (
    list_clause Fmt.(
      any "DELIMITED BY " ++ list ~sep:(any "OR@ ") pp_unstring_delimiter
    ) uds @
    list_clause Fmt.(any "INTO " ++ list ~sep:sp pp_unstring_target) uts @
    opt_clause Fmt.(any "POINTER " ++ pp_ident) up @
    opt_clause Fmt.(any "TALLYING " ++ pp_ident) ut
  ), Some dh)

(* WRITE *)

and pp_write_stmt ppf
  { write_to; write_from; write_advancing; write_retry; write_lock;
    write_error_handler = weh }
=
  let pos = Option.map (fun (we, _) -> Fmt.const pp_write_error we) weh in
  let dh = Option.map snd weh in
  pp_branching_stmt "WRITE" ?pos pp_write_target pp_statement ppf
    (write_to, (
      opt_clause Fmt.(any "FROM@ " ++ pp_ident_or_literal) write_from @
      opt_clause pp_advancing_phrase write_advancing @
      opt_clause pp_retry_clause write_retry @
      opt_clause
        Fmt.((fun ppf b -> if not b then pf ppf "NO ") ++ any "LOCK")
        write_lock
    ), dh)

and pp_statement ppf = function
  | Accept s -> pp_accept_stmt ppf s
  | Add s -> pp_basic_arithmetic_stmt ~sep:"TO" "ADD" ppf s
  | Allocate s -> pp_allocate_stmt ppf s
  | Alter s -> pp_alter_stmt ppf s
  | Call s -> pp_call_stmt ppf s
  | Cancel xs -> Fmt.pf ppf "CANCEL %a" Fmt.(list ~sep:sp pp_ident_or_strlit) xs
  | Close s -> pp_close_stmt ppf s
  | Compute s -> pp_compute_stmt ppf s
  | Continue -> Fmt.pf ppf "CONTINUE"
  | Delete s -> pp_delete_stmt ppf s
  | Disable ops -> Fmt.pf ppf "DISABLE %a" pp_mcs_command_operands ops
  | Display s -> pp_display_stmt ppf s
  | Divide s -> pp_divide_stmt ppf s
  | Enable ops -> Fmt.pf ppf "ENABLE %a" pp_mcs_command_operands ops
  | Enter s -> pp_enter_stmt ppf s
  | Entry s -> pp_entry_stmt ppf s
  | Evaluate s -> pp_evaluate_stmt ppf s
  | ExecBlock b -> Cobol_common.Exec_block.pp ppf b
  | Exit s -> pp_exit_stmt ppf s
  | Free names ->
      Fmt.pf ppf "FREE@ @[%a@]" Fmt.(list ~sep:sp (pp_with_loc pp_name)) names
  | Generate name ->
      Fmt.pf ppf "GENERATE@ %a" (pp_with_loc pp_name) name
  | GoTo s -> pp_goto_stmt ppf s
  | GoBack s ->
    Fmt.pf ppf "GOBACK%a"
      Fmt.(option (sp ++ pp_raising)) s.goback_raising;
    Option.iter (Fmt.pf ppf "@ %a" (pp_with_loc pp_ident_or_intlit))
      s.goback_returning
  | If s -> pp_if_stmt ppf s
  | Initialize s -> pp_initialize_stmt ppf s
  | Initiate ns ->
      Fmt.pf ppf "INITIATE@ %a" Fmt.(list ~sep:sp (pp_with_loc pp_name)) ns
  | Inspect s -> pp_inspect_stmt ppf s
  | Invoke s -> pp_invoke_stmt ppf s
  | LoneGoTo -> Fmt.pf ppf "GO TO"
  | Merge s -> pp_merge_stmt ppf s
  | Move s -> pp_move_stmt ppf s
  | Multiply s -> pp_multiply_stmt ppf s
  | NextSentence -> Fmt.pf ppf "NEXT SENTENCE"
  | Open s -> pp_open_stmt ppf s
  | PerformInline s -> pp_perform_inline_stmt ppf s
  | PerformTarget s -> pp_perform_target_stmt ppf s
  | Purge n -> Fmt.pf ppf "PURGE %a" (pp_with_loc pp_name) n
  | Raise ro -> pp_raise_operand ppf ro
  | Read s -> pp_read_stmt ppf s
  | Receive s -> pp_receive_stmt ppf s
  | Release s -> pp_release_stmt ppf s
  | Resume s -> pp_resume_stmt ppf s
  | Return s -> pp_return_stmt ppf s
  | Rewrite s -> pp_rewrite_stmt ppf s
  | Search s -> pp_search_stmt ppf s
  | SearchAll s -> pp_search_all_stmt ppf s
  | Send s -> pp_send_stmt ppf s
  | Set s -> pp_set_stmt ppf s
  | Sort s -> pp_sort_stmt ppf s
  | Start s -> pp_start_stmt ppf s
  | Stop s -> pp_stop_stmt ppf s
  | String s -> pp_string_stmt ppf s
  | Subtract s -> pp_basic_arithmetic_stmt ~sep:"FROM" "SUBTRACT" ppf s
  | Suppress -> Fmt.pf ppf "SUPPRESS PRINTING"
  | Terminate s -> pp_terminate_stmt ppf s
  | Transform s -> pp_transform_stmt ppf s
  | Unlock s -> pp_unlock_stmt ppf s
  | Unstring s -> pp_unstring_stmt ppf s
  | Validate xs -> Fmt.pf ppf "VALIDATE %a" Fmt.(list ~sep:sp pp_ident) xs
  | Write s -> pp_write_stmt ppf s

and pp_statements ppf =
  Fmt.(vbox @@ list ~sep:sp (box (pp_with_loc pp_statement))) ppf

let pp_dump_statements = pp_statements
