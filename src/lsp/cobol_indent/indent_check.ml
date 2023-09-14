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

open Cobol_common.Srcloc
open Cobol_common.Basics

open Indent_type
open Indent_keywords
open Indent_util

(*
	Remark:
	In general, `check_{scope-name}` handles the tokens of that scope,
	and will call a child "check function" to check its child scope in some cases,
	and go back to its parent "check function" when this scope is terminated.

	ex.
	`check_data_div` will call `check_RD` if it encounters a token "RD".
  When the RD is terminated by a ".", it will call the `check_data_div`.
	i.e.
  When `check_data_div` is called, and if encounter "RD",
  the `check_data_div` handle this token, update the arguments
  and call `check_RD`.
  When inside RD, encounter a ".", handle this ".", update the
  arguments and go back to `check_data_div`.

	In general, the `check_{scope-name}` does not treat the token which opens the scope itself.
	However, for the reason of brevity (and scope inference for incomplete code),
	the `check_ident_div` handles the token `IDENT_DIV` itself
	... `check_env_div`   ................. `ENV_DIV`   ......
	... `check_data_div`  ................. `DATA_DIV`  ......
	... `check_proc_div_header` ........... `PROC_DIV`  ......
	... `check_copy_replace` .............. `COPY/REPLACE` ...

*)

type text = Cobol_preproc.Text.t

let rec check_ident_div (text:text) (state:indent_state) (ifcheck:bool) =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload = TextWord "IDENTIFICATION"; loc} :: {payload = TextWord "DIVISION"; _}
    :: {payload = TextWord "."; _} :: wordlist ->
      let context = pop_until_division context in
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      let context = push_context COMPILATION_UNIT context in
      let context = push_context IDENT_DIV context in
      check_ident_div wordlist {state with context; acc} false

  (*TODO:Careful check for method/interface/factory/function....
         if any difference (of indentation) between these compilation unit *)
 | {payload = TextWord ("PROGRAM-ID"|"CLASS-ID"|"FACTORY"|"FUCNTION-ID"
                        |"INTERFACE-ID"|"METHOD-ID"|"OBJECT"); loc}
    :: {payload = TextWord "."; _} :: wordlist ->
      let context = pop_until_division context in
      begin match context with
      | (IDENT_DIV, _) :: (COMPILATION_UNIT, offset) :: _ ->
          let acc = check_pos loc offset acc ifcheck in
          check_ident_div wordlist {state with context; acc} false
      | _ ->
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          let context = push_context COMPILATION_UNIT context in
          let context = push_context IDENT_DIV context in
          check_ident_div wordlist {state with context; acc} false
      end

  | {payload = TextWord "OPTIONS"; loc}
    :: {payload = TextWord "."; _} :: wordlist ->
      let context = pop_until COMPILATION_UNIT context in
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      let context = push_context PARAGRAPH context in
      check_ident_div wordlist {state with context; acc} false

  (*jump to ENVIRONMENT DIVISION*)
  | {payload = TextWord "ENVIRONMENT"; _} :: {payload = TextWord "DIVISION"; _} :: _ ->
      check_env_div text {state with scope = ENV_DIV} ifcheck
  (*jump to DATA DIVISION*)
  | {payload = TextWord "DATA"; _} :: {payload = TextWord "DIVISION"; _} :: _ ->
      check_data_div text {state with scope = DATA_DIV} ifcheck
  (*jump to PROCEDURE DIVISION*)
  | {payload = TextWord "PROCEDURE"; _} :: {payload = TextWord "DIVISION"; _} :: _ ->
      check_proc_div_header text {state with scope = PROC_DIV_HEADER} ifcheck

  (*end compilation_unit*)
  | {payload = TextWord "END"; loc }
    :: {payload = TextWord ("PROGRAM"|"CLASS"|"FACTORY"|"FUNCTION"
                           |"OBJECT"|"METHOD"|"INTERFACE"); _} :: wordlist ->
      end_compilation_unit loc wordlist state ifcheck

  | {payload;loc} :: wordlist as _text ->
    begin match payload with
    | CDirWord _ ->
        check_ident_div wordlist state false
    | TextWord "." ->
        let context = handle_period context in
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_ident_div wordlist {state with context; acc} false
   | TextWord ("COPY"|"REPLACE") ->
        check_copy_replace text {state with scope = COPY_REPLACE} ifcheck
    | _ ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_ident_div wordlist {state with context; acc} false
    end


(*************ENVIRONMENT DIVISION****************)
(*TODO:Add check of clause in ENV DIVISION if need be*)
and check_env_div (text:text)  (state:indent_state) (ifcheck:bool)  =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload = TextWord "ENVIRONMENT"; loc} :: {payload = TextWord "DIVISION"; _} :: wordlist ->
      let context = pop_until_compilation_unit context in
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      let context = push_context ENV_DIV context in
      check_env_div wordlist {state with context; acc} false
  | {payload = TextWord _; loc} :: {payload = TextWord "SECTION"; _}
    :: {payload = TextWord "."; _} :: wordlist ->
      let context = pop_until SECTION context in
      let context =
        match context with
        | (SECTION, _) :: context
        | context -> context
      in
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      let context = push_context SECTION context in
      check_env_div wordlist {state with context; acc} false
  | {payload = TextWord ("SOURCE-COMPUTER"|"OBJECT-COMPUTER"|"SPECIAL-NAMES"|"REPOSITORY"
                        |"FILE-CONTROL"|"I-O-CONTROL"); loc}
    :: {payload = TextWord "."; _} :: wordlist ->
      let context = pop_until SECTION context in
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      let context = push_context PARAGRAPH context in
      check_env_div wordlist {state with context; acc} false
  (*jump to other division*)
  | {payload = TextWord "IDENTIFICATION"; _} :: {payload = TextWord "DIVISION"; _} :: _
  | {payload = TextWord ("PROGRAM-ID"|"CLASS-ID"|"FACTORY"|"FUCNTION-ID"
                        |"INTERFACE-ID"|"METHOD-ID"|"OBJECT"); _}
                        :: {payload = TextWord "."; _} :: _ ->
      check_ident_div text {state with scope = IDENT_DIV} ifcheck
  | {payload = TextWord "DATA"; _} :: {payload = TextWord "DIVISION"; _} :: _ ->
      check_data_div text {state with scope = DATA_DIV} ifcheck
  | {payload = TextWord "PROCEDURE"; _} :: {payload = TextWord "DIVISION"; _} :: _ ->
      check_proc_div_header text {state with scope = PROC_DIV_HEADER} ifcheck

  (*end compilation_unit*)
  | {payload = TextWord "END"; loc }
    :: {payload = TextWord ("PROGRAM"|"CLASS"|"FACTORY"|"FUNCTION"
                           |"OBJECT"|"METHOD"|"INTERFACE"); _} :: wordlist ->
      end_compilation_unit loc wordlist state ifcheck

  | {payload; loc} :: wordlist as _text->
    begin
    match payload with
    | CDirWord _ ->
        check_env_div wordlist state false
    | TextWord ("COPY"|"REPLACE") ->
        check_copy_replace text {state with scope = COPY_REPLACE} ifcheck
    | TextWord "SELECT" ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        let context = push_context SELECT context in
        check_env_div wordlist {state with context; acc} false
    | TextWord "." ->
        let context = handle_period context in
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_env_div wordlist {state with context; acc} false
    | _ ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_env_div wordlist {state with context; acc} false
    end

(*************DATA DIVISION****************)
(*TODO: Refine the check of clause of DATA DIVISION*)
and check_data_div (text:text) (state:indent_state) ifcheck  =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload = TextWord "DATA"; loc } :: {payload = TextWord "DIVISION"; _}
    :: wordlist ->
      let context = pop_until_compilation_unit context in
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      let context = push_context DATA_DIV context in
      check_data_div wordlist {state with context; acc} false
  | {payload = TextWord _; loc} :: {payload = TextWord "SECTION"; _} :: wordlist ->
      let context = pop_until SECTION context in
      let context =
        match context with
        | (SECTION, _) :: context
        | context -> context
      in
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      let context = push_context SECTION context in
      check_data_div wordlist {state with context; acc} false
  | {payload = TextWord "IDENTIFICATION"; _} :: {payload = TextWord "DIVISION"; _} :: _
  | {payload = TextWord ("PROGRAM-ID"|"CLASS-ID"|"FACTORY"|"FUCNTION-ID"
                        |"INTERFACE-ID"|"METHOD-ID"|"OBJECT"); _}
                        :: {payload = TextWord "."; _} :: _ ->
      check_ident_div text {state with scope = IDENT_DIV} ifcheck
  | {payload = TextWord "ENVIRONMENT"; _} :: {payload = TextWord "DIVISION"; _} :: _ ->
      check_proc_div text {state with scope = ENV_DIV} ifcheck
  | {payload = TextWord "PROCEDURE"; _} :: {payload = TextWord "DIVISION"; _} :: _ ->
      check_proc_div_header text {state with scope = PROC_DIV_HEADER} ifcheck
  (*end compilation_unit*)
  | {payload = TextWord "END"; loc }
    :: {payload = TextWord ("PROGRAM"|"CLASS"|"FACTORY"|"FUNCTION"
                           |"OBJECT"|"METHOD"|"INTERFACE"); _} :: wordlist ->
      end_compilation_unit loc wordlist state ifcheck

  | {payload; loc} :: wordlist as _text ->
    match payload with
    | CDirWord _ ->
        check_data_div wordlist state false
    | TextWord word ->
      begin match data_context_of_str word with
      | Compiler_directive _ ->
          check_copy_replace text {state with scope = COPY_REPLACE} ifcheck
      | Entry (FD|RD|SD as key) ->
          let context = pop_until SECTION context in
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          let context = push_context key context in
          check_fun key wordlist {state with scope = key; context; acc} false
      (*Data declaration*)
      (*77-level data description entry*)
      | Entry (LEVEL 77) ->
          let context = pop_until SECTION context in
          let context = push_context DATA_DESC context in
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          check_data_desc wordlist {state with scope = DATA_DESC; context; acc} false
      (*rename clause*)
      | Entry (LEVEL 66) ->
          let context = pop_until (LEVEL 1) context in
          let context = push_context DATA_DESC context in
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          check_data_desc wordlist {state with scope = DATA_DESC; context; acc} false
      | Entry (LEVEL level as key) ->
          let context = reduce_level level context in
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          let context = push_context key context in
          let context = push_context DATA_DESC context in
          check_data_desc wordlist {state with scope = DATA_DESC; context; acc} false
      | PERIOD ->
          let context = handle_period context in
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          check_data_div wordlist {state with context; acc} false
      | No_keyword ->
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          check_data_div wordlist {state with context; acc} false
      | _ -> failwith @@ failure_msg loc
      end
    | _ ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_data_div wordlist {state with context; acc} false

and check_data_div_entry clauses key (text:text) state ifcheck =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload; loc} :: wordlist as _text ->
    match payload with
    | CDirWord _ ->
        check_data_div_entry clauses key wordlist state ifcheck
    | TextWord ("COPY"|"REPLACE") ->
        check_copy_replace text {state with scope = COPY_REPLACE} ifcheck
    | TextWord str when StringSet.mem str clauses ->
        let context = pop_until key context in
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        let context = push_context DATA_DIV_CLAUSE context in
        check_data_div_entry clauses key wordlist {state with context; acc} false
    | TextWord "." ->
        let context = handle_period context in
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_data_div wordlist {state with scope = DATA_DIV; context; acc} false
    | _ ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_data_div_entry clauses key wordlist {state with acc} false

and check_FD text state ifcheck = check_data_div_entry
  (StringSet.of_list ["EXTERNAL"; "GLOBAL"; "FORMAT"; "BLOCK"; "CODE-SET"; "RECORD"; "LINAGE"])
  FD text state ifcheck

and check_RD text state ifcheck = check_data_div_entry
  (StringSet.of_list ["GLOBAL"; "CODE"; "CONTROL"; "CONTROLS"; "PAGE"])
  RD text state ifcheck

and check_SD text state ifcheck =
  check_data_div_entry (StringSet.of_list ["RECORD"]) SD text state ifcheck

(*TODO: if necessary, distinguish data description with screen description*)
and check_data_desc text state ifcheck =
  check_data_div_entry
  (StringSet.of_list
   [(*keyword of data description entry*)
    "REDEFINES"; "TYPEDES"; "ALIGNED"; "ANY"; "BASED"; "BLANK"; "CONSTANT"; "DYNAMIC";
    "EXTERNAL"; "GLOBAL"; "GROUP-USAGE"; "JUST"; "JUSTIFIED"; "OCCURS"; "PIC"; "PICTURE";
    "PROPERTY"; "SAME"; "SELECT"; "SIGN"; "LEADING"; "TAILING"; "SYNCHRONIZED"; "SYNC";
    "TYPE"; "CLASS"; "DEFAULT"; "DESTINATION"; "INVALID"; "PRESENT";
    "VARYING"; "VALIDATE-STATUS"; "VAL-STATUS"; "VALUE"; "VALUES";
    (*keyword of screen description entry*)
    "LINE"; "COL"; "COLUMN"; "ERASE"; "FULL"; "AUTO"; "SECURE"; "REQUIRED"; "DISPLAY";
    "NATIONAL"; "BELL"; "BLINK"; "HIGHTLIGHT"; "LOWLIGHT"; "REVERSE-VIDEO"; "UNDERLINE";
    "FOREGROUND-COLOR"; "BACKGROUND-COLOR"; "USING"
    (*TODO: The clauses FROM, TO are omitted here, since they can appear inside other clauses,
            which makes the problem a little complex.*)
    ])
  DATA_DESC text state ifcheck


(*************PROCEDURE DIVISION****************)
and check_proc_div_header (text:text) (state:indent_state) ifcheck =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload = TextWord "PROCEDURE"; loc} :: {payload = TextWord "DIVISION"; _} :: wordlist
    ->
    let context = pop_until_compilation_unit context in
    let acc = check_pos loc (offset_of_context context) acc ifcheck in
    let context = push_context PROC_DIV context in
    let context = push_context PROC_DIV_HEADER context in
    check_proc_div_header wordlist {state with context; acc} false
  | {payload; loc} :: wordlist ->
    match payload with
    | CDirWord _ ->
        check_proc_div_header wordlist state ifcheck
    | TextWord word ->
      begin match proc_context_of_str word with
      | Compiler_directive _ ->
          check_copy_replace text {state with scope = COPY_REPLACE} ifcheck
      | No_keyword ->
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          check_proc_div_header wordlist {state with acc} false
      | Phrase (USING|RAISING|BY as key) ->
          handle_phrase key loc wordlist state ifcheck
      | Inline_phrase ->
          handle_inline_phrase loc wordlist state ifcheck
      (*PERIOD here means the real beginning of the procedure divisoin*)
      | PERIOD ->
          let context = handle_period context in
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          (*for better indentation, we suppose that when procedure division begins,
            there is an implicit paragraph just after the procedure division. *)
          let context = push_context PARAGRAPH context in
          check_proc_div wordlist {state with scope = PROC_DIV; context; acc} false
      | _ ->
          failwith @@ failure_msg loc
      end
    | _  ->
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      check_proc_div_header wordlist {state with context; acc} false

and check_proc_div (text:text) (state:indent_state) ifcheck   =
  let context, acc = state.context, state.acc in
  match text with
  | {payload = TextWord "IDENTIFICATION"; _} :: {payload = TextWord "DIVISION"; _}
    :: _
  | {payload = TextWord ("PROGRAM-ID"|"CLASS-ID"|"FACTORY"|"FUCNTION-ID"
                        |"INTERFACE-ID"|"METHOD-ID"|"OBJECT"); _}
                        :: {payload = TextWord "."; _} :: _ ->
      check_ident_div text {state with scope = IDENT_DIV} ifcheck

  | {payload = TextWord "DECLARATIVES"; loc} :: {payload = TextWord "."; _} :: wordlist ->
      let context = pop_until PROC_DIV context in
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      let context = push_context DECLARATIVES context in
      let context = push_context PARAGRAPH context in
      check_proc_div wordlist {state with context; acc} false
  | {payload = TextWord "END"; loc} :: {payload = TextWord "DECLARATIVES"; _}
    :: {payload = TextWord "."; _}:: wordlist ->
      let context = pop_until DECLARATIVES context in
      begin match context with
      | (DECLARATIVES, _) :: context ->
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          (*for better indentation, we suppose that when procedure division begins,
            there is an implicit paragraph just after the procedure division. *)
          let context = push_context PARAGRAPH context in
          check_proc_div wordlist {state with context; acc} false
      | _ -> failwith @@ failure_msg loc end

  | {payload = TextWord name; loc} :: {payload = TextWord "SECTION"; _}
    :: {payload = TextWord "."; _} :: wordlist when not @@ is_statement name ->
      let context =
        match context with
        | (PARAGRAPH, _) :: (SECTION, _) :: context
        | (PARAGRAPH, _) :: context
        | context -> context
      in
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      let context = push_context SECTION context in
      let context = push_context PARAGRAPH context in
      check_proc_div wordlist {state with context; acc} false
  | {payload = TextWord name; loc} :: {payload = TextWord "."; _} :: wordlist
    when not @@ is_statement name ->
      let context =
        match context with
        | (PARAGRAPH, _) :: context -> context
        | _ -> context
      in
      let offset = offset_of_context context in
      let acc = check_pos loc offset acc ifcheck in
      let context = push_context PARAGRAPH context in
      check_proc_div wordlist {state with context; acc} false
  (*end compilation_unit*)
  | {payload = TextWord "END"; loc }
    :: {payload = TextWord ("PROGRAM"|"CLASS"|"FACTORY"|"FUNCTION"
                           |"OBJECT"|"METHOD"|"INTERFACE"); _} :: wordlist ->
      end_compilation_unit loc wordlist state ifcheck

  (*TODO: find a better way to distinguish PERFORM(inline) and PERFORM_CLOSED(out-of-line)*)
  (*A bug here, TODO.md for details*)
  | {payload = TextWord "PERFORM"; loc } :: _ :: {payload = TextWord "TIMES"; _} :: wordlist
  | {payload = TextWord "PERFORM"; loc } :: {payload = TextWord ("UNTIL"|"VARYING"|"WITH"|"TEST"); _}
    :: wordlist ->
      handle_open_scope PERFORM loc wordlist state ifcheck

  | {payload; loc} :: wordlist ->
    begin match payload with
    | CDirWord _ ->
        check_proc_div wordlist state ifcheck
    | TextWord word ->
      begin match proc_context_of_str word with
      | Compiler_directive _ ->
          check_copy_replace text {state with scope = COPY_REPLACE} ifcheck
      | Open_scope keyword ->
          handle_open_scope keyword loc wordlist state ifcheck
      | _ ->
          (* if all keywords of statements are implemented, this case never happens*)
          (*failwith ("error proc_div: "^ word)*)
          let offset = offset_of_context context in
          let acc = check_pos loc offset acc ifcheck in
          check_proc_div wordlist {state with acc} false
      end
    | _  ->
      let offset = offset_of_context context in
      let acc = check_pos loc offset acc ifcheck in
      check_proc_div wordlist {state with acc} false
    end
  | [] -> state


(* General statement, handles "common tokens" which can follow any statement *)
(* Notice that we cannot know when a statement terminates in general*)
(* When the code is in a statement not explicitly terminated,
   the next token can be
     1. keyword of other statement, "MOVE/ADD/DISPLAY..."
     2. "when" "else" "on size error" ... (it means the statement is nested)
     3. keyword inside the statement . ex. phrase/clause
     4. "END-xxx"
     5. PEROID *)
(*
  `check_statement` should be regarded as an auxiliary function,
  `check_statement` does not recursively call itself directly.

  At the begin, I define this function with the intention of reducing the duplicate code,
  because inside any statement, there are a lot of "common token" to treat,
  especially "NOT ON SIZE ERROR..." which are too long, hard to write.
  TODO: maybe we had better do some preprocess,
    ex. {TextWord "ON";loc} :: {TextWord "SIZE";_} :: {TextWord "ERROR";_} :: ...
        -> {TextWord "ON_SIZE_ERROR";loc} :: ...

  The `check_statement` is called by other functions like `check_if_stmt`,
  look at how `check_if_stmt` works:
      If the current token is a statement-specific token (like `THEN`)
        (or "common token" easy to write like `PERIOD`, `No_keyword`... ),
        treat the current token and call recursively itself,
     	Otherwise
   	    call `check_statement`.
*)
and check_statement (text:text) state ifcheck  =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  (*TODO:
      Maybe do the preprocessing to link some successive keywords.
      ex. "ON" "SIZE" "ERROR" -> "ON_SIZE_ERROR"
      (the code will be more brief if we do this)*)
  | {payload = TextWord "ON"; loc } :: {payload = TextWord "SIZE"; _}
    :: {payload = TextWord "ERROR"; _} :: wordlist
  | {payload = TextWord "SIZE"; loc} :: {payload = TextWord "ERROR"; _} :: wordlist ->
      handle_conditional_statement loc ON_SIZE_ERROR wordlist state ifcheck
  | {payload = TextWord "NOT"; loc } :: {payload = TextWord "ON"; _}
    :: {payload = TextWord "SIZE"; _} :: {payload = TextWord "ERROR"; _}:: wordlist
  | {payload = TextWord "NOT"; loc }
    :: {payload = TextWord "SIZE"; _} :: {payload = TextWord "ERROR"; _}:: wordlist ->
      handle_conditional_statement loc NOT_ON_SIZE_ERROR wordlist state ifcheck
  | {payload = TextWord "AT"; loc} :: {payload = TextWord "END"; _} :: wordlist
  | {payload = TextWord "END"; loc} :: wordlist ->
      handle_conditional_statement loc AT_END wordlist state ifcheck
  | {payload = TextWord "NOT"; loc} :: {payload = TextWord "AT"; _}
    :: {payload = TextWord "END"; _} :: wordlist
  | {payload = TextWord "NOT"; loc} :: {payload = TextWord "END"; _} :: wordlist ->
      handle_conditional_statement loc NOT_AT_END wordlist state ifcheck
  | {payload = TextWord "ON"; loc} :: {payload = TextWord "EXCEPTION"; _} :: wordlist
  (* TODO: Find a better way to solve the keyword conflict "(ON)EXCEPTION".
           the "ON EXCEPTION" can be simplified to "EXCEPTION",
           but "EXCEPTION" can appear inside some statements(RAISE/GOBACK/USE)
           I use a quite strange way to solve the problem (cannot be generalized)
           See `check_raise_stmt` `check_goback_stmt` `check_use_stmt`
  *)
  | {payload = TextWord "EXCEPTION"; loc} :: wordlist ->
      handle_conditional_statement loc ON_EXCEPTION wordlist state ifcheck
  | {payload = TextWord "NOT"; loc} :: {payload = TextWord "ON"; _}
    :: {payload = TextWord "EXCEPTION"; _} :: wordlist
  | {payload = TextWord "NOT"; loc}
    :: {payload = TextWord "EXCEPTION"; _} :: wordlist ->
      handle_conditional_statement loc NOT_ON_EXCEPTION wordlist state ifcheck
  | {payload = TextWord "INVALID"; loc } :: {payload = TextWord "KEY"; _} :: wordlist
  | {payload = TextWord "INVALID"; loc } :: wordlist ->
      handle_conditional_statement loc INVALID_KEY wordlist state ifcheck
  | {payload = TextWord "NOT"; loc } :: {payload = TextWord "INVALID"; _}
    :: {payload = TextWord "KEY"; _} :: wordlist
  | {payload = TextWord "NOT"; loc } :: {payload = TextWord "INVALID"; _}
    :: wordlist ->
      handle_conditional_statement loc NOT_INVALID_KEY wordlist state ifcheck
  | {payload = TextWord "AT"; loc } :: {payload = TextWord ("END-OF-PAGE"|"EOP"); _} :: wordlist
  | {payload = TextWord ("END-OF-PAGE"|"EOP"); loc} :: wordlist ->
      handle_conditional_statement loc AT_END_OF_PAGE wordlist state ifcheck
  | {payload = TextWord "NOT"; loc} :: {payload = TextWord "AT"; _}
    :: {payload = TextWord ("END-OF-PAGE"|"EOP"); _} :: wordlist
  | {payload = TextWord "NOT"; loc}
    :: {payload = TextWord ("END-OF-PAGE"|"EOP"); _} :: wordlist ->
      handle_conditional_statement loc NOT_AT_END_OF_PAGE wordlist state ifcheck
  | {payload = TextWord "ON"; loc } :: {payload = TextWord "OVERFLOW"; _} :: wordlist
  | {payload = TextWord "OVERFLOW"; loc} :: wordlist ->
      handle_conditional_statement loc ON_OVERFLOW wordlist state ifcheck
  | {payload = TextWord "NOT"; loc } :: {payload = TextWord "ON"; _}
    :: {payload = TextWord "OVERFLOW"; _} :: wordlist
  | {payload = TextWord "NOT"; loc }
    :: {payload = TextWord "OVERFLOW"; _} :: wordlist ->
      handle_conditional_statement loc NOT_ON_OVERFLOW wordlist state ifcheck

  (*TODO: find a better way to distinguish PERFORM(inline) and PERFORM_CLOSED(out-of-line)*)
  (*A bug here, TODO.md for details*)
  | {payload = TextWord "PERFORM"; loc } :: _ :: {payload = TextWord "TIMES"; _} :: wordlist
  | {payload = TextWord "PERFORM"; loc } :: {payload = TextWord ("UNTIL"|"VARYING"|"WITH"|"TEST"); _}
    :: wordlist ->
      handle_open_scope PERFORM loc wordlist state ifcheck

  | {payload = TextWord "ELSE"; loc} :: {payload = TextWord "IF"; _} :: wordlist ->
      let context = exp_scope_termination THEN context in
      begin match context with
      | (THEN, _) :: (IF, _) :: context' ->
          let acc = check_pos loc (offset_of_context context') acc ifcheck in
          check_if_stmt wordlist {state with scope = IF; context; acc} false
      | _ -> failwith @@ failure_msg loc
      end
(*
  | {payload = TextWord "NEXT"; loc} :: {payload = TextWord "SENTENCE"; _} :: wordlist

  The GnuCOBOL handles NEXT SENTENCE differently than described in the standard of COBOL.
  According the standard 1985/1989:2014
  ```
    IF x > 1 THEN NEXT SENTENCE
    DISPLAY "STH" .
  ```
  This DISPLAY statement should be the next statement of IF statement.
  But the result of GnuCOBOL shows that
    the DISPLAY statement is inside the THEN phrase(even though it is never reached).
  However, I am not sure since I cannot test the case in the mainframe.

  We must care about this case. Because the NEXT SENTENCE in fact terminates the
  Then branch of IF statement, so this IF statement can be implicitly terminated.  *)

  | {payload; loc} :: wordlist ->
     match payload with
    | CDirWord _ ->
      check_fun state.scope wordlist state ifcheck
    | TextWord word ->
      begin match proc_context_of_str word with
      | Compiler_directive _ ->
          check_copy_replace text {state with scope = COPY_REPLACE} ifcheck
      | Close_scope keyword ->
          handle_close_scope keyword loc wordlist state ifcheck
      | Open_scope keyword ->
          handle_open_scope keyword loc wordlist state ifcheck

      | Phrase THEN -> (*only for THEN inside INTIALIZE statement*)
          handle_inline_phrase loc wordlist state ifcheck

      | Phrase keyword ->
          handle_phrase keyword loc wordlist state ifcheck
      | Inline_phrase ->
          handle_inline_phrase loc wordlist state ifcheck
      | PERIOD ->
          let context = handle_period context in
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          let scope = match context with
            (*this case appears when indent a incomplet code, which begins directly with statement*)
            (*In general we avoid defining `state.scope` to `STATEMENT`,
              it is better to define `state.scope` to a specific statement like `IF`, `ADD`.
              `check_statement` is an auxiliary function *)
            | (BEGIN, _) :: _ -> STATEMENT
            | (scope, _) :: _ -> scope
            | [] -> failwith @@ failure_msg loc
          in
          check_proc_div wordlist {state with scope; context; acc} false
      | No_keyword
      | _ ->
          let offset = offset_of_context context in
          let acc = check_pos loc offset acc ifcheck in
          check_fun state.scope wordlist {state with acc} false
      end
    | _ ->
      let offset = offset_of_context context in
      let acc = check_pos loc offset acc ifcheck in
      check_fun state.scope wordlist {state with acc} false

(* if in need, we can seperate some statements from this general statement.
  ex. the `check_if_stmt` below

    in `check_if_stmt`, and `check_statement`, we handle `THEN` differently
      (`THEN` inside `IF` statement or `INITIALIZE` statement)
    of course, we can remove `check_if_stmt`, and add the check of context in
      `handle_then` to make it work correctly

  we can define the check_accept, check_display... for each statement,
  however, there are too many duplicate code to write...
  we could do that if the finer analysis is needed.  *)
and check_if_stmt (text:text) state ifcheck =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload; loc} :: wordlist ->
    match payload with
    | CDirWord _ ->
        check_if_stmt wordlist state ifcheck
    | TextWord word ->
      begin match proc_context_of_str word with
      | Compiler_directive _ ->
          check_copy_replace text {state with scope = COPY_REPLACE} ifcheck
      | Phrase THEN ->
          handle_if_then loc wordlist state ifcheck
      | Open_scope keyword ->
          handle_open_scope keyword loc wordlist state ifcheck
      | No_keyword ->
          let offset = offset_of_context context in
          let acc = check_pos loc offset acc ifcheck in
          check_if_stmt wordlist {state with acc} false
      (*other case, call the general function*)
      | _ -> check_statement text state ifcheck
      end
    | _ ->
      let acc = check_pos loc (offset_of_context context) acc ifcheck in
      check_if_stmt wordlist {state with acc} false

(* TODO: Find a better way to solve the keyword conflict "(ON)EXCEPTION"*)
(*`check_raise_stmt`, `check_goback_stmt`, `check_use_stmt` are to solve the keyword conflict of "EXCEPTION"*)
(* the idea: when the RAISE/GOBACK/USE statement do not allow an "EXCEPTION" phrase any more,
             add a DUMMY token onto the `context`,
             when check the wordlist, check the `context` top, if there is a DUMMY token,
             call `check_statement`                                                            *)
and check_raise_stmt (text:text) state ifcheck =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload; loc} :: wordlist ->
    match payload with
    | CDirWord _ ->
        check_raise_stmt wordlist state ifcheck
    | TextWord _ when context = [] || fst @@ List.hd context <> DUMMY_EXCEPTION ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        let context = push_context DUMMY_EXCEPTION context in
        check_raise_stmt wordlist {state with acc; context} false
    | _ ->
        check_statement text state ifcheck

and check_goback_stmt (text:text) state ifcheck =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload; loc} :: wordlist ->
    match payload with
    | CDirWord _ ->
        check_goback_stmt wordlist state ifcheck
    | TextWord "." ->
        let context = handle_period context in
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        let scope = match context with
          | (BEGIN, _) :: _ -> STATEMENT (*ERROR PRONE*)
          | (scope, _) :: _ -> scope
          | [] -> failwith @@ failure_msg loc
        in
        check_proc_div wordlist {state with scope; context; acc} false
    | TextWord "RAISING" ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        let context = push_context RAISING context in
        check_goback_stmt wordlist {state with context; acc} false
    | TextWord "LAST"
      (*"GOBACK [RAISING LAST EXCEPTION]"*)
      when context <> [] && fst @@ List.hd context = RAISING ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_goback_stmt wordlist {state with acc} false
    | TextWord _
      when context <> [] && fst @@ List.hd context = RAISING ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        let context = push_context DUMMY_EXCEPTION context in
        check_goback_stmt wordlist {state with context; acc} false
    | _ ->
        check_statement text state ifcheck

and check_use_stmt (text:text) state ifcheck =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload; loc} :: wordlist ->
    match payload with
    | CDirWord _ ->
        check_use_stmt wordlist state ifcheck
    | TextWord ("GLOBAL"|"AFTER"|"STANDARD") ->
      (*the keyword which can appear before "EXCEPTION"*)
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_use_stmt wordlist {state with acc} false
    | TextWord _ when context = [] || fst @@ List.hd context <> DUMMY_EXCEPTION ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        let context = push_context DUMMY_EXCEPTION context in
        check_use_stmt wordlist {state with acc; context} false
    | _ ->
        check_statement text state ifcheck
(* / *)

(*For alignment of arguments*)
(*fst_arg: if is the first argument in the line*)
and check_arguments (text:text) state ifcheck ~fst_arg =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload; loc} :: wordlist ->
    begin match payload with
    | CDirWord _ -> check_arguments wordlist state ifcheck ~fst_arg
    | TextWord word ->
      begin match proc_context_of_str word with
      | No_keyword when fst_arg ->
          let acc = check_pos loc (offset_of_context context) acc ifcheck in
          let pos = start_pos loc in
          let offset = pos.pos_cnum - pos.pos_bol in
          let offset_change =
            match acc with
            | [] -> 0
            | {lnum; _} :: _ when lnum <> pos.pos_lnum -> 0
            | x :: _ -> x.offset_modif - x.offset_orig
          in
          let context =
            match context with
            | (ARGUMENT, _) :: context' ->
                (ARGUMENT, offset + offset_change) :: context'
            | _ -> failwith @@ failure_msg loc
          in
          check_arguments wordlist {state with acc; context} false ~fst_arg:false
      | No_keyword -> check_arguments wordlist state false ~fst_arg:false
      | _ ->
          let scope, context =
            match context with
            | (ARGUMENT, _) :: ((TO|FROM|INTO|BY|GIVING), _) :: ((prev, _) :: _ as context)
            | (ARGUMENT, _) :: ((prev, _) :: _ as context) -> prev, context
            | _ -> failwith @@ failure_msg loc
          in
          check_fun scope text {state with scope; context} ifcheck
      end
    | _ ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_arguments wordlist {state with acc} false ~fst_arg
    end

and check_add_stmt (text:text) state ifcheck =
  match text with
  | [] -> state
  | {payload; loc} :: wordlist ->
      match payload with
      | CDirWord _ -> check_add_stmt wordlist state ifcheck
      | TextWord word ->
        begin match proc_context_of_str word with
        | No_keyword ->
            let context = push_context ARGUMENT state.context in
            check_arguments text {state with scope = ARGUMENT; context} ifcheck ~fst_arg:true
	      | Phrase keyword -> handle_phrase keyword loc wordlist state ifcheck
	      | _ -> check_statement text state ifcheck
	    end
  | _ -> failwith @@ failure_msg loc

and check_subtract_stmt text state ifcheck = check_add_stmt text state ifcheck
and check_multiply_stmt text state ifcheck = check_add_stmt text state ifcheck
and check_divide_stmt text state ifcheck = check_add_stmt text state ifcheck
(* / *)


(* there are some handle_functions.
   different from check_function,
   the check_function will call itself again (possibly stay in its scope) (except `check_statement`)
   the handle_function handles one token and go directly to another scope *)
and handle_open_scope keyword loc wordlist state ifcheck =
  let context = imp_scope_termination state.context in
  let offset = offset_of_context context in
  let acc = check_pos loc offset state.acc ifcheck in
  let context = push_context keyword context in
  match keyword with
  (*TODO: find a better way to handle the nested IF statement.
          If need to indent the condition expression, must change this*)
  | IF ->
      let context = push_context THEN context in
      check_fun keyword wordlist {state with scope = keyword; context; acc} false
  | _ ->
      check_fun keyword wordlist {state with scope = keyword; context; acc} false

and handle_close_scope keyword loc wordlist state ifcheck =
  let context = exp_scope_termination keyword state.context in
  match context with
  | (key, _) :: ((prev, offset) :: _ as context) when key = keyword ->
      let acc = check_pos loc offset state.acc ifcheck in
      let scope =
        (*this case appears when indent a incomplet code, which begins directly with statement*)
        (*In general we avoid defining `state.scope` to `STATEMENT`,
          it is better to define `state.scope` to a specific statement like `IF`, `ADD`.
          `check_statement` is an auxiliary function *)
        if prev = BEGIN then STATEMENT
        else prev
      in
      check_fun scope wordlist {state with scope; context; acc} false
  | _ ->  failwith @@ failure_msg loc

and handle_if_then loc wordlist state ifcheck =
  let context = imp_scope_termination state.context in
  match context with
  | (THEN, _):: (IF, _) :: context' ->
      let acc = check_pos loc (offset_of_context context') state.acc ifcheck in
      check_fun state.scope wordlist {state with scope = THEN; context; acc} false
  | _ ->
      failwith @@ failure_msg loc

and handle_else loc wordlist state ifcheck =
  let context = exp_scope_termination THEN state.context in
  match context with
  | (THEN, _) :: ((IF, _) :: context' as context) ->
      let acc = check_pos loc (offset_of_context context') state.acc ifcheck in
      let context = push_context ELSE context in
      check_fun state.scope wordlist {state with scope = ELSE; context; acc} false
  |_ ->
    failwith @@ failure_msg loc

(*for alignment of argument*)
and handle_operator keyword loc wordlist state ifcheck =
  let context = phrase_termination state.context in
  let offset = offset_of_context context in
  let acc = check_pos loc offset state.acc ifcheck in
  let context = push_context keyword context in
  let context = push_context ARGUMENT context in
  check_arguments wordlist {state with acc; context; scope = ARGUMENT} false ~fst_arg:true

and handle_to loc wordlist state ifcheck =
  handle_operator TO loc wordlist state ifcheck

and handle_into loc wordlist state ifcheck =
  handle_operator INTO loc wordlist state ifcheck

and handle_from loc wordlist state ifcheck =
  handle_operator FROM loc wordlist state ifcheck

and handle_giving loc wordlist state ifcheck =
  handle_operator GIVING loc wordlist state ifcheck

and handle_by loc wordlist state ifcheck =
  let context = phrase_termination_until USING state.context in
  let offset = offset_of_context context in
  let acc = check_pos loc offset state.acc ifcheck in
  let context = push_context BY context in
  let context = push_context ARGUMENT context in
  check_arguments wordlist {state with acc; context; scope = ARGUMENT} false ~fst_arg:true

(*using-phrase is the only phrase that we treat more carefully
  using-phrase can contain by content/reference phrase.       *)
and handle_using loc text state ifcheck =
  let context = phrase_termination state.context in
  let offset = offset_of_context context in
  let acc = check_pos loc offset state.acc ifcheck in
  let context = push_context USING context in
  check_using text {state with acc; context; scope = USING} false

and check_using (text:text) state ifcheck =
  match text with
  | [] -> state
  | {payload = TextWord "BY"; loc} :: {payload = TextWord ("REFERENCE"|"CONTENT"|"VALUE"); _} :: wordlist
  | {payload = TextWord ("REFERENCE"|"CONTENT"|"VALUE"); loc} :: wordlist ->
      handle_by loc wordlist state ifcheck
  | {payload = TextWord word; _} :: _  ->
      begin match proc_context_of_str word with
      | No_keyword ->
          let context = push_context ARGUMENT state.context in
          check_arguments text {state with context; scope = ARGUMENT} ifcheck ~fst_arg:true
      | _ -> check_statement text state ifcheck
      (*Since using-phrase alse appears in the procedure division header,
        it is not appropriate to call check_statement here.
        However, no bug found untilnow.*)
      end
  | _ -> check_statement text state ifcheck
(* / *)


and handle_phrase keyword loc wordlist state ifcheck =
  match keyword with
  | TO -> handle_to loc wordlist state ifcheck
  | INTO -> handle_into loc wordlist state ifcheck
  | GIVING -> handle_giving loc wordlist state ifcheck
  | FROM -> handle_from loc wordlist state ifcheck
  | BY -> handle_by loc wordlist state ifcheck
  | USING -> handle_using loc wordlist state ifcheck
  | ELSE -> handle_else loc wordlist state ifcheck
  | WHEN -> handle_when loc wordlist state ifcheck
  | _ ->
      let context = phrase_termination state.context in
      let offset = offset_of_context context in
      let acc = check_pos loc offset state.acc ifcheck in
      match context with
      | (prev, _) :: _ ->
          let context = push_context keyword context in
          check_fun prev wordlist {state with scope = prev; context; acc} false
      | _ -> failwith @@ failure_msg loc

and handle_inline_phrase loc wordlist state ifcheck =
  let context = phrase_termination state.context in
  let offset = offset_of_context context in
  let acc = check_pos loc offset state.acc ifcheck in
  match context with
  | (prev, _) :: _ ->
      check_fun prev wordlist {state with scope = prev; context; acc} false
  | _ -> failwith @@ failure_msg loc


and handle_conditional_statement loc keyword wordlist state ifcheck =
  let help keyword rev_keyword keyword_associated =
    let context, acc = state.context, state.acc in
    let context = phrase_termination context in
    match context with
    (*special case, SEARCH statement contains only `AT_END` but not `NOT_AT_END`*)
    | (SEARCH, offset) :: _ when keyword = AT_END ->
        let acc = check_pos loc offset acc ifcheck in
        let context = push_context SEARCH_AT_END context in
        check_fun keyword wordlist {state with scope = keyword; context; acc} false
    (* take `ON_SIZE_ERROR` for an example*)
    (* when the `ON_SIZE_ERROR` is just after an `ADD`, it must match this `ADD`*)
    | (key, offset) :: _ when List.mem key keyword_associated ->
        let acc = check_pos loc offset acc ifcheck in
        let context = push_context keyword context in
        check_fun keyword wordlist {state with scope = keyword; context; acc} false
    (* when the `ON_SIZE_ERROR` is not just after `ADD`,
      there must be a `NOT_ON_SIZE_ERROR` in the `context`, match them*)
    | _ ->
        let context = exp_scope_termination rev_keyword context in
        match context with
        | (key, _) :: context when key = rev_keyword ->
            let acc = check_pos loc (offset_of_context context) acc ifcheck in
            let context = push_context HELPTOKEN context in
            check_fun keyword wordlist {state with scope = keyword; context; acc} false
        | _ -> failwith @@ failure_msg loc
  in
  match keyword with
  | ON_SIZE_ERROR ->
      help ON_SIZE_ERROR NOT_ON_SIZE_ERROR [ADD; SUBTRACT; MULTIPLY; DIVIDE; COMPUTE]
  | NOT_ON_SIZE_ERROR ->
      help NOT_ON_SIZE_ERROR ON_SIZE_ERROR [ADD; SUBTRACT; MULTIPLY; DIVIDE; COMPUTE]
  | AT_END ->
      help AT_END NOT_AT_END [READ; RETURN; SEARCH]
  | NOT_AT_END ->
      help NOT_AT_END AT_END [READ; RETURN; SEARCH]
  | ON_EXCEPTION ->
      help ON_EXCEPTION NOT_ON_EXCEPTION [ACCEPT; CALL; DISPLAY]
  | NOT_ON_EXCEPTION ->
      help NOT_ON_EXCEPTION ON_EXCEPTION [ACCEPT; CALL; DISPLAY]
  | INVALID_KEY ->
      help INVALID_KEY NOT_INVALID_KEY [READ; WRITE; REWRITE; START; DELETE]
  | NOT_INVALID_KEY ->
      help NOT_INVALID_KEY INVALID_KEY [READ; WRITE; REWRITE; START; DELETE]
  | AT_END_OF_PAGE ->
      help  AT_END_OF_PAGE NOT_AT_END_OF_PAGE [WRITE]
  | NOT_AT_END_OF_PAGE ->
      help NOT_AT_END_OF_PAGE AT_END_OF_PAGE [WRITE]
  | ON_OVERFLOW ->
      help ON_OVERFLOW NOT_ON_OVERFLOW [CALL; STRING; UNSTRING]
  | NOT_ON_OVERFLOW ->
      help NOT_ON_OVERFLOW ON_OVERFLOW [CALL; STRING; UNSTRING]
  | _ -> failwith @@ failure_msg loc


and handle_when loc wordlist state ifcheck =
  let context = phrase_termination state.context in
  match context with
  | (key, offset) :: _ when key = EVALUATE || key = SEARCH ->
      let acc = check_pos loc offset state.acc ifcheck in
      let context = push_context WHEN context in
      check_fun state.scope wordlist {state with scope = WHEN; context; acc} false
  | _ ->
      let rec pop_until_ context =
        match context with
        | (key, _) :: context when (key <> SEARCH_AT_END) && (key <> WHEN) ->
            pop_until_ context
        | _ -> context
      in
      let context = pop_until_ context in
      match context with
      | (WHEN, _) :: context' ->
          let acc = check_pos loc (offset_of_context context') state.acc ifcheck in
          check_fun state.scope wordlist {state with scope = WHEN; context; acc} false
      | (SEARCH_AT_END, _) :: context ->
          let acc = check_pos loc (offset_of_context context) state.acc ifcheck in
          let context = push_context WHEN context in
          check_fun state.scope wordlist {state with scope = WHEN; context; acc} false
      | _ ->
          failwith @@ failure_msg loc


and end_compilation_unit loc wordlist ({context; acc; _} as state) ifcheck =
  let context = exp_scope_termination COMPILATION_UNIT context in
  let context =
    match context with
    | (COMPILATION_UNIT, _) :: context -> context
    | _ -> failwith @@ failure_msg loc
  in
  let acc = check_pos loc (offset_of_context context) acc ifcheck in
  check_ident_div wordlist {state with scope = IDENT_DIV; context; acc} false


(*Remark: if the COPY does not copy a complete paragraph/statement/data entry...,
          but a phrase/clause/identifier..., the check_copy_replace does not work *)
and check_copy_replace (text:text) (state:indent_state) (ifcheck:bool)  =
  let context, acc = state.context, state.acc in
  match text with
  | [] -> state
  | {payload; loc} :: wordlist ->
    let pos = start_pos loc in
    begin match payload with
    | TextWord "COPY" ->
        let offset_orig = pos.pos_cnum - pos.pos_bol in
        (*no check*)
        let offset =  offset_orig + offset_of_keyword COPY in
        let context = (COPY, offset) :: context in
        check_copy_replace wordlist  {state with context} false
    | TextWord "REPLACE" ->
        let offset_orig = pos.pos_cnum - pos.pos_bol in
        (*no check*)
        let offset = offset_orig + offset_of_keyword REPLACE in
        let context = (REPLACE, offset) :: context in
        check_copy_replace wordlist  {state with context} false
    | TextWord "REPLACING" ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        let context = push_context REPLACING_COPY context in
        check_copy_replace wordlist {state with context; acc} false
    | TextWord "." ->
        let rec help context =
          match context with
          | ((REPLACING_COPY|COPY|REPLACE), _) :: context ->
            help context
          | _ -> context
        in
        let context = help context in
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        let scope = (*ERROR-PRONE*)
          if context = [] then failwith @@ failure_msg loc
          else fst @@ List.hd context
        in
        check_fun scope wordlist
          {state with scope; context; acc}
          false
    | _ ->
        let acc = check_pos loc (offset_of_context context) acc ifcheck in
        check_copy_replace wordlist {state with context; acc} false
    end


(***********************************)
(* in order to indent incomplete souce code
   the indenter must infer the scope where the code begins.

   The inference is limited.
   It works only if the code begins with
   - division
   - section
   - paragraph          (*line begins with "No_keyword ." will be infered as paragraph*)
   - entry 		          (*line begins with number will be infered as level-numbr*)
   - statement
   - COPY/REPLACE

   The inference function does not handle any keyword but does the inference
   and return the call of
   - `check_ident_div`
   - `check_end_div`
   - `check_data_div`
   - `check_proc_header_div`
   - `check_proc_div`
   - `check_statement`
   - `check_copy_replace`

   (**WARNING**)
   we cannot return the call of `check_if_stmt`, `check_RD` or `check_data_desc`
   Because the keyword "IF" is not handled inside `check_if_stmt` but handled by the previous layer.
           the keyword "RD" is not handled inside `check_RD` but handled inside `check_data_div` ...

   Due to the incomplete code, the indentation becomes error-prone, must consider all possible cases
   when pop the context.
*)
and infer_scope (text:text) (state:indent_state) ifcheck  =
  let base_context loc =
    let base_offset =
      let pos = start_pos loc in
      pos.pos_cnum - pos.pos_bol
    in
    [(BEGIN, base_offset)]
  in
  match text with
  (*jump to COPY_REPLACE*)
  | {payload = TextWord ("COPY"|"REPLACE"); loc} :: _ ->
      let context = base_context loc in
      check_copy_replace text {state with scope = COPY_REPLACE; context} ifcheck
  (*jump to IDENTIFICATION DIVISION*)
  | {payload = TextWord "IDENTIFICATION"; loc} :: {payload = TextWord "DIVISION"; _}
    :: {payload = TextWord "."; _} ::_
  | {payload = TextWord ("PROGRAM-ID"|"CLASS-ID"|"FACTORY"|"FUCNTION-ID"|"INTERFACE-ID"
    |"METHOD-ID"|"OBJECT"|"OPTIONS"|"AUTHOR"|"DATE-WRITTEN"|"INSTALLATION"|"SECURITY"|"DATE-COMPILED"); loc}
    :: {payload = TextWord "."; _} :: _ ->
      let context = base_context loc in
      check_ident_div text {state with scope = IDENT_DIV; context} ifcheck
  (*jump to ENVIRONMENT DIVISION*)
  | {payload = TextWord "NVIRONMENT"; loc} :: {payload = TextWord "DIVISION"; _}
    :: {payload = TextWord "."; _} :: _
  | {payload = TextWord ("INPUT-OUTPUT"|"CONFIGURATION"); loc } :: {payload = TextWord "SECTION"; _}
    :: {payload = TextWord "."; _} :: _
  | {payload = TextWord ("SOURCE-COMPUTER"|"OBJECT-COMPUTER"|"SPECIAL-NAMES"|"REPOSITORY"
    |"FILE-CONTROL"|"I-O-CONTROL"); loc} :: {payload = TextWord "."; _} :: _
  | {payload = TextWord "SELECT";loc} :: _ ->
      let context = base_context loc in
      check_env_div text {state with scope = ENV_DIV; context} ifcheck
  (*jump to DATA DIVISION*)
  | {payload = TextWord "DATA"; loc} :: {payload = TextWord "DIVISION"; _} :: _
  | {payload = TextWord ("FILE"|"WORKING-STORAGE"|"LOCAL-STORAGE"|"LINKAGE"|"REPORT"|"SCREEN"); loc }
    :: {payload = TextWord "SECTION"; _} :: {payload = TextWord "."; _} :: _
  | {payload = TextWord ("FD"|"RD"|"SD"); loc } :: _ ->
      let context = base_context loc in
      check_data_div text {state with scope = DATA_DIV; context} ifcheck
  | {payload = TextWord str; loc } :: {payload = TextWord str2;_} :: _
    when ifcheck && is_data_decl str && str2 <> "." ->
      let context = base_context loc in
      check_data_div text {state with scope = DATA_DIV; context} ifcheck
  (*jump to PROCEDURE DIVISION header*)
  | {payload = TextWord "PROCEDURE"; loc} :: {payload = TextWord "DIVISION"; _} :: _ ->
      let context = base_context loc in
      check_proc_div_header text {state with scope = PROC_DIV_HEADER; context} ifcheck
  (*jump to PROCEDURE DIVISION*)
  | {payload = TextWord "DECLARATIVES" ; loc} :: _
  | {payload = TextWord "END"; loc} :: _ ->
      let context = base_context loc in
      check_proc_div text {state with scope = PROC_DIV; context} ifcheck
  | {payload = TextWord str; loc} :: {payload = TextWord "SECTION"; _}
    :: {payload = TextWord "."; _} :: _
  | {payload = TextWord str; loc} :: {payload = TextWord "."; _} :: _
    when not @@ is_statement str ->
      let context = base_context loc in
      check_proc_div text {state with scope = PROC_DIV; context} ifcheck
  | {payload = TextWord str; loc} :: _
    when is_statement str ->
      let context = base_context loc in
      begin match proc_context_of_str str with
      | Open_scope key ->
          check_statement text {state with scope = key; context} ifcheck
      | _ -> failwith @@ failure_msg loc
      end
  | [] -> state
  | {payload = TextWord str; loc} :: _->
      failwith ("error infer division: "^str^failure_msg loc )
  | {loc;_ } :: _ ->  failwith @@ "error infer division" ^ failure_msg loc


and check_fun = function
  | BEGIN -> infer_scope
  | COPY_REPLACE -> check_copy_replace
  | IDENT_DIV -> check_ident_div
  | ENV_DIV -> check_env_div
  | DATA_DIV | LEVEL _ -> check_data_div
  | DATA_DESC -> check_data_desc
  | FD -> check_FD
  | RD -> check_RD
  | SD -> check_SD
  | PROC_DIV_HEADER -> check_proc_div_header
  | PROC_DIV | SECTION | PARAGRAPH | DECLARATIVES ->
      check_proc_div
  | IF -> check_if_stmt
  | RAISE -> check_raise_stmt
  | GOBACK -> check_goback_stmt
  | USE -> check_use_stmt
  | ARGUMENT -> check_arguments ~fst_arg:true
  | ADD -> check_add_stmt
  | SUBTRACT -> check_subtract_stmt
  | MULTIPLY -> check_multiply_stmt
  | DIVIDE -> check_divide_stmt
  | USING -> check_using
  | STATEMENT | _ -> check_statement


let check_indentation (text:text) (state:indent_state) =
  match state.range with
  | None ->
    check_fun state.scope text state true
  | Some {start_line; end_line} ->
    let lnum =
      match text with
      | {loc; _} :: _ ->
        (start_pos loc).pos_lnum
      | _ -> 0
    in
    if (lnum >= start_line) && (lnum <= end_line) then
      check_fun state.scope text state true
    else
      state
