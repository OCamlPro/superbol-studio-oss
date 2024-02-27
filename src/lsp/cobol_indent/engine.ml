(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Types

(* Setting this flag to true will write a lot of stuff on stderr, but
   is quite useful to debug. *)

let verbose = false

type identification_division = unit

type control_division = unit

type level = {
  level : int ;
  level_indent : int ;
}

type data_division = {
  mutable level_stack : level list ;
}

type environment_division = unit

type statement = {
  stmt_token : token ;
  stmt_indent : int ;
  stmt_close_token : token ;
  mutable stmt_inner_indent : int option ;
}

type procedure_divison = {
  mutable proc_stack : statement list ;
}

type division =
  | Ident_div of identification_division
  | Control_div of control_division
  | Data_div of data_division
  | Env_div of environment_division
  | Proc_div of procedure_divison

type state = {
  mutable directive_stack : state list ;
  mutable division : division ;
  mutable indent : int ;

  (* for debug *)
  mutable tokens : int list ;
  mutable line : int ;
}

let area_b_offset = 4
let min_gap_offset = 4

let rec copy_division div =
  match div with
  | Proc_div div ->
    Proc_div { proc_stack = List.map copy_statement div.proc_stack }
  | Data_div div ->
    Data_div { level_stack = div.level_stack }
  | _ -> div

and copy_statement stmt =
  { stmt with stmt_inner_indent = stmt.stmt_inner_indent }

let push_state state =

  let state_copy = { state with
                     directive_stack = [] ;
                     division = copy_division state.division ;
                   }
  in
  state.directive_stack <- state_copy :: state.directive_stack

let display state fun_name tokens =
  Printf.eprintf "\n  > %s:\n%!" fun_name ;
  begin
    match tokens with
      [] -> ()
    | (token,e) :: _ ->
      Printf.eprintf "     token = %S\n%!" (Misc.string_of_token token);
      let edit = e.tok_edit in
      Printf.eprintf "     tok = { indent = %d ; length = %d} tok_bol = %b\n%!"
        e.tok_indent e.tok_length edit.bol ;
      let edit = edit.edit in
      Printf.eprintf
        "     edit = { lnum = %d offset_orig = %d offset_modif = %d}\n%!"
        edit.lnum  edit.offset_orig edit.offset_modif ;
  end ;
  begin
    Printf.eprintf "     state.indent = %d\n%!" state.indent ;
  end ;

  ()

let indent_tokens ~config tokens =

  let source_format = config.source_format in
  let state = {
    indent = 0;
    directive_stack = [] ;
    division = Ident_div () ;
    tokens = [] ;
    line = 0 ;
  } in


  let set_indent indent =
    if verbose then Printf.eprintf "    state.indent <- %d\n%!" indent;
    state.indent <- indent
  in
  let area_a () =
    if verbose then Printf.eprintf "{{area_a}}\n%!" ;
    set_indent 0 in
  let area_b () =
    if verbose then Printf.eprintf "{{area_b}}\n%!" ;
    set_indent area_b_offset in
  let incr () =
    if verbose then Printf.eprintf "{{incr}}\n%!" ;
    set_indent ( state.indent + config.arg_offset ) in
  let _decr () =
    set_indent ( max area_b_offset ( state.indent - config.arg_offset ) )
  in
  let get_gap e tokens =
    match tokens with
    | [] -> min_gap_offset
    | (_, e2) :: _ ->
      max 2 ( e2.tok_indent - e.tok_indent )
  in


  let token_indent descr =
    descr.tok_indent +
    (descr.tok_edit.edit.offset_modif - descr.tok_edit.edit.offset_orig )
  in


  (* set_indent to the current indent + the gap between this token and
     the next one. ~inline=true means that we should use as base
     indentation the current token indentation instead of the current
     state indentation. It might be the case that we want to always
     have ~inline=true ! *)
  let set_indent_to_gap ?(inline = false) e tokens =
    if verbose then Printf.eprintf "{{gap}}\n%!" ;
    let indent =
      if inline then token_indent e else state.indent in
    set_indent ( indent + get_gap e tokens )
  in

  let add_close_token div ?inner_indent token tok =
    if verbose then
      Printf.eprintf "[[add_close_token %S]]\n"
        (Misc.string_of_token token);
    let f close_token =
      if verbose then
        Printf.eprintf "[[push close_token %s]]\n%!"
          (Misc.string_of_token close_token) ;
      div.proc_stack <- {
        stmt_token  = token ;
        stmt_indent = (* state.indent *) token_indent tok ;
        stmt_close_token = close_token ;
        stmt_inner_indent = inner_indent ;
      } :: div.proc_stack
    in
    match token with
    | ACCEPT -> f END_ACCEPT
    | ADD -> f END_ADD
    | CALL -> f END_CALL
    | COMPUTE -> f END_COMPUTE
    | DELETE -> f END_DELETE
    | DISPLAY -> f END_DISPLAY
    | DIVIDE -> f END_DIVIDE
    | EVALUATE -> f END_EVALUATE
    | IF -> f END_IF
    | JSON -> f END_JSON
    | MODIFY -> f END_MODIFY
    | MOVE -> f MOVE (* no end... *)
    | MULTIPLY -> f END_MULTIPLY
    | PERFORM -> f END_PERFORM
    | READ -> f END_READ
    | RECEIVE -> f END_RECEIVE
    | RETURN -> f END_RETURN
    | REWRITE -> f END_REWRITE
    | SEARCH -> f END_SEARCH
    | SEND -> f END_SEND
    | START -> f END_START
    | STRING -> f END_STRING
    | SUBTRACT -> f END_SUBTRACT
    | UNSTRING -> f END_UNSTRING
    | WRITE -> f END_WRITE
    | XML -> f END_XML

    | _
      -> ()
(*
      Printf.kprintf failwith "No END-%s token"
        ( Misc.string_of_token token )
*)
  in

  let rec close_stmt div tokens =
    if verbose then
      Printf.eprintf "[[close_stmt %s]]\n%!"
        ( String.concat " | "
            ( List.map Misc.string_of_token tokens ));
    match div.proc_stack with
    | [] ->
      if verbose then
        Printf.eprintf "  [[empty stack]]\n%!";
      ()
    | stmt :: stack ->
      if verbose then
        Printf.eprintf "  [[stack pop with %s]]\n%!"
          (Misc.string_of_token stmt.stmt_close_token);
      div.proc_stack <- stack ;
      if not ( List.mem stmt.stmt_close_token tokens) then
        close_stmt div tokens
      else begin
        if verbose then
          Printf.eprintf "  [[finish indent %d]]\n%!"
            stmt.stmt_indent ;
        set_indent stmt.stmt_indent
      end
  in

  let rec continue_stmts ?replace div tokens =
    if verbose then
      Printf.eprintf "[[continue_stmts %s]]\n%!"
        ( String.concat " | "
            ( List.map Misc.string_of_token tokens ));
    match div.proc_stack with
    | [] ->
      if verbose then
        Printf.eprintf "  [[empty stack]]\n%!";
      ()
    | stmt :: stack ->
      if verbose then
        Printf.eprintf "  [[stack with %s]]\n%!"
          (Misc.string_of_token stmt.stmt_close_token);
      if not ( List.mem stmt.stmt_close_token tokens) then begin
        if verbose then
          Printf.eprintf "  [[pop stack with %s]]\n%!"
            (Misc.string_of_token stmt.stmt_close_token);
        div.proc_stack <- stack ;
        continue_stmts ?replace div tokens
      end else begin
        set_indent stmt.stmt_indent;
        match replace with
        | None -> ()
        | Some token ->
          div.proc_stack <- { stmt with stmt_close_token = token } :: stack
      end
  in

  let rec complete_stmts div tokens =
    if verbose then
      Printf.eprintf "[[complete_stmts %s]]\n%!"
        ( String.concat " | "
            ( List.map Misc.string_of_token tokens ));
    match div.proc_stack with
    | [] ->
      if verbose then
        Printf.eprintf "  [[empty stack]]\n%!";
      ()
    | stmt :: stack ->
      if verbose then
        Printf.eprintf "  [[stack with %s]]\n%!"
          (Misc.string_of_token stmt.stmt_close_token);
      if not ( List.mem stmt.stmt_close_token tokens) then begin
        if verbose then
          Printf.eprintf "  [[pop stack with %s]]\n%!"
            (Misc.string_of_token stmt.stmt_close_token);
        div.proc_stack <- stack ;
        complete_stmts div tokens
      end else begin
        set_indent ( stmt.stmt_indent + config.inner_offset );
        match stmt.stmt_inner_indent with
        | Some _ -> ()
        | None ->
          stmt.stmt_inner_indent <- Some ( state.indent + config.inner_offset )
      end
  in


  (*
  List.iter (fun token ->
      Printf.printf "   %s\n%!" ( Misc.string_of_token token)
    ) tokens;
*)

  let set_token_indent token descr =
    let edit =
      match descr.tok_edit with
      | { bol = false ; edit } ->
        if verbose then
          Printf.eprintf "  %s <- keep %d\n%!" ( Misc.string_of_token token )
            (token_indent descr);
        edit
      | { bol = true ; edit } ->
        if verbose then
          Printf.eprintf "  %s <- set %d "
            ( Misc.string_of_token token ) state.indent ;
        edit.offset_modif <- state.indent ;
        if verbose then
          if edit.offset_modif <> edit.offset_orig then begin
            Printf.eprintf "(change %d -> %d)\n%!"
              edit.offset_orig edit.offset_modif
          end else
            Printf.eprintf "(no change)\n%!";
        edit
    in

    let indent_diff = edit.offset_modif - edit.offset_orig in
    let max_indent = descr.tok_indent + indent_diff + descr.tok_length in

    if max_indent > source_format.max_text_length then begin
      let fix_indent = max_indent - source_format.max_text_length in
      if verbose || not (fix_indent <= indent_diff) then begin
        Printf.eprintf "offset_modif = %d\n" edit.offset_modif ;
        Printf.eprintf "offset_orig = %d\n" edit.offset_orig ;
        Printf.eprintf "indent_diff = %d\n" indent_diff ;
        Printf.eprintf "tok_indent = %d\n" descr.tok_indent ;
        Printf.eprintf "tok_length = %d\n" descr.tok_length ;
        Printf.eprintf "max_indent = %d\n" max_indent ;
        Printf.eprintf "max_text_length = %d\n"
          source_format.max_text_length ;
        Printf.eprintf "fix_indent = %d\n" fix_indent ;
        Printf.eprintf "fix_indent = %d > indent_diff = %d\b%!"
          fix_indent indent_diff ;
      end;
      assert (fix_indent <= indent_diff) ;
      edit.offset_modif <- edit.offset_modif - fix_indent ;
      if verbose then
        Printf.eprintf "  %s ==> fix: -%d --> %d\n%!"
          ( Misc.string_of_token token ) fix_indent edit.offset_modif ;
    end
  in

  let apply_directive dir =
    if verbose then
      Printf.eprintf "[[apply_directive %S]]\n%!" dir;
    match dir with
    | "IF" ->
      if verbose then
        Printf.eprintf "[[apply_directive %S: IF branch]]\n%!" dir;
      push_state state
    | "END-IF" ->
      begin
        match state.directive_stack with
        | [] -> (* weird *) ()
        | _ :: stack ->
          if verbose then
            Printf.eprintf "[[apply_directive %S: END-IF]]\n%!" dir;
          state.directive_stack <- stack
      end
    | "ELSE" | "ELIF" ->
      begin
        match state.directive_stack with
        | [] -> (* weird *) ()
        | saved_state :: _stack ->
          if verbose then
            Printf.eprintf "[[apply_directive %S: ELSE branch]]\n%!" dir;
          set_indent saved_state.indent ;
          state.division <- copy_division saved_state.division
      end
    | _ -> ()
  in

  let rec begin_line tokens =
    if verbose then display state "begin_line" tokens ;
    begin
      match state.division with
      | Proc_div div ->
        area_b ();
        if verbose then
          Printf.eprintf "  [[empty stack at line begin]]\n%!";
        div.proc_stack <- []
      | Ident_div _
      | Control_div _
      | Env_div _
      | Data_div _
        -> ()

    end;
    match tokens with
    | [] -> ()

    | (IDENTIFICATION, e1) :: (DIVISION, e2) :: tokens ->
      area_a () ;
      set_token_indent IDENTIFICATION e1 ;
      incr () ;
      set_token_indent DIVISION e2 ;
      let div = () in
      state.division <- Ident_div div;
      iter_ident div tokens

    (* PROGRAM-ID might appear at the end of PROCEDURE DIVISION, in
       this case, we must enter ENVIRONMENT DIVISION mode. *)
    | (
      PROGRAM_ID
    | FUNCTION_ID
      as token, e1) :: tokens ->
      area_a ();
      set_token_indent token e1;
      area_b ();
      let div = () in
      state.division <- Ident_div div;
      iter_ident div tokens

    | (CONTROL, e1) :: (DIVISION, e2) :: tokens ->
      area_a () ;
      set_token_indent CONTROL e1 ;
      incr () ;
      set_token_indent DIVISION e2 ;
      let div = () in
      state.division <- Control_div div;
      iter_env div tokens

    | (ENVIRONMENT, e1) :: (DIVISION, e2) :: tokens ->
      area_a () ;
      set_token_indent ENVIRONMENT e1 ;
      incr () ;
      set_token_indent DIVISION e2 ;
      let div = () in
      state.division <- Env_div div;
      iter_env div tokens

    | (DATA, e1) :: (DIVISION, e2) :: tokens ->
      area_a () ;
      set_token_indent DATA e1 ;
      incr () ;
      set_token_indent DIVISION e2 ;
      let div = {
        level_stack = [];
      } in
      state.division <- Data_div div;
      iter_data div tokens

    | (PROCEDURE, e1) :: (DIVISION, e2) :: tokens ->
      area_a () ;
      set_token_indent PROCEDURE e1 ;
      incr () ;
      set_token_indent DIVISION e2 ;
      let div = {
        proc_stack = [] ;
      } in
      state.division <- Proc_div div;
      if verbose then
        Printf.eprintf "new PROCEDURE DIV\n%!";
      iter_proc div tokens

    | (token, e1) :: (SECTION, e2) :: tokens when token <> EXIT ->
      area_a () ;
      set_token_indent token e1 ;
      incr ();
      set_token_indent SECTION e2 ;
      iter_div tokens

    | (DIRECTIVE dir as token, e) :: tokens ->
      let save_indent = state.indent in
      area_a ();
      set_token_indent token e;
      set_indent save_indent ;
      apply_directive dir ;
      begin_line tokens

    | (COPY, e) :: tokens ->
      area_a ();
      set_token_indent COPY e;
      set_indent_to_gap e tokens ;
      iter_div tokens

    | (REPLACE, e) :: tokens ->
      area_a ();
      set_token_indent COPY e;
      set_indent_to_gap e tokens ;
      iter_div tokens

    | (END, e1) :: ( (((PROGRAM | FUNCTION), _) :: _ ) as tokens ) ->
      area_a ();
      set_token_indent END e1 ;
      incr ();
      state.division <- Ident_div ();
      iter_div tokens

    | (EQUALEQUAL, e) :: tokens ->
      set_token_indent EQUALEQUAL e ;
      iter_replace tokens

    | _ ->
      match state.division with
      | Ident_div div -> begin_line_ident div tokens
      | Control_div div -> begin_line_control div tokens
      | Env_div div -> begin_line_env div tokens
      | Data_div div -> begin_line_data div tokens
      | Proc_div div -> begin_line_proc div tokens


  and iter tokens =
    if verbose then display state "iter" tokens ;
    match tokens with
    | [] -> ()
    | (DOT, e) :: tokens ->
      area_b ();
      set_token_indent DOT e ;
      begin_line tokens

    | (DIRECTIVE dir as token, e) :: tokens ->
      let save_indent = state.indent in
      area_a ();
      set_token_indent token e;
      set_indent save_indent ;
      apply_directive dir ;
      iter_div tokens

    | (REPLACING, e) :: tokens ->
      set_token_indent REPLACING e ;
      set_indent_to_gap e tokens ;
      iter_div tokens

    | (EQUALEQUAL, e) :: tokens ->
      set_token_indent EQUALEQUAL e ;
      iter_replace tokens

    | _ :: (DIVISION, _ ) :: _ ->
      begin_line tokens
    | (token, _) :: (SECTION, _ ) :: _ when token <> EXIT ->
      begin_line tokens

    | (token, e) :: tokens ->
      set_token_indent token e;
      iter_div tokens

  and iter_div tokens =
    if verbose then display state "iter_div" tokens ;
    match state.division with
    | Ident_div div -> iter_ident div tokens
    | Control_div div -> iter_control div tokens
    | Env_div div -> iter_env div tokens
    | Data_div div -> iter_data div tokens
    | Proc_div div -> iter_proc div tokens

  (* skip everything inside == ... == to avoid breaking indentation there *)
  and iter_replace tokens =
    if verbose then display state "iter_replace" tokens ;
    match tokens with
    | [] -> ()
    | (EQUALEQUAL, _e) :: tokens ->
      iter_div tokens
    | (_token, _e) :: tokens ->
      iter_replace tokens



  and begin_line_ident div tokens =
    if verbose then display state "begin_line_ident" tokens ;
    match tokens with

    | ( INFORMATION _ as token, e ) :: tokens ->
      area_a () ;
      set_token_indent token e ;
      area_b ();
      begin_line tokens

    | [] -> ()
    | (token, edit) :: tokens ->
      set_token_indent token edit;
      incr ();
      iter_ident div tokens

  and iter_ident _div tokens =
    if verbose then display state "iter_ident" tokens ;
    match tokens with
    | [] -> ()
    | _ -> iter tokens




  and begin_line_control div tokens =
    if verbose then display state "begin_line_control" tokens ;
    match tokens with

    | [] -> ()
    | _ ->
      iter_control div tokens

  and iter_control div tokens =
    if verbose then display state "iter_control" tokens ;
    match tokens with
    | [] -> ()
    | (
      ACCEPT
    | DISPLAY
      as token, e ) :: tokens ->
      area_b () ;
      set_token_indent token e ;
      incr ();
      iter_control div tokens
    | _ -> iter tokens




  and begin_line_env div tokens =
    if verbose then display state "begin_line_env" tokens ;
    match tokens with

    | [] -> ()
    | _ ->
      iter_env div tokens

  and iter_env div tokens =
    if verbose then display state "iter_env" tokens ;
    match tokens with

    | (
      ( FUNCTION
      | CLASS
      | DELEGATE
      ) as token, e) :: tokens ->
      area_b () ;
      set_token_indent token e ;
      incr ();
      iter_env div tokens

    | (
      ( I_O_CONTROL
      | FILE_CONTROL
      | SOURCE_COMPUTER
      | OBJECT_COMPUTER
      ) as token, e1) :: (DOT, e2) :: tokens ->
      area_a () ;
      set_token_indent token e1 ;
      set_token_indent DOT e2 ;
      area_b ();
      begin_line tokens

    | ( SPECIAL_NAMES
      | REPOSITORY
        as token, e1) :: (DOT, e2) :: tokens ->
      area_a ();
      set_token_indent token e1 ;
      set_token_indent token e2 ;
      area_b ();
      begin_line tokens

    | ( DECIMAL_POINT as token, e) :: tokens ->
      area_b ();
      set_token_indent token e ;
      incr ();
      begin_line tokens

(*
    | (
      (
        INPUT_OUTPUT
      | CONFIGURATION
      ) as token, e1) :: (SECTION, e2) :: tokens ->
      area_a () ;
      set_token_indent token e1 ;
      incr ();
      set_token_indent SECTION e2 ;
      iter_env div tokens
*)

    | (SELECT, e) :: tokens ->
      if config.select_in_area_a then
        area_a () else area_b ();
      set_token_indent COPY e;
      set_indent_to_gap e tokens ;
      iter_div tokens

    | [] -> ()
    | _ -> iter tokens





  and begin_line_data div tokens =
    if verbose then display state "begin_line_data" tokens ;
    match tokens with

    (*
    | (
      ( WORKING_STORAGE
      | LINKAGE
      | FILE
      | REPORT
      ) as token, e1) :: (SECTION, e2) :: tokens ->
      area_a () ;
      set_token_indent token e1 ;
      incr ();
      set_token_indent SECTION e2 ;
      iter_data div tokens
*)

    | ( CD | FD | RD | SD as token, e ) :: tokens ->
      area_a () ;
      set_token_indent token e ;
      area_b ();
      iter_data div tokens

    | (INTEGER n as token, e) :: tokens ->
      let level = try int_of_string n with _ -> max_int in
      if verbose then
        Printf.eprintf "[[level %d]]\n%!" level ;
      let gap = match config.data_item_offset with
        | None ->
          get_gap e tokens
        | Some n -> n
      in
      if level = 1 || level = 77 || level = 78 then begin
        area_a () ;
        set_token_indent token e ;
        let level_indent = state.indent + gap in
        div.level_stack <- [ { level ; level_indent } ] ;
      end else begin
        let rec iter () =
          match div.level_stack with
          | [] ->
            if verbose then
              Printf.eprintf "[[empty level stack]]\n%!" ;
            area_a ();
            set_token_indent token e ;
            let level_indent = state.indent + gap in
            div.level_stack <- [ { level ; level_indent } ] ;
          | l :: stack ->
            if verbose then
              Printf.eprintf "[[previous level %d]]\n%!" l.level;
            if (level = 66 && l.level = 1 )
            || (level <> 66 && l.level < level) then begin
              set_indent l.level_indent ;
              set_token_indent token e ;
              let level_indent = state.indent + gap in
              div.level_stack <- { level ; level_indent } :: div.level_stack ;
            end else begin
              div.level_stack <- stack ;
              iter ()
            end
        in
        iter ();
      end;
      begin
        if verbose then display state "level" tokens;
        match tokens with
        | [] -> ()
        | (token, e) :: tokens ->
          incr ();
          set_token_indent token e;
          set_indent_to_gap ~inline:true e tokens ;
          iter_data div tokens
      end
    | [] -> ()
    | (token, edit) :: tokens ->
      set_token_indent token edit;
      incr ();
      iter_data div tokens

  and iter_data _div tokens =
    if verbose then display state "iter_data" tokens ;
    match tokens with
    | [] -> ()
    | _ -> iter tokens

  and begin_line_proc div tokens =
    if verbose then display state "begin_line_proc" tokens ;
    match tokens with

    | (IDENT ident , e1) :: (SECTION, e2) :: (DOT, e3) :: tokens ->
      area_a ();
      set_token_indent (IDENT ident) e1 ;
      incr ();
      set_token_indent SECTION e2 ;

      area_a ();
      set_token_indent DOT e3 ;
      area_b ();
      begin_line tokens

    (* paragraph *)
    | ( IDENT _
      | INTEGER _
        as token1, e1) :: (DOT as token2, e2) :: tokens ->
      area_a ();
      set_token_indent token1 e1 ;
      set_token_indent token2 e2 ;
      begin_line tokens

    | [] -> ()
    | tokens ->
      iter_proc div tokens

  and reset_token_indent_proc div =
    if verbose then
      Printf.eprintf "    [[reset_token_indent_proc]]\n%!";
    match div.proc_stack with
    | [] ->
      if verbose then
        Printf.eprintf "\n[[no stack]]\n%!";
      area_b ()
    | { stmt_inner_indent = Some indent; _ } :: _ ->
      if verbose then
        Printf.eprintf "\n[[stack indent = %d]]\n%!" indent;
      set_indent indent
    | { stmt_inner_indent = None ; _ } :: stack ->
      if verbose then
        Printf.eprintf "[[pop reset_token_indent_proc]]\n%!";
      div.proc_stack <- stack ;
      reset_token_indent_proc div

  and iter_proc div tokens =
    if verbose then display state "iter_proc" tokens ;
    match tokens with

    | (IDENT _
       as token1 , e1) ::
      (SECTION
       as token2, e2) :: (DOT, e3) :: tokens
    | (ENTRY
       as token1, e1) ::
      (token2, e2) :: (DOT, e3) :: tokens
      ->
      area_a ();
      set_token_indent token1 e1 ;
      incr ();
      set_token_indent token2 e2 ;
      area_a ();
      set_token_indent DOT e3 ;
      area_b ();
      begin_line tokens

    | (END as token1, e1) :: (DECLARATIVES as token2, e2) :: tokens ->
      area_a ();
      set_token_indent token1 e1;
      incr ();
      set_token_indent token2 e2;
      iter_proc div tokens

    | (DECLARATIVES as token, e) :: tokens ->
      area_a ();
      set_token_indent token e;
      incr ();
      iter_proc div tokens

    | (
      (
        END_ACCEPT
      | END_ADD
      | END_CALL
      | END_COMPUTE
      | END_DELETE
      | END_DISPLAY
      | END_DIVIDE
      | END_EVALUATE
      | END_IF
      | END_JSON
      | END_MODIFY
      | END_MULTIPLY
      | END_PERFORM
      | END_READ
      | END_RECEIVE
      | END_RETURN
      | END_REWRITE
      | END_SEARCH
      | END_SEND
      | END_START
      | END_STRING
      | END_UNSTRING
      | END_WRITE
      | END_XML

      ) as token, e) :: tokens ->
      close_stmt div (if token = END_IF then
                        [ ELSE ; END_IF ]
                      else
                        [ token ] );
      set_token_indent token e;
      iter_proc div tokens;

    | (EXIT as token1, e1) :: (PERFORM as token2, e2) :: tokens
    | (RAISE as token1, e1) :: (EXCEPTION as token2, e2) :: tokens
      ->

      reset_token_indent_proc div ;
      set_token_indent token1 e1 ;
      add_close_token div token1 e1 ;
      set_indent_to_gap e1 ( (token2,e2) :: tokens) ;
      set_token_indent token2 e2 ;
      iter_proc div tokens

    | (
      (
        ACCEPT
      | ADD
      | ALLOCATE
      | CALL
      | CANCEL
      | CLOSE
      | COMMIT
      | COMPUTE
      | CONTINUE
      | DELETE
      | DISPLAY
      | DIVIDE
      (* | EVALUATE *)
      | EXIT
      | FREE
      | GENERATE
      | GO
      | GOBACK
      (*  | IF *)
      | INITIALIZE
      | INITIATE
      | INSPECT
      | INVOKE
      | JSON
      | MERGE
      | MODIFY
      | MOVE
      | MULTIPLY
      | OPEN
      (* PERFORM *)
      | RAISE
      | READ
      | RECEIVE
      | RELEASE
      | RESUME
      | RETURN
      | REWRITE
      | ROLLBACK
      | SEARCH
      | SEND
      | SET
      | SORT
      | START
      | STOP
      | STRING
      | SUBTRACT
      | SUPPRESS
      | TERMINATE
      | UNLOCK
      | UNSTRING
      | USE
      | VALIDATE
      | WRITE
      | XML
      ) as token, e) :: tokens ->

      reset_token_indent_proc div ;
      set_token_indent token e ;
      add_close_token div token e ;
      set_indent_to_gap e tokens ;
      iter_proc div tokens


    | (PERFORM as token, e) ::
      ( ( ((IDENT _ | INTEGER _), _) :: (token2, _) :: _ ) as tokens )
      when token2 <> TIMES ->

      if verbose then
        Printf.eprintf "[[PERFORM par]]\n%!";
      reset_token_indent_proc div ;
      set_token_indent token e ;
      add_close_token div PERFORM_PAR e ;
      set_indent_to_gap e tokens ;
      iter_proc div tokens

    | (TO as token, e) :: tokens ->
      begin

        match div.proc_stack with
        | { stmt_token = MOVE ; stmt_indent ; _ } :: _ ->
          if verbose then
            Printf.eprintf "[[ MOVE TO ]]\n%!";
          set_indent ( stmt_indent + 2 );
          set_token_indent token e;
          set_indent_to_gap ~inline:true e tokens ;
          iter_proc div tokens
        | _ ->
          set_token_indent token e;
          iter_proc div tokens
      end

    | (
      (
        EVALUATE
      | IF
      | PERFORM
      ) as token, e) :: tokens ->

      reset_token_indent_proc div ;
      set_token_indent token e ;
      let inner_indent = state.indent + config.inner_offset in
      add_close_token div token ~inner_indent e ;
      set_indent_to_gap e tokens ;
      if verbose then
        Printf.eprintf "\n[[Push stack]]\n%!";
      iter_proc div tokens

    | (ELSE, e1) :: (IF, e2) :: tokens ->
      continue_stmts div [ END_IF ] ;
      set_token_indent ELSE e1;
      incr () ;
      set_token_indent IF e2;
      set_indent_to_gap e1 tokens;
      iter_proc div tokens;

    | (THEN as token, e) :: tokens ->
      continue_stmts div [ END_IF ] ;
      set_token_indent token e;
      iter_proc div tokens;

    | (ELSE as token, e) :: tokens ->
      continue_stmts ~replace:ELSE div [ END_IF ] ;
      set_token_indent token e;
      iter_proc div tokens;

      (* [SIZE ERROR block] can happen in ADD, COMPUTE, DIVIDE,
         MULTIPLY, SUBTRACT *)
    | (SIZE, _) :: (ERROR, _) :: _ ->
      complete_stmts2 div tokens [ END_ADD ; END_COMPUTE ; END_DIVIDE ;
                                   END_MULTIPLY ; END_SUBTRACT ]

    (* [ON SIZE ERROR block] can happen in ADD, COMPUTE, DIVIDE,
       MULTIPLY, SUBTRACT *)
    | (ON, _) :: (SIZE, _) :: (ERROR, _) :: _ ->
      complete_stmts3 div tokens [ END_ADD ; END_COMPUTE ; END_DIVIDE ;
                                   END_MULTIPLY ; END_SUBTRACT ]


    (* [NOT SIZE ERROR block] can happen in ADD, COMPUTE,
       DIVIDE, MULTIPLY, SUBTRACT *)
    | (NOT, _) :: (SIZE, _) :: (ERROR, _) :: _ ->
      complete_stmts3 div tokens [ END_ADD ; END_COMPUTE ; END_DIVIDE ;
                                   END_MULTIPLY ; END_SUBTRACT ]

    (* [NOT ON SIZE ERROR block] can happen in ADD, COMPUTE,
       DIVIDE, MULTIPLY, SUBTRACT *)
    | (NOT, _) :: (ON, _) :: (SIZE,_) :: (ERROR,_) :: _ ->
      complete_stmts4 div tokens [ END_ADD ; END_COMPUTE ; END_DIVIDE ;
                                   END_MULTIPLY ; END_SUBTRACT ]

    (* [EXCEPTION block] can happen in ACCEPT, CALL, DELETE,
       DISPLAY, RECEIVE, SEND *)
    | (EXCEPTION, _) :: _ ->
      complete_stmts1 div tokens [ END_ACCEPT ; END_CALL ; END_DELETE ;
                                   END_DISPLAY ; END_RECEIVE ; END_SEND ]

    (* [ON EXCEPTION block] can happen in ACCEPT, CALL, DELETE,
       DISPLAY, RECEIVE, SEND *)
    | (ON, _) :: (EXCEPTION, _) :: _ ->
      complete_stmts2 div tokens [ END_ACCEPT ; END_CALL ; END_DELETE ;
                                   END_DISPLAY ; END_RECEIVE ; END_SEND ]

    (* [NOT EXCEPTION block] can happen in ACCEPT, CALL,
       DELETE, DISPLAY, RECEIVE, SEND *)
    | (NOT, _) :: (EXCEPTION, _) :: _ ->
      complete_stmts2 div tokens [ END_ACCEPT ; END_CALL ; END_DELETE ;
                                   END_DISPLAY ; END_RECEIVE ; END_SEND ]

    (* [NOT ON EXCEPTION block] can happen in ACCEPT, CALL,
       DELETE, DISPLAY, RECEIVE, SEND *)
    | (NOT, _) :: (ON, _) :: (EXCEPTION, _) :: _ ->
      complete_stmts3 div tokens [ END_ACCEPT ; END_CALL ; END_DELETE ;
                                   END_DISPLAY ; END_RECEIVE ; END_SEND ]

    (* [OVERFLOW block] can happen in STRING, UNSTRING *)
    | (OVERFLOW, _) :: _ ->
      complete_stmts1 div tokens [
        END_STRING ; END_UNSTRING ]

    (* [ON OVERFLOW block] can happen in STRING, UNSTRING *)
    | (ON, _) :: (OVERFLOW, _) :: _ ->
      complete_stmts2 div tokens [
        END_CALL ; (* <- only for MicroFocus *)
        END_STRING ; END_UNSTRING ]

    (* [NOT OVERFLOW block] can happen in STRING, UNSTRING *)
    | (NOT, _) :: (OVERFLOW, _) :: _ ->
      complete_stmts2 div tokens [ END_STRING ; END_UNSTRING ]

    (* [NOT ON OVERFLOW block] can happen in STRING, UNSTRING *)
    | (NOT, _) :: (ON, _) :: (OVERFLOW, _) :: _ ->
      complete_stmts3 div tokens [ END_STRING ; END_UNSTRING ]

    (* [END block] can happen in READ, RETURN, SEARCH *)
    | (END, _) :: _ ->
      complete_stmts1 div tokens [ END_READ ; END_RETURN ; END_SEARCH ]

    (* [AT END block] can happen in READ, RETURN, SEARCH *)
    | (AT, _) :: (END, _) :: _ ->
      complete_stmts2 div tokens [ END_READ ; END_RETURN ; END_SEARCH ]

    (* [NOT END block] can happen in READ, RETURN, SEARCH *)
    | (NOT, _) :: (END, _) :: _ ->
      complete_stmts2 div tokens [ END_READ ; END_RETURN ; END_SEARCH ]

    (* [NOT AT END block] can happen in READ, RETURN, SEARCH *)
    | (NOT, _) :: (AT, _) :: (END, _) :: _ ->
      complete_stmts3 div tokens [ END_READ ; END_RETURN ; END_SEARCH ]

    (* [END_OF_PAGE block] can happen in WRITE *)
    | (END_OF_PAGE, _) :: _ ->
      complete_stmts1 div tokens [ END_WRITE ]

    (* [AT END_OF_PAGE block] can happen in WRITE *)
    | (AT, _) :: (END_OF_PAGE, _) :: _ ->
      complete_stmts2 div tokens [ END_WRITE ]

    (* [NOT END_OF_PAGE block] can happen in WRITE *)
    | (NOT, _) :: (END_OF_PAGE, _) :: _ ->
      complete_stmts2 div tokens [ END_WRITE ]

    (* [NOT AT END_OF_PAGE block] can happen in WRITE *)
    | (NOT, _) :: (AT, _) :: (END_OF_PAGE, _) :: _ ->
      complete_stmts3 div tokens [ END_WRITE ]

    (* [NOT INVALID {KEY} block] can happen in DELETE, READ,
       REWRITE, START, WRITE *)
    | (NOT, _) :: (INVALID, _) :: _ ->
      complete_stmts2 div tokens [ END_DELETE ; END_READ ; END_REWRITE ;
                                   END_START ; END_WRITE ]

    (* [INVALID {KEY} block] can happen in DELETE, READ, REWRITE,
       START, WRITE *)
    | (INVALID, _) :: _ ->
      complete_stmts1 div tokens [ END_DELETE ; END_READ ; END_REWRITE ;
                                   END_START ; END_WRITE ]


    (* [NO/WITH DATA block] can happen in RECEIVE (deprecated ?) *)
    | ( (NO | WITH), _) :: (DATA, _) :: _ ->
      complete_stmts2 div tokens [ END_RECEIVE ]


    (* [WHEN ... block] can happen in EVALUATE, SEARCH *)
    | (WHEN as token, e) :: tokens ->
      continue_stmts div [ END_EVALUATE ; END_SEARCH ; END_PERFORM ] ;
      set_token_indent token e;
      iter_proc div tokens;

    | (FINALLY as token, e) :: tokens ->
      continue_stmts div [ END_PERFORM ] ;
      set_token_indent token e;
      iter_proc div tokens;

    | [] -> ()
    | _ -> iter tokens

  and complete_stmts1 div tokens end_tokens =
    match tokens with
    | (token1, e1) :: tokens ->
      complete_stmts div end_tokens ;
      set_token_indent token1 e1;
      incr () ;
      iter_proc div tokens;
    | _ -> assert false

  and complete_stmts2 div tokens end_tokens =
    match tokens with
    | (token1, e1) ::
      (token2, e2) :: tokens ->
      complete_stmts div end_tokens ;
      set_token_indent token1 e1;
      incr () ;
      set_token_indent token2 e2;
      iter_proc div tokens;
    | _ -> assert false

  and complete_stmts3 div tokens end_tokens =
    match tokens with
    | (token1, e1) ::
      (token2, e2) ::
      (token3, e3) ::
      tokens ->
      complete_stmts div end_tokens ;
      set_token_indent token1 e1;
      incr () ;
      set_token_indent token2 e2;
      set_token_indent token3 e3;
      iter_proc div tokens;
    | _ -> assert false

  and complete_stmts4 div tokens end_tokens =
    match tokens with
    | (token1, e1) ::
      (token2, e2) ::
      (token3, e3) ::
      (token4, e4) ::
      tokens ->
      complete_stmts div end_tokens ;
      set_token_indent token1 e1;
      incr () ;
      set_token_indent token2 e2;
      set_token_indent token3 e3;
      set_token_indent token4 e4;
      iter_proc div tokens;
    | _ -> assert false

  in
  begin_line tokens ;
  if verbose then
    Printf.eprintf "\n%!"
