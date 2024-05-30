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
open Cobol_common

let supplier tokens = 
  let state = ref tokens in 
  fun () ->
    let token, locstart, locend = match !state with
    | [] -> Grammar.EOF, Lexing.dummy_pos, Lexing.dummy_pos
    | t:: s -> state :=s ; t
  in
  token, locstart, locend

let init_checkpoint = Grammar.Incremental.main Lexing.dummy_pos

let parse text =        
  let module OM = Sql_overlay_manager in
  let tokens =
    List.fold_left (fun (tokens, prev_right_limit) w' ->
      match w'.payload with
      | Cobol_preproc.Text.TextWord tw ->
        let tokenizer ~loc:_ lexbuf =
          let s = Lexer.token lexbuf in
          (*let lstart, lend = OM.limits (Lazy.force loc) in
          s, lstart, lend*)
          s
        in
        let until (t(*, _lstart, _lend*)) =
          t = Grammar.EOF
        in

        let f (t': 'a with_loc) (tokens, prev_right_limit) =
          let t = t'.payload in
          let loc = t'.loc in
          let lstart, lend = OM.limits ((*Lazy.force*) loc) in
          (match prev_right_limit with
          | None -> ()
          | Some l -> OM.link_limits l lstart);
          (t, lstart, lend) :: tokens, Some lend
        in

        let tw' : string with_loc = Srcloc.locfrom tw w' in

        Tokenizing.fold_tokens ~tokenizer ~until ~f tw' 
          (tokens, prev_right_limit)
      | Cobol_preproc.Text.Separator s ->
        let t = match s with
          | ',' -> Grammar.COMMA
          | ';' -> Grammar.SEMICOLON
          | _ -> Grammar.SPECHAR(s)
        in
        let lstart, lend = OM.limits w'.loc in
        (match prev_right_limit with
        | None -> ()
        | Some l -> OM.link_limits l lstart);
        (t, lstart, lend) :: tokens, Some lend


      | pl ->
        let t = Format.asprintf "%a" Cobol_preproc.Text.pp_word pl in
        let lstart, lend = Srcloc.as_lexloc w'.loc in
        (Grammar.STRING t, lstart, lend) :: tokens,
        prev_right_limit

    ) ([], None) text |> fst |> List.rev
  in

  let ast = Grammar.MenhirInterpreter.loop (supplier tokens) init_checkpoint in
 (*  Format.fprintf Format.std_formatter "\n%a\n" Sql_ast.pp ast; *)
  ast
