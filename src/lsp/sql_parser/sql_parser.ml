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
  let tokens =
    List.fold_left (fun tokens w' ->
      match w'.payload with
      | Cobol_preproc.Text.TextWord tw ->
        let tokenizer ~loc lexbuf =
          let s = Lexer.token lexbuf in
          let lstart, lend = Srcloc.as_lexloc (Lazy.force loc) in
          s, lstart, lend
        in
        let until (t, _lstart, _lend) =
          t = Grammar.EOF
        in

        let f (t': 'a with_loc) tokens =
          let t = t'.payload in
          t :: tokens
        in

        let tw' : string with_loc = Srcloc.locfrom tw w' in

        Tokenizing.fold_tokens ~tokenizer ~until ~f tw' tokens
      | Cobol_preproc.Text.Separator s ->
        let t = match s with
          | ',' -> Grammar.COMMA
          | ';' -> Grammar.SEMICOLON
          | _ -> Grammar.SPECHAR(s)
      in
        let lstart, lend = Srcloc.as_lexloc w'.loc in
        (t, lstart, lend) :: tokens


      | pl ->
        let t = Format.asprintf "%a" Cobol_preproc.Text.pp_word pl in
        let lstart, lend = Srcloc.as_lexloc w'.loc in
        (Grammar.STRING t, lstart, lend) :: tokens

    ) [] text |> List.rev
  in

  let ast = Grammar.MenhirInterpreter.loop (supplier tokens) init_checkpoint in
  Format.fprintf Format.std_formatter "%a" Sql_ast.pp ast;
  ast
