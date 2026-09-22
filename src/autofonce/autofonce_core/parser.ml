(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open EzCompat (* for IntMap *)
open Ez_file.V1
open EzFile.OP
open Types

open Autofonce_m4.M4Types
module M4Printer = Autofonce_m4.M4Printer
module M4Parser = Autofonce_m4.M4Parser
module M4Types = Autofonce_m4.M4Types
module Misc = Autofonce_misc.Misc

let name_of_loc loc =
  M4Printer.string_of_location { loc with file = Filename.basename loc.file }

let int_of_string macro n =
  try
    int_of_string n
  with _ ->
    M4Parser.macro_error macro "int_of_string (%S)" n

type keep_files =
    KeepNone
  | KeepCopy
  | KeepLink

let load_file filename =
  (* Printf.eprintf "Loading tests from %s\n%!" filename; *)
  match M4Parser.parse_file filename with
  | exception ( M4Types.Error ( s, loc ) ) ->
      Printf.eprintf "%s:%d:%d: Error %s\n%!"
        loc.file loc.line loc.char s;
      exit 2
  | macros -> macros

type state = {
  keep_files : keep_files ;
  env_acc : string ;
  banner : string ;
  subst : string list ;
}

let bool_of_string bool macro =
  match String.lowercase_ascii bool with
  | "true" -> true
  | "false" -> false
  | _ ->
      M4Parser.macro_error macro
        "Could not parse boolean %S" bool

let load_file ~dirs ~keep_files ~path c filename =
  let rec iter_state s macros =

    let rec iter macros =
      match macros with
      | [] -> ()
      | macro :: macros ->
          match macro.kind with
          | Shell cmd   ->
              Printf.eprintf "At %s:\n  Discarding toplevel shell line: %s\n%!"
                (M4Printer.string_of_location macro.loc) cmd;
              iter macros
          | Macro ("m4_include", [ filename ]) ->
              let filename = M4Parser.to_string filename in
              let dirname = Filename.dirname macro.loc.file in
              let path = dirname :: path in
              let filename =
                try
                  Misc.find_in_path path filename
                with Not_found ->
                  M4Parser.macro_error macro
                    "Could not find file %s in path at 'm4_include' macro"
                    filename
              in
              let new_macros = load_file filename in

              (* AF_ENV and AF_COPY_ALL should not be propagated outside, so
                 use two different calls *)
              iter new_macros ;
              iter macros

          | Macro ( ( "AC_DEFUN" | "m4_define" ), [ macro_name ; macro_value ]) ->
              let macro_name = M4Parser.to_string macro_name in
              let macro_value = M4Parser.to_string macro_value in
              begin
                match macro_name, macro_value with
                | "AF_ENV", "$1" -> ()
                | _ ->
                    Printf.eprintf
                      "At %s:\n  Discarding macro definition of %S\n%!"
                      (M4Printer.string_of_location macro.loc) macro_name;
              end;
              iter macros
          | Macro ( ( "AF_COPYRIGHT" | "AT_COPYRIGHT" ), [ copyright ]) ->
              let copyright = M4Parser.to_string copyright in
              c.suite_copyright <- copyright ;
              iter macros
          | Macro ( ( "AF_ENV" | "AT_ENV" ) , [ env ]) ->
              let env = M4Parser.to_string env in
              let env_acc = Printf.sprintf "%s\n%s" s.env_acc env in
              iter_state { s with env_acc } macros
          | Macro ( ( "AF_INIT" | "AT_INIT" ) , [ name ]) ->
              let name = M4Parser.to_string name in
              c.suite_name <- name ;
              iter macros
          | Macro ( ( "AF_COLOR_TESTS" | "AT_COLOR_TESTS" ), [ ]) ->
              iter macros
          | Macro ( ( "AF_TESTED" | "AT_TESTED" ) , [ tested ]) ->
              let tested = M4Parser.to_string tested in
              c.suite_tested_programs <- c.suite_tested_programs @
                                         EzString.split_simplify tested ' ';
              iter macros
          | Macro ( ( "AF_BANNER" | "AT_BANNER" ), [ banner ]) ->
              let banner = M4Parser.to_string banner in
              c.suite_banners <- banner :: c.suite_banners ;
              let s = { s with banner } in
              iter_state s macros

          | Macro ( ( "AF_COPY_ALL" | "AT_COPY_ALL" ), [ bool ]) ->
              let bool = M4Parser.to_string bool in
              let keep_files = bool_of_string bool macro in
              let keep_files =
                if keep_files then KeepCopy
                else KeepNone
              in
              let s = { s with keep_files } in
              iter_state s macros

          | Macro ( ( "AF_LINK_ALL" | "AT_LINK_ALL" ), [ bool ]) ->
              let bool = M4Parser.to_string bool in
              let keep_files = bool_of_string bool macro in
              let keep_files =
                if keep_files then KeepLink
                else KeepNone
              in
              let s = { s with keep_files } in
              iter_state s macros

          | Macro ( ( "AF_SUBST" | "AT_SUBST" ), subst) ->
              let subst = List.map M4Parser.to_string subst in
              let s = { s with subst } in
              iter_state s macros

          | Comment _string ->
              iter_state s macros

          | Macro ( ( "AF_SETUP" | "AT_SETUP" ), [ name ]) ->
              c.suite_ntests <- c.suite_ntests + 1;
              let test_name = M4Parser.to_string name in
              let test_id = c.suite_ntests in
              let t = {
                test_suite = c;
                test_loc = macro.loc ;
                test_name ;
                test_id;
                test_env = s.env_acc;
                test_keywords = [];
                test_actions = [];
                test_banner = s.banner ;
                test_subst = s.subst ;
                test_keywords_set = StringSet.empty ;
                test_regen = false ;
              }
              in
              let steps = ref [0] in
              Hashtbl.add c.suite_test_by_id test_id t;
              c.suite_tests <- t :: c.suite_tests ;
              let before =
                match s.keep_files with
                | KeepNone -> []
                | KeepCopy ->
                    let files = list_files macro in
                    [ at_file steps macro ~copy:true ~promote:false files ]
                | KeepLink ->
                    let files = list_files macro in
                    [ at_file steps macro ~copy:true ~promote:false files ]
              in
              let macros, actions = iter_actions t steps [] macros in
              t.test_actions <- before @ actions ;
              let test_name = String.map (fun c ->
                  match c with
                  | 'a'..'z'
                  | 'A'..'Z'
                  | '-'
                  | '0'..'9' -> c
                  | _ -> ' ') t.test_name
              in
              let test_keywords =
                Filename.basename t.test_loc.file ::
                t.test_keywords
                @ EzString.split_simplify test_name ' '
              in
              let test_keywords =
                List.map String.lowercase_ascii test_keywords
              in
              t.test_keywords_set <- StringSet.of_list test_keywords ;
              iter_state s macros

          | Macro (_, _) ->
              M4Parser.macro_error macro "At top, unexpected macro %S"
                (M4Printer.string_of_macro macro)

    and list_files ( macro : statement ) =
      let dir = Filename.dirname macro.loc.file in
      match StringMap.find dir !dirs with
      | files -> files
      | exception Not_found ->
          let files = Sys.readdir dir |> Array.to_list in
          let files = List.sort compare files in
          let files = List.filter (fun file ->
              not ( Filename.check_suffix file ".at" )
            ) files
          in
          dirs := StringMap.add dir files !dirs ;
          files

    and iter_actions t steps actions macros =
      match macros with
      | [] -> [], List.rev actions
      | macro :: macros ->
          match macro.kind with

          | Macro ( ( "AF_KEYWORDS" | "AT_KEYWORDS" ), [ keywords]) ->
              let keywords = M4Parser.to_string keywords in
              t.test_keywords <- t.test_keywords
                                 @ EzString.split_simplify keywords ' ';
              iter_actions t steps actions macros

          | Macro ( ( "AF_CLEANUP" | "AT_CLEANUP" ) , [] ) ->
              macros,
              List.rev ( AT_CLEANUP { loc = macro.loc } :: actions )

          | Shell s when EzString.starts_with s ~prefix:"if "
                      && EzString.ends_with s ~suffix:"then"
            ->
              let len = String.length s in
              let test = String.sub s 3 (len-3-4) in
              parse_if t steps actions macro.loc test macros

          | _ ->
              let action = parse_action t steps macro in
              iter_actions t steps ( action :: actions ) macros

    and parse_if t steps actions check_loc check_command macros =
      let rec iter list macros =
        match macros with
        | [] -> failwith "unterminated if"
        | macro :: macros ->
            match macro.kind with
            | Shell "else" -> "else", List.rev list, macros
            | Shell "fi" -> "fi", List.rev list,  macros
            | _ -> iter (macro::list) macros
      in
      let (k, then_, macros) = iter [] macros in
      let (k, else_, macros) = if k = "fi" then k, [], macros else
          iter [] macros in
      assert (k <> "else");
      let action =
        (* Printf.eprintf "Shell: [ %s ]\n%!" cmd;*)
        AT_CHECK {
          check_step = next_step steps ;
          check_kind = "IF" ;
          check_command ;
          check_loc ;
          check_retcode = Some 0 ;
          check_stdout = Ignore ;
          check_stderr = Ignore ;
          check_test = t;
          check_run_if_pass = parse_actions t steps else_ ;
          check_run_if_fail = parse_actions t steps then_ ;
        }
      in
      iter_actions t steps (action :: actions) macros

    and parse_action t steps macro =
      match macro.kind with
      | Macro ( ( "AF_DATA" | "AT_DATA" ) , [ file ; content]) ->
          let file = M4Parser.to_string file in
          let content = M4Parser.to_string content in
          AT_DATA  { file ; content }

      | Macro ( ( "AF_CAPTURE_FILE" | "AT_CAPTURE_FILE" ) , [ file ] ) ->
          let file = M4Parser.to_string file in
          AT_CAPTURE_FILE  file

      | Macro ( ( "AF_XFAIL_IF" | "AT_XFAIL_IF" ) , [ test ]) ->
          let test = M4Parser.to_string test in
          begin
            match test with
            | "true" -> AT_XFAIL
            | command ->
                let step = next_step steps in
                let loc = macro.loc in
                AT_XFAIL_IF { step ; loc ; command }
          end

      | Macro ( ( "AF_SKIP_IF" | "AT_SKIP_IF" ) , [ test ]) ->
          let test = M4Parser.to_string test in
          begin
            match test with
            | "true" -> AT_SKIP
            | command ->
                let step = next_step steps in
                let loc = macro.loc in
                AT_SKIP_IF { step ; loc ; command }
          end

      | Macro ( ( "AF_FAIL_IF" | "AT_FAIL_IF" ), [ test ]) ->
          let test = M4Parser.to_string test in
          let loc = macro.loc in
          begin
            match test with
            | "true" -> AT_FAIL { loc }
            | command ->
                let step = next_step steps in
                AT_FAIL_IF { step ; loc ; command }
          end

      | Macro ( ( "AF_CHECK" | "AT_CHECK" ), args ) ->
          let check_loc = macro.loc in
          let check_command, args =
            match args with
            | [] -> M4Parser.macro_error macro "Missing argument 1 to AT_CHECK"
            | command :: args ->
                M4Parser.to_string command, args
          in
          let check_retcode, args =
            match args with
            | [] -> Some 0, []
            | { arg = "ignore" | "[ignore]" ; _ } :: args ->
                None, args
            | num :: args ->
                Some (
                  int_of_string macro
                    ( M4Parser.to_string num )), args
          in
          let check_stdout, args =
            match args with
            | [] ->
                Content "", []
            | arg :: args ->
                let arg = M4Parser.to_string arg in
                let arg = match arg with
                  | "ignore" -> Ignore
                  | "stdout" -> Save_to_file arg
                  | "expout" -> Diff_with_file arg
                  | s -> Content s
                in
                arg, args
          in
          let check_stderr, args =
            match args with
            | [] -> Content "", []
            | arg :: args ->
                let arg = M4Parser.to_string arg in
                let arg = match arg with
                  | "ignore" -> Ignore
                  | "stderr" -> Save_to_file arg
                  | "experr" -> Diff_with_file arg
                  | s -> Content s
                in
                arg, args
          in
          let check_run_if_fail, args =
            match args with
            | [] -> [], []
            | arg :: args ->
                let macros = M4Parser.parse_string
                    ~loc:arg.arg_loc arg.arg in
                parse_actions t steps macros, args
          in
          let check_run_if_pass, args =
            match args with
            | [] -> [], []
            | arg :: args ->
                let macros = M4Parser.parse_string
                    ~loc:arg.arg_loc arg.arg in
                parse_actions t steps macros, args
          in
          begin
            match args with
            | [] -> ()
            | _ ->
                M4Parser.macro_error macro "extra arguments to AT_CHECK"
          end;
          let check_step = next_step steps in
          AT_CHECK {
            check_test = t ;
            check_kind = "CHECK" ;
            check_loc ;
            check_step ;
            check_command ;
            check_retcode ;
            check_stdout ;
            check_stderr ;
            check_run_if_fail ;
            check_run_if_pass }

(*
    | Macro ("MANUAL_CHECK", _ ) ->
        (* TODO *)
        assert false
*)

      | Shell check_command ->
          (* Printf.eprintf "Shell: [ %s ]\n%!" cmd;*)
          AT_CHECK {
            check_step = next_step steps ;
            check_kind = "SHELL" ;
            check_command ;
            check_loc = macro.loc ;
            check_retcode = Some 0 ;
            check_stdout = Ignore ;
            check_stderr = Ignore ;
            check_test = t;
            check_run_if_pass = [] ;
            check_run_if_fail = [] ;
          }

      | Comment comment ->
          AF_COMMENT comment

      (* Extensions *)
      | Macro ( ( "AF_ENV" | "AT_ENV" ), [ env ] ) ->
          AF_ENV ( M4Parser.to_string env )

      | Macro ( ( "AF_COPY" | "AT_COPY" ), files ) ->
          let files = List.map M4Parser.to_string files in
          at_file steps macro ~copy:true files

      | Macro ( ( "AF_LINK" | "AT_LINK" ), files ) ->
          let files = List.map M4Parser.to_string files in
          at_file steps macro ~copy:false files

      (* Unknown macros *)
      | Macro (_, _) ->
          M4Parser.macro_error macro
            "unexpected test macro %S\n%!"
            (M4Printer.string_of_macro macro)

    and next_step steps =
      let step = String.concat "_"
          ( List.map ( Printf.sprintf "%02d" ) ( List.rev !steps ) )
      in
      steps := ( match !steps with
          | [] -> assert false
          | step :: tail -> (step+1) :: tail
        );
      step

    and parse_actions t steps macros =
      let former_steps = !steps in
      steps := 0 :: former_steps ;
      let macros, actions =
        iter_actions t steps [] macros in
      assert (macros = []);
      steps := former_steps;
      actions

    and at_file steps macro ~copy ?(promote=true) files =
      let dir = Filename.dirname macro.loc.file in
      let check_kind = if copy then "COPY" else "LINK" in
      let command =
        String.concat " && "
          ( List.map (fun file ->
                let filename = dir // file in
                if not (Sys.file_exists filename ) then
                  M4Parser.macro_error macro
                    "AF_COPY: file %S does not exist" filename;
                let basename = Filename.basename file in
                begin
                  match basename with
                  | "." | ".." ->
                      M4Parser.macro_error macro
                        "AF_%s file %s has no basename"
                        check_kind
                        file
                  | _ ->
                      Printf.sprintf "%s %s %s"
                        (if copy then "cp -R" else "ln -s")
                        filename
                        basename
                end
              ) files )
      in
      let step = next_step steps in
      let loc = macro.loc in
      AF_COPY { step ; loc ; command ; files ; copy ; promote }
    in
    iter macros
  in
  let macros = load_file filename in
  let s = {
    env_acc = "" ;
    keep_files = keep_files ;
    banner = "" ;
    subst = [] ;
  } in
  iter_state s macros;
  ()

let read ?(path=[]) filename =
  let suite_dir = Filename.dirname filename in
  let c = {
    suite_name = "";
    suite_ntests  = 0;
    suite_test_by_id = Hashtbl.create 1000 ;
    suite_tests = [] ;
    suite_tested_programs = [] ;
    suite_copyright = "" ;
    suite_banners = [];
    suite_file = Filename.basename filename;
    suite_dir ;
  }
  in
  let dirs = ref StringMap.empty in
  if Sys.is_directory filename then
    let select = EzFile.select ~deep:true ~glob:"*.at" () in
    EzFile.iter_dir ~select filename ~f:(fun file ->
        let filename = filename // file in
        load_file ~dirs ~keep_files:KeepCopy ~path c filename
      )
  else
    load_file ~dirs ~keep_files:KeepNone ~path c filename;
  c.suite_banners <- List.rev c.suite_banners ;
  c.suite_tests <- List.rev c.suite_tests ;
  c

let m4_escape ?(can_quote=true) s =
  let len = String.length s in

  (* Find for each position if a matching closing par is available *)
  let pars = Array.make len 0 in
  let rec iter ~npars ~nbras ~has_quote ~has_impaired ~pos =
    if pos > 0 then
      let pos = pos - 1 in
      let c = s.[pos] in

      pars.(pos) <- npars;
      let npars =
        match c with
        | '(' -> if npars > 0 then npars-1 else 0
        | ')' -> npars+1
        | _ -> npars
      in

      let nbras = match c with
        | '[' -> nbras-1
        | ']' -> nbras+1
        | _ -> nbras
      in
      let has_quote = has_quote || nbras > 0 in
      let has_impaired = has_impaired || nbras < 0 in
      iter ~npars ~nbras ~has_quote ~has_impaired ~pos
    else
      let has_impaired = has_impaired || nbras > 0 in
      has_quote && not has_impaired
  in
  let should_quote = iter ~npars:0 ~nbras:0
      ~has_quote:false
      ~has_impaired:false ~pos:len in
  if can_quote && should_quote then
    Printf.sprintf "[[%s]]" s
  else
    let b = Buffer.create len in
    Buffer.add_char b '[';
    let rec iter pos was_space npars =
      if pos < len then
        let c = s.[pos] in
        let pos = pos+1 in
        match c with
        | '[' ->
            Buffer.add_string b "@<:@";
            iter pos false npars
        | ']' ->
            Buffer.add_string b "@:>@";
            iter pos false npars
        | '(' ->
            Buffer.add_string b (
              if pars.(pos-1) = 0 then
                "@{:@"
              else
                "("
            );
            iter pos false (npars+1)
        | ')' ->
            Buffer.add_string b (if npars>0 then ")" else "@:}@");
            let npars = if npars>0 then npars-1 else 0 in
            iter pos false npars
        | ' ' | '\t' | '\012' ->
            Buffer.add_char b c;
            iter pos true npars;
        | '\n' ->
            if was_space then
              Buffer.add_string b "@&t@";
            Buffer.add_char b c;
            iter pos false npars;
        | c ->
            Buffer.add_char b c;
            iter pos false npars;
    in
    iter 0 false 0 ;
    Buffer.add_char b ']';
    Buffer.contents b
