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

open Cobol_common.Srcloc.INFIX
open Cobol_common.Srcloc.TYPES
open Grammar_tokens                              (* import token constructors *)

(* --- *)

let combined_tokens =
  (* /!\ WARNING: None of the constituents of combined tokens may be
     context-sensitive.

     Rationale: this would considerably complicate retokenization (which is
     necessary with the current solution to handle context-sensitive
     keywords) *)
  Hashtbl.of_seq @@
  Seq.map (fun (a, b) -> b, a) @@
  List.to_seq Text_keywords.combined_keywords

let pp_alphanum_string_prefix ppf Cobol_ptree.{ hexadecimal; quotation; str;
                                                runtime_repr } =
  if runtime_repr = Null_terminated_bytes then Fmt.char ppf 'Z';
  if hexadecimal then Fmt.char ppf 'X';
  match quotation with
  | Simple_quote -> Fmt.pf ppf "'%s" str
  | Double_quote -> Fmt.pf ppf "\"%s" str

let pp_token_string: token Pretty.printer = fun ppf ->
  let string s = Pretty.string ppf s
  and print format = Pretty.print ppf format in
  function
  | WORD w
  | WORD_IN_AREA_A w
  | PICTURE_STRING w
  | INFO_WORD w
  | DIGITS w
  | SINTLIT w -> string w
  | EIGHTY_EIGHT -> string "88"
  | FIXEDLIT (i, sep, d) -> print "%s%c%s" i sep d
  | FLOATLIT (i, sep, d, e) -> print "%s%c%sE%s" i sep d e
  | ALPHANUM a -> Cobol_ptree.pp_alphanum ppf a
  | ALPHANUM_PREFIX a -> pp_alphanum_string_prefix ppf a
  | NATLIT s -> print "N\"%s\"" s
  | BOOLIT b -> print "B\"%a\"" Cobol_ptree.pp_boolean b
  | COMMENT_ENTRY e -> print "%a" Fmt.(list ~sep:sp string) e
  | EXEC_BLOCK b -> Cobol_common.Exec_block.pp ppf b
  | INTERVENING_ c -> print "%c" c
  | t -> string @@
      try Text_lexer.show_token t
      with Not_found ->
      try Hashtbl.find combined_tokens t
      with Not_found -> "<unknown/unexpected token>"

let pp_token: token Pretty.printer = fun ppf ->
  let string s = Pretty.string ppf s
  and print format = Pretty.print ppf format in
  function
    | WORD w -> print "WORD[%s]" w
    | WORD_IN_AREA_A w -> print "WORD_IN_AREA_A[%s]" w
    | PICTURE_STRING w -> print "PICTURE_STRING[%s]" w
    | INFO_WORD s -> print "INFO_WORD[%s]" s
    | COMMENT_ENTRY _ as t -> print "COMMENT_ENTRY[%a]" pp_token_string t
    | EXEC_BLOCK _ as t -> print "EXEC_BLOCK[%a]" pp_token_string t
    | DIGITS i -> print "DIGITS[%s]" i
    | SINTLIT i -> print "SINT[%s]" i
    | FIXEDLIT (i, sep, d) -> print "FIXED[%s%c%s]" i sep d
    | FLOATLIT (i, sep, d, e) -> print "FLOAT[%s%c%sE%s]" i sep d e
    | INTERVENING_ c -> print "<%c>" c
    | tok when Text_keywords.is_intrinsic_token tok ->
        print "INTRINSIC_FUNC[%a]" pp_token_string tok
    | EOF -> string "EOF"
    | t -> pp_token_string ppf t

let pp_token': token with_loc Pretty.printer =
  Cobol_common.Srcloc.pp_with_loc pp_token

let pp_tokens : token with_loc list Pretty.printer =
  Pretty.list ~fopen:"@[" ~fclose:"@]" pp_token'

let pp_tokens_with_loc_info ?fsep : token with_loc list Pretty.printer =
  Pretty.list ~fopen:"@[" ?fsep ~fclose:"@]" begin fun ppf t ->
    Pretty.print ppf "%a@@%a"
      pp_token' t
      Cobol_common.Srcloc.pp_srcloc_struct ~@t
  end
