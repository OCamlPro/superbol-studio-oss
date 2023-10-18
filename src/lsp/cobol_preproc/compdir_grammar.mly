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

%{
  open Compdir_tree
%}

%token EOL
%token <string> TEXT_WORD
%token <string> ALPHANUM
%token CDIR_SET                  [@keyword ">>SET", "$SET"]
%token CDIR_SOURCE               [@keyword ">>SOURCE", "$SOURCE"]
%token FORMAT                    [@keyword]
%token FREE                      [@keyword]                       (* +COB2002 *)
%token IS                        [@keyword]
%token SOURCEFORMAT              [@keyword]

%token <Text.text_word> INVALID_

(* Entry points *)

%start <Compdir_tree.directive> compiler_directive

%start <unit> _unused_symbols             (* <- used to supress some warnings *)

(* -------------------------------------------------------------------------- *)

%%

(* --------------------- DEDICATED UTILITIES -------------------------------- *)

let loc (X) ==
  | x = X; { x, $sloc }

(* --- Entry points --------------------------------------------------------- *)

let compiler_directive :=
  | ~ = source_format; EOL; < >
  | ~ = set_sourceformat; EOL; < >
  | ~ = set_generic; EOL; < >

(* --- >>SOURCE | $ SET SOURCEFORMAT ---------------------------------------- *)

let source_format :=
  | CDIR_SOURCE; FORMAT?; IS?; free = loc(FREE);
    { Source_format_is_free (snd free) }
  | CDIR_SOURCE; FORMAT?; IS?; i = text_word;
    { Source_format_is i }

let set_sourceformat :=
  | CDIR_SET; SOURCEFORMAT; i = loc(ALPHANUM);  (* elementary_string_literal? *)
    { Set_sourceformat i }

(* --- >>SET ... | $ SET ... ------------------------------------------------ *)

let set_generic :=
  | CDIR_SET; w = text_word;
    { Set w }

(* --- Misc ----------------------------------------------------------------- *)

let text_word ==                                    (* text-word with position *)
  | ~ = loc(TEXT_WORD); < >

_unused_symbols:
  | INVALID_
{ () }

%%
