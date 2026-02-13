(**************************************************************************)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

%{
open Conf_ast
%}

%token <int> INT
%token <bool> BOOLEAN (* Because of `yes`/`no` *)
%token <string> IDENT
%token <string> ANY
%token <string> STRING
%token INCLUDE
%token COLON
%token STAR
%token EQ
%token PLUS

%token OK
%token WARNING
%token ARCHAIC
%token OBSOLETE
%token SKIP
%token IGNORE
%token ERROR
%token UNCONFORMABLE
%token EOF

%token RESERVED_WORDS
%token RESERVED
%token NOT_RESERVED
%token INTRINSIC_FUNCTION
%token NOT_INTRINSIC_FUNCTION
%token SYSTEM_NAME
%token NOT_SYSTEM_NAME
%token REGISTER
%token NOT_REGISTER

%start <Conf_ast.t> file

%%
let file :=
 | ~=rules; EOF; <>

let rules :=
 | r=rule; { [r] }
 | r=rule; rl=rules; { r::rl }

let rule :=
 | INCLUDE; COLON?; ~=STRING; <Include>
 | RESERVED_WORDS; COLON?; ~=any_or_string; <ReservedWords>
 | ~=word_rule; <>
 | key=IDENT; COLON?; value=value; { Value {key; value} }

let word_rule :=
 | RESERVED; COLON?; alias=any_or_string; o=STAR?; EQ; word=any_or_string;
  { let alias = match o with
      | Some _ -> Context (alias, word)
      | None -> Normal (alias, word)
    in
    Value {key = "reserved"; value = Alias alias} }
 | RESERVED; COLON?; value=any_or_string; o=STAR?;
  { Value {key = "reserved"; value =
    match o with
    | Some _ -> ContextWord value;
    | None -> String value; } }
 | key=word_key; COLON?; value=any_or_string;
  { Value {key; value = String value} }

let word_key :=
 | NOT_RESERVED; { "not-reserved" }
 | INTRINSIC_FUNCTION; { "intrinsic-function" }
 | NOT_INTRINSIC_FUNCTION; { "not-intrinsic-function" }
 | SYSTEM_NAME; { "system-name" }
 | NOT_SYSTEM_NAME; { "not-system-name" }
 | REGISTER; { "register" }
 | NOT_REGISTER; { "not-register" }

let any_or_string :=
 | ~=ANY; <>
 | ~=STRING; <>
 | ~=IDENT; <>

let value :=
 | ~=ANY; <Any>
 | ~=BOOLEAN; <Bool>
 | ~=INT; <Int>
 | ~=STRING; <String>
 | ~=IDENT; <Any>
 | ~=support; <Support>

let support :=
| ~=support_value; <Normal>
| PLUS; ~=support_value; <Additional>

let support_value :=
| OK;            { Ok }
| WARNING;       { Warning }
| ARCHAIC;       { Archaic }
| OBSOLETE;      { Obsolete }
| SKIP;          { Skip }
| IGNORE;        { Ignore }
| ERROR;         { Error }
| UNCONFORMABLE; { Unconformable }

