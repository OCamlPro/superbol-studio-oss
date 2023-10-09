%{
(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

    (* Directly imported from toml.7.0.1 *)

    open Internal_types
%}

(* OcamlYacc definitions *)
%token BANG
%token <bool> BOOL
%token <string> INTEGER
%token <string> FLOAT
%token <Internal_types.string_format * string> STRING_INLINE
%token <Internal_types.string_format * string> STRING_MULTILINE
%token <string> DATE
%token <string> KEY
%token LBRACK RBRACK LBRACE RBRACE EOF COMMA DOT
%token EQUAL INIT SET CLEAR
%token <string> COMMENT
%token EOL

%start toml

%type <string Internal_types.loc list * (
        (Internal_types.line * string Internal_types.loc list) list)> toml

%%
(* Grammar rules *)
toml:
 | maybe_eol keysValue_eol* group* EOF
   { $1, ( $2 @ List.flatten $3 ) }

group:
   | group_header_eol keysValue_eol* { $1 :: $2 }
 ;

group_header_eol:
  | group_header eol                 { ($1, $2) }
;

keysValue_eol:
  | keysValue eol                  { ($1, $2) }
;

maybe_eol:
  |                                { [] }
  | eol                            { $1 }
;

eol:
  | maybe_comment EOL maybe_eol          { $1 :: $3 }
;

  maybe_comment:
    | COMMENT                  { Internal_lexing.loc $sloc $1 }
    |                          { Internal_lexing.loc $sloc "" }
;

  group_header:
   | LBRACK LBRACK key_path RBRACK RBRACK {
       Internal_lexing.line $sloc @@ Array_item $3 }
   | LBRACK key_path RBRACK               {
       Internal_lexing.line $sloc @@ Table_item $2 }
   | LBRACK BANG INTEGER RBRACK               {
         Internal_lexing.line $sloc
                               @@ Error_item ( int_of_string $3 ) }
   | LBRACK RBRACK               {
         Internal_lexing.line $sloc @@ Table_item [] }
   | LBRACE RBRACE CLEAR value_loc {
     Internal_lexing.line $sloc @@ Set
                                     { bind_var = Internal_lexing.loc $sloc [];
                                       bind_op = OpUnset;
                                       bind_val = $4 }
       }

key:
 | STRING_INLINE { snd $1 }
 | KEY    { $1 }
 | FLOAT  { $1 }
 | INTEGER    { $1 }

key_path: k = separated_nonempty_list (DOT, key) { k }

op:
 | EQUAL { OpEqual }
 | INIT { OpInit }
 | SET { OpSet }
 | CLEAR { OpUnset }

keysValue:
 | key_path_loc op value_loc { Internal_lexing.line $sloc @@ Set
                                               { bind_var = $1 ;
                                                 bind_op = $2 ;
                                                 bind_val = $3 } }
value:
    BOOL { IBool($1) }
  | INTEGER { IInt $1 }
  | FLOAT { IFloat $1 }
  | STRING_INLINE { let (format, s) = $1 in IString(format,s) }
  | STRING_MULTILINE { let (format, s) = $1 in IString(format,s) }
  | DATE { IDate $1 }
  | LBRACK RBRACK { IArray [] }
  | LBRACK maybe_eol value_loc_eol array_end { IArray ( $3 :: $4 ) }
  | LBRACE separated_list(COMMA,
                          keys_set_value) RBRACE { ITable $2 }

key_path_loc :
  | key_path { Internal_lexing.loc $sloc $1 }

value_loc:
  | value { Internal_lexing.loc $sloc $1 }
 ;

value_loc_eol:
  | value_loc maybe_eol { $1 }
 ;

keys_set_value:
  | key_path_loc EQUAL value_loc   { { bind_var = $1 ;
                                       bind_op = OpEqual ;
                                       bind_val = $3 } }

array_end:
    comma_eol value_loc_eol array_end { $2 :: $3 }
  | comma_eol? RBRACK { [] }
 ;

comma_eol:
     COMMA maybe_eol { () }
;

%%
