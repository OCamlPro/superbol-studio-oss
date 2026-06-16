%{
(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open MiNode
%}

%token CARET
%token STAR PLUS EQUALS
%token TILDE AT AMP
%token COMMA LBRACE RBRACE LBRACKET RBRACKET
%token <string> IDENT
%token <string> STRING
%token <int> INT
%token EOL EOF

%start <MiNode.t> node

%%

node:
  | token_opt CARET IDENT results_opt eol
      { Result { token = $1;
                 class_ = resultClassOfString $3;
                 results = $4 } }

  | token_opt STAR IDENT results_opt eol
      { Async { token = $1;
                kind = Exec;
                class_ = asyncClassOfString $3;
                results = $4 } }

  | token_opt PLUS IDENT results_opt eol
      { Async { token = $1;
                kind = Status;
                class_ = asyncClassOfString $3;
                results = $4 } }

  | token_opt EQUALS IDENT results_opt eol
      { Async { token = $1;
                kind = Notify;
                class_ = asyncClassOfString $3;
                results = $4 } }

  | TILDE STRING eol
      { Stream { kind = Console; output = $2 } }

  | AT STRING eol
      { Stream { kind = Target; output = $2 } }

  | AMP STRING eol
      { Stream { kind = Log; output = $2 } }

token_opt:
  | /* epsilon */ { None }
  | INT           { Some $1 }

results_opt:
  | /* epsilon */        { [] }
  | COMMA key_value_list { $2 }

value:
  | STRING                           { Value.String ($1) }
  | LBRACKET RBRACKET                { Value.List (Empty) }
  | LBRACKET value_list RBRACKET     { Value.List (Values ($2)) }
  | LBRACKET key_value_list RBRACKET { Value.List (KeyValues ($2)) }
  | LBRACE RBRACE                    { Value.Tuple ([]) }
  | LBRACE key_value_list RBRACE     { Value.Tuple ($2) }

key_value:
  | IDENT EQUALS value { ($1, $3) }

value_list:
  | value                  { [$1] }
  | value_list COMMA value { $1 @ [$3] }

key_value_list:
  | key_value                      { [$1] }
  | key_value_list COMMA key_value { $1 @ [$3] }

eol:
  | EOL {}
  | EOF {}

%%
