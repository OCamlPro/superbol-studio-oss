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
%token <Cobol_data.Literal.alphanum> ALPHANUM
%token <Cobol_data.Literal.boolean> BOOLLIT
%token <Cobol_data.Literal.fixed> FIXEDLIT

(* Note: use the lexer to distinguish punctuation *)
%token EQ              "="          [@keyword (* symbol *)  "="]
%token GE              ">="         [@keyword (* symbol *) ">="]
%token GT              ">"          [@keyword (* symbol *)  ">"]
%token LE              "<="         [@keyword (* symbol *) "<="]
%token LT              "<"          [@keyword (* symbol *)  "<"]
%token NE              "<>"         [@keyword (* symbol *) "<>"]

%token PERIOD          "."
%token LPAR            "("
%token RPAR            ")"

%token CDIR_DEFINE     [@keyword ">>DEFINE"]
%token CDIR_ELIF       [@keyword ">>ELIF", "$ELIF"           (* GC extensions *)
                        , ">>ELSE-IF", "$ELSE-IF"]
%token CDIR_ELSE       [@keyword ">>ELSE", "$ELSE"]
%token CDIR_END        [@keyword "$END"]       (* Note: no `>>END` equivalent *)
%token CDIR_END_IF     [@keyword ">>END-IF"
                        ,         "$END-IF"]    (* <- undocumented, but found *)
%token CDIR_IF         [@keyword ">>IF", "$IF"]
%token CDIR_SET        [@keyword ">>SET", "$SET"]
%token CDIR_SOURCE     [@keyword ">>SOURCE"]

%token ADDRSV          [@keyword "ADDRSV", "ADD-RSV"]
%token ADDSYN          [@keyword "ADDSYN", "ADD-SYN"]
%token AREACHECK       [@keyword "AREACHECK", "AREA-CHECK"]
%token AS              [@keyword]
%token ASSIGN          [@keyword]
%token BOUND           [@keyword]
%token CALLFH          [@keyword]
%token CHECKNUM        [@keyword "CHECKNUM", "CHECK-NUM"]
%token COMP1           [@keyword "COMP1", "COMP-1"]
%token CONSTANT        [@keyword]
%token DEFINED         [@keyword]
%token DPCINDATA       [@keyword "DPCINDATA", "DPC-IN-DATA"]
%token EQUAL           [@keyword]
%token FOLDCOPYNAME    [@keyword "FOLDCOPYNAME", "FOLD-COPY-NAME"]
%token FORMAT          [@keyword]
%token FREE            [@keyword]
%token GREATER         [@keyword]
%token IS              [@keyword]
%token LESS            [@keyword]
%token MAKESYN         [@keyword "MAKESYN", "MAKE-SYN"]
%token NESTCALL        [@keyword]
%token NOAREACHECK     [@keyword "NOAREACHECK", "NO-AREACHECK", "NO-AREA-CHECK"]
%token NOBOUND         [@keyword "NOBOUND", "NO-BOUND"]
%token NOCHECKNUM      [@keyword "NOCHECKNUM", "NO-CHECKNUM", "NO-CHECK-NUM"]
%token NODPC_IN_DATA   [@keyword "NODPCINDATA", "NO-DPCINDATA", "NODPC-IN-DATA", "NO-DPC-IN-DATA"]
%token NOFOLDCOPYNAME  [@keyword "NOFOLDCOPYNAME", "NOFOLD-COPY-NAME", "NO-FOLD-COPY-NAME"]
%token NOODOSLIDE      [@keyword "NOODOSLIDE", "NO-ODOSLIDE"]
%token NOSPZERO        [@keyword "NOSPZERO", "NO-SPZERO"]
%token NOSSRANGE       [@keyword "NOSSRANGE", "NO-SSRANGE"]
%token NOT             [@keyword]
%token ODOSLIDE        [@keyword]
%token OFF             [@keyword]
%token OR              [@keyword]
%token OVERRIDE        [@keyword]
%token PARAMETER       [@keyword]
%token REMOVE          [@keyword]
%token SET             [@keyword]
%token SOURCEFORMAT    [@keyword "SOURCEFORMAT", "SOURCE-FORMAT"]
%token SPZERO          [@keyword]
%token SSRANGE         [@keyword]
%token THAN            [@keyword]
%token TO              [@keyword]

%token <Text.text_word> INVALID_

(* Entry points *)

%start <Compdir_tree.directive> compiler_directive
(* %start <Compdir_tree.lexing_directive> source_directive *)
(* %start <Compdir_tree.preproc_directive> define_directive *)

%start <unit> _unused_symbols             (* <- used to supress some warnings *)

(* -------------------------------------------------------------------------- *)

%%

(* --------------------- DEDICATED UTILITIES -------------------------------- *)

let loc (X) ==
  | x = X; { Cobol_common.Srcloc.{ payload = x; loc = raw $sloc } }

(* --- Entry points --------------------------------------------------------- *)

let compiler_directive :=
  | CDIR_SOURCE; ~ = source_directive; <Lexing>
  | CDIR_SET; ~ = set_sourceformat; <Lexing>
  | CDIR_SET; ~ = set_directive; <Preproc>
  | CDIR_DEFINE; ~ = define_directive; <Preproc>
  | CDIR_IF; ~ = if_directive; <Preproc>
  | CDIR_ELIF; ~ = elif_directive; <Preproc>
  | CDIR_ELSE; EOL; { Preproc Else }
  | CDIR_END; EOL; { Preproc End }
  | CDIR_END_IF; EOL; { Preproc End_if }

(* --- >>SOURCE | $ SET SOURCEFORMAT ---------------------------------------- *)

let source_directive :=
  | ~ = source_format; PERIOD?; EOL; < >

let source_format :=
  | FORMAT?; IS?; free = loc(FREE);
    { Source_format_is_free free.Cobol_common.Srcloc.loc }
  | FORMAT?; IS?; i = text_word;
    { Source_format_is i }

let set_sourceformat :=
  | SOURCEFORMAT; i = loc(ALPHANUM); PERIOD?; EOL;       (* elementary_string_literal? *)
    { Set_sourceformat i }
  | SOURCEFORMAT; LPAR; i = loc(TEXT_WORD); RPAR; PERIOD?; EOL;
    { Set_sourceformat i }

(* --- >>SET ... | $ SET ... ------------------------------------------------ *)

let set_directive :=
  | ~ = set; PERIOD?; EOL; < >

let set :=
  | ~ = loc(set_operand); <Set>

let set_operand :=
  | ADDRSV;         {Add_srv}
  | ADDSYN;         {Add_syn}
  | AREACHECK;      {Area_check true}
  | ASSIGN;         {Assign}
  | BOUND;          {Bound true}
  | CALLFH;         {Call_FH}
  | CHECKNUM;       {Check_num true}
  | COMP1;          {Comp_1}
  | CONSTANT;       {Constant}
  | DPCINDATA;      {DPC_in_data true}
  | FOLDCOPYNAME;   {Fold_copy_name true}
  | MAKESYN;        {Make_syn}
  | NESTCALL;       {Nest_call}
  | NOAREACHECK;    {Area_check false}
  | NOBOUND;        {Bound false}
  | NOCHECKNUM;     {Check_num false}
  | NODPC_IN_DATA;  {DPC_in_data false}
  | NOFOLDCOPYNAME; {Fold_copy_name false}
  | NOODOSLIDE;     {ODO_slide false}
  | NOSPZERO;       {SP_zero false}
  | NOSSRANGE;      {SS_range false}
  | ODOSLIDE;       {ODO_slide true}
  | REMOVE;         {Remove}
  | SPZERO;         {SP_zero true}
  | SSRANGE;        {SS_range true}

(* --- >>DEFINE ... | $ DEFINE ... ------------------------------------------- *)

let define_directive :=
  | ~ = define; EOL; < >

let define :=
  | ~ = var; AS?; OFF; <Define_off>

  | var = var; AS?; value = loc(define_expr); o = bo(OVERRIDE);
    { Define { var; value; override = o } }

let define_expr :=
  | x = loc(ALPHANUM); {Literal_definition (Alphanum x)}
  | x = loc(BOOLLIT); {Literal_definition (Boolean x)}
  | x = loc(FIXEDLIT); {Literal_definition (Numeric x)}
  | PARAMETER; {Parameter_definition}

(* --- >>IF ... ------------------------------------------------------------- *)

let if_directive :=
  | ~ = if_; EOL; < >

let if_ :=
  | c = loc(boolexpr);
    { If c }

let elif_directive :=
  | ~ = elif; EOL; < >

let elif :=
  | c = loc(boolexpr);
    { Elif c }

let boolexpr :=
  | var = var; IS?; neg_polarity = ibo(NOT); DEFINED;
    { Defined_condition { var; polarity = not neg_polarity } }
  | neg_polarity = ibo(NOT); var = var;
    { Value_condition { var; polarity = not neg_polarity } }
  | var = var; IS?; neg_polarity = ibo(NOT); o = condition_operator; r = term;
    { Constant_condition { left_operand = Variable var; right_operand = r;
                           polarity = not neg_polarity; operator = o } }
  | l = literal; IS?; neg_polarity = ibo(NOT); o = condition_operator; r = term;
    { Constant_condition { left_operand = Literal l; right_operand = r;
                           polarity = not neg_polarity; operator = o } }

let term :=
  | ~ = var; <Variable>
  | ~ = literal; <Literal>

let condition_operator :=
  | GREATER; THAN?; OR; EQUAL; TO?; {Ge}
  | GREATER; THAN?; {Gt}
  | LESS; THAN?; OR; EQUAL; TO?; {Le}
  | LESS; THAN?; {Lt}
  | EQUAL; TO?; {Eq}
  | "="; {Eq}
  | ">"; {Gt}
  | ">="; {Ge}
  | "<="; {Le}
  | "<"; {Lt}
  | "<>"; {Ne}

(* --- Misc ----------------------------------------------------------------- *)

let text_word ==                                    (* text-word with position *)
  | ~ = loc(TEXT_WORD); < >

let var :=
  | v = text_word; { Preproc_env.var' v }

let literal :=
  | ~ = loc(ALPHANUM); <Alphanum>
  | ~ = loc(BOOLLIT); <Boolean>
  | ~ = loc(FIXEDLIT); <Numeric>

_unused_symbols:
  | INVALID_
  | ADDRSV
  | ADDSYN
  | AREACHECK
  | ASSIGN
  | BOUND
  | CALLFH
  | CHECKNUM
  | COMP1
  | CONSTANT
  | DPCINDATA
  | EQUAL
  | FOLDCOPYNAME
  | GREATER
  | LESS
  | MAKESYN
  | NOAREACHECK
  | NOBOUND
  | NOCHECKNUM
  | NODPC_IN_DATA
  | NOFOLDCOPYNAME
  | NOODOSLIDE
  | NOSPZERO
  | NOSSRANGE
  | ODOSLIDE
  | OR
  | REMOVE
  | SET
  | SPZERO
  | SSRANGE
  | THAN
  | TO
{ () }

%%
