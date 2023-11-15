%{
(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Grammar_utils
open Cobol_ptree
open Cobol_ptree.Terms_helpers
open Cobol_common.Srcloc.INFIX

let split_last l =
  List.(let rl = rev l in hd rl, rev (tl rl))

let with_loc token location_limits =
  token &@ Grammar_utils.Overlay_manager.join_limits location_limits

let dual_handler_none =
  { dual_handler_pos = []; dual_handler_neg = [] }
%}

(* Tokens are listed in `grammar_tokens.mly' *)
%[@printer.header open Grammar_tokens]
%[@context.header open Grammar_contexts]

(* --- Post-actions --- *)

(* Post-actions are functions with pre-defined argument and return types, that
   can be executed whenever a production is reduced.  Post-actions always
   receive as first argument the semantic value that is produced.

   These actions may be specified using attributes attached to a rule as a whole
   (in which case the action is executed whenever any production defined within
   the rule is reduced).  Alternatively, a post-action may be attached to a
   single production by means of an attribute to the last item of its producers.
   When both attributes are given, the latter takes precedence.  *)

(* (\* Parameter module specific to post-actions *\) *)
(* %[@post.parameter Config: Cobol_config.T] *)

(* Tag declaration for post-actions.

   For now we only define a single kind of post-actions, that receives an
   optional source code location in addition to the semantic value (which is
   implicit in the declaration below), and returns a diagnostics result.  With
   this definition, grammar attributes "[@post.diagnostic ...]" may be used to
   create diagnostics based on the result of configuration feature
   verifications. *)
(* %[@post.tag diagnostic loc:Cobol_common.Srcloc.srcloc option -> *)
(*                            unit Cobol_common.Diagnostics.in_result] *)

%[@post.tag special_names Cobol_ptree.special_names_clause]

%[@post.tag pending string]

(* --- Recovery helpers --- *)

%[@recovery.header
  open Cobol_common.Srcloc.INFIX

  let dummy_loc =
    Grammar_utils.Overlay_manager.(join_limits (dummy_limit, dummy_limit))

  let dummy_string = "_" &@ dummy_loc
  let dummy_name = dummy_string

  let dummy_qualname: Cobol_ptree.qualname =
    Cobol_ptree.Name dummy_name

  let dummy_qualident =
    Cobol_ptree.{ ident_name = dummy_qualname;
                  ident_subscripts = [] }

  let dummy_ident =
    Cobol_ptree.QualIdent dummy_qualident

  let dummy_expr =
    Cobol_ptree.Atom (Fig Zero)

  let dummy_picture =
    Cobol_ptree.{ picture = "X" &@ dummy_loc;
                  picture_locale = None;
                  picture_depending = None }
]

%nonassoc lowest
%nonassoc ELSE

(* Set precedence of statements to be higher than imperative statement *)
(* This helps resolve conflicts in lists of statements, by prefering shift *)
%nonassoc ACCEPT
%nonassoc ADD
%nonassoc ALLOCATE
%nonassoc ALTER
%nonassoc CALL
%nonassoc CANCEL
%nonassoc CLOSE
%nonassoc COMPUTE
%nonassoc CONTINUE
%nonassoc DELETE
%nonassoc DISABLE
%nonassoc DISPLAY
%nonassoc DIVIDE
%nonassoc ENABLE
%nonassoc ENTER
%nonassoc EVALUATE
%nonassoc EXIT
%nonassoc FREE
%nonassoc GENERATE
%nonassoc GO
%nonassoc GOBACK
%nonassoc IF
%nonassoc INITIALIZE
%nonassoc INITIATE
%nonassoc INSPECT
%nonassoc INVOKE
%nonassoc MERGE
%nonassoc MOVE
%nonassoc MULTIPLY
%nonassoc OPEN
%nonassoc PERFORM
%nonassoc PURGE
%nonassoc RAISE
%nonassoc READ
%nonassoc RECEIVE
%nonassoc RELEASE
%nonassoc RESUME
%nonassoc RETURN
%nonassoc REWRITE
%nonassoc SEARCH
%nonassoc SEND
%nonassoc SET
%nonassoc SORT
%nonassoc START
%nonassoc STOP
%nonassoc STRING
%nonassoc SUBTRACT
%nonassoc SUPPRESS
%nonassoc TERMINATE
%nonassoc TRANSFORM
%nonassoc UNLOCK
%nonassoc UNSTRING
%nonassoc VALIDATE
%nonassoc WRITE

(* Precedence for empty terminators *)
%nonassoc no_term

(* Precedence higher than no_term so that we can keep shifting *)
%nonassoc EXCEPTION ON_EXCEPTION NOT_ON_EXCEPTION ON_SIZE_ERROR NOT_ON_SIZE_ERROR OVERFLOW ON_OVERFLOW NOT_ON_OVERFLOW INVALID_KEY NOT_INVALID_KEY AT_END NOT_AT_END AT_EOP END_OF_PAGE NOT_AT_EOP EOP DATA WITH_DATA (*NO_DATA*) END ON NEXT_PAGE

(* Set precedence of terminators to be higher than the absence of terminator *)
(* Allows to solve conflicts with nested terminated-statements *)
%nonassoc END_ACCEPT
%nonassoc END_ADD
%nonassoc END_CALL
%nonassoc END_COMPUTE
%nonassoc END_DELETE
%nonassoc END_DISPLAY
%nonassoc END_DIVIDE
%nonassoc END_EVALUATE
%nonassoc END_IF
%nonassoc END_MULTIPLY
%nonassoc END_READ
%nonassoc END_RECEIVE
%nonassoc END_RETURN
%nonassoc END_REWRITE
%nonassoc END_SEARCH
%nonassoc END_START
%nonassoc END_STRING
%nonassoc END_SUBTRACT
%nonassoc END_UNSTRING
%nonassoc END_WRITE
%nonassoc below_LINES
%nonassoc LINES
%nonassoc WORD TO FROM WHEN USING VALUE OPTIONAL SUM INVALID
%nonassoc BELL BLINK HIGHLIGHT LOWLIGHT REVERSE_VIDEO UNDERLINE
%nonassoc FOREGROUND_COLOR BACKGROUND_COLOR

%left OR
%left AND
%right AMPERSAND
%right DOUBLE_COLON AS

%nonassoc OF IN
%nonassoc below_RPAR
%nonassoc RPAR
%nonassoc LPAR
%nonassoc PLUS_SIGN
%nonassoc DASH_SIGN

(* Symbol types *)

%type <(statement, string) result> imperative_statement

(* Entry points *)

%start <Cobol_ptree.compilation_group> compilation_group
%start <condition> standalone_condition

%%

(* --------------------- DEDICATED UTILITIES -------------------------------- *)

let nel [@recovery []] [@symbol ""] (X) :=
 | x = X;             { [ x ] } %prec lowest
 | x = X; l = nel(X); { x :: l }

let rnel [@recovery []] [@symbol ""] (X) := ~ = nel (X); < >          (* alias *)

let loc_result (X) ==
  | res = loc (X); { Cobol_common.Srcloc.lift_result res }

let loc (X) ==
  | x = X; { x &@ Grammar_utils.Overlay_manager.join_limits $sloc }

let ioloc (X) ==
  |             {None}
  | ~ = loc(X); <Some>

(* --------------------- COMPILATION GROUPS AND UNITS ---------------------- *)

let compilation_group :=
  | control_division = option(loc(control_division));
    ul = ll(loc(compilation_unit));
    pdo = loc(program_definition_no_end)?; EOF;
    { { control_division;
        compilation_units =
          match pdo with
            | None -> ul
            | Some pd -> ul @ [((Program ~&pd): compilation_unit) &@<- pd] } }

(* --- CONTROL DIVISION --- *)

(* TODO: leave a flag/source location in the parse tree, and check support for
   CONTROL DIVISION later. *)
let control_division (* [@post.diagnostic fun _ -> Config.control_division#verify] *) :=
  | CONTROL; DIVISION; ".";
    option(default_section); {()} (* TODO: actually keep the section's contents *)

let default_section :=
  | DEFAULT; SECTION; "."; default_section_clauses

let default_section_clauses :=
  | option(mr(option(default_accept_clause);
              option(default_display_clause);
              "."))

let default_accept_clause :=
  | ACCEPT; IS?; word_or_terminal

let default_display_clause :=
  | DISPLAY; IS?; word_or_terminal

let word_or_terminal [@symbol "<word or TERMINAL>"] :=
  | WORD; {}
  | TERMINAL; {}

(* --- COMPILATION UNIT --- *)

let compilation_unit :=
 | ~ = program_prototype    ; <Program>
 | ~ = program_definition   ; <Program>
 | ~ = function_unit        ; <Function>
 | ~ = class_definition     ; <ClassDefinition>
 | ~ = interface_definition ; <InterfaceDefinition>


let program_definition_identification :=
 | h = identification_division_header;                    (* COB85: mandatory *)
   id = program_definition_id_paragraph;
   ip = informational_paragraphs;
   { h, id, ip }

let program_prototype_identification ==
 | ~ = identification_division_header; ~ = program_prototype_id_paragraph; < >

let function_identification :=
 | ~ = identification_division_header; ~ = function_id_paragraph; < >

let class_identification :=
 | ~ = identification_division_header; ~ = class_id_paragraph; < >

let factory_identification :=
 | ~ = identification_division_header; ~ = factory_paragraph; < >

let instance_identification :=
 | ~ = identification_division_header; ~ = object_paragraph; < >

let interface_identification :=
 | ~ = identification_division_header; ~ = interface_id_paragraph; < >

let method_identification :=
 | ~ = identification_division_header; ~ = method_id_paragraph; < >

let identification_division_header ==
  | IDENTIFICATION; DIVISION; ".";
    (* GnuCOBOL allows informational paragraphs before the `*-ID` entry; to
       simplify we allow them here for any kind of compilation unit (but ignore,
       except for normal programs, for now). *)
    ~ = informational_paragraphs; <Some>
  |                               {None}


program_definition [@cost 0]:
 | pd = program_definition_no_end
   pdl = loc(program_definition)* (* COB2002: PROCEDURE DIVISION must be present *)
   END PROGRAM ep = name "."
   { match pd.program_level with
       | ProgramDefinition { kind;
                             has_identification_division_header;
                             preliminary_informational_paragraphs;
                             supplementary_informational_paragraphs;
                             nested_programs = [] } ->
           { pd with
             program_level =
               ProgramDefinition { kind;
                                   has_identification_division_header;
                                   preliminary_informational_paragraphs;
                                   supplementary_informational_paragraphs;
                                   nested_programs = pdl };
             program_end_name = Some ep }
       | _ -> failwith "Cannot happen as per the grammar." }

program_definition_no_end:
 | pid = program_definition_identification
   opo = ro(loc(options_paragraph))
   edo = ro(loc(environment_division))
   ddo = ro(loc(data_division))
   pdo = ro(loc(program_procedure_division))
   { let h, ((program_name, program_as), kind), ip1 = pid in
     let ip0 = match h with None -> [] | Some h -> h in
     { program_name;
       program_as;
       program_level =
         ProgramDefinition { has_identification_division_header = h <> None;
                             preliminary_informational_paragraphs = ip0;
                             supplementary_informational_paragraphs = ip1;
                             nested_programs = []; kind };
       program_options = opo;
       program_env = edo;
       program_data = ddo;
       program_proc = pdo;
       program_end_name = None } }
(* Note: END PROGRAM is not mandatory on last top-level program
   if it does not contain nested programs (it may be used though) *)

program_prototype [@cost 999]:
 | pid = program_prototype_identification
   opo = ro(loc(options_paragraph))
   edo = ro(loc(environment_division))
   ddo = ro(loc(data_division))
   pdo = ro(loc(procedure_division))
   END PROGRAM ep = name "."
   { let _, (program_name, program_as) = pid in
     { program_name;
       program_as;
       program_level = ProgramPrototype;
       program_options = opo;
       program_env = edo;
       program_data = ddo;
       program_proc = pdo;
       program_end_name = Some ep } }

function_unit [@cost 999]:
 | fid = function_identification
   opo = ro(loc(options_paragraph))
   edo = ro(loc(environment_division))
   ddo = ro(loc(data_division))
   pdo = ro(procedure_division)
   END FUNCTION ef = name "."
   { let _, (name, as_, is_proto) = fid in
     { function_name = name;
       function_as = as_;
       function_is_proto = is_proto;
       function_options = opo;
       function_env = edo;
       function_data = ddo;
       function_proc = pdo;
       function_end_name = ef } } (* TODO: shoudn't we just check ef == name? *)

class_definition [@cost 999]:
 | cid = class_identification
   opo = ro(loc(options_paragraph))
   edo = ro(loc(environment_division))
   fdo = io(factory_definition) (* Note: inline to avoid conflict *)
   ido = ro(instance_definition)
   END CLASS ec = name "."
   { let _, (class_name, class_as, class_final,
             class_inherits, class_usings) = cid in
     { class_name;
       class_as;
       class_final;
       class_inherits;
       class_usings;
       class_options = opo;
       class_env = edo;
       class_factory = fdo;
       class_instance = ido;
       class_end_name = ec } }

factory_definition:
 | fp = factory_identification
   opo = ro(loc(options_paragraph))
   edo = ro(loc(environment_division))
   ddo = ro(loc(data_division))
   pdo = ro(object_procedure_division)
   END FACTORY "."
    { { factory_implements = snd fp;
        factory_options = opo;
        factory_env = edo;
        factory_data = ddo;
        factory_methods = pdo } }

instance_definition:
 | op = instance_identification
   opo = ro(loc(options_paragraph))
   edo = ro(loc(environment_division))
   ddo = ro(loc(data_division))
   pdo = ro(object_procedure_division)
   END OBJECT "."
    { { instance_implements = snd op;
        instance_options = opo;
        instance_env = edo;
        instance_data = ddo;
        instance_methods = pdo } }

interface_definition [@cost 999]:
 | iid = interface_identification
   opo = ro(loc(options_paragraph))
   edo = ro(loc(environment_division))
   pdo = ro(object_procedure_division)
   END INTERFACE ei = name "."
   { let _, (interface_name, interface_as,
             interface_inherits, interface_usings) = iid in
     { interface_name;
       interface_as;
       interface_inherits;
       interface_usings;
       interface_options = opo;
       interface_env = edo;
       interface_methods = pdo;
       interface_end_name = ei } }

method_definition: (* Note: used in PROCEDURE DIVISION, see below *)
 | mid = method_identification
   opo = ro(loc(options_paragraph))
   edo = ro(loc(environment_division))
   ddo = ro(loc(data_division))
   pdo = ro(procedure_division)
   END METHOD em = name "."
   { let _, (method_name, method_kind,
             method_override, method_final) = mid in
     { method_name;
       method_kind;
       method_override;
       method_final;
       method_options = opo;
       method_env = edo;
       method_data = ddo;
       method_proc = pdo;
       method_end_name = em } }


let informational_paragraphs :=                            (* ~COB85, -COB2002 *)
 rl(loc(informational_paragraph))

let informational_paragraph :=
  | ~ = informational_paragraph_header; "."; ~ = loc(comment_entry); < >

let informational_paragraph_header ==
  | AUTHOR;        {Author}
  | INSTALLATION;  {Installation}
  | DATE_WRITTEN;  {DateWritten}
  | DATE_MODIFIED; {DateModified}
  | DATE_COMPILED; {DateCompiled}
  | REMARKS;       {Remarks}
  | SECURITY;      {Security}

let info_word [@recovery "_"] [@symbol "<word>"] := INFO_WORD
let comment_entry [@recovery ["_"]] [@symbol "<comment entry>"] := COMMENT_ENTRY

let as__strlit_ := ~ = ro (pf (AS, string_literal)); < >

let program_id_header_prefix ==
  | PROGRAM_ID; "."; ~ = loc(info_word); ~ = as__strlit_; < >

let program_definition_id_paragraph [@context program_id_paragraph] :=
  | ids = program_id_header_prefix;
    pko = o(IS?; pk = program_kind; PROGRAM?; { pk }); ".";
    { ids, pko }

let program_kind :=
  | COMMON;     {Common}                      (* Only within a nested program *)
  | INITIAL;    {Initial}
  | RECURSIVE;  {Recursive}

let program_prototype_id_paragraph :=                              (* +COB2002 *)
  | ~ = program_id_header_prefix; IS?; PROTOTYPE; "."; < >

let function_id_paragraph :=
  | FUNCTION_ID; "."; i = name; slo = as__strlit_;
    proto = ibo(IS?; PROTOTYPE; {}); ".";                         (* +COB2002 *)
    { i, slo, proto }

let class_id_paragraph :=                                          (* +COB2002 *)
  | CLASS_ID; "."; i = name; slo = as__strlit_; f = bo(IS?; FINAL; {});
    il1 = lo(INHERITS; FROM?; il = names; { il });
    il2 = lo(USING; il = names; { il }); ".";
    { i, slo, f, il1, il2 }

let factory_paragraph [@context factory_paragraph] :=              (* +COB2002 *)
  | FACTORY; "."; ~ = lo(IMPLEMENTS; ~ = names; "."; < >); < >

let object_paragraph [@context object_paragraph] :=                (* +COB2002 *)
  | OBJECT; "."; ~ = lo(IMPLEMENTS; ~ = names; "."; < >); < >

let interface_id_paragraph :=                                      (* +COB2002 *)
  | INTERFACE_ID; "."; i = name; slo = as__strlit_;
    il1 = lo(INHERITS; FROM?; il = names; { il });
    il2 = lo(USING; il = names; { il }); ".";
    { i, slo, il1, il2 }

let method_id_paragraph :=                                         (* +COB2002 *)
  | METHOD_ID; "."; i = name; slo = as__strlit_;
    o = bo(OVERRIDE); f = bo(IS?; FINAL; {});
    { i, NamedMethod { as_ = slo }, o, f }
  | METHOD_ID; "."; pk = property_kind; PROPERTY; i = name;
    o = bo(OVERRIDE); f = bo(IS?; FINAL; {});
    { i, PropertyMethod { kind = pk }, o, f }


let options_paragraph [@context options_paragraph] :=              (* +COB2002 *)
  | OPTIONS; "."; ~ = lo(sf(rnel(loc(options_clause)),".")); < >

let options_clause :=
  | ~ = arithmetic_clause; < >
  | ~ = rounded_clause; < >
  | ~ = entry_convention_clause; < >
  | ~ = float_binary_clause; < >
  | ~ = float_decimal_clause; < >
  | ~ = intermediate_rounding_clause; < >

let arithmetic_clause [@context arithmetic_clause] :=
  | ARITHMETIC; IS?; ~ = arithmetic_mode; <Arithmetic>

let arithmetic_mode :=
 | NATIVE;           {Native}
 | STANDARD;         {Standard}                                   (* ~COB2002 *)
 | STANDARD_BINARY;  {StandardBinary}
 | STANDARD_DECIMAL; {StandardDecimal}

let rounded_clause [@context rounded_phrase] :=
  | DEFAULT; ROUNDED; MODE?; IS?; ~ = rounding_mode; <DefaultRoundedMode>

let entry_convention_clause [@context entry_convention_clause] :=
  | ENTRY_CONVENTION; IS?; COBOL; {EntryConvention COBOL}
(*| ENTRY_CONVENTION IS i = implementor_name (* none defined in standard *)
    { EntryConvention i } *)

let float_binary_clause [@context float_binary_clause] :=
  | FLOAT_BINARY; DEFAULT?; IS?; ~ = endianness_mode; <FloatBinaryDefault>

let float_decimal_clause [@context float_decimal_clause] :=
  | FLOAT_DECIMAL; DEFAULT?; IS?; ~ = encoding_endianness; <FloatDecimalDefault>

let intermediate_rounding_clause [@context intermediate_rounding_clause] :=
  | INTERMEDIATE; ROUNDING; IS?; ~ = rounding_mode; <IntermediateRounding>
    (* CHECKME: not all are valid *)


(* ------------------------- ENVIRONMENT DIVISION -------------------------- *)

let environment_division :=
 | ENVIRONMENT; DIVISION; ".";
   env_configuration = ro(loc(configuration_section));
   env_input_output  = ro(loc(input_output_section));
   { { env_configuration; env_input_output } }



(* ------------- ENVIRONMENT DIVISION / CONFIGURATION SECTION -------------- *)

let configuration_section :=
 | CONFIGURATION; SECTION; ".";
   source_computer_paragraph = ro(loc(source_computer_paragraph));
   object_computer_paragraph = ro(loc(object_computer_paragraph));
   special_names_paragraph   = ro(loc(special_names_paragraph));
   repository_paragraph      = ro(loc(repository_paragraph));     (* +COB2002 *)
   { { source_computer_paragraph;
       object_computer_paragraph;
       special_names_paragraph;
       repository_paragraph } }



(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / SOURCE-COMPUTER PARAGRAPH *)

let source_computer_paragraph :=     (* WITH DEBUGGING MODE removed in COB2002 *)
 | SOURCE_COMPUTER; "."; "."?;      (* COB2002 allows two consecutive dots *)
   {None}
 | SOURCE_COMPUTER; "."; i = name; wdm = bo(WITH?; DEBUGGING; MODE; {}); ".";
   {Some { source_computer_name = i;
           source_computer_with_debugging_mode = wdm; } }



(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / OBJECT-COMPUTER PARAGRAPH *)

let object_computer_paragraph [@context object_computer_paragraph] :=
 | OBJECT_COMPUTER; "."; "."?;         (* COB2002 allows two consecutive dots *)
   {None}
 | OBJECT_COMPUTER; "."; i = name; ocl = rl(loc(object_computer_clause)); ".";
   {Some { object_computer_name = i; object_computer_clauses = ocl; }}

let object_computer_clause :=
 | ~ = memory_size_clause; < >                            (* ~COB85, -COB2002 *)
 | ~ = character_classification_clause; < >               (* +COB2002 *)
 | ~ = program_collating_sequence_clause; < >             (* COB85 != COB2002 *)
 | ~ = segment_limit_clause; < >                          (* -COB2002 *)

let memory_size_clause :=                                  (* ~COB85, -COB2002 *)
 | MEMORY; SIZE?; ~ = integer; ~ = memory_size_unit; <ComputerMemorySize>

let memory_size_unit :=
 | WORDS;      {MemoryWords}
 | CHARACTERS; {MemoryCharacters}
 | MODULES;    {MemoryModules}

let character_classification_clause :=                             (* +COB2002 *)
 | CHARACTER?; CLASSIFICATION; cc = character_classification;
   { let a, n = cc in
     ComputerCharClassification { alphanumeric = a; national = n } }

let character_classification :=
 | IS?; l = locale_phrase; lo = ro(locale_phrase); { Some l, lo }
 | l = cc_alphanumeric;                            { Some l, None }
 | l = cc_national;                                { None, Some l }
 | la = cc_alphanumeric; ln = cc_national;         { Some la, Some ln }
 | ln = cc_national; la = cc_alphanumeric;         { Some la, Some ln }

let cc_alphanumeric := FOR; ALPHANUMERIC; IS?; ~ = locale_phrase; < >
let cc_national :=     FOR; NATIONAL;     IS?; ~ = locale_phrase; < >

let locale_phrase :=
  | i = name;       { CharClassificationName i }
  | LOCALE;         { CharClassificationLocale }
  | SYSTEM_DEFAULT; { CharClassificationSystemDefault }
  | USER_DEFAULT;   { CharClassificationUserDefault }

let program_collating_sequence_clause :=                   (* COB85 != COB2002 *)
  | PROGRAM?; COLLATING?; SEQUENCE; ~ = alphabet_specification;
    <ComputerProgCollatingSeq>

let segment_limit_clause :=                                        (* -COB2002 *)
  | SEGMENT_LIMIT; IS?; ~ = integer; <ComputerSegmentLimit>



(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / SPECIAL-NAMES PARAGRAPH *)

let special_names_paragraph :=
  | SPECIAL_NAMES; "."; ~ = rnel(loc(special_names_clause)); "."; < >
  | SPECIAL_NAMES; "."; "."?; { [] }    (* COB2002 allows two consecutive dots *)

let special_names_clause [@post.special_names] :=
  (* Note: most can be used multiple times *)
  | ~ = alphabet_name_clause;            < >
  | ~ = class_name_clause;               < >
  | ~ = crt_status_clause;               < >                       (* +COB2002 *)
  | ~ = currency_sign_clause;            < >
  | ~ = cursor_clause;                   < >                       (* +COB2002 *)
  | ~ = decimal_point_clause;            < >
  | ~ = dynamic_length_structure_clause; < >                       (* +COB2002 *)
  | ~ = locale_clause;                   < >                       (* +COB2002 *)
  | ~ = mnemonic_name_clause;            < >
  | ~ = symbolic_characters_clause;      < >
  | ~ = order_table_clause;              < >                       (* +COB2002 *)

let alphabet_name_clause [@context alphabet_clause] :=
  | ALPHABET; i = name; an = for_alphanumeric_or_national_opt;
    IS; cs = character_set;
    { AlphabetName { alphabet_name = i; category = an; characters = cs } }

let character_set :=
  | LOCALE; %prec lowest       { CharSetLocale None }              (* +COB2002 *)
  | LOCALE; i = name;          { CharSetLocale (Some i) }          (* +COB2002 *)
  | NATIVE;                    { CharSetNative }
  | STANDARD_1;                { CharSetStandard_1 }          (* Alphanum only *)
  | STANDARD_2;                { CharSetStandard_2 }          (* Alphanum only *)
  | UCS_4;                     { CharSetUCS_4 } (* +COB2002 *) (* National only *)
  | UTF_8;                     { CharSetUTF_8 } (* +COB2002 *) (* National only *)
  | UTF_16;                    { CharSetUTF_16 } (* +COB2002 *) (* National only *)
  | ll = rnel(literal_phrase); { CharSetCharacters ll }
(*| i = implementor_name { } *) (* the standard does not define any *)

let literal_phrase :=
  | l = string_or_int_literal;
    {CharactersRange (SingleCharacter l)}
  | l1 = string_or_int_literal; THROUGH; l2 = string_or_int_literal;
    {CharactersRange (CharacterRange { start_item = l1; end_item = l2 })}
  | l = string_or_int_literal; ll = rnel(pf(ALSO,string_or_int_literal));
    {CharactersList (l :: ll)}

let class_name_clause :=
  | CLASS; i = name;
    an = for_alphanumeric_or_national_opt; IS?;                   (* +COB2002 *)
    ll = nel(l = string_or_int_literal; lo = ro(pf(THROUGH,string_or_int_literal));
             { match lo with
               | None -> SingleCharacter l
               | Some l' -> CharacterRange { start_item = l; end_item = l' }});
    io = ro(pf(IN,name));
    { ClassName { class_name = i; category = an; characters = ll; source_charset = io } }

let crt_status_clause :=                                           (* +COB2002 *)
 | CRT; STATUS; IS?; ~ = name; <CRTStatus>

let currency_sign_clause [@context currency_clause] :=
  | CURRENCY; SIGN?; IS?; l = string_literal;  (* not fig const (hex allowed) *)
    lo = ro(pf(WITH?; PICTURE; IS?; p = loc(PICTURE_STRING); SYMBOL;
               { p }, string_literal));                (* +COB2002 *)
    { CurrencySign { sign = l; picture_symbol = lo } } (* not fig const, not hex, 1
                                                     char *)

let cursor_clause :=                                               (* +COB2002 *)
  | CURSOR; IS?; ~ = name; <Cursor>

let decimal_point_clause :=
  | DECIMAL_POINT; IS?; COMMA; {DecimalPointIsComma}

let dynamic_length_structure_clause
      [@context dynlen_struct_clause] :=                           (* +COB2002 *)
  | DYNAMIC; LENGTH; STRUCTURE?; i = name; IS?; sk = structure_kind;
    { DynLenStruct { name = i; kind = sk; } }

let structure_kind :=
  | sn = bo(SIGNED); sh = bo(SHORT); PREFIXED;
    { DynLenPrefixed { signed = sn; short = sh } }
  | DELIMITED; {DynLenDelimited}
  | ~ = name;  <DynLenPhysical>

let locale_clause :=                                               (* +COB2002 *)
  | LOCALE; i = name; IS?; is = name_or_string;
    { SpecialNameLocale { locale_name = i; external_name = is } }

(* This requires the implementor to actually define custom system names
   for switches, devices and features - the standard does not define any *)
let mnemonic_name_clause :=
  | i = name; mns = mnemonic_name_suffix;
    { let (mno, sso) = mns in
      MnemonicName { implementor_name = i; mnemonic_name = mno; status = sso } }

let mnemonic_name_suffix :=
  | IS; n = name;                     { Some n, None }
  | ss = status_switch;               { None, Some ss }
  | IS; n = name; ss = status_switch; { Some n, Some ss }

let status_switch :=
  | ON; STATUS?; IS?; i = name;       { StatusSwitchOn i }
  | OFF; STATUS?; IS?; i = name;      { StatusSwitchOff i }
  | (i1, i2) = mr( ON; STATUS?; IS?; i1 = name;
                   OFF; STATUS?; IS?; i2 = name; {i1, i2}
                 | OFF; STATUS?; IS?; i2 = name;
                   ON; STATUS?; IS?; i1 = name; {i1, i2});
    { StatusSwitch {on_ = i1; off = i2} }

let symbolic_characters_clause :=
  | SYMBOLIC; CHARACTERS?;
    an = for_alphanumeric_or_national_opt;                        (* +COB2002 *)
    scl = nel(~ = names; or_(IS,ARE)?; ~ = integers; < >);
    io = ro(pf(IN,name));
    { SymbolicChars { category = an; characters = scl; source_charset = io } }

let order_table_clause :=                                          (* +COB2002 *)
  | ORDER; TABLE; i = name; IS?; l = string_literal;
    { OrderTable { ordering_name = i; cultural_ordering = l } }

let for_alphanumeric_or_national_opt :=
  | (* epsilon *)       {Alphanumeric}
  | FOR?; ALPHANUMERIC; {Alphanumeric}
  | FOR?; NATIONAL;     {National}



(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / REPOSITORY PARAGRAPH *)

let repository_paragraph :=                                        (* +COB2002 *)
  | REPOSITORY; "."; ~ = ilo(sf(rnel(specifier),".")); < >

let specifier :=
  | ~ = class_specifier; < >
  | ~ = interface_specifier; < >
  | ~ = function_specifier; < >
  | PROGRAM; i = name; lo = as__strlit_;
    { ProgramSpecifier { name = i; external_name = lo } }
  | PROPERTY; i = name; lo = as__strlit_;
    { PropertySpecifier { name = i; external_name = lo } }

let class_specifier [@context class_specifier] :=
  | CLASS; i = name; lo = as__strlit_; eo = ro(expands_phrase);
    { ClassSpecifier { name = i; external_name = lo; expands = eo } }

let interface_specifier [@context interface_specifier] :=
  | INTERFACE; i = name; lo = as__strlit_; eo = ro(expands_phrase);
    { InterfaceSpecifier { name = i; external_name = lo; expands = eo } }

let expands_phrase :=
  | EXPANDS; i = name; USING; il = names;
    { { expands_name = i; expands_using = il } }

let function_specifier [@context function_specifier] :=
  | FUNCTION; i = name; lo = as__strlit_;
    { UserFunctionSpecifier { name = i; external_name = lo } }
  | FUNCTION; ~ = names; INTRINSIC; <IntrinsicFunctionSpecifier>
  | FUNCTION; ALL; INTRINSIC;       {IntrinsicFunctionAllSpecifier}

(* -------------- ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION -------------- *)

let input_output_section :=
 | INPUT_OUTPUT; SECTION; ".";
   file_control_paragraph = ro(loc(file_control_paragraph)); (* COB85: mandatory *)
   io_control_paragraph = ro(loc(io_control_paragraph));
    { { file_control_paragraph; io_control_paragraph } }



(* - ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION / FILE-CONTROL PARAGRAPH -- *)

let file_control_paragraph :=
 | FILE_CONTROL; "."; ~ = rl(select); < > (* COB85: non-empty list *)

let select :=
 | SELECT; o = bo(OPTIONAL); i = name;
   fcl = rnel(loc(select_clause)); ".";
   { { select_optional = o;
       select_name = i;
       select_clauses = fcl } }

let select_clause :=                  (* Note: some can be used multiple times *)
  | ~ = assign_clause;               < >
  | ~ = access_mode_clause;          < >
  | ~ = alternate_record_key_clause; < >
  | ~ = collating_sequence_clause;   < > (* +COB2002 *)
  | ~ = file_status_clause;          < >
  | ~ = lock_mode_clause;            < >                   (* +COB2002 *)
  | ~ = organization_clause;         < >
  | ~ = padding_character_clause;    < >           (* -COB2002 *)
  | ~ = record_delimiter_clause;     < >
  | ~ = record_key_clause;           < >
  | ~ = relative_key_clause;         < >
  | ~ = reserve_clause;              < >
  | ~ = sharing_clause;              < >                    (* +COB2002 *)

let assign_clause :=                                 (* USING added in COB2002 *)
  | ASSIGN; TO?; _assign_external_?;
    il = rnel(name_or_alphanum); io = ro(pf(USING,name));
    { SelectAssign { to_ = il; using = io } }
  | ASSIGN; USING; i = name;
    { SelectAssign { to_ = []; using = Some i; } }

let _assign_external_ [@post.pending fun () -> "EXTERNAL"] :=
  | EXTERNAL

let access_mode_clause :=
  | ACCESS; MODE?; IS?; ~ = access_mode; <SelectAccessMode>

let access_mode :=
 | DYNAMIC;    {AccessModeDynamic}
 | RANDOM;     {AccessModeRandom}
 | SEQUENTIAL; {AccessModeSequential}

let alternate_record_key_clause :=
 | ALTERNATE; RECORD; KEY?; IS?; i = qualname;
   il = lo(pf(SOURCE; IS?; {}, names));
   wd = bo(WITH?; DUPLICATES; {});
   { SelectAlternateRecordKey { key = i; source = il;
                                with_duplicates = wd } }

let collating_sequence_clause :=                                   (* +COB2002 *)
 | COLLATING?; SEQUENCE; ~ = alphabet_specification;
   <SelectCollatingSequenceOfFile>
 | COLLATING?; SEQUENCE; OF; il = names; IS; i = name;
   {SelectCollatingSequenceOfKey { keys = il; alphabet = i } }
 | COLLATING?; SEQUENCE; OF; il = ntl(name);
   {let i, il = split_last il in
    SelectCollatingSequenceOfKey { keys = il; alphabet = i } }

let file_status_clause :=
 | FILE?; STATUS; IS?; ~ = qualname; <SelectStatus>

let lock_mode_clause [@context lock_mode_clause] :=                (* +COB2002 *)
 | LOCK; MODE?; IS?; lm = lock_mode; wl = with_lock_clause;
   { SelectLockMode { mode = lm; with_lock = wl } }

let with_lock_clause [@recovery WithLockNone] :=
  | { WithLockNone }
  | WITH; LOCK; ON; m = bo(MULTIPLE); or_(RECORD,RECORDS);
    { WithLock { multiple = m } }

let lock_mode :=
 | MANUAL;    {LockManual}
 | AUTOMATIC; {LockAutomatic}

let organization_clause :=
 | io(ORGANIZATION; IS?; {}); ~ = organization; <SelectOrganization>

let organization :=
 | INDEXED;           {OrganizationIndexed}
 | RELATIVE;          {OrganizationRelative}
 | LINE?; SEQUENTIAL; {OrganizationSequential}         (* LINE for microfocus *)

let padding_character_clause :=                                    (* -COB2002 *)
 | PADDING; CHARACTER?; IS?; ~ = qualname_or_alphanum;
    <SelectPaddingCharacter>

let record_delimiter_clause :=
 | RECORD; DELIMITER; IS?; ~ = record_delimiter; <SelectRecordDelimiter>

let record_delimiter :=
 | STANDARD_1; {Standard_1}
(*| m = mnemonic_name  {}*) (* none defined by the standard *)

let record_key_clause :=
 | RECORD; KEY?; IS?; i = qualname;
   il = lo(pf(SOURCE; IS?,names));
   { SelectRecordKey { key = i; source = il } }

let relative_key_clause :=
 | RELATIVE; KEY?; IS?; ~ = name; <SelectRelativeKey>

let reserve_clause :=
 | RESERVE; ~ = integer; or_(AREA,AREAS)?; <SelectReserve>

let sharing_clause [@context sharing_clause] := (* +COB2002 *) (* Note: identical to sharing_phrase *)
 | SHARING; WITH?; ~ = sharing_mode; <SelectSharing>



(* -- ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION / I-O-CONTROL PARAGRAPH -- *)

let io_control_paragraph :=
 | I_O_CONTROL; "."; ~ = io_control_entry?; < >

let io_control_entry :=
 | rcl = rl(loc(rerun_clause));                                   (* -COB2002 *)
   sal = rl(loc(same_area_clause));
   mfl = rl(loc(multiple_file_clause));                           (* -COB2002 *)
   ".";
   { { io_control_rerun_clauses = rcl;
       io_control_same_area_clauses = sal;
       io_control_multiple_file_clauses = mfl; } }

let rerun_clause :=                                                (* -COB2002 *)
 | RERUN; io = ro(pf(ON,name)); EVERY?; rf = rerun_frequency;
   { { rerun_on = io; rerun_every = rf } }

let rerun_frequency :=
 | END; OF?; or_(REEL,UNIT); OF?; ~ = name; <RerunEndOf>
 | i = integer; RECORDS; OF?; id = name;    {RerunRecords (i, id)}
 | ~ = integer; CLOCK_UNITS;                <RerunClockUnits>
 | ~ = name;                                <RerunCond>

let same_area_clause :=
 | SAME; as_ = area_source; AREA?; FOR?; i = name; il = names;
   { { same_area_source = as_;
       same_area_file_name = i;
       same_area_file_names = il } }

let area_source :=
 |             {AreaSourceFile}
 | RECORD;     {AreaSourceRecord}
 | SORT;       {AreaSourceSortMerge}
 | SORT_MERGE; {AreaSourceSortMerge}

let multiple_file_clause :=                                        (* -COB2002 *)
 | MULTIPLE; FILE; TAPE?; CONTAINS?;
   ~ = nel(i = name; io = ro(pf(POSITION,integer));
           { { file_portion_name = i; file_portion_position = io } }); < >





(* ----------------------------- DATA DIVISION ----------------------------- *)

(*
- file section
    - file description       : constant, record, type
    - sort-merge description : constant, record, type
- working storage section    : constant, 77-level, record, type
- local storage section      : constant, 77-level, record, type
- linkage section            : constant, 77-level, record, type
- report section
    - report description     : constant, report group
- screen section             : constant, screen

+ file description   : FD file-name file-clauses* .
+ sort-merge desc    : SD file-name record-clause? .
+ report description : RD report-name report-clauses* .

- constant
- 77-level data description
- record description (data description with NO TYPEDEF in level 1)
- type declaration (data description with a TYPEDEF)
= data description (level-number entry-name? data-clause* )

- report group description (level-number entry-name? report-group-clauses* )

- screen description (level-number screen-name? screen-clauses* )

* 01 constant-name CONSTANT [IS GLOBAL] AS/FROM ... (constant)
* nn entry_name? type/next-group/line/picture/USAGE/... (report)
* nn entry_name? REDEFINES/TYPEDEF/ALIGNED/BASED/picture... (data=record/type)
* nn entry_name? GLOBAL/LINE/COLUMN/FULL/USAGE... (screen)

(*
record description entry =
  set of data description entries, first one of level 1
-> data item with level 1 without TYPEDEF is a record
  may be described as level-77 entries

type declaration entry =
  data description entry that contains a TYPEDEF clause
   lay have hierarchical structure

8.5.1.2, Levels, and in 13.16, Data description entry.
13.18.58, TYPEDEF clause.

*)

*)

let data_division :=
 | DATA; DIVISION; ".";
   fso  = ro(file_section);
   wsso = ro(working_storage_section);
   lsso = ro(local_storage_section);                              (* +COB2002 *)
   lso  = ro(linkage_section);
   cso  = ro(communication_section);                              (* -COB2002 *)
   rso  = ro(report_section);
   sso  = ro(screen_section);                                     (* +COB2002 *)
   { { file_section = fso;
       working_storage_section = wsso;
       local_storage_section = lsso;
       linkage_section = lso;
       communication_section = cso;
       report_section = rso;
       screen_section = sso; } }


let section(K, L) ==
  | K; SECTION; "."; ~ = loc(rl(loc(L))); < >

let file_section :=
  | ~ = section (FILE, file_or_sort_merge_descr_entry); < >

let working_storage_section :=
  | ~ = section (WORKING_STORAGE, constant_or_data_descr_entry); < >

let local_storage_section :=                                       (* +COB2002 *)
  | ~ = section (LOCAL_STORAGE, constant_or_data_descr_entry); < >

let linkage_section :=                                             (* +COB2002 *)
  | ~ = section (LINKAGE, constant_or_data_descr_entry); < >

let communication_section :=                                       (* -COB2002 *)
  | ~ = section (COMMUNICATION, communication_descr_entry); < >

let report_section :=
  | ~ = section (REPORT, report_descr_entry); < >

let screen_section :=                                              (* +COB2002 *)
  | ~ = section (SCREEN, constant_or_screen_descr_entry); < >


let elementary_level == ~ = DIGITS; <int_of_string>

let constant_level :=
  | EIGHTY_EIGHT;   { with_loc 88 $sloc }


file_or_sort_merge_descr_entry:
 | FD i = name
   fcl = rl(loc(file_descr_clause)) "."
   cdl = rl(loc(constant_or_data_descr_entry))
   { { file_name = i;
       file_items = cdl;
       file_clauses = FileFD fcl } }
 | SD i = name
   scl = rl(loc(sort_merge_file_descr_clause)) "."
   cdl = rl(loc(constant_or_data_descr_entry))
   { { file_name = i;
       file_items = cdl;
       file_clauses = FileSD scl } }

communication_descr_entry:
 | CD i = name FOR? in_ = bo(INITIAL) INPUT
   cl = rl(loc(communication_descr_clause))
   il = rl(entry_name_clause) "."
   cdl = rl(loc(constant_or_data_descr_entry))
   { { comm_name = i;
       comm_clauses = cl;
       comm_items = cdl;
       comm_direction = CommInput { initial = in_; items = il } } }
 | CD i = name FOR? in_ = bo(INITIAL) I_O
   cl = rl(loc(communication_descr_clause)) il = rl(name) "."
   cdl = rl(loc(constant_or_data_descr_entry))
   { { comm_name = i;
       comm_clauses = cl;
       comm_items = cdl;
       comm_direction = CommIO { initial = in_; items = il } } }
 | CD i = name FOR? OUTPUT
   cl = rl(loc(communication_descr_clause)) "."
   cdl = rl(loc(constant_or_data_descr_entry))
   { { comm_name = i;
       comm_clauses = cl;
       comm_items = cdl;
       comm_direction = CommOutput } }

let report_descr_entry :=
 | RD; i = name; rl = rl(loc(report_descr_clause)); ".";
   crl = rl(loc(constant_or_report_group_descr_entry));
   { { report_name = i;
       report_clauses = rl;
       report_items = crl } }

let constant_or_data_descr_entry :=
  | e = constant;
    { Constant e }
  | e = data_descr_entry;
    { Data e }                                  (* including level 77 entries *)
  | l = loc(elementary_level); dn = name; RENAMES; ri = loc(qualname);
    to_ = o(THROUGH; ~ = loc(qualname); < >); ".";
    { Renames { rename_level = l;
                rename_to = dn;
                rename_renamed = ri;
                rename_through = to_ } }
  | l = constant_level; cn = name;
    er(VALUE; IS? | VALUES; ARE?); vl = rnel(literal_through_literal);
    ao = o(IN; ~ = name; < >);
    wfo = o(WHEN?; SET?; TO?; FALSE; IS?; ~ = literal; < >); ".";
    { CondName { condition_name_level = l;
                 condition_name = cn;
                 condition_name_values = vl;
                 condition_name_alphabet = ao;
                 condition_name_when_false = wfo; } }
(* integer ident? cond_value_clause            (level 88 entries) *)

literal_through_literal:
 | l1 = literal
   { { condition_name_value = l1; condition_name_through = None } }
 | l1 = literal THROUGH l2 = literal
   { { condition_name_value = l1; condition_name_through = Some l2 } }

constant_or_report_group_descr_entry:
 | e = constant
     { Constant e: report_item_descr }
 | e = report_group_descr_entry
     { ReportGroup e }

let constant_or_screen_descr_entry :=
 | ~ = constant;           <Constant>
 | s = screen_descr_entry; {Screen s}

let file_descr_clause :=
 | ~ = external_clause;       <FileExternal>
 |     global_clause;         {FileGlobal}
 | ~ = format_clause;         < >                                 (* +COB2002 *)
 | ~ = block_contains_clause; < >
 | ~ = record_clause;         <FileRecord>
 | ~ = label_clause;          <FileLabel>                         (* -COB2002 *)
 | ~ = value_of_clause;       <FileValueOf>                       (* -COB2002 *)
 | ~ = data_clause;           <FileData>                          (* -COB2002 *)
 | ~ = linage_clause;         <FileLinage>
 | ~ = code_set_clause;       <FileCodeSet>
 | ~ = report_clause;         <FileReport>

let sort_merge_file_descr_clause :=
 | ~ = record_clause; <FileSDRecord>
 | ~ = data_clause;   <FileSDData>                                (* -COB2002 *)
 |     global_clause; {FileSDGlobal}

communication_descr_clause:
 | SYMBOLIC? QUEUE IS? i = name          { CommSymbolic (CommQueue, i) }       (* IN *)
 | SYMBOLIC? SUB_QUEUE_1 IS? i = name    { CommSymbolic (CommSubQueue1, i) }   (* IN *)
 | SYMBOLIC? SUB_QUEUE_2 IS? i = name    { CommSymbolic (CommSubQueue2, i) }   (* IN *)
 | SYMBOLIC? SUB_QUEUE_3 IS? i = name    { CommSymbolic (CommSubQueue3, i) }   (* IN *)
 | SYMBOLIC? SOURCE IS? i = name         { CommSymbolic (CommSource, i) }      (* IN *)
 | SYMBOLIC? TERMINAL IS? i = name       { CommSymbolic (CommTerminal, i) }    (* I-O *)
 | io(SYMBOLIC) DESTINATION IS? i = name { CommSymbolic (CommDestination, i) } (* OUT *)
 | DESTINATION COUNT IS? i = name        { CommDestinationCount i }        (* OUT *)
 | DESTINATION TABLE OCCURS
   i = integer TIMES?
   il = lo(pf(INDEXED BY? {},nel(name))) { CommDestinationTable (i, il) } (* OUT *)
 | MESSAGE? COUNT IS? i = name           { CommMessageCount i }           (* IN *)
 | MESSAGE DATE IS? i = name             { CommMessageDate i }      (* IN/I-O *)
 | MESSAGE TIME IS? i = name             { CommMessageTime i }      (* IN/I-O *)
 | TEXT LENGTH IS? i = name              { CommTextLength i }       (* IN/OUT/I-O *)
 | STATUS KEY IS? i = name               { CommStatusKey i }        (* IN/OUT/I-O *)
 | END KEY IS? i = name                  { CommEndKey i }           (* IN/I-O *)
 | ERROR KEY IS? i = name                { CommErrorKey i }         (* OUT *)

let report_descr_clause :=
 |     global_clause;     {Global}
 | ~ = code_clause;       < >
 | ~ = control_clause;    < >
 | ~ = page_limit_clause; < >



let format_clause :=                                               (* +COB2002 *)
 | FORMAT; BIT; DATA?;       {FileFormat Bit}
 | FORMAT; CHARACTER; DATA?; {FileFormat Character}
 | FORMAT; NUMERIC; DATA?;   {FileFormat Numeric}

let block_contains_clause :=
 | BLOCK; CONTAINS?; i = integer; io = io(pf(TO,integer));
   cr = file_block_contents;
   { FileBlockContains { from = i; to_ = io;
                         characters_or_records = cr; } }

let file_block_contents ==
  |             {FileBlockContainsCharacters}
  | CHARACTERS; {FileBlockContainsCharacters}
  | RECORDS;    {FileBlockContainsRecords}

record_clause:
 | RECORD CONTAINS? i = integer CHARACTERS?
    { FixedLength i }
 | RECORD CONTAINS? i1 = integer TO i2 = integer CHARACTERS?
    { FixedOrVariableLength { min_length = i1;
                              max_length = i2 } }
 | RECORD IS? VARYING IN? SIZE?
   lengths = from_to_characters_opt
   depending = ro(depending_phrase)
   { let min_length, max_length = lengths in
     VariableLength { min_length; max_length; depending } }

from_to_characters_opt:
 | CHARACTERS?                                    { None,    None }
 | FROM? i1 = integer CHARACTERS?                 { Some i1, None }
 | TO i2 = integer CHARACTERS?                    { None,    Some i2 }
 | FROM? i1 = integer TO i2 = integer CHARACTERS? { Some i1, Some i2 }

label_clause:
 | LABEL mr(RECORD IS? | RECORDS ARE? {}) STANDARD { LabelStandard }
 | LABEL mr(RECORD IS? | RECORDS ARE? {}) OMITTED  { LabelOmitted }

value_of_clause:
 | VALUE OF iil = nel(i = name IS? il = qualname_or_literal
                        { { value_of_valued = i; value_of_value = il; } })
  { iil }

data_clause:
 | DATA_RECORD IS? il = names   { il }
 | DATA_RECORDS ARE? il = names { il }

linage_clause:
 | l = linage_header
   wfa = ro(pf(WITH? FOOTING AT? {}, qualname_or_integer))
   lat = io(pf(LINES? AT? TOP {}, qualname_or_integer))
   lab = ro(pf(LINES? AT? BOTTOM {}, qualname_or_integer))
   { { file_linage_lines = l;
       file_linage_with_footing_at = wfa;
       file_linage_lines_at_top = lat;
       file_linage_lines_at_bottom = lab; } }

let linage_header :=
 | LINAGE; IS?; ~ = qualname_or_integer; %prec below_LINES < >
 | LINAGE; IS?; ~ = qualname_or_integer; LINES;            < >

let code_set_clause :=
 | CODE_SET; ~ = alphabet_specification; < >

let report_clause_prefix == REPORT; IS? | REPORTS; ARE?
let report_clause :=
 | report_clause_prefix; ~ = names; < >

let code_clause := CODE; IS?; ~ = ident; <Code>

let control_clause_prefix == CONTROL; IS? | CONTROLS; ARE?
let control_clause :=
 | control_clause_prefix;           il = names; { Control {final = false; controls = il} }
 | control_clause_prefix; FINAL; il = rl(name); { Control {final = true;  controls = il} }

let limit_is_ == LIMIT; IS? | LIMITS; ARE?
let page_limit_clause :=
 | PAGE; limit_is_?;
   lco = page_line_col;
   ho = ro(pf(HEADING; IS?,integer));
   fdo = ro(pf(FIRST; DETAIL; IS?,integer));
   lcho = io(pf(LAST; mr(CH | CONTROL; HEADING); IS?,integer));
   ldo = ro(pf(LAST; DETAIL; IS?,integer));
   fo = ro(pf(FOOTING; IS?,integer));
    { let lo, co = lco in
      PageLimit { lines = lo;
                  columns = co;
                  heading = ho;
                  first_detail = fdo;
                  last_control_heading = lcho;
                  last_detail = ldo;
                  footing = fo; } }

let page_line_col :=
 | c = integer; COLUMNS;                               { None,   Some c }
 | l = integer; or_(LINE,LINES)?;                      { Some l, None   }
 | l = integer; or_(LINE,LINES); c = integer; COLUMNS; { Some l, Some c }




let constant :=
  (* BYTE-LENGTH is sensitive throughout "constant entry" w.r.t ISO/IEC 2014.
     However, like in GnuCOBOL we restrict the scope to the only places where
     the keyword is relevant. *)
  | l = loc(elementary_level); n = name; spec = constant_spec; ".";
    { let go, cv = spec in
      { constant_level = l;
        constant_name = n;
        constant_global = go;
        constant_value = cv } }

let constant_spec_prefix ==
  | CONSTANT; ~ = ibo(global_clause); < >

let constant_spec :=
  | go = constant_spec_prefix; AS?; e = loc(expression);
    { go, ConstExpr ~&e &@<- e } (* or plain ident *)
  | p = constant_value_length; OF?; n = name;
    { fst p, match snd p with
        | `ByteLength -> ConstByteLength n &@<- n
        | `Length -> ConstLength n &@<- n }
  | go = constant_spec_prefix; FROM; n = name;
    { go, ConstFrom n &@<- n }

let constant_value_length [@context constant] :=
  | go = constant_spec_prefix; AS?; BYTE_LENGTH; {go, `ByteLength}
  | go = constant_spec_prefix; AS?; LENGTH;      {go, `Length}

let data_descr_entry :=
  | l = loc(elementary_level);
    eno = ro(entry_name_clause);
    dcl = rl(loc(data_descr_clause)); ".";
    { { data_level = l;
        data_name = eno;
        data_clauses = dcl } }

let report_group_descr_entry :=
  | l = elementary_level;
    eno = ro(entry_name_clause);
    rcl = rl(loc(report_group_descr_clause)); ".";
    { { report_level = l;
        report_data_name = eno;
        report_group_clauses = rcl } }

let screen_descr_entry [@context screen_descr_entry] :=
  | l = elementary_level;
    eno = ro(entry_name_clause);
    scl = rl(loc(screen_descr_clause)); ".";
    { { screen_level = l;
        screen_data_name = eno;
        screen_clauses = scl } }

let entry_name_clause :=
  | n = name; {         DataName n &@<- n}         (* data name / screen name *)
  | FILLER;   {with_loc DataFiller $sloc }



data_descr_clause: (* P255 *)
 | c = redefines_clause       { DataRedefines c }
 | c = typedef_clause         { c }                               (* +COB2002 *)
 |     aligned_clause         { DataAligned }                     (* +COB2002 *)
 |     any_length_clause      { DataAnyLength }                   (* +COB2002 *)
 |     based_clause           { DataBased }                       (* +COB2002 *)
 |     blank_when_zero_clause { DataBlankWhenZero }
 |     constant_record_clause { DataConstantRecord }              (* +COB2002 *)
 | c = dynamic_length_clause  { c }                               (* +COB2002 *)
 | c = external_clause        { DataExternal c }
 |     global_clause          { DataGlobal }
 | c = group_usage_clause     { DataGroupUsage c }                (* +COB2002 *)
 |     justified_clause       { DataJustified }
 | c = data_occurs_clause     { DataOccurs c }
 | c = loc(picture_clause)    { DataPicture c }
 | c = loc(property_clause)   { DataProperty c }                  (* +COB2002 *)
 | c = same_as_clause         { DataSameAs c }                    (* +COB2002 *)
 | c = select_when_clause     { DataSelectWhen c }                (* +COB2002 *)
 | c = sign_clause            { DataSign c }
 | c = synchronized_clause    { DataSynchronized c }
 | c = data_type_clause       { DataType c }                      (* +COB2002 *)
 | c = usage_clause           { DataUsage c }
 | c = validation_clause      { DataValidation c }                (* +COB2002 *)
 | c = data_value_clause      { DataValue c }


report_group_descr_clause: (* P286 *)
 | c = report_type_clause         { ReportType c }
 | c = next_group_clause          { ReportNextGroup c }
 | c = report_line_clause         { ReportLine c }
 | c = loc(picture_clause)        { ReportPicture c }
 | c = report_screen_usage_clause { ReportUsage c }
 | c = sign_clause                { ReportSign c }
 |     justified_clause           { ReportJustified }
 | c = report_column_clause       { c }
 |     blank_when_zero_clause     { ReportBlankWhenZero }
 | c = source_clause              { c }
 | c = sum_clause                 { c }
 | c = report_value_clause        { c }
 | c = present_when_clause        { ReportPresentWhen c }         (* +COB2002 *)
 |     group_indicate_clause      { ReportGroupIndicate }
 | c = report_occurs_clause       { c }                           (* +COB2002 *)
 | c = varying_clause             { ReportVarying c }             (* +COB2002 *)

screen_descr_clause: (* P293 *) (* +COB2002 *)
 |     global_clause              { ScreenGlobal }
 | c = screen_line_clause         { ScreenLine c }
 | c = screen_column_clause       { ScreenColumn c }
 | c = blank_clause               { ScreenBlank c }
 | c = erase_clause               { ScreenErase c }
 | c = screen_attribute_clauses   { ScreenAttribute c }
 | c = loc(picture_clause)        { ScreenPicture c }
 | c = source_destination_clauses { ScreenSourceDestination c }
 |     blank_when_zero_clause     { ScreenBlankWhenZero }
 |     justified_clause           { ScreenJustified }
 | c = sign_clause                { ScreenSign c }
 |     full_clause                { ScreenFull }
 |     auto_clause                { ScreenAuto }
 |     secure_clause              { ScreenSecure }
 |     required_clause            { ScreenRequired }
 | c = screen_occurs_clause       { ScreenOccurs c }
 | c = report_screen_usage_clause { ScreenUsage c }



(* ---------- Rules common to data, reports and screens ---------- *)

let blank_when_zero_clause := BLANK; WHEN?; ZERO

let justified_clause := JUSTIFIED; RIGHT?

let picture_clause
      [@recovery dummy_picture]
      [@symbol "<picture clause>"] :=
  | PICTURE; IS?; picture = loc(PICTURE_STRING);
    picture_locale = ro(picture_locale_phrase);
    picture_depending = ro(depending_phrase);
    { { picture; picture_locale; picture_depending } }

let picture_locale_phrase
      [@recovery { locale_name = None; locale_size = "0" }]
      [@symbol "<locale phrase>"] :=
  | LOCALE; io = pf(IS?, name)?; SIZE; IS?; i = integer;
    { {locale_name = io; locale_size = i} }

let sign_clause :=
  | o(SIGN; IS?; {});
    lt = mr(LEADING; { LeadingSign } | TRAILING; { TrailingSign });
    sc = bo(SEPARATE; CHARACTER?; {});
    { { sign_position = lt;
        sign_separate_character = sc; } }



(* ---------- Rules common to data, screens and constants ---------- *)

let global_clause := GLOBAL | IS_GLOBAL



(* ---------- Rules common to data and reports ---------- *)

let key_is :=
  | ad = sort_direction; KEY?; IS?; il = qualnames;
    { { sort_key_direction = ad; sort_key_names = il } }

let sort_direction ==
  | ASCENDING;  {SortAscending}
  | DESCENDING; {SortDescending}

let indexed_by := INDEXED; BY?; ~ = names; < >
let depending_phrase := DEPENDING; ON?; ~ = loc(reference); < >
let step_phrase := STEP; ~ = integer; < >

let varying_clause :=
  | VARYING; ~ = nel(i = name; fe = ro(pf(FROM,expression));
                     be = ro(pf(BY,expression));
                     { { data_varying = i;
                         data_varying_from = fe;
                         data_varying_by = be; } }); < >



(* ---------- Rules common to reports and screens ---------- *)

let report_screen_usage_clause :=
  | USAGE; IS?; DISPLAY;  { Display }
  | USAGE; IS?; NATIONAL; { National }                            (* +COB2002 *)



(* ---------- Rules specific to data ---------- *)

let redefines_clause := REDEFINES; ~ = name; < >
let typedef_clause [@context typedef_clause] :=
  | or_(TYPEDEF,IS_TYPEDEF); s = bo(STRONG); { DataTypedef { strong = s } }
let aligned_clause := ALIGNED
let any_length_clause := ANY; LENGTH
let based_clause := BASED
let constant_record_clause := CONSTANT_RECORD

let dynamic_length_clause :=
  | DYNAMIC; LENGTH?; ido = ro(name); io = ro(pf(LIMIT; IS?,integer));
    { DataDynamicLength { dynamic_length_structure_name = ido; limit_is = io } }

let external_clause :=
  | or_(EXTERNAL,IS_EXTERNAL); ~ = as__strlit_; < >

let group_usage_clause [@recover GroupUsageBit] :=
  | GROUP_USAGE; IS?; BIT;      {GroupUsageBit}
  | GROUP_USAGE; IS?; NATIONAL; {GroupUsageNational}

let data_occurs_clause :=
  | ~ = occurs_fixed_clause;     < >
  | ~ = occurs_depending_clause; < >
  | ~ = occurs_dynamic_clause;   < >

let occurs_fixed_clause [@context occurs_clause] :=
  | OCCURS; i = loc(integer); TIMES?; kl = rl(key_is); ib = lo(indexed_by);
    { OccursFixed { times = i; key_is = kl; indexed_by = ib; } }

let occurs_depending_clause [@context occurs_clause] :=
  | OCCURS; i1 = loc(integer); TO; i2 = loc(integer); TIMES?;
    d = depending_phrase; kl = rl(key_is); il = lo(indexed_by);
    { OccursDepending { from = i1; to_ = i2; depending = d;
                        key_is = kl; indexed_by = il; } }

let occurs_dynamic_clause [@context occurs_clause] :=
  | OCCURS; DYNAMIC; co = ro(capacity_phrase);
    i1o = ro(pf(FROM,loc(integer))); i2o = ro(pf(TO,loc(integer)));
    i = loc(bo(INITIALIZED)); kl = rl(key_is); il = lo(indexed_by);
    { OccursDynamic { capacity_in = co; from = i1o; to_ = i2o;
                      initialized = i; key_is = kl; indexed_by = il; } }

let capacity_phrase := CAPACITY; IN?; ~ = name; < >

let property_clause :=
  | PROPERTY;
    wno = ro(pf(WITH?; NO; {},property_kind));
    f = bo(IS?; FINAL; {});
    { { property_with_no = wno; property_is_final = f } }

let property_kind ==
  | GET; {PropertyGet}
  | SET; {PropertySet}
let same_as_clause := SAME; AS; ~ = name; < >
let select_when_clause :=
  | SELECT; WHEN; ~ = name; <SelectWhen>
  | SELECT; WHEN; OTHER;    {SelectWhenOther}

let synchronized_clause :=
  | SYNCHRONIZED; LEFT;  {SynchronizedLeft}
  | SYNCHRONIZED; RIGHT; {SynchronizedRight}
  | SYNCHRONIZED;        {SynchronizedDefault}

let data_type_clause := TYPE; TO?; ~ = name; < >

let usage_clause :=
  | USAGE; IS?; ~ = usage; < >
  | ~ = usage; < >                                                  (* COBOL85 *)

usage [@context usage_clause   (* ok as none of leftmost terminals are C/S *)]:
  | BINARY                                        { Binary }
  | DISPLAY                                       { Display }
  | INDEX                                         { Index }
  | PACKED_DECIMAL                                { PackedDecimal }
  (* All the following are +COB2002 *)
  | BIT                                           { Bit }
  | BINARY_CHAR so = signedness_                  { BinaryChar so }
  | BINARY_SHORT so = signedness_                 { BinaryShort so }
  | BINARY_LONG so = signedness_                  { BinaryLong so }
  | BINARY_DOUBLE so = signedness_                { BinaryDouble so }
  | FLOAT_EXTENDED                                { FloatExtended }
  | FLOAT_LONG                                    { FloatLong }
  | FLOAT_SHORT                                   { FloatShort }
  | FLOAT_BINARY_32 eo = endianness_mode_         { FloatBinary32 eo }
  | FLOAT_BINARY_64 eo = endianness_mode_         { FloatBinary64 eo }
  | FLOAT_BINARY_128 eo = endianness_mode_        { FloatBinary128 eo }
  | FLOAT_DECIMAL_16 ee = encoding_endianness_opt { FloatDecimal16 ee }
  | FLOAT_DECIMAL_34 ee = encoding_endianness_opt { FloatDecimal34 ee }
  | NATIONAL                                      { National }
  | OBJECT REFERENCE rk = ro(object_reference_kind) { ObjectReference rk }
  | FUNCTION_POINTER TO? i = name                 { FunctionPointer i }
  | POINTER io = ro(pf(TO?,name))                 { Pointer io }
  | PROGRAM_POINTER io = ro(pf(TO?,name))         { ProgramPointer io }

  | COMP                                          { Binary }
  | COMP_0                                        { UsagePending `Comp0 }
  | COMP_1                                        { UsagePending `Comp1 }
  | COMP_2                                        { UsagePending `Comp2 }
  | COMP_3                                        { UsagePending `Comp3 }
  | COMP_4                                        { Binary }
  | COMP_5                                        { UsagePending `Comp5 }
  | COMP_6                                        { UsagePending `Comp6 }
  | COMP_X                                        { UsagePending `CompX }
  | COMP_N                                        { UsagePending `CompN }
  | COMP_9                                        { UsagePending `Comp9 }
  | COMP_10                                       { UsagePending `Comp10 }
  | COMP_15                                       { UsagePending `Comp15 }
  | BINARY_C_LONG so = signedness_                { UsagePending (`BinaryCLong so) }

let signedness_ := ~ = ro(signedness); < >
let signedness ==
  | SIGNED;   { Signed }
  | UNSIGNED; { Unsigned }

let endianness_mode_ := ~ = ro(endianness_mode); < >
let endianness_mode :=
  | HIGH_ORDER_LEFT;  { HighOrderLeft }
  | HIGH_ORDER_RIGHT; { HighOrderRight }

let encoding_mode :=
  | BINARY_ENCODING;  { BinaryEncoding }
  | DECIMAL_ENCODING; { DecimalEncoding }

let encoding_endianness_opt :=
  | { { encoding_mode = None; encoding_endianness = None } }
  | ~ = encoding_endianness; < >

let encoding_endianness :=
  | ecm = encoding_mode;
    { {encoding_mode = Some ecm; encoding_endianness = None } }
  | edm = endianness_mode;
    { {encoding_mode = None; encoding_endianness = Some edm } }
  | ecm = encoding_mode; edm = endianness_mode;
    { {encoding_mode = Some ecm; encoding_endianness = Some edm } }
  | edm = endianness_mode; ecm = encoding_mode;
    { {encoding_mode = Some ecm; encoding_endianness = Some edm } }

let object_reference_kind :=
  | f = bo(FACTORY; OF?; {}); ACTIVE_CLASS;
    { ActiveClass { factory_of = f } }
  | f = bo(FACTORY; OF?; {}); i = name; o = bo(ONLY);
    { Name { class_or_interface_name = i; factory_of = f; only = o; } }

let validation_clause :=
  | ~ = class_clause;           <Class>
  | ~ = default_clause;         <Default>
  | ~ = destination_clause;     <Destination>
  | ~ = invalid_when_clause;    <InvalidWhen>
  | ~ = present_when_clause;    <PresentWhen>
  | ~ = varying_clause;         <Varying>
  | ~ = validate_status_clause; < >

let class_clause := CLASS; IS?; ~ = class_; < >

let class_ :=
  | ALPHABETIC;       {Alphabetic}
  | ALPHABETIC_LOWER; {AlphabeticLower}
  | ALPHABETIC_UPPER; {AlphabeticUpper}
  | BOOLEAN;          {Boolean}
  | NUMERIC;          {Numeric}
  | ~ = name;         <ClassOrAlphabet>

let default_clause [@context default_clause] :=
  | DEFAULT; IS?; ~ = ident_or_literal; <Some>
  | DEFAULT; IS?; NONE;                 {None}

let destination_clause := DESTINATION; IS?; ~ = idents; < >
let invalid_when_clause := ~ = nel(INVALID; WHEN; ~ = condition; < >); < >
let validate_status_clause [@context validate_status_clause] :=
  | VALIDATE_STATUS;
    IS?; il = ident_or_literal;
    WHEN?; ene = error_or_no_error;
    vsl = lo(pf(ON,rnel(validation_stage)));
    FOR; idl = idents;
    { ValidateStatus { is_ = il; when_ = ene; on = vsl; for_ = idl; } }

let error_or_no_error :=
  | ERROR;     {ValidateWhenError}
  | NO; ERROR; {ValidateWhenNoError}

let validation_stage :=
  | FORMAT;   {ValidationStageFormat}
  | CONTENT;  {ValidationStageContent}
  | RELATION; {ValidationStageRelation}

let data_value_clause_prefix == VALUE; IS? | VALUES; ARE?
let data_value_clause :=
  | data_value_clause_prefix; ~ = loc(literal); <ValueData>
  | data_value_clause_prefix; ~ = nel(ll = rnel(loc(literal));
                                      FROM; fl = subscripts;
                                      tl = lo(TO; ~ = subscripts; < >);
                                      { { table_data_values = ll;
                                          table_data_from = fl;
                                          table_data_to = tl } }); <ValueTable>

(* /* *)
(* cond_value_clause: // for 88 entries *)
(*  | mr(VALUE IS? | VALUES ARE? {}) *)
(*    ll = literal_through_literal+ *)
(*    ao = pf(IN,ident)? *)
(*    wf = pf(WHEN? SET? TO? FALSE IS? {},literal)? *)
(*     {} *)
(*  | or_(VALUE,VALUES) *)
(*    ll = literal_through_literal+ *)
(*    ao = pf(IN,ident)? *)
(*    vi = pf(or_(IS,ARE)?,valid_or_invalid) *)
(*    wc = pf(WHEN,condition)? *)
(*     {} *)
(* ; *)

(* literal_through_literal: // literal range *)
(*  | l = literal lo = pf(THROUGH,literal)? { (l, lo) } *)
(* ; *)

(* valid_or_invalid: *)
(*  | VALID   { Valid } *)
(*  | INVALID { Invalid } *)
(* ; *)
(* */ *)



(* ---------- Rules specific to reports ---------- *)

report_type_clause:
 | TYPE IS? DETAIL                      { Detail }
 | TYPE IS? mr(RH | REPORT HEADING {})  { ReportHeading }
 | TYPE IS? mr(RF | REPORT FOOTING {})  { ReportFooting }
 | TYPE IS? mr(PH | PAGE HEADING {})    { PageHeading }
 | TYPE IS? mr(PF | PAGE FOOTING {})    { PageFooting }
 | TYPE IS? mr(CH | CONTROL HEADING {})
   fo = o(io(or_(ON,FOR))
            if_ = report_data_name_or_final op = bo(OR PAGE {}) { if_, op })
    { ControlHeading fo }
 | TYPE IS? mr(CF | CONTROL FOOTING {})
   fo = o(io(or_(ON,FOR))
            if_ = report_data_name_or_final { if_ } )
    { ControlFooting fo }

next_group_clause:
 | NEXT GROUP IS? i = integer                       { ReportNextAbsolute i }
 | NEXT GROUP IS? or_(PLUS,"+") i = integer         { ReportNextRelative i }
 | NEXT GROUP IS? NEXT PAGE wr = bo(WITH? RESET {}) { ReportNextNextPage wr }

report_line_clause:
 | line_header pl = rnel(line_position) { pl }

line_header [@context line_clause                             (*NUMBERS only*)]:
 | LINE mr(NUMBER IS? | NUMBERS ARE? | or_(IS,ARE)? {}) | LINES ARE {}

line_position:
 | i = integer  %prec lowest    { LineAbsolute (i, false) }
 | i = integer io(ON) NEXT_PAGE { LineAbsolute (i, true) }
 | or_(PLUS,"+") i = integer    { LineRelative  i }
 | ON? NEXT_PAGE                { LineOnNextPage }

report_column_clause:
 | a = column_header or_(IS,ARE)? pl = rnel(column_position)
   { ReportColumn { alignment = a; position = pl } }

let column_header [@context column_clause              (* NUMBERS & CENTER *)] :=
  | or_(COL,COLUMN); or_(NUMBER,NUMBERS)?; ~ = alignment; < >
  | COLUMNS; ~ = alignment; < >

let column_position :=
  | ~ = integer;                <ColumnAbsolute>
  | or_(PLUS,"+"); ~ = integer; <ColumnRelative>

let alignment :=
  | LEFT?;  {AlignLeft}
  | CENTER; {AlignRight}
  | RIGHT;  {AlignCenter}

source_clause:
 | mr(SOURCE IS? | SOURCES ARE? {})
   el = source_operands rmo = rounded_phrase_opt
    { ReportSource { source = el; rounding = rmo; } }

(* Not rigorously exact (see P389), but too complicated to parse otherwise  *)
source_operands:
 | e = expression            { [e] }
 | el = ntl(arithmetic_term) { el }

sum_clause:
 | sl = nel(sum_phrase)
   ro = io(pf(RESET ON? {}, report_data_name_or_final))
   rm = rounded_phrase_opt
     { ReportSum { sum_of = sl; reset_on = ro; rounding = rm; } }

sum_phrase:
  | SUM OF? el = sum_operands il = lo(pf(UPON,names))    (* TODO: recov? *)
     { { sum_operands = el; sum_upon_items = il } }

(* Not sure which expressions are valid here (see P381) *)
sum_operands:
 | e = expression            { [e] }
 | el = ntl(arithmetic_term) { el }

let report_data_name_or_final :=
  | ~ = qualident; <ReportDataName>
  | FINAL;         {ReportFinal}

let report_value_clause :=
  | mr(VALUE; IS?| VALUES; ARE?); ~ = literal; <ReportValue>

let present_when_clause := PRESENT; WHEN; ~ = condition; < >
let group_indicate_clause := GROUP; INDICATE?

let report_occurs_clause [@context occurs_clause] :=
 | OCCURS; from = integer; to_ = io(pf(TO,integer)); TIMES?;
   depending = ro(depending_phrase);
   step = ro(step_phrase);
   { ReportOccurs { from; to_; depending; step } }



(* ---------- Rules specific to screens ---------- *)

let screen_line_clause :=
 | LINE; NUMBER?; IS?; ~ = screen_line_column_clause; < >

let screen_column_clause :=
 | or_(COL,COLUMN); NUMBER?; IS?; ~ = screen_line_column_clause; < >

let screen_line_column_clause :=
 | ~ = ident_or_integer;                      <Absolute>
 | pm = plus_or_minus; ii = ident_or_integer; {Relative (pm, ii)}

let plus_or_minus :=
 | mr(PLUS  | "+"); { Plus }
 | mr(MINUS | "-"); { Minus }

let blank_clause :=
 | BLANK; LINE;   { Line }
 | BLANK; SCREEN; { Screen }

let erase_clause [@context erase_clause] :=
 | ERASE; mr(EOL | END?; OF?; LINE; {});   { EndOfLine }
 | ERASE; mr(EOS | END?; OF?; SCREEN; {}); { EndOfScreen }

let screen_attribute_clauses :=
  | ~ = nel(loc(screen_attribute_clause)); < >

screen_attribute_clause:
 | BELL                                      { Bell }
 | BLINK                                     { Blink }
 | HIGHLIGHT                                 { Highlight }
 | LOWLIGHT                                  { Lowlight }
 | REVERSE_VIDEO                             { ReverseVideo }
 | UNDERLINE                                 { Underline }
 | FOREGROUND_COLOR IS? i = ident_or_integer { ForegroundColor i }
 | BACKGROUND_COLOR IS? i = ident_or_integer { BackgroundColor i }

// TODO: not really a list, should disambiguate later
let source_destination_clauses :=
 | ~ = nel(loc(source_destination_clause)); < >

source_destination_clause:
 | FROM ii = ident_or_literal { From ii }
 | TO i = ident               { To i }
 | USING i = ident            { Using i }
 | VALUE IS? l = literal      { Value l }

let full_clause == FULL
let auto_clause == AUTO
let secure_clause == SECURE
let required_clause == REQUIRED
let screen_occurs_clause := OCCURS; ~ = integer; TIMES?; < >






(* -------------------- PROCEDURE DIVISION -------------------- *)

procedure_division:
 | PROCEDURE DIVISION
   ul = ilo(pf(USING,rnel(loc(using_clause))))
   ro = ro(returning)                                             (* +COB2002 *)
   rl = ilo(raising_phrase) "."                                   (* +COB2002 *)
   dl = lo(declaratives)
   sl = rl(loc(section_paragraph))
   { { procedure_using_clauses = ul;
       procedure_returning = ro;
       procedure_raising_phrases = rl;
       procedure_declaratives = dl;
       procedure_paragraphs = sl } }

program_procedure_division:
 | PROCEDURE DIVISION
   ul = ilo(pf(USING,rnel(loc(using_clause))))
   ro = ro(returning)                                             (* +COB2002 *)
   rl = ilo(raising_phrase) "."                                   (* +COB2002 *)
   dl = lo(declaratives)
   sl = section_paragraphs
   { { procedure_using_clauses = ul;
       procedure_returning = ro;
       procedure_raising_phrases = rl;
       procedure_declaratives = dl;
       procedure_paragraphs = sl } }

let object_procedure_division :=                                   (* +COB2002 *)
 | PROCEDURE; DIVISION; "."; ~ = rl(loc(method_definition)); < >

(* COB85: only USING ident+ (in the IPC module, P541) *)
let using_clause :=
 | io(BY?; REFERENCE);
   ~ = nell(o = ibo(OPTIONAL); n = name; { { using_by_reference_optional = o;
                                             using_by_reference = n } });
   %prec lowest                             <UsingByReference>
 | BY?; VALUE; ~ = nell(name); %prec lowest <UsingByValue>

(* Ambiguous, only class name may have factory *)
let raising_phrase :=
 | RAISING; ~ = nel(loc(f = bo(FACTORY; OF?); i = name;
                        { { raising_factory = f; raising = i} })); < >
  (* exception / interface name / class name *)

let declaratives :=
 | DECLARATIVES;      "."; ~ = rnel(loc(decl_section_paragraph));
   END; DECLARATIVES; "."; < >

(* Ambigous sections and paragraphs *)
let decl_section_paragraph :=
 | i = procedure_name_decl;
   so = o(SECTION; ~ = ro(integer); "."; ~ = use_statement; < >); ".";
   sl = rl(loc(sentence));
    { let io, us = match so with
          | None -> None, None
          | Some (io, us) -> io, Some us
      in
      { declarative_name = i;
        declarative_segment = io;
        declarative_use = us;
        declarative_sentences = sl } }
(* segment number from 0 to 99, less than 50 *)

(* Ambigous sections and paragraphs *)
let section_paragraphs :=
 | (* Empty *)                       { [] }
 | ~ = rnel(loc(section_paragraph)); < >
 | sl = loc(rnel(loc(sentence)));
   tl = rl(loc(section_paragraph));
   { ({ paragraph_name = None;
        paragraph_is_section = false;
        paragraph_segment = None;
        paragraph_sentences = ~&sl } &@<- sl) :: tl }

let section_paragraph :=
 | i = procedure_name_decl;
   s = o(SECTION; ~ = ro(integer); < >); ".";
   sl = rl(loc(sentence));
   { let is_section, sg = match s with None -> false, None | Some v -> true, v in
     { paragraph_name = Some i;
       paragraph_is_section = is_section;
       paragraph_segment = sg;
       paragraph_sentences = sl } }
(* segment number from 0 to 99 *)

let sentence :=
  (* | ~ = rl(loc(imperative_statement)); "."; < > *)
  | stmts = rl (loc_result (imperative_statement)); ".";
    { List.filter_map Result.to_option stmts }



(*
procedural statement

declarative statements    actions that may be taken during processing of other statements
                              start by USE
imperative statements     unconditional actions
                             unconditional action or conditional delimited by explicit termin
conditional statements    actions depending on a condition
                             conditional phrase not terminated by explicit terminator

imperative-statement = one or more imperative statements ended by separator DOT or
                       by any phrase associated with thet general format
*)


(* An imperative statement specifies an unconditional action to be taken by the
 * runtime element or is a conditional statement that is delimited by its explicit
 * scope terminator. *)
let imperative_statement [@recovery Result.Error "bad statement"] :=
 | ~ = unconditional_action;       <Result.Ok>
 | ~ = if_statement_explicit_term; <Result.Ok>

(* A conditional statement specifies that the truth value of a condition is evaluated
 * and used to determine subsequent flow of control. Any statement with a conditional
 * phrase that is not terminated by its explicit scope terminator is a conditional statement. *)
let if_statement_explicit_term :=
 | ~ = if_statement; < >                                       (* Conditional *)

let imp_stmts [@recovery []] [@symbol ""] :=
  | stmts = nell (loc_result (imperative_statement));
%prec lowest
   { List.filter_map Result.to_option stmts }
(* prec annotation needed to solve a conflict involving IF and WRITE *)


let oterm_(X) == er(X | {} %prec no_term)              (* optional terminators *)




(* ---------- Error handling rules ---------- *)

(* COB85: the negative case must be after the positive case *)
let handler (X, NOT_X) ==
 | X; isl = imp_stmts;     { { dual_handler_pos = isl; dual_handler_neg = [] } }
 | NOT_X; isl = imp_stmts; { { dual_handler_pos = []; dual_handler_neg = isl } }
 | (isl1, isl2) =
     mr( X; isl1 = imp_stmts; NOT_X; isl2 = imp_stmts; {isl1, isl2}
       | NOT_X; isl2 = imp_stmts; X; isl1 = imp_stmts; {isl1, isl2});
   { { dual_handler_pos = isl1; dual_handler_neg = isl2 } }

let handler_opt (X, NOT_X) ==
 | (* epsilon *)           { dual_handler_none }
 | ~ = handler (X, NOT_X); < >


let on_overflow := OVERFLOW | ON_OVERFLOW


(* Used by CALL *)
let overflow_or_exception_handler ==
 | on_overflow; ~ = imp_stmts;                 <CallOnOverflow>     (* &COB85 *)
 | ~ = handler(on_exception,NOT_ON_EXCEPTION); <CallOnException>
let on_exception := EXCEPTION | ON_EXCEPTION

(* Used by READ *)
let at_end_or_invalid_key_handler ==
 | h = handler(at_end,NOT_AT_END);           {ReadAtEnd, h}
 | h = handler(INVALID_KEY,NOT_INVALID_KEY); {ReadInvalidKey, h}
let at_end := END | AT_END

(* Used by WRITE *)
let end_of_page_or_invalid_key_handler ==
 | h = handler(at_eop,NOT_AT_EOP);           {WriteAtEndOfPage, h}
 | h = handler(INVALID_KEY,NOT_INVALID_KEY); {WriteInvalidKey, h}
let at_eop := EOP | AT_EOP | END_OF_PAGE



(* ---------- Identifier and literals ---------- *)



let name [@recovery dummy_name] [@symbol "<word>"] :=
 | i = loc(WORD); < >
let names := ~ = rnel(name); < >

let in_of := IN | OF

let qualname [@recovery dummy_qualname] [@symbol "<qualified name>"] :=
 | n = name; %prec lowest           {Name n: qualname}
 | n = name; in_of; qdn = qualname; {Qual (n, qdn)}
let qualnames := ~ = rnel(qualname); < >
let reference == qualname

let refmod ==
 | "("; refmod_left = expression_no_all;
   ":"; refmod_length = ro(expression_no_all); ")";
   { { refmod_left; refmod_length } }

let literal_int_ident :=
 | ~ = loc(DIGITS); < >
 | EIGHTY_EIGHT;   { with_loc "88" $sloc }

let procedure_name_decl :=
 | ~ = loc(WORD_IN_AREA_A); < >
 | ~ = procedure_name;      < >

let procedure_name := (* Can be present in paragraph or section name and level number *)
 | ~ = name;              < >
 | ~ = literal_int_ident; < >

let qualified_procedure_name :=
 | qdn = qualname;                 { qdn }
 | li = literal_int_ident;         { Name li }
 | pn1 = literal_int_ident; in_of;
   pn2 = literal_int_ident;        { Qual (pn1, Name pn2) }

let argument :=
 | e = expression_no_all; %prec lowest {ArgExpr e}
 | OMITTED;                            {ArgOmitted}

let arguments ==
 | "("; ")";                      { [] }
 | "("; al = rnel(argument); ")"; { al }

let optional_arguments_list :=
 | (* Empty *) %prec lowest       { [] }
 | ~ = arguments;                 < >

let subscript_first [@recovery SubSAll] [@symbol "<subscript>"] [@cost 0] :=
 | ALL;                                  {SubSAll}
 | e = expression_no_all;                {SubSExpr e}
 | i = name; s = sign; offset = integer; {SubSIdx (i, s, offset): subscript}

let subscript_following [@recovery SubSAll] [@symbol "<subscript>"] [@cost 0] :=
 | ALL;                                  {SubSAll}
 | e = expression_par_unop;              {SubSExpr e}
 | i = name; s = sign; offset = integer; {SubSIdx (i, s, offset): subscript}

let subscripts [@recovery []] [@symbol "<subscripts>"] [@cost 0] :=
 | "("; s = subscript_first; sl = rnel(subscript_following); ")"; { s::sl }
 | "("; s = subscript_first; ")";                                 { [s] }

(* Only for functions which name is also a keyword in COBOL *)
let intrinsic_function_name :=
 | LENGTH;      { "LENGTH" }
 | RANDOM;      { "RANDOM" }
 | REVERSE;     { "REVERSE" }
 | SIGN;        { "SIGN" }
 | SUM;         { "SUM" }

let function_name [@recovery dummy_name] [@symbol "<function-name>"] :=
 | ~ = name;                         < >
 | ~ = loc(intrinsic_function_name); < >

let inline_invocation :=
 | i = ident; "::"; l = literal; al = optional_arguments_list;
   { { invoke_class = i; invoke_meth = l; invoke_args = al } }

let object_view :=
  | i = ident; AS; s = object_view_spec;
    { { object_view_ident = i; object_view_spec = s } }

let object_view_spec ==
 | n = name;                    {ObjViewAmbiguous n: object_view_spec}
 | ~ = name; ONLY;              <ObjViewOnly>
 | FACTORY; OF; ~ = name;       <ObjViewFactory>
 | FACTORY; OF; ~ = name; ONLY; <ObjViewFactoryOnly>
 | UNIVERSAL;                   {ObjViewUniversal}

let object_ref :=
 | EXCEPTION_OBJECT;    {ExceptionObject}
 | NULL;                {Null}
 | SELF;                {Self}
 | n = name; OF; SUPER; {Super (Some n)}
 | SUPER;               {Super  None}

let address :=
 | ADDRESS; OF; i = ident;                     {DataAddress i}
 | ADDRESS; OF; PROGRAM; i = ident_or_literal; {ProgAddress i}

let counter :=
 | k = counter_kind; %prec lowest     { { counter_kind = k; counter_name = None   } }
 | k = counter_kind; in_of; n = name; { { counter_kind = k; counter_name = Some n } }

let counter_kind ==
 | LINAGE_COUNTER; {LineageCounter}
 | PAGE_COUNTER;   {PageCounter}
 | LINE_COUNTER;   {LineCounter}

let qualident ==
 | qdn = qualname;                  { { ident_name = qdn; ident_subscripts = [] } }
 | qdn = qualname; sl = subscripts; { { ident_name = qdn; ident_subscripts = sl } }

let function_ident ==
 | FUNCTION; n = function_name; al = arguments; { { call_fun = n; call_args = al } }
 | FUNCTION; n = function_name;                 { { call_fun = n; call_args = [] } }

let base_ident ==                 (* identifier without reference modification *)
 | q = qualident;                {QualIdent q} (* Works for object property too *)
 | f = function_ident;           {InlineCall f}
 | i = inline_invocation;        {InlineInvoke i}
 | o = object_view;              {ObjectView o}
 | r = object_ref;               {ObjectRef r} (* Includes predefined address (NULL) *)
 | a = address;                  {Address a}
 | c = counter;                  {Counter c}

let ident [@symbol "<identifier>"] [@recovery dummy_ident] :=
  | i = base_ident; %prec below_RPAR { UPCAST.base_ident_with_refmod i }
  | i = base_ident; r = refmod;      { RefMod (i, r) }

let idents [@symbol "<identifiers>"] [@recovery []] :=
  | ~ = rnel(ident); < >

let ident_or_literal
      [@symbol "<identifier or literal>"] [@cost 0]
      [@recovery Cobol_ptree.UPCAST.ident_with_literal dummy_ident] :=
  | i = ident; %prec lowest { UPCAST.ident_with_literal i }
  | l = literal;            { UPCAST.literal_with_ident l }

let figurative_constant [@recovery Zero]
      [@symbol "<figurative constant>"] :=
  |      ~ = figurative_constant_no_all; < >
  | ALL; l = nonnumeric_literal_no_all;  { All l }
(*ALL symbolic-character (alphanum, national) (defined in SPECIAL-NAMES)*)

let figurative_constant_no_all ==
  | ZERO;       {Zero} (* alphanum, national, boolean, numeric *)
  | SPACE;      {Space} (* alphanum, national *)
  | QUOTE;      {Quote} (* alphanum, national *)
  | LOW_VALUE;  {LowValue} (* alphanum, national *)
  | HIGH_VALUE; {HighValue} (* alphanum, national *)
(* | i = ident  { Symbolic i } *) (*conflict in ident_or_xxx_literal*)

let integers := ~ = rnel(integer); < >
let integer [@recovery "0"]
      [@symbol "<integer literal>"] :=
  | ~ = DIGITS;   < >
  | ~ = SINTLIT;  < >
  | EIGHTY_EIGHT; {"88"}

let fixedlit [@recovery fixed_zero] [@cost 10]
      [@symbol "<fixed-point literal>"] :=
  | (i, _, d) = FIXEDLIT; { Cobol_ptree.fixed_of_strings i d }

let floatlit [@recovery floating_zero] [@cost 10]
      [@symbol "<floating-point literal>"] :=
  | (i, _, d, e) = FLOATLIT; { Cobol_ptree.floating_of_strings i d e }

let alphanum ==             (* TODO: attach interpretation (hex, etc) into AST *)
 | a = ALPHANUM; { fst a, (match snd a with Apostrophe -> Squote | Quote -> Dquote ) }
 | h = HEXLIT;   { h, Hex }

let literal [@recovery Integer "0"] [@symbol "<literal>"] :=
 | a = alphanum;  {Alphanum a}
 | n = NATLIT;    {National n}
 | b = BOOLIT;    {Boolean b}
 | i = integer;   {Integer i}
 | f = fixedlit;  {Fixed f}
 | f = floatlit;  {Floating f}
 | f = figurative_constant;            {Fig f}
 | l1 = nonnumeric_literal_no_all; "&";
   l2 = nonnumeric_literal_no_all;     {Concat (l1, l2): literal}

(*
literal_no_all:
 | l = elementary_literal             { l }
 | f = figurative_constant_no_all     { Figurative (f) }
 | l1 = nonnumeric_literal_no_all "&"
   l2 = nonnumeric_literal_no_all     { Concat (l1, l2) : literal }
;
*)

(* concat : both operands shall be of the same class *)
(* result of concatenation shall be less than 8191 character positions *)
(* was 160 in older standard (COB85 ?) *)
(* note : & should be between spaces *)




let numeric_literal [@symbol "<numeric literal>"] :=
 | i = integer;  { Integer i : numlit }
 | f = fixedlit; { Fixed f }
 | f = floatlit; { Floating f }
 | ZERO;         { NumFig Zero }
(* Note: numeric literals do NOT allow figurative constants with ALL *)




let elementary_string_literal ==
 | a = alphanum; { Alphanum a }
 | n = NATLIT;   { National n : strlit }

let string_literal [@symbol "<string literal>"] :=
 | l = elementary_string_literal;  { l }
 | f = figurative_constant;        { Fig f }
 | l1 = string_literal_no_all; "&";
   l2 = string_literal_no_all;     { StrConcat (l1, l2) : strlit }

let string_literal_no_all [@symbol "<string literal>"] :=
 | l = elementary_string_literal;  { l: strlit }
 | f = figurative_constant_no_all; { Fig f }
 | l1 = string_literal_no_all; "&";
   l2 = string_literal_no_all;     { StrConcat (l1, l2) : strlit }




elementary_string_or_int_literal:
 | a = alphanum { Alphanum a }
 | n = NATLIT   { National n }
 | i = integer  { Integer i }

string_or_int_literal:
 | l = elementary_string_or_int_literal { l }
 | f = figurative_constant              { Fig f }
 | l1 = string_literal_no_all "&"
   l2 = string_literal_no_all       { StrConcat (l1, l2) : strlit_or_intlit }

(*
string_or_int_literal_no_all:
 | l = elementary_string_or_int_literal  { l }
 | f = figurative_constant_no_all        { Figurative (f) }
 | l1 = string_or_int_literal_no_all "&"
   l2 = string_or_int_literal_no_all { Concat (l1, l2) : strlit_or_intlit }
*)

elementary_nonnumeric_literal:
 | a = alphanum { Alphanum a }
 | n = NATLIT   { National n }
 | b = BOOLIT   { Boolean b }

nonnumeric_literal:
 | l = elementary_nonnumeric_literal  { l }
 | f = figurative_constant            { Fig f }
 | l1 = nonnumeric_literal_no_all "&"
   l2 = nonnumeric_literal_no_all     { Concat (l1, l2): nonnumlit }

nonnumeric_literal_no_all:
 | l = elementary_nonnumeric_literal  { l }
 | f = figurative_constant_no_all     { Fig f }
 | l1 = nonnumeric_literal_no_all "&"
   l2 = nonnumeric_literal_no_all     { Concat (l1, l2): nonnumlit }





(* Used in many *)
let qualname_or_literal :=
 | n = qualname; { UPCAST.qualname_with_literal n }
 | l = literal;  { UPCAST.literal_with_qualdatname l }

(* Used in ADD, DIVIDE, MULTIPLY, PERFORM, SUBTRACT *)

let ident_or_numeric :=
 | i = ident;           { UPCAST.ident_with_numeric i }
 | l = numeric_literal; { UPCAST.numeric_with_ident l }
let idents_or_numerics == ~ = rnel(ident_or_numeric); < >

(* Used in CALL *)
let name_or_string :=
 | i = name;           { Name i }
 | s = string_literal; { UPCAST.string_with_name s }

let ident_or_string :=
 | i = ident; %prec below_RPAR { UPCAST.ident_with_string i }
 | s = string_literal;         { UPCAST.string_with_ident s }
let idents_or_strings == ~ = rnel(ident_or_string); < >

(* UNSTRING *)
(* These statements explicitly forbid the ALL literal,
   which (by chance) helps solve some conflicts *)

let ident_or_string_no_all :=
 | i = ident;                 { UPCAST.ident_with_string i }
 | l = string_literal_no_all; { UPCAST.string_with_ident l }

(* INSPECT, STRING, TRANSFORM *)

let ident_or_nonnumeric :=
 | i = ident;              { UPCAST.ident_with_nonnum i }
 | l = nonnumeric_literal; { UPCAST.nonnum_with_ident l }

(* INSPECT, STRING, TRANSFORM *)
(* These statements explicitly forbid the ALL literal,
   which (by chance) helps solve some conflicts *)
let ident_or_nonnumeric_no_all :=
 | i = ident;                     { UPCAST.ident_with_nonnum i }
 | l = nonnumeric_literal_no_all; { UPCAST.nonnum_with_ident l }

(* Used in ENABLE, DISABLE *)
let name_or_alphanum :=
 | i = name;     { Name i }
 | a = alphanum; { Alphanum a }

let qualname_or_alphanum :=
 | n = qualname; { UPCAST.qualname_with_alphanum n }
 | a = alphanum; { Alphanum a }

let ident_or_alphanum :=
 | i = ident;    { UPCAST.ident_with_alphanum i }
 | a = alphanum; { Alphanum a }

(* Used in line_number (ACCEPT, DISPLAY) and PERFORM *)
let qualname_or_integer :=
 | n = qualname; { UPCAST.qualname_with_integer n }
 | i = integer;  { Integer i }

let ident_or_integer :=
 | i = ident;   { UPCAST.ident_with_integer i }
 | ZERO;        { NumFig Zero }
 | i = integer; { Integer i : ident_or_intlit }

(* ---------- Expressions ---------- *)

let expression [@recovery Atom (Fig Zero)] [@symbol "<expression>"] [@cost 0] :=
 | e1 = expression; "+"; e2 = expr_term; { Binop (e1, BPlus, e2) }
 | e1 = expression; "-"; e2 = expr_term; { Binop (e1, BMinus, e2) }
 | e = expr_term;                        { e }

let expression_no_all [@recovery dummy_expr] [@symbol "<expression>"] (* [@cost 0]  *):=
 | e1 = expression_no_all; "+"; e2 = expr_term_no_all; { Binop (e1, BPlus, e2) }
 | e1 = expression_no_all; "-"; e2 = expr_term_no_all; { Binop (e1, BMinus, e2) }
 | e = expr_term_no_all;                               { e }

let expression_par_unop  [@recovery dummy_expr] [@symbol "<expression>"] (* [@cost 0]  *) :=
 | e1 = expression_par_unop; "+"; e2 = expr_term; { Binop (e1, BPlus, e2) }
 | e1 = expression_par_unop; "-"; e2 = expr_term; { Binop (e1, BMinus, e2) }
 | e = expr_term_par_unop;                        { e }

(* --- *)

expr_term:
 | e1 = expr_term o = binop e2 = expr_factor { Binop (e1, o, e2) }
 | e = expr_factor                           { e }

expr_term_no_all:
 | e1 = expr_term_no_all o = binop e2 = expr_factor_no_all { Binop (e1, o, e2) }
 | e = expr_factor_no_all                           { e }

expr_term_par_unop:
 | e1 = expr_term_par_unop o = binop e2 = expr_factor { Binop (e1, o, e2) }
 | e = expr_factor_par_unop                           { e }

(* --- *)

expr_factor:
 | e1 = expr_unary "**" e2 = expr_factor { Binop (e1, BPow, e2) }
 | e = expr_unary                       { e }

expr_factor_no_all:
 | e1 = expr_unary_no_all "**" e2 = expr_factor_no_all { Binop (e1, BPow, e2) }
 | e = expr_unary_no_all                       { e }

expr_factor_par_unop:
 | e1 = expr_unary_par "**" e2 = expr_factor { Binop (e1, BPow, e2) }
 | e = expr_unary_par                       { e }

(* --- *)

expr_unary:
 | e = atomic_expression          { e }
 | o = unop e = atomic_expression { Unop (o, e) }

let expr_unary_no_all ==
 | e = atomic_expression_no_all;           { e }
 | o = unop; e = atomic_expression_no_all; { Unop (o, e) }

let expr_unary_par == atomic_expression_no_all

(* --- *)

let atomic_expression [@recovery dummy_expr] [@symbol "<atomic expression>"] :=
 | e = arithmetic_term;      { e }
 | "("; e = expression; ")"; { e } (* arith or boolean *)

let atomic_expression_no_all [@recovery dummy_expr] [@symbol "<atomic expression>"] :=
 | e = arithmetic_term_no_all;      { e }
 | "("; e = expression_no_all; ")"; { e } (* arith or boolean *)

(* --- *)

arithmetic_term:
 | i = ident               { Atom (UPCAST.ident_with_literal i) } (* numeric or boolean *)
 | i = integer             { Atom (Integer i) }
 | f = fixedlit            { Atom (Fixed f) }
 | f = floatlit            { Atom (Floating f) }
 | b = BOOLIT              { Atom (Boolean b) } (* boolean *)
 | f = figurative_constant { Atom (Fig f) } (* numeric or boolean (NB: or strlits) *)
 | a = alphanum            { Atom (Alphanum a) } (* NB: quick relaxation for now *)

arithmetic_term_no_all:
 | i = ident    { Atom (UPCAST.ident_with_literal i) } (* numeric or boolean *)
 | i = integer  { Atom (Integer i) }
 | f = fixedlit { Atom (Fixed f) }
 | f = floatlit { Atom (Floating f) }
 | b = BOOLIT   { Atom (Boolean b) } (* boolean *)
 | a = alphanum { Atom (Alphanum a) }         (* NB: quick relaxation for now *)
 | f = figurative_constant_no_all { Atom (Fig f) } (* numeric or boolean (NB: or strlits) *)

(* --- *)

%inline binop:
 | "*"   { BMul }
 | "/"   { BDiv }
 | B_AND { BAnd }
 | B_OR  { BOr }
 | B_XOR { BXor }

%inline unop:
 | "+"   { UPlus }
 | "-"   { UMinus }
 | B_NOT { UNot }

(* ---------- Conditions ---------- *)



condition:
 | complex_condition { $1 }

complex_condition:
 | nonrel_condition { $1 }
 | flat_relation_condition %prec lowest { $1 }
 | complex_condition logop complex_condition { Logop ($1, $2, $3) }

%inline logop:
 | AND           { LAnd }
 | OR            { LOr }

%inline flat_relation_condition:
 | neg = ibo(NOT) c = relation_condition
   suff = io (pair (logop, flat_combination_operand))
   { relation_condition ~neg c suff }

nonrel_condition:
 | n = ibo(NOT)     e = expression %prec lowest { neg_simple_cond ~neg:n @@ Expr e }
 | n = ibo(NOT)     c = extended_condition      { neg_condition ~neg:n c }
 | n = ibo(NOT) "(" c  = complex_condition ")"  { neg_condition ~neg:n c }

flat_combination_operand:
 | r = io(relop)    e = expression             { FlatAmbiguous (r, e) }
 |         NOT      e = expression             { FlatNotExpr e }
 | n = ibo(NOT)     c = relation_condition     { FlatRel (n, c) }
 | n = ibo(NOT)     c = extended_condition     { FlatOther (neg_condition ~neg:n c) }
 | n = ibo(NOT) "(" c =  complex_condition ")" { FlatOther (neg_condition ~neg:n c) }
 | flat_combination_operand logop flat_combination_operand
                                               { FlatComb ($1, $2, $3) }

relation_condition:
 | expression relop expression { $1, $2, $3 }

extended_condition:
 | e = expression io(IS) n = bo(NOT) c = class_condition
    { neg_simple_cond ~neg:n @@ ClassCond (e, c) } (* exp = ident *)
 | e = expression io(IS) n = bo(NOT) s = sign_condition
    { neg_simple_cond ~neg:n @@ SignCond (e, s) } (* exp = arith exp *)
 | e = expression io(IS) n = bo(NOT) OMITTED
    { neg_simple_cond ~neg:n @@ Omitted e } (* exp = ident *)

relop [@recovery Eq] [@symbol "<relational arithmetic operator>"]:
 | io(IS) n = ibo(NOT) GREATER THAN?
 | io(IS) n = ibo(NOT) ">"            { if n then Le else Gt }
 | io(IS) n = ibo(NOT) LESS THAN?
 | io(IS) n = ibo(NOT) "<"            { if n then Ge else Lt }
 | io(IS) n = ibo(NOT) EQUAL TO?
 | io(IS) n = ibo(NOT) "="            { if n then Ne else Eq }
 | io(IS) n = ibo(NOT) "<>"           { if n then Eq else Ne }
 | io(IS) n = ibo(NOT) GREATER THAN? OR EQUAL TO?
 | io(IS) n = ibo(NOT) ">="           { if n then Lt else Ge }
 | io(IS) n = ibo(NOT) LESS THAN? OR EQUAL TO?
 | io(IS) n = ibo(NOT) "<="           { if n then Gt else Le }

class_condition:
 | i = name                     { AlphabetOrClass i }
 | c = class_condition_no_ident { c }

class_condition_no_ident [@recovery ClassNumeric]:
 | ALPHABETIC                   { Alphabetic }
 | ALPHABETIC_LOWER             { AlphabeticLower }
 | ALPHABETIC_UPPER             { AlphabeticUpper }
 | BOOLEAN                      { ClassBoolean }
 | FARTHEST_FROM_ZERO           { FarthestFromZero }
 | FLOAT_INFINITY               { FloatInfinity }
 | FLOAT_NOT_A_NUMBER           { FloatNotANumber }
 | FLOAT_NOT_A_NUMBER_QUIET     { FloatNotANumberQuiet }
 | FLOAT_NOT_A_NUMBER_SIGNALING { FloatNotANumberSignaling }
 | IN_ARITHMETIC_RANGE          { InArithmeticRange }
 | NEAREST_TO_ZERO              { NearestToZero }
 | NUMERIC                      { ClassNumeric }

sign_condition:
 | s = sign_condition_no_zero { s }
 | ZERO                       { SgnZero }

sign_condition_no_zero [@recovery SgnPositive]:
 | POSITIVE { SgnPositive }
 | NEGATIVE { SgnNegative : signz }



(* ---------- Rules common to several statements ---------- *)



(* ACCEPT, DISPLAY *)

let position :=
 | l = line_number;                    { LinePosition l }
 | c = column_number;                  { ColumnPosition c }
 | l = line_number; c = column_number; { LineColumnPosition (l, c) }
 | c = column_number; l = line_number; { LineColumnPosition (l, c) }

let line_number :=
 | LINE; NUMBER?; ~ = ident_or_integer; < >
(* integer must be unsigned *)

let column_number :=
 | or_(COL,COLUMN); NUMBER?; ~ = ident_or_integer; < >
(* integer must be unsigned *)



(* ADD, COMPUTE, DIVIDE, MULTIPLY, SUBTRACT *)

let rounded_ident :=
  | i = ident; rm = rounded_phrase_opt;
    { { rounded_ident = i; rounded_rounding = rm} }
let rounded_idents == ~ = rnel(rounded_ident); < >

let rounded_phrase_opt :=
  | (* epsilon *)                         {RoundingNotAny} (* = ROUNDED MODE TRUNCATION *)
  | ~ = rounded_phrase; < >

let rounded_phrase [@context rounded_phrase] :=
  | ROUNDED;                              {RoundingDefault} (* use default rounding *)
  | ROUNDED; MODE; IS?; ~ = rounding_mode; <RoundingMode>

let rounding_mode :=
 | AWAY_FROM_ZERO;           {AwayFromZero}
 | NEAREST_AWAY_FROM_ZERO;   {NearestAwayFromZero}
 | NEAREST_EVEN;             {NearestEven}
 | NEAREST_TOWARD_ZERO;      {NearestTowardZero}
 | PROHIBITED;               {Prohibited}
 | TOWARD_GREATER;           {TowardGreater}
 | TOWARD_LESSER;            {TowardLesser}
 | TRUNCATION;               {Truncation}



(* ALLOCATE, CALL, INVOKE, Procedure division header *)

let returning := RETURNING; ~ = loc(ident); < >



(* CALL, INVOKE *)
(* Ambiguous & different between standards *)
(*
COB85:
  (BY REFERENCE)? identifier...
  BY CONTENT identifier...
COB2002:
  (BY REFERENCE)? identifier/OMITTED
  (BY CONTENT)? identifier/literal/expression
  (BY VALUE)? identifier/literal/expression
*)

let using_by :=
 | b = call_using_by?; e = arithmetic_term;
   { { call_using_by = b;
       call_using_expr = Some e } }             (* COB85: ident, COB2002: exp *)
 | b = call_using_by?; OMITTED;
   { { call_using_by = b;
       call_using_expr = None } }                                 (* +COB2002 *)

let call_using_by [@recovery CallUsingByReference] :=
 | BY?; REFERENCE; {CallUsingByReference}
 | BY?; CONTENT;   {CallUsingByContent}
 | BY?; VALUE;     {CallUsingByValue}                             (* +COB2002 *)



(* DELETE, OPEN, READ, REWRITE, WRITE *)

let retry_phrase [@context retry_phrase] :=
 | RETRY; ~ = expression; TIMES;        <RetryNTimes>
 | RETRY; FOR; ~ = expression; SECONDS; <RetryForNSeconds>
 | RETRY; FOREVER;                      {RetryForever}



(* DISABLE, ENABLE *)

let with_key :=
 | WITH?; KEY; ~ = ident_or_alphanum; < >

let mcs_kind :=
 | INPUT; TERMINAL?; {MCSInput}
 | I_O; TERMINAL;    {MCSInputOutput}
 | OUTPUT;           {MCSOutput}

let mcs_command :=
  | io = mcs_kind; i = name; iao = io(with_key);
    { { mcs_command_kind = io;
        mcs_command_target = i;
        mcs_command_key = iao } }


(* EXIT, GOBACK *)

let raising_exception :=                                           (* COB2002+ *)
 | RAISING; i = ident;           {RaisingIdent i}
 | RAISING; EXCEPTION; i = name; {RaisingException i}
 | RAISING; LAST; EXCEPTION;     {RaisingLastException}
 | RAISING; LAST; %prec lowest   {RaisingLastException} (* unexplainable conflict using option *)



(* MERGE, SORT *)

let on_key :=
 | ON?; d = sort_direction; KEY?; il = qualnames;
   { { sort_key_direction = d; sort_key_names = il } }
 (* | ON?; DESCENDING; KEY?; il = qualnames; *)
 (*   { { sort_key_direction = SortDescending; sort_key_names = il } } *)

let collating_sequence_phrase :=
 | COLLATING?; SEQUENCE; ~ = alphabet_specification; < >

let alphabet_specification :=
 | IS?; i = name; io = ro(name); { { alphanumeric = Some i; national = io } }
 | i = cs_alphanumeric;          { { alphanumeric = Some i; national = None } }
 | i = cs_national;              { { alphanumeric = None; national = Some i } }
 | (ia, in_) = mr( ia = cs_alphanumeric; in_ = cs_national; {ia, in_}
                 | in_ = cs_national; ia = cs_alphanumeric; {ia, in_});
   { { alphanumeric = Some ia; national = Some in_ } }

let cs_alphanumeric := FOR; ALPHANUMERIC; IS?; ~ = name; < >
let cs_national := FOR; NATIONAL; IS?; ~ = name; < >

let output_or_giving :=
 | OUTPUT; PROCEDURE; IS?; i = procedure_name; io = ro(pf(THROUGH,procedure_name));
   { OutputProcedure { procedure_start = i; procedure_end = io } }
 | GIVING; ~ = names; <Giving>



(* OPEN, USE *)

let open_mode :=
 | INPUT;  {OpenInput}
 | OUTPUT; {OpenOutput}
 | I_O;    {OpenInputOutput}
 | EXTEND; {OpenExtend}


(* PERFORM, SEND *)

let after_or_before :=
 | AFTER;  {After}
 | BEFORE; {Before}


(* READ, REWRITE, WRITE *)

let with_lock :=
 | /*%prec lowest*/    {None}
 | io(WITH); LOCK;     {Some true}
 | io(WITH); NO; LOCK; {Some false}


(* ---------- Rules specific to a single statement ---------- *)



(* ACCEPT STATEMENT *)

(* TODO: FROM and TO and USING clauses *)
%public let unconditional_action := ~ = accept_statement; <Accept>
accept_statement [@context accept_stmt]:
 | ACCEPT i = loc(ident) end_accept
   { AcceptGeneric i }
 | ACCEPT i1 = loc(ident) FROM i2 = name end_accept
   { AcceptFromDevice { item = i1; device_item = i2 } }
 | ACCEPT i = loc(ident) FROM ddt = date_day_time end_accept
   { AcceptTemporal { item = i; date_time = ddt } }
 | ACCEPT i = name MESSAGE? COUNT end_accept            (* -COB2002 *)
   { AcceptMsgCount i }
 | ACCEPT i = name AT p = position                      (* +COB2002 *)
   h = handler_opt(on_exception,NOT_ON_EXCEPTION) end_accept
   { AcceptAtScreen { item = i;
                    position = Some p;
                    on_exception = h; } }
 | ACCEPT i = name
   h = handler(on_exception,NOT_ON_EXCEPTION) end_accept (* +COB2002 *)
   { AcceptAtScreen { item = i;
                      position = None;
                      on_exception = h; } }
 | ACCEPT item = loc(ident)                                             (* MF *)
   FROM ENVIRONMENT env_item = loc(ident_or_nonnumeric_no_all)
   on_exception = handler_opt(on_exception,NOT_ON_EXCEPTION) end_accept
   { AcceptFromEnv { item; env_item; on_exception } }

let end_accept := oterm_(END_ACCEPT)

let date_day_time :=
 | DATE; y = bo(YYYYMMDD); {Date y}                      (* YYYYMMDD +COB2002 *)
 | DAY; y = bo(YYYYDDD);   {Day y}                       (* YYYYDDD +COB2002 *)
 | DAY_OF_WEEK;            {DayOfWeek}
 | TIME;                   {Time}



(* ADD STATEMENT *)

%public let unconditional_action := ~ = add_statement; < >
add_statement:
 | ADD inl = idents_or_numerics TO irl = rounded_idents
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR) end_add
   { Add { basic_arith_operands =
             ArithSimple { sources = inl; targets = irl };
           basic_arith_on_size_error = h } }
 | ADD inl = idents_or_numerics TO in_ = ident_or_numeric
   GIVING irl = rounded_idents
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR) end_add
   { Add { basic_arith_operands =
             ArithGiving { sources = inl;
                           to_or_from_item = in_;
                           targets = irl };
           basic_arith_on_size_error = h } }
 | ADD inl = idents_or_numerics (* Same as above without 'TO' *)
   GIVING irl = rounded_idents
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR) end_add
   { let in_, inl = split_last inl in
     Add { basic_arith_operands =
             ArithGiving { sources = inl;
                           to_or_from_item = in_;
                           targets = irl };
           basic_arith_on_size_error = h } }
 | ADD CORRESPONDING i = qualname TO ir = rounded_ident
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR) end_add
   { Add { basic_arith_operands =
             ArithCorresponding { source = i; target = ir };
           basic_arith_on_size_error = h } }

let end_add := oterm_(END_ADD)


(* ALLOCATE STATEMENT (+COB2002) *)

%public let unconditional_action := ~ = allocate_statement; < >
let allocate_statement [@context allocate_stmt] :=
 | ALLOCATE; e = expression; CHARACTERS; ib = bo(INITIALIZED); r = returning;
   { Allocate { allocate_kind = AllocateCharacters e;
                allocate_initialized = ib;
                allocate_returning = Some r } }
 | ALLOCATE; i = name; ib = bo(INITIALIZED); ro = ro(returning);
   { Allocate { allocate_kind = AllocateDataItem i;
                allocate_initialized = ib;
                allocate_returning = ro } }



(* ALTER STATEMENT (~COB85, -COB2002) *)

%public let unconditional_action := ~ = alter_statement; < >
let alter_statement :=
  | ALTER; ~ = l(loc(i1 = qualified_procedure_name; TO; o(PROCEED; TO);
                     i2 = qualified_procedure_name;
                     { { alter_source = i1; alter_target = i2 } })); <Alter>



(* CALL STATEMENT *)

%public let unconditional_action := ~ = call_statement; < >
let call_statement :=
  | CALL; cp = call_prefix; ul = lo(pf(USING,rnel(loc(using_by))));
    ro = ro(returning); oeho = io(overflow_or_exception_handler);
    oterm_(END_CALL);
    { Call { call_prefix = cp;
             call_using = ul;
             call_returning = ro;
             call_error_handler = oeho } }

let call_prefix :=
  | i = ident_or_string; <CallGeneral>
  | ian = ident_or_string; AS; in_ = ident_or_nested;
    { CallProto { called = Some ian; prototype = in_ } }
  | NESTED;
    { CallProto { called = None; prototype = CallProtoNested } }

let ident_or_nested :=
  | ~ = ident; <CallProtoIdent>
  | NESTED;    {CallProtoNested}

(* CANCEL STATEMENT (+COB85) *)

%public let unconditional_action := ~ = cancel_statement; < >
let cancel_statement :=
  | CANCEL; ~ = idents_or_strings; <Cancel>



(* CLOSE STATEMENT *)

%public let unconditional_action := ~ = close_statement; < >
let close_statement :=
  | CLOSE; ~ = nel(i = name; cf = ro(close_format);
                   { { close_item = i; close_format = cf } }); <Close>

let close_format :=
  | or_(REEL,UNIT); ~ = bo(FOR?; REMOVAL); <CloseUnitReel>
  | WITH?; NO; REWIND;                     {CloseWithNoRewind}
  | WITH?; LOCK;                           {CloseWithLock}



(* COMPUTE STATEMENT *)

%public let unconditional_action := ~ = compute_statement; < >
let compute_statement :=
  | COMPUTE; irl = rounded_idents; "="; e = expression;
    h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR);
    oterm_(END_COMPUTE);
    { Compute { compute_targets = irl; compute_expr = e;
                compute_on_size_error = h } }
(* COB2002: added boolean form with bool_expr and no rounded (and no err) *)


(* CONTINUE STATEMENT *)

%public let unconditional_action := ~ = continue_statement; < >
let continue_statement := CONTINUE; {Continue}



(* DELETE STATEMENT *)

%public let unconditional_action := ~ = delete_statement; < >
let delete_statement :=
 | DELETE; i = name; RECORD?; ro = ro(retry_phrase);
   h = handler_opt(INVALID_KEY,NOT_INVALID_KEY);
   oterm_(END_DELETE);
   { Delete { delete_targets = i; delete_retry = ro;
              delete_on_invalid_key = h} }
(* retry: +COB2002 *)



(* DISABLE STATEMENT (+COB85, -COB2002) *)

%public let unconditional_action := ~ = disable_statement; < >
let disable_statement :=
 | DISABLE; ~ = mcs_command; <Disable>



(* DISPLAY STATEMENT *)

(* TODO: FROM and USING clauses *)
%public let unconditional_action := ~ = display_statement; <Display>
let display_statement :=
  (* Ambiguous case *)
  | DISPLAY; i = ident_or_literal; end_display;
    { DisplayDefault i }

  (* Device case (disambiguated) *)
  | DISPLAY; i = ident_or_literal; d = display_device_disambiguated; end_display;
    { let ill, io, wna = d in
      DisplayDevice { displayed_items = i :: ill;
                      upon = io;
                      advancing = wna } }

  (* Screen case (disambiguated) *) (* +COB2002 *)
  | DISPLAY; i = name; dsd = display_screen_disambiguated; end_display;
    { DisplayScreen { screen_item = i;
                      position = fst dsd;
                      on_exception = snd dsd; } }

let end_display := oterm_(END_DISPLAY)

let display_device_disambiguated ==
 | ~ = rnel(ident_or_literal); ~ = ro(loc(upon)); ~ = ibo(with_no_advancing); < >
 | i = loc(upon); wna = ibo(with_no_advancing); { [], Some i, wna }
 | with_no_advancing;                      { [], None, true }

let upon :=
 | UPON; ~ = name; <DisplayUponName>
 | UPON; ~ = loc(display_device_mnemonic); <DisplayUponDeviceViaMnemonic>
let display_device_mnemonic ==
 | ENVIRONMENT_NAME;  {DisplayDeviceEnvName}
 | ENVIRONMENT_VALUE; {DisplayDeviceEnvValue}
 | ARGUMENT_NUMBER;   {DisplayDeviceArgNumber}
 | COMMAND_LINE;      {DisplayDeviceCommandLine}
let with_no_advancing := io(WITH); NO; ADVANCING

let display_screen_disambiguated ==
 | AT; p = position; h = handler_opt(on_exception,NOT_ON_EXCEPTION); { Some p, h }
 | h = handler(on_exception,NOT_ON_EXCEPTION); { None, h }


(* DIVIDE STATEMENT *)
(* Slightly ambiguous: INTO ident ? *)
(* Also deal with remainder -> single ir TODO *)

%public let unconditional_action := ~ = divide_statement; <Divide>
divide_statement:
 | DIVIDE in_ = ident_or_numeric INTO irl = rounded_idents
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR); end_divide
   { { divide_operands = DivideInto { divisor = in_; dividends = irl };
       divide_on_size_error = h } }
 | DIVIDE in1 = ident_or_numeric INTO in2 = ident_or_numeric
   GIVING irl = rounded_idents ro = ro(pf(REMAINDER,ident))
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR); (* no remainder: single ir *)
   end_divide
   { { divide_operands = DivideGiving { divisor = in1;
                                        dividend = in2;
                                        giving = irl;
                                        into = true;
                                        remainder = ro };
       divide_on_size_error = h } }
 | DIVIDE in1 = ident_or_numeric BY in2 = ident_or_numeric
   GIVING irl = rounded_idents ro = ro(pf(REMAINDER,ident))
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR); (* no remainder: single ir *)
   end_divide
   { { divide_operands = DivideGiving { dividend = in1;
                                        divisor = in2;
                                        giving = irl;
                                        into = false;
                                        remainder = ro };
       divide_on_size_error = h } }

let end_divide := oterm_(END_DIVIDE)


(* ENABLE STATEMENT (+COB85, -COB2002) *)

%public let unconditional_action := ~ = enable_statement; < >
(* let enable_statement := *)
(*  | ENABLE; io = input_output; i = name; iao = io(with_key); *)
(*    { Enable { enable_kind = io; enable_target =  i; enable_key = iao} } *)

let enable_statement :=
 | ENABLE; ~ = mcs_command; <Enable>


(* ENTER STATEMENT (~COB85, -COB2002) *)

%public let unconditional_action := ~ = enter_statement; < >
let enter_statement :=
 | ENTER; i = name; io = ro(name); ".";
   { Enter { enter_language = i; enter_routine = io} }



(* EVALUATE STATEMENT (+COB85) *)
(* COBOL 85 and COBOL 2002 differ significantly *)
(* Particularly ambiguous, especially the selection subjects/objects *)
(*

selection subject:

 when ident is numeric or boolean of length 1, it is really
 considered as an ident and not as an arith/bool expression

selection object:

partial_exp = sequence of cobol words starting with
  a relational operator,
  a class condition without identifier,
  a sign condition without identifier,
  a sign condition without the arithmetic expression

selection_subject followed by partial_exp = conditional exp

Partial exp = ident/zero is ambiguous, could be
 NOT? class_condition / sign_condition (partial exp)
 NOT? expression (ident/zero)
*)

%public let unconditional_action := ~ = evaluate_statement; < >
let evaluate_statement :=
 | EVALUATE; ssl = selection_subjects; wl = nell(when_phrase);
   isl = when_other; oterm_(END_EVALUATE);
   { Evaluate { eval_subjects = ssl;
                eval_branches = wl;
                eval_otherwise = isl; } }

let selection_subjects :=
 | ss = selection_subject;                                 { [ss] }
 | ss = selection_subject; ALSO; ssl = selection_subjects; { ss :: ssl }

let selection_subject :=
 | c = condition;  {Subject c}                        (* also arith/bool expr *)
 | TRUE;           {SubjectConst true}
 | FALSE;          {SubjectConst false}

let selection_objects :=
 | so = selection_object;                                { [so] }
 | so = selection_object; ALSO; sol = selection_objects; { so :: sol }

let selection_object :=
 | c = condition;           {SelCond c}                 (* also arith/bool exp*)
 | ~ = range_expression;    < >
 | ~ = partial_expression;  < >                                   (* +COB2002 *)
 | TRUE;                    {SelConst true}
 | FALSE;                   {SelConst false}
 | ANY;                     {SelAny: selection_object}

let range_expression :=
 | b = ibo(NOT); i1 = expression; THROUGH;
   i2 = expression; i = ro(pf(IN,name));
   { SelRange { negated = b; start = i1; stop = i2; alphabet = i } }

let partial_expression :=
 | o = relop; e = expression;
   { SelRelation { relation = o; expr = e } } (* relation (general, bool, pointer) *)
 | IS; n = bo(NOT); c = class_condition;
   { SelClassCond { negated = n; class_specifier = c } } (* class *) (* exp = ident *)
 | n = bo(NOT); c = class_condition_no_ident;
   { SelClassCond { negated = n; class_specifier = c } } (* class *) (* exp = ident *)
 | IS; n = bo(NOT); s = sign_condition;
   { SelSignCond { negated = n; sign_specifier = s } } (* sign *) (* exp = arith exp *)
 | n = bo(NOT); s = sign_condition_no_zero;
   { SelSignCond { negated = n; sign_specifier = s } } (* sign *) (* exp = arith exp *)
 | io(IS); n = bo(NOT); OMITTED;
   { SelOmitted { negated = n } }                   (* omitted *) (* exp = ident *)

let when_phrase :=
 | wl = rnel(when_selection_objects); isl = imp_stmts;
   { {eval_selection = wl; eval_actions = isl} }

let when_selection_objects := WHEN; ~ = selection_objects; < >

let when_other :=
 | %prec lowest { [] }
 | WHEN; OTHER; ~ = imp_stmts; < >



(* EXIT STATEMENT *)

%public let unconditional_action := ~ = exit_statement; < >
let exit_statement [@context exit_stmt] :=
 | EXIT; ~ = exit_spec; <Exit>
let exit_spec [@recovery ExitSimple] :=
 | %prec lowest                          { ExitSimple }
 | PROGRAM; ro = ro(raising_exception);  { ExitProgram ro }
 | METHOD; ro = ro(raising_exception);   { ExitMethod ro }        (* COB2002+ *)
 | FUNCTION; ro = ro(raising_exception); { ExitFunction ro }      (* COB2002+ *)
 | PERFORM; c = bo(CYCLE);               { ExitPerform c }        (* COB2002+ *)
 | PARAGRAPH;                            { ExitParagraph }        (* COB2002+ *)
 | SECTION;                              { ExitSection }          (* COB2002+ *)



(* FREE STATEMENT (+COB2002) *)

%public let unconditional_action := ~ = free_statement; < >
let free_statement :=
 | FREE; ~ = names; <Free>


(* GENERATE STATEMENT (+COB85, -COB2002) *)

%public let unconditional_action := ~ = generate_statement; < >
let generate_statement :=
 | GENERATE; ~ = name; <Generate>



(* GO TO STATEMENT *)

%public let unconditional_action := ~ = go_to_statement; < >
let go_to_statement :=
 | GO; TO?; i = qualified_procedure_name;
   { GoTo i }
 | GO; TO?; il = rnel(qualified_procedure_name);
   DEPENDING; ON?; i = ident;
   { GoToDepending { goto_depending_targets = il; goto_depending_on = i; } }
 | GO; TO?;         (* COB85; obsolete; should be sole statement of paragraph *)
   { LoneGoTo }



(* GOBACK STATEMENT (+COB2002) *)

%public let unconditional_action := ~ = goback_statement; < >
let goback_statement :=
 | GOBACK; ~ = ro(raising_exception); <GoBack>



(* IF STATEMENT *)

let if_statement :=
 | IF; c = condition; THEN?; ib = if_body; oterm_(END_IF);
   { let sn, sno = ib in
     If {condition = c; then_branch = sn; else_branch = sno} }
(* COB2002: END IF mandatory on ELSE, NEXT STATEMENT archaic *)

let if_body :=
 | isl = imp_stmts; %prec lowest      { Statements isl, None }
 | isl = imp_stmts; ep = else_phrase; { Statements isl, Some ep }
 | NEXT; SENTENCE; %prec lowest       { NextSentence, None }
 | NEXT; SENTENCE; ep = else_phrase;  { NextSentence, Some ep }

let else_phrase :=
 | ELSE; NEXT; SENTENCE;  { NextSentence }
 | ELSE; isl = imp_stmts; { Statements isl }


(* INITIALIZE STATEMENT (+COB85) *)

%public let unconditional_action := ~ = initialize_statement; < >
let initialize_statement :=
 | INITIALIZE; il = idents;
   wf = ibo(io(WITH); FILLER);                                    (* +COB2002 *)
   co = io(category_to_value);                                    (* +COB2002 *)
   rl = ilo(then_replacing);
   d = ibo(THEN?; TO?; DEFAULT);
   { Initialize {
         init_items = il;
         init_filler = wf;
         init_category = co;
         init_replacings = rl;
         init_to_default = d; } }

let category_to_value :=
 | ALL; TO?; VALUE;          {InitAll}
 | ~ = init_data_category; TO?; VALUE; <InitCategory>

let then_replacing :=
 | THEN?; REPLACING;
   ~ = nel(c = init_data_category; DATA?; BY; il = ident_or_literal;
           { { init_replacing_category = c;
               init_replacing_replacement_item = il } }); < >

let init_data_category :=
 | ALPHABETIC;          {InitCategoryAlphabetic}
 | ALPHANUMERIC;        {InitCategoryAlphanumeric}
 | ALPHANUMERIC_EDITED; {InitCategoryAlphanumericEdited}
 | BOOLEAN;             {InitCategoryBoolean}                     (* +COB2002 *)
 | DATA_POINTER;        {InitCategoryDataPointer}                 (* +COB2002 *)
 | FUNCTION_POINTER;    {InitCategoryFunctionPointer}
 | NATIONAL;            {InitCategoryNational}                    (* +COB2002 *)
 | NATIONAL_EDITED;     {InitCategoryNationalEdited}              (* +COB2002 *)
 | NUMERIC;             {InitCategoryNumeric}
 | NUMERIC_EDITED;      {InitCategoryNumericEdited}
 | OBJECT_REFERENCE;    {InitCategoryObjectReference}             (* +COB2002 *)
 | PROGRAM_POINTER;     {InitCategoryProgramPointer}              (* +COB2002 *)



(* INITIATE STATEMENT (+COB85) *)

%public let unconditional_action := ~ = initiate_statement; < >
let initiate_statement :=
 | INITIATE; ~ = names; <Initiate>



(* INSPECT STATEMENT *)

%public let unconditional_action := ~ = inspect_statement; < >
let inspect_statement :=
 | INSPECT; i = ident; s = inspect_spec;
   { Inspect { inspect_item = i; inspect_spec = s } }

let inspect_spec :=
 | TALLYING; tpl = rnell(tallying);
   { InspectTallying tpl }
 | REPLACING; rpl = rnel(loc(replacing_phrase));
   { InspectReplacing rpl }
 | TALLYING; tpl = rnell(tallying);
   REPLACING; rpl = rnel(loc(replacing_phrase));
   { InspectBoth (tpl, rpl) }
 | CONVERTING;
   il1 = ident_or_nonnumeric_no_all; TO; il2 = ident_or_nonnumeric;
   abl = rl(inspect_where);
   { InspectConverting { converting_from = il1;
                         converting_to = il2;
                         converting_where = abl; } }

let tallying :=
  | i = qualident; FOR; tfl = rnel(loc(tallying_for));
    { { tallying_target = i; tallying_clauses = tfl } }

let tallying_for :=
 | CHARACTERS; l = rl(inspect_where); {TallyingCharacters l}
 | ALL; l = ident_after_before_list;        {TallyingRange (TallyAll, l)}
 | LEADING; l = ident_after_before_list;    {TallyingRange (TallyLeading, l)}

let ident_after_before_list :=
 | iab = ident_after_before; %prec below_RPAR                { [iab] }
 | iab = ident_after_before; iabl = ident_after_before_list; { iab::iabl }

let ident_after_before :=
 | il = ident_or_nonnumeric_no_all; ab = rl(inspect_where);
   { { tallying_item = il; tallying_where = ab } }

let inspect_where :=
 | AFTER;  INITIAL?; i = ident_or_nonnumeric_no_all; {InspectAfter, i}
 | BEFORE; INITIAL?; i = ident_or_nonnumeric_no_all; {InspectBefore, i}

let replacing_phrase :=
 | CHARACTERS; BY; il = ident_or_nonnumeric_no_all; abo = rl(inspect_where);
   { ReplacingCharacters { replacement = il; where = abo } }
 | ALL; l = rnel(ident_by_after_before);     {ReplacingRange (ReplaceAll, l)}
 | LEADING; l = rnel(ident_by_after_before); {ReplacingRange (ReplaceLeading, l)}
 | FIRST; l = rnel(ident_by_after_before);   {ReplacingRange (ReplaceFirst, l)}

let ident_by_after_before :=
 | il1 = ident_or_nonnumeric_no_all; BY;
   il2 = ident_or_nonnumeric_no_all;
   abo = rl(inspect_where);
   { {replacing_item = il1; replacing_by = il2; replacing_where = abo} }



(* INVOKE STATEMENT (+COB2002) *)

%public let unconditional_action := ~ = invoke_statement; < >
let invoke_statement :=
 | INVOKE; i = ident; is = ident_or_string;
   ul = lo(pf(USING,rnel(loc(using_by)))); ro = ro(returning);
   { Invoke { invoke_target = i;
              invoke_method = is;
              invoke_using = ul;
              invoke_returning = ro } }



(* MERGE STATEMENT *)

%public let unconditional_action := ~ = merge_statement; < >
let merge_statement :=
 | MERGE; i = name; okl = rnel(on_key); cso = ro(collating_sequence_phrase);
   USING; il = names; og = output_or_giving;
   { Merge { merge_file = i;
             merge_keys = okl;
             merge_collating = cso;
             merge_using = il;
             merge_target = og } }



(* MOVE STATEMENT *)

%public let unconditional_action := ~ = move_statement; < >
let move_statement :=
 | MOVE; from = ident_or_literal; TO; to_ = idents;
   { Move (MoveSimple { from; to_ }) }
 | MOVE; CORRESPONDING; from = ident; TO; to_ = idents;
   { Move (MoveCorresponding { from; to_ }) }



(* MULTIPLY STATEMENT *)

%public let unconditional_action := ~ = multiply_statement; <Multiply>
let multiply_statement :=
 | MULTIPLY; in_ = ident_or_numeric; BY; irl = rounded_idents;
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR);end_multiply;
   { { multiply_operands = MultiplyBy { multiplier = in_; multiplicand = irl };
       multiply_on_size_error = h; } }
 | MULTIPLY; in1 = ident_or_numeric; BY; in2 = ident_or_numeric;
   GIVING; irl = rounded_idents;
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR);end_multiply;
   { { multiply_operands = MultiplyGiving { multiplier = in1;
                                            multiplicand = in2;
                                            targets = irl; };
       multiply_on_size_error = h; } }

let end_multiply := oterm_(END_MULTIPLY)

(* OPEN STATEMENT (+COB85) *)
(* COB85 has may restrictions over COB2002 as to accepted syntax *)

%public let unconditional_action := ~ = open_statement; < >
let open_statement :=
 | OPEN; ~ = rnel(open_phrase); <Open>

let open_phrase :=
 | om = open_mode;
   so = ro(sharing_phrase);                                       (* +COB2002 *)
   ro = ro(retry_phrase);                                         (* +COB2002 *)
   frl = rnel(file_with_opt);
   { { open_mode = om; open_sharing = so; open_retry = ro; open_files = frl} }

let sharing_phrase [@context sharing_phrase] :=
 | SHARING; WITH?; ~ = sharing_mode; < >

let sharing_mode :=
 | ALL; OTHER?; {SharingAllOther}
 | NO; OTHER?;  {SharingNoOther}
 | READ; ONLY;  {SharingReadOnly}

let file_with_opt :=
 | i = name; rwo = reversed_or_no_rewind_opt;
    { { named_file_name = i; named_file_option = rwo } }

let reversed_or_no_rewind_opt :=
 |                                    {None}
 | REVERSED;                          {Some FileOptReversed} (* -COB2002 *)
 | mr(NO; REWIND | WITH; NO; REWIND); {Some FileOptWithNoRewind}



(* PERFORM STATEMENT *)
(* COB85 and COB2002 differ significantly *)
(* TODO: COB85 first format also takes instructions... *)

%public let unconditional_action := ~ = perform_statement; < >
let perform_statement :=
 | PERFORM; i = qualified_procedure_name;
   io = ro(pf(THROUGH,qualified_procedure_name));
   po = io(perform_phrase);
   { PerformTarget { perform_target = { procedure_start = i;
                                        procedure_end = io };
                     perform_mode = po } }
 | PERFORM; po = ro(perform_phrase); isl = imp_stmts; END_PERFORM;
   { PerformInline { perform_inline_mode = po;
                     perform_statements = isl } }

let perform_phrase :=
 | ~ = ident_or_integer; TIMES; <PerformNTimes>
 | wt = ro(with_test); UNTIL; until = condition;
   { PerformUntil { with_test = wt; until } }
 | wt = ro(with_test); VARYING; v = loc(varying_phrase);
   vl = l(pf(AFTER,loc(varying_phrase)));
   { PerformVarying { with_test = wt; varying = v; after = vl } }

let with_test := WITH?; TEST; ~ = after_or_before; < >

let varying_phrase :=
 | i = ident; FROM; in_ = ident_or_numeric;
   ino = ro(pf(BY,ident_or_numeric));
   UNTIL; c = condition;
   { { varying_ident = i; varying_from = in_;
       varying_by = ino; varying_until = c } }



(* PURGE STATEMENT (+COB85, -COB2002) *)

%public let unconditional_action := ~ = purge_statement; < >
let purge_statement :=
 | PURGE; ~ = name; <Purge>



(* RAISE STATEMENT (+COB2002) *)

%public let unconditional_action := ~ = raise_statement; <Raise>
let raise_statement :=
 | RAISE; EXCEPTION; i = name; { RaiseException i }
 | RAISE; i = ident;           { RaiseIdent i }



(* READ STATEMENT *)
(* Very ambiguous: sequential and random cases merged and disambiguated later *)
(* Also, COB2002 has more options. *)

%public let unconditional_action := ~ = read_statement; < >
let read_statement [@context read_stmt] :=
 | READ; i = name; npo = ro(read_direction); RECORD?;
   io = ro(pf(INTO,ident));
   lro = ro(lock_or_retry);                                       (* +COB2002 *)
   wlo = with_lock;                                               (* +COB2002 *)
   ko = ro(pf(KEY; IS?, qualname));
   ho = io(at_end_or_invalid_key_handler);
   oterm_(END_READ);
   { Read { read_file = i;
            read_direction = npo;
            read_into = io;
            read_lock_behavior = lro;
            read_lock = wlo;
            read_key = ko;
            read_error_handler = ho } }

let read_direction := NEXT; {ReadNext} | PREVIOUS; {ReadPrevious} (* +COB2002 *)

let lock_or_retry :=
 | ADVANCING; ON?; LOCK; {ReadAdvancingOnLock}
 | IGNORING; LOCK;       {ReadIgnoringLock}
 | ~ = retry_phrase;     <ReadRetry>



(* RECEIVE STATEMENT (+COB85, -COB2002) *)

%public let unconditional_action := ~ = receive_statement; < >
let receive_statement :=
 | RECEIVE; i1 = name; ms = message_or_segment; INTO; i2 = ident;
   nd = ilo(pf(NO_DATA,imp_stmts));
   wd = ilo(pf(with_data,imp_stmts));
   oterm_(END_RECEIVE);
   { Receive { receive_name = i1; receive_kind = ms; receive_into = i2;
               receive_on_no_data = { dual_handler_pos = nd;
                                      dual_handler_neg = wd } } }

let message_or_segment := MESSAGE; {MCSMessage} | SEGMENT; {MCSSegment}
let with_data := DATA | WITH_DATA



(* RELEASE STATEMENT *)

%public let unconditional_action := ~ = release_statement; < >
let release_statement :=
 | RELEASE; i = name; ilo = ro(pf(FROM,ident_or_literal));
   { Release { release_item = i; release_from = ilo } } (* Literal only in COB2002+ *)



(* RESUME STATEMENT (+COB2002) *)

%public let unconditional_action := ~ = resume_statement; < >
let resume_statement [@context resume_stmt] :=
 | RESUME; AT?; NEXT; STATEMENT;    { ResumeNextStatement }
 | RESUME; AT?; i = qualified_procedure_name; { Resume i }



(* RETURN STATEMENT *)

%public let unconditional_action := ~ = return_statement; < >
let return_statement :=
 | RETURN; i = name; RECORD?; io = ro(pf(INTO,loc(ident)));
   (*   ae = at_end nae = ilo(NOT nae = at_end { nae }) end_return*)
   isl1 = pf(at_end,imp_stmts);
   isl2 = ilo(pf(NOT_AT_END,imp_stmts)); oterm_(END_RETURN);
   { Return { return_file = i; return_into = io;
              return_at_end = { dual_handler_pos = isl1;
                                dual_handler_neg = isl2 } } }



(* REWRITE STATEMENT *)

%public let unconditional_action := ~ = rewrite_statement; < >
let rewrite_statement :=
 | REWRITE; if_ = write_target; RECORD?;                  (* RECORD: +COB2002 *)
   ilo = ro(pf(FROM,ident_or_literal));
   ro = ro(retry_phrase);                                         (* +COB2002 *)
   wl = with_lock;                                                (* +COB2002 *)
   h = handler_opt(INVALID_KEY,NOT_INVALID_KEY);
   oterm_(END_REWRITE);
   { Rewrite { rewrite_to = if_;
               rewrite_from = ilo;
               rewrite_retry = ro;
               rewrite_lock = wl;
               rewrite_invalid_key_handler = h } }



(* SEARCH STATEMENT *)
(* TODO: merge when_clause / statements_or_next *)

%public let unconditional_action := ~ = search_statement; < >
let search_statement :=
 | SEARCH; i = qualname;
   io = ro(pf(VARYING,ident));
   ae = ilo(pf(at_end,imp_stmts));
   wcl = nell(loc(when_clause));
   end_search;
   { Search { search_item = i;
              search_varying = io;
              search_at_end = ae;
              search_when_clauses = wcl } }
 | SEARCH; ALL; i = qualname;
   ae = ilo(pf(at_end,imp_stmts));
   WHEN; sc = search_condition; scl = ll(and_clause);
   sn = statements_or_next;
   end_search;
   { SearchAll { search_all_item = i;
                 search_all_at_end = ae;
                 search_all_conditions = sc :: scl;
                 search_all_action = sn } }

let end_search := oterm_(END_SEARCH)

let when_clause :=
 | WHEN; c = condition; s = statements_or_next;
   { { search_when_cond = c; search_when_stmts = s } }

let and_clause :=
 | AND; ~ = search_condition; < >

let search_condition :=
 | i = qualident; IS?; or_(EQUAL, "="); TO?; e = expression;
   { IsEqual { data_item = i; condition = e } }
 | i = qualident; {Cond i}

let statements_or_next ==
 | isl = imp_stmts; {Statements isl}
 | NEXT; SENTENCE;  {NextSentence}



(* SEND STATEMENT (+COB85, -COB2002) *)
(* Somewhat ambiguous *)

%public let unconditional_action := ~ = send_statement; < >
let send_statement :=
 | SEND; i1 = name; FROM; i2 = ident;
   { Send { send_name = i1;
            send_operands = SendSimple { from = i2 } } }
 | SEND; i = name; io = io(pf(FROM,ident)); WITH?; ei = ending_indicator;
   ao = ro(advancing_phrase); r = bo(REPLACING; LINE);
   { Send { send_name = i;
            send_operands = SendWith { from = io;
                                       ending_indicator = ei;
                                       advancing = ao;
                                       replace = r } } }

let ending_indicator :=
 | i = ident; {EndingIndicator i}
 | ESI;       {EndingIndicatorESI}
 | EMI;       {EndingIndicatorEMI}
 | EGI;       {EndingIndicatorEGI}

let advancing_phrase :=
 | stage = after_or_before; ADVANCING?; PAGE;
   { AdvancingPage { stage } }
 | stage = after_or_before; ADVANCING?; i = ident_or_integer;
   b = bo(or_(LINE,LINES));
   { AdvancingLines { stage; lines = i; ambiguous = not b } }



(* SET STATEMENT *)
(* Very ambiguous *)

%public let unconditional_action := ~ = set_statement; <Set>
let set_statement [@context set_stmt] :=
  (* Ambiguous cases (formats 1, 2, 5, 7, 8, 9, 10 and 14) *)
  | SET; i = ident; TO; la = locale_or_ambiguous;
    { match la with
        | `Locale ld ->
            SetSaveLocale { target = i; locale = ld }
        | `Expr e ->
            SetAmbiguous { targets = [i]; value = e;
                           set_method = SetMethodTo } }
 | SET; i = ident; il = idents; TO; e = expression;
   { SetAmbiguous { targets = i :: il; set_method = SetMethodTo; value = e } }
 | SET; il = idents; ud = up_down; e = expression;
   { SetAmbiguous { targets = il; set_method = ud; value = e } }
 (* Switch and Condition (formats 3 and 4) *)
 | SET; sl = rnell(i = ident; TO; oo = on_or_off;
                   { { set_switch_targets = [i]; set_switch_value = oo } });
   { SetSwitch sl }
 | SET; sl = rnell(i = ident; il = idents; TO; oo = on_or_off;
                   { { set_switch_targets = i :: il; set_switch_value = oo } });
   { SetSwitch sl }
 | SET; cl = rnell(i = ident; TO; tf = boollit;
                   { { set_condition_targets = [i]; set_condition_value = tf } });
   { SetCondition cl }
 | SET; cl = rnell(i = ident; il = idents; TO; tf = boollit;
                   { { set_condition_targets = i :: il; set_condition_value = tf } });
   { SetCondition cl }

 (* The following were added in COB2002 (formats 6, 11, 12, 13, 15) *)
 | SET; i = name; al = set_attribute_switches;
   { SetAttribute { name = i; attribute_switches = al } }
 | SET; LOCALE; ld = locale_or_default; TO; lv = locale_value_or_ident;
   { SetLocale { target = ld; source = lv } }
 | SET; LAST; EXCEPTION; TO; OFF;
   { SetSavedException }
 | SET; CONTENT; OF?; il = idents; TO; fc = float_content; so = ro(sign);
   { SetFloatContent { targets = il; content = fc; sign = so } }

let locale_or_ambiguous :=
  | LOCALE; ld = lc_all_or_default; { `Locale ld }
  | e = expression;                 { `Expr e }

let set_attribute_switches [@context set_attribute_stmt] :=
  | ATTRIBUTE; ~ = rnel(screen_attribute_on_off); < >

let screen_attribute_on_off :=
  | sa = screen_attribute_name; oo = on_or_off;
    { { set_attribute = sa; set_attribute_switch_value = oo} }

let up_down :=
 | UP; BY;   {SetMethodUp}
 | DOWN; BY; {SetMethodDown}

let on_or_off :=
 | ON;  {On}
 | OFF; {Off}

let boollit :=
 | TRUE;  {true}
 | FALSE; {false}

let screen_attribute_name :=
 | BELL;          {ScreenBell}
 | BLINK;         {ScreenBlink}
 | HIGHLIGHT;     {ScreenHighlight}
 | LOWLIGHT;      {ScreenLowlight}
 | REVERSE_VIDEO; {ScreenReverseVideo}
 | UNDERLINE;     {ScreenUnderline}

let locale_category :=
 | LC_ALL;      {LcAll}
 | LC_COLLATE;  {LcCollate}
 | LC_CTYPE;    {LcCtype}
 | LC_MESSAGES; {LcMessages}
 | LC_MONETARY; {LcMonetary}
 | LC_NUMERIC;  {LcNumeric}
 | LC_TIME;     {LcTime}

let locale_or_default :=
 | l = locale_category; {SetLocaleTarget l}
 | USER_DEFAULT;        {SetLocaleTargetUserDefault}

let locale_value_or_ident :=
 | i = ident;      {SetLocaleSource i}
 | USER_DEFAULT;   {SetLocaleSourceUserDefault}
 | SYSTEM_DEFAULT; {SetLocaleSourceSystemDefault}

let lc_all_or_default :=
 | LC_ALL;       {SetSaveLocaleLcAll}
 | USER_DEFAULT; {SetSaveLocaleUserDefault}

let float_content :=
 | FARTHEST_FROM_ZERO; ~ = bo(IN_ARITHMETIC_RANGE); <FarthestFromZero>
 | NEAREST_TO_ZERO; ~ = bo(IN_ARITHMETIC_RANGE);    <NearestToZero>
 | FLOAT_INFINITY;                                  {FloatInfinity}
 | FLOAT_NOT_A_NUMBER;                              {FloatNotANumber}
 | FLOAT_NOT_A_NUMBER_SIGNALING;                    {FloatNotANumberSignaling}

let sign :=
 | POSITIVE; {SgnPositive}
 | NEGATIVE; {SgnNegative}



(* SORT STATEMENT *)

%public let unconditional_action := ~ = sort_statement; < >
let sort_statement :=
 | SORT; i = qualident;
   wd = ibo(WITH?; DUPLICATES; IN?; ORDER?);
   cso = ro(collating_sequence_phrase);
   { Sort (SortTable { table = i;
                       keys = [];
                       duplicate_in_order = wd;
                       collating = cso }) }
 | SORT; i = qualident; okl = rnel(on_key);
   wd = ibo(WITH?; DUPLICATES; IN?; ORDER?);
   cso = ro(collating_sequence_phrase);
   { Sort (SortTable { table = i;
                       keys = okl;
                       duplicate_in_order = wd;
                       collating = cso }) }
 | SORT; i = qualident; okl = rnel(on_key);
   wd = ibo(WITH?; DUPLICATES; IN?; ORDER?);
   cso = ro(collating_sequence_phrase);
   iu = input_or_using;
   og = output_or_giving;
   { Sort (SortFile { file = i;
                      keys = okl;
                      duplicate_in_order = wd;
                      collating = cso;
                      source = iu;
                      target = og }) }
(* COB2002 also has an alternate more restricted form for tables *)

let input_or_using :=
 | INPUT; PROCEDURE; IS?; i = procedure_name;
   io = ro(pf(THROUGH,procedure_name));
   { SortInputProcedure { procedure_start = i; procedure_end = io } }
 | USING; names = names;
   { SortUsing names }



(* START STATEMENT *)

%public let unconditional_action := ~ = start_statement; < >
let start_statement :=
 | START; i = name; fpo = io(start_position);
   h = handler_opt(INVALID_KEY,NOT_INVALID_KEY); oterm_(END_START);
   { Start { start_file = i; start_position = fpo; start_on_invalid_key = h} }

let start_position ==
 | FIRST; { StartPositionFirst }
 | LAST;  { StartPositionLast }
 | KEY; ro = relop; i = qualname;
   wlo = io(pf(WITH?; LENGTH,expression)); (* arith *)
   { StartPositionKey { operator = ro; name = i; length = wlo } }



(* STOP STATEMENT *)

%public let unconditional_action := ~ = stop_statement; <Stop>
let stop_statement [@context stop_stmt] :=
  | STOP; RUN; so = ro(with_status); { StopRun so }       (* status: +COB2002 *)
  | STOP; l = literal;               { StopLiteral l }    (* ~COB85, -COB2002 *)

let with_status :=
  | WITH; stop_kind = stop_kind;
    STATUS?; stop_status = ident_or_literal; { { stop_kind; stop_status } }

let stop_kind :=
  | ERROR; {StopRunError}
  | NORMAL; {StopRunNormal}



(* STRING STATEMENT *)

%public let unconditional_action := ~ = string_statement; < >
let string_statement :=
 | STRING; ss = nell(source_string); INTO; i = ident;
   wp = io(pf(WITH?; POINTER,ident));
   h = handler_opt(on_overflow,NOT_ON_OVERFLOW); oterm_(END_STRING);
   { String { string_sources = ss; string_target = i;
              string_pointer = wp; string_on_overflow = h } }

let source_string :=
 | i = ident_or_nonnumeric_no_all; db = ro(s_delimited_by);
   { { string_source = i; string_delimiter = db} }

(* NOTE: This is not perfectly standard, but we can figure out the delimiter for each item
 * in later analysis *)
let s_delimited_by :=
 | DELIMITED; BY?; ~ = ident_or_nonnumeric_no_all; <StringDelimiter>
 | DELIMITED; BY?; SIZE;                           {StringDelimiterSize}



(* SUBTRACT STATEMENT *)

%public let unconditional_action := ~ = subtract_statement; < >
let subtract_statement :=
 | SUBTRACT; inl = idents_or_numerics; FROM; irl = rounded_idents;
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR); end_subtract;
   { Subtract { basic_arith_operands =
                  ArithSimple { sources = inl; targets = irl };
                basic_arith_on_size_error = h } }
 | SUBTRACT; inl = idents_or_numerics; FROM; in_ = ident_or_numeric;
   GIVING; irl = rounded_idents;
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR); end_subtract;
   { Subtract { basic_arith_operands =
                  ArithGiving { sources = inl;
                                to_or_from_item = in_;
                                targets = irl };
                basic_arith_on_size_error = h } }
 | SUBTRACT; CORRESPONDING; i = qualname; FROM; ir = rounded_ident;
   h = handler_opt(ON_SIZE_ERROR,NOT_ON_SIZE_ERROR); end_subtract;
   { Subtract { basic_arith_operands =
                  ArithCorresponding { source = i; target = ir };
                basic_arith_on_size_error = h } }
(* COB85: 1 ON SIZE ERROR, THEN 1 NOT ON SIZE ERROR
   COB2002: order irrelevant *)

let end_subtract := oterm_(END_SUBTRACT)



(* SUPPRESS STATEMENT (+COB85) *)
(* May only be used in USE BEFORE REPORTING *)

%public let unconditional_action := ~ = suppress_statement; < >
let suppress_statement :=
 | SUPPRESS; PRINTING?; {Suppress}



(* TERMINATE STATEMENT (+COB85) *)

%public let unconditional_action := ~ = terminate_statement; < >
let terminate_statement :=
 | TERMINATE; ~ = names; <Terminate>



(* TRANSFORM STATEMENT (GCOS7, MF (via IBM OS/VS), maybe others, and obviously
   GnuCOBOL) --- TODO: dialect option? *)

%public let unconditional_action := ~ = transform_statement; < >
let transform_statement :=
  | TRANSFORM; i = loc(ident); CHARACTERS?;
    FROM; from = loc(ident_or_nonnumeric_no_all);            (* CHECKME: ALL? *)
    TO; to_ = loc(ident_or_nonnumeric);                      (* ditto *)
    { Transform { transform_ident = i;
                  transform_from = from;
                  transform_to = to_ } }



(* UNLOCK STATEMENT (+COB2014 ?) *)

%public let unconditional_action := ~ = unlock_statement; < >
let unlock_statement :=
  | UNLOCK; i = name; ro = bo(or_(RECORD,RECORDS));
    { Unlock { unlock_file = i; unlock_record = ro } }



(* UNSTRING STATEMENT *)

%public let unconditional_action := ~ = unstring_statement; < >
let unstring_statement :=
 | UNSTRING; i = ident; dbo = unstring_delimiters;
   INTO; idl = rnel(unstring_target);
   wp = io(pf(WITH?; POINTER,ident));
   ti = io(pf(TALLYING; IN?,ident));
   h = handler_opt(on_overflow,NOT_ON_OVERFLOW); oterm_(END_UNSTRING);
   { Unstring { unstring_source = i;
                unstring_delimiters = dbo;
                unstring_targets = idl;
                unstring_pointer = wp;
                unstring_tallying = ti;
                unstring_on_overflow = h } }

let unstring_delimiters [@recovery []] [@symbol ""] :=
 | (* empty *) { [] }
 | DELIMITED; BY?; ao = bo(ALL); il = ident_or_string_no_all;
   aill = l(OR; ao = bo(ALL); il = ident_or_string_no_all;
            { { unstring_delimiter_by_all = ao; unstring_delimiter = il } });
   { { unstring_delimiter_by_all = ao; unstring_delimiter = il } :: aill }

let unstring_target :=
 | i = ident; i1o = ro(pf(DELIMITER; IN?,ident));
   i2o = ro(pf(COUNT; IN?,ident));
   { { unstring_target = i;
       unstring_target_delimiter = i1o;
       unstring_target_count = i2o } }



(* USE STATEMENT *)

(*
SEQUENTIAL/RELATIVE/INDEXED IO (when USE is present) (P 408, 446, 488)
  USE AFTER STANDARD {EXCEPTION/ERROR} PROCEDURE ON {fname/IN/OUT/EXT}...
IPC (P 550)
  USE GLOBAL? AFTER STANDARD {EXCEPTION/ERROR} PROCEDURE ON {fn/IN/OUT/EXT}...
  USE GLOBAL? BEFORE REPORTING ident
REPORT WRITER (P 640)
  USE AFTER STANDARD {EXCEPTION/ERROR} PROCEDURE ON {fn/OUT/EXT}...
  USE BEFORE REPORTING ident
DEBUGGING (P 692)
  USE FOR DEBUGGING ON {cn/fn/pn/[ALL REFS OF]id/ALL PROCS}...
*)

let use_statement :=
 | USE; g = ibo(GLOBAL); io(AFTER); io(STANDARD);
   or_(EXCEPTION, ERROR); PROCEDURE?; ON?; io = names_or_open_mode;
   { UseAfterFileException { global = g; trigger = io } }
 | USE; g = bo(GLOBAL); BEFORE; REPORTING; i = ident;
   { UseBeforeReporting { global = g; report_group = i } }
 | USE; FOR?; DEBUGGING; ON?; dt = rnel(debug_target);            (* -COB2002 *)
   { UseForDebugging dt }
 | USE; io(AFTER); exception_condition;
   efl = rnel(use_after_exception);                               (* +COB2002 *)
   { UseAfterIOException efl }
 | USE; io(AFTER); exception_object; i = name;                    (* +COB2002 *)
   { UseAfterExceptionObject i }

let names_or_open_mode :=
 | i = names;      { UseFileExceptionOnNames i }
 | om = open_mode; { UseFileExceptionOnOpenMode om }

let exception_condition == EXCEPTION; CONDITION | EC
let exception_object == EXCEPTION; OBJECT | EO

let use_after_exception :=
 | i = name; fl = rl(pf(FILE,name));
   { { use_after_exception = i; use_after_exception_on_files = fl } }

let debug_target :=
 | all = bo(ALL; REFERENCES?; OF?); procedure = qualified_procedure_name;
   { UseForDebuggingProcedure { all; procedure } }
 | ALL; PROCEDURES;
   { UseForDebuggingAllProcedures }



(* VALIDATE STATEMENT (+COB2002) *)

%public let unconditional_action := ~ = validate_statement; < >
let validate_statement :=
 | VALIDATE; ~ = idents; <Validate>



(* WRITE STATEMENT *)
(* Sequential and random cases are ambiguous -> merged *)
%public let unconditional_action := ~ = write_statement; < >
write_statement:
 | WRITE if_ = write_target
   fo = ro(pf(FROM,ident_or_literal))
   ao = ro(advancing_phrase)
   ro = ro(retry_phrase)
   wl = with_lock
   ho = io(end_of_page_or_invalid_key_handler)
   oterm_(END_WRITE)
   { Write { write_to = if_;
             write_from = fo;
             write_advancing = ao;
             write_retry = ro;
             write_lock = wl;
             write_error_handler = ho } }

let write_target :=
 | n = qualname;   {WriteTargetName n}
 | FILE; n = name; {WriteTargetFile n}

(* --- Standalone (for testing) --------------------------------------------- *)

standalone_condition: condition EOF { $1 };

(* -------------------------------------------------------------------------- *)

%%
