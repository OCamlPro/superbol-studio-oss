(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

%parameter <CONFIG: Cobol_config.Types.T>
%parameter <Overlay_manager: Src_overlay.MANAGER>
%{
  open Cobol_common.Srcloc.INFIX
  open Cobol_common.Diagnostics.TYPES
  module PP_UTILS = Preproc_utils.Make (CONFIG)
%}

(* Tokens are listed in `preproc_tokens.mly' *)

(* Entry points *)

%start <Preproc_directives.copy_statement
          Cobol_common.Diagnostics.with_diags
          Cobol_common.Srcloc.with_loc> copy_statement
%start <Preproc_directives.replace_statement
          Cobol_common.Diagnostics.with_diags
          Cobol_common.Srcloc.with_loc> replace_statement

%start <unit> _unused_symbols             (* <- used to supress some warnings *)

(* -------------------------------------------------------------------------- *)

%%

(* --------------------- DEDICATED UTILITIES -------------------------------- *)

let loc (X) ==
  | x = X; { x &@ Overlay_manager.join_limits $sloc }

(* --- COPY ----------------------------------------------------------------- *)

let copy_statement := ~ = loc (copy_statement_); EOL; < >
let copy_statement_ :=
  | COPY;
    l = copy_lib;
    sp = copy_suppress_printing;
    replacing = copy_replacings;
    ".";
    { let { result = replacing; diags }
        = PP_UTILS.filter_map_4_list_with_diags' replacing in
      { result = CDirCopy { library = l; suppress_printing = sp; replacing };
        diags } }

let copy_lib :=
  | l = fileloc; c = pf(or_(OF, IN),fileloc)?; { { libname = l; cbkname = c } }

let copy_suppress_printing :=
  | { false }
  | SUPPRESS; PRINTING?; { true }

let copy_replacings :=
  | { [] }
  | REPLACING; ~ = nell(loc(copy_replacing_clause)); < >

let copy_replacing_clause ==
  | repl_from = copy_replacing_text; BY;
    repl_to   = copy_replacing_text;
    { Text_processor.replacing repl_from repl_to }
  | repl_dir = leading_or_trailing;
    repl_from = loc(replacing_src); BY;
    repl_to = loc(replacing_dst);
    { PP_UTILS.replacing' ~repl_dir repl_from repl_to }

let replacing_src :=
  | ~ = PSEUDO_TEXT; <`PseudoText>
  | a = loc(ALPHANUM); {`Alphanum [ Text.alphanum_as_pseudoword a ]}

let replacing_dst :=
  | ~ = PSEUDO_TEXT; < >
  | a = loc(ALPHANUM); {[ Text.alphanum_as_pseudoword a ]}

let copy_replacing_text ==
  | ~ = loc(PSEUDO_TEXT); < >
  | ~ = loc(copy_replacing_text_identifier); < >

let copy_replacing_text_token ==
  | t = loc(ALPHANUM); { Text.pseudoword_of_alphanum t }
  | t = loc(TEXT_WORD);  { Text.pseudoword_of_string t }

let copy_replacing_text_identifier :=
  | c = copy_replacing_text_token;
    { [c] }
  | c = copy_replacing_text_identifier; _in = loc(IN); tok = copy_replacing_text_token;
    { c @ [Text.pseudoword_of_string ("IN" &@<- _in); tok] }
  | c = copy_replacing_text_identifier; _of = loc(OF); tok = copy_replacing_text_token;
    { c @ [Text.pseudoword_of_string ("OF" &@<- _of); tok] }
  | c = copy_replacing_text_identifier;
    _lpar = loc("("); cl = copy_replacing_text_token+; _rpar = loc(")");
    { let lpar = Text.pseudoword_of_string ("(" &@<- _lpar)
      and rpar = Text.pseudoword_of_string (")" &@<- _rpar) in
      c @ [lpar] @ cl @ [rpar] }

let leading_or_trailing ==
  | LEADING;  { Preproc_directives.Leading }
  | TRAILING; { Preproc_directives.Trailing }

(* --- REPLACE -------------------------------------------------------------- *)

let replace_statement := ~ = loc(replace_statement_); EOL; < >
let replace_statement_ :=
  | REPLACE; also = ibo(ALSO); replacing = nell(loc(copy_replacing_clause)); ".";
    { let { result = replacing; diags }
        = PP_UTILS.filter_map_4_list_with_diags' replacing in
      { result = CDirReplace { also; replacing }; diags } }
  | REPLACE; last = ibo(LAST); OFF; ".";
    { Cobol_common.Diagnostics.simple_result @@
        Preproc_directives.CDirReplaceOff { last } }

(* ISO/IEC 1989:2014 only allows the following clauses in "REPLACE"; however we
   allow the same clauses as GnuCOBOL. *)
(* let replace_clause := *)
(*   | repl_dir = leading_or_trailing?; *)
(*     repl_from = loc(replacing_src); BY; *)
(*     repl_to   = loc(replacing_dst); *)
(*     { replacing' ?repl_dir repl_from repl_to } *)

let text_word ==                                    (* text-word with position *)
  | ~ = loc(TEXT_WORD); < >

let fileloc :=
  | t = text_word; { (`Word, ~&t) &@<- t }
  | a = loc(ALPHANUM); { (`Alphanum, fst ~&a) &@<- a }

(* --- Misc ----------------------------------------------------------------- *)

_unused_symbols:
  | BOOLIT
  | NATLIT
  | HEXLIT
  | NULLIT
  | ALPHANUM_PREFIX
{ () }

%%
