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

(*TODO: this type mix the keyword(ex. MOVE) and context(ex. COMPILATION_UNIT)*)
type context_kind =
  | BEGIN (*the default scope*)
  | COMPILATION_UNIT (*compilation_unit*)
  (* | PROGRAM| FUNCTION| CLASS| INTERFACE| FACTORY| OBJECT| METHOD *)
  | COPY_REPLACE
  | COPY
  | REPLACING_COPY
  | REPLACE
  | IDENT_DIV
  | ENV_DIV
  | SELECT (*in FILE-CONTROL*)

  | DATA_DIV
  | DATA_DESC
  (*TODO: DATA_DIV_CLAUSE is the token for all clause inside data division.
          define different tokens for different clauses this if need be*)
  | DATA_DIV_CLAUSE
  | FD | RD | SD
  | LEVEL of int
  (*procedure division*)
  | PROC_DIV
  | PROC_DIV_HEADER
  | DECLARATIVES
  | STATEMENT
  | SECTION
  | PARAGRAPH
  (******token of keyword in ProcDiv******)
  | ACCEPT | ADD | ALLOCATE | CALL | CANCEL | CLOSE | COMPUTE | CONTINUE
  | DELETE | DISPLAY | DIVIDE | EVALUATE | EXIT | FREE
  | GENERATE | GO | GOBACK | IF | INITIALIZE | INITIATE | INSPECT | INVOKE
  | MERGE | MOVE | MULTIPLY | OPEN
  | RAISE | READ | RELEASE | RESUME | RETURN | REWRITE
  | SEARCH | SET | SORT | START | STOP | STRING | SUBTRACT | SUPPRESS
  | TERMINATE | UNLOCK | UNSTRING | USE | VALIDATE | WRITE
  (*specil case:
    PERFORM_CLOSED can be implicitly terminated,
    but PERFORM not.
    ex.
      "PERFORM PARA1" is PERFORM_CLOSED, it does not have the END-PERFORM;
      "PERFORM 3 Times {nested-statement}" is PERFORM, it must be ended with END-PERFORM
    (*TODO: according to standard ISO_IEC_1989_2014, it could be terminated by "."
            but by test of gnucobol, it must be ended with a END-PERFORM...
            We suppose that the "." can terminate a PERFORM statement.*)
  *)
  | PERFORM | PERFORM_CLOSED
  (*Phrase/clause inside statement*)
  | ON_SIZE_ERROR | NOT_ON_SIZE_ERROR
  | AT_END | NOT_AT_END | SEARCH_AT_END
  | ON_EXCEPTION | NOT_ON_EXCEPTION
  | INVALID_KEY | NOT_INVALID_KEY
  | AT_END_OF_PAGE | NOT_AT_END_OF_PAGE
  | ON_OVERFLOW | NOT_ON_OVERFLOW
  | HELPTOKEN
  (*
    We only implement these phrases/clauses (which influence the indentation)
    Other phrases/clauses are considered as inline
  *)
  | AFTER (*INSPECT/PERFORM/WRITE*)
  | AT (*DISPLAY/ACCEPT*)
  | BEFORE (*INSPECT/PRFORM/WRITE*)
  | BY (*CALL/INVOKE/DIVIDE/INITIALIZE/INSPECT/MULTIPLY/PERFORM/SET/proc-div-header*)
  | CONVERTING (*INSPECT*)
  | ELSE (*IF*)
  | FROM (*ACCEPT/PERFORM/RELEASE/REWRITE/SUBTRACT/WRITE*)
  | GIVING (*ADD/SUBTRACT/MULTIPLY/DIVIDE/MERGE/SORT*)
  | INTO (*DIVIDE/READ/RETURN/STRING/UNSTRING*)
  | RAISING (*EXIT/GOBACK/proc-div-header*)
  | REPLACING (*INITIALIZE/INSPECT*)
  | SEQUENCE (*MERGE/SORT*) (*COLLATING SEQUENCE*)
  | TALLYING (*INSPECT/UNSTRING*)
  | THEN (*IF*)
  | TO (*ADD/INITIALIZE/INSPECT/MOVE/SET*)
  | UNTIL (*PERFORM*)
  | USING (*CALL/MERGE/INVOKE/SORT/proc-div-header*)
  | VARYING (*SEARCH/PERFORM*)
  | WHEN (*EVALUATE/SEARCH*)

  | DUMMY_EXCEPTION (*a help token to solve a bug*)
  (*Not a keyword, used for alignment of argument*)
  | ARGUMENT

  (*Statement of standard 1985.
    To avoid possible keyword conflict,
    the phrases inside these statements are not implemented *)
  | ALTER | DISABLE | ENABLE | PURGE | RECEIVE | SEND

(* data_context is used in the data division*)
type data_context =
  | PERIOD
  | Compiler_directive of context_kind
  | Entry of context_kind
  | No_keyword

(* proc_context is used in the procedure division*)
(*  When checking indentation of Procedur Division,
    All "combination of keywords" will be checked first by the pattern matching
    of string,
    i.e. sth like "ELSE IF", "ON SIZE ERROR" ...
    Then, we transform the string to token of proc_context (1 string -> 1 token)
*)
type proc_context =
  | PERIOD
  | Compiler_directive of context_kind
  | Open_scope of context_kind
  | Close_scope of context_kind
  | Phrase of context_kind
  (* We define Inline_phrase in order to terminate the phrase before it.*)
  | Inline_phrase
  (*  Other word which has no influence on the indentation is mapped to
      No_keyword or Other_keyword.                                      *)
  | No_keyword
  | Other_keyword

(*to record the indentation error

  lnum: the line number of the indentation error
  offset_orig: original offset
  offset_modif: correct offset

ex.
  _______________________
  |Para1.
  | move 3 to 4.
  |

  it comes an indenteur_record
  {lnum = 2 (line num of "move" in the whole file   i.e. pos_lnum) ;
   offset_orig = 1;
   offset_modif = 4 (set in user_define, offset of paragraph) }
*)
type indent_record =
  { lnum:int;
    offset_orig:int;
    offset_modif: int;
    src_format: Cobol_preproc.Src_format.any
      (** This is the source format for the change. Ideally, this information
      should not have to be recorded here, but could be obtained from the line
      number -- however, we don't have the infrastructure for that currently.
      *)
  }

type range = {start_line:int;
              end_line  :int  }

type context = (context_kind * int) list

type indent_state =
    {
      src_format: Cobol_preproc.Src_format.any;
      scope: context_kind; (*indicate where the current code is*)
      context: context;    (*the stack of (context_kind, offset)*)
      acc: indent_record list;
      range : range option;

      (*TODO:
           - Use a new type for `scope`
           - it may be strange to save range into the accumulator
             since range is decided at the begin and does not change any more.
             However, due to the limitation of the API "fold_text_lines",
             I find no good method to do it.
      *)
    }
