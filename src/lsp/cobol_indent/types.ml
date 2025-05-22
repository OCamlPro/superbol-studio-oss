(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

type unparsed_config = ( string * int ) list

type indent_record = {
  lnum :         int;
  offset_orig :  int;
  mutable offset_modif : int;
}

type edit_space_operation = {
  line : int ;
  char : int ; (* relative to begin-of-line, before change application *)
  spaces : int ; (* positive for addition, negative for deletion *)
}

type range = {
  start_line : int ;
  end_line   : int ;
}

type source_format = {
  name : string ;
  free : bool  ;      (* has column identifier *)
  skip_before : int ;      (* fixed = 6 *)
  max_text_length : int ;  (* fixed = 8..72 = 65 chars *)
}

type config = {

  (* the scanner currently loses some tokens to improve indentation,
     for example the name after PROGRAM-ID if it is on the next
     line. Use this flag to prevent this behavior.

     Also, Scanner.skipped_revlines is only filled when this option is
     set to false.
*)
  scan_for_indent : bool ;
  verbosity : int ; (* global verbosity *)

  source_format : source_format ;
  arg_offset : int ;
  inner_offset : int ;
  data_item_offset : int option ;
  select_in_area_a : bool ;

  whole_file_indent : int ; (* whole file has an offset, usually 1 ! *)
}

type edit = { bol : bool ; edit : indent_record }

type token_descr = {
  tok_indent : int ;
  tok_length : int ;
  tok_edit : edit ;
}

type token =
  | EOF
  | LEXING_ERROR of string
  | IDENT of string
  | CHARS of string
  | INTEGER of string
  | NUMBER of string
  | COMMENT of string
  | DIRECTIVE of string
  | INFORMATION of string

  | DOT
  | LPAREN
  | RPAREN
  | EQUALEQUAL
  | COMMA
  | SEMI
  | COLON
  | EQUAL
  | MINUS
  | PLUS
  | GT | GTE
  | LT | LTE
  | DIV
  | MUL
  | DOLLAR
  | AMPER
  | SHARP

  (* keywords *)
  | ACCEPT   | END_ACCEPT
  | ADD      | END_ADD
  | ALLOCATE
  | ALPHABET
  | AT
  | CALL     | END_CALL
  | CANCEL
  | CD
  | CLASS
  | CLOSE
  | COMMIT
  | COMPUTE  | END_COMPUTE
  | CONFIGURATION
  | CONTINUE
  | CONTROL
  | COPY
  | DATA
  | DECIMAL_POINT
  | DECLARATIVES
  | DEFAULT
  | DELETE   | END_DELETE
  | DELEGATE
  | DISPLAY  | END_DISPLAY
  | DIVIDE   | END_DIVIDE
  | DIVISION
  | ELSE
  | END
  | END_EXEC
  | END_OF_PAGE
  | ENTRY
  | ENVIRONMENT
  | ERROR
  | EVALUATE | END_EVALUATE
  | EXCEPTION
  | EXEC
  | EXIT
  | EXTERNAL
  | FD
  | FILE
  | FILE_CONTROL
  | FINALLY
  | FREE
  | FUNCTION
  | FUNCTION_ID
  | GENERATE
  | GO
  | GOBACK
  | IDENTIFICATION
  | IF       | END_IF
  | INITIALIZE
  | INITIATE
  | INPUT
  | INPUT_OUTPUT
  | INSPECT
  | INVALID
  | INVOKE
  | I_O
  | I_O_CONTROL
  | JSON     | END_JSON
  | LINKAGE
  | MERGE
  | MODIFY   | END_MODIFY
  | MOVE
  | MULTIPLY | END_MULTIPLY
  | NO
  | NOT
  | OBJECT_COMPUTER
  | ON
  | OPEN
  | OPTIONAL
  | OUTPUT
  | OVERFLOW
  | PERFORM  | END_PERFORM
  | PERFORM_PAR
  | PROCEDURE
  | PROGRAM
  | PROGRAM_ID
  | RAISE
  | RD
  | READ     | END_READ
  | RECEIVE  | END_RECEIVE
  | RELEASE
  | REPLACE
  | REPLACING
  | REPORT
  | REPOSITORY
  | RESUME
  | RETURN   | END_RETURN
  | REWRITE  | END_REWRITE
  | ROLLBACK
  | RUN
  | SD
  | SEARCH   | END_SEARCH
  | SECTION
  | SELECT
  | SEND     | END_SEND
  | SET
  | SIZE
  | SORT
  | SOURCE_COMPUTER
  | SPECIAL_NAMES
  | START    | END_START
  | STOP
  | STRING   | END_STRING
  | SUBTRACT | END_SUBTRACT
  | SUPPRESS
  | TERMINATE
  | TEST
  | THEN
  | TIMES
  | TO
  | UNLOCK
  | UNSTRING | END_UNSTRING
  | UNTIL
  | USE
  | VALIDATE
  | VARYING
  | WHEN
  | WITH
  | WORKING_STORAGE
  | WRITE    | END_WRITE
  | XML      | END_XML



let keywords = [
  "ACCEPT", ACCEPT ;      "END-ACCEPT", END_ACCEPT ;
  "ADD", ADD ;            "END-ADD", END_ADD ;
  "ALLOCATE", ALLOCATE ;
  "ALPHABET", ALPHABET ;
  "AT", AT ;
  "CALL", CALL ;          "END-CALL", END_CALL ;
  "CANCEL", CANCEL ;
  "CD", CD ;
  "CLASS", CLASS ;
  "CLOSE", CLOSE ;
  "COMMIT", COMMIT ;
  "COMPUTE", COMPUTE ;    "END-COMPUTE", END_COMPUTE ;
  "CONFIGURATION", CONFIGURATION ;
  "CONTINUE", CONTINUE ;
  "CONTROL", CONTROL ;
  "COPY", COPY ;
  "DATA", DATA ;
  "DECIMAL-POINT", DECIMAL_POINT ;
  "DECLARATIVES", DECLARATIVES ;
  "DEFAULT", DEFAULT ;
  "DELEGATE", DELEGATE ;
  "DELETE", DELETE ;      "END-DELETE", END_DELETE ;
  "DISPLAY", DISPLAY ;    "END-DISPLAY", END_DISPLAY ;
  "DIVIDE", DIVIDE ;      "END-DIVIDE", END_DIVIDE ;
  "DIVISION", DIVISION ;
  "ELSE", ELSE ;
  "EOP", END_OF_PAGE ;
  "END", END ;
  "END-EXEC", END_EXEC ;
  "END-OF-PAGE", END_OF_PAGE ;
  "ENTRY", ENTRY ;
  "ENVIRONMENT", ENVIRONMENT ;
  "ERROR", ERROR ;
  "EVALUATE", EVALUATE ;  "END-EVALUATE", END_EVALUATE ;
  "EXCEPTION", EXCEPTION ;
  "EXEC", EXEC ;
  "EXIT", EXIT ;
  "EXTERNAL", EXTERNAL ;
  "FD", FD ;
  "FILE", FILE ;
  "FILE-CONTROL", FILE_CONTROL ;
  "FINALLY", FINALLY ;
  "FREE", FREE ;
  "FUNCTION", FUNCTION ;
  "FUNCTION-ID", FUNCTION_ID ;
  "GENERATE", GENERATE ;
  "GO", GO ;
  "GOBACK", GOBACK ;
  "IDENTIFICATION", IDENTIFICATION ;
  "IF", IF ;              "END-IF", END_IF ;
  "INITIALIZE", INITIALIZE ;
  "INITIATE", INITIATE ;
  "INPUT", INPUT ;
  "INPUT-OUTPUT", INPUT_OUTPUT ;
  "INSPECT", INSPECT ;
  "INVALID", INVALID ;
  "INVOKE", INVOKE ;
  "I-O", I_O ;
  "I-O-CONTROL", I_O_CONTROL ;
  "JSON", JSON ;          "END-JSON", END_JSON ;
  "LINKAGE", LINKAGE ;
  "MERGE", MERGE ;
  "MODIFY", MODIFY ;      "END-MODIFY", END_MODIFY ;
  "MOVE", MOVE ;
  "MULTIPLY", MULTIPLY ;  "END-MULTIPLY", END_MULTIPLY ;
  "NO", NO ;
  "NOT", NOT ;
  "OBJECT-COMPUTER", OBJECT_COMPUTER ;
  "ON", ON ;
  "OPEN", OPEN ;
  "OPTIONAL", OPTIONAL ;
  "OUTPUT", OUTPUT ;
  "OVERFLOW", OVERFLOW ;
  "PERFORM", PERFORM ;    "END-PERFORM", END_PERFORM ;
  "PROCEDURE", PROCEDURE ;
  "PROGRAM", PROGRAM ;
  "PROGRAM-ID", PROGRAM_ID ;
  "RAISE", RAISE ;
  "RD", RD ;
  "READ", READ ;          "END-READ", END_READ ;
  "RECEIVE", RECEIVE ;    "END-RECEIVE", END_RECEIVE ;
  "RELEASE", RELEASE ;
  "REPLACING", REPLACING ;
  "REPLACE", REPLACE ;
  "REPORT", REPORT ;
  "REPOSITORY", REPOSITORY ;
  "RESUME", RESUME ;
  "RETURN", RETURN ;      "END-RETURN", END_RETURN ;
  "REWRITE", REWRITE ;    "END-REWRITE", END_REWRITE ;
  "ROLLBACK", ROLLBACK ;
  "RUN", RUN ;
  "SD", SD ;
  "SEARCH", SEARCH ;      "END-SEARCH", END_SEARCH ;
  "SECTION", SECTION ;
  "SELECT", SELECT ;
  "SEND", SEND ;          "END-SEND", END_SEND ;
  "SET" , SET ;
  "SIZE", SIZE ;
  "SPECIAL-NAMES", SPECIAL_NAMES ;
  "SORT", SORT ;
  "SOURCE-COMPUTER", SOURCE_COMPUTER ;
  "START", START ;        "END-START", END_START ;
  "STOP", STOP ;
  "STRING", STRING ;      "END-STRING", END_STRING ;
  "SUBTRACT", SUBTRACT ;  "END-SUBTRACT", END_SUBTRACT ;
  "SUPPRESS", SUPPRESS ;
  "TERMINATE", TERMINATE ;
  "TEST", TEST ;
  "THEN", THEN ;
  "TIMES", TIMES ;
  "TO", TO ;
  "UNLOCK", UNLOCK ;
  "UNSTRING", UNSTRING ;  "END-UNSTRING", END_UNSTRING ;
  "UNTIL", UNTIL ;
  "USE", USE ;
  "VALIDATE", VALIDATE ;
  "VARYING", VARYING ;
  "WHEN", WHEN ;
  "WITH", WITH ;
  "WORKING-STORAGE", WORKING_STORAGE ;
  "WRITE", WRITE ;        "END-WRITE", END_WRITE ;
  "XML", XML ;            "END-XML", END_XML ;
]
