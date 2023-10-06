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

(* open Cobol_common.Basics *)

let keywords_all = fst @@ List.split Cobol_parser.Keywords.keywords


(* TODO: Too many keywords, hard to classification *)

(* let keywords_proc =
  let keywords = [
    "ACCEPT"; "ADD";
    "CALL"; "COMPUTE";
    "DELETE"; "DISPLAY"; "DIVIDE";
    "EVALUATE";
    "IF";
    "MULTIPLY";
    "PERFORM";
    "READ"; "RETURN"; "REWRITE";
    "SEARCH"; "START"; "STRING"; "SUBTRACT";
    "UNSTRING";
    "WRITE";
  ] in
  (List.map (fun x -> "END-"^x) keywords) @ keywords

  @ [
    "ALLOCATE";
    "CANCEL"; "CLOSE"; "CONTINUE";
    "EXIT";
    "FREE";
    "GO"; "GOBACK"; "GENERATE";
    "INITIALIZE"; "INITIATE"; "INSPECT"; "INVOKE";
    "MERGE"; "MOVE";
    "OPEN";
    "RAISE"; "RELEASE"; "RESUME";
    "SET"; "SORT"; "STOP"; "SUPPRESS";
    "TERMINATE";
    "UNLOCK"; "USE";
    "VALIDATE";

    "ON SIZE ERROR"; "NOT ON SIZE ERROR";
    "ON EXCEPTION"; "NOT ON EXCEPTION";
    "AT END"; "NOT AT END";
    "INVALID KEY"; "NOT INVALID KEY";
    "AT END OF PAGE"; "NOT AT END OF PAGE";
    "AT EOP"; "NOT AT EOP";
    "ON OVERFLOW"; "NOT ON OVERFLOW";
    "SIZE"; "ERROR"; "INVALIDE"; "KEY"; "EXCEPTION"; "EOP"; "END"; "PAGE"; "OVERFLOW";

    "SECTION"; "FUNCTION"; "DIRECTIVE";

    "WHEN"; "ELSE";

    "WITH"; "IN"; "INTO"; "FROM"; "TO"; "NOT"; "ON"; "BY"; "AT"; "OF";
    "AND"; "ALSO"; "OR"; "IS"; "EQUAL";

  ]

(*TODO: (If need)
        If the partial parsing could give more information
        like in which statement the position is(or even better, in which clause/phrase),
        Then we can remove the keywords that cannot appear in this statement from
        the keyword list.
*)

(* let keywords_accept = [
  "ACCEPT"; "FROM"; "LINE NUMBER"; "COL NUMBER"; "COLUMN NUMBER";
  "DATE"; "DAY"; "DAY-OF-WEEK"; "TIME"; "YYYYMMDD"; "YYYYDDD";
  "AT"; "ON EXCEPTION"; "NOT ON EXCEPTION"; "EXCEPTION"
  ] *)

let keywords_proc_other = [

  (*ACCEPT*)
  "DATE"; "DAY"; "DAY-OF-WEEK"; "TIME"; "YYYYMMDD"; "YYYYDDD";
  "LINE"; "NUMBER"; "COL"; "COLUMN"; "CHARACTERS";
  "LINE NUMBER"; "COL NUMBER"; "COLUMN NUMBER";

  (*ALLOCATE*)
  "INITIALIZED"; "RETURNING"; "CHARACTERS";

  (*CALL*)
  "USING BY REFERENCE"; "USING BY CONTENT";
  (* "RETURNING";(*remove duplicate word*) *)
  "BY REFERENCE"; "BY CONTENT"; "REFERENCE"; "CONTENT"; "USING";

  (*CLOSE*)
  "REEL"; "UNIT";
  "FOR"; "FOR REMOVAL"; "REMOVAL";
  "WITH NO REWIND";
  "WITH LOCK";

  (*DELETE*)
  "RECORD";

  (*DISPLAY*)
  "UPON";
  (* "LINE"; "NUMBER"; "COL"; "COLUMN"; "CHARACTERS";
  "LINE NUMBER"; "COL NUMBER"; "COLUMN NUMBER"; *)
  "WITH NO ADVANCING"; "ADVANCING";

  (*DIVIDE*)
  (* "INTO"; "BY" *)
  "GIVING";

  (*EVALUATE*)
  "ALSO"; "OTHER";
  "TRUE"; "FALSE"; "ANY";
  "THROUGH"; "THRU";

  (*EXIT*)
  "PROGRAM"; "METHOD"; "PARAGRAPH"; "CYCLE";
  "LAST EXCEPTION"; "RAISING";

  (*GO*)
  "DEPENDING ON"; "DEPENDING";

  (*GOBACK*)
   (* "RASING"; "EXCEPTION";
   "LAST EXCEPTION"; *)

  (*IF*)
  "NEXT SENTENCE"; "NEXT"; "SENTENCE"; "THEN";

  (*INITIALIZE*)
  "WITH FILLER"; "ALL"; "VALUE";
  "THEN REPLACING"; "DATA"; "THEN TO DEFAULT";
  "ALPHABETIC"; "ALPHANUMERIC"; "ALPHANUMERIC-EDITED";
  "BOOLEAN"; "DATA-POINTER"; "FUNCTION-POINTER";
  "NATIONAL"; "NATIONAL-EDITED"; "NUMERIC";
  "NUMERIC-EDITED"; "OBJECT-REFERENCE"; "PROGRAM-POINTER";

  (*INSPECT*)
  "TALLYING"; "REPLACING"; "CONVERTING";
  "LEADING"; "BEFORE"; "AFTER"; "INITIAL"; "BEFORE INITIAL"; "AFTER INITIAL";
  "OMITTED"; "CHARACTERS"; "FIRST";

  (*INVOKE*)
  "USING BY REFERENCE";
  "USING BY CONTENT";
  "USING BY VALUE";

  (*MERGE*)
  "ON ASCENDING KEY"; "ASCENDING KEY"; "ASCENDING"; "KEY";
  "ON DESCENDING KEY"; "DESCENDING KEY"; "DESCENDING";
  "COLLATING SEQUENCE";
  "OUTPUT PROCEDURE";
  "PROCEDURE";

  (*MOVE*)
  "CORR"; "CORRESPONDING";

  (*OPEN*)
  "INPUT"; "OUTPUT"; "I-O"; "EXTEND";
  "SHARING WITH";
  "ALL OTHER";
  "NO OTHER";
  "READ ONLY";

  (*PERFORM*)
  "WITH TEST"; "UNTIL"; "VARYING";

  (*READ*)
  "PREVIOUS"; "PREVIOUS RECORD";
  "ADVANCING ON LOCK";
  (*sharing-phrase*)
  "IGNORING LOCK";
  "WITH NO LOCK";

  (*RESUME*)
  "NEXT STATEMENT"; "STATEMENT";

  (*SET*)
  "UP BY"; "DOWN BY"; "OFF"; "ATTRIBUTE";
  "ADDRESS"; "ADDRESS OF";
  "BELL"; "BLINK"; "HIGHLIGHT"; "LOWLIGHT"; "REVERSE-VIDEO"; "UNDERLINE";
  "LOCALE"; "LC_ALL"; "LC_COLLATE"; "LC_CTYPE"; "LC_MESSAGES"; "LC_MONETARY";
  "LC_NUMERIC";"LC_TIME"; "USER-DEFAULT"; "SYSTEM-DEFAULT";
  "CONTENT OF"; "FARTHEST-FROM-ZERO"; "IN-ARITHMETIC-RANGE";
  "FLOAT-INFINITY"; "FLOAT-NOT-A-NUMBER";
  "FLOAT-NOT-A-NUMBER-SIGNALING"; "NEAREST-TO-ZERO";
  "IN-ARITHMETIC-RANGE"; "NEGATIVE"; "POSITIVE"; "SIGN";

  (*SORT*)
  "WITH DUPLICATES IN ORDER";
  "INPUT PROCEDURE IS";
  "OUTPUT PROCEDURE IS";

  (*START*)
  "WITH LENGTH";
  "FIRST";
  "LAST";

  (*STOP*)
  "WITH ERROR STATUS";
  "WITH NORMAL STATUS";
  "STOP RUN";

  (*STRING*)
  "DELIMITED BY";

  (*SUPPRESS*)
  "SUPPRESS PRINTING";

  (*UNLOCK*)
  "RECORDS";

  (*UNSTRING*)
  "DELIMITER IN"; "COUNT IN";
  "TALLYING IN";

  (*USE*)
  "GLOBAL";
  "AFTER STANDARD";
  "BEFORE REPORTING";
  "EC"; "EXCEPTION CONDITION";
  "EO"; "EXCEPTION OBJECT";
  "AFTER STANDARD";

  (*WRITE*)
  "BEFORE ADVANCING";
  "AFTER ADVANCING";
  "ADVANCING";
  "LINES";
  "FILE";
]


let keywords_data = [
    "DATA"; "DIVISION"; "SECTION";
    "WORKING-STORAGE SECTION"; "FILE SECTION"; "REPORT SECTION"; "LOCAL-STORAGE SECTION";
    "LINKAGE SECTION"; "SCREEN SECTION"; "SD";

    "FD"; "IS"; "EXTERNAL"; "GLOBAL"; "AS"; "FORMAT"; "BIT"; "CHARACTER"; "NUMERIC";
    "BLOCK"; "CONTAINS"; "TO"; "CHARACTERS"; "RECORDS"; "CODE-SET"; "FOR"; "ALPHANUMERIC";
    "NATIONAL"; "REPORT"; "REPORTS"; "ARE";

    "CONSTANT";

    "BYTE-LENGTH"; "LENGTH"; "FROM";

    "RD"; "CODE"; "CONTROL"; "CONTROLS"; "FINAL"; "PAGE"; "LIMIT"; "LIMITS";
    "LINE"; "LINES"; "COLS"; "COLUMNS"; "HEADING"; "FIRST"; "DE"; "DETAIL";
    "LAST"; "CH"; "CONTROL HEADING"; "FOOTING";

    "USAGE"; "DISPLAY"; "NATIONAL"; "BLANK"; "WHEN"; "PRESENT"; "GROUP"; "OCCURS";
    "DEPENDING ON"; "DEPENDING"; "ON"; "STEP";

    "REDEFINES"; "TYPEDEF"; "STRONG"; "ALIGNED"; "ANY LENGTH"; "ANY"; "LENGTH"; "BASED";
    "BLANK WHEN ZERO"; "BLANK"; "ZERO"; "RECORD"; "DYNAMIC";
    "GROUP-USAGE"; "GROUP-USAGE IS BIT"; "GROUP-USAGE IS NATIONAL";
    "JUST"; "JUSTIFIED"; "RIGHT"; "JUST RIGHT"; "JUSTIFIED RIGHT";
    "PROPERTY"; "WITH"; "NO"; "SET"; "GET"; "FINAL";
    "SAME AS"; "SIGN"; "LEADING"; "TRAILING"; "SEPARATE"; "SEPARATE CHARACTER";
    "SYNCHRONIZED"; "SYNC"; "LEFT"; "RIGHT";
    "TYPE"; "DESTINATION";
    "INVALID WHEN"; "INVALID";
    "PRESENT WHEN"; "PRESENT";
    "VARYING"; "FROM"; "BY";
    "RENAMES"; "THROUGH"; "THRU";

    "PLUS"; "MINUS"; "BLANK SCREEN"; "FULL"; "AUTO"; "SECURE"; "REQUIRED"; "TIMES";
    "USAGE IS DISPLAY"; "USAGE IS NATIONAL";
    "BLANK LINE"; "BLANK SCREEN";
    "ERASE"; "END OF LINE"; "END OF SCREEN"; "EOL"; "EOS";
    "BELL"; "HIGHLIGHT"; "LOWLIGHT"; "RESERVED-VIDEO"; "UNDERLINE";
    "FOREGROUND-COLOR"; "BACKGROUND-COLOR";
    "USING"; "VALUE";

    "CLASS"; "ALPHABETIC"; "ALPHABETIC-LOWER"; "ALPHABETIC-UPPER"; "BOOLEAN";

    "CODE";

    "LEFT"; "CENTER"; "RIGHT";

    "DEFAULT"; "NONE";

    "FILLER";

    "FORMAT BIT DATA"; "FORMAT CHARACTER DATA"; "FORMAT NUMERIC DATA";

    "INDICATE";

    "LINAGE"; "WITH FOOTING AT"; "LINES AT TOP"; "TOP";
    "LINES AT BOTTOM"; "BOTTOM";

    "ON NEXT PAGE";

    "ASCENDING"; "ASCENDING KEY IS"; "DESCENDING"; "DESCENDING KEY IS";
    "INDEXED"; "INDEXED BY"; "STEP";

    "CAPACITY"; "INITIALIZED";

    "PIC"; "PICTURE"; "LOCALE"; "SIZE"; "A"; "N"; "X"; "Z"; "1"; "*"; "9"; "+"; "-";
    "CR"; "DB"; "E"; "S"; "V"; "."; "P";

    "OTHER";

    "SOURCE"; "SUM"; "UPON"; "RESET";

    "ON"; "FOR"; "REPORT HEADING"; "RH"; "PAGE HEADING"; "PH"; "CONTROL HEADING"; "CH";
    "CONTROL FOOTING"; "CF"; "PAGE FOOTING"; "PF"; "REPORT FOOTING"; "RF";

    "BINARY"; "BINARY-CHAR"; "BINARY-SHORT"; "BINARY-LONG"; "BINARY-DOUBLE";
    "COMPUTATIONAL"; "COMP"; "FLOAT-BINARY-32"; "FLOAT-BINARY-64";
    "FLOAT-BINARY-128"; "FLOAT-DECIMAL-16"; "FLOAT-DECIMAL-34"; "FLOAT-EXTENDED";
    "FLOAT-LONG"; "FLOAT-SHORT"; "INDEX"; "OBJECT REFERENCE"; "FACTORY";
    "ACTIVE-CLASS"; "ONLY"; "PACKED-DECIMAL"; "POINTER"; "FUNCTION-POINTER";
    "PROGRAM-POINTER"; "BINARY-ENCODING"; "DECIMAL-ENCODING";
    "HIGH-ORDER-LEFT"; "HIGH-ORDER-RIGHT";
    "VALIDATE-STATUS"; "VAL-STATUS"; "ERROR"; "CONTENT"; "RELATION";
    "VALUES"
  ];;

let keywords_data = StringSet.elements @@ StringSet.of_list keywords_data
let keywords_proc = StringSet.elements @@ StringSet.of_list (keywords_proc @ keywords_proc_other)
 *)
