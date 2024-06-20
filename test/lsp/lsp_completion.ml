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

open EzCompat                                                    (* StringMap *)
open Lsp.Types
open Lsp_testing

let complete_positions (doc, positions) : string -> unit =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  (* let doc = LSP.Server.find_document prog server in *)
  let location_as_srcloc = new srcloc_resuscitator_cache in
  let completions_at_position ?key (position: Position.t) =
    let location =
      let range = Range.create ~start:position ~end_:position in
      Location.create ~range ~uri:prog.uri
    in
    let params = CompletionParams.create ~position ~textDocument:prog () in
    Pretty.out "%a%a(line %d, character %d):\n"
      location_as_srcloc#pp location
      Fmt.(option ~none:nop (string ++ sp)) key
      position.line position.character;
    match LSP.Request.complete server params with
    | None ->
        Pretty.out "Failed completion@."
    | Some `CompletionList { items; _ } when items == [] ->
        Pretty.out "Empty completion list@."
    | Some `CompletionList { items; _ } ->
        let pp_comp_item ppf (item: CompletionItem.t) =
          Fmt.string ppf item.label in
        Pretty.out "List of completions (%d entries): [%a]\n" (List.length items) (Fmt.list ~sep:(Fmt.any ";") pp_comp_item) items;

  in
  StringMap.iter (fun n p -> completions_at_position ~key:n p) positions.pos_map;
  List.iter (fun p -> completions_at_position p) positions.pos_anonymous;
  end_with_postproc
;;

let%expect_test "division-and-section-completion" =
  let end_with_postproc = complete_positions @@ extract_position_markers {cobol|
        _|_IDENTIFICATION D_|_IVISION._|_
        _|_PROGRAM-ID _|_. _|_prog.
        _|_DATA _|_DIVISION.
        _|_WORKING-STORAGE _|_SECTION.
        01 DATA-NAME PIC X.
        _|_PROCEDURE _|_DIVISION.
          DISPLAY DATA-NAME
          STOP RUN.
  |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:2.8:
       1
       2 >         IDENTIFICATION DIVISION.
    ----           ^
       3           PROGRAM-ID . prog.
       4           DATA DIVISION.
    (line 1, character 8):
    List of completions (7 entries): [CLASS-ID;CONTROL;FUNCTION-ID;ID;IDENTIFICATION;INTERFACE-ID;PROGRAM-ID]
    __rootdir__/prog.cob:2.24:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                           ^
       3           PROGRAM-ID . prog.
       4           DATA DIVISION.
    (line 1, character 24):
    List of completions (1 entries): [DIVISION .]
    __rootdir__/prog.cob:2.32:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                                   ^
       3           PROGRAM-ID . prog.
       4           DATA DIVISION.
    (line 1, character 32):
    List of completions (11 entries): [AUTHOR;CLASS-ID;DATE-COMPILED;DATE-MODIFIED;DATE-WRITTEN;FUNCTION-ID;INSTALLATION;INTERFACE-ID;PROGRAM-ID;REMARKS;SECURITY]
    __rootdir__/prog.cob:3.8:
       1
       2           IDENTIFICATION DIVISION.
       3 >         PROGRAM-ID . prog.
    ----           ^
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
    (line 2, character 8):
    List of completions (11 entries): [AUTHOR;CLASS-ID;DATE-COMPILED;DATE-MODIFIED;DATE-WRITTEN;FUNCTION-ID;INSTALLATION;INTERFACE-ID;PROGRAM-ID;REMARKS;SECURITY]
    __rootdir__/prog.cob:3.19:
       1
       2           IDENTIFICATION DIVISION.
       3 >         PROGRAM-ID . prog.
    ----                      ^
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
    (line 2, character 19):
    List of completions (7 entries): [ALL;HIGH-VALUES;LOW-VALUES;.;QUOTES;SPACES;ZEROS]
    __rootdir__/prog.cob:3.21:
       1
       2           IDENTIFICATION DIVISION.
       3 >         PROGRAM-ID . prog.
    ----                        ^
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
    (line 2, character 21):
    List of completions (6 entries): [ALL;HIGH-VALUES;LOW-VALUES;QUOTES;SPACES;ZEROS]
    __rootdir__/prog.cob:4.8:
       1
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID . prog.
       4 >         DATA DIVISION.
    ----           ^
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
    (line 3, character 8):
    List of completions (29 entries): [END PROGRAM;AUTHOR;COMMUNICATION;CONFIGURATION;DATA;DATE-COMPILED;DATE-MODIFIED;DATE-WRITTEN;ENVIRONMENT;FD;FILE;FILE-CONTROL;ID;IDENTIFICATION;INPUT-OUTPUT;INSTALLATION;I-O-CONTROL;LINKAGE;LOCAL-STORAGE;OPTIONS;PROCEDURE;PROGRAM-ID;REMARKS;REPORT;SCREEN;SD;SECURITY;SELECT;WORKING-STORAGE]
    __rootdir__/prog.cob:4.13:
       1
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID . prog.
       4 >         DATA DIVISION.
    ----                ^
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
    (line 3, character 13):
    List of completions (1 entries): [DIVISION .]
    __rootdir__/prog.cob:5.8:
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID . prog.
       4           DATA DIVISION.
       5 >         WORKING-STORAGE SECTION.
    ----           ^
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
    (line 4, character 8):
    List of completions (15 entries): [END PROGRAM;COMMUNICATION;DATA;FD;FILE;ID;IDENTIFICATION;LINKAGE;LOCAL-STORAGE;PROCEDURE;PROGRAM-ID;REPORT;SCREEN;SD;WORKING-STORAGE]
    __rootdir__/prog.cob:5.24:
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID . prog.
       4           DATA DIVISION.
       5 >         WORKING-STORAGE SECTION.
    ----                           ^
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
    (line 4, character 24):
    List of completions (1 entries): [SECTION .]
    __rootdir__/prog.cob:7.8:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         PROCEDURE DIVISION.
    ----           ^
       8             DISPLAY DATA-NAME
       9             STOP RUN.
    (line 6, character 8):
    List of completions (11 entries): [END PROGRAM;COMMUNICATION;ID;IDENTIFICATION;LINKAGE;LOCAL-STORAGE;PROCEDURE;PROGRAM-ID;REPORT;SCREEN;WORKING-STORAGE]
    __rootdir__/prog.cob:7.18:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         PROCEDURE DIVISION.
    ----                     ^
       8             DISPLAY DATA-NAME
       9             STOP RUN.
    (line 6, character 18):
    List of completions (1 entries): [DIVISION] |}];;

let%expect_test "datadiv-completion" =
  let end_with_postproc = complete_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 _|_DATA-NAME _|_PIC X.
        _|_01 VAR PICTURE _|_X _|_USAGE _|_DISPLAY.
          88 BB _|_VALUES ARE _|_"x" _|_THRU "Z".
        PROCEDURE DIVISION.
          DISPLAY DATA-NAME.
          STOP RUN.
  |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:6.11:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^
       7           01 VAR PICTURE X USAGE DISPLAY.
       8             88 BB VALUES ARE "x" THRU "Z".
    (line 5, character 11):
    List of completions (73 entries): [ALIGNED;ANY;BASED;BINARY;BINARY-CHAR;BINARY-C-LONG;BINARY-DOUBLE;BINARY-LONG;BINARY-SHORT;BIT;BLANK;CLASS;COMP;COMP-0;COMP-1;COMP-10;COMP-15;COMP-2;COMP-3;COMP-4;COMP-5;COMP-6;COMP-9;COMP-N;COMP-X;DEFAULT;DESTINATION;DISPLAY;DYNAMIC;EXTERNAL;FILLER;FLOAT-BINARY-128;FLOAT-BINARY-32;FLOAT-BINARY-64;FLOAT-DECIMAL-16;FLOAT-DECIMAL-34;FLOAT-EXTENDED;FLOAT-LONG;FLOAT-SHORT;FUNCTION-POINTER;GLOBAL;GROUP-USAGE;INDEX;INVALID;IS EXTERNAL;IS GLOBAL;IS TYPEDEF;JUSTIFIED;LEADING;NATIONAL;OBJECT;OCCURS;PACKED-DECIMAL;.;PICTURE;POINTER;PRESENT;PROCEDURE-POINTER;PROGRAM-POINTER;PROPERTY;REDEFINES;SAME;SELECT;SIGN;SYNCHRONIZED;TRAILING;TYPE;TYPEDEF;USAGE;VAL-STATUS;VALUE;VALUES;VARYING]
    __rootdir__/prog.cob:6.21:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----                        ^
       7           01 VAR PICTURE X USAGE DISPLAY.
       8             88 BB VALUES ARE "x" THRU "Z".
    (line 5, character 21):
    List of completions (74 entries): [ALIGNED;ANY;BASED;BINARY;BINARY-CHAR;BINARY-C-LONG;BINARY-DOUBLE;BINARY-LONG;BINARY-SHORT;BIT;BLANK;CLASS;COMP;COMP-0;COMP-1;COMP-10;COMP-15;COMP-2;COMP-3;COMP-4;COMP-5;COMP-6;COMP-9;COMP-N;COMP-X;CONSTANT;DEFAULT;DESTINATION;DISPLAY;DYNAMIC;EXTERNAL;FLOAT-BINARY-128;FLOAT-BINARY-32;FLOAT-BINARY-64;FLOAT-DECIMAL-16;FLOAT-DECIMAL-34;FLOAT-EXTENDED;FLOAT-LONG;FLOAT-SHORT;FUNCTION-POINTER;GLOBAL;GROUP-USAGE;INDEX;INVALID;IS EXTERNAL;IS GLOBAL;IS TYPEDEF;JUSTIFIED;LEADING;NATIONAL;OBJECT;OCCURS;PACKED-DECIMAL;.;PICTURE;POINTER;PRESENT;PROCEDURE-POINTER;PROGRAM-POINTER;PROPERTY;REDEFINES;RENAMES;SAME;SELECT;SIGN;SYNCHRONIZED;TRAILING;TYPE;TYPEDEF;USAGE;VAL-STATUS;VALUE;VALUES;VARYING]
    __rootdir__/prog.cob:7.8:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         01 VAR PICTURE X USAGE DISPLAY.
    ----           ^
       8             88 BB VALUES ARE "x" THRU "Z".
       9           PROCEDURE DIVISION.
    (line 6, character 8):
    List of completions (11 entries): [END PROGRAM;COMMUNICATION;ID;IDENTIFICATION;LINKAGE;LOCAL-STORAGE;PROCEDURE;PROGRAM-ID;REPORT;SCREEN;WORKING-STORAGE]
    __rootdir__/prog.cob:7.23:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         01 VAR PICTURE X USAGE DISPLAY.
    ----                          ^
       8             88 BB VALUES ARE "x" THRU "Z".
       9           PROCEDURE DIVISION.
    (line 6, character 23):
    List of completions (1 entries): [IS]
    __rootdir__/prog.cob:7.25:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         01 VAR PICTURE X USAGE DISPLAY.
    ----                            ^
       8             88 BB VALUES ARE "x" THRU "Z".
       9           PROCEDURE DIVISION.
    (line 6, character 25):
    List of completions (74 entries): [ALIGNED;ANY;BASED;BINARY;BINARY-CHAR;BINARY-C-LONG;BINARY-DOUBLE;BINARY-LONG;BINARY-SHORT;BIT;BLANK;CLASS;COMP;COMP-0;COMP-1;COMP-10;COMP-15;COMP-2;COMP-3;COMP-4;COMP-5;COMP-6;COMP-9;COMP-N;COMP-X;DEFAULT;DEPENDING;DESTINATION;DISPLAY;DYNAMIC;EXTERNAL;FLOAT-BINARY-128;FLOAT-BINARY-32;FLOAT-BINARY-64;FLOAT-DECIMAL-16;FLOAT-DECIMAL-34;FLOAT-EXTENDED;FLOAT-LONG;FLOAT-SHORT;FUNCTION-POINTER;GLOBAL;GROUP-USAGE;INDEX;INVALID;IS EXTERNAL;IS GLOBAL;IS TYPEDEF;JUSTIFIED;LEADING;LOCALE;NATIONAL;OBJECT;OCCURS;PACKED-DECIMAL;.;PICTURE;POINTER;PRESENT;PROCEDURE-POINTER;PROGRAM-POINTER;PROPERTY;REDEFINES;SAME;SELECT;SIGN;SYNCHRONIZED;TRAILING;TYPE;TYPEDEF;USAGE;VAL-STATUS;VALUE;VALUES;VARYING]
    __rootdir__/prog.cob:7.31:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         01 VAR PICTURE X USAGE DISPLAY.
    ----                                  ^
       8             88 BB VALUES ARE "x" THRU "Z".
       9           PROCEDURE DIVISION.
    (line 6, character 31):
    List of completions (38 entries): [BINARY;BINARY-CHAR;BINARY-C-LONG;BINARY-DOUBLE;BINARY-LONG;BINARY-SHORT;BIT;COMP;COMP-0;COMP-1;COMP-10;COMP-15;COMP-2;COMP-3;COMP-4;COMP-5;COMP-6;COMP-9;COMP-N;COMP-X;DISPLAY;FLOAT-BINARY-128;FLOAT-BINARY-32;FLOAT-BINARY-64;FLOAT-DECIMAL-16;FLOAT-DECIMAL-34;FLOAT-EXTENDED;FLOAT-LONG;FLOAT-SHORT;FUNCTION-POINTER;INDEX;IS;NATIONAL;OBJECT;PACKED-DECIMAL;POINTER;PROCEDURE-POINTER;PROGRAM-POINTER]
    __rootdir__/prog.cob:8.16:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           01 VAR PICTURE X USAGE DISPLAY.
       8 >           88 BB VALUES ARE "x" THRU "Z".
    ----                   ^
       9           PROCEDURE DIVISION.
      10             DISPLAY DATA-NAME.
    (line 7, character 16):
    List of completions (2 entries): [VALUE;VALUES]
    __rootdir__/prog.cob:8.27:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           01 VAR PICTURE X USAGE DISPLAY.
       8 >           88 BB VALUES ARE "x" THRU "Z".
    ----                              ^
       9           PROCEDURE DIVISION.
      10             DISPLAY DATA-NAME.
    (line 7, character 27):
    List of completions (6 entries): [ALL;HIGH-VALUES;LOW-VALUES;QUOTES;SPACES;ZEROS]
    __rootdir__/prog.cob:8.31:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           01 VAR PICTURE X USAGE DISPLAY.
       8 >           88 BB VALUES ARE "x" THRU "Z".
    ----                                  ^
       9           PROCEDURE DIVISION.
      10             DISPLAY DATA-NAME.
    (line 7, character 31):
    List of completions (13 entries): [ALL;FALSE;HIGH-VALUES;IN;LOW-VALUES;.;QUOTES;SET;SPACES;THRU;TO;WHEN;ZEROS] |}]

let%expect_test "procedure-paragraph-completion" =
  let end_with_postproc = complete_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 DATA-NAME PIC X.
        01 VAR PICTURE X USAGE DISPLAY.
        PROCEDURE DIVISION.
          _|_FIRST-SECTION _|_SECTION.
            _|_DISPLAY DATA-NAME.
          _|_FIRST-PARAGRAPH.
            DISPLAY VAR.
            SECOND-SECTION SECTION.
              PERFORM _|_FIRST-PARAGRAPH.
          STOP RUN.
  |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:9.10:
       6           01 DATA-NAME PIC X.
       7           01 VAR PICTURE X USAGE DISPLAY.
       8           PROCEDURE DIVISION.
       9 >           FIRST-SECTION SECTION.
    ----             ^
      10               DISPLAY DATA-NAME.
      11             FIRST-PARAGRAPH.
    (line 8, character 10):
    List of completions (62 entries): [END PROGRAM;ACCEPT;ADD;ALLOCATE;ALTER;CALL;CANCEL;CLOSE;COMPUTE;CONTINUE;DECLARATIVES;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;ENTER;ENTRY;EVALUATE;EXIT;FREE;GENERATE;GO;GOBACK;ID;IDENTIFICATION;IF;INITIALIZE;INITIATE;INSPECT;INVOKE;MERGE;MOVE;MULTIPLY;NEXT SENTENCE;OPEN;PERFORM;.;PROGRAM-ID;PURGE;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REWRITE;SEARCH;SEND;SET;SORT;START;STOP;STRING;SUBTRACT;SUPPRESS;TERMINATE;TRANSFORM;UNLOCK;UNSTRING;VALIDATE;WRITE]
    __rootdir__/prog.cob:9.24:
       6           01 DATA-NAME PIC X.
       7           01 VAR PICTURE X USAGE DISPLAY.
       8           PROCEDURE DIVISION.
       9 >           FIRST-SECTION SECTION.
    ----                           ^
      10               DISPLAY DATA-NAME.
      11             FIRST-PARAGRAPH.
    (line 8, character 24):
    List of completions (2 entries): [.;SECTION]
    __rootdir__/prog.cob:10.12:
       7           01 VAR PICTURE X USAGE DISPLAY.
       8           PROCEDURE DIVISION.
       9             FIRST-SECTION SECTION.
      10 >             DISPLAY DATA-NAME.
    ----               ^
      11             FIRST-PARAGRAPH.
      12               DISPLAY VAR.
    (line 9, character 12):
    List of completions (61 entries): [END PROGRAM;ACCEPT;ADD;ALLOCATE;ALTER;CALL;CANCEL;CLOSE;COMPUTE;CONTINUE;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;ENTER;ENTRY;EVALUATE;EXIT;FREE;GENERATE;GO;GOBACK;ID;IDENTIFICATION;IF;INITIALIZE;INITIATE;INSPECT;INVOKE;MERGE;MOVE;MULTIPLY;NEXT SENTENCE;OPEN;PERFORM;.;PROGRAM-ID;PURGE;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REWRITE;SEARCH;SEND;SET;SORT;START;STOP;STRING;SUBTRACT;SUPPRESS;TERMINATE;TRANSFORM;UNLOCK;UNSTRING;VALIDATE;WRITE]
    __rootdir__/prog.cob:11.10:
       8           PROCEDURE DIVISION.
       9             FIRST-SECTION SECTION.
      10               DISPLAY DATA-NAME.
      11 >           FIRST-PARAGRAPH.
    ----             ^
      12               DISPLAY VAR.
      13               SECOND-SECTION SECTION.
    (line 10, character 10):
    List of completions (61 entries): [END PROGRAM;ACCEPT;ADD;ALLOCATE;ALTER;CALL;CANCEL;CLOSE;COMPUTE;CONTINUE;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;ENTER;ENTRY;EVALUATE;EXIT;FREE;GENERATE;GO;GOBACK;ID;IDENTIFICATION;IF;INITIALIZE;INITIATE;INSPECT;INVOKE;MERGE;MOVE;MULTIPLY;NEXT SENTENCE;OPEN;PERFORM;.;PROGRAM-ID;PURGE;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REWRITE;SEARCH;SEND;SET;SORT;START;STOP;STRING;SUBTRACT;SUPPRESS;TERMINATE;TRANSFORM;UNLOCK;UNSTRING;VALIDATE;WRITE]
    __rootdir__/prog.cob:14.22:
      11             FIRST-PARAGRAPH.
      12               DISPLAY VAR.
      13               SECOND-SECTION SECTION.
      14 >               PERFORM FIRST-PARAGRAPH.
    ----                         ^
      15             STOP RUN.
      16
    (line 13, character 22):
    List of completions (78 entries): [FIRST-SECTION;FIRST-PARAGRAPH;FIRST-PARAGRAPH IN FIRST-SECTION;SECOND-SECTION;DATA-NAME;VAR;ACCEPT;ADD;ADDRESS;ALLOCATE;ALTER;CALL;CANCEL;CLOSE;COMPUTE;CONTINUE;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;END-PERFORM;ENTER;ENTRY;EVALUATE;EXCEPTION-OBJECT;EXIT;FOREVER;FREE;FUNCTION;GENERATE;GO;GOBACK;IF;INITIALIZE;INITIATE;INSPECT;INVOKE;LINAGE-COUNTER;LINE-COUNTER;MERGE;MOVE;MULTIPLY;NEXT SENTENCE;NULL;OPEN;PAGE-COUNTER;PERFORM;PURGE;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REWRITE;SEARCH;SELF;SEND;SET;SORT;START;STOP;STRING;SUBTRACT;SUPER;SUPPRESS;TERMINATE;TEST;TRANSFORM;UNLOCK;UNSTRING;UNTIL;VALIDATE;VARYING;WITH;WRITE;ZEROS] |}]

let%expect_test "qualified-data-ref-completion" =
  let end_with_postproc = complete_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 AA.
          02 CC PICTURE X USAGE DISPLAY.
            03 DD PICTURE X USAGE DISPLAY.
        01 BB.
          02 CC PICTURE X USAGE DISPLAY.
        PROCEDURE DIVISION.
          DISPLAY _|_AA.
          STOP RUN.
  |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Unexpected PICTURE clause for group item 'CC'","range":{"end":{"character":25,"line":6},"start":{"character":16,"line":6}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:12.18:
       9           01 BB.
      10             02 CC PICTURE X USAGE DISPLAY.
      11           PROCEDURE DIVISION.
      12 >           DISPLAY AA.
    ----                     ^
      13             STOP RUN.
      14
    (line 11, character 18):
    List of completions (23 entries): [AA;CC;CC IN AA;DD;DD IN CC IN AA;BB;CC;CC IN BB;ADDRESS;ALL;EXCEPTION-OBJECT;FUNCTION;HIGH-VALUES;LINAGE-COUNTER;LINE-COUNTER;LOW-VALUES;NULL;PAGE-COUNTER;QUOTES;SELF;SPACES;SUPER;ZEROS] |}]

let%expect_test "procedure-completion" =
  let end_with_postproc = complete_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 AA PIC X.
        01 BB PIC X.
        PROCEDURE DIVISION.
          _|_DISPLAY AA _|_.
          MOVE _|_AA _|_TO _|_BB.
          MULTIPLY 4 _|_BY 2 _|_GIVING BB
            ON SIZE ERROR
            DISPLAY "ERROR"
          END-MULTIPLY.
          MULTIPLY AA _|_BY BB _|_ROUNDED _|_MODE _|_IS _|_TRUNCATION
            _|_ON SIZE _|_ERROR
            _|_DISPLAY "ERROR"
          _|_END-MULTIPLY.
          STOP RUN.
  |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:9.10:
       6           01 AA PIC X.
       7           01 BB PIC X.
       8           PROCEDURE DIVISION.
       9 >           DISPLAY AA .
    ----             ^
      10             MOVE AA TO BB.
      11             MULTIPLY 4 BY 2 GIVING BB
    (line 8, character 10):
    List of completions (62 entries): [END PROGRAM;ACCEPT;ADD;ALLOCATE;ALTER;CALL;CANCEL;CLOSE;COMPUTE;CONTINUE;DECLARATIVES;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;ENTER;ENTRY;EVALUATE;EXIT;FREE;GENERATE;GO;GOBACK;ID;IDENTIFICATION;IF;INITIALIZE;INITIATE;INSPECT;INVOKE;MERGE;MOVE;MULTIPLY;NEXT SENTENCE;OPEN;PERFORM;.;PROGRAM-ID;PURGE;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REWRITE;SEARCH;SEND;SET;SORT;START;STOP;STRING;SUBTRACT;SUPPRESS;TERMINATE;TRANSFORM;UNLOCK;UNSTRING;VALIDATE;WRITE]
    __rootdir__/prog.cob:9.21:
       6           01 AA PIC X.
       7           01 BB PIC X.
       8           PROCEDURE DIVISION.
       9 >           DISPLAY AA .
    ----                        ^
      10             MOVE AA TO BB.
      11             MULTIPLY 4 BY 2 GIVING BB
    (line 8, character 21):
    List of completions (108 entries): [AA;BB;AS FACTORY OF;AS UNIVERSAL;OF SUPER;ACCEPT;ADD;ADDRESS;ALL;ALLOCATE;ALTER;AS;AT;BACKGROUND-COLOR;BELL;BLANK;BLINK;CALL;CANCEL;CLOSE;COL;COLUMN;COMPUTE;CONTINUE;CONTROL;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;END-DISPLAY;ENTER;ENTRY;ERASE;EVALUATE;EXCEPTION;EXCEPTION-OBJECT;EXIT;FOREGROUND-COLOR;FREE;FUNCTION;GENERATE;GO;GOBACK;GRID;HIGHLIGHT;HIGH-VALUES;IF;IN;INITIALIZE;INITIATE;INSPECT;INVOKE;LEFTLINE;LINAGE-COUNTER;LINE;LINE-COUNTER;LOWLIGHT;LOW-VALUES;MERGE;MODE;MOVE;MULTIPLY;NEXT SENTENCE;NOT ON EXCEPTION;NULL;OF;ON EXCEPTION;OPEN;OVERLINE;PAGE-COUNTER;PERFORM;.;POSITION;PURGE;QUOTES;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REVERSE-VIDEO;REWRITE;SEARCH;SELF;SEND;SET;SIZE;SORT;SPACES;START;STOP;STRING;SUBTRACT;SUPER;SUPPRESS;TERMINATE;TRANSFORM;UNDERLINE;UNLOCK;UNSTRING;UPON;VALIDATE;WITH;WITH NO ADVANCING;WRITE;ZEROS]
    __rootdir__/prog.cob:10.15:
       7           01 BB PIC X.
       8           PROCEDURE DIVISION.
       9             DISPLAY AA .
      10 >           MOVE AA TO BB.
    ----                  ^
      11             MULTIPLY 4 BY 2 GIVING BB
      12               ON SIZE ERROR
    (line 9, character 15):
    List of completions (18 entries): [AA;BB;ADDRESS;ALL;CORRESPONDING;EXCEPTION-OBJECT;FUNCTION;HIGH-VALUES;LINAGE-COUNTER;LINE-COUNTER;LOW-VALUES;NULL;PAGE-COUNTER;QUOTES;SELF;SPACES;SUPER;ZEROS]
    __rootdir__/prog.cob:10.18:
       7           01 BB PIC X.
       8           PROCEDURE DIVISION.
       9             DISPLAY AA .
      10 >           MOVE AA TO BB.
    ----                     ^
      11             MULTIPLY 4 BY 2 GIVING BB
      12               ON SIZE ERROR
    (line 9, character 18):
    List of completions (7 entries): [AS FACTORY OF;AS UNIVERSAL;OF SUPER;AS;IN;OF;TO]
    __rootdir__/prog.cob:10.21:
       7           01 BB PIC X.
       8           PROCEDURE DIVISION.
       9             DISPLAY AA .
      10 >           MOVE AA TO BB.
    ----                        ^
      11             MULTIPLY 4 BY 2 GIVING BB
      12               ON SIZE ERROR
    (line 9, character 21):
    List of completions (11 entries): [AA;BB;ADDRESS;EXCEPTION-OBJECT;FUNCTION;LINAGE-COUNTER;LINE-COUNTER;NULL;PAGE-COUNTER;SELF;SUPER]
    __rootdir__/prog.cob:11.21:
       8           PROCEDURE DIVISION.
       9             DISPLAY AA .
      10             MOVE AA TO BB.
      11 >           MULTIPLY 4 BY 2 GIVING BB
    ----                        ^
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
    (line 10, character 21):
    List of completions (1 entries): [BY]
    __rootdir__/prog.cob:11.26:
       8           PROCEDURE DIVISION.
       9             DISPLAY AA .
      10             MOVE AA TO BB.
      11 >           MULTIPLY 4 BY 2 GIVING BB
    ----                             ^
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
    (line 10, character 26):
    List of completions (1 entries): [GIVING]
    __rootdir__/prog.cob:15.22:
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15 >           MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
    ----                         ^
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
    (line 14, character 22):
    List of completions (7 entries): [AS FACTORY OF;AS UNIVERSAL;OF SUPER;AS;BY;IN;OF]
    __rootdir__/prog.cob:15.28:
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15 >           MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
    ----                               ^
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
    (line 14, character 28):
    List of completions (79 entries): [AA;BB;AS FACTORY OF;AS UNIVERSAL;OF SUPER;ACCEPT;ADD;ADDRESS;ALLOCATE;ALTER;AS;CALL;CANCEL;CLOSE;COMPUTE;CONTINUE;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;END-MULTIPLY;ENTER;ENTRY;EVALUATE;EXCEPTION-OBJECT;EXIT;FREE;FUNCTION;GENERATE;GIVING;GO;GOBACK;IF;IN;INITIALIZE;INITIATE;INSPECT;INVOKE;LINAGE-COUNTER;LINE-COUNTER;MERGE;MOVE;MULTIPLY;NEXT SENTENCE;NOT ON SIZE ERROR;NULL;OF;ON SIZE ERROR;OPEN;PAGE-COUNTER;PERFORM;.;PURGE;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REWRITE;ROUNDED;SEARCH;SELF;SEND;SET;SORT;START;STOP;STRING;SUBTRACT;SUPER;SUPPRESS;TERMINATE;TRANSFORM;UNLOCK;UNSTRING;VALIDATE;WRITE]
    __rootdir__/prog.cob:15.36:
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15 >           MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
    ----                                       ^
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
    (line 14, character 36):
    List of completions (72 entries): [AA;BB;ACCEPT;ADD;ADDRESS;ALLOCATE;ALTER;CALL;CANCEL;CLOSE;COMPUTE;CONTINUE;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;END-MULTIPLY;ENTER;ENTRY;EVALUATE;EXCEPTION-OBJECT;EXIT;FREE;FUNCTION;GENERATE;GO;GOBACK;IF;INITIALIZE;INITIATE;INSPECT;INVOKE;LINAGE-COUNTER;LINE-COUNTER;MERGE;MODE;MOVE;MULTIPLY;NEXT SENTENCE;NOT ON SIZE ERROR;NULL;ON SIZE ERROR;OPEN;PAGE-COUNTER;PERFORM;.;PURGE;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REWRITE;SEARCH;SELF;SEND;SET;SORT;START;STOP;STRING;SUBTRACT;SUPER;SUPPRESS;TERMINATE;TRANSFORM;UNLOCK;UNSTRING;VALIDATE;WRITE]
    __rootdir__/prog.cob:15.41:
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15 >           MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
    ----                                            ^
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
    (line 14, character 41):
    List of completions (9 entries): [AWAY-FROM-ZERO;IS;NEAREST-AWAY-FROM-ZERO;NEAREST-EVEN;NEAREST-TOWARD-ZERO;PROHIBITED;TOWARD-GREATER;TOWARD-LESSER;TRUNCATION]
    __rootdir__/prog.cob:15.44:
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15 >           MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
    ----                                               ^
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
    (line 14, character 44):
    List of completions (8 entries): [AWAY-FROM-ZERO;NEAREST-AWAY-FROM-ZERO;NEAREST-EVEN;NEAREST-TOWARD-ZERO;PROHIBITED;TOWARD-GREATER;TOWARD-LESSER;TRUNCATION]
    __rootdir__/prog.cob:16.12:
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15             MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
      16 >             ON SIZE ERROR
    ----               ^
      17               DISPLAY "ERROR"
      18             END-MULTIPLY.
    (line 15, character 12):
    List of completions (71 entries): [AA;BB;ACCEPT;ADD;ADDRESS;ALLOCATE;ALTER;CALL;CANCEL;CLOSE;COMPUTE;CONTINUE;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;END-MULTIPLY;ENTER;ENTRY;EVALUATE;EXCEPTION-OBJECT;EXIT;FREE;FUNCTION;GENERATE;GO;GOBACK;IF;INITIALIZE;INITIATE;INSPECT;INVOKE;LINAGE-COUNTER;LINE-COUNTER;MERGE;MOVE;MULTIPLY;NEXT SENTENCE;NOT ON SIZE ERROR;NULL;ON SIZE ERROR;OPEN;PAGE-COUNTER;PERFORM;.;PURGE;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REWRITE;SEARCH;SELF;SEND;SET;SORT;START;STOP;STRING;SUBTRACT;SUPER;SUPPRESS;TERMINATE;TRANSFORM;UNLOCK;UNSTRING;VALIDATE;WRITE]
    __rootdir__/prog.cob:16.20:
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15             MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
      16 >             ON SIZE ERROR
    ----                       ^
      17               DISPLAY "ERROR"
      18             END-MULTIPLY.
    (line 15, character 20):
    Empty completion list
    __rootdir__/prog.cob:17.12:
      14             END-MULTIPLY.
      15             MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
      16               ON SIZE ERROR
      17 >             DISPLAY "ERROR"
    ----               ^
      18             END-MULTIPLY.
      19             STOP RUN.
    (line 16, character 12):
    List of completions (56 entries): [ACCEPT;ADD;ALLOCATE;ALTER;CALL;CANCEL;CLOSE;COMPUTE;CONTINUE;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;ENTER;ENTRY;EVALUATE;EXIT;FREE;GENERATE;GO;GOBACK;IF;INITIALIZE;INITIATE;INSPECT;INVOKE;MERGE;MOVE;MULTIPLY;NEXT SENTENCE;OPEN;PERFORM;PURGE;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REWRITE;SEARCH;SEND;SET;SORT;START;STOP;STRING;SUBTRACT;SUPPRESS;TERMINATE;TRANSFORM;UNLOCK;UNSTRING;VALIDATE;WRITE]
    __rootdir__/prog.cob:18.10:
      15             MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
      18 >           END-MULTIPLY.
    ----             ^
      19             STOP RUN.
      20
    (line 17, character 10):
    List of completions (104 entries): [AA;BB;ACCEPT;ADD;ADDRESS;ALL;ALLOCATE;ALTER;AT;BACKGROUND-COLOR;BELL;BLANK;BLINK;CALL;CANCEL;CLOSE;COL;COLUMN;COMPUTE;CONTINUE;CONTROL;DELETE;DISABLE;DISPLAY;DIVIDE;ENABLE;END-DISPLAY;END-MULTIPLY;ENTER;ENTRY;ERASE;EVALUATE;EXCEPTION;EXCEPTION-OBJECT;EXIT;FOREGROUND-COLOR;FREE;FUNCTION;GENERATE;GO;GOBACK;GRID;HIGHLIGHT;HIGH-VALUES;IF;INITIALIZE;INITIATE;INSPECT;INVOKE;LEFTLINE;LINAGE-COUNTER;LINE;LINE-COUNTER;LOWLIGHT;LOW-VALUES;MERGE;MODE;MOVE;MULTIPLY;NEXT SENTENCE;NOT ON EXCEPTION;NOT ON SIZE ERROR;NULL;ON EXCEPTION;OPEN;OVERLINE;PAGE-COUNTER;PERFORM;.;POSITION;PURGE;QUOTES;RAISE;READ;RECEIVE;RELEASE;RESUME;RETURN;REVERSE-VIDEO;REWRITE;SEARCH;SELF;SEND;SET;SIZE;SORT;SPACES;START;STOP;STRING;SUBTRACT;SUPER;SUPPRESS;TERMINATE;TRANSFORM;UNDERLINE;UNLOCK;UNSTRING;UPON;VALIDATE;WITH;WITH NO ADVANCING;WRITE;ZEROS] |}]

(* Testing completion with nullable expected tokens (default_section_clauses) *)
let%expect_test "control-completion" =
  let end_with_postproc = complete_positions @@ extract_position_markers {cobol|
        CONTROL DIVISION.
        _|_DEFAULT _|_SECTION.
          _|_ACCEPT _|_TERMINAL
          _|_DISPLAY IS TERMINAL.
        _|_IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        PROCEDURE DIVISION.
          STOP RUN.
  |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:3.8:
       1
       2           CONTROL DIVISION.
       3 >         DEFAULT SECTION.
    ----           ^
       4             ACCEPT TERMINAL
       5             DISPLAY IS TERMINAL.
    (line 2, character 8):
    List of completions (7 entries): [CLASS-ID;DEFAULT;FUNCTION-ID;ID;IDENTIFICATION;INTERFACE-ID;PROGRAM-ID]
    __rootdir__/prog.cob:3.16:
       1
       2           CONTROL DIVISION.
       3 >         DEFAULT SECTION.
    ----                   ^
       4             ACCEPT TERMINAL
       5             DISPLAY IS TERMINAL.
    (line 2, character 16):
    List of completions (1 entries): [SECTION .]
    __rootdir__/prog.cob:4.10:
       1
       2           CONTROL DIVISION.
       3           DEFAULT SECTION.
       4 >           ACCEPT TERMINAL
    ----             ^
       5             DISPLAY IS TERMINAL.
       6           IDENTIFICATION DIVISION.
    (line 3, character 10):
    List of completions (9 entries): [ACCEPT;CLASS-ID;DISPLAY;FUNCTION-ID;ID;IDENTIFICATION;INTERFACE-ID;.;PROGRAM-ID]
    __rootdir__/prog.cob:4.17:
       1
       2           CONTROL DIVISION.
       3           DEFAULT SECTION.
       4 >           ACCEPT TERMINAL
    ----                    ^
       5             DISPLAY IS TERMINAL.
       6           IDENTIFICATION DIVISION.
    (line 3, character 17):
    List of completions (2 entries): [IS;TERMINAL]
    __rootdir__/prog.cob:5.10:
       2           CONTROL DIVISION.
       3           DEFAULT SECTION.
       4             ACCEPT TERMINAL
       5 >           DISPLAY IS TERMINAL.
    ----             ^
       6           IDENTIFICATION DIVISION.
       7           PROGRAM-ID. prog.
    (line 4, character 10):
    List of completions (2 entries): [DISPLAY;.]
    __rootdir__/prog.cob:6.8:
       3           DEFAULT SECTION.
       4             ACCEPT TERMINAL
       5             DISPLAY IS TERMINAL.
       6 >         IDENTIFICATION DIVISION.
    ----           ^
       7           PROGRAM-ID. prog.
       8           PROCEDURE DIVISION.
    (line 5, character 8):
    List of completions (6 entries): [CLASS-ID;FUNCTION-ID;ID;IDENTIFICATION;INTERFACE-ID;PROGRAM-ID] |}]

