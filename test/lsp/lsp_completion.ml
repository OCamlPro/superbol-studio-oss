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

let completion_positions (doc, positions) : string -> unit =
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
    match LSP.Request.completion server params with
    | None ->
        Pretty.out "Failed completion@."
    | Some `CompletionList { items; _ } when items == [] ->
        Pretty.out "Empty completion list@."
    | Some `CompletionList { items; _ } ->
        let pp_comp_item ppf (item: CompletionItem.t) =
          Fmt.string ppf item.label in
        Pretty.out "List of completions (%d entries): [%a]\n"
          (List.length items) (Fmt.list ~sep:(Fmt.any ";") pp_comp_item) items;

  in
  StringMap.iter (fun n p -> completions_at_position ~key:n p) positions.pos_map;
  List.iter (fun p -> completions_at_position p) positions.pos_anonymous;
  end_with_postproc
;;

let%expect_test "division-and-section-completion" =
  let end_with_postproc = completion_positions @@ extract_position_markers {cobol|
        _|_IDENTIFICATION D_|_IVISION._|_
        PROGRAM-ID _|_. _|_prog.
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
    List of completions (7 entries): [PROGRAM-ID;INTERFACE-ID.;IDENTIFICATION DIVISION.;ID DIVISION.;FUNCTION-ID;CONTROL DIVISION.;CLASS-ID.]
    __rootdir__/prog.cob:2.24:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                           ^
       3           PROGRAM-ID . prog.
       4           DATA DIVISION.
    (line 1, character 24):
    List of completions (1 entries): [DIVISION.]
    __rootdir__/prog.cob:2.32:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                                   ^
       3           PROGRAM-ID . prog.
       4           DATA DIVISION.
    (line 1, character 32):
    List of completions (11 entries): [SECURITY.;REMARKS.;PROGRAM-ID;INTERFACE-ID.;INSTALLATION.;FUNCTION-ID;DATE-WRITTEN.;DATE-MODIFIED.;DATE-COMPILED.;CLASS-ID.;AUTHOR.]
    __rootdir__/prog.cob:3.19:
       1
       2           IDENTIFICATION DIVISION.
       3 >         PROGRAM-ID . prog.
    ----                      ^
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
    (line 2, character 19):
    List of completions (7 entries): [ZEROS;SPACES;QUOTES;.;LOW-VALUES;HIGH-VALUES;ALL]
    __rootdir__/prog.cob:3.21:
       1
       2           IDENTIFICATION DIVISION.
       3 >         PROGRAM-ID . prog.
    ----                        ^
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
    (line 2, character 21):
    List of completions (6 entries): [ZEROS;SPACES;QUOTES;LOW-VALUES;HIGH-VALUES;ALL]
    __rootdir__/prog.cob:4.8:
       1
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID . prog.
       4 >         DATA DIVISION.
    ----           ^
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
    (line 3, character 8):
    List of completions (29 entries): [WORKING-STORAGE SECTION.;SELECT;SECURITY.;SD;SCREEN SECTION.;REPORT SECTION.;REMARKS.;PROGRAM-ID;PROCEDURE DIVISION;OPTIONS.;LOCAL-STORAGE SECTION.;LINKAGE SECTION.;I-O-CONTROL.;INSTALLATION.;INPUT-OUTPUT SECTION.;IDENTIFICATION DIVISION.;ID DIVISION.;FILE-CONTROL.;FILE SECTION.;FD;ENVIRONMENT DIVISION.;END PROGRAM;DATE-WRITTEN.;DATE-MODIFIED.;DATE-COMPILED.;DATA DIVISION.;CONFIGURATION SECTION.;COMMUNICATION SECTION.;AUTHOR.]
    __rootdir__/prog.cob:4.13:
       1
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID . prog.
       4 >         DATA DIVISION.
    ----                ^
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
    (line 3, character 13):
    List of completions (1 entries): [DIVISION.]
    __rootdir__/prog.cob:5.8:
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID . prog.
       4           DATA DIVISION.
       5 >         WORKING-STORAGE SECTION.
    ----           ^
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
    (line 4, character 8):
    List of completions (15 entries): [WORKING-STORAGE SECTION.;SD;SCREEN SECTION.;REPORT SECTION.;PROGRAM-ID;PROCEDURE DIVISION;LOCAL-STORAGE SECTION.;LINKAGE SECTION.;IDENTIFICATION DIVISION.;ID DIVISION.;FILE SECTION.;FD;END PROGRAM;DATA DIVISION.;COMMUNICATION SECTION.]
    __rootdir__/prog.cob:5.24:
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID . prog.
       4           DATA DIVISION.
       5 >         WORKING-STORAGE SECTION.
    ----                           ^
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
    (line 4, character 24):
    List of completions (1 entries): [SECTION.]
    __rootdir__/prog.cob:7.8:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         PROCEDURE DIVISION.
    ----           ^
       8             DISPLAY DATA-NAME
       9             STOP RUN.
    (line 6, character 8):
    List of completions (11 entries): [WORKING-STORAGE SECTION.;SCREEN SECTION.;REPORT SECTION.;PROGRAM-ID;PROCEDURE DIVISION;LOCAL-STORAGE SECTION.;LINKAGE SECTION.;IDENTIFICATION DIVISION.;ID DIVISION.;END PROGRAM;COMMUNICATION SECTION.]
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
  let end_with_postproc = completion_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 _|_AA _|_PIC X.
        01 BB PIC X VAL-STATUS AA _|_WHEN _|_ERROR _|_ON RELATION FOR AA.
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
       6 >         01 AA PIC X.
    ----              ^
       7           01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
       8           01 VAR PICTURE X USAGE DISPLAY.
    (line 5, character 11):
    List of completions (74 entries): [VARYING;VALUES;VALUE;VAL-STATUS;USAGE;TYPEDEF;TYPE;TRAILING;SYNCHRONIZED;SIGN;SELECT WHEN;SAME AS;REDEFINES;PROPERTY;PROGRAM-POINTER;PROCEDURE-POINTER;PRESENT WHEN;POINTER;PICTURE;.;PACKED-DECIMAL;OCCURS;OBJECT REFERENCE;NATIONAL;LEADING;JUSTIFIED;IS TYPEDEF;IS GLOBAL;IS EXTERNAL;INVALID WHEN;INDEX;GROUP-USAGE;GLOBAL;FUNCTION-POINTER;FLOAT-SHORT;FLOAT-LONG;FLOAT-EXTENDED;FLOAT-DECIMAL-34;FLOAT-DECIMAL-16;FLOAT-BINARY-64;FLOAT-BINARY-32;FLOAT-BINARY-128;FILLER;EXTERNAL;DYNAMIC;DISPLAY;DESTINATION;DEFAULT;CONSTANT RECORD;COMP-X;COMP-N;COMP-9;COMP-6;COMP-5;COMP-4;COMP-3;COMP-2;COMP-15;COMP-10;COMP-1;COMP-0;COMP;CLASS;BLANK;BIT;BINARY-SHORT;BINARY-LONG;BINARY-DOUBLE;BINARY-C-LONG;BINARY-CHAR;BINARY;BASED;ANY LENGTH;ALIGNED]
    __rootdir__/prog.cob:6.14:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 AA PIC X.
    ----                 ^
       7           01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
       8           01 VAR PICTURE X USAGE DISPLAY.
    (line 5, character 14):
    List of completions (75 entries): [VARYING;VALUES;VALUE;VAL-STATUS;USAGE;TYPEDEF;TYPE;TRAILING;SYNCHRONIZED;SIGN;SELECT WHEN;SAME AS;RENAMES;REDEFINES;PROPERTY;PROGRAM-POINTER;PROCEDURE-POINTER;PRESENT WHEN;POINTER;PICTURE;.;PACKED-DECIMAL;OCCURS;OBJECT REFERENCE;NATIONAL;LEADING;JUSTIFIED;IS TYPEDEF;IS GLOBAL;IS EXTERNAL;INVALID WHEN;INDEX;GROUP-USAGE;GLOBAL;FUNCTION-POINTER;FLOAT-SHORT;FLOAT-LONG;FLOAT-EXTENDED;FLOAT-DECIMAL-34;FLOAT-DECIMAL-16;FLOAT-BINARY-64;FLOAT-BINARY-32;FLOAT-BINARY-128;EXTERNAL;DYNAMIC;DISPLAY;DESTINATION;DEFAULT;CONSTANT RECORD;CONSTANT;COMP-X;COMP-N;COMP-9;COMP-6;COMP-5;COMP-4;COMP-3;COMP-2;COMP-15;COMP-10;COMP-1;COMP-0;COMP;CLASS;BLANK;BIT;BINARY-SHORT;BINARY-LONG;BINARY-DOUBLE;BINARY-C-LONG;BINARY-CHAR;BINARY;BASED;ANY LENGTH;ALIGNED]
    __rootdir__/prog.cob:7.34:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 AA PIC X.
       7 >         01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
    ----                                     ^
       8           01 VAR PICTURE X USAGE DISPLAY.
       9             88 BB VALUES ARE "x" THRU "Z".
    (line 6, character 34):
    List of completions (6 entries): [WHEN;OF;NO ERROR;IN;ERROR;AS]
    __rootdir__/prog.cob:7.39:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 AA PIC X.
       7 >         01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
    ----                                          ^
       8           01 VAR PICTURE X USAGE DISPLAY.
       9             88 BB VALUES ARE "x" THRU "Z".
    (line 6, character 39):
    List of completions (2 entries): [NO ERROR;ERROR]
    __rootdir__/prog.cob:7.45:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 AA PIC X.
       7 >         01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
    ----                                                ^
       8           01 VAR PICTURE X USAGE DISPLAY.
       9             88 BB VALUES ARE "x" THRU "Z".
    (line 6, character 45):
    List of completions (2 entries): [ON;FOR]
    __rootdir__/prog.cob:8.8:
       5           WORKING-STORAGE SECTION.
       6           01 AA PIC X.
       7           01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
       8 >         01 VAR PICTURE X USAGE DISPLAY.
    ----           ^
       9             88 BB VALUES ARE "x" THRU "Z".
      10           PROCEDURE DIVISION.
    (line 7, character 8):
    List of completions (11 entries): [WORKING-STORAGE SECTION.;SCREEN SECTION.;REPORT SECTION.;PROGRAM-ID;PROCEDURE DIVISION;LOCAL-STORAGE SECTION.;LINKAGE SECTION.;IDENTIFICATION DIVISION.;ID DIVISION.;END PROGRAM;COMMUNICATION SECTION.]
    __rootdir__/prog.cob:8.23:
       5           WORKING-STORAGE SECTION.
       6           01 AA PIC X.
       7           01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
       8 >         01 VAR PICTURE X USAGE DISPLAY.
    ----                          ^
       9             88 BB VALUES ARE "x" THRU "Z".
      10           PROCEDURE DIVISION.
    (line 7, character 23):
    List of completions (1 entries): [IS]
    __rootdir__/prog.cob:8.25:
       5           WORKING-STORAGE SECTION.
       6           01 AA PIC X.
       7           01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
       8 >         01 VAR PICTURE X USAGE DISPLAY.
    ----                            ^
       9             88 BB VALUES ARE "x" THRU "Z".
      10           PROCEDURE DIVISION.
    (line 7, character 25):
    List of completions (75 entries): [VARYING;VALUES;VALUE;VAL-STATUS;USAGE;TYPEDEF;TYPE;TRAILING;SYNCHRONIZED;SIGN;SELECT WHEN;SAME AS;REDEFINES;PROPERTY;PROGRAM-POINTER;PROCEDURE-POINTER;PRESENT WHEN;POINTER;PICTURE;.;PACKED-DECIMAL;OCCURS;OBJECT REFERENCE;NATIONAL;LOCALE;LEADING;JUSTIFIED;IS TYPEDEF;IS GLOBAL;IS EXTERNAL;INVALID WHEN;INDEX;GROUP-USAGE;GLOBAL;FUNCTION-POINTER;FLOAT-SHORT;FLOAT-LONG;FLOAT-EXTENDED;FLOAT-DECIMAL-34;FLOAT-DECIMAL-16;FLOAT-BINARY-64;FLOAT-BINARY-32;FLOAT-BINARY-128;EXTERNAL;DYNAMIC;DISPLAY;DESTINATION;DEPENDING;DEFAULT;CONSTANT RECORD;COMP-X;COMP-N;COMP-9;COMP-6;COMP-5;COMP-4;COMP-3;COMP-2;COMP-15;COMP-10;COMP-1;COMP-0;COMP;CLASS;BLANK;BIT;BINARY-SHORT;BINARY-LONG;BINARY-DOUBLE;BINARY-C-LONG;BINARY-CHAR;BINARY;BASED;ANY LENGTH;ALIGNED]
    __rootdir__/prog.cob:8.31:
       5           WORKING-STORAGE SECTION.
       6           01 AA PIC X.
       7           01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
       8 >         01 VAR PICTURE X USAGE DISPLAY.
    ----                                  ^
       9             88 BB VALUES ARE "x" THRU "Z".
      10           PROCEDURE DIVISION.
    (line 7, character 31):
    List of completions (38 entries): [PROGRAM-POINTER;PROCEDURE-POINTER;POINTER;PACKED-DECIMAL;OBJECT REFERENCE;NATIONAL;IS;INDEX;FUNCTION-POINTER;FLOAT-SHORT;FLOAT-LONG;FLOAT-EXTENDED;FLOAT-DECIMAL-34;FLOAT-DECIMAL-16;FLOAT-BINARY-64;FLOAT-BINARY-32;FLOAT-BINARY-128;DISPLAY;COMP-X;COMP-N;COMP-9;COMP-6;COMP-5;COMP-4;COMP-3;COMP-2;COMP-15;COMP-10;COMP-1;COMP-0;COMP;BIT;BINARY-SHORT;BINARY-LONG;BINARY-DOUBLE;BINARY-C-LONG;BINARY-CHAR;BINARY]
    __rootdir__/prog.cob:9.16:
       6           01 AA PIC X.
       7           01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
       8           01 VAR PICTURE X USAGE DISPLAY.
       9 >           88 BB VALUES ARE "x" THRU "Z".
    ----                   ^
      10           PROCEDURE DIVISION.
      11             DISPLAY DATA-NAME.
    (line 8, character 16):
    List of completions (2 entries): [VALUES;VALUE]
    __rootdir__/prog.cob:9.27:
       6           01 AA PIC X.
       7           01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
       8           01 VAR PICTURE X USAGE DISPLAY.
       9 >           88 BB VALUES ARE "x" THRU "Z".
    ----                              ^
      10           PROCEDURE DIVISION.
      11             DISPLAY DATA-NAME.
    (line 8, character 27):
    List of completions (6 entries): [ZEROS;SPACES;QUOTES;LOW-VALUES;HIGH-VALUES;ALL]
    __rootdir__/prog.cob:9.31:
       6           01 AA PIC X.
       7           01 BB PIC X VAL-STATUS AA WHEN ERROR ON RELATION FOR AA.
       8           01 VAR PICTURE X USAGE DISPLAY.
       9 >           88 BB VALUES ARE "x" THRU "Z".
    ----                                  ^
      10           PROCEDURE DIVISION.
      11             DISPLAY DATA-NAME.
    (line 8, character 31):
    List of completions (13 entries): [ZEROS;WHEN;TO FALSE;THRU;SPACES;SET;QUOTES;.;LOW-VALUES;IN;HIGH-VALUES;FALSE;ALL] |}]

let%expect_test "procedure-paragraph-completion" =
  let end_with_postproc = completion_positions @@ extract_position_markers {cobol|
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
    List of completions (62 entries): [WRITE;VALIDATE;UNSTRING;UNLOCK;TRANSFORM;TERMINATE;SUPPRESS;SUBTRACT;STRING;STOP;START;SORT;SET;SEND;SEARCH;REWRITE;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;PURGE;PROGRAM-ID;.;PERFORM;OPEN;NEXT SENTENCE;MULTIPLY;MOVE;MERGE;INVOKE;INSPECT;INITIATE;INITIALIZE;IF;IDENTIFICATION DIVISION.;ID DIVISION.;GOBACK;GO;GENERATE;FREE;EXIT;EVALUATE;ENTRY;ENTER;END PROGRAM;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;DECLARATIVES.;CONTINUE;COMPUTE;CLOSE;CANCEL;CALL;ALTER;ALLOCATE;ADD;ACCEPT]
    __rootdir__/prog.cob:9.24:
       6           01 DATA-NAME PIC X.
       7           01 VAR PICTURE X USAGE DISPLAY.
       8           PROCEDURE DIVISION.
       9 >           FIRST-SECTION SECTION.
    ----                           ^
      10               DISPLAY DATA-NAME.
      11             FIRST-PARAGRAPH.
    (line 8, character 24):
    List of completions (2 entries): [SECTION;.]
    __rootdir__/prog.cob:10.12:
       7           01 VAR PICTURE X USAGE DISPLAY.
       8           PROCEDURE DIVISION.
       9             FIRST-SECTION SECTION.
      10 >             DISPLAY DATA-NAME.
    ----               ^
      11             FIRST-PARAGRAPH.
      12               DISPLAY VAR.
    (line 9, character 12):
    List of completions (61 entries): [WRITE;VALIDATE;UNSTRING;UNLOCK;TRANSFORM;TERMINATE;SUPPRESS;SUBTRACT;STRING;STOP;START;SORT;SET;SEND;SEARCH;REWRITE;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;PURGE;PROGRAM-ID;.;PERFORM;OPEN;NEXT SENTENCE;MULTIPLY;MOVE;MERGE;INVOKE;INSPECT;INITIATE;INITIALIZE;IF;IDENTIFICATION DIVISION.;ID DIVISION.;GOBACK;GO;GENERATE;FREE;EXIT;EVALUATE;ENTRY;ENTER;END PROGRAM;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;CONTINUE;COMPUTE;CLOSE;CANCEL;CALL;ALTER;ALLOCATE;ADD;ACCEPT]
    __rootdir__/prog.cob:11.10:
       8           PROCEDURE DIVISION.
       9             FIRST-SECTION SECTION.
      10               DISPLAY DATA-NAME.
      11 >           FIRST-PARAGRAPH.
    ----             ^
      12               DISPLAY VAR.
      13               SECOND-SECTION SECTION.
    (line 10, character 10):
    List of completions (61 entries): [WRITE;VALIDATE;UNSTRING;UNLOCK;TRANSFORM;TERMINATE;SUPPRESS;SUBTRACT;STRING;STOP;START;SORT;SET;SEND;SEARCH;REWRITE;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;PURGE;PROGRAM-ID;.;PERFORM;OPEN;NEXT SENTENCE;MULTIPLY;MOVE;MERGE;INVOKE;INSPECT;INITIATE;INITIALIZE;IF;IDENTIFICATION DIVISION.;ID DIVISION.;GOBACK;GO;GENERATE;FREE;EXIT;EVALUATE;ENTRY;ENTER;END PROGRAM;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;CONTINUE;COMPUTE;CLOSE;CANCEL;CALL;ALTER;ALLOCATE;ADD;ACCEPT]
    __rootdir__/prog.cob:14.22:
      11             FIRST-PARAGRAPH.
      12               DISPLAY VAR.
      13               SECOND-SECTION SECTION.
      14 >               PERFORM FIRST-PARAGRAPH.
    ----                         ^
      15             STOP RUN.
      16
    (line 13, character 22):
    List of completions (78 entries): [ZEROS TIMES;WRITE;WITH TEST;VARYING;VALIDATE;UNTIL;UNSTRING;UNLOCK;TRANSFORM;TEST;TERMINATE;SUPPRESS;SUPER;SUBTRACT;STRING;STOP;START;SORT;SET;SEND;SELF;SEARCH;REWRITE;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;PURGE;PERFORM;PAGE-COUNTER;OPEN;NULL;NEXT SENTENCE;MULTIPLY;MOVE;MERGE;LINE-COUNTER;LINAGE-COUNTER;INVOKE;INSPECT;INITIATE;INITIALIZE;IF;GOBACK;GO;GENERATE;FUNCTION;FREE;FOREVER;EXIT;EXCEPTION-OBJECT;EVALUATE;ENTRY;ENTER;END-PERFORM;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;CONTINUE;COMPUTE;CLOSE;CANCEL;CALL;ALTER;ALLOCATE;ADDRESS OF;ADD;ACCEPT;DATA-NAME;VAR;SECOND-SECTION;FIRST-SECTION;FIRST-PARAGRAPH IN FIRST-SECTION;FIRST-PARAGRAPH] |}]

let%expect_test "qualified-data-ref-completion" =
  let end_with_postproc = completion_positions @@ extract_position_markers {cobol|
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
    List of completions (23 entries): [ZEROS;SUPER;SPACES;SELF;QUOTES;PAGE-COUNTER;NULL;LOW-VALUES;LINE-COUNTER;LINAGE-COUNTER;HIGH-VALUES;FUNCTION;EXCEPTION-OBJECT;ALL;ADDRESS OF;AA;CC IN AA;CC;DD IN CC IN AA;DD;BB;CC IN BB;CC] |}]

let%expect_test "procedure-completion" =
  let end_with_postproc = completion_positions @@ extract_position_markers {cobol|
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
    List of completions (62 entries): [WRITE;VALIDATE;UNSTRING;UNLOCK;TRANSFORM;TERMINATE;SUPPRESS;SUBTRACT;STRING;STOP;START;SORT;SET;SEND;SEARCH;REWRITE;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;PURGE;PROGRAM-ID;.;PERFORM;OPEN;NEXT SENTENCE;MULTIPLY;MOVE;MERGE;INVOKE;INSPECT;INITIATE;INITIALIZE;IF;IDENTIFICATION DIVISION.;ID DIVISION.;GOBACK;GO;GENERATE;FREE;EXIT;EVALUATE;ENTRY;ENTER;END PROGRAM;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;DECLARATIVES.;CONTINUE;COMPUTE;CLOSE;CANCEL;CALL;ALTER;ALLOCATE;ADD;ACCEPT]
    __rootdir__/prog.cob:9.21:
       6           01 AA PIC X.
       7           01 BB PIC X.
       8           PROCEDURE DIVISION.
       9 >           DISPLAY AA .
    ----                        ^
      10             MOVE AA TO BB.
      11             MULTIPLY 4 BY 2 GIVING BB
    (line 8, character 21):
    List of completions (105 entries): [ZEROS;WRITE;WITH NO ADVANCING;WITH;VALIDATE;UPON;UNSTRING;UNLOCK;UNDERLINE;TRANSFORM;TERMINATE;SUPPRESS;SUPER;SUBTRACT;STRING;STOP;START;SPACES;SORT;SIZE;SET;SEND;SELF;SEARCH;REWRITE;REVERSE-VIDEO;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;QUOTES;PURGE;POSITION;.;PERFORM;PAGE-COUNTER;OVERLINE;OPEN;ON EXCEPTION;OF;NULL;NOT ON EXCEPTION;NEXT SENTENCE;MULTIPLY;MOVE;MODE;MERGE;LOW-VALUES;LOWLIGHT;LINE-COUNTER;LINE;LINAGE-COUNTER;LEFTLINE;INVOKE;INSPECT;INITIATE;INITIALIZE;IN;IF;HIGH-VALUES;HIGHLIGHT;GRID;GOBACK;GO;GENERATE;FUNCTION;FREE;FOREGROUND-COLOR;EXIT;EXCEPTION-OBJECT;EXCEPTION;EVALUATE;ERASE;ENTRY;ENTER;END-DISPLAY;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;CONTROL;CONTINUE;COMPUTE;COLUMN;COL;CLOSE;CANCEL;CALL;BLINK;BLANK;BELL;BACKGROUND-COLOR;AT;AS;ALTER;ALLOCATE;ALL;ADDRESS OF;ADD;ACCEPT;AA;BB]
    __rootdir__/prog.cob:10.15:
       7           01 BB PIC X.
       8           PROCEDURE DIVISION.
       9             DISPLAY AA .
      10 >           MOVE AA TO BB.
    ----                  ^
      11             MULTIPLY 4 BY 2 GIVING BB
      12               ON SIZE ERROR
    (line 9, character 15):
    List of completions (18 entries): [ZEROS;SUPER;SPACES;SELF;QUOTES;PAGE-COUNTER;NULL;LOW-VALUES;LINE-COUNTER;LINAGE-COUNTER;HIGH-VALUES;FUNCTION;EXCEPTION-OBJECT;CORRESPONDING;ALL;ADDRESS OF;AA;BB]
    __rootdir__/prog.cob:10.18:
       7           01 BB PIC X.
       8           PROCEDURE DIVISION.
       9             DISPLAY AA .
      10 >           MOVE AA TO BB.
    ----                     ^
      11             MULTIPLY 4 BY 2 GIVING BB
      12               ON SIZE ERROR
    (line 9, character 18):
    List of completions (4 entries): [TO;OF;IN;AS]
    __rootdir__/prog.cob:10.21:
       7           01 BB PIC X.
       8           PROCEDURE DIVISION.
       9             DISPLAY AA .
      10 >           MOVE AA TO BB.
    ----                        ^
      11             MULTIPLY 4 BY 2 GIVING BB
      12               ON SIZE ERROR
    (line 9, character 21):
    List of completions (11 entries): [SUPER;SELF;PAGE-COUNTER;NULL;LINE-COUNTER;LINAGE-COUNTER;FUNCTION;EXCEPTION-OBJECT;ADDRESS OF;AA;BB]
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
    List of completions (4 entries): [OF;IN;BY;AS]
    __rootdir__/prog.cob:15.28:
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15 >           MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
    ----                               ^
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
    (line 14, character 28):
    List of completions (76 entries): [WRITE;VALIDATE;UNSTRING;UNLOCK;TRANSFORM;TERMINATE;SUPPRESS;SUPER;SUBTRACT;STRING;STOP;START;SORT;SET;SEND;SELF;SEARCH;ROUNDED;REWRITE;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;PURGE;.;PERFORM;PAGE-COUNTER;OPEN;ON SIZE ERROR;OF;NULL;NOT ON SIZE ERROR;NEXT SENTENCE;MULTIPLY;MOVE;MERGE;LINE-COUNTER;LINAGE-COUNTER;INVOKE;INSPECT;INITIATE;INITIALIZE;IN;IF;GOBACK;GO;GIVING;GENERATE;FUNCTION;FREE;EXIT;EXCEPTION-OBJECT;EVALUATE;ENTRY;ENTER;END-MULTIPLY;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;CONTINUE;COMPUTE;CLOSE;CANCEL;CALL;AS;ALTER;ALLOCATE;ADDRESS OF;ADD;ACCEPT;AA;BB]
    __rootdir__/prog.cob:15.36:
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15 >           MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
    ----                                       ^
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
    (line 14, character 36):
    List of completions (72 entries): [WRITE;VALIDATE;UNSTRING;UNLOCK;TRANSFORM;TERMINATE;SUPPRESS;SUPER;SUBTRACT;STRING;STOP;START;SORT;SET;SEND;SELF;SEARCH;REWRITE;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;PURGE;.;PERFORM;PAGE-COUNTER;OPEN;ON SIZE ERROR;NULL;NOT ON SIZE ERROR;NEXT SENTENCE;MULTIPLY;MOVE;MODE;MERGE;LINE-COUNTER;LINAGE-COUNTER;INVOKE;INSPECT;INITIATE;INITIALIZE;IF;GOBACK;GO;GENERATE;FUNCTION;FREE;EXIT;EXCEPTION-OBJECT;EVALUATE;ENTRY;ENTER;END-MULTIPLY;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;CONTINUE;COMPUTE;CLOSE;CANCEL;CALL;ALTER;ALLOCATE;ADDRESS OF;ADD;ACCEPT;AA;BB]
    __rootdir__/prog.cob:15.41:
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15 >           MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
    ----                                            ^
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
    (line 14, character 41):
    List of completions (9 entries): [TRUNCATION;TOWARD-LESSER;TOWARD-GREATER;PROHIBITED;NEAREST-TOWARD-ZERO;NEAREST-EVEN;NEAREST-AWAY-FROM-ZERO;IS;AWAY-FROM-ZERO]
    __rootdir__/prog.cob:15.44:
      12               ON SIZE ERROR
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15 >           MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
    ----                                               ^
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
    (line 14, character 44):
    List of completions (8 entries): [TRUNCATION;TOWARD-LESSER;TOWARD-GREATER;PROHIBITED;NEAREST-TOWARD-ZERO;NEAREST-EVEN;NEAREST-AWAY-FROM-ZERO;AWAY-FROM-ZERO]
    __rootdir__/prog.cob:16.12:
      13               DISPLAY "ERROR"
      14             END-MULTIPLY.
      15             MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
      16 >             ON SIZE ERROR
    ----               ^
      17               DISPLAY "ERROR"
      18             END-MULTIPLY.
    (line 15, character 12):
    List of completions (71 entries): [WRITE;VALIDATE;UNSTRING;UNLOCK;TRANSFORM;TERMINATE;SUPPRESS;SUPER;SUBTRACT;STRING;STOP;START;SORT;SET;SEND;SELF;SEARCH;REWRITE;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;PURGE;.;PERFORM;PAGE-COUNTER;OPEN;ON SIZE ERROR;NULL;NOT ON SIZE ERROR;NEXT SENTENCE;MULTIPLY;MOVE;MERGE;LINE-COUNTER;LINAGE-COUNTER;INVOKE;INSPECT;INITIATE;INITIALIZE;IF;GOBACK;GO;GENERATE;FUNCTION;FREE;EXIT;EXCEPTION-OBJECT;EVALUATE;ENTRY;ENTER;END-MULTIPLY;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;CONTINUE;COMPUTE;CLOSE;CANCEL;CALL;ALTER;ALLOCATE;ADDRESS OF;ADD;ACCEPT;AA;BB]
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
    List of completions (56 entries): [WRITE;VALIDATE;UNSTRING;UNLOCK;TRANSFORM;TERMINATE;SUPPRESS;SUBTRACT;STRING;STOP;START;SORT;SET;SEND;SEARCH;REWRITE;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;PURGE;PERFORM;OPEN;NEXT SENTENCE;MULTIPLY;MOVE;MERGE;INVOKE;INSPECT;INITIATE;INITIALIZE;IF;GOBACK;GO;GENERATE;FREE;EXIT;EVALUATE;ENTRY;ENTER;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;CONTINUE;COMPUTE;CLOSE;CANCEL;CALL;ALTER;ALLOCATE;ADD;ACCEPT]
    __rootdir__/prog.cob:18.10:
      15             MULTIPLY AA BY BB ROUNDED MODE IS TRUNCATION
      16               ON SIZE ERROR
      17               DISPLAY "ERROR"
      18 >           END-MULTIPLY.
    ----             ^
      19             STOP RUN.
      20
    (line 17, character 10):
    List of completions (104 entries): [ZEROS;WRITE;WITH NO ADVANCING;WITH;VALIDATE;UPON;UNSTRING;UNLOCK;UNDERLINE;TRANSFORM;TERMINATE;SUPPRESS;SUPER;SUBTRACT;STRING;STOP;START;SPACES;SORT;SIZE;SET;SEND;SELF;SEARCH;REWRITE;REVERSE-VIDEO;RETURN;RESUME;RELEASE;RECEIVE;READ;RAISE;QUOTES;PURGE;POSITION;.;PERFORM;PAGE-COUNTER;OVERLINE;OPEN;ON EXCEPTION;NULL;NOT ON SIZE ERROR;NOT ON EXCEPTION;NEXT SENTENCE;MULTIPLY;MOVE;MODE;MERGE;LOW-VALUES;LOWLIGHT;LINE-COUNTER;LINE;LINAGE-COUNTER;LEFTLINE;INVOKE;INSPECT;INITIATE;INITIALIZE;IF;HIGH-VALUES;HIGHLIGHT;GRID;GOBACK;GO;GENERATE;FUNCTION;FREE;FOREGROUND-COLOR;EXIT;EXCEPTION-OBJECT;EXCEPTION;EVALUATE;ERASE;ENTRY;ENTER;END-MULTIPLY;END-DISPLAY;ENABLE;DIVIDE;DISPLAY;DISABLE;DELETE;CONTROL;CONTINUE;COMPUTE;COLUMN;COL;CLOSE;CANCEL;CALL;BLINK;BLANK;BELL;BACKGROUND-COLOR;AT;ALTER;ALLOCATE;ALL;ADDRESS OF;ADD;ACCEPT;AA;BB] |}]

(* Testing completion with nullable expected tokens (default_section_clauses) *)
let%expect_test "control-completion" =
  let end_with_postproc = completion_positions @@ extract_position_markers {cobol|
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
    List of completions (7 entries): [PROGRAM-ID;INTERFACE-ID.;IDENTIFICATION DIVISION.;ID DIVISION.;FUNCTION-ID;DEFAULT SECTION.;CLASS-ID.]
    __rootdir__/prog.cob:3.16:
       1
       2           CONTROL DIVISION.
       3 >         DEFAULT SECTION.
    ----                   ^
       4             ACCEPT TERMINAL
       5             DISPLAY IS TERMINAL.
    (line 2, character 16):
    List of completions (1 entries): [SECTION.]
    __rootdir__/prog.cob:4.10:
       1
       2           CONTROL DIVISION.
       3           DEFAULT SECTION.
       4 >           ACCEPT TERMINAL
    ----             ^
       5             DISPLAY IS TERMINAL.
       6           IDENTIFICATION DIVISION.
    (line 3, character 10):
    List of completions (9 entries): [PROGRAM-ID;.;INTERFACE-ID.;IDENTIFICATION DIVISION.;ID DIVISION.;FUNCTION-ID;DISPLAY;CLASS-ID.;ACCEPT]
    __rootdir__/prog.cob:4.17:
       1
       2           CONTROL DIVISION.
       3           DEFAULT SECTION.
       4 >           ACCEPT TERMINAL
    ----                    ^
       5             DISPLAY IS TERMINAL.
       6           IDENTIFICATION DIVISION.
    (line 3, character 17):
    List of completions (2 entries): [TERMINAL;IS]
    __rootdir__/prog.cob:5.10:
       2           CONTROL DIVISION.
       3           DEFAULT SECTION.
       4             ACCEPT TERMINAL
       5 >           DISPLAY IS TERMINAL.
    ----             ^
       6           IDENTIFICATION DIVISION.
       7           PROGRAM-ID. prog.
    (line 4, character 10):
    List of completions (2 entries): [.;DISPLAY]
    __rootdir__/prog.cob:6.8:
       3           DEFAULT SECTION.
       4             ACCEPT TERMINAL
       5             DISPLAY IS TERMINAL.
       6 >         IDENTIFICATION DIVISION.
    ----           ^
       7           PROGRAM-ID. prog.
       8           PROCEDURE DIVISION.
    (line 5, character 8):
    List of completions (6 entries): [PROGRAM-ID;INTERFACE-ID.;IDENTIFICATION DIVISION.;ID DIVISION.;FUNCTION-ID;CLASS-ID.] |}]

