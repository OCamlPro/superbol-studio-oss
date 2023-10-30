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

open Lsp.Types
open Lsp_testing

let make_free_format_project () =
  make_lsp_project () ~toml:{toml|
    [cobol]
    source-format = "free"
  |toml}

let format_doc doc =
  let { projdir; end_with_postproc }, server = make_free_format_project () in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  let params =
    let options = FormattingOptions.create ~insertSpaces:true ~tabSize:2 () in
    DocumentFormattingParams.create ~options ~textDocument:prog ()
  in
  let doc = (LSP.Types.URIMap.find prog.uri server.docs).textdoc in
  let formatted = LSP.Request.formatting server params in
  Option.map (fun edits ->
    Lsp.Text_document.apply_text_document_edits doc edits |> Lsp.Text_document.text
  ) formatted, end_with_postproc

let doc = {cobol|
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
       para-1.
       DISPLAY "HELLO"
       STOP RUN.
  |cobol};;

let%expect_test "simple-formatting-request" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
           PROGRAM-ID. HELLO.
           PROCEDURE DIVISION.
            para-1.
                DISPLAY "HELLO"
                STOP RUN. |}]


let doc = {cobol|
        para-1.
        IF X>9
        THEN
        IF X>6
        THEN
        DISPLAY "2"
        else
        move 1 to x
        else
        move 1 to x.  |cobol};;

let%expect_test "formatting-request-nested-if" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":14,"line":1},"start":{"character":8,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
            para-1.
                IF X>9
                THEN
                    IF X>6
                    THEN
                        DISPLAY "2"
                    else
                        move 1 to x
                else
                    move 1 to x. |}]


let doc = {cobol|
        WORKING-STORAGE SECTION.
        01 x.
        05 y.
            10 z pic 999.
          05 h pic 99.
        66 z renames x.
        01 X1
        OCCURS 3 times
        DEPENDING ON X2
        ASCENDING KEY is X3
        INDEXED BY X4
        value 999.              |cobol};;

let%expect_test "formatting-request-data" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect{|
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":23,"line":1},"start":{"character":8,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
            WORKING-STORAGE SECTION.
            01 x.
                05 y.
                    10 z pic 999.
                05 h pic 99.
                66 z renames x.
            01 X1
                OCCURS 3 times
                    DEPENDING ON X2
                    ASCENDING KEY is X3
                    INDEXED BY X4
                value 999. |}]


let doc = {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. X.
        PROCEDURE DIVISION.
        DISPLAY "I'm in X"
        CALL "X1"
        CALL "X2"
        STOP RUN.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. X1.
        PROCEDURE DIVISION.
        DISPLAY "I'm in X1"
        CALL "X11"
        CALL "X12"
        EXIT Program.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. X11.
        PROCEDURE DIVISION.
        DISPLAY "I'm in X11"
        EXIT Program.
        END PROGRAM X11.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. X12.
        PROCEDURE DIVISION.
        DISPLAY "I'm in X12"
        EXIT Program.
        END PROGRAM X12.

        END PROGRAM X1.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. X2.
        PROCEDURE DIVISION.
        DISPLAY "I'm in X2"
        EXIT Program.
        END PROGRAM X2.

        END PROGRAM X.
        |cobol};;

let%expect_test "formatting-request-nested-program" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect{|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
            IDENTIFICATION DIVISION.
            PROGRAM-ID. X.
            PROCEDURE DIVISION.
                 DISPLAY "I'm in X"
                 CALL "X1"
                 CALL "X2"
                 STOP RUN.
             IDENTIFICATION DIVISION.
             PROGRAM-ID. X1.
             PROCEDURE DIVISION.
                  DISPLAY "I'm in X1"
                  CALL "X11"
                  CALL "X12"
                  EXIT Program.
              IDENTIFICATION DIVISION.
              PROGRAM-ID. X11.
              PROCEDURE DIVISION.
                   DISPLAY "I'm in X11"
                   EXIT Program.
              END PROGRAM X11.
              IDENTIFICATION DIVISION.
              PROGRAM-ID. X12.
              PROCEDURE DIVISION.
                   DISPLAY "I'm in X12"
                   EXIT Program.
              END PROGRAM X12.
             END PROGRAM X1.
             IDENTIFICATION DIVISION.
             PROGRAM-ID. X2.
             PROCEDURE DIVISION.
                  DISPLAY "I'm in X2"
                  EXIT Program.
             END PROGRAM X2.
            END PROGRAM X. |}]


let doc = {cobol|
       MOVE VAR-1 TO VAR-2 VAR-3
       VAR-4
       VAR-5.
  |cobol};;

let%expect_test "formatting-request-alignment-argument" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":11,"line":1},"start":{"character":7,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
           MOVE VAR-1 TO VAR-2 VAR-3
                         VAR-4
                         VAR-5. |}]


let doc = {cobol|
        if x>1
        move 1 to x
        else if x>2
        move 2 to x
        else if x>3
        move 3 to x
        else
        move 4 to x.
  |cobol};;

let%expect_test "formatting-request-else-if" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":10,"line":1},"start":{"character":8,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
            if x>1
                move 1 to x
            else if x>2
                move 2 to x
            else if x>3
                move 3 to x
            else
                move 4 to x. |}]


let doc = {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. MACESDS.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT MACC ASSIGN TO RRDSFILE
        ORGANIZATION RELATIVE
        ACCESS MODE DYNAMIC
        RELATIVE KEY RK
        FILE STATUS FS.
        DATA DIVISION.
        FILE SECTION.
        FD MACC.
        01 MREC.
        05 MNO PIC 9(5).
        05 MNAME PIC X(10).
        WORKING-STORAGE SECTION.
        01 FS PIC X(2).
        01 A PIC 99 VALUE 00.
        01 B PIC 9(5) VALUE ZERO.
        01 IREC.
        05 INO PIC 9(5).
        05 INAME PIC X(10).
        01 RK PIC 9(02) VALUE 01.
        PROCEDURE DIVISION.
        0001.
        DISPLAY "ENTER 1.SEAR/2.WRITE/3.REWR/4.DEL/5.DELALL/6.DISP".
        ACCEPT A.
        IF A = 1 GO 1SEARCH
        ELSE IF A = 2 GO 2WRITE
        ELSE IF A = 3 GO 3REWRITE
        ELSE IF A = 4 GO 4DELETE
        ELSE IF A = 5 GO 5DELALL
        ELSE IF A = 6 GO 6DISPLAY
        ELSE DISPLAY "INVALID INPUT"
        GO 0001.
        STOP RUN.
        1SEARCH.
        OPEN INPUT MACC.
        ACCEPT B.
        0002.
        READ MACC NEXT AT END DISPLAY B "NOT FOUND", GO 000X.
        IF B = MNO DISPLAY "FOUND " MNO ":" ,
        DISPLAY " AT POS:" A " FOR NAME: " MNAME,
        GO 000X.
        ADD 1 TO A.
        GO TO 0002.
        2WRITE.
        OPEN I-O MACC.
        IF FS = 00 PERFORM RKKEY UNTIL FS = 10
        ELSE OPEN OUTPUT MACC.
        DISPLAY RK.
        ACCEPT MNO.
        ACCEPT MNAME.
        WRITE MREC INVALID KEY DISPLAY "DUPLICATE KEY!".
        GO 000X.
        3REWRITE.
        OPEN I-O MACC.
        ACCEPT RK.
        ACCEPT MNO.
        ACCEPT MNAME.
        REWRITE MREC INVALID KEY DISPLAY "NOT FOUND".
        GO 000X.
        4DELETE.
        OPEN I-O MACC.
        ACCEPT RK.
        DELETE MACC INVALID KEY DISPLAY "NOT FOUND".
        GO 000X.
        5DELALL.
        OPEN I-O MACC.
        MOVE 01 TO RK.
        0003.
        DELETE MACC INVALID KEY GO 000X.
        ADD 01 TO RK.
        GO 0003.
        6DISPLAY.
        OPEN INPUT MACC.
        0005.
        READ MACC NEXT INTO IREC AT END GO 000X.
        DISPLAY INO, " ", INAME.
        GO 0005.
        000X.
        CLOSE MACC.
        DISPLAY "CONTINUE?1/0".
        ACCEPT A.
        IF A = 0 STOP RUN ELSE GO 0001.
        RKKEY.
        READ MACC NEXT.
        ADD 1 TO RK.
  |cobol};;

let%expect_test "formatting-request-whole-program" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect {|
    misc_sections_visitor.ml:0:
      (Cobol_ptree__Misc_sections_visitor.fold_select_clause): missing visitor
      implementation
    data_sections_visitor.ml:0:
      (Cobol_ptree__Data_sections_visitor.fold_file_section): missing visitor
      implementation
    data_sections_visitor.ml:0:
      (Cobol_ptree__Data_sections_visitor.fold_data_clause): partial visitor
      implementation
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
            IDENTIFICATION DIVISION.
            PROGRAM-ID. MACESDS.
            ENVIRONMENT DIVISION.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT MACC ASSIGN TO RRDSFILE
                       ORGANIZATION RELATIVE
                       ACCESS MODE DYNAMIC
                       RELATIVE KEY RK
                       FILE STATUS FS.
            DATA DIVISION.
            FILE SECTION.
            FD MACC.
            01 MREC.
                05 MNO PIC 9(5).
                05 MNAME PIC X(10).
            WORKING-STORAGE SECTION.
            01 FS PIC X(2).
            01 A PIC 99 VALUE 00.
            01 B PIC 9(5) VALUE ZERO.
            01 IREC.
                05 INO PIC 9(5).
                05 INAME PIC X(10).
            01 RK PIC 9(02) VALUE 01.
            PROCEDURE DIVISION.
             0001.
                 DISPLAY "ENTER 1.SEAR/2.WRITE/3.REWR/4.DEL/5.DELALL/6.DISP".
                 ACCEPT A.
                 IF A = 1 GO 1SEARCH
                 ELSE IF A = 2 GO 2WRITE
                 ELSE IF A = 3 GO 3REWRITE
                 ELSE IF A = 4 GO 4DELETE
                 ELSE IF A = 5 GO 5DELALL
                 ELSE IF A = 6 GO 6DISPLAY
                 ELSE DISPLAY "INVALID INPUT"
                     GO 0001.
                 STOP RUN.
             1SEARCH.
                 OPEN INPUT MACC.
                 ACCEPT B.
             0002.
                 READ MACC NEXT AT END DISPLAY B "NOT FOUND", GO 000X.
                 IF B = MNO DISPLAY "FOUND " MNO ":" ,
                     DISPLAY " AT POS:" A " FOR NAME: " MNAME,
                     GO 000X.
                 ADD 1 TO A.
                 GO TO 0002.
             2WRITE.
                 OPEN I-O MACC.
                 IF FS = 00 PERFORM RKKEY UNTIL FS = 10
                 ELSE OPEN OUTPUT MACC.
                 DISPLAY RK.
                 ACCEPT MNO.
                 ACCEPT MNAME.
                 WRITE MREC INVALID KEY DISPLAY "DUPLICATE KEY!".
                 GO 000X.
             3REWRITE.
                 OPEN I-O MACC.
                 ACCEPT RK.
                 ACCEPT MNO.
                 ACCEPT MNAME.
                 REWRITE MREC INVALID KEY DISPLAY "NOT FOUND".
                 GO 000X.
             4DELETE.
                 OPEN I-O MACC.
                 ACCEPT RK.
                 DELETE MACC INVALID KEY DISPLAY "NOT FOUND".
                 GO 000X.
             5DELALL.
                 OPEN I-O MACC.
                 MOVE 01 TO RK.
             0003.
                 DELETE MACC INVALID KEY GO 000X.
                 ADD 01 TO RK.
                 GO 0003.
             6DISPLAY.
                 OPEN INPUT MACC.
             0005.
                 READ MACC NEXT INTO IREC AT END GO 000X.
                 DISPLAY INO, " ", INAME.
                 GO 0005.
             000X.
                 CLOSE MACC.
                 DISPLAY "CONTINUE?1/0".
                 ACCEPT A.
                 IF A = 0 STOP RUN ELSE GO 0001.
             RKKEY.
                 READ MACC NEXT.
                 ADD 1 TO RK. |}]



let doc = {cobol|
       CALL STH
       NOT ON EXCEPTION
       RAISE EXCEPTION exception-name-1
       EXCEPTION
       DISPLAY "ERROR"
       END-CALL.
  |cobol};;

let%expect_test "formatting-request-on-exception" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect{|
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":11,"line":1},"start":{"character":7,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
           CALL STH
               NOT ON EXCEPTION
                   RAISE EXCEPTION exception-name-1
               EXCEPTION
                   DISPLAY "ERROR"
           END-CALL. |}]


let doc = {cobol|
       PROCEDURE DIVISION.
       para-1.
       PERFORM 3 TIMES
       PERFORM PARA-2
       END-PERFORM
       STOP RUN.
       PARA-2.
       DISPLAY "HELLO".
  |cobol};;

let%expect_test "formatting-request-perform" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":16,"line":1},"start":{"character":7,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
           PROCEDURE DIVISION.
            para-1.
                PERFORM 3 TIMES
                    PERFORM PARA-2
                END-PERFORM
                STOP RUN.
            PARA-2.
                DISPLAY "HELLO". |}]
