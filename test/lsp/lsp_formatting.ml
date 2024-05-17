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
  let _doc = (LSP.Types.URIMap.find prog.uri server.docs).textdoc in
  let formatted = LSP.Request.formatting server params in
  Option.map (fun edits ->
      let edits = List.map TextEdit.yojson_of_t edits in
      Format.pp_print_list
        (fun fmt elt -> Format.fprintf fmt "%s" (Yojson.Safe.to_string elt))
        Format.str_formatter edits;
      Format.flush_str_formatter ()
    (* Lsp.Text_document.apply_text_document_edits doc edits |> Lsp.Text_document.text *)
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
    {"newText":"","range":{"end":{"character":7,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":7,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"","range":{"end":{"character":7,"line":3},"start":{"character":0,"line":3}}}
    {"newText":"","range":{"end":{"character":3,"line":4},"start":{"character":0,"line":4}}}
    {"newText":"","range":{"end":{"character":3,"line":5},"start":{"character":0,"line":5}}} |}]

let doc = {cobol|
       PROGRAM-ID. HELLO.
                   PROCEDURE DIVISION.
                                   para-1.
                                                                   DISPLAY "HELLO"
                                                                                       STOP RUN.
  |cobol};;

let%expect_test "unindent-formatting-request" =
  let doc', end_with_postproc = format_doc doc in
  begin match doc' with
  | None -> Pretty.out "formatting error"
  | Some doc' -> Pretty.out "%s" doc'
  end;
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"newText":"","range":{"end":{"character":7,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":19,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"","range":{"end":{"character":35,"line":3},"start":{"character":0,"line":3}}}
    {"newText":"","range":{"end":{"character":63,"line":4},"start":{"character":0,"line":4}}}
    {"newText":"","range":{"end":{"character":83,"line":5},"start":{"character":0,"line":5}}} |}]


let doc = {cobol|
       PROCEDURE DIVISION.
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
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":16,"line":1},"start":{"character":7,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"newText":"","range":{"end":{"character":7,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":8,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"","range":{"end":{"character":4,"line":3},"start":{"character":0,"line":3}}}
    {"newText":"","range":{"end":{"character":4,"line":4},"start":{"character":0,"line":4}}}
    {"newText":"","range":{"end":{"character":1,"line":5},"start":{"character":0,"line":5}}}
    {"newText":"","range":{"end":{"character":1,"line":6},"start":{"character":0,"line":6}}}
    {"newText":"  ","range":{"end":{"character":0,"line":7},"start":{"character":0,"line":7}}}
    {"newText":"","range":{"end":{"character":1,"line":8},"start":{"character":0,"line":8}}}
    {"newText":"  ","range":{"end":{"character":0,"line":9},"start":{"character":0,"line":9}}}
    {"newText":"","range":{"end":{"character":4,"line":10},"start":{"character":0,"line":10}}}
    {"newText":"","range":{"end":{"character":1,"line":11},"start":{"character":0,"line":11}}} |}]


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
    {"newText":"","range":{"end":{"character":8,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":4,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"","range":{"end":{"character":4,"line":3},"start":{"character":0,"line":3}}}
    {"newText":"","range":{"end":{"character":8,"line":4},"start":{"character":0,"line":4}}}
    {"newText":"","range":{"end":{"character":6,"line":5},"start":{"character":0,"line":5}}}
    {"newText":"","range":{"end":{"character":4,"line":6},"start":{"character":0,"line":6}}}
    {"newText":"","range":{"end":{"character":4,"line":7},"start":{"character":0,"line":7}}}
    {"newText":"","range":{"end":{"character":1,"line":8},"start":{"character":0,"line":8}}}
    {"newText":"","range":{"end":{"character":1,"line":9},"start":{"character":0,"line":9}}}
    {"newText":"","range":{"end":{"character":1,"line":10},"start":{"character":0,"line":10}}}
    {"newText":"","range":{"end":{"character":1,"line":11},"start":{"character":0,"line":11}}}
    {"newText":"","range":{"end":{"character":1,"line":12},"start":{"character":0,"line":12}}} |}]


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
    {"newText":"","range":{"end":{"character":8,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":8,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"","range":{"end":{"character":8,"line":3},"start":{"character":0,"line":3}}}
    {"newText":"","range":{"end":{"character":4,"line":4},"start":{"character":0,"line":4}}}
    {"newText":"","range":{"end":{"character":4,"line":5},"start":{"character":0,"line":5}}}
    {"newText":"","range":{"end":{"character":4,"line":6},"start":{"character":0,"line":6}}}
    {"newText":"","range":{"end":{"character":4,"line":7},"start":{"character":0,"line":7}}}
    {"newText":"","range":{"end":{"character":8,"line":9},"start":{"character":0,"line":9}}}
    {"newText":"","range":{"end":{"character":8,"line":10},"start":{"character":0,"line":10}}}
    {"newText":"","range":{"end":{"character":8,"line":11},"start":{"character":0,"line":11}}}
    {"newText":"","range":{"end":{"character":4,"line":12},"start":{"character":0,"line":12}}}
    {"newText":"","range":{"end":{"character":4,"line":13},"start":{"character":0,"line":13}}}
    {"newText":"","range":{"end":{"character":4,"line":14},"start":{"character":0,"line":14}}}
    {"newText":"","range":{"end":{"character":4,"line":15},"start":{"character":0,"line":15}}}
    {"newText":"","range":{"end":{"character":8,"line":17},"start":{"character":0,"line":17}}}
    {"newText":"","range":{"end":{"character":8,"line":18},"start":{"character":0,"line":18}}}
    {"newText":"","range":{"end":{"character":8,"line":19},"start":{"character":0,"line":19}}}
    {"newText":"","range":{"end":{"character":4,"line":20},"start":{"character":0,"line":20}}}
    {"newText":"","range":{"end":{"character":4,"line":21},"start":{"character":0,"line":21}}}
    {"newText":"","range":{"end":{"character":8,"line":22},"start":{"character":0,"line":22}}}
    {"newText":"","range":{"end":{"character":8,"line":24},"start":{"character":0,"line":24}}}
    {"newText":"","range":{"end":{"character":8,"line":25},"start":{"character":0,"line":25}}}
    {"newText":"","range":{"end":{"character":8,"line":26},"start":{"character":0,"line":26}}}
    {"newText":"","range":{"end":{"character":4,"line":27},"start":{"character":0,"line":27}}}
    {"newText":"","range":{"end":{"character":4,"line":28},"start":{"character":0,"line":28}}}
    {"newText":"","range":{"end":{"character":8,"line":29},"start":{"character":0,"line":29}}}
    {"newText":"","range":{"end":{"character":8,"line":31},"start":{"character":0,"line":31}}}
    {"newText":"","range":{"end":{"character":8,"line":33},"start":{"character":0,"line":33}}}
    {"newText":"","range":{"end":{"character":8,"line":34},"start":{"character":0,"line":34}}}
    {"newText":"","range":{"end":{"character":8,"line":35},"start":{"character":0,"line":35}}}
    {"newText":"","range":{"end":{"character":4,"line":36},"start":{"character":0,"line":36}}}
    {"newText":"","range":{"end":{"character":4,"line":37},"start":{"character":0,"line":37}}}
    {"newText":"","range":{"end":{"character":8,"line":38},"start":{"character":0,"line":38}}}
    {"newText":"","range":{"end":{"character":8,"line":40},"start":{"character":0,"line":40}}} |}]


let doc = {cobol|
       PROCEDURE DIVISION.
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
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":16,"line":1},"start":{"character":7,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"newText":"","range":{"end":{"character":7,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":3,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"           ","range":{"end":{"character":0,"line":3},"start":{"character":0,"line":3}}}
    {"newText":"           ","range":{"end":{"character":0,"line":4},"start":{"character":0,"line":4}}} |}]


let doc = {cobol|
       PROCEDURE DIVISION.
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
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":16,"line":1},"start":{"character":7,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"newText":"","range":{"end":{"character":7,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":4,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"","range":{"end":{"character":1,"line":3},"start":{"character":0,"line":3}}}
    {"newText":"","range":{"end":{"character":4,"line":4},"start":{"character":0,"line":4}}}
    {"newText":"","range":{"end":{"character":1,"line":5},"start":{"character":0,"line":5}}}
    {"newText":"","range":{"end":{"character":4,"line":6},"start":{"character":0,"line":6}}}
    {"newText":"","range":{"end":{"character":1,"line":7},"start":{"character":0,"line":7}}}
    {"newText":"","range":{"end":{"character":4,"line":8},"start":{"character":0,"line":8}}}
    {"newText":"","range":{"end":{"character":1,"line":9},"start":{"character":0,"line":9}}} |}]


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
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"newText":"","range":{"end":{"character":8,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":8,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"","range":{"end":{"character":8,"line":3},"start":{"character":0,"line":3}}}
    {"newText":"","range":{"end":{"character":8,"line":4},"start":{"character":0,"line":4}}}
    {"newText":"","range":{"end":{"character":8,"line":5},"start":{"character":0,"line":5}}}
    {"newText":"","range":{"end":{"character":4,"line":6},"start":{"character":0,"line":6}}}
    {"newText":"   ","range":{"end":{"character":0,"line":7},"start":{"character":0,"line":7}}}
    {"newText":"   ","range":{"end":{"character":0,"line":8},"start":{"character":0,"line":8}}}
    {"newText":"   ","range":{"end":{"character":0,"line":9},"start":{"character":0,"line":9}}}
    {"newText":"   ","range":{"end":{"character":0,"line":10},"start":{"character":0,"line":10}}}
    {"newText":"","range":{"end":{"character":8,"line":11},"start":{"character":0,"line":11}}}
    {"newText":"","range":{"end":{"character":8,"line":12},"start":{"character":0,"line":12}}}
    {"newText":"","range":{"end":{"character":8,"line":13},"start":{"character":0,"line":13}}}
    {"newText":"","range":{"end":{"character":8,"line":14},"start":{"character":0,"line":14}}}
    {"newText":"","range":{"end":{"character":5,"line":15},"start":{"character":0,"line":15}}}
    {"newText":"","range":{"end":{"character":5,"line":16},"start":{"character":0,"line":16}}}
    {"newText":"","range":{"end":{"character":8,"line":17},"start":{"character":0,"line":17}}}
    {"newText":"","range":{"end":{"character":8,"line":18},"start":{"character":0,"line":18}}}
    {"newText":"","range":{"end":{"character":8,"line":19},"start":{"character":0,"line":19}}}
    {"newText":"","range":{"end":{"character":8,"line":20},"start":{"character":0,"line":20}}}
    {"newText":"","range":{"end":{"character":8,"line":21},"start":{"character":0,"line":21}}}
    {"newText":"","range":{"end":{"character":5,"line":22},"start":{"character":0,"line":22}}}
    {"newText":"","range":{"end":{"character":5,"line":23},"start":{"character":0,"line":23}}}
    {"newText":"","range":{"end":{"character":8,"line":24},"start":{"character":0,"line":24}}}
    {"newText":"","range":{"end":{"character":8,"line":25},"start":{"character":0,"line":25}}}
    {"newText":"","range":{"end":{"character":8,"line":26},"start":{"character":0,"line":26}}}
    {"newText":"","range":{"end":{"character":4,"line":27},"start":{"character":0,"line":27}}}
    {"newText":"","range":{"end":{"character":4,"line":28},"start":{"character":0,"line":28}}}
    {"newText":"","range":{"end":{"character":4,"line":29},"start":{"character":0,"line":29}}}
    {"newText":"","range":{"end":{"character":4,"line":30},"start":{"character":0,"line":30}}}
    {"newText":"","range":{"end":{"character":4,"line":31},"start":{"character":0,"line":31}}}
    {"newText":"","range":{"end":{"character":4,"line":32},"start":{"character":0,"line":32}}}
    {"newText":"","range":{"end":{"character":4,"line":33},"start":{"character":0,"line":33}}}
    {"newText":"","range":{"end":{"character":4,"line":34},"start":{"character":0,"line":34}}}
    {"newText":"","range":{"end":{"character":4,"line":35},"start":{"character":0,"line":35}}}
    {"newText":"","range":{"end":{"character":1,"line":36},"start":{"character":0,"line":36}}}
    {"newText":"","range":{"end":{"character":4,"line":37},"start":{"character":0,"line":37}}}
    {"newText":"","range":{"end":{"character":8,"line":38},"start":{"character":0,"line":38}}}
    {"newText":"","range":{"end":{"character":4,"line":39},"start":{"character":0,"line":39}}}
    {"newText":"","range":{"end":{"character":4,"line":40},"start":{"character":0,"line":40}}}
    {"newText":"","range":{"end":{"character":8,"line":41},"start":{"character":0,"line":41}}}
    {"newText":"","range":{"end":{"character":4,"line":42},"start":{"character":0,"line":42}}}
    {"newText":"","range":{"end":{"character":4,"line":43},"start":{"character":0,"line":43}}}
    {"newText":"","range":{"end":{"character":1,"line":44},"start":{"character":0,"line":44}}}
    {"newText":"","range":{"end":{"character":1,"line":45},"start":{"character":0,"line":45}}}
    {"newText":"","range":{"end":{"character":4,"line":46},"start":{"character":0,"line":46}}}
    {"newText":"","range":{"end":{"character":4,"line":47},"start":{"character":0,"line":47}}}
    {"newText":"","range":{"end":{"character":8,"line":48},"start":{"character":0,"line":48}}}
    {"newText":"","range":{"end":{"character":4,"line":49},"start":{"character":0,"line":49}}}
    {"newText":"","range":{"end":{"character":4,"line":50},"start":{"character":0,"line":50}}}
    {"newText":"","range":{"end":{"character":4,"line":51},"start":{"character":0,"line":51}}}
    {"newText":"","range":{"end":{"character":4,"line":52},"start":{"character":0,"line":52}}}
    {"newText":"","range":{"end":{"character":4,"line":53},"start":{"character":0,"line":53}}}
    {"newText":"","range":{"end":{"character":4,"line":54},"start":{"character":0,"line":54}}}
    {"newText":"","range":{"end":{"character":4,"line":55},"start":{"character":0,"line":55}}}
    {"newText":"","range":{"end":{"character":4,"line":56},"start":{"character":0,"line":56}}}
    {"newText":"","range":{"end":{"character":8,"line":57},"start":{"character":0,"line":57}}}
    {"newText":"","range":{"end":{"character":4,"line":58},"start":{"character":0,"line":58}}}
    {"newText":"","range":{"end":{"character":4,"line":59},"start":{"character":0,"line":59}}}
    {"newText":"","range":{"end":{"character":4,"line":60},"start":{"character":0,"line":60}}}
    {"newText":"","range":{"end":{"character":4,"line":61},"start":{"character":0,"line":61}}}
    {"newText":"","range":{"end":{"character":4,"line":62},"start":{"character":0,"line":62}}}
    {"newText":"","range":{"end":{"character":4,"line":63},"start":{"character":0,"line":63}}}
    {"newText":"","range":{"end":{"character":8,"line":64},"start":{"character":0,"line":64}}}
    {"newText":"","range":{"end":{"character":4,"line":65},"start":{"character":0,"line":65}}}
    {"newText":"","range":{"end":{"character":4,"line":66},"start":{"character":0,"line":66}}}
    {"newText":"","range":{"end":{"character":4,"line":67},"start":{"character":0,"line":67}}}
    {"newText":"","range":{"end":{"character":4,"line":68},"start":{"character":0,"line":68}}}
    {"newText":"","range":{"end":{"character":8,"line":69},"start":{"character":0,"line":69}}}
    {"newText":"","range":{"end":{"character":4,"line":70},"start":{"character":0,"line":70}}}
    {"newText":"","range":{"end":{"character":4,"line":71},"start":{"character":0,"line":71}}}
    {"newText":"","range":{"end":{"character":8,"line":72},"start":{"character":0,"line":72}}}
    {"newText":"","range":{"end":{"character":4,"line":73},"start":{"character":0,"line":73}}}
    {"newText":"","range":{"end":{"character":4,"line":74},"start":{"character":0,"line":74}}}
    {"newText":"","range":{"end":{"character":4,"line":75},"start":{"character":0,"line":75}}}
    {"newText":"","range":{"end":{"character":8,"line":76},"start":{"character":0,"line":76}}}
    {"newText":"","range":{"end":{"character":4,"line":77},"start":{"character":0,"line":77}}}
    {"newText":"","range":{"end":{"character":8,"line":78},"start":{"character":0,"line":78}}}
    {"newText":"","range":{"end":{"character":4,"line":79},"start":{"character":0,"line":79}}}
    {"newText":"","range":{"end":{"character":4,"line":80},"start":{"character":0,"line":80}}}
    {"newText":"","range":{"end":{"character":4,"line":81},"start":{"character":0,"line":81}}}
    {"newText":"","range":{"end":{"character":8,"line":82},"start":{"character":0,"line":82}}}
    {"newText":"","range":{"end":{"character":4,"line":83},"start":{"character":0,"line":83}}}
    {"newText":"","range":{"end":{"character":4,"line":84},"start":{"character":0,"line":84}}}
    {"newText":"","range":{"end":{"character":4,"line":85},"start":{"character":0,"line":85}}}
    {"newText":"","range":{"end":{"character":4,"line":86},"start":{"character":0,"line":86}}}
    {"newText":"","range":{"end":{"character":8,"line":87},"start":{"character":0,"line":87}}}
    {"newText":"","range":{"end":{"character":4,"line":88},"start":{"character":0,"line":88}}}
    {"newText":"","range":{"end":{"character":4,"line":89},"start":{"character":0,"line":89}}} |}]



let doc = {cobol|
       PROCEDURE DIVISION.
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
    {"params":{"diagnostics":[{"message":"Invalid syntax","range":{"end":{"character":16,"line":1},"start":{"character":7,"line":1}},"severity":1}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"newText":"","range":{"end":{"character":7,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":3,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"   ","range":{"end":{"character":0,"line":4},"start":{"character":0,"line":4}}}
    {"newText":"   ","range":{"end":{"character":0,"line":6},"start":{"character":0,"line":6}}}
    {"newText":"","range":{"end":{"character":3,"line":7},"start":{"character":0,"line":7}}} |}]


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
    {"newText":"","range":{"end":{"character":7,"line":1},"start":{"character":0,"line":1}}}
    {"newText":"","range":{"end":{"character":7,"line":2},"start":{"character":0,"line":2}}}
    {"newText":"","range":{"end":{"character":3,"line":3},"start":{"character":0,"line":3}}}
    {"newText":"","range":{"end":{"character":3,"line":5},"start":{"character":0,"line":5}}}
    {"newText":"","range":{"end":{"character":3,"line":6},"start":{"character":0,"line":6}}}
    {"newText":"","range":{"end":{"character":7,"line":7},"start":{"character":0,"line":7}}}
    {"newText":"","range":{"end":{"character":3,"line":8},"start":{"character":0,"line":8}}} |}]
