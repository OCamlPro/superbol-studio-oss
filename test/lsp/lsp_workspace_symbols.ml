(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Workspace symbols, for open documents and for documents whose symbols are
    held in the project's cache. *)

open Ez_file.FileString.OP
open Lsp_caching                         (* layout, server, write, open_doc... *)

module LSP = Cobol_lsp

let prog = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "BOOK".
       01 CUSTOMER-RECORD.
          05 CUSTOMER-NAME PIC X(10).
          05 CUSTOMER-KIND PIC X.
             88 CUSTOMER-IS-VIP VALUE "V".
       01 FILLER PIC X.
       01 MAX-COUNT CONSTANT AS 10.
       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
       PROCESS-CUSTOMER.
           MOVE "x" TO CUSTOMER-NAME.
       END-OF-JOB.
           STOP RUN.
|cobol}

let other = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. other.
       PROCEDURE DIVISION.
       PRINT-CUSTOMERS.
           STOP RUN.
|cobol}

let make_project () =
  let projdir =
    Superbol_testutils.Tempdir.make_n_enter "superbol-workspace-symbols" in
  ignore (write ~projdir layout.project_config_filename "");
  ignore (write ~projdir "BOOK.cpy" "       01 BOOK-ITEM PIC X.\n");
  ignore (write ~projdir "prog.cob" prog);
  ignore (write ~projdir ("sub" // "other.cbl") other);
  projdir

let query server query =
  match
    LSP.Request.INTERNAL.workspace_symbol server
      (Lsp.Types.WorkspaceSymbolParams.create ~query ())
  with
  | None -> print_endline "no result"
  | Some symbols ->
      Pretty.out "@[<v>%a@]@."
        (Fmt.list begin fun ppf
          Lsp.Types.SymbolInformation.{ name; kind; containerName;
                                        location = { uri; range }; _ } ->
          Fmt.pf ppf "%s (%s%a) in %s:%d" name
            (Yojson.Safe.to_string @@ Lsp.Types.SymbolKind.yojson_of_t kind)
            Fmt.(option (any ", in " ++ string)) containerName
            (Filename.basename @@ Lsp.Uri.to_path uri)
            (range.start.line + 1)
        end) symbols

let%expect_test "symbols of open documents" =
  let projdir = make_project () in
  let server = open_doc (server ()) ~projdir "prog.cob" in
  ignore [%expect.output];
  query server "";
  [%expect {|
    BOOK-ITEM (13, in prog) in prog.cob:6
    CUSTOMER-IS-VIP (22, in prog) in prog.cob:10
    CUSTOMER-KIND (8, in prog) in prog.cob:9
    CUSTOMER-NAME (8, in prog) in prog.cob:8
    CUSTOMER-RECORD (13, in prog) in prog.cob:7
    END-OF-JOB (12, in MAIN-LOGIC) in prog.cob:17
    MAIN-LOGIC (3, in prog) in prog.cob:14
    MAX-COUNT (14, in prog) in prog.cob:12
    PROCESS-CUSTOMER (12, in MAIN-LOGIC) in prog.cob:15
    prog (2) in prog.cob:2 |}];
  query server "cust";
  [%expect {|
    CUSTOMER-IS-VIP (22, in prog) in prog.cob:10
    CUSTOMER-KIND (8, in prog) in prog.cob:9
    CUSTOMER-NAME (8, in prog) in prog.cob:8
    CUSTOMER-RECORD (13, in prog) in prog.cob:7
    PROCESS-CUSTOMER (12, in MAIN-LOGIC) in prog.cob:15 |}];
  query server "pc";
  [%expect {| PROCESS-CUSTOMER (12, in MAIN-LOGIC) in prog.cob:15 |}];
  query server "zzz";
  [%expect {| |}];
  cleanup projdir;;

let%expect_test "symbols of documents that are not open are retrieved from \
                 the cache" =
  let projdir = make_project () in
  (* A first session stores both programs upon opening them. *)
  let first = open_doc (server ()) ~projdir "prog.cob" in
  ignore (open_doc first ~projdir ("sub" // "other.cbl"));
  ignore [%expect.output];
  (* A later session only opens one of them. *)
  let server = open_doc (server ()) ~projdir "prog.cob" in
  ignore [%expect.output];
  query server "cust";
  [%expect {|
    CUSTOMER-IS-VIP (22, in prog) in prog.cob:10
    CUSTOMER-KIND (8, in prog) in prog.cob:9
    CUSTOMER-NAME (8, in prog) in prog.cob:8
    CUSTOMER-RECORD (13, in prog) in prog.cob:7
    PRINT-CUSTOMERS (12, in OTHER) in other.cbl:5
    PROCESS-CUSTOMER (12, in MAIN-LOGIC) in prog.cob:15 |}];
  (* Changing a file that is not open makes its symbols unavailable until it is
     analyzed again. *)
  ignore (write ~projdir ("sub" // "other.cbl") (other ^ "      * changed\n"));
  query server "cust";
  [%expect {|
    CUSTOMER-IS-VIP (22, in prog) in prog.cob:10
    CUSTOMER-KIND (8, in prog) in prog.cob:9
    CUSTOMER-NAME (8, in prog) in prog.cob:8
    CUSTOMER-RECORD (13, in prog) in prog.cob:7
    PROCESS-CUSTOMER (12, in MAIN-LOGIC) in prog.cob:15 |}];
  cleanup projdir;;

let%expect_test "hidden directories and the project's work directory are \
                 skipped" =
  let projdir = make_project () in
  let program name = Printf.sprintf {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. %s.
       PROCEDURE DIVISION.
           STOP RUN.
|cobol} name
  in
  let files = [ ".hidden" // "hidden.cbl", "hidden-prog";
                "_superbol" // "work.cbl", "work-prog";
                "_legacy" // "legacy.cbl", "legacy-prog" ] in
  (* A first session stores all programs upon opening them... *)
  List.fold_left begin fun server (filename, name) ->
    ignore (write ~projdir filename (program name));
    open_doc server ~projdir filename
  end (server ()) files |> ignore;
  ignore [%expect.output];
  (* ... but a later session only finds those that are not skipped. *)
  let server = open_doc (server ()) ~projdir "prog.cob" in
  ignore [%expect.output];
  query server "prog";
  [%expect {|
    prog (2) in prog.cob:2
    legacy-prog (2) in legacy.cbl:2 |}];
  cleanup projdir;;

(** Opens each of [files] in a first session, so their symbols are stored, and
    then queries [query] in a later session that only opens [prog.cob]. *)
let query_after_storing ~projdir ~query:q files =
  let program name = Printf.sprintf {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. %s.
       PROCEDURE DIVISION.
           STOP RUN.
|cobol} name
  in
  List.fold_left begin fun server (filename, name) ->
    ignore (write ~projdir filename (program name));
    open_doc server ~projdir filename
  end (server ()) files |> ignore;
  let server = open_doc (server ()) ~projdir "prog.cob" in
  query server q

let%expect_test "directories excluded by default are skipped" =
  let projdir = make_project () in
  query_after_storing ~projdir ~query:"prog"
    [ "_build" // "built.cbl", "built-prog";
      "node_modules" // "x" // "nm.cbl", "nm-prog";
      "src" // "kept.cbl", "kept-prog" ];
  [%expect.output] |> String.split_on_char '\n' |>
  List.filter (fun l -> String.ends_with ~suffix:":2" l) |>
  List.iter print_endline;
  [%expect {|
    prog (2) in prog.cob:2
    kept-prog (2) in kept.cbl:2 |}];
  cleanup projdir;;

let%expect_test "the project configuration replaces directories excluded by \
                 default" =
  let projdir = make_project () in
  ignore (write ~projdir layout.project_config_filename
            "[cobol]\nexclude-dirs = [\"generated\", \"./sub/skipped/\"]\n");
  query_after_storing ~projdir ~query:"prog"
    [ "generated" // "gen.cbl", "gen-prog";
      "a" // "generated" // "gen2.cbl", "gen2-prog";
      "sub" // "skipped" // "skip.cbl", "skip-prog";
      "other" // "skipped" // "kept.cbl", "kept-prog";
      "_build" // "built.cbl", "built-prog" ];
  [%expect.output] |> String.split_on_char '\n' |>
  List.filter (fun l -> String.ends_with ~suffix:":2" l) |>
  List.iter print_endline;
  [%expect {|
    prog (2) in prog.cob:2
    built-prog (2) in built.cbl:2
    kept-prog (2) in kept.cbl:2 |}];
  cleanup projdir;;
