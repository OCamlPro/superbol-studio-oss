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

let rec count (doc_symbols: DocumentSymbol.t list) =
  List.fold_left (fun acc (doc_sym: DocumentSymbol.t) ->
      match doc_sym.children with
      | None -> acc + 1
      | Some children -> acc + 1 + count children) 0 doc_symbols

let pp_range ppf (range: Range.t) =
  Fmt.pf ppf "%d:%d -> %d:%d" range.start.line range.start.character
    range.end_.line range.end_.character

let s_of_kind (kind: SymbolKind.t) =
  match kind with
  | File -> "File"
  | Module -> "Module"
  | Namespace -> "Namespace"
  | Package -> "Package"
  | Class -> "Class"
  | Method -> "Method"
  | Property -> "Property"
  | Field -> "Field"
  | Constructor -> "Constructor"
  | Enum -> "Enum"
  | Interface -> "Interface"
  | Function -> "Function"
  | Variable -> "Variable"
  | Constant -> "Constant"
  | String -> "String"
  | Number -> "Number"
  | Boolean -> "Boolean"
  | Array -> "Array"
  | Object -> "Object"
  | Key -> "Key"
  | Null -> "Null"
  | EnumMember -> "EnumMember"
  | Struct -> "Struct"
  | Event -> "Event"
  | Operator -> "Operator"
  | TypeParameter -> "TypeParameter"


let rec print_symbol_with_children ppf (doc_sym: DocumentSymbol.t) =
  Fmt.pf ppf "@[<v 2>%s(%s)\t%a@;%a@]"
    doc_sym.name
    (s_of_kind doc_sym.kind)
    pp_range doc_sym.range
    Fmt.(option (list ~sep:sp print_symbol_with_children)) doc_sym.children

let document_symbol doc : string -> unit =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  let params = DocumentSymbolParams.create () ~textDocument:prog in
  begin match LSP.Request.document_symbol server params with
    | None ->
      Pretty.out "No document symbol@."
    | Some `DocumentSymbol document_symbols ->
      Pretty.out "@.@[<v 2>Doc symbols (%d total):@;%a@]@\n"
      (count document_symbols)
      Fmt.(list ~sep:sp print_symbol_with_children)
      document_symbols
  end;
  end_with_postproc
;;

let%expect_test "emtpy-document-symbol" =
  let end_with_postproc = document_symbol {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
  |cobol}
 in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    Doc symbols (1 total):
      PROGRAM-ID. prog(Module)	1:8 -> 2:25 |}]

let%expect_test "procedure-symbol" =
  let end_with_postproc = document_symbol {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. full.
        PROCEDURE DIVISION.
          sec1 SECTION.
            para11.
            para12.
          sec2 SECTION.
            para21.
          sec3 SECTION.
        END PROGRAM full.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. onesection.
        PROCEDURE DIVISION.
          sec1 SECTION.
        END PROGRAM onesection.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. oneparagraph.
        PROCEDURE DIVISION.
          para1.
        END PROGRAM oneparagraph.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. anon.
        PROCEDURE DIVISION.
          display "anon".
        END PROGRAM anon.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. anon_n_sec.
        PROCEDURE DIVISION.
          display "anon".
          sec1 SECTION.
        END PROGRAM anon_n_sec.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. emtpy.
        PROCEDURE DIVISION.
        END PROGRAM empty.
  |cobol}
 in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    Doc symbols (23 total):
      PROGRAM-ID. full(Module)	1:8 -> 10:25
        PROCEDURE DIVISION(Module)	3:8 -> 9:23
          sec1 SECTION(Function)	4:10 -> 6:19
            para11(Function)	5:12 -> 5:19
            para12(Function)	6:12 -> 6:19
          sec2 SECTION(Function)	7:10 -> 8:19
            para21(Function)	8:12 -> 8:19
          sec3 SECTION(Function)	9:10 -> 9:23
      PROGRAM-ID. onesection(Module)	12:8 -> 16:31
        PROCEDURE DIVISION(Module)	14:8 -> 15:23
          sec1 SECTION(Function)	15:10 -> 15:23
      PROGRAM-ID. oneparagraph(Module)	18:8 -> 22:33
        PROCEDURE DIVISION(Module)	20:8 -> 21:16
          para1(Function)	21:10 -> 21:16
      PROGRAM-ID. anon(Module)	24:8 -> 28:25
        PROCEDURE DIVISION(Module)	26:8 -> 27:25
          Unnamed paragraph(Function)	27:10 -> 27:25
      PROGRAM-ID. anon_n_sec(Module)	30:8 -> 35:31
        PROCEDURE DIVISION(Module)	32:8 -> 34:23
          Unnamed paragraph(Function)	33:10 -> 33:25
          sec1 SECTION(Function)	34:10 -> 34:23
      PROGRAM-ID. emtpy(Module)	37:8 -> 40:26
        PROCEDURE DIVISION(Module)	39:8 -> 39:27 |}]

let%expect_test "datadiv-symbol" =
  let end_with_postproc = document_symbol {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01.
          88 fillercond VALUE "aaaa".
          02 aa PIC X.
          88 aacond VALUE "a".
          02 bb.
            03 cc.
              04 dd PIC X.
            03 ee PIC X.
            03.
              04 ff PIC X.
              88 ffcond VALUE "a".
        66 renanme-aa RENAMES aa THRU bb.
        01 cons constant "constant".
        01 zz occurs 10 times indexed by ind.
          02 yy PIC X.
        END PROGRAM prog.
  |cobol}
 in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    Doc symbols (18 total):
      PROGRAM-ID. prog(Module)	1:8 -> 20:25
        DATA DIVISION(Module)	3:8 -> 19:22
          WORKING-STORAGE SECTION(Module)	4:8 -> 19:22
            01 FILLER(Variable)	5:8 -> 16:41
              88 fillercond(Boolean)	6:10 -> 6:37
              02 aa(Variable)	7:10 -> 8:30
                88 aacond(Boolean)	8:10 -> 8:30
              02 bb(Variable)	9:10 -> 15:34
                03 cc(Variable)	10:12 -> 11:26
                  04 dd(Variable)	11:14 -> 11:26
                03 ee(Variable)	12:12 -> 12:24
                03 FILLER(Variable)	13:12 -> 15:34
                  04 ff(Variable)	14:14 -> 15:34
                    88 ffcond(Boolean)	15:14 -> 15:34
              66 renanme-aa(Variable)	16:8 -> 16:41
            01 cons(Constant)	17:8 -> 17:36
            01 zz(Variable)	18:8 -> 19:22
              02 yy(Variable)	19:10 -> 19:22 |}]

let%expect_test "envdiv-symbol" =
  let end_with_postproc = document_symbol {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER.
          LINUX.
        REPOSITORY.
          FUNCTION ALL INTRINSIC.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        I-O-CONTROL.
        END PROGRAM prog.
  |cobol}
 in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    Doc symbols (4 total):
      PROGRAM-ID. prog(Module)	1:8 -> 12:25
        ENVIRONMENT DIVISION(Module)	3:8 -> 11:20
          CONFIGURATION SECTION(Module)	4:8 -> 8:33
          INPUT-OUTPUT SECTION(Module)	9:8 -> 11:20 |}]


let%expect_test "many-division-symbol" =
  let end_with_postproc = document_symbol {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER.
          LINUX.
        DATA DIVISION.
        LOCAL-STORAGE SECTION.
        01 aa PIC X USAGE DISPLAY.
        WORKING-STORAGE SECTION.
        01 bb PIC X USAGE DISPLAY.
        PROCEDURE DIVISION.
          para.
        END PROGRAM prog.
  |cobol}
 in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    Doc symbols (10 total):
      PROGRAM-ID. prog(Module)	1:8 -> 14:25
        ENVIRONMENT DIVISION(Module)	3:8 -> 6:16
          CONFIGURATION SECTION(Module)	4:8 -> 6:16
        DATA DIVISION(Module)	7:8 -> 11:34
          WORKING-STORAGE SECTION(Module)	10:8 -> 11:34
            01 bb(Variable)	11:8 -> 11:34
          LOCAL-STORAGE SECTION(Module)	8:8 -> 9:34
            01 aa(Variable)	9:8 -> 9:34
        PROCEDURE DIVISION(Module)	12:8 -> 13:15
          para(Function)	13:10 -> 13:15 |}]

