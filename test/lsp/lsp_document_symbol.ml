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

let string_of_kind (kind: SymbolKind.t) =
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
    (string_of_kind doc_sym.kind)
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
      PROGRAM-ID. prog(Function)	1:8 -> 2:25 |}]

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
      PROGRAM-ID. full(Function)	1:8 -> 10:25
        PROCEDURE DIVISION(Function)	3:8 -> 9:23
          sec1 SECTION(Function)	4:10 -> 6:19
            para11(Function)	5:12 -> 5:19
            para12(Function)	6:12 -> 6:19
          sec2 SECTION(Function)	7:10 -> 8:19
            para21(Function)	8:12 -> 8:19
          sec3 SECTION(Function)	9:10 -> 9:23
      PROGRAM-ID. onesection(Function)	12:8 -> 16:31
        PROCEDURE DIVISION(Function)	14:8 -> 15:23
          sec1 SECTION(Function)	15:10 -> 15:23
      PROGRAM-ID. oneparagraph(Function)	18:8 -> 22:33
        PROCEDURE DIVISION(Function)	20:8 -> 21:16
          para1(Function)	21:10 -> 21:16
      PROGRAM-ID. anon(Function)	24:8 -> 28:25
        PROCEDURE DIVISION(Function)	26:8 -> 27:25
          Anonymous paragraph(Function)	27:10 -> 27:25
      PROGRAM-ID. anon_n_sec(Function)	30:8 -> 35:31
        PROCEDURE DIVISION(Function)	32:8 -> 34:23
          Anonymous paragraph(Function)	33:10 -> 33:25
          sec1 SECTION(Function)	34:10 -> 34:23
      PROGRAM-ID. emtpy(Function)	37:8 -> 40:26
        PROCEDURE DIVISION(Function)	39:8 -> 39:27 |}]

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
      PROGRAM-ID. prog(Function)	1:8 -> 20:25
        DATA DIVISION(Function)	3:8 -> 19:22
          WORKING-STORAGE SECTION(Function)	4:8 -> 19:22
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
      PROGRAM-ID. prog(Function)	1:8 -> 12:25
        ENVIRONMENT DIVISION(Function)	3:8 -> 11:20
          CONFIGURATION SECTION(Function)	4:8 -> 8:33
          INPUT-OUTPUT SECTION(Function)	9:8 -> 11:20 |}]


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
      PROGRAM-ID. prog(Function)	1:8 -> 14:25
        ENVIRONMENT DIVISION(Function)	3:8 -> 6:16
          CONFIGURATION SECTION(Function)	4:8 -> 6:16
        DATA DIVISION(Function)	7:8 -> 11:34
          WORKING-STORAGE SECTION(Function)	10:8 -> 11:34
            01 bb(Variable)	11:8 -> 11:34
          LOCAL-STORAGE SECTION(Function)	8:8 -> 9:34
            01 aa(Variable)	9:8 -> 9:34
        PROCEDURE DIVISION(Function)	12:8 -> 13:15
          para(Function)	13:10 -> 13:15 |}]

let%expect_test "object-symbol" =
  let end_with_postproc = document_symbol {cobol|
    CLASS-ID. Account INHERITS Base.
    ENVIRONMENT DIVISION.
    CONFIGURATION SECTION.
    REPOSITORY.
    CLASS Base.

    FACTORY.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 number-of-accounts PIC 9(5) VALUE ZERO.
    PROCEDURE DIVISION.

    METHOD-ID. newAccount
    DATA DIVISION.
    LOCAL-STORAGE SECTION.
    LINKAGE SECTION.
    01 an-object USAGE IS OBJECT REFERENCE ACTIVE-CLASS.
    PROCEDURE DIVISION RETURNING an-object.
    begin-here.
      INVOKE SELF "new" RETURNING an-object.
      INVOKE an-object "initializeAccount" USING BY CONTENT number-of-accounts.
      GOBACK.
    END METHOD newAccount.

    METHOD-ID. addAccount
    PROCEDURE DIVISION.
    method-start.
      ADD 1 TO number-of-accounts.
      GOBACK.
    END METHOD addAccount.

    METHOD-ID. removeAccount
    PROCEDURE DIVISION.
    main-entry.
      SUBTRACT 1 FROM number-of-accounts.
      GOBACK.
    END METHOD removeAccount.
    END FACTORY.

    OBJECT.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 account-balance PIC S9(9)V99.
    01 account-number PIC X(9).
    01 the-date PIC 9(8).
    PROCEDURE DIVISION.

    METHOD-ID. displayUI
    DATA DIVISION.
    LOCAL-STORAGE SECTION.
    01 in-data.
      03 action-type PIC X.
      03 in-amount PIC S9(9)V99.
      03 in-wrk PIC X(12).
    PROCEDURE DIVISION.
    method-start.
      DISPLAY "Enter D for Deposit, B for Balance or W for Withdrawal"
      ACCEPT in-data
      EVALUATE action-type
        WHEN "D"
          PERFORM get-amount
          INVOKE SELF "deposit" USING in-amount
        WHEN "W"
          PERFORM get-amount
          INVOKE SELF "withdraw" USING in-amount
        WHEN "B"
          INVOKE SELF "balance"
        WHEN OTHER
          DISPLAY "Enter valid transaction type."
          GOBACK
      END-EVALUATE
      GOBACK.
    get-amount.
      DISPLAY "Enter amount 9(9).99"
      ACCEPT in-wrk
      COMPUTE in-amount = FUNCTION NUMVAL (in-wrk).
    END METHOD displayUI.

    METHOD-ID. balance
    DATA DIVISION.
    LOCAL-STORAGE SECTION.
    01 display-balance PIC ZZZ,ZZZ,ZZ9.99B.
    PROCEDURE DIVISION.
    disp-balance.
      MOVE account-balance to display-balance
      DISPLAY "Your Account Balance is:" display-balance
      GOBACK.
    END METHOD balance.

    METHOD-ID. deposit
    DATA DIVISION.
    LINKAGE SECTION.
    01 in-deposit PIC S9(9)V99.
    PROCEDURE DIVISION USING in-deposit.
    make-deposit.
      ADD in-deposit TO account-balance
      GOBACK.
    END METHOD deposit.

    METHOD-ID. withdraw
    DATA DIVISION.
    LINKAGE SECTION.
    01 in-withdraw PIC S9(9)V99.
    PROCEDURE DIVISION USING in-withdraw.
    withdraw-start.
      IF account-balance >= in-withdraw
        SUBTRACT in-withdraw FROM account-balance
      ELSE
        DISPLAY "Your Balance is Inadequate"
      END-IF
      GOBACK.
    END METHOD withdraw.

    METHOD-ID. initializeAccount
    DATA DIVISION.
    LINKAGE SECTION.
    01 new-account-number PIC 9(5).
    PROCEDURE DIVISION USING new-account-number.
    Begin-initialization.
      MOVE ZERO TO account-balance
      MOVE new-account-number TO account-number
      MOVE FUNCTION CURRENT-DATE (1: 8) TO the-date
      GOBACK.
    END METHOD initializeAccount.
    END OBJECT.
    END CLASS Account.
  |cobol}
 in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    Doc symbols (60 total):
      CLASS-ID. Account(Class)	1:4 -> 126:22
        ENVIRONMENT DIVISION(Function)	2:4 -> 5:15
          CONFIGURATION SECTION(Function)	3:4 -> 5:15
        FACTORY.(Object)	7:4 -> 38:16
          DATA DIVISION(Function)	8:4 -> 10:46
            WORKING-STORAGE SECTION(Function)	9:4 -> 10:46
              01 number-of-accounts(Variable)	10:4 -> 10:46
          METHOD-ID. newAccount(Method)	13:4 -> 23:26
            DATA DIVISION(Function)	14:4 -> 17:56
              LINKAGE SECTION(Function)	16:4 -> 17:56
                01 an-object(Variable)	17:4 -> 17:56
              LOCAL-STORAGE SECTION(Function)	15:4 -> 15:26
            PROCEDURE DIVISION(Function)	18:4 -> 22:13
              begin-here(Function)	19:4 -> 22:13
          METHOD-ID. addAccount(Method)	25:4 -> 30:26
            PROCEDURE DIVISION(Function)	26:4 -> 29:13
              method-start(Function)	27:4 -> 29:13
          METHOD-ID. removeAccount(Method)	32:4 -> 37:29
            PROCEDURE DIVISION(Function)	33:4 -> 36:13
              main-entry(Function)	34:4 -> 36:13
        OBJECT.(Object)	40:4 -> 125:15
          DATA DIVISION(Function)	41:4 -> 45:25
            WORKING-STORAGE SECTION(Function)	42:4 -> 45:25
              01 account-balance(Variable)	43:4 -> 43:36
              01 account-number(Variable)	44:4 -> 44:31
              01 the-date(Variable)	45:4 -> 45:25
          METHOD-ID. displayUI(Method)	48:4 -> 77:25
            DATA DIVISION(Function)	49:4 -> 54:26
              LOCAL-STORAGE SECTION(Function)	50:4 -> 54:26
                01 in-data(Variable)	51:4 -> 54:26
                  03 action-type(Variable)	52:6 -> 52:27
                  03 in-amount(Variable)	53:6 -> 53:32
                  03 in-wrk(Variable)	54:6 -> 54:26
            PROCEDURE DIVISION(Function)	55:4 -> 76:51
              method-start(Function)	56:4 -> 72:13
              get-amount(Function)	73:4 -> 76:51
          METHOD-ID. balance(Method)	79:4 -> 88:23
            DATA DIVISION(Function)	80:4 -> 82:43
              LOCAL-STORAGE SECTION(Function)	81:4 -> 82:43
                01 display-balance(Variable)	82:4 -> 82:43
            PROCEDURE DIVISION(Function)	83:4 -> 87:13
              disp-balance(Function)	84:4 -> 87:13
          METHOD-ID. deposit(Method)	90:4 -> 98:23
            DATA DIVISION(Function)	91:4 -> 93:31
              LINKAGE SECTION(Function)	92:4 -> 93:31
                01 in-deposit(Variable)	93:4 -> 93:31
            PROCEDURE DIVISION(Function)	94:4 -> 97:13
              make-deposit(Function)	95:4 -> 97:13
          METHOD-ID. withdraw(Method)	100:4 -> 112:24
            DATA DIVISION(Function)	101:4 -> 103:32
              LINKAGE SECTION(Function)	102:4 -> 103:32
                01 in-withdraw(Variable)	103:4 -> 103:32
            PROCEDURE DIVISION(Function)	104:4 -> 111:13
              withdraw-start(Function)	105:4 -> 111:13
          METHOD-ID. initializeAccount(Method)	114:4 -> 124:33
            DATA DIVISION(Function)	115:4 -> 117:35
              LINKAGE SECTION(Function)	116:4 -> 117:35
                01 new-account-number(Variable)	117:4 -> 117:35
            PROCEDURE DIVISION(Function)	118:4 -> 123:13
              Begin-initialization(Function)	119:4 -> 123:13 |}]

let%expect_test "interface-symbol" =
  let end_with_postproc = document_symbol {cobol|
    INTERFACE-ID. BaseFactoryInterface.
    PROCEDURE DIVISION.
    METHOD-ID. New2
    DATA DIVISION.
    LINKAGE SECTION.
    01 outObject USAGE OBJECT REFERENCE ACTIVE-CLASS.
    PROCEDURE DIVISION RETURNING outObject.
    END METHOD New2.
    END INTERFACE BaseFactoryInterface.

    INTERFACE-ID. Interface.
    PROCEDURE DIVISION.
    METHOD-ID. FactoryObject
    DATA DIVISION.
    LINKAGE SECTION.
    01 outFactory USAGE OBJECT REFERENCE FACTORY OF ACTIVE-CLASS.
    PROCEDURE DIVISION RETURNING outFactory.
    END METHOD FactoryObject.
    END INTERFACE BaseInterface.
|cobol}
  in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    Doc symbols (12 total):
      INTERFACE-ID. BaseFactoryInterface(Interface)	1:4 -> 9:39
        METHOD-ID. New2(Method)	3:4 -> 8:20
          DATA DIVISION(Function)	4:4 -> 6:53
            LINKAGE SECTION(Function)	5:4 -> 6:53
              01 outObject(Variable)	6:4 -> 6:53
          PROCEDURE DIVISION(Function)	7:4 -> 7:43
      INTERFACE-ID. INTERFACE(Interface)	11:4 -> 19:32
        METHOD-ID. FactoryObject(Method)	13:4 -> 18:29
          DATA DIVISION(Function)	14:4 -> 16:65
            LINKAGE SECTION(Function)	15:4 -> 16:65
              01 outFactory(Variable)	16:4 -> 16:65
          PROCEDURE DIVISION(Function)	17:4 -> 17:44 |}]
