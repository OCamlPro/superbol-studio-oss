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

open Prog_printer

let dotest = Typeck_testing.show_diagnostics

let%expect_test "pics-incompatible-with-usage" =
  dotest @@ prog "pics-incompatible-with-usage"
    ~working_storage:{|
       77 A PIC 9/9 USAGE BINARY.
       77 B PIC 9,9 USAGE COMP.
       77 C PIC $$99999 USAGE PACKED-DECIMAL.
       77 D PIC X(9) USAGE BIT.
       77 E PIC 9 USAGE BIT.
       77 F PIC A USAGE NATIONAL.
       77 G PIC 1 USAGE BINARY.
    |};
  [%expect {|
    prog.cob:4.16-4.19:
       1          PROGRAM-ID. pics-incompatible-with-usage.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A PIC 9/9 USAGE BINARY.
    ----                   ^^^
       5          77 B PIC 9,9 USAGE COMP.
       6          77 C PIC $$99999 USAGE PACKED-DECIMAL.
    >> Error: PICTURE of category numeric-edited is incompatible with USAGE
              BINARY; expected a PICTURE for numeric data item

    prog.cob:5.16-5.19:
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4          77 A PIC 9/9 USAGE BINARY.
       5 >        77 B PIC 9,9 USAGE COMP.
    ----                   ^^^
       6          77 C PIC $$99999 USAGE PACKED-DECIMAL.
       7          77 D PIC X(9) USAGE BIT.
    >> Error: PICTURE of category numeric-edited is incompatible with USAGE
              BINARY; expected a PICTURE for numeric data item

    prog.cob:6.16-6.23:
       3          WORKING-STORAGE SECTION.
       4          77 A PIC 9/9 USAGE BINARY.
       5          77 B PIC 9,9 USAGE COMP.
       6 >        77 C PIC $$99999 USAGE PACKED-DECIMAL.
    ----                   ^^^^^^^
       7          77 D PIC X(9) USAGE BIT.
       8          77 E PIC 9 USAGE BIT.
    >> Error: PICTURE of category numeric-edited is incompatible with USAGE
              PACKED-DECIMAL; expected a PICTURE for numeric data item

    prog.cob:7.16-7.20:
       4          77 A PIC 9/9 USAGE BINARY.
       5          77 B PIC 9,9 USAGE COMP.
       6          77 C PIC $$99999 USAGE PACKED-DECIMAL.
       7 >        77 D PIC X(9) USAGE BIT.
    ----                   ^^^^
       8          77 E PIC 9 USAGE BIT.
       9          77 F PIC A USAGE NATIONAL.
    >> Error: PICTURE of category alphanumeric is incompatible with USAGE
              BIT; expected a PICTURE for boolean data item

    prog.cob:8.16-8.17:
       5          77 B PIC 9,9 USAGE COMP.
       6          77 C PIC $$99999 USAGE PACKED-DECIMAL.
       7          77 D PIC X(9) USAGE BIT.
       8 >        77 E PIC 9 USAGE BIT.
    ----                   ^
       9          77 F PIC A USAGE NATIONAL.
      10          77 G PIC 1 USAGE BINARY.
    >> Error: PICTURE of category numeric is incompatible with USAGE
              BIT; expected a PICTURE for boolean data item

    prog.cob:9.16-9.17:
       6          77 C PIC $$99999 USAGE PACKED-DECIMAL.
       7          77 D PIC X(9) USAGE BIT.
       8          77 E PIC 9 USAGE BIT.
       9 >        77 F PIC A USAGE NATIONAL.
    ----                   ^
      10          77 G PIC 1 USAGE BINARY.
      11          PROCEDURE DIVISION.
    >> Error: PICTURE of category alphabetic is incompatible with USAGE
              NATIONAL; expected a PICTURE for boolean, national,
              national-edited, numeric, or numeric-edited data item

    prog.cob:10.16-10.17:
       7          77 D PIC X(9) USAGE BIT.
       8          77 E PIC 9 USAGE BIT.
       9          77 F PIC A USAGE NATIONAL.
      10 >        77 G PIC 1 USAGE BINARY.
    ----                   ^
      11          PROCEDURE DIVISION.
      12
    >> Error: PICTURE of category boolean is incompatible with USAGE
              BINARY; expected a PICTURE for numeric data item |}];;
