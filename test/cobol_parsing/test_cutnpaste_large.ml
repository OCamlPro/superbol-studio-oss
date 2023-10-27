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

open Ez_file
open FileString.OP
open Testsuite_utils                      (* implemented in `../output-tests' *)

let check_initial_ptree ~ptree0 diags =
  if ptree0 = None then begin
    Pretty.out "%a@." Cobol_common.Diagnostics.Set.pp diags;
    Pretty.failwith "Unable to parse the original program.";
  end (* else Test_appending.show_ptree 0 0 ptree0 diags *)

let check_new_ptree i n ~ptree0 ptree' diags =
  if Option.compare
      Cobol_ptree.compare_compilation_group ptree0 ptree' = 0
  then Pretty.out "Ok@."
  else Test_appending.show_ptree i n ptree' diags

let%expect_test "cut-n-paste-mf" =
  let config = Testsuite_utils.from_dialect Cobol_config.DIALECT.mf_strict in
  deep_iter mf_root ~glob:"DayDiffDriver.[cC][bB][lL]" (* <- pick large-ish file *)
    ~f:begin fun path ->
      let file = srcdir // mf_testsuite // path in
      Pretty.out "Considering `%s'.@." file;
      Parser_testing.simulate_cut_n_paste ~config ~repeat:200
        ~f0:check_initial_ptree ~f:check_new_ptree @@
      Parser_testing.extract_position_markers @@
      Parser_testing.insert_periodic_position_markers ~period:51 @@
      FileString.read_file file;
    end;
  end_with_postproc [%expect.output];
  [%expect {|
    Loading: `__srcdir__/import/gnucobol/config/mf-strict.conf'
    Considering `__srcdir__/test/testsuite/microfocus/www.csis.ul.ie/SubProg/DayDiff/DayDiffDriver.cbl'.
    Cutting chunk 54/120 @ 75:10(2754)-79:7(2805) ("RFORM DisplayErrorMessage\...)
    Putting it back
    Ok
    Cutting chunk 15/120 @ 15:41(765)-18:9(816) ("uroDate\" is used to conve...)
    Putting it back
    Ok
    Cutting chunk 63/120 @ 91:60(3213)-93:29(3264) ("t\r\n    EVALUATE TRUE\r\n...)
    Putting it back
    Ok
    Cutting chunk 33/120 @ 45:14(1683)-47:13(1734) ("tValidFirstDate UNTIL Date...)
    Putting it back
    Ok
    Cutting chunk 57/120 @ 81:68(2907)-83:19(2958) ("NCING.\r\n    ACCEPT Secon...)
    Putting it back
    Ok
    Cutting chunk 35/120 @ 48:31(1785)-50:25(1836) ("UNTIL DateIsValid.\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 114/120 @ 184:7(5814)-187:7(5865) ("ference                   ...)
    Putting it back
    Ok
    Cutting chunk 41/120 @ 56:32(2091)-57:24(2142) ("ING FirstDate, FirstDate.\...)
    Putting it back
    Ok
    Cutting chunk 11/120 @ 12:41(561)-13:21(612) ("ser and the date required ...)
    Putting it back
    Ok
    Cutting chunk 58/120 @ 83:19(2958)-84:16(3009) (" USING   BY CONTENT   Seco...)
    Putting it back
    Ok
    Cutting chunk 81/120 @ 121:24(4131)-122:31(4182) ("           PIC XX.\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 40/120 @ 54:40(2040)-56:32(2091) (" DayDifference.\r\n\r\n   ...)
    Putting it back
    Ok
    Cutting chunk 15/120 @ 15:41(765)-18:9(816) ("uroDate\" is used to conve...)
    Putting it back
    Ok
    Cutting chunk 96/120 @ 151:10(4896)-153:24(4947) ("CTION.\r\n01  YYYYDDMMDate...)
    Putting it back
    Ok
    Cutting chunk 100/120 @ 157:41(5100)-159:44(5151) ("8).\r\n\r\nPROCEDURE DIVIS...)
    Putting it back
    Ok
    Cutting chunk 106/120 @ 172:3(5406)-174:2(5457) ("GRAM-ID. GetDayDiff.\r\nAU...)
    Putting it back
    Ok
    Cutting chunk 6/120 @ 8:12(306)-9:8(357) ("yDiff\" program is include...)
    Putting it back
    Ok
    Cutting chunk 1/120 @ 1:29(51)-3:21(102) ("E\"\r\nIDENTIFICATION DIVI...)
    Putting it back
    Ok
    Cutting chunk 17/120 @ 20:21(867)-22:34(918) ("ON.\r\n01  Dates.\r\n    0...)
    Putting it back
    Ok
    Cutting chunk 11/120 @ 12:41(561)-13:21(612) ("ser and the date required ...)
    Putting it back
    Ok
    Cutting chunk 75/120 @ 110:19(3825)-113:5(3876) ("N.\r\nDATA DIVISION.\r\nWO...)
    Putting it back
    Ok
    Cutting chunk 63/120 @ 91:60(3213)-93:29(3264) ("t\r\n    EVALUATE TRUE\r\n...)
    Putting it back
    Ok
    Cutting chunk 89/120 @ 138:22(4539)-140:14(4590) ("N.\r\nPROGRAM-ID. SortDate...)
    Putting it back
    Ok
    Cutting chunk 62/120 @ 91:9(3162)-91:60(3213) ("AY \"Invalid date . Return...)
    Putting it back
    Ok
    Cutting chunk 85/120 @ 128:26(4335)-130:5(4386) ("YYYDay.\r\n    MOVE DDMMMo...)
    Putting it back
    Ok
    Cutting chunk 73/120 @ 107:6(3723)-108:27(3774) (".    Michael Coughlan.\r\n...)
    Putting it back
    Ok
    Cutting chunk 41/120 @ 56:32(2091)-57:24(2142) ("ING FirstDate, FirstDate.\...)
    Putting it back
    Ok
    Cutting chunk 113/120 @ 183:2(5763)-184:7(5814) ("  Date2                   ...)
    Putting it back
    Ok
    Cutting chunk 68/120 @ 96:32(3468)-97:16(3519) ("DISPLAY \"Day contains all...)
    Putting it back
    Ok
    Cutting chunk 54/120 @ 75:10(2754)-79:7(2805) ("RFORM DisplayErrorMessage\...)
    Putting it back
    Ok
    Cutting chunk 80/120 @ 120:17(4080)-121:24(4131) ("                  PIC XX.\...)
    Putting it back
    Ok
    Cutting chunk 59/120 @ 84:16(3009)-85:7(3060) ("            BY REFERENCE V...)
    Putting it back
    Ok
    Cutting chunk 12/120 @ 13:21(612)-14:18(663) ("ogram are in different for...)
    Putting it back
    Ok
    Cutting chunk 109/120 @ 175:48(5559)-176:49(5610) ("\r\n* The first date passe...)
    Putting it back
    Ok
    Cutting chunk 119/120 @ 195:2(6069)-197:0(6093) ("D PROGRAM DayDriver.\r\n\r\n")
    Putting it back
    Ok
    Cutting chunk 108/120 @ 174:53(5508)-175:48(5559) ("o\r\n* Dates. The dates mu...)
    Putting it back
    Ok
    Cutting chunk 73/120 @ 107:6(3723)-108:27(3774) (".    Michael Coughlan.\r\n...)
    Putting it back
    Ok
    Cutting chunk 109/120 @ 175:48(5559)-176:49(5610) ("\r\n* The first date passe...)
    Putting it back
    Ok
    Cutting chunk 61/120 @ 86:34(3111)-91:9(3162) ("e\r\n    END-IF.\r\n\r\n\r...)
    Putting it back
    Ok
    Cutting chunk 20/120 @ 24:46(1020)-25:46(1071) ("XX.\r\n    02 SecondDatePr...)
    Putting it back
    Ok
    Cutting chunk 28/120 @ 36:13(1428)-37:20(1479) ("ontainsZeros         VALUE...)
    Putting it back
    Ok
    Cutting chunk 58/120 @ 83:19(2958)-84:16(3009) (" USING   BY CONTENT   Seco...)
    Putting it back
    Ok
    Cutting chunk 24/120 @ 31:34(1224)-32:43(1275) ("PIC 9.\r\n    88 DateIsVal...)
    Putting it back
    Ok
    Cutting chunk 105/120 @ 167:17(5355)-172:3(5406) ("ateToEuroDate.\r\n\r\n\r\n...)
    Putting it back
    Ok
    Cutting chunk 40/120 @ 54:40(2040)-56:32(2091) (" DayDifference.\r\n\r\n   ...)
    Putting it back
    Ok
    Cutting chunk 64/120 @ 93:29(3264)-94:16(3315) ("   DISPLAY \"Date is not n...)
    Putting it back
    Ok
    Cutting chunk 67/120 @ 95:50(3417)-96:32(3468) ("tains all zeros.\"\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 27/120 @ 35:6(1377)-36:13(1428) (" YearContainsZeros        ...)
    Putting it back
    Ok
    Cutting chunk 31/120 @ 39:34(1581)-43:4(1632) ("VALUE 6.\r\n            \r...)
    Putting it back
    Ok
    Cutting chunk 94/120 @ 148:0(4794)-149:7(4845) ("    02 DDMMMonth          ...)
    Putting it back
    Ok
    Cutting chunk 110/120 @ 176:49(5610)-179:3(5661) ("cond\r\n* Date and the dif...)
    Putting it back
    Ok
    Cutting chunk 37/120 @ 51:17(1887)-53:1(1938) ("eToSortDate\" USING Second...)
    Putting it back
    Ok
    Cutting chunk 93/120 @ 146:12(4743)-148:0(4794) ("Temp.\r\n    02 DDMMDay   ...)
    Putting it back
    Ok
    Cutting chunk 47/120 @ 63:16(2397)-64:5(2448) ("ondDatePrn \" is \" DayDif...)
    Putting it back
    Ok
    Cutting chunk 3/120 @ 5:17(153)-6:5(204) ("ts the difference in days ...)
    Putting it back
    Ok
    Cutting chunk 96/120 @ 151:10(4896)-153:24(4947) ("CTION.\r\n01  YYYYDDMMDate...)
    Putting it back
    Ok
    Cutting chunk 14/120 @ 14:69(714)-15:41(765) ("Y format\r\n* to YYYYMMDD ...)
    Putting it back
    Ok
    Cutting chunk 103/120 @ 163:12(5253)-164:27(5304) ("YYear     TO DDMMYear.\r\n...)
    Putting it back
    Ok
    Cutting chunk 33/120 @ 45:14(1683)-47:13(1734) ("tValidFirstDate UNTIL Date...)
    Putting it back
    Ok
    Cutting chunk 6/120 @ 8:12(306)-9:8(357) ("yDiff\" program is include...)
    Putting it back
    Ok
    Cutting chunk 106/120 @ 172:3(5406)-174:2(5457) ("GRAM-ID. GetDayDiff.\r\nAU...)
    Putting it back
    Ok
    Cutting chunk 87/120 @ 131:20(4437)-134:10(4488) ("P TO YYYYDDMMDate.\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 51/120 @ 71:17(2601)-72:45(2652) ("ate.\r\n    CALL \"Validat...)
    Putting it back
    Ok
    Cutting chunk 14/120 @ 14:69(714)-15:41(765) ("Y format\r\n* to YYYYMMDD ...)
    Putting it back
    Ok
    Cutting chunk 112/120 @ 181:15(5712)-183:2(5763) (".\r\n01  Date1            ...)
    Putting it back
    Ok
    Cutting chunk 90/120 @ 140:14(4590)-141:34(4641) ("chael Coughlan.\r\n* Conve...)
    Putting it back
    Ok
    Cutting chunk 14/120 @ 14:69(714)-15:41(765) ("Y format\r\n* to YYYYMMDD ...)
    Putting it back
    Ok
    Cutting chunk 104/120 @ 164:27(5304)-167:17(5355) ("MMYYYYDate.\r\n    EXIT PR...)
    Putting it back
    Ok
    Cutting chunk 51/120 @ 71:17(2601)-72:45(2652) ("ate.\r\n    CALL \"Validat...)
    Putting it back
    Ok
    Cutting chunk 31/120 @ 39:34(1581)-43:4(1632) ("VALUE 6.\r\n            \r...)
    Putting it back
    Ok
    Cutting chunk 90/120 @ 140:14(4590)-141:34(4641) ("chael Coughlan.\r\n* Conve...)
    Putting it back
    Ok
    Cutting chunk 36/120 @ 50:25(1836)-51:17(1887) ("ate\" USING FirstDate, Fir...)
    Putting it back
    Ok
    Cutting chunk 91/120 @ 141:34(4641)-144:3(4692) ("at to one in DDMMYYYY\r\n\...)
    Putting it back
    Ok
    Cutting chunk 108/120 @ 174:53(5508)-175:48(5559) ("o\r\n* Dates. The dates mu...)
    Putting it back
    Ok
    Cutting chunk 59/120 @ 84:16(3009)-85:7(3060) ("            BY REFERENCE V...)
    Putting it back
    Ok
    Cutting chunk 44/120 @ 59:20(2244)-60:34(2295) ("O FirstDatePrn.\r\n    MOV...)
    Putting it back
    Ok
    Cutting chunk 116/120 @ 188:6(5916)-190:24(5967) ("\r\n   COMPUTE Difference ...)
    Putting it back
    Ok
    Cutting chunk 101/120 @ 159:44(5151)-161:33(5202) ("YYYDate.\r\nBegin.\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 17/120 @ 20:21(867)-22:34(918) ("ON.\r\n01  Dates.\r\n    0...)
    Putting it back
    Ok
    Cutting chunk 31/120 @ 39:34(1581)-43:4(1632) ("VALUE 6.\r\n            \r...)
    Putting it back
    Ok
    Cutting chunk 101/120 @ 159:44(5151)-161:33(5202) ("YYYDate.\r\nBegin.\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 109/120 @ 175:48(5559)-176:49(5610) ("\r\n* The first date passe...)
    Putting it back
    Ok
    Cutting chunk 33/120 @ 45:14(1683)-47:13(1734) ("tValidFirstDate UNTIL Date...)
    Putting it back
    Ok
    Cutting chunk 69/120 @ 97:16(3519)-98:8(3570) ("hGreaterThan12  DISPLAY \"...)
    Putting it back
    Ok
    Cutting chunk 31/120 @ 39:34(1581)-43:4(1632) ("VALUE 6.\r\n            \r...)
    Putting it back
    Ok
    Cutting chunk 83/120 @ 124:34(4233)-126:37(4284) (" PIC X(8).\r\n\r\nPROCEDUR...)
    Putting it back
    Ok
    Cutting chunk 111/120 @ 179:3(5661)-181:15(5712) ("IRONMENT DIVISION.\r\nDATA...)
    Putting it back
    Ok
    Cutting chunk 26/120 @ 33:50(1326)-35:6(1377) ("\n    88 DateNotNumeric   ...)
    Putting it back
    Ok
    Cutting chunk 119/120 @ 195:2(6069)-197:0(6093) ("D PROGRAM DayDriver.\r\n\r\n")
    Putting it back
    Ok
    Cutting chunk 31/120 @ 39:34(1581)-43:4(1632) ("VALUE 6.\r\n            \r...)
    Putting it back
    Ok
    Cutting chunk 60/120 @ 85:7(3060)-86:34(3111) ("DateIsNotValid \r\n       ...)
    Putting it back
    Ok
    Cutting chunk 107/120 @ 174:2(5457)-174:53(5508) ("This module finds the diff...)
    Putting it back
    Ok
    Cutting chunk 25/120 @ 32:43(1275)-33:50(1326) ("\n    88 DateIsNotValid   ...)
    Putting it back
    Ok
    Cutting chunk 108/120 @ 174:53(5508)-175:48(5559) ("o\r\n* Dates. The dates mu...)
    Putting it back
    Ok
    Cutting chunk 14/120 @ 14:69(714)-15:41(765) ("Y format\r\n* to YYYYMMDD ...)
    Putting it back
    Ok
    Cutting chunk 25/120 @ 32:43(1275)-33:50(1326) ("\n    88 DateIsNotValid   ...)
    Putting it back
    Ok
    Cutting chunk 110/120 @ 176:49(5610)-179:3(5661) ("cond\r\n* Date and the dif...)
    Putting it back
    Ok
    Cutting chunk 114/120 @ 184:7(5814)-187:7(5865) ("ference                   ...)
    Putting it back
    Ok
    Cutting chunk 16/120 @ 18:9(816)-20:21(867) ("NT DIVISION.\r\nDATA DIVIS...)
    Putting it back
    Ok
    Cutting chunk 4/120 @ 6:5(204)-6:56(255) ("calls three contained subp...)
    Putting it back
    Ok
    Cutting chunk 18/120 @ 22:34(918)-23:40(969) ("PIC X(8).\r\n    02 Second...)
    Putting it back
    Ok
    Cutting chunk 75/120 @ 110:19(3825)-113:5(3876) ("N.\r\nDATA DIVISION.\r\nWO...)
    Putting it back
    Ok
    Cutting chunk 111/120 @ 179:3(5661)-181:15(5712) ("IRONMENT DIVISION.\r\nDATA...)
    Putting it back
    Ok
    Cutting chunk 74/120 @ 108:27(3774)-110:19(3825) ("YY format to one in YYYYMM...)
    Putting it back
    Ok
    Cutting chunk 109/120 @ 175:48(5559)-176:49(5610) ("\r\n* The first date passe...)
    Putting it back
    Ok
    Cutting chunk 34/120 @ 47:13(1734)-48:31(1785) ("sNotValid TO TRUE.\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 32/120 @ 43:4(1632)-45:14(1683) ("n.\r\n    SET DateIsNotVal...)
    Putting it back
    Ok
    Cutting chunk 23/120 @ 29:34(1173)-31:34(1224) ("PIC ----,--9.\r\n\r\n01  V...)
    Putting it back
    Ok
    Cutting chunk 59/120 @ 84:16(3009)-85:7(3060) ("            BY REFERENCE V...)
    Putting it back
    Ok
    Cutting chunk 26/120 @ 33:50(1326)-35:6(1377) ("\n    88 DateNotNumeric   ...)
    Putting it back
    Ok
    Cutting chunk 114/120 @ 184:7(5814)-187:7(5865) ("ference                   ...)
    Putting it back
    Ok
    Cutting chunk 28/120 @ 36:13(1428)-37:20(1479) ("ontainsZeros         VALUE...)
    Putting it back
    Ok
    Cutting chunk 36/120 @ 50:25(1836)-51:17(1887) ("ate\" USING FirstDate, Fir...)
    Putting it back
    Ok
    Cutting chunk 119/120 @ 195:2(6069)-197:0(6093) ("D PROGRAM DayDriver.\r\n\r\n")
    Putting it back
    Ok
    Cutting chunk 30/120 @ 38:27(1530)-39:34(1581) ("       VALUE 5.\r\n    88 ...)
    Putting it back
    Ok
    Cutting chunk 11/120 @ 12:41(561)-13:21(612) ("ser and the date required ...)
    Putting it back
    Ok
    Cutting chunk 109/120 @ 175:48(5559)-176:49(5610) ("\r\n* The first date passe...)
    Putting it back
    Ok
    Cutting chunk 45/120 @ 60:34(2295)-62:25(2346) ("rn.\r\n    DISPLAY SPACES....)
    Putting it back
    Ok
    Cutting chunk 86/120 @ 130:5(4386)-131:20(4437) ("OVE DDMMYear     TO YYYYYe...)
    Putting it back
    Ok
    Cutting chunk 118/120 @ 190:75(6018)-195:2(6069) ("\r\n   EXIT PROGRAM.\r\n\r...)
    Putting it back
    Ok
    Cutting chunk 112/120 @ 181:15(5712)-183:2(5763) (".\r\n01  Date1            ...)
    Putting it back
    Ok
    Cutting chunk 102/120 @ 161:33(5202)-163:12(5253) ("\r\n    MOVE YYYYMonth    ...)
    Putting it back
    Ok
    Cutting chunk 20/120 @ 24:46(1020)-25:46(1071) ("XX.\r\n    02 SecondDatePr...)
    Putting it back
    Ok
    Cutting chunk 30/120 @ 38:27(1530)-39:34(1581) ("       VALUE 5.\r\n    88 ...)
    Putting it back
    Ok
    Cutting chunk 1/120 @ 1:29(51)-3:21(102) ("E\"\r\nIDENTIFICATION DIVI...)
    Putting it back
    Ok
    Cutting chunk 25/120 @ 32:43(1275)-33:50(1326) ("\n    88 DateIsNotValid   ...)
    Putting it back
    Ok
    Cutting chunk 8/120 @ 9:59(408)-11:12(459) ("een\r\n* two dates entered...)
    Putting it back
    Ok
    Cutting chunk 34/120 @ 47:13(1734)-48:31(1785) ("sNotValid TO TRUE.\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 104/120 @ 164:27(5304)-167:17(5355) ("MMYYYYDate.\r\n    EXIT PR...)
    Putting it back
    Ok
    Cutting chunk 72/120 @ 105:14(3672)-107:6(3723) (" DIVISION.\r\nPROGRAM-ID. ...)
    Putting it back
    Ok
    Cutting chunk 9/120 @ 11:12(459)-11:63(510) ("entered by the user are va...)
    Putting it back
    Ok
    Cutting chunk 61/120 @ 86:34(3111)-91:9(3162) ("e\r\n    END-IF.\r\n\r\n\r...)
    Putting it back
    Ok
    Cutting chunk 59/120 @ 84:16(3009)-85:7(3060) ("            BY REFERENCE V...)
    Putting it back
    Ok
    Cutting chunk 7/120 @ 9:8(357)-9:59(408) ("gram and is used to get th...)
    Putting it back
    Ok
    Cutting chunk 100/120 @ 157:41(5100)-159:44(5151) ("8).\r\n\r\nPROCEDURE DIVIS...)
    Putting it back
    Ok
    Cutting chunk 93/120 @ 146:12(4743)-148:0(4794) ("Temp.\r\n    02 DDMMDay   ...)
    Putting it back
    Ok
    Cutting chunk 31/120 @ 39:34(1581)-43:4(1632) ("VALUE 6.\r\n            \r...)
    Putting it back
    Ok
    Cutting chunk 22/120 @ 28:29(1122)-29:34(1173) ("     PIC S9(7).\r\n    02 ...)
    Putting it back
    Ok
    Cutting chunk 93/120 @ 146:12(4743)-148:0(4794) ("Temp.\r\n    02 DDMMDay   ...)
    Putting it back
    Ok
    Cutting chunk 90/120 @ 140:14(4590)-141:34(4641) ("chael Coughlan.\r\n* Conve...)
    Putting it back
    Ok
    Cutting chunk 99/120 @ 155:36(5049)-157:41(5100) ("IC XX.\r\n\r\n01  DDMMYYYY...)
    Putting it back
    Ok
    Cutting chunk 33/120 @ 45:14(1683)-47:13(1734) ("tValidFirstDate UNTIL Date...)
    Putting it back
    Ok
    Cutting chunk 116/120 @ 188:6(5916)-190:24(5967) ("\r\n   COMPUTE Difference ...)
    Putting it back
    Ok
    Cutting chunk 45/120 @ 60:34(2295)-62:25(2346) ("rn.\r\n    DISPLAY SPACES....)
    Putting it back
    Ok
    Cutting chunk 46/120 @ 62:25(2346)-63:16(2397) ("ce between \" FirstDatePrn...)
    Putting it back
    Ok
    Cutting chunk 99/120 @ 155:36(5049)-157:41(5100) ("IC XX.\r\n\r\n01  DDMMYYYY...)
    Putting it back
    Ok
    Cutting chunk 55/120 @ 79:7(2805)-81:17(2856) ("dSecondDate.\r\n    DISPLA...)
    Putting it back
    Ok
    Cutting chunk 33/120 @ 45:14(1683)-47:13(1734) ("tValidFirstDate UNTIL Date...)
    Putting it back
    Ok
    Cutting chunk 43/120 @ 58:14(2193)-59:20(2244) ("fference TO DayDifferenceP...)
    Putting it back
    Ok
    Cutting chunk 54/120 @ 75:10(2754)-79:7(2805) ("RFORM DisplayErrorMessage\...)
    Putting it back
    Ok
    Cutting chunk 99/120 @ 155:36(5049)-157:41(5100) ("IC XX.\r\n\r\n01  DDMMYYYY...)
    Putting it back
    Ok
    Cutting chunk 44/120 @ 59:20(2244)-60:34(2295) ("O FirstDatePrn.\r\n    MOV...)
    Putting it back
    Ok
    Cutting chunk 81/120 @ 121:24(4131)-122:31(4182) ("           PIC XX.\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 39/120 @ 53:52(1989)-54:40(2040) ("econdDate\r\n             ...)
    Putting it back
    Ok
    Cutting chunk 50/120 @ 70:42(2550)-71:17(2601) ("YYYY format \" WITH NO ADV...)
    Putting it back
    Ok
    Cutting chunk 77/120 @ 114:37(3927)-115:42(3978) ("C XXXX.\r\n    02 YYYYMont...)
    Putting it back
    Ok
    Cutting chunk 97/120 @ 153:24(4947)-154:29(4998) ("           PIC XXXX.\r\n  ...)
    Putting it back
    Ok
    Cutting chunk 115/120 @ 187:7(5865)-188:6(5916) ("RE DIVISION USING Date1, D...)
    Putting it back
    Ok
    Cutting chunk 111/120 @ 179:3(5661)-181:15(5712) ("IRONMENT DIVISION.\r\nDATA...)
    Putting it back
    Ok
    Cutting chunk 92/120 @ 144:3(4692)-146:12(4743) ("A DIVISION.\r\nWORKING-STO...)
    Putting it back
    Ok
    Cutting chunk 6/120 @ 8:12(306)-9:8(357) ("yDiff\" program is include...)
    Putting it back
    Ok
    Cutting chunk 36/120 @ 50:25(1836)-51:17(1887) ("ate\" USING FirstDate, Fir...)
    Putting it back
    Ok
    Cutting chunk 79/120 @ 118:3(4029)-120:17(4080) ("KAGE SECTION.\r\n01  DDMMY...)
    Putting it back
    Ok
    Cutting chunk 119/120 @ 195:2(6069)-197:0(6093) ("D PROGRAM DayDriver.\r\n\r\n")
    Putting it back
    Ok
    Cutting chunk 45/120 @ 60:34(2295)-62:25(2346) ("rn.\r\n    DISPLAY SPACES....)
    Putting it back
    Ok
    Cutting chunk 23/120 @ 29:34(1173)-31:34(1224) ("PIC ----,--9.\r\n\r\n01  V...)
    Putting it back
    Ok
    Cutting chunk 40/120 @ 54:40(2040)-56:32(2091) (" DayDifference.\r\n\r\n   ...)
    Putting it back
    Ok
    Cutting chunk 67/120 @ 95:50(3417)-96:32(3468) ("tains all zeros.\"\r\n    ...)
    Putting it back
    Ok
    Cutting chunk 15/120 @ 15:41(765)-18:9(816) ("uroDate\" is used to conve...)
    Putting it back
    Ok
    Cutting chunk 1/120 @ 1:29(51)-3:21(102) ("E\"\r\nIDENTIFICATION DIVI...)
    Putting it back
    Ok
    Cutting chunk 15/120 @ 15:41(765)-18:9(816) ("uroDate\" is used to conve...)
    Putting it back
    Ok
    Cutting chunk 80/120 @ 120:17(4080)-121:24(4131) ("                  PIC XX.\...)
    Putting it back
    Ok
    Cutting chunk 84/120 @ 126:37(4284)-128:26(4335) (", YYYYDDMMDate.\r\nBegin.\...)
    Putting it back
    Ok
    Cutting chunk 78/120 @ 115:42(3978)-118:3(4029) ("\r\n    02 YYYYDay        ...)
    Putting it back
    Ok
    Cutting chunk 72/120 @ 105:14(3672)-107:6(3723) (" DIVISION.\r\nPROGRAM-ID. ...)
    Putting it back
    Ok
    Cutting chunk 74/120 @ 108:27(3774)-110:19(3825) ("YY format to one in YYYYMM...)
    Putting it back
    Ok
    Cutting chunk 75/120 @ 110:19(3825)-113:5(3876) ("N.\r\nDATA DIVISION.\r\nWO...)
    Putting it back
    Ok
    Cutting chunk 76/120 @ 113:5(3876)-114:37(3927) ("YYYDDMMTemp.\r\n    02 YYY...)
    Putting it back
    Ok
    Cutting chunk 6/120 @ 8:12(306)-9:8(357) ("yDiff\" program is include...)
    Putting it back
    Ok
    Cutting chunk 79/120 @ 118:3(4029)-120:17(4080) ("KAGE SECTION.\r\n01  DDMMY...)
    Putting it back
    Ok
    Cutting chunk 92/120 @ 144:3(4692)-146:12(4743) ("A DIVISION.\r\nWORKING-STO...)
    Putting it back
    Ok
    Cutting chunk 27/120 @ 35:6(1377)-36:13(1428) (" YearContainsZeros        ...)
    Putting it back
    Ok
    Cutting chunk 76/120 @ 113:5(3876)-114:37(3927) ("YYYDDMMTemp.\r\n    02 YYY...)
    Putting it back
    Ok
    Cutting chunk 108/120 @ 174:53(5508)-175:48(5559) ("o\r\n* Dates. The dates mu...)
    Putting it back
    Ok
    Cutting chunk 118/120 @ 190:75(6018)-195:2(6069) ("\r\n   EXIT PROGRAM.\r\n\r...)
    Putting it back
    Ok
    Cutting chunk 106/120 @ 172:3(5406)-174:2(5457) ("GRAM-ID. GetDayDiff.\r\nAU...)
    Putting it back
    Ok
    Cutting chunk 13/120 @ 14:18(663)-14:69(714) ("ortDate\" subprogram is us...)
    Putting it back
    Ok
    Cutting chunk 108/120 @ 174:53(5508)-175:48(5559) ("o\r\n* Dates. The dates mu...)
    Putting it back
    Ok
    Cutting chunk 116/120 @ 188:6(5916)-190:24(5967) ("\r\n   COMPUTE Difference ...)
    Putting it back
    Ok
    Cutting chunk 80/120 @ 120:17(4080)-121:24(4131) ("                  PIC XX.\...)
    Putting it back
    Ok
    Cutting chunk 6/120 @ 8:12(306)-9:8(357) ("yDiff\" program is include...)
    Putting it back
    Ok
    Cutting chunk 65/120 @ 94:16(3315)-94:67(3366) ("ContainsZeros   DISPLAY \"...)
    Putting it back
    Ok
    Cutting chunk 56/120 @ 81:17(2856)-81:68(2907) ("r the second date in DDMMY...)
    Putting it back
    Ok
    Cutting chunk 43/120 @ 58:14(2193)-59:20(2244) ("fference TO DayDifferenceP...)
    Putting it back
    Ok
    Cutting chunk 92/120 @ 144:3(4692)-146:12(4743) ("A DIVISION.\r\nWORKING-STO...)
    Putting it back
    Ok
    Cutting chunk 98/120 @ 154:29(4998)-155:36(5049) ("      PIC XX.\r\n    02 YY...)
    Putting it back
    Ok
    Cutting chunk 55/120 @ 79:7(2805)-81:17(2856) ("dSecondDate.\r\n    DISPLA...)
    Putting it back
    Ok
    Cutting chunk 110/120 @ 176:49(5610)-179:3(5661) ("cond\r\n* Date and the dif...)
    Putting it back
    Ok
    Cutting chunk 62/120 @ 91:9(3162)-91:60(3213) ("AY \"Invalid date . Return...)
    Putting it back
    Ok
    Cutting chunk 71/120 @ 98:59(3621)-105:14(3672) ("onth.\"\r\n    END-EVALUAT...)
    Putting it back
    Ok
|}];;
