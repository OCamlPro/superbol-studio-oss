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

let show_last_ptree i n ptree diags =
  if i = n then Test_appending.show_ptree i n ptree diags

let%expect_test "line-by-line-incremental-mf" =
  (* find . \( -type f -a -iname '*.cbl' \) -printf '%s %p\n' | sort -nr | head *)
  (* 6554 ./ReportWriter/RepWriteSumm.cbl *)
  (* 6543 ./ReportWriter/RepWriteFull.cbl *)
  (* 6093 ./SubProg/DayDiff/DayDiffDriver.cbl *)
  (* 4493 ./SubProg/Multiply/DriverProg.cbl *)
  (* 4477 ./Strings/RefMod.cbl *)
  (* 3821 ./ReportWriter/RepWriteB.cbl *)
  (* 3156 ./SeqRpt/SeqRpt.CBL *)
  (* 3142 ./ReportWriter/RepWriteA.cbl *)
  (* 3087 ./Strings/UnstringFileEg.cbl *)
  (* 2413 ./SubProg/DateValid/ValiDate.cbl *)
  let config =
    Testsuite_utils.from_dialect ~strict:true Cobol_config.DIALECT.MicroFocus in
  deep_iter mf_root ~glob:"RepWriteSumm.[cC][bB][lL]" (* <- pick largest file *)
    ~f:begin fun path ->
      let file = srcdir // mf_testsuite // path in
      Pretty.out "Considering `%s'.@." file;
      Parser_testing.iteratively_append_chunks ~config ~f:show_last_ptree @@
      Parser_testing.extract_position_markers @@
      Parser_testing.insert_periodic_position_markers ~period:42 @@
      FileString.read_file file;
    end;
  end_with_postproc [%expect.output];
  [%expect {|
    Loading configuration from
    `__srcdir__/import/gnucobol/config/mf-strict.conf'
    Considering `__srcdir__/test/testsuite/microfocus/www.csis.ul.ie/ReportWriter/RepWriteSumm.cbl'.
    Appending chunk 1/157 @ 0:0-1:10 ("      $ SET SOURCEFORMAT\"...)
    Appending chunk 2/157 @ 1:10-2:26 ("TION DIVISION.\r\nPROGRAM-...)
    Appending chunk 3/157 @ 2:26-5:2 ("Summary.\r\nAUTHOR.  Micha...)
    Appending chunk 4/157 @ 5:2-6:21 ("VIRONMENT DIVISION.\r\nINP...)
    Appending chunk 5/157 @ 6:21-8:25 ("\r\nFILE-CONTROL.\r\n    S...)
    Appending chunk 6/157 @ 8:25-9:21 ("GN TO \"GBSALES.DAT\"\r\n ...)
    Appending chunk 7/157 @ 9:21-10:18 ("ON IS LINE SEQUENTIAL.\r\n...)
    Appending chunk 8/157 @ 10:18-12:0 ("le ASSIGN TO \"SUMMARYSALE...)
    Appending chunk 9/157 @ 12:0-15:9 ("\r\nDATA DIVISION.\r\nFILE...)
    Appending chunk 10/157 @ 15:9-17:17 ("File.\r\n01  SalesRecord.\...)
    Appending chunk 11/157 @ 17:17-18:21 (" VALUE HIGH-VALUES.\r\n   ...)
    Appending chunk 12/157 @ 18:21-19:31 ("   PIC 9.\r\n    02 SalesP...)
    Appending chunk 13/157 @ 19:31-22:1 ("\n    02 ValueOfSale      ...)
    Appending chunk 14/157 @ 22:1-24:0 ("D  PrintFile\r\n    REPORT...)
    Appending chunk 15/157 @ 24:0-26:14 ("\r\nWORKING-STORAGE SECTIO...)
    Appending chunk 16/157 @ 26:14-28:19 ("\r\n    02 TableValues.\r\...)
    Appending chunk 17/157 @ 28:19-28:61 ("     PIC X(18) VALUE \"Dub...)
    Appending chunk 18/157 @ 28:61-29:40 ("\r\n       03 FILLER      ...)
    Appending chunk 19/157 @ 29:40-30:19 ("\"Cork     Galway   \".\r\...)
    Appending chunk 20/157 @ 30:19-30:61 ("     PIC X(18) VALUE \"Sli...)
    Appending chunk 21/157 @ 30:61-31:40 ("\r\n       03 FILLER      ...)
    Appending chunk 22/157 @ 31:40-32:29 ("\"Limerick\".\r\n    02 FI...)
    Appending chunk 23/157 @ 32:29-33:33 ("Values.\r\n       03 CityN...)
    Appending chunk 24/157 @ 33:33-36:8 ("CCURS 7 TIMES.\r\n\r\n01  ...)
    Appending chunk 25/157 @ 36:8-37:29 ("ableValues.\r\n       03 F...)
    Appending chunk 26/157 @ 37:29-38:36 ("(35)\r\n                  ...)
    Appending chunk 27/157 @ 38:36-39:8 ("32100435005670012300234003...)
    Appending chunk 28/157 @ 39:8-40:15 ("3 FILLER        PIC X(35)\...)
    Appending chunk 29/157 @ 40:15-40:57 ("         VALUE \"123005430...)
    Appending chunk 30/157 @ 40:57-41:29 ("220013300\".\r\n       03 ...)
    Appending chunk 31/157 @ 41:29-42:36 ("(35)\r\n                  ...)
    Appending chunk 32/157 @ 42:36-43:8 ("32100176001870013300144001...)
    Appending chunk 33/157 @ 43:8-44:15 ("3 FILLER        PIC X(35)\...)
    Appending chunk 34/157 @ 44:15-44:57 ("         VALUE \"321001230...)
    Appending chunk 35/157 @ 44:57-45:29 ("770018800\".\r\n       03 ...)
    Appending chunk 36/157 @ 45:29-46:36 ("(35)\r\n                  ...)
    Appending chunk 37/157 @ 46:36-47:8 ("34500456005430011100122001...)
    Appending chunk 38/157 @ 47:8-48:15 ("3 FILLER        PIC X(35)\...)
    Appending chunk 39/157 @ 48:15-48:57 ("         VALUE \"190001800...)
    Appending chunk 40/157 @ 48:57-49:29 ("330022200\".\r\n       03 ...)
    Appending chunk 41/157 @ 49:29-50:36 ("(35)\r\n                  ...)
    Appending chunk 42/157 @ 50:36-51:8 ("15600145001460022200111002...)
    Appending chunk 43/157 @ 51:8-52:15 ("3 FILLER        PIC X(35)\...)
    Appending chunk 44/157 @ 52:15-52:57 ("         VALUE \"120001320...)
    Appending chunk 45/157 @ 52:57-53:29 ("210043200\".\r\n       03 ...)
    Appending chunk 46/157 @ 53:29-54:36 ("(35)\r\n                  ...)
    Appending chunk 47/157 @ 54:36-56:6 ("16500164001760011100777003...)
    Appending chunk 48/157 @ 56:6-57:10 (" FILLER REDEFINES TableVal...)
    Appending chunk 49/157 @ 57:10-58:20 ("City OCCURS 7 TIMES.\r\n  ...)
    Appending chunk 50/157 @ 58:20-60:7 ("te  PIC 9(3)V99 OCCURS 9 T...)
    Appending chunk 51/157 @ 60:7-61:29 ("cVariables.\r\n    02 Comm...)
    Appending chunk 52/157 @ 61:29-62:33 ("(4)V99.\r\n    02 Percenta...)
    Appending chunk 53/157 @ 62:33-63:31 ("ALUE .05.\r\n    02 Salary...)
    Appending chunk 54/157 @ 63:31-65:3 (")V99.\r\n    02 SalesPerso...)
    Appending chunk 55/157 @ 65:3-68:9 (" 02 CityNow          PIC 9...)
    Appending chunk 56/157 @ 68:9-70:17 ("CTION.\r\nRD  SalesReport\...)
    Appending chunk 57/157 @ 70:17-72:9 ("FINAL\r\n                C...)
    Appending chunk 58/157 @ 72:9-73:18 ("       SalesPersonNum \r\n...)
    Appending chunk 59/157 @ 73:18-76:3 ("66\r\n    HEADING 1\r\n   ...)
    Appending chunk 60/157 @ 76:3-79:6 (" LAST DETAIL 42\r\n    FOO...)
    Appending chunk 61/157 @ 79:6-81:5 ("PE IS PAGE HEADING.\r\n   ...)
    Appending chunk 62/157 @ 81:5-82:12 ("  03 COLUMN 12     PIC X(3...)
    Appending chunk 63/157 @ 82:12-82:54 ("            VALUE \"An exa...)
    Appending chunk 64/157 @ 82:54-85:12 (" Program\".\r\n\r\n    02 ...)
    Appending chunk 65/157 @ 85:12-86:19 ("LUMN 6      PIC X(17)\r\n ...)
    Appending chunk 66/157 @ 86:19-87:23 ("ble Salesperson\".\r\n    ...)
    Appending chunk 67/157 @ 87:23-88:30 (" PIC X(26)\r\n          VA...)
    Appending chunk 68/157 @ 88:30-91:7 ("Salary Report\".\r\n\r\n  ...)
    Appending chunk 69/157 @ 91:7-92:1 ("03 COLUMN 2      PIC X(4) ...)
    Appending chunk 70/157 @ 92:1-92:43 ("      03 COLUMN 12     PIC...)
    Appending chunk 71/157 @ 92:43-93:29 ("lesperson\".\r\n       03 ...)
    Appending chunk 72/157 @ 93:29-96:5 ("(4) VALUE \"Sale\".\r\n\r\...)
    Appending chunk 73/157 @ 96:5-96:47 ("  03 COLUMN 2      PIC X(4...)
    Appending chunk 74/157 @ 96:47-97:41 ("\n       03 COLUMN 13     ...)
    Appending chunk 75/157 @ 97:41-98:33 ("umber\".\r\n       03 COLU...)
    Appending chunk 76/157 @ 98:33-101:22 ("VALUE \"Value\".\r\n\r\n\r...)
    Appending chunk 77/157 @ 101:22-103:8 (" DETAIL.\r\n    02 LINE IS...)
    Appending chunk 78/157 @ 103:8-104:16 ("3 COLUMN 1      PIC X(9)\r...)
    Appending chunk 79/157 @ 104:16-104:58 ("        SOURCE CityName(Ci...)
    Appending chunk 80/157 @ 104:58-106:2 ("DICATE.\r\n       03 COLUM...)
    Appending chunk 81/157 @ 106:2-106:44 ("                      SOUR...)
    Appending chunk 82/157 @ 106:44-107:22 ("m  GROUP INDICATE.\r\n    ...)
    Appending chunk 83/157 @ 107:22-109:1 ("  PIC $$,$$$.99 SOURCE Val...)
    Appending chunk 84/157 @ 109:1-111:21 ("\n01  SalesPersonGrp\r\n  ...)
    Appending chunk 85/157 @ 111:21-111:63 ("OOTING SalesPersonNum  NEX...)
    Appending chunk 86/157 @ 111:63-113:17 ("\n    02 LINE IS PLUS 1.\r...)
    Appending chunk 87/157 @ 113:17-113:59 ("15     PIC X(21) VALUE \"S...)
    Appending chunk 88/157 @ 113:59-114:35 ("son\".\r\n       03 COLUMN...)
    Appending chunk 89/157 @ 114:35-115:23 ("E SalesPersonNum.\r\n     ...)
    Appending chunk 90/157 @ 115:23-116:23 (" PIC X VALUE \"=\".\r\n   ...)
    Appending chunk 91/157 @ 116:23-118:4 (" PIC $$$$$,$$$.99 SUM Valu...)
    Appending chunk 92/157 @ 118:4-119:22 ("02 LINE IS PLUS 1.\r\n    ...)
    Appending chunk 93/157 @ 119:22-120:0 ("  PIC X(19) VALUE \"Sales ...)
    Appending chunk 94/157 @ 120:0-121:0 ("       03 COLUMN 43     PI...)
    Appending chunk 95/157 @ 121:0-121:42 ("       03 COLUMN 45     PI...)
    Appending chunk 96/157 @ 121:42-123:21 ("OURCE Commission.\r\n\r\n ...)
    Appending chunk 97/157 @ 123:21-124:39 (".\r\n       03 COLUMN 15  ...)
    Appending chunk 98/157 @ 124:39-125:15 (" \"Salesperson salary is\"...)
    Appending chunk 99/157 @ 125:15-126:15 ("N 43     PIC X VALUE \"=\"...)
    Appending chunk 100/157 @ 126:15-127:0 ("N 45     PIC $$$$$,$$$.99 ...)
    Appending chunk 101/157 @ 127:0-129:16 ("\r\n    02 LINE IS PLUS 1....)
    Appending chunk 102/157 @ 129:16-130:23 (" 15     PIC X(30)\r\n     ...)
    Appending chunk 103/157 @ 130:23-131:0 (" VALUE \"Current  salesper...)
    Appending chunk 104/157 @ 131:0-131:42 ("       03 COLUMN 45     PI...)
    Appending chunk 105/157 @ 131:42-134:4 ("PersonNow.\r\n\r\n    02 L...)
    Appending chunk 106/157 @ 134:4-135:11 ("   03 COLUMN 15     PIC X(...)
    Appending chunk 107/157 @ 135:11-135:53 ("             VALUE \"Previ...)
    Appending chunk 108/157 @ 135:53-136:30 ("umber = \".\r\n       03 C...)
    Appending chunk 109/157 @ 136:30-140:12 ("SOURCE SalesPersonNum.\r\n...)
    Appending chunk 110/157 @ 140:12-140:54 ("TYPE IS CONTROL FOOTING Ci...)
    Appending chunk 111/157 @ 140:54-142:7 ("P PLUS 2.\r\n    02 LINE I...)
    Appending chunk 112/157 @ 142:7-142:49 ("03 COLUMN 15     PIC X(9) ...)
    Appending chunk 113/157 @ 142:49-143:38 ("\".\r\n       03 COLUMN 25...)
    Appending chunk 114/157 @ 143:38-144:19 ("E CityName(CityCode).\r\n ...)
    Appending chunk 115/157 @ 144:19-145:19 ("     PIC X VALUE \"=\".\r\...)
    Appending chunk 116/157 @ 145:19-147:8 (" 45  PIC $$$$$,$$$.99 SUM ...)
    Appending chunk 117/157 @ 147:8-148:26 ("INE IS PLUS 1.\r\n       0...)
    Appending chunk 118/157 @ 148:26-149:33 ("C X(12)\r\n               ...)
    Appending chunk 119/157 @ 149:33-150:28 ("rrent city\".\r\n       03...)
    Appending chunk 120/157 @ 150:28-151:28 ("X VALUE \"=\".\r\n       0...)
    Appending chunk 121/157 @ 151:28-153:21 ("9 SOURCE CityNow.\r\n\r\n ...)
    Appending chunk 122/157 @ 153:21-155:4 (".\r\n       03 COLUMN 15  ...)
    Appending chunk 123/157 @ 155:4-155:46 ("                    VALUE ...)
    Appending chunk 124/157 @ 155:46-156:40 ("\r\n       03 COLUMN 43   ...)
    Appending chunk 125/157 @ 156:40-157:40 ("\r\n       03 COLUMN 45   ...)
    Appending chunk 126/157 @ 157:40-160:28 ("ityCode.\r\n\r\n\r\n01  To...)
    Appending chunk 127/157 @ 160:28-161:20 ("NTROL FOOTING FINAL.\r\n  ...)
    Appending chunk 128/157 @ 161:20-163:3 ("4.\r\n       03 COLUMN 15 ...)
    Appending chunk 129/157 @ 163:3-163:45 ("                     VALUE...)
    Appending chunk 130/157 @ 163:45-164:41 ("\n       03 COLUMN 43     ...)
    Appending chunk 131/157 @ 164:41-165:41 ("\n       03 COLUMN 45     ...)
    Appending chunk 132/157 @ 165:41-169:2 ("SUM CS.\r\n\r\n\r\n01  TYP...)
    Appending chunk 133/157 @ 169:2-170:24 ("  02 LINE IS 53.\r\n      ...)
    Appending chunk 134/157 @ 170:24-170:66 ("PIC X(29) VALUE \"Programm...)
    Appending chunk 135/157 @ 170:66-171:34 ("hlan\".\r\n       03 COLUM...)
    Appending chunk 136/157 @ 171:34-172:26 ("ALUE \"Page :\".\r\n      ...)
    Appending chunk 137/157 @ 172:26-175:11 ("C Z9 SOURCE PAGE-COUNTER.\...)
    Appending chunk 138/157 @ 175:11-178:2 ("IVISION.\r\nDECLARATIVES.\...)
    Appending chunk 139/157 @ 178:2-179:2 ("  USE BEFORE REPORTING Sal...)
    Appending chunk 140/157 @ 179:2-180:25 ("lculate-Salary.\r\n    MUL...)
    Appending chunk 141/157 @ 180:25-181:35 ("ntage\r\n          GIVING ...)
    Appending chunk 142/157 @ 181:35-182:39 (".\r\n    ADD Commission, F...)
    Appending chunk 143/157 @ 182:39-183:24 ("SalesPersonNum )\r\n      ...)
    Appending chunk 144/157 @ 183:24-187:4 ("\r\nEND DECLARATIVES.\r\n\...)
    Appending chunk 145/157 @ 187:4-189:11 ("n.\r\n    OPEN INPUT Sales...)
    Appending chunk 146/157 @ 189:11-191:5 ("TPUT PrintFile.\r\n    REA...)
    Appending chunk 147/157 @ 191:5-192:8 ("    AT END SET EndOfFile T...)
    Appending chunk 148/157 @ 192:8-194:8 ("READ.\r\n    INITIATE Sale...)
    Appending chunk 149/157 @ 194:8-195:19 ("ORM PrintSalaryReport\r\n ...)
    Appending chunk 150/157 @ 195:19-197:3 ("ndOfFile.\r\n    TERMINATE...)
    Appending chunk 151/157 @ 197:3-198:12 (" CLOSE SalesFile, PrintFil...)
    Appending chunk 152/157 @ 198:12-202:15 (".\r\n\r\n\r\nPrintSalaryRe...)
    Appending chunk 153/157 @ 202:15-203:26 ("de TO CityNow.\r\n    MOVE...)
    Appending chunk 154/157 @ 203:26-204:23 ("O SalesPersonNow.\r\n    G...)
    Appending chunk 155/157 @ 204:23-206:18 ("t.\r\n    READ SalesFile\r...)
    Appending chunk 156/157 @ 206:18-210:1 ("ET EndOfFile TO TRUE\r\n  ...)
    Appending chunk 157/157 @ 210:1-211:0 ("\r\n")
    Parse-tree:
      IDENTIFICATION DIVISION.
      PROGRAM-ID. REPORTEXAMPLESUMMARY.
      AUTHOR. MICHAEL COUGHLAN .
      ENVIRONMENT DIVISION.
      INPUT-OUTPUT SECTION.
      FILE-CONTROL.
      SELECT SALESFILE ASSIGN "GBSALES.DAT" SEQUENTIAL.
      SELECT PRINTFILE ASSIGN "SUMMARYSALESREPORT.LPT".
      DATA DIVISION.
        FILE SECTION.
        FD SALESFILE.
        01 SALESRECORD.
          88 ENDOFFILE VALUE HIGH-VALUES.
          02 CITYCODE PIC 9.
          02 SALESPERSONNUM PIC 9.
          02 VALUEOFSALE PIC 9(4)V99.
        FD PRINTFILE
          REPORT IS SALESREPORT.
        WORKING-STORAGE SECTION.
        01 NAMETABLE.
          02 TABLEVALUES.
            03 FILLER PIC X(18) VALUE "Dublin   Belfast  ".
            03 FILLER PIC X(18) VALUE "Cork     Galway   ".
            03 FILLER PIC X(18) VALUE "Sligo    Waterford".
            03 FILLER PIC X(9) VALUE "Limerick".
          02 FILLER REDEFINES TABLEVALUES.
            03 CITYNAME PIC X(9) OCCURS 7.
        01 RATETABLE.
          02 TABLEVALUES.
            03 FILLER PIC X(35) VALUE "12300321004350056700123002340034500".
            03 FILLER PIC X(35) VALUE "12300543001230034200111001220013300".
            03 FILLER PIC X(35) VALUE "12000321001760018700133001440015500".
            03 FILLER PIC X(35) VALUE "32100123003210012000166001770018800".
            03 FILLER PIC X(35) VALUE "34500345004560054300111001220013200".
            03 FILLER PIC X(35) VALUE "19000180001780017900444003330022200".
            03 FILLER PIC X(35) VALUE "16700156001450014600222001110021200".
            03 FILLER PIC X(35) VALUE "12000132001230014300121003210043200".
            03 FILLER PIC X(35) VALUE "15400165001640017600111007770033300".
          02 FILLER REDEFINES TABLEVALUES.
            03 CITY OCCURS 7.
              04 FIXEDRATE PIC 9(3)V99 OCCURS 9.
        01 MISCVARIABLES.
          02 COMMISSION PIC 9(4)V99.
          02 PERCENTAGE PIC V99 VALUE .05.
          02 SALARY PIC 9(6)V99.
          02 SALESPERSONNOW PIC 9.
          02 CITYNOW PIC 9.
        REPORT SECTION.
        RD SALESREPORT
          CONTROL FINAL CITYCODE
          SALESPERSONNUM
          PAGE LIMIT IS 66 LINES
          HEADING 1
          FIRST DETAIL 6
          LAST DETAIL 42
          FOOTING 52.
        01 TYPE PH.
          02 LINE NUMBER 1.
            03 COLUMN LEFT 12 PIC X(32) VALUE "An example COBOL Report Program".
          02 LINE NUMBER 2.
            03 COLUMN LEFT 6 PIC X(17) VALUE "Bible Salesperson".
            03 COLUMN LEFT 23 PIC X(26) VALUE " - Sales and Salary Report".
          02 LINE NUMBER 4.
            03 COLUMN LEFT 2 PIC X(4) VALUE "City".
            03 COLUMN LEFT 12 PIC X(11) VALUE "Salesperson".
            03 COLUMN LEFT 28 PIC X(4) VALUE "Sale".
          02 LINE NUMBER 5.
            03 COLUMN LEFT 2 PIC X(4) VALUE "Name".
            03 COLUMN LEFT 13 PIC X(6) VALUE "Number".
            03 COLUMN LEFT 28 PIC X(5) VALUE "Value".
        01 DETAILLINE TYPE DETAIL.
          02 LINE NUMBER PLUS 1.
            03 COLUMN LEFT 1 PIC X(9) SOURCE CITYNAME(CITYCODE)  GROUP.
            03 COLUMN LEFT 15 PIC 9 SOURCE SALESPERSONNUM  GROUP.
            03 COLUMN LEFT 25 PIC $$,$$$.99 SOURCE VALUEOFSALE .
        01 SALESPERSONGRP TYPE CF FOR SALESPERSONNUM NEXT GROUP IS PLUS 2.
          02 LINE NUMBER PLUS 1.
            03 COLUMN LEFT 15 PIC X(21) VALUE "Sales for salesperson".
            03 COLUMN LEFT 37 PIC 9 SOURCE SALESPERSONNUM .
            03 COLUMN LEFT 43 PIC X VALUE "=".
            03 SMS COLUMN LEFT 45 PIC $$$$$,$$$.99 SUM VALUEOFSALE .
          02 LINE NUMBER PLUS 1.
            03 COLUMN LEFT 15 PIC X(19) VALUE "Sales commission is".
            03 COLUMN LEFT 43 PIC X VALUE "=".
            03 COLUMN LEFT 45 PIC $$$$$,$$$.99 SOURCE COMMISSION .
          02 LINE NUMBER PLUS 1.
            03 COLUMN LEFT 15 PIC X(22) VALUE "Salesperson salary is".
            03 COLUMN LEFT 43 PIC X VALUE "=".
            03 COLUMN LEFT 45 PIC $$$$$,$$$.99 SOURCE SALARY .
          02 LINE NUMBER PLUS 1.
            03 COLUMN LEFT 15 PIC X(30) VALUE "Current  salesperson number = ".
            03 COLUMN LEFT 45 PIC 9 SOURCE SALESPERSONNOW .
          02 LINE NUMBER PLUS 1.
            03 COLUMN LEFT 15 PIC X(30) VALUE "Previous salesperson number = ".
            03 COLUMN LEFT 45 PIC 9 SOURCE SALESPERSONNUM .
        01 CITYGRP TYPE CF FOR CITYCODE NEXT GROUP IS PLUS 2.
          02 LINE NUMBER PLUS 2.
            03 COLUMN LEFT 15 PIC X(9) VALUE "Sales for".
            03 COLUMN LEFT 25 PIC X(9) SOURCE CITYNAME(CITYCODE) .
            03 COLUMN LEFT 43 PIC X VALUE "=".
            03 CS COLUMN LEFT 45 PIC $$$$$,$$$.99 SUM SMS .
          02 LINE NUMBER PLUS 1.
            03 COLUMN LEFT 15 PIC X(12) VALUE "Current city".
            03 COLUMN LEFT 43 PIC X VALUE "=".
            03 COLUMN LEFT 45 PIC 9 SOURCE CITYNOW .
          02 LINE NUMBER PLUS 1.
            03 COLUMN LEFT 15 PIC X(13) VALUE "Previous city".
            03 COLUMN LEFT 43 PIC X VALUE "=".
            03 COLUMN LEFT 45 PIC 9 SOURCE CITYCODE .
        01 TOTALSALESGRP TYPE CF FOR FINAL.
          02 LINE NUMBER PLUS 4.
            03 COLUMN LEFT 15 PIC X(11) VALUE "Total sales".
            03 COLUMN LEFT 43 PIC X VALUE "=".
            03 COLUMN LEFT 45 PIC $$$$$,$$$.99 SUM CS .
        01 TYPE PF.
          02 LINE NUMBER 53.
            03 COLUMN LEFT 1 PIC X(29) VALUE "Programmer - Michael Coughlan".
            03 COLUMN LEFT 45 PIC X(6) VALUE "Page :".
            03 COLUMN LEFT 52 PIC Z9 SOURCE PAGE-COUNTER .
      PROCEDURE DIVISION.
      DECLARATIVES.
      CALC SECTION.
        USE BEFORE REPORTING SALESPERSONGRP.
      CALCULATE-SALARY.
      MULTIPLY SMS BY PERCENTAGE GIVING COMMISSION ROUNDED END-MULTIPLY.
      ADD COMMISSION TO FIXEDRATE(CITYCODE, SALESPERSONNUM) GIVING SALARY END-ADD.
      END DECLARATIVES.
        MAIN SECTION.

        BEGIN.
        OPEN INPUT SALESFILE.

        OPEN OUTPUT PRINTFILE.

        READ SALESFILE AT END SET ENDOFFILE TO TRUE END-READ.

        INITIATE SALESREPORT.

        PERFORM PRINTSALARYREPORT UNTIL ENDOFFILE.

        TERMINATE SALESREPORT.

        CLOSE SALESFILE PRINTFILE.

        STOP RUN.
        PRINTSALARYREPORT.
        MOVE CITYCODE TO CITYNOW.

        MOVE SALESPERSONNUM TO SALESPERSONNOW.

        GENERATE SALESREPORT.

        READ SALESFILE AT END SET ENDOFFILE TO TRUE END-READ.
|}];;
