      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  ReportExampleFull.
AUTHOR.  Michael Coughlan.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT SalesFile ASSIGN TO "GBSALES.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
    SELECT PrintFile ASSIGN TO "SALESREPORT.LPT".


DATA DIVISION.
FILE SECTION.
FD  SalesFile.
01  SalesRecord.
    88 EndOfFile  VALUE HIGH-VALUES.
    02 CityCode         PIC 9.
    02 SalesPersonNum   PIC 9.
    02 ValueOfSale      PIC 9(4)V99.

FD  PrintFile
    REPORT IS SalesReport.

WORKING-STORAGE SECTION.
01  NameTable.
    02 TableValues.
       03 FILLER        PIC X(18) VALUE "Dublin   Belfast  ".
       03 FILLER        PIC X(18) VALUE "Cork     Galway   ".
       03 FILLER        PIC X(18) VALUE "Sligo    Waterford".
       03 FILLER        PIC X(9)  VALUE "Limerick".
    02 FILLER REDEFINES TableValues.
       03 CityName     PIC X(9) OCCURS 7 TIMES.

01  RateTable.
    02 TableValues.
       03 FILLER        PIC X(35)
                        VALUE "12300321004350056700123002340034500".
       03 FILLER        PIC X(35)
                        VALUE "12300543001230034200111001220013300".
       03 FILLER        PIC X(35)
                        VALUE "12000321001760018700133001440015500".
       03 FILLER        PIC X(35)
                        VALUE "32100123003210012000166001770018800".
       03 FILLER        PIC X(35)
                        VALUE "34500345004560054300111001220013200".
       03 FILLER        PIC X(35)
                        VALUE "19000180001780017900444003330022200".
       03 FILLER        PIC X(35)
                        VALUE "16700156001450014600222001110021200".
       03 FILLER        PIC X(35)
                        VALUE "12000132001230014300121003210043200".
       03 FILLER        PIC X(35)
                        VALUE "15400165001640017600111007770033300".

    02 FILLER REDEFINES TableValues.
       03 City OCCURS 7 TIMES.
          04 FixedRate  PIC 9(3)V99 OCCURS 9 TIMES.

01  MiscVariables.
    02 Commission       PIC 9(4)V99.
    02 Percentage       PIC V99 VALUE .05.
    02 Salary           PIC 9(6)V99.
    02 SalesPersonNow   PIC 9.
    02 CityNow          PIC 9.


REPORT SECTION.
RD  SalesReport
    CONTROLS ARE FINAL
                CityCode
                SalesPersonNum 
    PAGE LIMIT IS 66
    HEADING 1
    FIRST DETAIL 6
    LAST DETAIL 42
    FOOTING 52.

01  TYPE IS PAGE HEADING.
    02 LINE 1.
       03 COLUMN 12     PIC X(32)
                        VALUE "An example COBOL Report Program".

    02 LINE 2.
       03 COLUMN 6      PIC X(17)
          VALUE "Bible Salesperson".
       03 COLUMN 23     PIC X(26)
          VALUE " - Sales and Salary Report".

    02 LINE 4.
       03 COLUMN 2      PIC X(4) VALUE "City".
       03 COLUMN 12     PIC X(11) VALUE "Salesperson".
       03 COLUMN 28     PIC X(4) VALUE "Sale".

    02 LINE 5.
       03 COLUMN 2      PIC X(4) VALUE "Name".
       03 COLUMN 13     PIC X(6) VALUE "Number".
       03 COLUMN 28     PIC X(5) VALUE "Value".


01  DetailLine TYPE IS DETAIL.
    02 LINE IS PLUS 1.
       03 COLUMN 1      PIC X(9)
                        SOURCE CityName(CityCode) GROUP INDICATE.
       03 COLUMN 15     PIC 9
                        SOURCE SalesPersonNum  GROUP INDICATE.
       03 COLUMN 25     PIC $$,$$$.99 SOURCE ValueOfSale.
		

01  SalesPersonGrp
    TYPE IS CONTROL FOOTING SalesPersonNum  NEXT GROUP PLUS 2.
    02 LINE IS PLUS 1.
       03 COLUMN 15     PIC X(21) VALUE "Sales for salesperson".
       03 COLUMN 37     PIC 9 SOURCE SalesPersonNum.
       03 COLUMN 43     PIC X VALUE "=".
       03 SMS COLUMN 45 PIC $$$$$,$$$.99 SUM ValueOfSale.

    02 LINE IS PLUS 1.
       03 COLUMN 15     PIC X(19) VALUE "Sales commission is".
       03 COLUMN 43     PIC X VALUE "=".
       03 COLUMN 45     PIC $$$$$,$$$.99 SOURCE Commission.

    02 LINE IS PLUS 1.
       03 COLUMN 15     PIC X(22) VALUE "Salesperson salary is".
       03 COLUMN 43     PIC X VALUE "=".
       03 COLUMN 45     PIC $$$$$,$$$.99 SOURCE Salary.

    02 LINE IS PLUS 1.
       03 COLUMN 15     PIC X(30)
                        VALUE "Current  salesperson number = ".
       03 COLUMN 45     PIC 9 SOURCE SalesPersonNow.

    02 LINE IS PLUS 1.
       03 COLUMN 15     PIC X(30)
                        VALUE "Previous salesperson number = ".
       03 COLUMN 45     PIC 9 SOURCE SalesPersonNum.



01  CityGrp TYPE IS CONTROL FOOTING CityCode NEXT GROUP PLUS 2.
    02 LINE IS PLUS 2.
       03 COLUMN 15     PIC X(9) VALUE "Sales for".
       03 COLUMN 25     PIC X(9) SOURCE CityName(CityCode).
       03 COLUMN 43     PIC X VALUE "=".
       03 CS COLUMN 45  PIC $$$$$,$$$.99 SUM SMS.

    02 LINE IS PLUS 1.
       03 COLUMN 15     PIC X(12)
                        VALUE "Current city".
       03 COLUMN 43     PIC X VALUE "=".
       03 COLUMN 45     PIC 9 SOURCE CityNow.

    02 LINE IS PLUS 1.
       03 COLUMN 15     PIC X(13)
                        VALUE "Previous city".
       03 COLUMN 43     PIC X VALUE "=".
       03 COLUMN 45     PIC 9   SOURCE CityCode.


01  TotalSalesGrp TYPE IS CONTROL FOOTING FINAL.
    02 LINE IS PLUS 4.
       03 COLUMN 15     PIC X(11)
                        VALUE "Total sales".
       03 COLUMN 43     PIC X VALUE "=".
       03 COLUMN 45     PIC $$$$$,$$$.99 SUM CS.


01  TYPE IS PAGE FOOTING.
    02 LINE IS 53.
       03 COLUMN 1      PIC X(29) VALUE "Programmer - Michael Coughlan".
       03 COLUMN 45     PIC X(6) VALUE "Page :".
       03 COLUMN 52     PIC Z9 SOURCE PAGE-COUNTER.


PROCEDURE DIVISION.
DECLARATIVES.
Calc SECTION.
    USE BEFORE REPORTING SalesPersonGrp.
Calculate-Salary.
    MULTIPLY SMS BY Percentage
          GIVING Commission ROUNDED.
    ADD Commission, FixedRate(CityCode,SalesPersonNum )
          GIVING Salary.
END DECLARATIVES.

Main SECTION.
Begin.
    OPEN INPUT SalesFile.
    OPEN OUTPUT PrintFile.
    READ SalesFile
         AT END SET EndOfFile TO TRUE
    END-READ.
    INITIATE SalesReport.
    PERFORM PrintSalaryReport
            UNTIL EndOfFile.
    TERMINATE SalesReport.
    CLOSE SalesFile, PrintFile.
    STOP RUN.


PrintSalaryReport.
    MOVE CityCode TO CityNow.
    MOVE SalesPersonNum  TO SalesPersonNow.
    GENERATE DetailLine.
    READ SalesFile
          AT END SET EndOfFile TO TRUE
    END-READ.


	
