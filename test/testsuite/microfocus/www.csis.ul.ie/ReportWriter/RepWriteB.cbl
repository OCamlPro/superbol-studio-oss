      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  ReportExampleB.
AUTHOR.  Michael Coughlan.
* A simplified version of full program
* Contains all the control breaks but no declaratives

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT SalesFile ASSIGN TO "GBSALES.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
    SELECT PrintFile ASSIGN TO "SALESREPORTB.LPT".


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


01  CityGrp TYPE IS CONTROL FOOTING CityCode NEXT GROUP PLUS 2.
    02 LINE IS PLUS 2.
       03 COLUMN 15     PIC X(9) VALUE "Sales for".
       03 COLUMN 25     PIC X(9) SOURCE CityName(CityCode).
       03 COLUMN 43     PIC X VALUE "=".
       03 CS COLUMN 45  PIC $$$$$,$$$.99 SUM SMS.


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
    GENERATE DetailLine.
    READ SalesFile
          AT END SET EndOfFile TO TRUE
    END-READ.


	
