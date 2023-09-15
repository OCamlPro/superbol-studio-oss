      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. DateDriver.
AUTHOR.  Michael Coughlan.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 FILLER                   PIC 9 VALUE 0.
   88 EndOfInput            VALUE 1.

01 InputDateIn              PIC 9(8).


01 ValidationResult         PIC 9.
   88 DateIsValid           VALUE 0.
   88 DateNotNumeric        VALUE 1.
   88 YearContainsZeros     VALUE 2.
   88 MonthContainsZeros    VALUE 3.
   88 DayContainsZeros      VALUE 4.
   88 MonthGreaterThan12    VALUE 5.
   88 DayTooGreatForMonth   VALUE 6.


PROCEDURE DIVISION.
Begin.
    DISPLAY "Input Date as DDMMYYYY > " WITH NO ADVANCING
    ACCEPT InputDateIn

    CALL "Validate"
          USING InputDateIn, ValidationResult.

    DISPLAY "RESULT = " ValidationResult
    DISPLAY "DATE IS NOW = " InputDateIn
    EVALUATE TRUE
       WHEN DateIsValid           DISPLAY "Date is valid."
       WHEN DateNotNumeric        DISPLAY "Date is not numeric."
       WHEN YearContainsZeros     DISPLAY "Year contains all zeros."
       WHEN MonthContainsZeros    DISPLAY "Month contains all zeros."
       WHEN DayContainsZeros      DISPLAY "Day contains all zeros."
       WHEN MonthGreaterThan12    DISPLAY "Month too great."
       WHEN DayTooGreatForMonth   DISPLAY "Day too great for month."
    END-EVALUATE.

    STOP RUN.



