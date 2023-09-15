      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. Validate IS INITIAL.
AUTHOR.  Michael Coughlan.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 MonthDayTable.
   02 TableValues           PIC X(24)
            VALUE "312831303130313130313031".
   02 FILLER REDEFINES TableValues.
      03 DaysInMonth 
            OCCURS 12 TIMES PIC 99.


01 CurruptDate              PIC 9(8).

01 LeapQuot                 PIC 9(4).
01 LeapRemain               PIC 9(4).

01 FILLER                   PIC 9 VALUE ZERO.
   88 LeapYear              VALUE 1.


LINKAGE SECTION.
01 InputDateLA.
   02 DayLA                 PIC 99.
   02 MonthLA               PIC 99.
      88 MonthInvalid       VALUE 13 THRU 99.
      88 MonthIsFebruary    VALUE 2. 
   02 YearLA                PIC 9(4).

01 ValidationResultLB       PIC 9.
   88 DateIsValid           VALUE 0.
   88 DateNotNumeric        VALUE 1.
   88 YearContainsZeros     VALUE 2.
   88 MonthContainsZeros    VALUE 3.
   88 DayContainsZeros      VALUE 4.
   88 MonthGreaterThan12    VALUE 5.
   88 DayTooGreatForMonth   VALUE 6.

PROCEDURE DIVISION USING InputDateLA, ValidationResultLB.
Begin.
   EVALUATE TRUE
     WHEN InputDateLA NOT NUMERIC  SET DateNotNumeric     TO TRUE
     WHEN YearLA EQUAL TO ZEROS    SET YearContainsZeros  TO TRUE
     WHEN MonthLA EQUAL TO ZEROS   SET MonthContainsZeros TO TRUE
     WHEN DayLA EQUAL TO ZEROS     SET DayContainsZeros   TO TRUE
     WHEN MonthInvalid             SET MonthGreaterThan12 TO TRUE
     WHEN OTHER PERFORM CheckForValidDay
   END-EVALUATE

   EXIT PROGRAM.


CheckForValidDay.
*  Years evenly divisible by 4 are leap years, but
*  years evenly divisible by 100 are not leap years, but
*  years evenly divisible by 400 are leap years.

   DIVIDE YearLA BY 400 GIVING LeapQuot REMAINDER LeapRemain.
   IF LeapRemain = 0
      SET LeapYear TO TRUE
    ELSE 
      DIVIDE YearLA BY 100 GIVING LeapQuot REMAINDER LeapRemain
      IF LeapRemain NOT = 0 
         DIVIDE YearLA BY 4 GIVING LeapQuot REMAINDER LeapRemain
         IF LeapRemain = 0
            SET LeapYear TO TRUE
         END-IF
      END-IF
   END-IF

  IF LeapYear AND MonthIsFebruary 
         MOVE 29 TO DaysInMonth(2)
  END-IF
  IF DayLA GREATER THAN DaysInMonth(MonthLA)
     SET DayTooGreatForMonth TO TRUE
   ELSE
     SET DateIsValid TO TRUE
  END-IF.

