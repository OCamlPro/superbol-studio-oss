      $ set NESTCALL
      $ SET SOURCEFORMAT "FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. DayDiffDriver.
AUTHOR. Michael Coughlan.
* This program gets the difference in days between two dates.
* It calls three contained subprograms and one external subprogram
* to do the actual work. 
* The "GetDayDiff" program is included as a contained
* subprogram and is used to get the difference in days between
* two dates entered by the user.
* The dates entered by the user are validated by calling my "Validate" 
* subprogram.  The dates entered by the user and the date required by
* the "GetDayDiff" program are in different formats.
* The "EuroDateToSortDate" subprogram is used to convert from DDMMYYYY format
* to YYYYMMDD format and the "SortDateToEuroDate" is used to convert it back.


ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  Dates.
    02 FirstDate                  PIC X(8).
    02 SecondDate                 PIC X(8).
    02 FirstDatePrn               PIC XX/XX/XXXX.
    02 SecondDatePrn              PIC XX/XX/XXXX.

01  DayDiffs.
    02 DayDifference              PIC S9(7).
    02 DayDifferencePrn           PIC ----,--9.

01  ValidationResult              PIC 9.
    88 DateIsValid                VALUE 0.
    88 DateIsNotValid             VALUE 1 THRU 6.
    88 DateNotNumeric             VALUE 1.
    88 YearContainsZeros          VALUE 2.
    88 MonthContainsZeros         VALUE 3.
    88 DayContainsZeros           VALUE 4.
    88 MonthGreaterThan12         VALUE 5.
    88 DayTooGreatForMonth        VALUE 6.
            

PROCEDURE DIVISION.
Begin.
    SET DateIsNotValid TO TRUE.
    PERFORM GetValidFirstDate UNTIL DateIsValid.

    SET DateIsNotValid TO TRUE.
    PERFORM GetValidSecondDate UNTIL DateIsValid.
    
    CALL "EuroDateToSortDate" USING FirstDate, FirstDate.
    CALL "EuroDateToSortDate" USING SecondDate, SecondDate.
    
    CALL "GetDayDiff" USING BY CONTENT FirstDate,  SecondDate
                            BY REFERENCE DayDifference.

    CALL "SortDateToEuroDate" USING FirstDate, FirstDate.
    CALL "SortDateToEuroDate" USING SecondDate, SecondDate.
    MOVE DayDifference TO DayDifferencePrn.
    MOVE FirstDate TO FirstDatePrn.
    MOVE SecondDate TO SecondDatePrn.
    DISPLAY SPACES.
    DISPLAY "The difference between " FirstDatePrn " and "
             SecondDatePrn " is " DayDifferencePrn " days.".
    
    STOP RUN.


GetValidFirstDate.
    DISPLAY SPACES
    DISPLAY "Enter the first  date in DDMMYYYY format " WITH NO ADVANCING.
    ACCEPT FirstDate.
    CALL "Validate" USING   BY CONTENT   FirstDate,
                            BY REFERENCE ValidationResult.
    IF DateIsNotValid 
        PERFORM DisplayErrorMessage
    END-IF.


GetValidSecondDate.
    DISPLAY SPACES
    DISPLAY "Enter the second date in DDMMYYYY format " WITH NO ADVANCING.
    ACCEPT SecondDate.
    CALL "Validate" USING   BY CONTENT   SecondDate,
                            BY REFERENCE ValidationResult.
    IF DateIsNotValid 
        PERFORM DisplayErrorMessage
    END-IF.


DisplayErrorMessage.
    DISPLAY "Invalid date . Return code  = " ValidationResult
    EVALUATE TRUE
       WHEN DateNotNumeric      DISPLAY "Date is not numeric."
       WHEN YearContainsZeros   DISPLAY "Year contains all zeros."
       WHEN MonthContainsZeros  DISPLAY "Month contains all zeros."
       WHEN DayContainsZeros    DISPLAY "Day contains all zeros."
       WHEN MonthGreaterThan12  DISPLAY "Month to great."
       WHEN DayTooGreatForMonth DISPLAY "Day to great for month."
    END-EVALUATE.





IDENTIFICATION DIVISION.
PROGRAM-ID. EuroDateToSortDate.
AUTHOR.    Michael Coughlan.
* Converts a date in DDMMYYYY format to one in YYYYMMDD

ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  YYYYDDMMTemp.
    02 YYYYYear                    PIC XXXX.
    02 YYYYMonth                   PIC XX.
    02 YYYYDay                     PIC XX.

LINKAGE SECTION.
01  DDMMYYYYDate.
    02 DDMMDay                     PIC XX.
    02 DDMMMonth                   PIC XX.
    02 DDMMYear                    PIC XXXX.

01  YYYYDDMMDate                   PIC X(8).

PROCEDURE DIVISION USING DDMMYYYYDate, YYYYDDMMDate.
Begin.
    MOVE DDMMDay      TO YYYYDay.
    MOVE DDMMMonth    TO YYYYMonth.
    MOVE DDMMYear     TO YYYYYear.
    MOVE YYYYDDMMTEMP TO YYYYDDMMDate.
    EXIT PROGRAM.

END PROGRAM EuroDateToSortDate.



IDENTIFICATION DIVISION.
PROGRAM-ID. SortDateToEuroDate.
AUTHOR.     Michael Coughlan.
* Converts a date in YYYYMMDD format to one in DDMMYYYY

ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  DDMMYYYYTemp.
    02 DDMMDay                     PIC XX.
    02 DDMMMonth                   PIC XX.
    02 DDMMYear                    PIC XXXX.

LINKAGE SECTION.
01  YYYYDDMMDate.
    02 YYYYYear                    PIC XXXX.
    02 YYYYMonth                   PIC XX.
    02 YYYYDay                     PIC XX.

01  DDMMYYYYDate                   PIC X(8).

PROCEDURE DIVISION USING YYYYDDMMDate, DDMMYYYYDate.
Begin.
    MOVE YYYYDay      TO DDMMDay.
    MOVE YYYYMonth    TO DDMMMonth.
    MOVE YYYYYear     TO DDMMYear.
    MOVE DDMMYYYYTEMP TO DDMMYYYYDate.
    EXIT PROGRAM.

END PROGRAM SortDateToEuroDate.



IDENTIFICATION DIVISION.
PROGRAM-ID. GetDayDiff.
AUTHOR. Michael Coughlan.
* This module finds the difference in days between two
* Dates. The dates must be in the form YYYYMMDD.
* The first date passed is subtracted from the second
* Date and the difference is returned.

ENVIRONMENT DIVISION.
DATA DIVISION.
LINKAGE SECTION.
01  Date1                          PIC 9(8).
01  Date2                          PIC 9(8).
01  Difference                     PIC S9(7).


PROCEDURE DIVISION USING Date1, Date2, Difference.
Begin.
   COMPUTE Difference =
           FUNCTION INTEGER-OF-DATE(Date2)- FUNCTION INTEGER-OF-DATE(Date1)
   EXIT PROGRAM.

END PROGRAM GetDayDiff.

END PROGRAM DayDriver.

