      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  MileageCounter.
AUTHOR.  Michael Coughlan.
* Simulates a mileage counter

DATA DIVISION.
WORKING-STORAGE SECTION.
01 Counters.
   02 HundredsCount          PIC 99 VALUE ZEROS.
   02 TensCount              PIC 99 VALUE ZEROS.
   02 UnitsCount             PIC 99 VALUE ZEROS.

01 DisplayItems.
   02 PrnHunds               PIC 9.
   02 PrnTens                PIC 9.
   02 PrnUnits               PIC 9.

PROCEDURE DIVISION.
Begin.
   DISPLAY "Using an out-of-line Perform".
   DISPLAY "About to start mileage counter simulation".
   PERFORM CountMilage
      VARYING HundredsCount FROM 0 BY 1 UNTIL HundredsCount > 9
      AFTER   TensCount FROM 0 BY 1 UNTIL TensCount > 9
      AFTER   UnitsCount FROM 0 BY 1 UNTIL UnitsCount > 9
   DISPLAY "End of mileage counter simulation."

   DISPLAY "Now using in-line Performs"
   DISPLAY "About to start mileage counter simulation".
   PERFORM VARYING HundredsCount FROM 0 BY 1 UNTIL HundredsCount > 9
     PERFORM VARYING TensCount FROM 0 BY 1 UNTIL TensCount > 9
        PERFORM VARYING UnitsCount FROM 0 BY 1 UNTIL UnitsCount > 9
           MOVE HundredsCount TO PrnHunds
           MOVE TensCount     TO  PrnTens
           MOVE UnitsCount    TO PrnUnits
           DISPLAY PrnHunds "-" PrnTens "-" PrnUnits
        END-PERFORM
     END-PERFORM
   END-PERFORM.
   DISPLAY "End of mileage counter simulation."
   STOP RUN.



CountMilage.
   MOVE HundredsCount TO PrnHunds
   MOVE TensCount     TO  PrnTens
   MOVE UnitsCount    TO PrnUnits
   DISPLAY PrnHunds "-" PrnTens "-" PrnUnits.

 
