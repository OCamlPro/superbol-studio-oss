      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. Steadfast IS INITIAL.
AUTHOR. Michael Coughlan.
* This sub-program is demonstrates the use of the
* IS INITIAL phrase. Each time the program is called
* it is as if it had been called for the very first time.
* All data items are initialized to their VALUE clauses.


ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 RunningTotal     PIC 9(4) VALUE 150.

LINKAGE SECTION.
01 Param1           PIC 99.

PROCEDURE DIVISION USING Param1.
Begin.
    ADD Param1 TO RunningTotal.
    DISPLAY "The total so far is " RunningTotal

    EXIT PROGRAM.

