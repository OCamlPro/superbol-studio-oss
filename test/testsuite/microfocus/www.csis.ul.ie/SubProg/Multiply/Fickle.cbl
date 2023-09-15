      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. Fickle.
AUTHOR. Michael Coughlan.
* This sub-program is demonstrates State Memory.
* Each time the program is called it remembers its
* state from the previous call. We can get rid of
* State Memory by using the IS INITIAL phrase.


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

