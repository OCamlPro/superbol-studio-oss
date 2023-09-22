      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID. MultiplyNums.
AUTHOR. Michael Coughlan.

* This sub-program is CALLed from a Cobol program.
* it requires 5 parameters.
*    2 to contain the numbers to be multiplied
*    2 to contain strings to be displayed
*    1 to return the result of the multiplication.


ENVIRONMENT DIVISION.
DATA DIVISION.
LINKAGE SECTION.
* Parameters passed to the subprogram must have corresponding 
* entries in the LINKAGE SECTION of the CALLed subprogram but 
* they don't have to be declared in any particular order in the
* LINKAGE SECTION.

01 Param1           PIC 9(3).
01 Param2           PIC 9(3).
01 Answer           PIC 9(6) COMP.
01 StrA             PIC X(20).
01 StrB             PIC X(20).

PROCEDURE DIVISION USING Param1, Param2, StrA, StrB, Answer.
Begin.
*   The order of the Parameters in the USING clause is vital.
*   Parameters correspond by relative position not by name.
*   i.e. the name of an Parameter does not have to be the same
*        in the subprogram as it was in the main_program
*   The CALLed program should adher to the same considerations of good
*   programming as the main program.   The only difference will be that 
*   the STOP RUN will be replaced by the EXIT PROGRAM statement.
    DISPLAY ">>>>>>>>>>>>>>>>> In the sub-program"
    DISPLAY StrA Param1
    DISPLAY StrB Param2

    MULTIPLY Param1 BY Param2 GIVING Answer.


    MOVE "VALUE OVERWRITTEN" TO StrA
    MOVE "VALUE OVERWRITTEN" TO StrB
*   This is done to demonstrate the differece between passing
*   BY CONTENT and passing BY REFERENCE.  If you pass BY REFERENCE
*   you give the called program the opportunity to corrupt your data. 
*   You should only pass BY REFERENCE if you require the called
*   program to pass back data - as is the case with the result of 
*   the multiplication.

    DISPLAY "<<<<<<<<<<<<<< Leaving sub-program now".
    EXIT PROGRAM.

