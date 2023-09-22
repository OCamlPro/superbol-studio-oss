      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  PerformFormat1.
AUTHOR.  Michael Coughlan.
* Illustrates how the first format of the PERFORM may
* be used to change the flow of control through a program.
* Use the output of this program to get an understanding of how
* this format of the PERFORM works.

PROCEDURE DIVISION.
TopLevel.
    DISPLAY "In TopLevel. Starting to run program"
    PERFORM OneLevelDown
    DISPLAY "Back in TopLevel.".
    STOP RUN.


TwoLevelsDown.
    DISPLAY ">>>>>>>> Now in TwoLevelsDown."
    PERFORM ThreeLevelsDown.
    DISPLAY ">>>>>>>> Back in TwoLevelsDown.".


OneLevelDown.
    DISPLAY ">>>> Now in OneLevelDown"
    PERFORM TwoLevelsDown
    DISPLAY ">>>> Back in OneLevelDown".


ThreeLevelsDown.
    DISPLAY ">>>>>>>>>>>> Now in ThreeLevelsDown".

