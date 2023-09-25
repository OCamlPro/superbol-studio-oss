      $ SET SOURCEFORMAT"FREE"
IDENTIFICATION DIVISION.
PROGRAM-ID.  PerformFormat4.
AUTHOR.  Michael Coughlan.
* An example program using the PERFORM..VARYING format.
* Pay particular attention to the values produced by the
* WITH TEST BEFORE and WITH TEST AFTER loops.
* Note that the PERFORM within a PERFORM produces the same
* results as the PERFORM..VARYING..AFTER


DATA DIVISION.
WORKING-STORAGE SECTION.
01  LoopCount          PIC 9  VALUE ZEROS.
01  LoopCount2         PIC S9 VALUE ZEROS.

PROCEDURE DIVISION.
Begin.
    DISPLAY "Start WHILE Iteration of LoopBody"
    PERFORM LoopBody WITH TEST BEFORE
        VARYING LoopCount FROM 1 BY 2
        UNTIL LoopCount GREATER THAN 5.
    DISPLAY "Finished WHILE iteration.  LoopCount = " LoopCount.

    DISPLAY "Start REPEAT Iteration of LoopBody"
    PERFORM LoopBody WITH TEST AFTER
        VARYING LoopCount FROM 1 BY 2 
        UNTIL LoopCount GREATER THAN 5.
    DISPLAY "Finished REPEAT iteration. LoopCount = " LoopCount.

    DISPLAY "Start inline loops"
    PERFORM VARYING LoopCount FROM 1 BY 1 
                    UNTIL LoopCount GREATER THAN 4
       PERFORM VARYING LoopCount2 FROM 5 BY -2 
                       UNTIL LoopCount2 LESS THAN ZERO
          DISPLAY "InLine loop " WITH NO ADVANCING
          DISPLAY "LoopCount = " LoopCount " LoopCount2 = " LoopCount2
       END-PERFORM
    END-PERFORM.
    DISPLAY "Finished inline loops".
    
    DISPLAY "Start PERFORM VARYING..AFTER".
    PERFORM LoopBody VARYING LoopCount FROM 1 BY 1
                       UNTIL LoopCount GREATER THAN 4
                 AFTER LoopCount2 FROM 5 BY -2
                       UNTIL LoopCount2 LESS THAN ZERO.
    DISPLAY "Finished PERFORM VARYING..AFTER".
    STOP RUN.


LoopBody.
    DISPLAY "LoopBody " WITH NO ADVANCING
    DISPLAY "LoopCount = " LoopCount " LoopCount2 = " LoopCount2.

