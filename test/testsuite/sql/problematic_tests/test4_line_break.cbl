       IDENTIFICATION DIVISION.
       PROGRAM-ID. test4_line_break. 
       DATA DIVISION.         
       WORKING-STORAGE SECTION. 
           01 VARIABLE PIC X(64).
       PROCEDURE DIVISION. 
      * PROBLEM WITH - (Line break in COBOL)   
           MOVE 'UPDATE TAB1 SET FLD1=FLD1+100, FLD2=FLD2+300'
      * >> Hint: Missing TO
      -          TO VARIABLE.
      * >> Error: Unexpected `TO` in continuation
