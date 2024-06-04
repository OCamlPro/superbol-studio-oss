       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. test3_perform. 
       DATA DIVISION.  
       WORKING-STORAGE SECTION. 
           01 VARIABLE PIC 9(4) VALUE 0.
       
       PROCEDURE DIVISION. 
       100-MAIN.       
      *  Sql Statement after PERFORM are not reconised as Expression 
      
           PERFORM UNTIL SQLCODE < 0

           EXEC SQL
               SELECT COUNT(*) INTO :T1 FROM EMPTABLE
           END-EXEC

           DISPLAY "HELLO WORLD"
           END-PERFORM.  
           STOP RUN.

      * Invalid syntax*



