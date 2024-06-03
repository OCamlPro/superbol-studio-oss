       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. test1_data_division. 
       DATA DIVISION.  
       WORKING-STORAGE SECTION. 
       
       EXEC SQL 
        INCLUDE EMPREC 
       END-EXEC. 
       
           01 VARIABLE PIC X(64).
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 


      * When parsed, this become  

      * WORKING-STORAGE SECTION.EXEC SQL INCLUDE EMPREC END-EXEC
      * 01 VARIABLE PIC X(64).EXEC SQL INCLUDE SQLCA END-EXEC
       
       PROCEDURE DIVISION. 
         DISPLAY "HELLO WORLD"
         STOP RUN.
