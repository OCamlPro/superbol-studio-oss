
       IDENTIFICATION DIVISION.
       PROGRAM-ID. sql_test16.   
       PROCEDURE DIVISION. 

           EXEC SQL
               SELECT BIDULE FROM EMPTABLE 
               UNION 
               SELECT MACHIN FROM EMPTABLE
           END-EXEC. 
           STOP RUN.