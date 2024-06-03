      *  When there are several SQL statements in a row, 
      *  the expression endpoint is white rather than blue.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. test2_dot.   
       PROCEDURE DIVISION. 
           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
           END-EXEC.      
           
           EXEC SQL
              START TRANSACTION
           END-EXEC.                                                    

           EXEC SQL
               SELECT COUNT(*) INTO :T1 FROM EMPTABLE
           END-EXEC. 