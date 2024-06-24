       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL001A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 
       
           01 DATASRC PIC X(64).
           01 DBUSR  PIC X(64).
           01 DBPWD  PIC X(64).
           
           01 T1     PIC 9(3) VALUE 0.  
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBPWD FROM ENVIRONMENT-VALUE.
         
         DISPLAY '***************************************'.
         DISPLAY " DATASRC  : " DATASRC.
         DISPLAY " DB       : " DBUSR.
         DISPLAY " USER     : " DBPWD.
         DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
           END-EXEC.      
           
           DISPLAY 'CONNECT SQLCODE: ' SQLCODE

           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

           EXEC SQL WHENEVER NOT FOUND PERFORM 300-NOTFOUND END-EXEC.

           EXEC SQL WHENEVER SQLWARNING PERFORM 400-SQLWARNING END-EXEC.

           EXEC SQL WHENEVER SQLERROR PERFORM 500-SQLERROR END-EXEC.

           MOVE 'INSERT INTO TAB1(FLD1, FLD2) VALUES(?, ?)' 
                TO DYNSTMT1-ARR.

           MOVE FUNCTION LENGTH(FUNCTION TRIM(DYNSTMT1-ARR))
             TO DYNSTMT1-LEN.
           
           EXEC SQL 
               PREPARE SQLSTMT1 FROM :DYNSTMT1
           END-EXEC.
           DISPLAY 'PREPARE SQLSTMT1: ' SQLCODE

           MOVE 1 TO T1.
           MOVE 2 TO T2.
           EXEC SQL EXECUTE SQLSTMT1 USING :T1, :T2 END-EXEC.
           DISPLAY 'EXECUTE SQLSTMT1(1): ' SQLCODE

           MOVE 5 TO T1.
           MOVE 6 TO T2.
           EXEC SQL EXECUTE SQLSTMT1 USING :T1, :T2 END-EXEC.
           DISPLAY 'EXECUTE SQLSTMT1(2): ' SQLCODE

           
           MOVE 999 TO CKEY.
           MOVE 'PRE-SEL-1' TO OP.
           EXEC SQL 
                SELECT FLD2 INTO :T2
                FROM TAB1
                WHERE FLD2 = :CKEY
           END-EXEC.
           MOVE 'POST-SEL-1' TO OP.

           MOVE 'PRE-SP1-1' TO OP.
           EXEC SQL SAVEPOINT SP1 END-EXEC.
           MOVE 'POST-SP1-1' TO OP.

           MOVE 'PRE-BAD-SELECT1' TO OP.
      * THIS SHOULD FAIL
           EXEC SQL 
                SELECT FLD2 INTO :T2
                FROM XXXXX
                WHERE FLD2 = :CKEY
           END-EXEC.
      * THIS SHOULD FAIL
           MOVE 'POST-BAD-SELECT1' TO OP.

           MOVE 'PRE-RB-1' TO OP.
           EXEC SQL ROLLBACK TO SAVEPOINT SP1 END-EXEC.
           MOVE 'POST-RB-1' TO OP.

           MOVE 998 TO CKEY.

           EXEC SQL WHENEVER NOT FOUND PERFORM 301-NOTFOUND END-EXEC.

           MOVE 'PRE-SEL-2' TO OP.
           EXEC SQL 
                SELECT FLD2 INTO :T2
                FROM TAB1
                WHERE FLD2 = :CKEY
           END-EXEC.
           MOVE 'POST-SEL-2' TO OP.
      
           EXEC SQL WHENEVER SQLERROR PERFORM 501-SQLERROR END-EXEC.

           MOVE 'PRE-SP1-2' TO OP.
           EXEC SQL SAVEPOINT SP1 END-EXEC.
           MOVE 'POST-SP1-2' TO OP.

           MOVE 'PRE-SEL-3' TO OP.
      * THIS SHOULD FAIL
           EXEC SQL 
                SELECT FLD2 INTO :T2
                FROM YYYYY
                WHERE FLD2 = :CKEY
           END-EXEC.
      * THIS SHOULD FAIL
           MOVE 'POST-SEL-3' TO OP.

           MOVE 'PRE-RB-2' TO OP.
           EXEC SQL ROLLBACK TO SAVEPOINT SP1 END-EXEC.
           MOVE 'POST-RB-2' TO OP.

           MOVE 'PRE-COMMIT' TO OP.
           EXEC SQL COMMIT END-EXEC.
           MOVE 'POST-COMMIT' TO OP.

           EXEC SQL
              CONNECT RESET
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'DISCONNECT SQLCODE. ' SQLCODE
              DISPLAY 'DISCONNECT SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.
       
       100-EXIT. 
             STOP RUN.


       300-NOTFOUND.
             DISPLAY OP ', DATA NOT FOUND FOR KEY = ' CKEY.

       400-SQLWARNING.
             DISPLAY OP ', WARNING - SQLCODE : ' SQLCODE
             DISPLAY OP ', WARNING - SQLSTATE: ' SQLSTATE.

       500-SQLERROR.
             DISPLAY OP ', WARNING - SQLCODE : ' SQLCODE
             DISPLAY OP ', ERROR - SQLSTATE  : ' SQLSTATE.

       301-NOTFOUND.
             DISPLAY OP ', DATA NOT FOUND (2) FOR KEY = ' CKEY.

       501-SQLERROR.
             DISPLAY OP ', WARNING - SQLCODE : ' SQLCODE
             DISPLAY OP ', ERROR (2) - SQLSTATE  : ' SQLSTATE.

       999-END.
