       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL009A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 
       
           01 DATASRC-1 PIC X(64).
           01 DBUSR-1   PIC X(64).           
           
           01 DATASRC-2 PIC X(64).
           01 DBUSR-2   PIC X(64).
           
           01 T1     PIC 9(4) VALUE 0.  
           01 T2     PIC 9(4) VALUE 0.  
           01 TOT    PIC 9(4) VALUE 0.  
           
           01 CURREC PIC 9(4).  

       EXEC SQL AT CONN1
            DECLARE CRSR01 CURSOR FOR
                SELECT FLD1 FROM TAB1 ORDER BY FLD1
       END-EXEC. 
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC1" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC-1 FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR1" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR-1 FROM ENVIRONMENT-VALUE.

         DISPLAY '***************************************'.
         DISPLAY " DATASRC1  : " DATASRC-1.
         DISPLAY " USER1     : " DBUSR-1.
         DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DATASRC-1 AS CONN1 USER :DBUSR-1
           END-EXEC.    
           
           DISPLAY 'CONNECT SQLCODE(1): ' SQLCODE.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.          

           EXEC SQL AT CONN1
              START TRANSACTION
	       END-EXEC.         

       100-MAIN.       

           EXEC SQL AT CONN1 DROP TABLE IF EXISTS TAB1 END-EXEC.
           DISPLAY 'CONNECT DROP(1): ' SQLCODE.

           EXEC SQL AT CONN1 DROP TABLE IF EXISTS TAB2 END-EXEC.
           DISPLAY 'CONNECT DROP(3): ' SQLCODE.

           EXEC SQL AT CONN1 CREATE TABLE TAB1 (FLD1 INT) END-EXEC.
           DISPLAY 'CONNECT CREATE(1): ' SQLCODE.
           
           EXEC SQL AT CONN1 
                INSERT INTO TAB1 (FLD1) VALUES (1),(3),(5)
           END-EXEC.
           DISPLAY 'CONNECT INSERT(1): ' SQLCODE.
           
           EXEC SQL AT CONN1
               SELECT SUM(FLD1) INTO :T1 FROM TAB1
           END-EXEC. 
           DISPLAY 'CONNECT SUM(1): ' SQLCODE.
           
           COMPUTE TOT = T1 + T2.      

           DISPLAY 'T1   : ' T1.
           DISPLAY 'T2   : ' T2.
           DISPLAY 'TOTAL: ' TOT.

           EXEC SQL AT CONN1 SAVEPOINT SP1 END-EXEC.

      *    THIS SHOULD FAIL
           EXEC SQL AT CONN1
               SELECT SUM(FLD2) INTO :T2 FROM TAB2
           END-EXEC. 
           DISPLAY 'SQLSTATE FAIL2 (OK IF <> 00000): ' SQLSTATE.  

           EXEC SQL AT CONN1 ROLLBACK TO SAVEPOINT SP1 END-EXEC.
           
       100-CURSOR1-TEST.            
      *  open cursor (bad syntax, should trigger an error)

           EXEC SQL AT CONN1
               OPEN CRSR01 
           END-EXEC.
           DISPLAY 'SQLCODE OPEN CRSR01 : ' SQLCODE.
           DISPLAY 'SQLERRMCOPEN CRSR01 : ' SQLERRMC.

           MOVE 1 TO CURREC.
           MOVE 0 TO TOT.

           PERFORM UNTIL SQLCODE < 0 OR SQLCODE = 100

      *  fetch from cursor (bad syntax, should trigger an error)
           EXEC SQL AT CONN1
               FETCH CRSR01 INTO :T1
           END-EXEC

           DISPLAY 'SQLCODE : ' SQLCODE
           DISPLAY 'SQLERRMC: ' SQLERRMC
                  
           IF SQLCODE <> 100 THEN
      *         display the record
                DISPLAY 'CRSR01 rec #' CURREC ' : [' T1 ']'
                ADD 1 TO CURREC
                ADD T1 TO TOT
           END-IF
           END-PERFORM.  

           DISPLAY 'TOT CRSR01 :' TOT.

       CLOSE-CRSRS.

      *    close the cursors

           EXEC SQL CLOSE CRSR01 END-EXEC.       

           EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.
