       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL003A. 
       
       
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

       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC1" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC-1 FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR1" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR-1 FROM ENVIRONMENT-VALUE.
         
         DISPLAY "DATASRC2" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC-2 FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR2" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR-2 FROM ENVIRONMENT-VALUE.         

         DISPLAY '***************************************'.
         DISPLAY " DATASRC1  : " DATASRC-1.
         DISPLAY " USER1     : " DBUSR-1.
         DISPLAY " DATASRC2  : " DATASRC-2.
         DISPLAY " USER2     : " DBUSR-2.         
         DISPLAY '***************************************'.
           
           EXEC SQL AT CONN1
                DECLARE CRSR01 CURSOR FOR
                    SELECT FLD1 FROM TAB1 ORDER BY FLD1
           END-EXEC. 
           
           DISPLAY 'CONNECT SQLCODE(1): ' SQLCODE

           EXEC SQL AT CONN2
                DECLARE CRSR02 CURSOR FOR
                    SELECT FLD2 FROM TAB2 ORDER BY FLD2
           END-EXEC. 
           
           DISPLAY 'CONNECT SQLCODE(1): ' SQLCODE
           
           EXEC SQL 
                INCLUDE SQLCA 
           END-EXEC. 
           
           DISPLAY 'CONNECT SQLCODE(1): ' SQLCODE

           EXEC SQL
              CONNECT TO :DATASRC-1 AS CONN1 USER :DBUSR-1
           END-EXEC.    
           
           DISPLAY 'CONNECT SQLCODE(1): ' SQLCODE
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.         
           
           EXEC SQL
              CONNECT TO :DATASRC-2 AS CONN2 USER :DBUSR-2
           END-EXEC.   
           
           DISPLAY 'CONNECT SQLCODE(2): ' SQLCODE
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

      *     EXEC SQL AT CONN1
      *        START TRANSACTION
      *     END-EXEC.         
      *
      *     EXEC SQL AT CONN2
      *        START TRANSACTION
      *     END-EXEC.                 
           

       100-MAIN.       

      *     EXEC SQL AT CONN1 DROP TABLE IF EXISTS TAB1 END-EXEC.
      *     DISPLAY 'CONNECT DROP(1): ' SQLCODE
      *
      *     EXEC SQL AT CONN1 DROP TABLE IF EXISTS TAB2 END-EXEC.
      *     DISPLAY 'CONNECT DROP(3): ' SQLCODE
      *
      *     EXEC SQL AT CONN2 DROP TABLE IF EXISTS TAB2 END-EXEC.
      *     DISPLAY 'CONNECT DROP(2): ' SQLCODE
      *
      *     EXEC SQL AT CONN2 DROP TABLE IF EXISTS TAB1 END-EXEC.
      *     DISPLAY 'CONNECT DROP(4): ' SQLCODE

      *    EXEC SQL AT CONN1 CREATE TABLE TAB1 (FLD1 INT) END-EXEC.
      *    DISPLAY 'CONNECT CREATE(1): ' SQLCODE
      *
      *    EXEC SQL AT CONN2 CREATE TABLE TAB2 (FLD2 INT) END-EXEC.
      *    DISPLAY 'CONNECT CREATE(2): ' SQLCODE
           
           EXEC SQL AT CONN1 
                INSERT INTO TAB1 (FLD1) VALUES (1)
           END-EXEC.
           DISPLAY 'CONNECT INSERT(1-1): ' SQLCODE
           DISPLAY 'CONNECT INSERT(1-1): ' SQLERRMC(1:SQLERRML)

           EXEC SQL AT CONN1 
                INSERT INTO TAB1 (FLD1) VALUES (3)
           END-EXEC.
           DISPLAY 'CONNECT INSERT(1-2): ' SQLCODE
           DISPLAY 'CONNECT INSERT(1-2): ' SQLERRMC(1:SQLERRML)

           EXEC SQL AT CONN1 
                INSERT INTO TAB1 (FLD1) VALUES (5)
           END-EXEC.
           DISPLAY 'CONNECT INSERT(1-3): ' SQLCODE
           DISPLAY 'CONNECT INSERT(1-3): ' SQLERRMC(1:SQLERRML)
 
           EXEC SQL AT CONN2 
                INSERT INTO TAB2 (FLD2) VALUES (100)
           END-EXEC.           
           DISPLAY 'CONNECT INSERT(2-1): ' SQLCODE
           DISPLAY 'CONNECT INSERT(2-1): ' SQLERRMC(1:SQLERRML)

           EXEC SQL AT CONN2 
                INSERT INTO TAB2 (FLD2) VALUES (300)
           END-EXEC.           
           DISPLAY 'CONNECT INSERT(2-2): ' SQLCODE
           DISPLAY 'CONNECT INSERT(2-2): ' SQLERRMC(1:SQLERRML)

           EXEC SQL AT CONN2 
                INSERT INTO TAB2 (FLD2) VALUES (500)
           END-EXEC.           
           DISPLAY 'CONNECT INSERT(2-3): ' SQLCODE
           DISPLAY 'CONNECT INSERT(2-3): ' SQLERRMC(1:SQLERRML)

           EXEC SQL AT CONN1
               SELECT SUM(FLD1) INTO :T1 FROM TAB1
           END-EXEC. 
           DISPLAY 'CONNECT SUM(1): ' SQLCODE
           
           EXEC SQL AT CONN2
               SELECT SUM(FLD2) INTO :T2 FROM TAB2
           END-EXEC.      
           DISPLAY 'CONNECT SUM(2): ' SQLCODE
           
           COMPUTE TOT = T1 + T2.      

           DISPLAY 'T1   : ' T1.
           DISPLAY 'T2   : ' T2.
           DISPLAY 'TOTAL: ' TOT.

           EXEC SQL AT CONN1 SAVEPOINT SP1 END-EXEC.
           DISPLAY 'SQLCODE SAVEPOINT SP1: ' SQLCODE.

           EXEC SQL AT CONN2 SAVEPOINT SP2 END-EXEC.
           DISPLAY 'SQLCODE SAVEPOINT SP2: ' SQLCODE.

      *    THIS SHOULD FAIL
           EXEC SQL AT CONN2
               SELECT SUM(FLD1) INTO :T1 FROM TAB1
           END-EXEC. 
           DISPLAY 'SQLSTATE FAIL1 (OK IF <> 00000): ' SQLSTATE.
           IF SQLSTATE <> '00000' THEN
                DISPLAY 'SQLSTATE FAIL1: OK'
           ELSE
                DISPLAY 'SQLSTATE FAIL1: KO'
           END-IF.
           
      *    THIS SHOULD FAIL
           EXEC SQL AT CONN1
               SELECT SUM(FLD2) INTO :T2 FROM TAB2
           END-EXEC. 
           DISPLAY 'SQLSTATE FAIL2 (OK IF <> 00000): ' SQLSTATE.  
           IF SQLSTATE <> '00000' THEN
                DISPLAY 'SQLSTATE FAIL2: OK'
           ELSE
                DISPLAY 'SQLSTATE FAIL2: KO'
           END-IF.

           EXEC SQL AT CONN1 ROLLBACK TO SAVEPOINT SP1 END-EXEC.
           DISPLAY 'SQLCODE ROLLBACK SP1: ' SQLCODE.

           EXEC SQL AT CONN2 ROLLBACK TO SAVEPOINT SP2 END-EXEC.
           DISPLAY 'SQLCODE ROLLBACK SP2: ' SQLCODE.
           
       100-CURSOR1-TEST.            
      *  open cursor
           EXEC SQL 
               OPEN CRSR01
           END-EXEC.
           DISPLAY 'SQLCODE OPEN CRSR01 : ' SQLCODE.
           DISPLAY 'SQLERRMCOPEN CRSR01 : ' SQLERRMC.

           MOVE 1 TO CURREC.
           MOVE 0 TO TOT.

           EXEC SQL
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

           DISPLAY 'TOT CRSR01 :' TOT.

       100-CURSOR2-TEST.            
      *  open cursor
           EXEC SQL 
               OPEN CRSR02
           END-EXEC.
           DISPLAY 'SQLCODE OPEN CRSR02 : ' SQLCODE.
           DISPLAY 'SQLERRMCOPEN CRSR02 : ' SQLERRMC.

           MOVE 1 TO CURREC.
           MOVE 0 TO TOT.


           EXEC SQL
               FETCH CRSR02 INTO :T2
           END-EXEC

           DISPLAY 'SQLCODE : ' SQLCODE
           DISPLAY 'SQLERRMC: ' SQLERRMC
                  
           IF SQLCODE <> 100 THEN
      *         display the record
                DISPLAY 'CRSR02 rec #' CURREC ' : [' T2 ']'
                ADD 1 TO CURREC
                ADD T2 TO TOT
           END-IF

           DISPLAY 'TOT CRSR02 :' TOT.


       CLOSE-CRSRS.

      *    close the cursors


           EXEC SQL CLOSE CRSR01 END-EXEC.     

           EXEC SQL CLOSE CRSR02 END-EXEC.     

      *    we test both types of disconnections

           EXEC SQL CONNECT RESET CONN1 END-EXEC.
           EXEC SQL DISCONNECT CONN2 END-EXEC.

       100-EXIT. 
             STOP RUN.
