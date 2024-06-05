       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL006A. 
       
       
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

           01  DYNSTMT1   SQL TYPE IS VARCHAR(100).
           01  DNUM1   PIC S9(4)V99.
           01  DNUM2   PIC S9(4)V99.
           01  DNUM3   PIC S9(4)V99.
           01  T1      PIC 9(4) VALUE 3.
           01  T2      PIC 9(4) VALUE 4.
           01  TOT1    PIC 9(4) VALUE 0.
           01  TOT2    PIC 9(4) VALUE 0.

       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
         
       PROCEDURE DIVISION. 
 
       000-CONNECT.

           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'CONNECT SQLCODE. ' SQLCODE
              DISPLAY 'CONNECT SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

           MOVE 'INSERT INTO TAB1(FLD1, FLD2) VALUES(:1, :2)' 
                  TO DYNSTMT1-ARR.

           MOVE FUNCTION LENGTH(FUNCTION TRIM(DYNSTMT1-ARR))
             TO DYNSTMT1-LEN.
           
           EXEC SQL 
               PREPARE SQLSTMT1 FROM :DYNSTMT1
           END-EXEC.
           DISPLAY 'PREPARE SQLSTMT1: ' SQLCODE
           DISPLAY 'PREPARE SQLSTMT1: ' SQLERRMC(1:SQLERRML)

           MOVE 1 TO T1.
           MOVE 2 TO T2.
           EXEC SQL EXECUTE SQLSTMT1 USING :T1, :T2 END-EXEC.
           DISPLAY 'EXECUTE SQLSTMT1(1): ' SQLCODE

           MOVE 5 TO T1.
           MOVE 6 TO T2.
           EXEC SQL EXECUTE SQLSTMT1 USING :T1, :T2 END-EXEC.
           DISPLAY 'EXECUTE SQLSTMT1(2): ' SQLCODE

           EXEC SQL EXECUTE IMMEDIATE 
                'UPDATE TAB1 SET FLD1=FLD1+100, FLD2=FLD2+300'
           END-EXEC.
           DISPLAY 'EXECUTE IMMEDIATE(1): ' SQLCODE

           MOVE 'UPDATE TAB1 SET FLD1=FLD1+100, FLD2=FLD2+300'
      -          TO DYNSTMT1-ARR.

           MOVE FUNCTION LENGTH(FUNCTION TRIM(DYNSTMT1-ARR))
             TO DYNSTMT1-LEN.

           EXEC SQL EXECUTE IMMEDIATE :DYNSTMT1 END-EXEC.
           DISPLAY 'EXECUTE IMMEDIATE(2): ' SQLCODE

           EXEC SQL
                SELECT SUM(FLD1), SUM(FLD2)
                    INTO :TOT1, :TOT2 FROM TAB1
           END-EXEC
           DISPLAY 'SUM SQLCODE: ' SQLCODE.
           
           DISPLAY 'TOT1: ' TOT1.
           DISPLAY 'TOT2: ' TOT2.

           EXEC SQL COMMIT END-EXEC.

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
