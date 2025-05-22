       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL005C. 
       
       
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

           01 PAYLOAD PIC X(100).

           01 VCFLD1 SQL TYPE IS VARCHAR(100).
           01 CFLD1  SQL TYPE IS CHAR(100).

           EXEC SQL VAR VCFLD2 IS VARCHAR(100) END-EXEC.
           EXEC SQL VAR CFLD2 IS CHAR(100)     END-EXEC.

           01 VCFLD2 PIC X(100).
           01 CFLD2  PIC X(100).

           01 VCFLD3 PIC X(100).
           01 CFLD3  PIC X(100).

           EXEC SQL VAR VCFLD3 IS VARCHAR(100) END-EXEC.
           EXEC SQL VAR CFLD3 IS CHAR(100)     END-EXEC.

           01 OUT1 PIC X(120).
           01 OUT2 PIC X(120).

       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
         
       PROCEDURE DIVISION. 
 
       000-CONNECT.

           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
           DISPLAY "PAYLOAD" UPON ENVIRONMENT-NAME.
           ACCEPT PAYLOAD FROM ENVIRONMENT-VALUE.
           
           DISPLAY 'PAYLOAD IS [' PAYLOAD ']'.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.      

           IF SQLCODE <> 0 THEN
              DISPLAY 'CONNECT SQLCODE. ' SQLCODE
              DISPLAY 'CONNECT SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

      * case 1

           MOVE PAYLOAD TO VCFLD1-ARR.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(VCFLD1-ARR)) 
                TO VCFLD1-LEN.

           MOVE PAYLOAD TO CFLD1.
           
           EXEC SQL
              INSERT INTO TAB00 (CID, VCFLD, CFLD)
                VALUES(99, :VCFLD1, :CFLD1)
           END-EXEC.
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'INSERT SQLCODE. ' SQLCODE
              DISPLAY 'INSERT SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           EXEC SQL
              SELECT CONCAT('[', CONCAT (VCFLD, ']'))
              INTO :OUT1 FROM TAB00
              WHERE CID = 99
           END-EXEC.

           EXEC SQL
              SELECT CONCAT('[', CONCAT(CFLD, ']'))
              INTO :OUT2 FROM TAB00
              WHERE CID = 99
           END-EXEC.

           IF SQLCODE <> 0 THEN
              DISPLAY 'SELECT-LEN SQLCODE. ' SQLCODE
              DISPLAY 'SELECT-LEN SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           DISPLAY 'VCFLD(1): ' OUT1.
           DISPLAY 'CFLD(1) : ' OUT2.

      * case 2

           MOVE PAYLOAD TO VCFLD2.
           MOVE PAYLOAD TO CFLD2.
           
           EXEC SQL
              INSERT INTO TAB00 (CID, VCFLD, CFLD)
                VALUES(98, :VCFLD2, :CFLD2)
           END-EXEC.
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'INSERT SQLCODE. ' SQLCODE
              DISPLAY 'INSERT SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           EXEC SQL
              SELECT CONCAT('[', CONCAT(VCFLD, ']')) 
              INTO :OUT1 FROM TAB00
              WHERE CID = 98
           END-EXEC.

           EXEC SQL
              SELECT CONCAT('[', CONCAT(CFLD, ']'))
              INTO :OUT2 FROM TAB00
              WHERE CID = 98
           END-EXEC.

           IF SQLCODE <> 0 THEN
              DISPLAY 'SELECT-LEN SQLCODE. ' SQLCODE
              DISPLAY 'SELECT-LEN SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           DISPLAY 'VCFLD(2): ' OUT1.
           DISPLAY 'CFLD(2) : ' OUT2.

      * case 3

           MOVE PAYLOAD TO VCFLD3.
           MOVE PAYLOAD TO CFLD3.
           
           EXEC SQL
              INSERT INTO TAB00 (CID, VCFLD, CFLD)
                VALUES(97, :VCFLD3, :CFLD3)
           END-EXEC.
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'INSERT SQLCODE. ' SQLCODE
              DISPLAY 'INSERT SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           EXEC SQL
              SELECT CONCAT('[', CONCAT(VCFLD, ']'))
              INTO :OUT1 FROM TAB00
              WHERE CID = 97
           END-EXEC.

           EXEC SQL
              SELECT CONCAT('[', CONCAT(CFLD, ']'))
              INTO :OUT2 FROM TAB00
              WHERE CID = 97
           END-EXEC.

           IF SQLCODE <> 0 THEN
              DISPLAY 'SELECT-LEN SQLCODE. ' SQLCODE
              DISPLAY 'SELECT-LEN SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           DISPLAY 'VCFLD(3): ' OUT1.
           DISPLAY 'CFLD(3) : ' OUT2.

           EXEC SQL
              COMMIT
           END-EXEC.                 

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
