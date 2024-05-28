       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL030A. 
       
       
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
           
           DISPLAY '***************************************'.
           DISPLAY " DATASRC  : " DATASRC.
           DISPLAY " AUTH     : " DBUSR.
           DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.      
           
           EXEC SQL
              START TRANSACTION
           END-EXEC.           
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'CONNECT SQLCODE. ' SQLCODE
              DISPLAY 'CONNECT SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

          EXEC SQL
          SET APPLICATION_NAME TO "Identifier1 Identifier2 Identifier3"
          END-EXEC.

           EXEC SQL
                DROP TABLE IF EXISTS TAB00
           END-EXEC.
           
           EXEC SQL
                CREATE TABLE TAB00 (
                    CID   INTEGER,
                    VCFLD1 VARCHAR(100),
                    CFLD1 CHAR(100)
                )
           END-EXEC.

           MOVE 100     TO VCFLD1-LEN.
           MOVE PAYLOAD TO VCFLD1-ARR.

           MOVE PAYLOAD TO CFLD1.
           
           EXEC SQL
              INSERT INTO TAB00 (CID, VCFLD1, CFLD1)
                VALUES(99, :VCFLD1, :CFLD1)
           END-EXEC.
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'INSERT SQLCODE. ' SQLCODE
              DISPLAY 'INSERT SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           EXEC SQL
              SELECT CONCAT('[', VCFLD1, ']') 
              INTO :OUT1 FROM TAB00
           END-EXEC.

           EXEC SQL
              SELECT CONCAT('[', CFLD1, ']') 
              INTO :OUT2 FROM TAB00
           END-EXEC.

           IF SQLCODE <> 0 THEN
              DISPLAY 'SELECT-LEN SQLCODE. ' SQLCODE
              DISPLAY 'SELECT-LEN SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           DISPLAY 'VCFLD1: ' OUT1.
           DISPLAY 'CFLD1 : ' OUT2.

           DISPLAY SQ0002.

           EXEC SQL PREPARE ST1
                FROM
                    "SELECT MAX(CID) FROM TAB00 WHERE
      -              VCFLD1 NOT IN (
      -              'AAAAA' ,'BBBBBB' ,'CCCCCCCC' ,'DDDD' ,'EEEEEEEE'
      -              )"
           END-EXEC.
           IF SQLCODE <> 0 THEN
              DISPLAY 'PREPARE-1 SQLCODE. ' SQLCODE
              DISPLAY 'PREPARE-1 SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           EXEC SQL PREPARE ST2
                FROM
                    "SELECT MAX(CID) FROM TAB00 WHERE
      -              VCFLD1 NOT IN (
      -              'AAAAA ' ,'BBBBBB  ' ,'CCCCCCCC   ' ,
      -              ' DDDD' ,'  EEEEEEEE    '
      -              )"
           END-EXEC.
           IF SQLCODE <> 0 THEN
              DISPLAY 'PREPARE-1 SQLCODE. ' SQLCODE
              DISPLAY 'PREPARE-1 SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.

           DISPLAY SQ0008.

           DISPLAY SQ0009.

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
