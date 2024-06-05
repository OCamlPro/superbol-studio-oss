       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL025A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  

       FILE SECTION.
      
       WORKING-STORAGE SECTION. 
       
           01 DATASRC     PIC X(64).
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).

           01 CUR-STEP    PIC X(16).

           01 IDX         PIC 9(2).

           01 TAB00-REC.
                03 CID        PIC 9(12).
                03 FLD01      PIC S9(4) USAGE COMP-3.
                03 FLD02      PIC X(12).

           01 TMPNUM          PIC 9(4).
               
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       EXEC SQL
              DECLARE CRSR_TAB00 CURSOR FOR
                 SELECT * FROM TAB00
                    ORDER BY CID
       END-EXEC.

       PROCEDURE DIVISION. 
 
       000-CONNECT.
           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
           ACCEPT DBPWD FROM ENVIRONMENT-VALUE.

           EXEC SQL WHENEVER SQLERROR GO TO 999-PRG-ERR END-EXEC.

           MOVE 'CONNECT' TO CUR-STEP.
           EXEC SQL
              CONNECT :DBUSR IDENTIFIED BY :DBPWD
                        USING :DATASRC
           END-EXEC.        

           MOVE 1      TO CID.
           MOVE -1     TO FLD01.
           MOVE 'ABCD' TO FLD02.
      
           MOVE 'INSERT' TO CUR-STEP.
           EXEC SQL
               INSERT INTO TAB00
                   VALUES (:TAB00-REC)
           END-EXEC.
           DISPLAY 'INSERT SQLCODE: ' SQLCODE

           MOVE 'SELECT(*)' TO CUR-STEP.
           EXEC SQL
               SELECT *
                    INTO :TAB00-REC
                 FROM TAB00 
                 WHERE CID=1
           END-EXEC.

           DISPLAY 'SELECT(*) SQLCODE: ' SQLCODE

           DISPLAY 'CID(*)  : ' CID.
           DISPLAY 'FLD01(*): ' FLD01.
           DISPLAY 'FLD02(*): ' FLD02.

           MOVE 'SELECT(ENUM)' TO CUR-STEP.
           EXEC SQL
               SELECT CID, FLD01, FLD02
                    INTO :TAB00-REC
                 FROM TAB00 
                 WHERE CID=1
           END-EXEC.
           DISPLAY 'SELECT(ENUM) SQLCODE: ' SQLCODE
       
           DISPLAY 'CID(ENUM)  : ' CID.
           DISPLAY 'FLD01(ENUM): ' FLD01.
           DISPLAY 'FLD02(ENUM): ' FLD02.

      * now with a cursor
      
      * insert test records

           MOVE 'INSERT10' TO CUR-STEP.
           MOVE 1 TO IDX.

           PERFORM UNTIL IDX > 10 OR SQLCODE < 0 OR SQLCODE = 100

               MOVE IDX TO CID
               
               MOVE IDX TO TMPNUM
               ADD 100 TO TMPNUM
               MOVE TMPNUM TO FLD01
               
               ADD 100 TO TMPNUM
               MOVE TMPNUM TO FLD02
               
               EXEC SQL
                    INSERT INTO TAB00 VALUES (:TAB00-REC)
               END-EXEC     
               
               DISPLAY IDX ' - INSERT SQLCODE: ' IDX SQLCODE
               
               ADD 1 TO IDX

           END-PERFORM.

      * read test records back

           MOVE 0 TO CID.
           MOVE 0 TO FLD01.
           MOVE SPACES TO FLD02.

           EXEC SQL AT CONN1
               OPEN CRSR_TAB00 
           END-EXEC.
           DISPLAY 'SQLCODE OPEN CRSR_TAB00 : ' SQLCODE.

           MOVE 1 TO IDX.

           PERFORM UNTIL SQLCODE < 0 OR SQLCODE = 100

               EXEC SQL AT CONN1
                   FETCH CRSR_TAB00 INTO :TAB00-REC
               END-EXEC
               
               DISPLAY IDX ' - FETCH SQLCODE: ' SQLCODE
               
               DISPLAY IDX ' - CID(FETCH)  : ' CID
               DISPLAY IDX ' - FLD01(FETCH): ' FLD01
               DISPLAY IDX ' - FLD02(FETCH): ' FLD02
               
               ADD 1 TO IDX
                  
           END-PERFORM.      

           MOVE 'DISCONNECT' TO CUR-STEP.
           EXEC SQL
              CONNECT RESET
           END-EXEC.        

       200-EXIT.
           STOP RUN.

       999-PRG-ERR.
           DISPLAY 'ERR - ' CUR-STEP ' : ' SQLCODE.
           DISPLAY 'ERR - ' CUR-STEP ' : ' SQLERRMC(1:SQLERRML).
           MOVE -1 TO RETURN-CODE.