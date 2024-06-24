       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL037B. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  

       FILE SECTION.
      
       WORKING-STORAGE SECTION. 
       
           01 DATASRC     PIC X(255).
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).

           01 CUR-STEP    PIC X(16).

           01 IDX         PIC 9(2).

           01 TAB00-REC.
                03 CID        PIC 9(12).
                03 FLD01      PIC S9(8) USAGE COMP-3.
                03 FLD02      PIC X(32).

           01 TMPNUM          PIC 9(4).

           01 VAR1            PIC 9(8).
           01 VAR2            PIC 9(8).
           01 VAR3            PIC X(8).
               
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       EXEC SQL
              DECLARE CRSR_TAB00 CURSOR FOR
                 SELECT * FROM TAB00
                    ORDER BY CID
                 FOR UPDATE
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

           MOVE 'DROP' TO CUR-STEP.
           EXEC SQL
            DROP TABLE IF EXISTS TAB00
           END-EXEC.

           MOVE 'CREATE' TO CUR-STEP.
           EXEC SQL
            CREATE TABLE TAB00 (
                CID INT, 
                FLD01 INT, 
                FLD02 VARCHAR(255), 
                PRIMARY KEY(CID))
           END-EXEC.

           MOVE 1      TO CID.
           MOVE -1     TO FLD01.
           MOVE 'ABCD' TO FLD02.
      
      * insert test records

           MOVE 'INSERT' TO CUR-STEP.
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

      * read test records back and update them

           MOVE 0 TO CID.
           MOVE 0 TO FLD01.
           MOVE SPACES TO FLD02.

           MOVE 'OPEN CRSR 1' TO CUR-STEP.
           EXEC SQL
               OPEN CRSR_TAB00 
           END-EXEC.
           DISPLAY 'SQLCODE OPEN CRSR_TAB00 (1): ' SQLCODE.

           MOVE 1 TO IDX.

           MOVE 30000 TO VAR1.
           MOVE 'YYYY' TO VAR2.

           PERFORM UNTIL SQLCODE < 0 OR SQLCODE = 100

               MOVE 'FETCH' TO CUR-STEP
               EXEC SQL
                   FETCH CRSR_TAB00 INTO :TAB00-REC
               END-EXEC
               
               DISPLAY IDX ' - 1 - FETCH SQLCODE: ' SQLCODE

               DISPLAY IDX ' - 1 - CID(FETCH)  : ' CID
               DISPLAY IDX ' - 1 - FLD01(FETCH): ' FLD01
               DISPLAY IDX ' - 1 - FLD02(FETCH): ' FLD02
               
               MOVE 'UPDATE' TO CUR-STEP

               IF SQLCODE <> 100 THEN

      ** this differs from other test cases because in SQLite
      ** we cannot update the key (actually we can, but this
      ** would cause an infinite loop, since cursors are 
      ** apparently dynamic)

                   EXEC SQL
                       UPDATE TAB00
                         SET 
                           FLD01 = FLD01 + :VAR1,
                           FLD02 = FLD02 || :VAR2
                         WHERE CURRENT OF CRSR_TAB00
                   END-EXEC

                   DISPLAY IDX ' - UPDATE-SQLCODE: ' SQLCODE

               END-IF

               ADD 1 TO IDX                

           END-PERFORM.     
           
           EXEC SQL
               CLOSE CRSR_TAB00 
           END-EXEC.
           DISPLAY 'SQLCODE CLOSE CRSR_TAB00 (1): ' SQLCODE.

      * second round, check if the update worked

           MOVE 0 TO CID.
           MOVE 0 TO FLD01.
           MOVE SPACES TO FLD02.

           MOVE 'OPEN CRSR 2' TO CUR-STEP.
           EXEC SQL
               OPEN CRSR_TAB00 
           END-EXEC.
           DISPLAY 'SQLCODE OPEN CRSR_TAB00 (2): ' SQLCODE.

           MOVE 1 TO IDX.

           PERFORM UNTIL SQLCODE < 0 OR SQLCODE = 100

               MOVE 'FETCH' TO CUR-STEP
               EXEC SQL
                   FETCH CRSR_TAB00 INTO :TAB00-REC
               END-EXEC
               
               DISPLAY IDX ' - 2 - FETCH SQLCODE: ' SQLCODE

               DISPLAY IDX ' - 2 - CID(FETCH)  : ' CID
               DISPLAY IDX ' - 2 - FLD01(FETCH): ' FLD01
               DISPLAY IDX ' - 2 - FLD02(FETCH): ' FLD02

               ADD 1 TO IDX                

           END-PERFORM.     
           
           EXEC SQL
               CLOSE CRSR_TAB00 
           END-EXEC.
           DISPLAY 'SQLCODE CLOSE CRSR_TAB00 (2): ' SQLCODE.


      * close connection
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