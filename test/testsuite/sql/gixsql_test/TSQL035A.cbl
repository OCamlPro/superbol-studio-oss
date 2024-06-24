       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL035A. 
       
       
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
           01 DBS         PIC X(64).

           01  SQLCOMMAND.
               49 SQLCOMMAND-LEN    PIC S9(8) COMP-5.
               49 SQLCOMMAND-ARR    PIC X(250).

           01 IDX         PIC 9(8).
           01 VAR-TMP     PIC 9(8).
           01 VAR-RES     PIC 9(8).

           01 CID         PIC 9(8).
           01 FLD         PIC X(100).
           
           01 CUR-STEP    PIC X(16).
           
           01 PREPSTMT-2-NAME   PIC X(64).
               
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       EXEC SQL AT :DBS
           DECLARE VM1 CURSOR FOR 
                SELECT CID, FLD FROM TAB01
       END-EXEC.
      
       EXEC SQL AT :DBS
           DECLARE VM2 CURSOR FOR 
                SELECT CID, FLD FROM TAB02
       END-EXEC.
      
       EXEC SQL AT :DBS
           DECLARE VM3 CURSOR WITH HOLD FOR 
                SELECT CID, FLD FROM TAB03
       END-EXEC.

       EXEC SQL AT :DBS
           DECLARE VM4 CURSOR FOR 
                SELECT CID, FLD FROM TAB04
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

           MOVE 'CONN1' TO DBS.

           MOVE 'CONNECT' TO CUR-STEP.
           EXEC SQL
             CONNECT :DBUSR
                     IDENTIFIED BY :DBPWD
                     AT            :DBS
                     USING         :DATASRC
           END-EXEC.

      * INSERTs

           MOVE 1 TO IDX

           PERFORM UNTIL IDX > 10 OR SQLCODE < 0 OR SQLCODE = 100
               MOVE 'INSERT1' TO CUR-STEP
               MOVE IDX TO VAR-TMP
               EXEC SQL AT :DBS
                   INSERT INTO TAB01 (CID)
                       VALUES (:VAR-TMP)
               END-EXEC

               ADD 1 TO IDX
           END-PERFORM.

           MOVE 1 TO IDX

           PERFORM UNTIL IDX > 10 OR SQLCODE < 0 OR SQLCODE = 100
               MOVE 'INSERT2' TO CUR-STEP
               MOVE IDX TO VAR-TMP
               EXEC SQL AT :DBS
                   INSERT INTO TAB02 (CID)
                       VALUES (:VAR-TMP)
               END-EXEC

               ADD 1 TO IDX
           END-PERFORM.

           MOVE 1 TO IDX

           PERFORM UNTIL IDX > 10 OR SQLCODE < 0 OR SQLCODE = 100
               MOVE 'INSERT3' TO CUR-STEP
               MOVE IDX TO VAR-TMP
               EXEC SQL AT :DBS
                   INSERT INTO TAB03 (CID)
                       VALUES (:VAR-TMP)
               END-EXEC

               ADD 1 TO IDX
           END-PERFORM.

           MOVE 1 TO IDX

           PERFORM UNTIL IDX > 10 OR SQLCODE < 0 OR SQLCODE = 100
               MOVE 'INSERT4' TO CUR-STEP
               MOVE IDX TO VAR-TMP
               EXEC SQL AT :DBS
                   INSERT INTO TAB04 (CID)
                       VALUES (:VAR-TMP)
               END-EXEC

               ADD 1 TO IDX
           END-PERFORM.

      * VM1 : READ 1 + CLOSE

           MOVE 'OPEN-VM1' TO CUR-STEP. 
           DISPLAY 'OPEN-VM1'.
           EXEC SQL
               OPEN VM1 
           END-EXEC.
          
           EXEC SQL
               FETCH VM1 
               INTO :CID, :FLD
           END-EXEC.
           DISPLAY 'VM1 - CID: ' CID.
          
           EXEC SQL
               CLOSE VM1 
           END-EXEC.
           DISPLAY 'CLOSED VM1'.

      * VM2 : READ 2 + CLOSE

           MOVE 'OPEN-VM2' TO CUR-STEP. 
           MOVE 'OPEN-VM2' TO CUR-STEP. 
           EXEC SQL
               OPEN VM2 
           END-EXEC.
          
           EXEC SQL
               FETCH VM2 
               INTO :CID, :FLD
           END-EXEC.
           DISPLAY 'VM2 - CID: ' CID.
          
           EXEC SQL
               CLOSE VM2 
           END-EXEC.
           DISPLAY 'CLOSED VM2'.

      * VM3 : READ 3 - DO NOT CLOSE

           MOVE 'OPEN-VM3' TO CUR-STEP. 
           MOVE 'OPEN-VM3' TO CUR-STEP. 
           EXEC SQL 
               OPEN VM3 
           END-EXEC.
          
           EXEC SQL
               FETCH VM3
               INTO :CID, :FLD
           END-EXEC.
           DISPLAY 'VM3(1) - CID: ' CID.
          
           DISPLAY 'VM3 WILL *NOT* BE CLOSED'.

      * VM4 : READ 4 + CLOSE

           MOVE 'OPEN-VM4' TO CUR-STEP. 
           MOVE 'OPEN-VM4' TO CUR-STEP. 
           EXEC SQL
               OPEN VM4 
           END-EXEC.
          
           EXEC SQL
               FETCH VM4
               INTO :CID, :FLD
           END-EXEC.
           DISPLAY 'VM4 - CID: ' CID.
          
           EXEC SQL 
               CLOSE VM4 
           END-EXEC.
           DISPLAY 'CLOSED VM4'.

      * COMMIT

      *    MOVE 'COMMIT' TO CUR-STEP.
      *    EXEC SQL AT :DBS COMMIT END-EXEC.
      
      * VM3 : READ 3 (AGAIN) + CLOSE
         
           MOVE 'FETCH-VM3-2' TO CUR-STEP.
           EXEC SQL
               FETCH VM3
               INTO :CID, :FLD
           END-EXEC.
           DISPLAY 'VM3(2) - CID: ' CID.

           MOVE 'CLOSE-VM3' TO CUR-STEP.
           EXEC SQL
               CLOSE VM3 
           END-EXEC.
           DISPLAY 'CLOSED VM3'.

      * DONE  

           MOVE 'DISCONNECT' TO CUR-STEP.
           EXEC SQL
              CONNECT RESET :DBS
           END-EXEC.        

       200-EXIT.
           STOP RUN.

       999-PRG-ERR.
           DISPLAY 'ERR - ' CUR-STEP ' : ' SQLCODE.
           DISPLAY 'ERR - ' CUR-STEP ' : ' SQLERRMC(1:SQLERRML).
           MOVE -1 TO RETURN-CODE.