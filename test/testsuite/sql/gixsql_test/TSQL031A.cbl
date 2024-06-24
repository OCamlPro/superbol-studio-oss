       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL031A-1. 
       
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

           01 CID PIC 9(12).
           01 FLD1 PIC X(100).
           01 FLD2 PIC X(100).

           01 OP PIC X(64).

           01 F-REC-CNT        PIC 9(8).
           01 F-REC-CNT-ACTUAL PIC 9(8).

           01 CNT PIC 9(8).

           01 F-NATIVE-CRSR PIC X VALUE ' '.

           EXEC SQL DECLARE CRSR01 CURSOR FOR
                SELECT CID, FLD1, FLD2 FROM TAB00
           END-EXEC.

           EXEC SQL 
                INCLUDE SQLCA 
           END-EXEC. 
         
       PROCEDURE DIVISION. 
 
       000-CONNECT.

           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
           DISPLAY "F_REC_CNT" UPON ENVIRONMENT-NAME.
           ACCEPT F-REC-CNT FROM ENVIRONMENT-VALUE.
           DISPLAY "F_NATIVE_CRSR" UPON ENVIRONMENT-NAME.
           ACCEPT F-NATIVE-CRSR FROM ENVIRONMENT-VALUE.

           EXEC SQL WHENEVER SQLERROR PERFORM 500-SQLERROR END-EXEC.

           IF F-REC-CNT = 0 OR F-REC-CNT = LOW-VALUE THEN
                DISPLAY 'RECORD-COUNT NOT SET'
                MOVE 1 TO RETURN-CODE
                GO TO 100-EXIT
           END-IF.

           MOVE 'CONNECT' TO OP.
           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.

           MOVE 'START-TX' TO OP.
           IF F-NATIVE-CRSR <> ' ' THEN
               DISPLAY 'CURSORS: NATIVE'
               EXEC SQL
                  START TRANSACTION
               END-EXEC
           ELSE
               DISPLAY 'CURSORS: EMULATED'
           END-IF.

       100-MAIN.

           MOVE 1 TO CID.
           MOVE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO FLD1.
           MOVE 'abcdefghijklmnopqrstuvwxyz' TO FLD2.

           MOVE 'INSERT-LOOP' TO OP.
           PERFORM UNTIL CID > F-REC-CNT OR SQLCODE <> 0
              EXEC SQL
                   INSERT INTO TAB00 (CID, FLD1, FLD2)
                        VALUES(:CID, :FLD1, :FLD2)
              END-EXEC

              ADD 1 TO CID

           END-PERFORM.

           EXEC SQL 
                SELECT COUNT(*)
                    INTO :F-REC-CNT-ACTUAL
                    FROM TAB00
           END-EXEC.

           DISPLAY '# RECS REQUESTED: ' F-REC-CNT.
           DISPLAY '# RECS INSERTED : ' F-REC-CNT-ACTUAL.

      * read back

           EXEC SQL OPEN CRSR01 END-EXEC.

           MOVE 0 TO CNT.

           PERFORM UNTIL SQLCODE < 0 OR SQLCODE = 100
               
               EXEC SQL
                   FETCH CRSR01 INTO :CID, :FLD1, :FLD2
               END-EXEC 

               IF SQLCODE = 0 THEN
                  ADD 1 TO CNT
               END-IF

           END-PERFORM.

           EXEC SQL CLOSE CRSR01 END-EXEC.

           DISPLAY '# RECS READ: 'CNT.

           MOVE 'CONNECT RESET' TO OP.
           EXEC SQL
              CONNECT RESET
           END-EXEC.      
       
       100-EXIT. 
             STOP RUN.

       500-SQLERROR.
          DISPLAY FUNCTION TRIM(OP) '-SQLCODE  : ' SQLCODE
          DISPLAY FUNCTION TRIM(OP) '-SQLSTATE : ' SQLSTATE
          DISPLAY FUNCTION TRIM(OP) '-SQLERRM  : ' SQLERRMC(1:SQLERRML)
          MOVE 1 TO RETURN-CODE.

       999-END.

