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
           01 FLD1 PIC X(64).
           01 FLD2 PIC X(64).

           01 OP PIC X(64).

           EXEC SQL 
                INCLUDE SQLCA 
           END-EXEC. 

           01 F-REC-CNT        PIC 9(8) VALUE 10.
           01 F-REC-CNT-ACTUAL PIC 9(8).
         
       PROCEDURE DIVISION. 
 
       000-CONNECT.

           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.

           EXEC SQL WHENEVER SQLERROR PERFORM 500-SQLERROR END-EXEC.

           EXEC SQL WHENEVER NOT FOUND PERFORM 600-NOT-FOUND END-EXEC.

           MOVE 'CONNECT' TO OP.
           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.

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

      * this will obviously fail

           MOVE 'SEL-ERR' TO OP.
           EXEC SQL
                SELECT CID, FLD1, FLD2
                    INTO :CID, :FLD1, :FLD2
                    FROM TAB00 
                    WHERE CID = 99
           END-EXEC.

           DISPLAY FUNCTION TRIM(OP) ' SQLCODE  : ' SQLCODE

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

       600-NOT-FOUND.
          DISPLAY FUNCTION TRIM(OP) '*SQLCODE  : ' SQLCODE
          DISPLAY FUNCTION TRIM(OP) '*SQLSTATE : ' SQLSTATE
          DISPLAY FUNCTION TRIM(OP) '*SQLERRM  : ' SQLERRMC(1:SQLERRML)
          MOVE 0 TO RETURN-CODE.

       999-END.

