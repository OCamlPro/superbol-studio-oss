       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL006B. 
       
       
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

           01  S-SQLCOMMAND.
               03 S-SQLCOMMAND-LEN    PIC S9(8) COMP-5.
               03 S-SQLCOMMAND-ARR    PIC X(250).
       PROCEDURE DIVISION. 
 
       000-CONNECT.

           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
           
           DISPLAY '***************************************'.
           DISPLAY " DATASRC  : " DATASRC.
           DISPLAY " AUTH     : " DBUSR.
           DISPLAY '***************************************'.

       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
         

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'CONNECT SQLCODE. ' SQLCODE
              DISPLAY 'CONNECT SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

           EXEC SQL START TRANSACTION END-EXEC.

           MOVE EXEC-SQLCOMMAND TO S-SQLCOMMAND-ARR.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(S-SQLCOMMAND-ARR))
             TO S-SQLCOMMAND-LEN.

           EXEC SQL AT :DBS
               PREPARE P1 FROM :S-SQLCOMMAND 
           END-EXEC.

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
