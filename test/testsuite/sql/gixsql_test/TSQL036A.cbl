       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL036A. 
       
       
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
           01 DBUSR   PIC X(64).
           01 DBPWD   PIC X(64).
           
           01 TESTGRP.
            03 TESTNUM     PIC S9(8).     
            03 TESTREM     PIC 9(2).     
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBPWD FROM ENVIRONMENT-VALUE.

         EXEC SQL
            CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
         END-EXEC.      
         
         DISPLAY 'CONNECT SQLCODE: ' SQLCODE
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.

       100-MAIN.

           EXEC SQL
             SELECT
                TESTNUM INTO :TESTNUM FROM TAB01 
           END-EXEC.


           DISPLAY 'SELECT SQLCODE : ' SQLCODE.
           DISPLAY 'SELECT SQLSTATE: ' SQLSTATE.

      * we do not check SQLCODE and stop on error, so we can
      * display the field content and SQLCODE itself

           DISPLAY 'RES: [' TESTNUM ']'.           
           DISPLAY 'REM: [' TESTREM ']'.           

           EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.