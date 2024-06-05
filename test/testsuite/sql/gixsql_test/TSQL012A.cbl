       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL012A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       
       DATA DIVISION.  
       
       FILE SECTION.  
       
       WORKING-STORAGE SECTION. 
       
       EXEC SQL 
        INCLUDE EMPREC 
       END-EXEC. 
       
           01 DATASRC PIC X(64).
           01 DBS     PIC X(64).
           01 DBUSR   PIC X(64).
           01 DBPWD   PIC X(64).
           
           01 T1          PIC 9(3) VALUE 0.  
           01 TABROWID    PIC 9(8) VALUE 0.  
           01 DESCRIPTOR  PIC 9(8) VALUE 0.  
           01 OID         PIC 9(8) VALUE 0.  
           01 LEN         PIC 9(8) VALUE 0.  
           01 RES         PIC 9(8) VALUE 0.  

           01 VAR-L       PIC 9(8) VALUE 0.       
           01 VAR-I       PIC 9(8) VALUE 0.       
           01 VAR-ROWID   PIC 9(8) VALUE 0.       
       
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

           MOVE 4 TO T1.

           EXEC SQL AT :DBS
             SELECT
                TABROWID INTO :TABROWID FROM TAB_A 
             WHERE
                HISTID = (SELECT MAX(HISTID) FROM TAB_A WHERE
                           REFNR         = :T1)
           END-EXEC.

           DISPLAY 'SELECT SQLCODE: ' SQLCODE.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.     

           DISPLAY 'RES: ' TABROWID.           

           EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.