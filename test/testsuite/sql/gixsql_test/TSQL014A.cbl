       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL014A. 
       
       
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
       
           01 DATASRC     PIC X(64).
           01 DBS         PIC X(64).
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).
           01 BLOB1       PIC X(64).
           
           01 LEN         PIC 9(8) COMP-3.
           01 OFFSET      PIC 9(8) COMP-3.
           01 REC1        PIC X(1000000).

           01 T1          PIC 9(3) VALUE 0.  
       
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

         EXEC SQL AT :DBS ALLOCATE :BLOB1 END-EXEC.
         EXEC SQL AT :DBS
                LOB WRITE :LEN FROM :REC1
                INTO :BLOB1 AT :OFFSET
         END-EXEC.
         EXEC SQL AT :DBS FREE :BLOB1 END-EXEC

         EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.

       200-END.
