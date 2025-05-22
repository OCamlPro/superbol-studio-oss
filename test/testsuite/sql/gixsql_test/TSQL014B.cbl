       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL014B. 
       
       
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
           01 DBS         PIC X(64).
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).
           
           01 TABKEY                   PIC  9(018).
           01 LEN                      PIC S9(09).
           01 DESCRIPTOR               PIC S9(09).
           01 RESINT                   PIC S9(09).
           01 OFFSET                   PIC S9(09) VALUE 1.
       
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

         EXEC SQL AT :DBS
             SELECT lo_open(BLOBFLD,393216) INTO :DESCRIPTOR
                    FROM TAB WHERE TABKEY = :TABKEY
         END-EXEC.

         EXEC SQL AT :DBS
             SELECT lo_lseek (:DESCRIPTOR, :OFFSET,0) INTO :RESINT
                    FROM TAB WHERE TABKEY = :TABKEY
         END-EXEC.

         EXEC SQL AT :DBS
             SELECT LENGTH INTO :LEN FROM TAB
                    WHERE TABKEY= :TABKEY
         END-EXEC.

         EXEC SQL AT :DBS
             SELECT lo_close (:DESCRIPTOR) INTO :RESINT
                    FROM TAB WHERE TABKEY = :TABKEY
         END-EXEC.

         EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.

       200-END.
