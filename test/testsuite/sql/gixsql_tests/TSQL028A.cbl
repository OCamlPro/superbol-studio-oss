       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL028A. 
       
       
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
           
           01 DBS       PIC X(64).
           01 OID       PIC X(64).

               
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

           MOVE 'CONN1' TO DBS.

           EXEC SQL
             CONNECT :DBUSR
                     IDENTIFIED BY :DBPWD
                     AT            :DBS
                     USING         :DATASRC
           END-EXEC.

           EXEC SQL
                DELETE FROM POS WHERE OID = :OID
           END-EXEC.

           EXEC SQL
              CONNECT RESET :DBS
           END-EXEC.        

       200-EXIT.
           STOP RUN.
