       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL021C. 
       
       
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
       
           01 DATASRC         PIC X(64).
           01 DBUSR           PIC X(64).
           01 DBPWD           PIC X(64).
           01 DBNAME          PIC X(64).
                              
           01 T1              PIC 9(4).
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.

         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         
         DISPLAY "DBUSR" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         
         DISPLAY "DBPWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBPWD FROM ENVIRONMENT-VALUE.
         
         DISPLAY "DBNAME" UPON ENVIRONMENT-NAME.
         ACCEPT DBNAME FROM ENVIRONMENT-VALUE.

       100-MAIN.


      * mode 6 (anonymous) 

         EXEC SQL
            CONNECT TO            :DBNAME
                    USER          :DBUSR
                    USING         :DATASRC
                    IDENTIFIED BY :DBPWD
         END-EXEC.
         DISPLAY 'CONNECT 6A SQLCODE: ' SQLCODE.
         IF SQLCODE <> 0 THEN
            GO TO 100-EXIT
         END-IF.   

         
       100-EXIT. 
             STOP RUN.

       200-END.
