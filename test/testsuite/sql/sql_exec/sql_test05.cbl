       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL001A. 
       
       
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
           01 DBPWD  PIC X(64).
           
           01 T1     PIC 9(3) VALUE 0.  
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
         ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
         ACCEPT DBPWD FROM ENVIRONMENT-VALUE.
         
         DISPLAY '***************************************'.
         DISPLAY " DATASRC  : " DATASRC.
         DISPLAY " DB       : " DBUSR.
         DISPLAY " USER     : " DBPWD.
         DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
           END-EXEC.      
           
           DISPLAY 'CONNECT SQLCODE: ' SQLCODE

           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

           EXEC SQL
              START TRANSACTION
           END-EXEC.   


           EXEC SQL
             SELECT 1 INTO :T1
           END-EXEC.           

           EXEC SQL AT :DBS
             SELECT
                TABROWID INTO :TABROWID FROM TAB_A 
             WHERE
                HISTID = (SELECT MAX(HISTID) FROM TAB_A WHERE
                           REFNR         = :T1)
           END-EXEC.

           EXEC SQL
              SELECT CONCAT('[', CONCAT (VCFLD, ']'))
              INTO :OUT1 FROM TAB00
              WHERE CID = 99
           END-EXEC.

           EXEC SQL
              SELECT CONCAT('[', CONCAT(CFLD, ']'))
              INTO :OUT2 FROM TAB00
              WHERE CID = 99
           END-EXEC.

           EXEC SQL
              INSERT INTO TAB00 (CID, VCFLD, CFLD)
                VALUES(98, :VCFLD2, :CFLD2)
           END-EXEC.

           EXEC SQL
              COMMIT
           END-EXEC.                 
           


           DISPLAY 'SELECT SQLCODE : ' SQLCODE.
           
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.     

           DISPLAY 'RES: ' T1.           

           EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
      *       STOP RUN.