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
                       UPDATE TAB00
                         SET 
                           FLD01 = FLD01 + 20000,
                           FLD02 = CONCAT(FLD02, 'XXXX')
                         WHERE CURRENT OF CRSR_TAB00
                   END-EXEC

                   
           EXEC SQL DECLARE APXT.P_HISTORY_TD TABLE                     00120001
           ( DATAVAL                        DATE NOT NULL,              00130001
             DATARIL                        DATE NOT NULL,              00140001
             PROGVER                        INTEGER NOT NULL,           00150001
             DATAVERS                       DATE NOT NULL,              00160001
             DATALOAD                       TIMESTAMP NOT NULL,         00170001
             USERNAME                       VARCHAR(15) NOT NULL,       00180001
             CID                            VARCHAR(60) NOT NULL,       00190001
             FILENAME                       VARCHAR(60) NOT NULL        00200001
           ) END-EXEC.                                                  00210001
      *****************
           EXEC SQL
               SELECT COUNT(*) INTO :T1 FROM EMPTABLE
           END-EXEC. 

           DISPLAY 'SELECT SQLCODE : ' SQLCODE.
           
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.     

           DISPLAY 'RES: ' T1.           

           EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
      *       STOP RUN.