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
              INCLUDE SQLCA 
           END-EXEC. 

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
           END-EXEC.   

           EXEC SQL AT CONN1
              DECLARE CRSR01 CURSOR FOR
                  SELECT FLD1 FROM TAB1 ORDER BY FLD1
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
               SELECT COUNT(*) INTO :T1 FROM EMPTABLE
           END-EXEC.  

           EXEC SQL AT CONN1 
                INSERT INTO TAB1 (FLD1) VALUES (1)
           END-EXEC.
           DISPLAY 'CONNECT INSERT(1-1): ' SQLCODE
           DISPLAY 'CONNECT INSERT(1-1): ' SQLERRMC(1:SQLERRML)

           EXEC SQL AT CONN1 
                INSERT INTO TAB1 (FLD1) VALUES (3)
           END-EXEC.
           DISPLAY 'CONNECT INSERT(1-2): ' SQLCODE
           DISPLAY 'CONNECT INSERT(1-2): ' SQLERRMC(1:SQLERRML)

           EXEC SQL AT CONN1 
                INSERT INTO TAB1 (FLD1) VALUES (5)
           END-EXEC.
           DISPLAY 'CONNECT INSERT(1-3): ' SQLCODE
           DISPLAY 'CONNECT INSERT(1-3): ' SQLERRMC(1:SQLERRML)
 
           DISPLAY 'SELECT SQLCODE : ' SQLCODE.
         
           EXEC SQL AT CONN1
               SELECT SUM(FLD1) INTO :T1 FROM TAB1
           END-EXEC. 
           DISPLAY 'CONNECT SUM(1): ' SQLCODE

           EXEC SQL AT CONN1 SAVEPOINT SP1 END-EXEC.
           DISPLAY 'SQLCODE SAVEPOINT SP1: ' SQLCODE.

           EXEC SQL AT CONN2 SAVEPOINT SP2 END-EXEC.
           DISPLAY 'SQLCODE SAVEPOINT SP2: ' SQLCODE.

           EXEC SQL 
               OPEN CRSR01
           END-EXEC.
     
           EXEC SQL
               FETCH CRSR01 INTO :T1
           END-EXEC

             

           EXEC SQL CLOSE CRSR01 END-EXEC.     

           EXEC SQL CLOSE CRSR02 END-EXEC.          
           
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.     

           DISPLAY 'RES: ' T1.           

           EXEC SQL CONNECT RESET END-EXEC.
           EXEC SQL DISCONNECT CONN2 END-EXEC.
           
       100-EXIT. 
      *       STOP RUN.