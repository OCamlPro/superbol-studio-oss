       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL041A. 
       
       
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

           01 BFLD1 PIC X(300) USAGE VARRAW.      
           01 BFLD2 PIC X(300) USAGE VARRAW.      

           01 REC-ID          PIC 9999.

           01 CUR-OP PIC X(32).
           01 COMPARE-RES PIC X(64).

       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
         
       PROCEDURE DIVISION. 
 
       000-CONNECT.

           EXEC SQL WHENEVER SQLERROR PERFORM 999-ERR END-EXEC.

           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
           
           DISPLAY '***************************************'.
           DISPLAY " DATASRC  : " DATASRC.
           DISPLAY " AUTH     : " DBUSR.
           DISPLAY '***************************************'.

           MOVE 'CONNECT' TO CUR-OP.
           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'CONNECT SQLCODE. ' SQLCODE
              DISPLAY 'CONNECT SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.

       100-MAIN.
            
           MOVE 'SELECT-1' TO CUR-OP.
           EXEC SQL
              SELECT 
                ID, DATA 
              INTO 
                :REC-ID, :BFLD1 
              FROM BINTEST
              WHERE ID = 1
           END-EXEC.
      
           MOVE 'INSERT-1' TO CUR-OP.
           EXEC SQL
              INSERT INTO BINTEST(ID, DATA)
                VALUES(2, :BFLD1)
           END-EXEC.

           MOVE 'COMPARE' TO CUR-OP.
           EXEC SQL
                SELECT 
                CASE WHEN
                    (SELECT DATA FROM BINTEST WHERE ID=1) = 
                    (SELECT DATA FROM BINTEST WHERE ID=2) 
                        THEN 'DATA COMPARE OK'
                        ELSE 'DATA COMPARE KO'
                END 
                INTO :COMPARE-RES
           END-EXEC.

           DISPLAY COMPARE-RES.
       
       100-DISCONNECT.

           MOVE 'RESET' TO CUR-OP.
           EXEC SQL
              CONNECT RESET
           END-EXEC.      
       
       100-EXIT. 
             STOP RUN.

       999-ERR.
            DISPLAY 'ERROR AT: ' CUR-OP
            DISPLAY 'SQLCODE : ' SQLCODE
            DISPLAY 'SQLERRMC: ' SQLERRMC(1:SQLERRML)
            MOVE 1 TO RETURN-CODE