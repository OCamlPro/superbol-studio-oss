       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL005B. 
       
       
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

      *    01 BFLD SQL TYPE IS BINARY(100).
      *    01 VBFLD SQL TYPE IS VARBINARY(100).
      *    
      *    01 CFLD SQL TYPE IS CHAR(100).

      *    01 VCFLD PIC X(100) VARYING.      

      *    01 VBFLD1 PIC X(100) USAGE VARYING.      
      *    01 VBFLD2 PIC X(100) USAGE VARRAW.      

           01 NUM1          PIC 99V99.

           01 TOT           PIC 9(8).
           
           01 SDIGIT.
              03 SDIGIT01 PIC X(1).
              03 SDIGIT02 PIC X(1).
                
           01 NDIGIT REDEFINES SDIGIT PIC S9(04) COMP.


         
       PROCEDURE DIVISION. 
 
       000-CONNECT.

           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.

       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
           
           DISPLAY '***************************************'.
           DISPLAY " DATASRC  : " DATASRC.
           DISPLAY " AUTH     : " DBUSR.
           DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'CONNECT SQLCODE. ' SQLCODE
              DISPLAY 'CONNECT SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

           EXEC SQL
              SELECT BFLD, VBFLD, CFLD, VCFLD, NUM1 
                INTO :BFLD, :VBFLD, :CFLD, :VCFLD, :NUM1 FROM BINTEST
           END-EXEC.
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'SELECT SQLCODE. ' SQLCODE
              DISPLAY 'SELECT SQLERRMC. ' SQLERRMC
              GO TO 100-EXIT
           END-IF.
           
           MOVE LOW-VALUES TO SDIGIT01.
           MOVE LOW-VALUES TO SDIGIT02.

           MOVE VBFLD-ARR(1:1) TO SDIGIT02.
           DISPLAY 'DIGIT1 : ' NDIGIT.

           MOVE VBFLD-ARR(2:1) TO SDIGIT02.
           DISPLAY 'DIGIT2 : ' NDIGIT.

           MOVE VBFLD-ARR(83:1) TO SDIGIT02.
           DISPLAY 'DIGIT83: ' NDIGIT.

           DISPLAY 'BFLD        : [' BFLD ']'.
      *    DISPLAY 'BFLD LENGTH: ' LENGTH OF BFLD.

           DISPLAY 'VBFLD-ARR  : [' VBFLD-ARR(1:VBFLD-LEN) ']'.
           DISPLAY 'VBFLD-ARR  : [' VBFLD-ARR(1:88) ']'.
           DISPLAY 'VBFLD-LEN: ' VBFLD-LEN.

           DISPLAY 'CFLD        : [' CFLD ']'.
      *    DISPLAY 'CFLD LENGTH: ' LENGTH OF CFLD.

           DISPLAY 'VCFLD-ARR  : [' VCFLD-ARR(1:VCFLD-LEN) ']'.
           DISPLAY 'VCFLD-LEN: ' VCFLD-LEN.

           EXEC SQL
              CONNECT RESET
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'DISCONNECT SQLCODE. ' SQLCODE
              DISPLAY 'DISCONNECT SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.

       
       100-EXIT. 
             STOP RUN.
