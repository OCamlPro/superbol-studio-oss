       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL013A. 
       
       
       ENVIRONMENT DIVISION. 
       
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-AT. 
       OBJECT-COMPUTER. IBM-AT. 
       
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 

            SELECT PAYROLL-REGISTER-DATA
                ASSIGN TO EXTERNAL DATAIN
                          ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.  

       FILE SECTION.
      
       FD  PAYROLL-REGISTER-DATA
           LABEL RECORDS ARE OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS PAYROLL-REGISTER-RECORD.
      
       01  PAYROLL-REGISTER-RECORD.
           03  PRR-DEPARTMENT-NUMBER   PIC 9(02).
           03  FILLER                  PIC X(01).
           03  PRR-EMPLOYEE-KEY.
               05  PRR-EMPLOYEE-NO     PIC 9(04).
               05  FILLER              PIC X(01).
               05  PRR-GENDER          PIC X(01).
               05  FILLER              PIC X(01).
               05  PRR-EMPLOYEE-NAME   PIC X(20).

       WORKING-STORAGE SECTION. 
       
       EXEC SQL 
        INCLUDE EMPREC 
       END-EXEC. 
       
           01 DATASRC     PIC X(64).
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).
                          
           01 MY-FD       PIC X(64) VALUE 'MYFD'.
           01 MY-FD2      PIC X(64) VALUE 'FD'.
           01 FD-01       PIC X(64) VALUE 'FD-01'.
           01 MY-FD-01    PIC X(64) VALUE 'MY-FD-01'.
           
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

           EXEC SQL
             SELECT 1 INTO :T1
           END-EXEC.

           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.     
           DISPLAY 'SELECT SQLCODE: ' SQLCODE.

           DISPLAY 'RES: ' T1.          
           IF T1 = 1 THEN
               PERFORM HANDLE-MY-FD THRU HANDLE-MY-FD-EX
           END-IF.

           EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.

       HANDLE-MY-FD.
           DISPLAY 'T1: ' T1.
           DISPLAY 'FD: ' MY-FD.
       HANDLE-MY-FD-EX.
           EXIT.

       200-END.
