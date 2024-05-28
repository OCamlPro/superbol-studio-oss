       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL016A. 
       
       
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

           01 COL1        PIC 9(6).
           01 COL2        PIC 9(6).
           01 TABKEY      PIC 9(6).
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 
       
       EXEC SQL AT :DBS
              DECLARE CSKEY01N CURSOR FOR
                 SELECT COL1, COL2 FROM TAB_A
                 WHERE KEY01 >= :TABKEY
                 ORDER BY KEY01 ASC
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
              CONNECT TO :DATASRC AS :DBS USER :DBUSR USING :DBPWD
           END-EXEC.      

           DISPLAY 'CONNECT SQLCODE: ' SQLCODE.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

        MOVE 6 TO TABKEY.

      *  open cursor
            EXEC SQL
                OPEN CSKEY01N
            END-EXEC 
            DISPLAY 'OPEN SQLCODE: ' SQLCODE.
            DISPLAY 'OPEN SQLERRM: ' SQLERRM.

      *  loop until no more data
           PERFORM UNTIL SQLCODE < 0 OR SQLCODE = 100
       
      *  fetch a data item 
           EXEC SQL
               FETCH CSKEY01N INTO 
                 :COL1, :COL2
           END-EXEC

           DISPLAY 'FETCH SQLCODE: ' SQLCODE
       
      *  display the record

           DISPLAY 'COL1: [' COL1 ']'
           DISPLAY 'COL2: [' COL2 ']'

           END-PERFORM.
       
           DISPLAY 'All records in this table have been selected'. 
       
       CLOSE-LOOP.
      *  close the cursor 
           EXEC SQL 
               CLOSE CSKEY01N 
           END-EXEC. 
       

       EXEC SQL CONNECT RESET :DBS END-EXEC.

       100-EXIT. 
             STOP RUN.

       200-END.
