       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL019A. 
       
       
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

           01 REC-ID        PIC 9(4).

           01 TORNW-1       PIC S9(018)V9(12) COMP-3.
           01 TORNW-2       PIC S9(018)       COMP-3.
           01 TORNW-3       PIC 9(018)        COMP-3.
           01 TORNW-4       PIC 9(018)V9(12)  COMP-3.

           01 TORNR-1       PIC S9(018)V9(12) COMP-3.
           01 TORNR-2       PIC S9(018)       COMP-3.
           01 TORNR-3       PIC 9(018)        COMP-3.
           01 TORNR-4       PIC 9(018)V9(12)  COMP-3.
       
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

           DISPLAY 'CONNECT SQLCODE: ' SQLCODE.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.
      
           MOVE 1 TO REC-ID.
           MOVE -42.74 TO TORNW-1.

      * PIC S9(018)V9(12) COMP-3.

           EXEC SQL
                INSERT INTO TAB_A(ID, TORNW) VALUES (:REC-ID, :TORNW-1)
           END-EXEC.
           DISPLAY 'INSERT 1 SQLCODE: ' SQLCODE.
           DISPLAY 'INSERT 1 DATA   : ' TORNW-1.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

           MOVE 2 TO REC-ID.
           MOVE -112 TO TORNW-2.
       
      * PIC S9(018)       COMP-3.
       
           
           EXEC SQL
                INSERT INTO TAB_A(ID, TORNW) VALUES (:REC-ID, :TORNW-2)
           END-EXEC.
           DISPLAY 'INSERT 2 SQLCODE: ' SQLCODE.
           DISPLAY 'INSERT 2 DATA   : ' TORNW-2.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.
       
           MOVE 3 TO REC-ID.
           MOVE 237 TO TORNW-3.
       
      * PIC 9(018)        COMP-3.
       
           EXEC SQL
                INSERT INTO TAB_A(ID, TORNW) VALUES (:REC-ID, :TORNW-3)
           END-EXEC.
           DISPLAY 'INSERT 3 SQLCODE: ' SQLCODE.
           DISPLAY 'INSERT 3 DATA   : ' TORNW-3.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.
       
           MOVE 4 TO REC-ID.
           MOVE 127.22 TO TORNW-4.
       
      * PIC 9(018)V9(12)  COMP-3.
       
           EXEC SQL
                INSERT INTO TAB_A(ID, TORNW) VALUES (:REC-ID, :TORNW-4)
           END-EXEC.
           DISPLAY 'INSERT 4 SQLCODE: ' SQLCODE.
           DISPLAY 'INSERT 4 DATA   : ' TORNW-4.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

      * read tests

            EXEC SQL
                 SELECT TORNW INTO :TORNR-1 FROM TAB_A
                     WHERE ID = 1
            END-EXEC.
            DISPLAY 'SELECT 1 SQLCODE: ' SQLCODE.
            IF SQLCODE <> 0 THEN
               GO TO 100-EXIT
            END-IF.
            DISPLAY 'SELECT 1 DATA   : ' TORNR-1.

           EXEC SQL
                SELECT TORNW INTO :TORNR-2 FROM TAB_A
                    WHERE ID = 2
           END-EXEC.
           DISPLAY 'SELECT 2 SQLCODE: ' SQLCODE.
           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.
           DISPLAY 'SELECT 2 DATA   : ' TORNR-2.

            EXEC SQL
                 SELECT TORNW INTO :TORNR-3 FROM TAB_A
                     WHERE ID = 3
            END-EXEC.
            DISPLAY 'SELECT 3 SQLCODE: ' SQLCODE.
            IF SQLCODE <> 0 THEN
               GO TO 100-EXIT
            END-IF.
            DISPLAY 'SELECT 3 DATA   : ' TORNR-3.
       
            EXEC SQL
                 SELECT TORNW INTO :TORNR-4 FROM TAB_A
                     WHERE ID = 4
            END-EXEC.
            DISPLAY 'SELECT 4 SQLCODE: ' SQLCODE.
            IF SQLCODE <> 0 THEN
               GO TO 100-EXIT
            END-IF.
            DISPLAY 'SELECT 4 DATA   : ' TORNR-4.

           EXEC SQL COMMIT END-EXEC.
            
           EXEC SQL CONNECT RESET END-EXEC.

           IF TORNW-1 = TORNR-1 THEN
                DISPLAY 'RES 1 : OK'
           ELSE
                DISPLAY 'RES 1 : KO'
           END-IF.

           IF TORNW-2 = TORNR-2 THEN
                DISPLAY 'RES 2 : OK'
           ELSE
                DISPLAY 'RES 2 : KO'
           END-IF.

           IF TORNW-3 = TORNR-3 THEN
                DISPLAY 'RES 3 : OK'
           ELSE
                DISPLAY 'RES 3 : KO'
           END-IF.

           IF TORNW-4 = TORNR-4 THEN
                DISPLAY 'RES 4 : OK'
           ELSE
                DISPLAY 'RES 4 : KO'
           END-IF.
       100-EXIT. 

           IF SQLCODE <> 0 THEN
              DISPLAY 'SQLERRM ' SQLERRMC(1:SQLERRML)
           END-IF.

           STOP RUN.

       200-END.
