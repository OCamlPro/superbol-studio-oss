       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL020A. 
       
       
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
           01 DBS         PIC X(64).
           01 DBUSR       PIC X(64).
           01 DBPWD       PIC X(64).
           01 BLOB1       PIC X(64).
           
           01 LEN         PIC 9(8) COMP-3.
           01 OFFSET      PIC 9(8) COMP-3.
           01 REC1        PIC X(1000000).

           01 VAR1        PIC 9(3) VALUE 0.  
           01 VAR2        PIC 9(3) VALUE 0.  

           01 VARA        PIC X(64).  
           01 VARB        PIC X(64).  
           01 VAR3        PIC X(64).  
       
       
       PROCEDURE DIVISION. 
 
       000-CONNECT.
       
       EXEC SQL 
            INCLUDE SQLCA 
       END-EXEC. 

       EXEC SQL
          DECLARE CK08 CURSOR FOR
             SELECT VAR1 FROM TAB
             WHERE
                KVAR08 >= (
                    :VARA
                 || :VARB
                 || CASE WHEN :VAR2::numeric < 0 THEN
                       'A' || TO_CHAR(999999.9999999 +
                                      :VAR2::numeric,
                                      'FM000000D0000000')
                    ELSE
                       'B' || TO_CHAR(0 +
                                      :VAR2::numeric,
                                      'FM000000D0000000')
                    END
                 || CASE WHEN :VAR3::numeric < 0 THEN
                       'A' || TO_CHAR(999999.9999999 +
                                      :VAR3::numeric,
                                      'FM000000D0000000')
                    ELSE
                       'B' || TO_CHAR(0 +
                                      :VAR3::numeric,
                                      'FM000000D0000000')
                    END
                         )
             ORDER BY KVAR08 ASC
       END-EXEC.
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

         EXEC SQL AT :DBS
                SELECT VAR1::numeric 
                    INTO :VAR1
                    FROM TAB 
                        WHERE :VAR2::numeric = 10
         END-EXEC.

         EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.

       200-END.
