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
             CONNECT :DBUSR-1
                     IDENTIFIED BY :DBPWD-1
                     AT            :DBID-1
                     USING         :DATASRC-1
           END-EXEC.

           EXEC SQL AT CONN1 CREATE TABLE TAB1 (FLD1 INT) END-EXEC.
           DISPLAY 'CREATE(1): ' SQLCODE

           EXEC SQL AT CONN2 CREATE TABLE TAB2 (FLD2 INT) END-EXEC.
           DISPLAY 'CREATE(2): ' SQLCODE
           
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

                     END-EXEC. 
      *  declare cursor for select 
           EXEC SQL
               DECLARE EMPTBL CURSOR FOR
               SELECT                     
                    ENO,
                    LNAME,
                    FNAME,
                    STREET,
                    CITY,
                    ST,
                    ZIP,
                    DEPT,
                    PAYRATE,
                    COM,
                    MISCDATA,
                    DNUM1,
                    DNUM2,
                    DNUM3
                 FROM EMPTABLE
               ORDER BY LNAME
           END-EXEC.

           EXEC SQL
               DECLARE EMPTBL CURSOR FOR
               SELECT                     
                    ENO,
                    LNAME,
                    FNAME,
                    STREET,
                    CITY,
                    ST,
                    ZIP,
                    DEPT,
                    PAYRATE,
                    COM,
                    MISCDATA,
                    DNUM1,
                    DNUM2,
                    DNUM3
                 FROM EMPTABLE
               WHERE ENO BETWEEN :ENO-START AND :ENO-END
           END-EXEC.
           
           EXEC SQL
               SELECT                     
                        ENO,
                        LNAME,
                        FNAME,
                        STREET,
                        CITY,
                        ST,
                        ZIP,
                        DEPT,
                        PAYRATE,
                        COM,
                        MISCDATA,
                        DNUM1,
                        DNUM2,
                        DNUM3
               INTO :ENO,:LNAME,:FNAME,:STREET,:CITY, 
                 :ST,:ZIP,:DEPT,:PAYRATE, 
                 :COM,:MISCDATA,:DNUM1,:DNUM2,:DNUM3
               FROM EMPTABLE
                  WHERE ENO BETWEEN :ENO-START AND :ENO-START
           END-EXEC.

           EXEC SQL 
             FETCH EMPTBL INTO 
               :ENO,:LNAME,:FNAME,:STREET,:CITY, 
               :ST,:ZIP,:DEPT,:PAYRATE, 
               :COM,:MISCDATA,:DNUM1,:DNUM2,:DNUM3
           END-EXEC 

       EXEC SQL AT :DBS
              DECLARE CSKEY01N CURSOR FOR
                 SELECT COL1, COL2 FROM TAB_A
                 WHERE KEY01 >= :TABKEY
                 ORDER BY KEY01 ASC
       END-EXEC.

       
       EXEC SQL AT :DBS
              DECLARE CSKEY01N CURSOR FOR
                 SELECT COL1, COL2 FROM TAB_A
                 WHERE KEY01 >= :TABKEY
                 ORDER BY KEY01 ASC
       END-EXEC.
              
      *  fetch a data item 
           EXEC SQL
               FETCH EMPTBL INTO 
                 :ENO,:LNAME,:FNAME,:STREET,:CITY, 
                 :ST,:ZIP,:DEPT,:PAYRATE, 
                 :COM,:MISCDATA,:DNUM1,:DNUM2,:DNUM3
           END-EXEC. 
                  
           DISPLAY 'CONNECT SQLCODE: ' SQLCODE

           IF SQLCODE <> 0 THEN
              GO TO 100-EXIT
           END-IF.

       100-MAIN.

           EXEC SQL VAR
              VARD IS VARCHAR(120)
           END-EXEC.         

           EXEC SQL
              START TRANSACTION
           END-EXEC.                                                    

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