       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL004A. 
       
       
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

        EXEC SQL VAR
              VARD IS VARCHAR(120)
        END-EXEC.       
       
           01 DATASRC PIC X(64).
           01 DBUSR  PIC X(64).
           01 DBPWD  PIC X(64).

           01 T1            PIC 9(3) VALUE 0.  
           01 DISP-RATE     PIC 9(15). 
           01 DISP-COM      PIC 9(3).  
           01 DISP-CODE     PIC 9(8) COMP-3. 
           01 FAKE-CHAR     PIC X.  
           01 ANSS          PIC X. 
           01 COM-NULL-IND  PIC S9(4) COMP. 
           
           01 VARC SQL TYPE IS VARCHAR(20).
      
           01 VARD PIC X(32).
           01 VARE PIC X(32).

        EXEC SQL VAR
              VARE IS VARCHAR(120)
        END-EXEC.                 
                  
       
       EXEC SQL 
            INCLUDE SQLCA 
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
           
       PROCEDURE DIVISION. 
 
       000-CONNECT.

           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'SQLCODE. ' SQLCODE
              DISPLAY 'SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.
       100-MAIN.

      *     EXEC SQL
      *        START TRANSACTION
      *     END-EXEC.                

      *  open cursor
           EXEC SQL
               OPEN EMPTBL
           END-EXEC 

           MOVE SQLCODE TO DISP-CODE
           DISPLAY 'open code:' DISP-CODE.
           DISPLAY 'open msg :' SQLERRMC.
       
      *  fetch a data item 
           EXEC SQL
               FETCH EMPTBL INTO 
                 :ENO,:LNAME,:FNAME,:STREET,:CITY, 
                 :ST,:ZIP,:DEPT,:PAYRATE, 
                 :COM,:MISCDATA,:DNUM1,:DNUM2,:DNUM3
           END-EXEC. 
       
       100-test. 
           MOVE SQLCODE TO DISP-CODE
           DISPLAY 'fetch ' DISP-CODE
           DISPLAY 'fetch ' SQLCODE 
           DISPLAY 'fetch ' SQLERRMC(1:SQLERRML)
       
      *  loop until no more data
           PERFORM UNTIL SQLCODE < 0 OR SQLCODE = 100
       
      *  display the record
           MOVE PAYRATE TO DISP-RATE
           MOVE COM TO DISP-COM
           DISPLAY 'employee #: [' ENO ']'
           DISPLAY 'last name : [' LNAME ']'
           DISPLAY 'first name: [' FNAME ']'
           DISPLAY 'street    : [' STREET ']'
           DISPLAY 'city      : [' CITY ']'
           DISPLAY 'state     : [' ST ']'
           DISPLAY 'zip code  : [' ZIP ']'
           DISPLAY 'department: [' DEPT ']'
           DISPLAY 'payrate   : [' PAYRATE ']'
           DISPLAY 'commission: [' COM ']'
           DISPLAY 'misc      : [' MISCDATA-TEXT ']'
           DISPLAY 'misc (len): [' MISCDATA-LEN ']'
           DISPLAY 'dnum1     : [' DNUM1 ']'
           DISPLAY 'dnum2     : [' DNUM2 ']'
           DISPLAY 'dnum3     : [' DNUM3 ']'
           
           IF COM-NULL-IND < 0 
               DISPLAY 'commission is null' 
           ELSE 
               DISPLAY 'commission ' DISP-COM 
           END-IF 

           EXEC SQL 
             FETCH EMPTBL INTO 
               :ENO,:LNAME,:FNAME,:STREET,:CITY, 
               :ST,:ZIP,:DEPT,:PAYRATE, 
               :COM,:MISCDATA,:DNUM1,:DNUM2,:DNUM3
           END-EXEC 

           MOVE SQLCODE TO DISP-CODE 
           DISPLAY 'fetch ' DISP-CODE 
           DISPLAY 'fetch ' SQLCODE 
           DISPLAY 'fetch ' SQLERRMC 
           END-PERFORM  
       
           DISPLAY 'All records in this table have been selected'. 
       
       CLOSE-LOOP.
      *  close the cursor 
           EXEC SQL 
               CLOSE EMPTBL 
           END-EXEC. 
       
      * we try a single open + fetch + close to see if the cursor
      * is still available for opening after being closed

      *  open cursor
           EXEC SQL
               OPEN EMPTBL
           END-EXEC 

           DISPLAY 'reopen code:' SQLCODE.
           DISPLAY 'reopen msg :' SQLERRMC(1:SQLERRML)
       
      *  fetch a data item 
           EXEC SQL
               FETCH EMPTBL INTO 
                 :ENO,:LNAME,:FNAME,:STREET,:CITY, 
                 :ST,:ZIP,:DEPT,:PAYRATE, 
                 :COM,:MISCDATA,:DNUM1,:DNUM2,:DNUM3
           END-EXEC. 
           DISPLAY 'refetch ' SQLCODE 
           DISPLAY 'refetch ' SQLERRMC(1:SQLERRML)

           DISPLAY '(reopen) employee #: [' ENO ']'
           DISPLAY '(reopen) last name : [' LNAME ']'
           DISPLAY '(reopen) first name: [' FNAME ']'
           DISPLAY '(reopen) street    : [' STREET ']'
           DISPLAY '(reopen) city      : [' CITY ']'
           DISPLAY '(reopen) state     : [' ST ']'
           DISPLAY '(reopen) zip code  : [' ZIP ']'
           DISPLAY '(reopen) department: [' DEPT ']'
           DISPLAY '(reopen) payrate   : [' PAYRATE ']'
           DISPLAY '(reopen) commission: [' COM ']'
           DISPLAY '(reopen) misc      : [' MISCDATA-TEXT ']'
           DISPLAY '(reopen) misc (len): [' MISCDATA-LEN ']'
           DISPLAY '(reopen) dnum1     : [' DNUM1 ']'
           DISPLAY '(reopen) dnum2     : [' DNUM2 ']'
           DISPLAY '(reopen) dnum3     : [' DNUM3 ']'

      *  close the cursor 
           EXEC SQL 
               CLOSE EMPTBL 
           END-EXEC. 
           DISPLAY 'reclose ' SQLCODE 
           DISPLAY 'reclose ' SQLERRMC(1:SQLERRML)
       EXEC SQL CONNECT RESET END-EXEC.

       100-EXIT. 
             STOP RUN.