       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. TSQL042A. 
       
       
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
       
           01 DBNAME PIC X(64).
           01 DBAUTH PIC X(64).
           01 T1     PIC 9(3) VALUE 0.  
           01 DISP-RATE PIC 9(15). 
           01 DISP-COM PIC 9(3).  
           01 DISP-CODE PIC 9(8). 
           01 FAKE-CHAR PIC X.  
           01 ANSS PIC X. 
           01 COM-NULL-IND PIC S9(4) COMP. 
           
           01 VARC PIC X(20).
           01 VARD PIC X(20).
       
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
                    MISCDATA
                 FROM EMPTABLE
               ORDER BY LNAME
           END-EXEC.
           
       PROCEDURE DIVISION. 
 
       000-CONNECT.
         DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
         ACCEPT DBNAME FROM ENVIRONMENT-VALUE.
         DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
         ACCEPT DBAUTH FROM ENVIRONMENT-VALUE.
         
      *   DISPLAY '***************************************'.
      *   DISPLAY " DB  : " DBNAME.
      *   DISPLAY " USER: " DBAUTH.
      *   DISPLAY '***************************************'.

           EXEC SQL
              CONNECT TO :DBNAME USER :DBAUTH
           END-EXEC.      
           
           IF SQLCODE <> 0 THEN
              DISPLAY 'SQLCODE. ' SQLCODE
              DISPLAY 'SQLERRM. ' SQLERRM
              GO TO 100-EXIT
           END-IF.
       100-MAIN.

           EXEC SQL
              START TRANSACTION
	       END-EXEC.                                                    
       
      *  open cursor
           EXEC SQL
               OPEN EMPTBL
           END-EXEC 
           MOVE SQLCODE TO DISP-CODE
           DISPLAY 'open ' DISP-CODE.
           DISPLAY 'open ' SQLERRM.
       
      *  fetch a data item 
           EXEC SQL
               FETCH EMPTBL INTO 
                 :ENO,:LNAME,:FNAME,:STREET,:CITY, 
                 :ST,:ZIP,:DEPT,:PAYRATE, 
                 :COM,:MISCDATA
           END-EXEC. 
       
       100-test. 
           MOVE SQLCODE TO DISP-CODE
           DISPLAY 'fetch ' DISP-CODE
       
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
           
           IF COM-NULL-IND < 0 
               DISPLAY LNAME ': commission is null' 
           ELSE 
               DISPLAY LNAME ': commission ' DISP-COM 
           END-IF 

           EXEC SQL 
             FETCH EMPTBL INTO 
               :ENO,:LNAME,:FNAME,:STREET,:CITY, 
               :ST,:ZIP,:DEPT,:PAYRATE, 
               :COM:COM-NULL-IND,:MISCDATA
           END-EXEC 

           MOVE SQLCODE TO DISP-CODE 
           DISPLAY 'fetch ' DISP-CODE 
           DISPLAY 'fetch ' SQLCODE 
           END-PERFORM  
       
           DISPLAY 'All records in this table have been selected'. 
       
       CLOSE-LOOP.
      *  close the cursor 
           EXEC SQL 
               CLOSE EMPTBL 
           END-EXEC. 

      * this should insert a NULL
           MOVE -1 TO COM-NULL-IND.
           MOVE 1.3 TO COM.
           EXEC SQL
               INSERT INTO EMPTABLE 
                    (LNAME, FNAME, PAYRATE, COM)
                    VALUES 
                    ('XYZ1', 'ABC1', 94.00, :COM:COM-NULL-IND)
           END-EXEC.
           DISPLAY 'INSERT 1 - SQLCODE     : ' SQLCODE.
           DISPLAY 'INSERT 1 - SQLERRM     : ' SQLERRMC(1:SQLERRML).

      * this should insert the actual value (1.8)
           MOVE 0 TO COM-NULL-IND.
           MOVE 1.8 TO COM.
           EXEC SQL
               INSERT INTO EMPTABLE 
                    (LNAME, FNAME, PAYRATE, COM)
                    VALUES 
                    ('XYZ2', 'ABC2', 95.00, :COM:COM-NULL-IND)
           END-EXEC.
           DISPLAY 'INSERT 2 - SQLCODE     : ' SQLCODE.
           DISPLAY 'INSERT 2 - SQLERRM     : ' SQLERRMC(1:SQLERRML).

           MOVE 0 TO PAYRATE.
           MOVE 0 TO COM.

      * we read them back
           EXEC SQL
               SELECT PAYRATE, COM
                INTO :PAYRATE, :COM:COM-NULL-IND
                FROM EMPTABLE 
                    WHERE LNAME = 'XYZ1' AND 
                        FNAME = 'ABC1' AND 
                        COM IS NULL
           END-EXEC.
           DISPLAY 'READBACK 1 - SQLCODE     : ' SQLCODE.
           DISPLAY 'READBACK 1 - SQLERRM     : ' SQLERRMC(1:SQLERRML).
           DISPLAY 'READBACK 1 - PAYRATE     : ' PAYRATE.
           DISPLAY 'READBACK 1 - COM         : ' COM.
           DISPLAY 'READBACK 1 - COM-NULL-IND: ' COM-NULL-IND.

           EXEC SQL
               SELECT PAYRATE, COM
                INTO :PAYRATE, :COM:COM-NULL-IND
                FROM EMPTABLE 
                    WHERE LNAME = 'XYZ2' AND 
                        FNAME = 'ABC2' AND 
                            COM = 1.8
           END-EXEC.
           DISPLAY 'READBACK 2 - SQLCODE     : ' SQLCODE.
           DISPLAY 'READBACK 2 - SQLERRM     : ' SQLERRMC(1:SQLERRML).
           DISPLAY 'READBACK 2 - PAYRATE     : ' PAYRATE.
           DISPLAY 'READBACK 2 - COM         : ' COM.
           DISPLAY 'READBACK 2 - COM-NULL-IND: ' COM-NULL-IND.

           EXEC SQL COMMIT END-EXEC.

           EXEC SQL CONNECT RESET END-EXEC.
       
       100-EXIT. 
             STOP RUN.

