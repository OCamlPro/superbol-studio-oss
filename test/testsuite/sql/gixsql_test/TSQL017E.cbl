       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSQL017E.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------*
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
      *
       01 DBS                 PIC  X(010).
       01 TID                 PIC S9(018).
       01 MTIME               PIC  X(026).

       77 KEY-ID-1            PIC  9(008) VALUE 42.

       78 KEY-ID-2            VALUE 49.
      *
           EXEC SQL END DECLARE SECTION END-EXEC.
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       PROCEDURE DIVISION.
       TMAIN SECTION.
       P001-A.
      *
           EXEC SQL AT :DBS
              DECLARE CRSR017D CURSOR FOR
                 SELECT TABID, MTIME FROM TAB
                 WHERE
                    KEY01 >= (
                        MYFUNC(:KEY-ID-1 , :KEY-ID-2 , 099, '1')
                             )
                 ORDER BY KEY01 ASC
           END-EXEC.
      *
           IF SQLCODE NOT = 0
              GO TO P003-C.
      *
           EXEC SQL
              OPEN CRSR017D
           END-EXEC.
      *
           IF SQLCODE NOT = 0
              GO TO P003-C.
      *
       P002-B.
      *
           EXEC SQL
               FETCH CRSR017D
               INTO
                 :TID,
                 :MTIME
           END-EXEC.

           EXEC SQL
              CLOSE CRSR017D
            END-EXEC.


          EXEC SQL
            SELECT K1, K2 
                INTO :KEY-ID-1 , :KEY-ID-2
                FROM TAB
                WHERE KEY01 = 0
          END-EXEC.
            
      *
       P003-C.
           GOBACK.