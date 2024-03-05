       IDENTIFICATION DIVISION.
       PROGRAM-ID. MACESDS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT MAC ASSIGN TO AS-ESDSFILE.
       DATA DIVISION.
       FILE SECTION.
       FD MAC.
       01 MREC.
                  05 MNO PIC 9(5).
                  05 MNAME PIC X(10).
       WORKING-STORAGE SECTION.
       01 A PIC 99 VALUE ZERO.
       01 B PIC 9(5) VALUE ZERO.
       PROCEDURE DIVISION.
       0001.
           DISPLAY "ENTER 1.SEARCH/2.INSERT/3.REWRITE/4.DEL/5.DEL ALL 6.DISP".
                ACCEPT A.
                IF A = 1 GO 1SEARCH
                ELSE IF A = 2 GO 2WRITE
                ELSE IF A = 3 GO 3REWRITE
                ELSE IF A = 4 GO 4DELETE
                ELSE IF A = 5 GO 5DELALL
                ELSE IF A = 6 GO 6DISPLAY
                ELSE DISPLAY "INVALID INPUT"
                GO 0001.
                STOP RUN.
        1SEARCH.
                OPEN INPUT MAC.
           DISPLAY "ENTER RECORD NO TO BE SEARCHED".
                ACCEPT B.
        0002.
                READ MAC AT END DISPLAY B "NOT FOUND", GO 000X.
                IF B = MNO DISPLAY "FOUND " MNO ":" ,
                DISPLAY " AT POS:" A " FOR NAME: " MNAME,
                GO 000X.
                ADD 1 TO A.
                GO TO 0002.
       2WRITE.
                 OPEN EXTEND MAC.
                 ACCEPT MNO.
                 ACCEPT MNAME.
                 WRITE MREC.
                 GO 000X.
        3REWRITE.
                 OPEN I-O MAC.
            DISPLAY "ENTER RECORD NO TO BE REWRITED".
                 ACCEPT B.
         0003.
                 READ MAC AT END DISPLAY "MNO NOT FOUND" GO 000X.
                 IF MNO NOT = B GO 0003.
                 ACCEPT MNO.
                 ACCEPT MNAME.
                 REWRITE MREC.
                 GO 000X.
                 4DELETE.
                 OPEN I-O MAC.
            DISPLAY "ENTER RECORD NO TO BE DELETED".
                 ACCEPT B.
        0004.
                 READ MAC AT END DISPLAY "MNO NOT FOUND" GO 000X.
                 IF MNO NOT = B GO 0003.
                 MOVE SPACES TO MREC.
                 REWRITE MREC.
                 GO 000X.
         5DELALL.
            DISPLAY "SEQ FILE!SO ALL RECORDS ARE DELETED".
            DISPLAY "ARE YOU SURE(1/0)".
                 ACCEPT A.
                 IF A = 1 OPEN OUTPUT MAC DISPLAY "RECORDS DELETED" GO 000X
                 ELSE GO 000X.
        6DISPLAY.
                OPEN INPUT MAC.
        0005.
                READ MAC AT END GO 000X.
                DISPLAY MNO, " ", MNAME.
                GO 0005.
        000X.
                 CLOSE MAC.
            DISPLAY "CONTINUE?1/0".
                 ACCEPT A.
                 IF A = 0 STOP RUN ELSE GO 0001.
