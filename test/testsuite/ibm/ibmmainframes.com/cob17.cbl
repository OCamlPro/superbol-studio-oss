                             IDENTIFICATION DIVISION.
                             PROGRAM-ID. MACESDS.
                             ENVIRONMENT DIVISION.
                             INPUT-OUTPUT SECTION.
                             FILE-CONTROL.
                             SELECT MACC ASSIGN TO KSDSFILE
                                        ORGANIZATION INDEXED
                                        ACCESS MODE DYNAMIC
                                        RECORD KEY MNO
                                        FILE STATUS FS.
                             DATA DIVISION.
                             FILE SECTION.
                             FD MACC.
                             01 MREC.
                                      05 MNO PIC 9(5).
                                      05 MNAME PIC X(10).
                             WORKING-STORAGE SECTION.
                             01 FS PIC X(2).
                             01 A PIC 99 VALUE 00.
                             01 B PIC 9(5) VALUE ZERO.
                             01 IREC.
                                     05 INO PIC 9(5).
                                     05 INAME PIC X(10).
                             PROCEDURE DIVISION.
                             0001.
                                DISPLAY "ENTER 1.SEAR/2.WRITE/3.REWR/4.DEL/5.DELALL/6.DISP".
                                     ACCEPT A.
                                     IF A = 1 GO 1SEARCH
                                     ELSE IF A = 2 GO 2WRITE
                                     ELSE IF A = 3 GO 3REWRITE
                                     ELSE IF A = 4 GO 4DELETE
                                     ELSE IF A = 5 GO 5DELALL
                                     ELSE IF A = 6 GO 6DISPLAY
                                     ELSE DISPLAY "INVALID INPUT" GO 0001.
                                     STOP RUN.
                             1SEARCH.
                                     OPEN INPUT MACC.
                                     ACCEPT B.
                             0002.
                                      READ MACC NEXT AT END DISPLAY B "NOT FOUND", GO 000X.
                                      IF B = MNO DISPLAY "FOUND " MNO ":" ,
                                      DISPLAY " AT POS:" A " FOR NAME: " MNAME,
                                      GO 000X.
                                      ADD 1 TO A.
                                      GO TO 0002.
                             2WRITE.
                                      OPEN I-O MACC.
                                      IF FS = 95 DISPLAY "TYPE ISAM BEFORE RUN THIS PROGRAM"
                                      ELSE IF FS NOT = 00 OPEN OUTPUT MACC.
                                      ACCEPT MNO.
                                      ACCEPT MNAME.
                                      WRITE MREC INVALID KEY DISPLAY "DUPLICATE KEY!".
                                      GO 000X.
                             3REWRITE.
                                      OPEN I-O MACC.
                                      ACCEPT MNO.
                                      ACCEPT MNAME.
                                      REWRITE MREC.
                                      GO 000X.
                             4DELETE.
                                      OPEN I-O MACC.
                                      ACCEPT MNO.
                                      DELETE MACC INVALID KEY DISPLAY "NOT FOUND".
                                      GO 000X.
                             5DELALL.
                                      OPEN I-O MACC.
                             0003.
                                      READ MACC NEXT AT END DISPLAY "RECORDS DELETED" GO 000X.
                                      DELETE MACC INVALID KEY GO 000X.
                                      GO 0003.
                             6DISPLAY.
                                      OPEN INPUT MACC.
                             0005.
                                      READ MACC NEXT INTO IREC AT END GO 000X.
                                      DISPLAY INO, " ", INAME.
                                      GO 0005.
                             000X.
                                      CLOSE MACC.
                                      DISPLAY "CONTINUE?1/0".
                                      ACCEPT A.
                                      IF A = 0 STOP RUN ELSE GO 0001.
