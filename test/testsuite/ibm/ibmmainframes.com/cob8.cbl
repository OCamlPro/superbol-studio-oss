       IDENTIFICATION DIVISION.
       PROGRAM-ID. MACSR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 A VALUE "123546798012345".
                  05 A1 OCCURS 5 INDEXED BY I.
                                     10 A2 PIC 9 OCCURS 3 INDEXED BY J.
        01 K PIC 99 VALUE 1.
        01 X PIC 9.
        PROCEDURE DIVISION.
        0001.
                 SET I TO 0.
                 ACCEPT X.
        001.
                 SET I UP BY 1.
                 SET J TO 1.
        002.
                 SEARCH A2 VARYING K
                 WHEN A2 ( I , J ) = X DISPLAY "FOUND AT:" K
                 SET J UP BY 1 ADD 1 TO K GO 002.
                 IF I < 5 GO 001.
                 STOP RUN.
