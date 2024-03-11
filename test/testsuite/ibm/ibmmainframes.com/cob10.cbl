       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A.
                 05 AA PIC X(5) OCCURS 100 TIMES INDEXED BY I D.
       01 B PIC X(5).
       01 C PIC 9.
       PROCEDURE DIVISION.
       0001.
                SET I TO 0.
                SET D TO 1.
       00A.
                DISPLAY "MENU".
                DISPLAY "1.INSERT".
                DISPLAY "2.RETRIVE".
                DISPLAY "3.EXIT".
                ACCEPT C.
                IF C = 1 PERFORM 0002 DISPLAY "ITEM INSERTED"
                ELSE IF C = 2 GO 0003
                ELSE STOP RUN.
                GO 00A.
       0002.
                IF I > 100 SET I TO 1 DISPLAY "QUEUE CIRCULARED".
                DISPLAY "ENTER VALUE ".
                ACCEPT B.
                SET I UP BY 1.
                MOVE B TO AA ( I ).
        0003.
                IF D > I DISPLAY "QUEUE EMPTY" GO 00A.
                MOVE AA ( D ) TO B.
                DISPLAY "ITEM RECEIVED:" B.
                SET D UP BY 1.
                IF D > 100 SET D TO 1.
                GO 00A.
