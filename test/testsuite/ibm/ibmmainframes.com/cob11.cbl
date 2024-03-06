       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A.
                 05 AA PIC X(5) OCCURS 100 TIMES INDEXED BY I.
       01 B PIC X(5).
       01 C PIC 9.
       PROCEDURE DIVISION.
       0001.
                 SET I TO 0.
       00A.
                  DISPLAY "MENU".
                  DISPLAY "1.PUSH".
                  DISPLAY "2.POP".
                  DISPLAY "3.POP ALL"
                  DISPLAY "4.EXIT".
                  ACCEPT C.
                  IF C = 1 PERFORM 0002 DISPLAY "ITEM PUSHED"
                  ELSE IF C = 2 PERFORM 0003
                  ELSE IF C = 3 PERFORM 0003 UNTIL I = 0
                  ELSE STOP RUN.
                  GO 00A.
        0002.
                  SET I UP BY 1.
                  IF I > 100 DISPLAY "STACK FULL".
                  DISPLAY "ENTER VALUE ".
                  ACCEPT B.
                  MOVE B TO AA ( I ).
        0003.
                  MOVE AA ( I ) TO B.
                  DISPLAY "ITEM POPPED:" B.
                  SET I DOWN BY 1.
                  IF I = 0 OR I < 0 DISPLAY "EMPTY STACK".
