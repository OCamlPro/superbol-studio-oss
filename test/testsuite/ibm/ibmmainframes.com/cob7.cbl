       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTTAB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A VALUE ZEROES.
                 05 AA PIC 99 OCCURS 1 TO 99 DEPENDING N.
       01 N PIC 99.
       01 I PIC 99 VALUE 1.
       01 J PIC 99.
       01 K PIC 99.
       01 T PIC XX.
       PROCEDURE DIVISION.
       001.
               DISPLAY "ENTER NO OF ELEMENTS IN TABLE:".
               ACCEPT N.
               DISPLAY "ENTER ELEMENTS:".
               PERFORM 0002 N TIMES.
               PERFORM 0001 VARYING I FROM 1 BY 1 UNTIL I > N.
               MOVE 1 TO I.
               DISPLAY "THE SORTED TABLE IS:".
               PERFORM 0003 N TIMES.
               STOP RUN.
        0001.
               COMPUTE K = I + 1.
               PERFORM 00001 VARYING J FROM K BY 1 UNTIL J > N.
        00001.
               IF AA ( I ) > AA ( J )
               MOVE AA ( I ) TO T
               MOVE AA ( J ) TO AA ( I )
               MOVE T TO AA ( J ).
        0002.
               ACCEPT AA(I).
               ADD 1 TO I.
        0003.
               DISPLAY AA(I).
               ADD 1 TO I.
