       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLEN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 N PIC 99 VALUE 0.
        01 E.
                  05 EE PIC 99 OCCURS 5 TO 10 DEPENDING F INDEXED I.
        01 F PIC 9 VALUE 6.
        PROCEDURE DIVISION.
        0001-MC.
                  COMPUTE F = F - 2.
                  SET I TO 1.
                  SEARCH EE VARYING N
                  WHEN 1 = 2 NEXT SENTENCE.
                  DISPLAY "LENGTH OF TABLE E IS:" N.
                  STOP RUN.
