                           IDENTIFICATION DIVISION.
                           PROGRAM-ID. MAA.
                           DATA DIVISION.
                           WORKING-STORAGE SECTION.
                           01 YY PIC 99.
                           01 YYYY.
                                      05 Y1 PIC 99 VALUE 19.
                                      05 Y2 PIC 99 VALUE 00.
                           PROCEDURE DIVISION.
                           0001.
                                    ACCEPT YY.
                                    MOVE YY TO Y2.
                                    IF YY < 03
                                         MOVE 20 TO Y1.
                                    DISPLAY YYYY.
                                    STOP RUN.
