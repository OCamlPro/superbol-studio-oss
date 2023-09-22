                            IDENTIFICATION DIVISION.
                            PROGRAM-ID. MAC.
                            DATA DIVISION.
                            WORKING-STORAGE SECTION.
                            01 A.
                                      05 A1 PIC 99.
                                      05 A2 PIC 99.
                                      05 A3 PIC 99.
                           01 B.
                                      05 A1 PIC 9(4).
                                      05 FILLER PIC X VALUE '/'.
                                      05 A2 PIC 9(2).
                                      05 FILLER PIC X VALUE '/'.
                                      05 A3 PIC 9(2).
                           PROCEDURE DIVISION.
                           MAAC.
                                     ACCEPT A FROM DATE.
                                     MOVE CORR A TO B.
                                     IF NOT (A1 OF A < 10 )
                                               INSPECT A1 OF B REPLACING FIRST "00" BY "19"
                                     ELSE
                                               INSPECT A1 OF B REPLACING FIRST "00" BY "20".
                                     DISPLAY "CURRENT DATE IS (ISO) :" B.
                                     STOP RUN.
