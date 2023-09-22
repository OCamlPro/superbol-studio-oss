                           IDENTIFICATION DIVISION.
                           PROGRAM-ID. MACJUL.
                           DATA DIVISION.
                           WORKING-STORAGE SECTION.
                           01 A.
                                     05 A1 PIC 9999.
                                     05 A2 PIC 99.
                                     05 A3 PIC 99.
                           01 B.
                                     05 B1 PIC 9999.
                                     05 B2 PIC 999.
                           01 C PIC 9 VALUE 2.
                           PROCEDURE DIVISION.
                           0001.
                                    DISPLAY "ENTER GREGORIAN DATE (YYYYMMDD):".
                                    ACCEPT A.
                                    IF A NOT NUMERIC OR A2 > 12 OR A3 > 31
                                    DISPLAY "INVALID DATE" GO 0001.
                                    MOVE A1 TO B1.
                                    COMPUTE B2 = A3.
                                    IF A2 = 12 ADD 30 TO B2 SUBTRACT 1 FROM A2.
                                    IF A2 = 11 ADD 31 TO B2 SUBTRACT 1 FROM A2.
                                    IF A2 = 10 ADD 30 TO B2 SUBTRACT 1 FROM A2.
                                    IF A2 = 9 ADD 31 TO B2 SUBTRACT 1 FROM A2.
                                    IF A2 = 8 ADD 31 TO B2 SUBTRACT 1 FROM A2.
                                    IF A2 = 7 ADD 30 TO B2 SUBTRACT 1 FROM A2.
                                    IF A2 = 6 ADD 31 TO B2 SUBTRACT 1 FROM A2.
                                    IF A2 = 5 ADD 30 TO B2 SUBTRACT 1 FROM A2.
                                    IF A2 = 4 ADD 31 TO B2 SUBTRACT 1 FROM A2.
                                    IF A2 = 3 ADD 28 TO B2 SUBTRACT 1 FROM A2
                                    DIVIDE A1 BY 4 GIVING A1 REMAINDER C
                                    IF C = 0 ADD 1 TO B2.
                                    IF A2 = 2 ADD 31 TO B2.
                                    DISPLAY " ".
                                    DISPLAY "JULION DATE IS(YYYYDDD):".
                                    DISPLAY B1 ":" B2.
                                    STOP RUN.
