                            IDENTIFICATION DIVISION.
                            PROGRAM-ID. MAC.
                            DATA DIVISION.
                            WORKING-STORAGE SECTION.
                            01 X PIC 9(4) VALUE 8048.
                            01 Y PIC 9(4) VALUE 21.
                            01 Z PIC 9(4) VALUE 31.
                            01 I PIC 9(5) VALUE 0.
                            01 A PIC 9(4).
                            01 B PIC 9(4).
                            01 N PIC 9(2) VALUE 1.
                            PROCEDURE DIVISION.
                            0001.
                                    DISPLAY "ENTER LIMIT:".
                                    ACCEPT N.    
                                    DISPLAY "RANDOM SERIES:".
                                    PERFORM 0002 N TIMES.
                                    STOP RUN.
                            0002.
                                    COMPUTE A = Y * I + Z.
                                    DIVIDE X INTO A GIVING B REMAINDER I.
                                    DISPLAY I.
                                    ADD 1 TO I.
