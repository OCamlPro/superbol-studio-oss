                            IDENTIFICATION DIVISION.
                            PROGRAM-ID. PRIME.
                            DATA DIVISION.
                            WORKING-STORAGE SECTION.
                            01 A .
                                     05 AA PIC 99 OCCURS 10 TIMES.
                            01 I PIC 99 VALUE 1.
                            01 N PIC 9(10) VALUE 1.
                            01 D PIC 9(10) VALUE 2.
                            01 V PIC 9(10) VALUE 1.
                            01 T1 PIC 99.
                            01 T2 PIC 99.
                            PROCEDURE DIVISION.
                            0001.
                                     DISPLAY "ENTER NO".
                                     ACCEPT N.
                                     DISPLAY "PRIME FACTORS ARE: ".
                                     COMPUTE V = N / 2.
                                     PERFORM 0002.
                                     COMPUTE I = 1.
                                     PERFORM 0003 UNTIL I > 10.
                                     STOP RUN.
                            0002.
                                     DIVIDE N BY D GIVING T1 REMAINDER T2.
                                     IF T2 = 0
                                     COMPUTE AA ( I ) = D
                                     ADD 1 TO I
                                     COMPUTE N = N / D
                                     ELSE ADD 1 TO D.
                                     IF D < N OR = N GO 0002.
                            0003.
                                     DISPLAY AA ( I ).
                                     ADD 1 TO I.
