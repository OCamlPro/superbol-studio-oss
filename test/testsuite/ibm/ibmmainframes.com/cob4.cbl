                            IDENTIFICATION DIVISION.
                            PROGRAM-ID. DAYFIND.
                            DATA DIVISION.
                            WORKING-STORAGE SECTION.
                            01 Y PIC 9(4).
                            01 M PIC 9(2).
                            01 D PIC 9(2).
                            01 A PIC 99 VALUE ZERO.
                            01 B PIC 9 VALUE ZERO.
                            01 C PIC 99 VALUE ZERO.
                            01 F VALUE "12060708091011".
                                      05 FF PIC 99 OCCURS 7.
                            01 E PIC 9999 VALUE 0012.
                            01 T PIC 9999 VALUE ZERO.
                            01 I PIC 9 VALUE 1.

                            PROCEDURE DIVISION.
                            0001A.
                                     DISPLAY "ENTER 4 DIGIT YEAR >=0001 & <=9999".
                                     ACCEPT Y.
                            0002A.
                                     DISPLAY "ENTER MONTH(INTEGER)".
                                     ACCEPT M.
                                     IF M < 1 OR > 12 DISPLAY "INVALID MONTH" GO 0002A.
                            0003A.
                                     DISPLAY "ENTER DATE(INTEGER)".
                                     ACCEPT D.
                                     IF D < 1 OR > 31 DISPLAY "INVALID DATE" GO 0003A.
                                     MOVE D TO C.
                            0000X.
                                    COMPUTE A = FF ( I ).
                                    IF E = Y GO 0000Y.
                                    ADD 1 TO I.
                                    IF I > 7 COMPUTE I = 1.
                                    DIVIDE E BY 4 GIVING T REMAINDER B.
                                    IF E < Y AND B = 0 ADD 1 TO I.
                                    IF I > 7 COMPUTE I = 1.
                                    ADD 1 TO E.
                                    GO 0000X.
                              0000Y.
                                     IF B = 0 AND M > 2 ADD 1 TO A.
                                     IF M = 1
                                     ADD A TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 2
                                     ADD A 3 TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 3
                                     ADD A 3 TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 4
                                     SUBTRACT 1 FROM A
                                     ADD A TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 5
                                     ADD A 1 TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 6
                                     ADD A 4 TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 7
                                     SUBTRACT 1 FROM A
                                     ADD A TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 8
                                     ADD A 2 TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 9
                                     SUBTRACT 2 FROM A
                                     ADD A TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 10
                                     ADD A TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 11
                                     ADD A 3 TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE IF M = 12
                                     SUBTRACT 2 FROM A
                                     ADD A TO D
                                     DIVIDE D BY 7 GIVING A REMAINDER B
                                     ELSE DISPLAY "COBOL FAILED".
                                     DISPLAY Y "/" M "/" C " IS:".
                                     IF B = 0 DISPLAY "SUNDAY"
                                     ELSE IF B = 1 DISPLAY "MONDAY"
                                     ELSE IF B = 2 DISPLAY "TUESDAY"
                                     ELSE IF B = 3 DISPLAY "WEDNESDAY"
                                     ELSE IF B = 4 DISPLAY "THURSDAY"
                                     ELSE IF B = 5 DISPLAY "FRIDAY"
                                     ELSE IF B = 6 DISPLAY "SATURDAY"
                                     ELSE DISPLAY "COBOL RUNTIME ERROR".
                                     STOP RUN.
