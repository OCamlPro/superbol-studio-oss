       program-id. init.
       data division.
       working-storage section.
       77 A PIC X VALUE "A".
       77 B PIC X OCCURS 2 VALUE "B".
       77 C PIC X OCCURS 31 VALUE "C".
       77 D PIC X(5) OCCURS 49 VALUE "ABCDE".
       procedure division.
           display A
           display B (2)
           display C (2), "*", C (15), "*", C (31)
           display D (1), "*", D (21), "*", D (40), "*", D (49)
           stop run.
