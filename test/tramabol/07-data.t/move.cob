       program-id. prog.
       data division.
       working-storage section.
       77 A             PIC X(5)  VALUE "hello".
       77 B             PIC X(3)  VALUE SPACES.
       77 C             PIC X(8)  VALUE "abcdefgh".
       77 N             PIC 999   VALUE 42.
       77 M             PIC 9     VALUE 0.
       77 S             PIC S99   VALUE -7.
       01 T.
          05 E          PIC X     OCCURS 3.
       77 I             PIC 9     VALUE 2.
       procedure division.
           move A to B
           display B
           move A to C
           display C
           move "xy" to C(3:2) T
           display C " " T
           move N to M
           display M
           move 1234 to N
           display N
           move S to N
           display N
           move -12 to S
           display S
           move zeros to B
           display B
           move all "ab" to C
           display C
           move N to A
           display A
           move A(2:2) to E(I) E(1)
           display T
           move "z" to E(I).
           display T.
