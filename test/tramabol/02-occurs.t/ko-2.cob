       program-id. bounds-checks-2.
       data division.
       working-storage section.
       >>DEFINE CHECK AS PARAMETER
       01 X               PIC X(6) VALUE "ABCDEF".
       01 Y-1 REDEFINES X OCCURS 2.
          02 Y-2          OCCURS 3 PIC X.
       01 Z-1 REDEFINES X OCCURS 3 PIC XX.
       01 T-1 REDEFINES X OCCURS 1.
          02 T-2          OCCURS 1.
              03 T-3      OCCURS 3 PIC XX.
       procedure division.
       >>IF   CHECK = 0
           display Y-2 (0, 3)
       >>ELIF CHECK = 1
           display Y-2 (3, 3)
       >>ELIF CHECK = 2
           display Y-2 (1, 4)
       >>ELIF CHECK = 3
           display Y-2 (1, -1)
       >>ELIF CHECK = 4
           display Y-2 (1)
       >>ELIF CHECK = 5
           display T-3 (1, 2)
       >>ELIF CHECK = 6
           display T-3 (1, 2, 3, 4)
       >>ELIF CHECK = 7
           display T-3 (1, 2, 0.5, 4)
       >>ELIF CHECK = 8
           display T-3 (1, 2, 0.5)
       >>ELSE
           display T-3 (1, 1, 4)
       >>END-IF
           stop run.
