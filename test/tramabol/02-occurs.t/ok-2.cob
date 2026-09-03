       program-id. ok.
       data division.
       working-storage section.
       01 X               PIC X(6) VALUE "ABCDEF".
       01 Y-1 REDEFINES X OCCURS 2.
          02 Y-2          OCCURS 3 PIC X.
       01 Z-1 REDEFINES X OCCURS 3 PIC XX.
       01 T-1 REDEFINES X OCCURS 1.
          02 T-2          OCCURS 1.
              03 T-3      OCCURS 3 PIC XX.
       procedure division.
           display X
           display Y-1 (1)
           display Y-2 (1, 3)
           display Y-2 (2, 3)
           display Z-1 (1)
           display Z-1 (3)
           display T-3 (1, 1, 2)
           stop run.
