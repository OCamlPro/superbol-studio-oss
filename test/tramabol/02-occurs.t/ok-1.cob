       program-id. ok.
       data division.
       working-storage section.
       01 X             PIC X(6) VALUE "ABCDEF".
       01 Y REDEFINES X	PIC X    OCCURS 6.
       01 Z REDEFINES X	PIC XX   OCCURS 3.
       procedure division.
           display X
           display Y (1) "*" Y (6)
           display Z (1) "*" Z (2) "*" Z (3)
           stop run.
