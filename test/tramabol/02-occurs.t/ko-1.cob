       program-id. bounds-checks-1.
       data division.
       working-storage section.
       >>DEFINE CHECK AS PARAMETER
       01 X             PIC X(6) VALUE "ABCDEF".
       01 Y REDEFINES X PIC X    OCCURS 6.
       01 Z REDEFINES X PIC XX   OCCURS 3.
       procedure division.
       >>IF   CHECK = 0
           display Y (0)
       >>ELIF CHECK = 1
           display Y (7)
       >>ELIF CHECK = 2
           display Z (-1)
       >>ELIF CHECK = 3
           display Z (4)
       >>ELSE
           display Z ("a")
       >>END-IF
           stop run.
