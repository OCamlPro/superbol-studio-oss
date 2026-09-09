       program-id. ok.
       data division.
       working-storage section.
       01 X               PIC X(6) VALUE "ABCDEF".
       01 Y               PIC 999  VALUE 123.
       procedure division.
           display X (1:2)
           display X (3:1)
           display X (4:3)
           display X (4:4)
           display X (6:1)
           display X (6:2)
           display X (7:1)
           display Y (1:3)
           display Y (3:1)
           display Y (4:1)
           stop run.
