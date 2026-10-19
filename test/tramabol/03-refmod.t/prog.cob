       program-id. prog.
       data division.
       working-storage section.
       >>DEFINE ERROR AS PARAMETER
       01 X               PIC X(6) VALUE "ABCDEF".
       01 Y               PIC 999  VALUE 123.
       77 ZER             PIC 9    VALUE 0.
       77 ONE             PIC 9    VALUE 1.
       77 TWO             PIC 9    VALUE 2.
       procedure division.
           display X (1:2)
           display X (3:1)
           display X (4:3)
           display X (6:1)
           display X (6:ONE)
           display X (1:)
           display X (3:)
           display X (4:)
           display X (6:)
           display X (ONE:)
           display Y (1:3)
           display Y (2:2)
           display Y (3:1)
           display Y (1:)
           display Y (2:)
           display Y (3:)
       >>IF   ERROR = 0
           display X (4:4)
       >>ELIF ERROR = 1
           display X (6:2)
       >>ELIF ERROR = 2
           display X (6:TWO)
       >>ELIF ERROR = 3
           display X (-1:1)
       >>ELIF ERROR = 4
           display Y (4:1)
       >>ELIF ERROR = 5
           display Y (0:1)
       >>ELIF ERROR = 6
           display Y (ZER:1)
       >>END-IF
           stop run.
