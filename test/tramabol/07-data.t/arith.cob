       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9     VALUE 2.
       77 J             PIC 99    VALUE 95.
       77 S             PIC S99   VALUE 3.
       77 D             PIC 9V9   VALUE 1.6.
       77 R             PIC 9     VALUE 0.
       procedure division.
           add 3 to I
           display I
           add I to J
           display J
           add 1 to I J S
           display I " " J " " S
           subtract 10 from S
           display S
           subtract S from I
           display I
           add D to R
           display R
           add D to R rounded
           display R
           add I to I J
           display I " " J.
