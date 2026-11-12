       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9    VALUE 2.
       77 J             PIC 9    VALUE 0.
       procedure division.
           go to a b c depending on I
           display "KO".
       a.
           display "KO".
       b.
           display "b"
           go to a b c depending on J
           display "fell through".
       c.
           display "c".
