       program-id. prog.
       procedure division.
       main.
           perform a thru c
           display "main 2"
           perform b
           display "main 3"
           perform x
           display "KO".
       a.
           display "a"
           perform b thru c.
       b.
           display "b".
       c.
           display "c".
       x.
           display "x"
           go to y.
       y.
           display "y".
