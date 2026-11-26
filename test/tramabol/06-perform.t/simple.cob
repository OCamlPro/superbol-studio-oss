       program-id. prog.
       procedure division.
       main.
           display "main"
           perform a
           perform b thru c
           perform s2
           perform e
           display "back"
           go to finish.
       a.
           display "a"
           perform c.
       b.
           display "b".
       c.
           display "c".
       finish.
           display "finish"
           stop run.
       s2 section.
       d.
           display "d".
       e.
           display "e".
