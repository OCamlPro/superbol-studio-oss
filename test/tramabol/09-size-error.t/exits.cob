       program-id. prog.
       procedure division.
       main.
           exit program
           display "after exit program"
           perform p
           display "KO".
       p.
           display "p"
           goback
           display "KO p".
