       program-id. prog.
       procedure division.
           display "start".
       p1.
           display "p1"
           go to p3.
       p2.
           display "KO".
       p3.
           display "p3".
       p4.
           display "p4"
           go to p6
           display "KO".
       p5.
           display "p5"
           stop run.
       p6.
           display "p6"
           go to p5.
