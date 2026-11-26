       program-id. prog.
       procedure division.
       s1 section.
       p.
           display "p of s1"
           go to s2.
       q.
           display "q of s1"
           stop run.
       s2 section.
       q.
           display "q of s2"
           go to p.
       p.
           display "p of s2"
           go to q of s1.
