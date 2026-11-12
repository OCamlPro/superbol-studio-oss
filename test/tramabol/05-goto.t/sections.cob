       program-id. prog.
       procedure division.
       s1 section.
           display "s1".
       p.
           display "p of s1"
           go to s3.
       s2 section.
       p.
           display "p of s2"
           stop run.
       s3 section.
           display "s3"
           go to p of s2.
