       program-id. prog.
       procedure division.
       main section.
       m.
           perform s1
           display "after s1"
           perform p2
           display "after p2"
           perform p1 thru s1
           display "after p1 thru s1"
           stop run.
       s1 section.
       p1.
           display "p1".
       p2.
           display "p2".
