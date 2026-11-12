       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 99    VALUE 0.
       procedure division.
       main.
           perform p 2 times
           perform p thru q varying I from 3 by 2 until I = 9
           perform q with test after until I = 11
           display "end " I
           stop run.
       p.
           display "p " I.
       q.
           add 1 to I
           display "q " I.
