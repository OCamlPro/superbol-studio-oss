       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9     VALUE 0.
       procedure division.
       main.
           perform forever
             add 1 to I
             if I = 2 then
               exit perform cycle
             end-if
             display "loop " I
             if I = 4 then
               exit perform
             end-if
           end-perform
           display "out " I
           perform p
           perform s1
           if I = 4 next sentence end-if display "KO".
           display "after sentence"
           perform q
           display "KO end".
       p.
           display "p"
           exit paragraph
           display "KO p".
       s1 section.
       q.
           display "q"
           if I = 4 then
             exit section
           end-if
           display "KO q".
       r.
           display "r".
