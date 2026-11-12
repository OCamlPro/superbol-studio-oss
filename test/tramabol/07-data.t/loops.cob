       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 99    VALUE 0.
       77 N             PIC 99    VALUE 0.
       procedure division.
       main.
           add 1 to I
           display I
           if I = 3 then
             go to next-step
           end-if
           go to main.
       next-step.
           move 0 to I.
       again.
           perform body
           if N = 4 then
             display "done " I " " N
             stop run
           end-if
           go to again.
       body.
           add 1 to N
           add N to I.
