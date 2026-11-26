       program-id. prog.
       data division.
       working-storage section.
       77 Z             PIC 9    VALUE 1.
       77 S             PIC 9    VALUE 3.
       procedure division.
           display "A"
           if Z = 1 then
             display "B"
             if Z = 2 then
               display "KO1"
             else
               display "C"
             end-if
             display "D"
           else
             display "KO2"
           end-if
           display "E"
           if Z = 1 then
             stop S
           end-if
           display "KO3".
