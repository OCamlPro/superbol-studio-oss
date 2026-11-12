       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9     VALUE 1.
       procedure division.
           if I = 1 then
             perform
               display "KO"
               evaluate I
                 when 1 display "KO"
               end-evaluate
             end-perform
           end-if.
