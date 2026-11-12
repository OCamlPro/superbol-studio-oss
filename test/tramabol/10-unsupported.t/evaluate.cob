       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9     VALUE 1.
       procedure division.
           evaluate I
             when 1 display "one"
             when other display "other"
           end-evaluate.
