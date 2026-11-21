       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9     VALUE 8.
       77 J             PIC 99    VALUE 95.
       77 S             PIC S9    VALUE -8.
       77 N             PIC 9     VALUE 0.
       procedure division.
           add 3 to I J
             on size error display "size error " I " " J
             not on size error display "KO"
           end-add
           add 1 to I
             not on size error display "ok " I
           end-add
           add 1 to I
             on size error display "size error " I
           end-add
           display "I=" I
           subtract 3 from S
             on size error
               display "size error " S
               if S = -8 then
                 subtract 1 from S
                   on size error display "KO"
                   not on size error display "nested ok " S
                 end-subtract
               end-if
           end-subtract
           add 9 to I
           display "truncated " I
           perform until N = 1
             add 1 to I
               on size error
                 display "overflow at " I
                 move 1 to N
             end-add
           end-perform
           display "end " I.
