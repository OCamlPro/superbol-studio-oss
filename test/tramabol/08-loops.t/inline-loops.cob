       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 99    VALUE 0.
       77 J             PIC 99    VALUE 0.
       77 N             PIC 9     VALUE 3.
       procedure division.
           perform 2 times
             display "times"
           end-perform
           perform N times
             add 1 to I
             subtract 1 from N
           end-perform
           display I " " N
           move 0 to I
           perform until I = 2
             add 1 to I
             display "until " I
           end-perform
           perform with test after until I = 2
             display "test after " I
           end-perform
           perform varying I from 1 by 2 until I = 7
             display "varying " I
           end-perform
           display "I=" I
           perform varying I from 1 until I = 3
                   after J from 1 until J = 3
             display I " " J
           end-perform
           display I " " J
           perform with test after varying I from 1 until I = 2
                   after J from 1 until J = 2
             display I " " J
           end-perform
           display I " " J
           perform 0 times
             display "KO"
           end-perform
           perform
             display "once"
           end-perform.
