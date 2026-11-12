       program-id. prog.
       data division.
       working-storage section.
       77 I             PIC 9     VALUE 0.
       77 J             PIC 9     VALUE 0.
       77 K             PIC 9     VALUE 0.
       procedure division.
           perform varying I from 1 until I = 3
                   after J from 1 until J = 3
                   after K from 1 until K = 3
             display I J K
           end-perform
           display "end " I J K
           perform with test after
                   varying I from 1 until I = 2
                   after J from 1 until J = 2
                   after K from 1 until K = 2
             display I J K
           end-perform
           display "end " I J K.
