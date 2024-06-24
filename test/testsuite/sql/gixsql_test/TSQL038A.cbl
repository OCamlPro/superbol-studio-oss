        identification division.
        program-id. TSQL038A.
        data division.
        working-storage section.
        01 block1.
                03 mypic1 pic x(10).
        01.
                03 mypic2 pic x(10).

        procedure division.
        a-main section.
                display 'hello world'.
                stop run.