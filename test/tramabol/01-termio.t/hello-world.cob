       program-id. prog.
       data division.
       working-storage section.
       01 filler.
         02 who pic a(5) value "world".
       procedure division.
           display "hello " who "!"
           stop run.
