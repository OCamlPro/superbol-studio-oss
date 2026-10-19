       program-id. prog.
       data division.
       working-storage section.
       77 Z             PIC 9    VALUE 0.
       procedure division.
           if Z = 1 then
	     display "KO"
	   else
	     display "OK"
	   end-if
           stop run.
