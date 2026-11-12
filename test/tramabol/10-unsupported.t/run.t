  $ tramabol evaluate.cob
  evaluate.cob:6.11-9.23:
     3          working-storage section.
     4          77 I             PIC 9     VALUE 1.
     5          procedure division.
     6 >            evaluate I
  ----              ^^^^^^^^^^
     7 >              when 1 display "one"
  ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     8 >              when other display "other"
  ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     9 >            end-evaluate.
  ----  ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: unsupported statement
  [1]
  $ tramabol nested.cob
  nested.cob:9.15-11.27:
     6              if I = 1 then
     7                perform
     8                  display "KO"
     9 >                evaluate I
  ----                  ^^^^^^^^^^
    10 >                  when 1 display "KO"
  ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    11 >                end-evaluate
  ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    12                end-perform
    13              end-if.
  Error: unsupported statement
  [1]
