  $ tramabol simple.cob
  main
  a
  c
  b
  c
  d
  e
  e
  back
  finish
  Terminated with status: 0
  $ tramabol overlap.cob
  a
  b
  c
  b
  c
  main 2
  b
  main 3
  x
  y
  Terminated with status: 0
  $ tramabol sections.cob
  p1
  p2
  after s1
  p2
  after p2
  p1
  p2
  after p1 thru s1
  Terminated with status: 0
  $ tramabol within.cob
  x
  z
  end
  Terminated with status: 0
  $ tramabol overflow.cob
  overflow.cob:6.11-6.20:
     3          main.
     4              perform p.
     5          p.
     6 >            perform p.
  ----              ^^^^^^^^^
  Error: Stack overflow, possible PERFORM depth exceeded (stack size is 255)
  [1]
  $ tramabol --max-steps 5 budget.cob
  p
  p
  p
  Error: Step budget exhausted (5 steps executed)
  [1]
