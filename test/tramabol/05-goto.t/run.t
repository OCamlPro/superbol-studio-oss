  $ tramabol paragraphs.cob
  start
  p1
  p3
  p4
  p6
  p5
  Terminated with status: 0
  $ tramabol sections.cob
  s1
  p of s1
  s3
  p of s2
  Terminated with status: 0
  $ tramabol local.cob
  p of s1
  q of s2
  p of s2
  q of s1
  Terminated with status: 0
  $ tramabol depending.cob
  b
  fell through
  c
  Terminated with status: 0
  $ tramabol undefined.cob
  undefined.cob:3.17-3.24:
     1          program-id. prog.
     2          procedure division.
     3 >            go to nowhere.
  ----                    ^^^^^^^
  >> Error: Unknown procedure-name 'nowhere'
  
  [1]
