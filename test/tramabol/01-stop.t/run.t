  $ tramabol stop.cob
  Terminated with status: 42
  [42]

  $ tramabol invalid.cob
  invalid.cob:6.11-6.17:
     3          working-storage section.
     4          77 s pic AA value "AB".
     5          procedure division.
     6 >            stop s.
  ----              ^^^^^^
  Error: Invalid data-type encountered (integer expected)
  [1]
