  $ tramabol inline-loops.cob
  times
  times
  03 0
  until 01
  until 02
  test after 02
  varying 01
  varying 03
  varying 05
  I=07
  01 01
  01 02
  02 01
  02 02
  03 01
  01 01
  01 02
  02 01
  02 02
  02 02
  once
  Terminated with status: 0
  $ tramabol varying3.cob
  111
  112
  121
  122
  211
  212
  221
  222
  end 311
  111
  112
  121
  122
  211
  212
  221
  222
  end 222
  Terminated with status: 0
  $ tramabol outofline.cob
  p 00
  p 00
  p 03
  q 04
  p 06
  q 07
  q 10
  q 11
  end 11
  Terminated with status: 0
  $ tramabol exits.cob
  loop 1
  loop 3
  loop 4
  out 4
  p
  q
  after sentence
  q
  Terminated with status: 0
