  $ tramabol ok-1.cob --show
  ok-1.cob:7.19-7.26:
     4          01 X               PIC X(6) VALUE "ABCDEF".
     5          01 Y               PIC 999  VALUE 123.
     6          procedure division.
     7 >            display X (1:2)
  ----                      ^^^^^^^
     8              display X (3:1)
     9              display X (4:3)
  Error: unsupported term X (1:2)
  ok-1.cob:8.19-8.26:
     5          01 Y               PIC 999  VALUE 123.
     6          procedure division.
     7              display X (1:2)
     8 >            display X (3:1)
  ----                      ^^^^^^^
     9              display X (4:3)
    10              display X (4:4)
  Error: unsupported term X (3:1)
  ok-1.cob:9.19-9.26:
     6          procedure division.
     7              display X (1:2)
     8              display X (3:1)
     9 >            display X (4:3)
  ----                      ^^^^^^^
    10              display X (4:4)
    11              display X (6:1)
  Error: unsupported term X (4:3)
  ok-1.cob:10.19-10.26:
     7              display X (1:2)
     8              display X (3:1)
     9              display X (4:3)
    10 >            display X (4:4)
  ----                      ^^^^^^^
    11              display X (6:1)
    12              display X (6:2)
  Error: unsupported term X (4:4)
  ok-1.cob:11.19-11.26:
     8              display X (3:1)
     9              display X (4:3)
    10              display X (4:4)
    11 >            display X (6:1)
  ----                      ^^^^^^^
    12              display X (6:2)
    13              display X (7:1)
  Error: unsupported term X (6:1)
  ok-1.cob:12.19-12.26:
     9              display X (4:3)
    10              display X (4:4)
    11              display X (6:1)
    12 >            display X (6:2)
  ----                      ^^^^^^^
    13              display X (7:1)
    14              display Y (1:3)
  Error: unsupported term X (6:2)
  ok-1.cob:13.19-13.26:
    10              display X (4:4)
    11              display X (6:1)
    12              display X (6:2)
    13 >            display X (7:1)
  ----                      ^^^^^^^
    14              display Y (1:3)
    15              display Y (3:1)
  Error: unsupported term X (7:1)
  ok-1.cob:14.19-14.26:
    11              display X (6:1)
    12              display X (6:2)
    13              display X (7:1)
    14 >            display Y (1:3)
  ----                      ^^^^^^^
    15              display Y (3:1)
    16              display Y (4:1)
  Error: unsupported term Y (1:3)
  ok-1.cob:15.19-15.26:
    12              display X (6:2)
    13              display X (7:1)
    14              display Y (1:3)
    15 >            display Y (3:1)
  ----                      ^^^^^^^
    16              display Y (4:1)
    17              stop run.
  Error: unsupported term Y (3:1)
  ok-1.cob:16.19-16.26:
    13              display X (7:1)
    14              display Y (1:3)
    15              display Y (3:1)
    16 >            display Y (4:1)
  ----                      ^^^^^^^
    17              stop run.
  Error: unsupported term Y (4:1)
  unit: {
    name: ok
    records: {
      record: X
      storage: WORKING-STORAGE
      item: {
        qualname: X
        offset: 0
        size: 48
        layout: {
          elementary
          usage: {
            display
            category: ALPHANUMERIC(6)
          }
          value: "ABCDEF"
        }
      }
    }{
      record: Y
      storage: WORKING-STORAGE
      item: {
        qualname: Y
        offset: 0
        size: 24
        layout: {
          elementary
          usage: {
            display
            category: NUMERIC(digits = 3, scale = 0, sign = unsigned)
          }
          value: 123
        }
      }
    }
  }
  [1]

  $ tramabol ok-1.cob
  ok-1.cob:7.19-7.26:
     4          01 X               PIC X(6) VALUE "ABCDEF".
     5          01 Y               PIC 999  VALUE 123.
     6          procedure division.
     7 >            display X (1:2)
  ----                      ^^^^^^^
     8              display X (3:1)
     9              display X (4:3)
  Error: unsupported term X (1:2)
  ok-1.cob:8.19-8.26:
     5          01 Y               PIC 999  VALUE 123.
     6          procedure division.
     7              display X (1:2)
     8 >            display X (3:1)
  ----                      ^^^^^^^
     9              display X (4:3)
    10              display X (4:4)
  Error: unsupported term X (3:1)
  ok-1.cob:9.19-9.26:
     6          procedure division.
     7              display X (1:2)
     8              display X (3:1)
     9 >            display X (4:3)
  ----                      ^^^^^^^
    10              display X (4:4)
    11              display X (6:1)
  Error: unsupported term X (4:3)
  ok-1.cob:10.19-10.26:
     7              display X (1:2)
     8              display X (3:1)
     9              display X (4:3)
    10 >            display X (4:4)
  ----                      ^^^^^^^
    11              display X (6:1)
    12              display X (6:2)
  Error: unsupported term X (4:4)
  ok-1.cob:11.19-11.26:
     8              display X (3:1)
     9              display X (4:3)
    10              display X (4:4)
    11 >            display X (6:1)
  ----                      ^^^^^^^
    12              display X (6:2)
    13              display X (7:1)
  Error: unsupported term X (6:1)
  ok-1.cob:12.19-12.26:
     9              display X (4:3)
    10              display X (4:4)
    11              display X (6:1)
    12 >            display X (6:2)
  ----                      ^^^^^^^
    13              display X (7:1)
    14              display Y (1:3)
  Error: unsupported term X (6:2)
  ok-1.cob:13.19-13.26:
    10              display X (4:4)
    11              display X (6:1)
    12              display X (6:2)
    13 >            display X (7:1)
  ----                      ^^^^^^^
    14              display Y (1:3)
    15              display Y (3:1)
  Error: unsupported term X (7:1)
  ok-1.cob:14.19-14.26:
    11              display X (6:1)
    12              display X (6:2)
    13              display X (7:1)
    14 >            display Y (1:3)
  ----                      ^^^^^^^
    15              display Y (3:1)
    16              display Y (4:1)
  Error: unsupported term Y (1:3)
  ok-1.cob:15.19-15.26:
    12              display X (6:2)
    13              display X (7:1)
    14              display Y (1:3)
    15 >            display Y (3:1)
  ----                      ^^^^^^^
    16              display Y (4:1)
    17              stop run.
  Error: unsupported term Y (3:1)
  ok-1.cob:16.19-16.26:
    13              display X (7:1)
    14              display Y (1:3)
    15              display Y (3:1)
    16 >            display Y (4:1)
  ----                      ^^^^^^^
    17              stop run.
  Error: unsupported term Y (4:1)
  [1]
