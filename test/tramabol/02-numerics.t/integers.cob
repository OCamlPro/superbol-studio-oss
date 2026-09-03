       program-id. integers.
       data division.
       working-storage section.
       procedure division.
           display 0
           display 1
           display 2
           display -1
           display -2
           display  2147483647          *> Int32.max_int
           display -2147483648          *> Int32.min_int
           display  2147483648          *> succ Int32.max_int
           display -2147483649          *> pred Int32.min_int
           display  9223372036854775807 *> Int64.max_int
           display -9223372036854775808 *> Int64.min_int
           display  9223372036854775808 *> succ Int64.max_int
           display -9223372036854775809 *> pred Int64.min_int
           stop run.
