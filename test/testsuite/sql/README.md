
The tests in `/gixsql_test` come from the [GixSQL](https://github.com/mridoni/gixsql)'s test suite

The tests in `/sql_exec` are inspirend from [GixSQL](https://github.com/mridoni/gixsql)'s testsuite



## List of simple tests:
These test are only for testing the parser, they make no sense, dont try to compile them.
#### sql_test1
- connect to
- start transaction
- select

#### sql_test2
- include
- declare cursor
- insert into
- savepoint
- open cursor
- fetch cursor
- close
- disconnect

#### sql_test3
- connect
- create table
- declare cursor
- fetch data

#### sql_test4
- var smt is type(size)

#### sql_test5
- select
- commit

#### sql_test6
- ignore (not in gix)

#### sql_test7
- lob (not in gix)

#### sql_test8
- execute
- preprare

#### sql_test9
- big select

#### sql_test10
- big statement with no sense (todo)

#### sql_test11
- big declare (todo)

#### sql_test12
- different format for connect

#### sql_test13
- exeption 
- begin end

#### sql_test14
- whenever

#### sql_test15
- update
- declare table

