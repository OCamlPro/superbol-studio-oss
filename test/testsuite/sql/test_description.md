In /gixsql_tests, there are real test, who don't work becase there are thing that are not implemented in superbol.
I created the test in sql_exec uniquely for testing sql parsing, _these are not real programs_!.

## List of things that are missing in superbol:

### Probleme description: 
 Whith the superbol colorisation, some "." after SQL Statements are white and not blue. Is this a probleme with parising?
- Exemple of file where the problem is occuring: sql_exec/TSQL001A.cbl

### Probleme description: 
 Sql is not correclty pretty printed when in DATA DIVISION, it can't be reparsed
- Exemple of file where the problem is occuring: sql_exec/TSQL001B.cbl

### Probleme description: 
 Sql Statement after PERFORM are not reconised as Expression 
- Exemple of file where the problem is occuring: sql_exec/TSQL003B.cbl

### Probleme description: 
 PROBLEM WITH - (Line break in COBOL)
- Exemple of file where the problem is occuring: sql_exec/TSQL006A.cbl

### Probleme description: 
 ??????? (everything)
- Exemple of file where the problem is occuring: sql_exec/TSQL007A.cbl


### Probleme description: 
 a display statement disappear between parse and reparse
- Exemple of file where the problem is occuring: sql_exec/TSQL009A.cbl

### Probleme description: 
 Invalid syntaxe for superbol
- Exemple of file where the problem is occuring: sql_exec/TSQL0015A.cbl

### Probleme description: 
" MOVE 'INSERT INTO TAB_A (KEY01, COL1, COL2)
    -          ' VALUES ($1, $2, $3)' TO STMT-1 "
    is not accepted by superbol
- Exemple of file where the problem is occuring: sql_exec/TSQL0017A.cbl

### Probleme description: 
 Invalid syntax by Superbol. I don't know why
- Exemple of file where the problem is occuring: sql_exec/TSQL017C.cbl


### Probleme description: 
 Internal warning: unable to join locations via limits
- Exemple of file where the problem is occuring: sql_exec/TSQL018A.cbl


### Probleme description: 
 Missing <identifiers> 
- Exemple of file where the problem is occuring: sql_exec/TSQL021A.cbl



## List of simple tests:
#### sql_test1
connect to
start transaction
select

#### sql_test2
include
declare cursor
insert into
savepoint
open cursor
fetch cursor
close
disconnect

#### sql_test3
connect
create table
declare cursor
fetch data

#### sql_test4
var smt is type(size)

#### sql_test5
select
commit

#### sql_test6
ignore (not in gix)

#### sql_test7
lob (not in gix)

#### sql_test8
execute
preprare

#### sql_test9
big select

#### sql_test10
big statement with no sense

#### sql_test11
big declare

#### sql_test12
different format for connect

#### sql_test13
begin end

#### sql_test14
whenever

#### sql_test15
update
declare table

