(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

{
  open Grammar

  (*In alphabetical order*)
  let get_keyword x = 
  match String.uppercase_ascii x with
   | "ALL" -> ALL
   | "AND" -> AND
   | "AS" -> AS
   | "ASC" -> ASC
   | "AT" -> AT
   | "BEGIN" -> BEGIN
   | "BETWEEN" -> BETWEEN
   | "BY" -> BY
   | "CLOSE" -> CLOSE
   | "COMMIT" -> COMMIT
   | "CONNECT" -> CONNECT
   | "CURRENT" -> CURRENT
   | "CURSOR" -> CURSOR
   | "DECLARE" -> DECLARE
   | "DEFAULT" -> DEFAULT
   | "DELETE" -> DELETE
   | "DESC" -> DESC
   | "DISCONNECT" -> DISCONNECT
   | "END" -> END
   | "EXEC" -> EXEC
   | "EXECUTE" -> EXECUTE
   | "FETCH" -> FETCH
   | "FOR" -> FOR
   | "FOUND" -> FOUND
   | "FROM" -> FROM
   | "GOTO" -> GOTO
   | "GO" -> GO
   | "GROUP" -> GROUP
   | "HOLD" -> HOLD
   | "IDENTIFIED" -> IDENTIFIED
   | "IGNORE" -> IGNORE
   | "INNER" -> INNER
   | "IMMEDIATE" -> IMMEDIATE
   | "IN" -> IN
   | "INCLUDE" -> INCLUDE
   | "INSERT" -> INSERT
   | "INTO" -> INTO
   | "JOIN" -> JOIN
   | "LEFT" -> LEFT
   | "NATURAL" -> NATURAL
   | "NOT" -> NOT
   | "NULL" -> NULL
   | "OF" -> OF
   | "ON" -> ON
   | "OR" -> OR
   | "ORDER" -> ORDER
   | "OPEN" -> OPEN
   | "OUTER" -> OUTER
   | "PERFORM" -> PERFORM
   | "PREPARE" -> PREPARE
   | "RELEASE" -> RELEASE
   | "RESET" -> RESET
   | "RIGHT" -> RIGHT
   | "ROLLBACK" -> ROLLBACK
   | "SAVEPOINT" -> SAVEPOINT
   | "SECTION" -> SECTION
   | "SELECT" -> SELECT
   | "SET" -> SET
   | "START" -> START
   | "STATEMENT" -> STATEMENT
   | "SQL" -> SQL
   | "SQLERROR" -> SQLERROR
   | "SQLWARNING" -> SQLWARNING
   | "TABLE" -> TABLE
   | "TO" -> TO
   | "TRAN" -> TRANSACTION
   | "TRANSACTION" -> TRANSACTION
   | "UPDATE" -> UPDATE
   | "USER" -> USER
   | "USING" -> USING
   | "VALUES" -> VALUES
   | "WHEN" -> WHEN
   | "WHERE" -> WHERE
   | "WHENEVER" -> WHENEVER
   | "WITH" -> WITH
   | "WORK" -> WORK
   | _ -> TOKEN x (* "Brut SQL" or sql variable, stay the same *)
}

rule token = parse
  | "END-EXEC"
      { END_EXEC }
  | "end-exec"
      { END_EXEC }
  | "END-EXEC."
      { END_EXEC }
  | ':' (['a'-'z' 'A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_' '-']* as s)
      { COBOL_VAR s }
  | '\\' (['a'-'z' 'A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as s)
      { BACKSLASHED_VAR s }
  | ['a'-'z' 'A'-'Z' '0'-'9']['A'-'Z' 'a'-'z' '0'-'9' '-' '_' '*' ]* as s 
  (*If you want to make a better sql parsing, this is to rework*)
      { get_keyword s }
  | ['0'-'9']+ as s
      { DIGITS s }
  | '\'' ( ['A'-'Z' 'a'-'z' '0'-'9' '_' '(' '*' ')' '.' '[' ']']* as s) '\''
      { STRING s}


(*   | ['>' '<']+ as s (*sql opeatuin*)
      { TOKEN s } *)
  | "||"
      { OR }
  | ['='] 
      { EQUAL }
  | ['+'] 
      { PLUS }
  | ['-'] 
      { MINUS }
  | "<=" 
      { LESS_EQ }
  | ">=" 
       { GREAT_EQ }
  | "<" 
        { LESS }
  | ">" 
        { GREAT }  
  | ','
      { COMMA }
  | '.'
      { DOT }
  | '('
      { LPAR }
  | ')'
      { RPAR }
  | '*'
      { STAR }
(*   | _ as s 
      { TOKEN s } *)
  | eof
      { EOF }

{
  (* when empty lexbuf, we use an exeption... this is not very cool *)
  let token lexbuf =
    try token lexbuf with _ -> EOF
}
