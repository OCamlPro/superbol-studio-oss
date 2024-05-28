%{
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
open Sql_ast
open Cobol_common.Srcloc.INFIX
module Overlay_manager = Sql_overlay_manager

%}
%token COMMA SEMICOLON EQUAL PLUS MINUS OR AND IN
%token LESS_EQ LESS GREAT_EQ GREAT DIFF
%token <char> SPECHAR
%token EOF
%token EXEC SQL END_EXEC
%token BEGIN END DECLARE SECTION DISCONNECT LPAR RPAR DOT STAR COUNT_STAR
(*For whenever*)
%token WHENEVER NOT FOUND SQLERROR SQLWARNING CONTINUE PERFORM GOTO GO
(*For include*)
%token INCLUDE
(*For Connect*)
%token CONNECT TO USER IDENTIFIED BY AT USING RESET AS
(*For Transaction*)
%token START TRANSACTION
(*For Rollback*)
%token ROLLBACK WORK RELEASE SAVEPOINT 
(*For commit*)
%token COMMIT
(*FOr other esql with opt at*)
%token SELECT INTO STATEMENT CURSOR FOR PREPARE FROM EXECUTE IMMEDIATE WITH HOLD
%token INSERT OPEN CLOSE FETCH DELETE UPDATE SET IGNORE ALL OF WHERE CURRENT TABLE
%token WHEN ORDER VALUES NULL DEFAULT GROUP
%token JOIN INNER NATURAL LEFT RIGHT OUTER ON 
(*Sort by*)
%token DESC ASC
%token <string> TOKEN
%token <string> STRING
%token <string> DIGITS
%token <string> COBOL_VAR
%token <string> BACKSLASHED_VAR
%start <esql_instuction> main 

%%


let loc (X) ==
  | x = X; { x &@ Overlay_manager.join_limits $sloc }

let main :=
| EXEC; SQL; stm=esql; option(SEMICOLON); END_EXEC; EOF; {stm}

let cobol_var :=
| c = loc(COBOL_VAR); {c}

let sql_var_name :=
| s = loc(TOKEN); {s} 
(* | s = loc(STRING); {LiteralStr s} *) (*TODO*)

let variable := 
| s=sql_var_name; {SqlVar s} 
| s=cobol_var; {CobolVar s} 

let literalVar := 
| v = variable; {LiteralVar v}
| t = sql_var_name; DOT; lst = separated_nonempty_list(DOT, sql_var_name); {LiteralDot (t::lst)}

let literal :=
| l=literalVar; {l}
| v = loc(DIGITS); {LiteralNum v}
| s = loc(STRING); {LiteralStr s} (*TODO Differentiate 'variable' and "string" and 'char' *)


let esql := 
| AT; v= variable; stm=esql_with_opt_at; {At(v, stm)}
| stm=esql_with_opt_at; {stm}
| BEGIN; {Begin}
| BEGIN; stm=esql_with_opt_at; END; SEMICOLON; {stm} (*I don't know what this is for, but it is sometimes used*)
| BEGIN; DECLARE; SECTION; {BeginDeclare}
| END; DECLARE; SECTION; {EndDeclare}
| WHENEVER; c=whenever_condition; k= whenever_continuation; {Whenever(c, k)}
| INCLUDE; i= sql_var_name; {Include i}
| CONNECT; c=connect_stm; {Connect c}
| DISCONNECT; s=option(variable); {Disconnect s}
| DISCONNECT; ALL; {DisconnectAll}
| IGNORE; sql=sql; {Ignore sql} (*TODO the "sql" can be anything*)

let esql_with_opt_at := 
| i = sql; {Sql i}
| i=sql_select; INTO; 
  s=separated_nonempty_list(COMMA, cobol_var); 
  lst = list(select_option);
  {SelectInto(s, i, lst)}
| START; TRANSACTION; 
  {StartTransaction}
| DECLARE; table_name= literalVar; TABLE; sql=sql;
  {DeclareTable(table_name, sql)}
| DECLARE; crs= sql_var_name; CURSOR; FOR; sql=sql_query; option(forUpdate);
  {DeclareCursor(crs, sql)} (*TODO: COmplete For Update and create a type for it in ast*)
| DECLARE; crs= sql_var_name; CURSOR; WITH; HOLD; FOR; sql=sql_query; option(forUpdate);
  {DeclareCursor(crs, sql)} (*TODO: With Hold in AST*)
| PREPARE; name= sql_var_name; FROM; sql=sql; 
  {Prepare(name, sql)}
| EXECUTE; IMMEDIATE; arg=execute_immediate_arg; 
  {ExecuteImmediate arg}
| EXECUTE; name= sql_var_name; into = option(into_list_cob_var); using= option(using_list_cob_var); 
  {ExecuteIntoUsing(name,into, using)}
| SAVEPOINT; s= variable; 
  {Savepoint s}
| ROLLBACK; r=option(rb_work_or_tran); a=option(rb_args); 
  {Rollback(r, a)}
| COMMIT; wt= option(rb_work_or_tran); RELEASE; 
  {Commit(wt, true)}
| COMMIT; wt= option(rb_work_or_tran); 
  {Commit(wt, false)}
| INSERT; INTO; tab = table; VALUES; v=value;
  {Insert (tab, v)}
| DELETE; sql= sql; 
  {Delete sql}
| UPDATE; table=sql_var_name; sql=sql_update; x=option(update_arg); 
  {Update(table, sql, x)}

(*Unexeped At, but we have to parse it*)
| OPEN; cursor = sql_var_name; ul = option(using_list_cob_var); {Open (cursor, ul)}
| FETCH; sql=sql; l = into_list_cob_var; {Fetch(sql,l)}
| CLOSE; cursor = sql_var_name; {Close cursor}

let table :=
| s = sql_var_name;LPAR; l=separated_nonempty_list(COMMA, sql_var_name); RPAR; {TableLst (s, l)}
| s = sql_var_name; {Table s}

let value :=
| l= separated_nonempty_list(COMMA, value_list); {l}

let value_list :=
| LPAR; l = separated_nonempty_list(COMMA, literal); RPAR; {ValueList l}
| NULL; {ValueNull}
| DEFAULT; {ValueDefault}

(*TODO: forUpdate is incomplete, I have to implement this syntaxe:
FOR {
  READ ONLY 
| UPDATE [OF unqualified-column-name[, unqualified-column-name]. ..]
}*)

let forUpdate := 
| FOR; UPDATE; {} 



let into_list_cob_var :=
| INTO; into= separated_nonempty_list(COMMA,cobol_var); {into}

let using_list_cob_var :=
| USING; LPAR;  using= separated_nonempty_list(COMMA, cobol_var); RPAR; {using}

let execute_immediate_arg :=
| x = STRING; {[Sql_instr x]}
| c = cobol_var; {[ Sql_var( CobolVar c) ]}



let update_arg :=
| WHERE; CURRENT; OF; v=sql_var_name; {WhereCurrentOf v}
| FROM; sql=sql; {UpdateSql( [Sql_instr "FROM"] @ sql)}

let rb_work_or_tran :=
| WORK; {Work}
| TRANSACTION; {Transaction}

let rb_args :=
| RELEASE; {Release}
| TO; SAVEPOINT; v =  sql_var_name; {To v}
| TO; v =  sql_var_name; {To v} 

let connect_stm :=
(*
EXEC SQL CONNECT TO :dbname [ AS :db_conn_id ] 
USER :username USING :db_data_source 
IDENTIFIED BY :password 
*)
| TO; dbname=  literalVar; db_conn_id=option(as_var); USER; username= literalVar;
  USING; db_data_source=  literalVar; IDENTIFIED; BY; password=  literalVar;
  { Connect_to_idby {dbname; db_conn_id; username; db_data_source; password} }

(* 
EXEC SQL CONNECT TO :db_data_source [ AS :db_conn_id ]
USER :username.:opt_password [ USING password ];
-> Supporté si il n'y as pas de opt_passwod
*)
| TO; db_data_source=  literalVar; db_conn_id=option(as_var); USER; username= literalVar;
  password = option(using_var);
  { Connect_to {db_data_source; db_conn_id; username; password} }
(* 
EXEC SQL CONNECT USING :db_data_source 
(credentials must be embedded to be able to connect) 
*)
| USING; db_data_source= literalVar; {Connect_using{db_data_source}}
(* 
EXEC SQL CONNECT :username IDENTIFIED BY :password 
[ AT :db_conn_id ] [ USING :db_data_source] (mode 4 is unsupported)
*)
| username=  literalVar; IDENTIFIED; BY; password=  literalVar; 
  db_conn_id = option(at_var); db_data_source= option(using_var);
  {Connect_user{username; password; db_conn_id; db_data_source}}
| RESET; name=option( literalVar); 
  {Connect_reset name }

let at_var:= AT; p= literalVar; {p}

let using_var:= USING; p= literalVar; {p}

let as_var:= AS; v= literalVar; {v} 

let whenever_condition :=
| NOT; FOUND; {Not_found_whenever}
| SQLERROR; {SqlError_whenever}
| SQLWARNING; {SqlWarning_whenever}

let whenever_continuation :=
| CONTINUE;  {Continue}
| PERFORM; label= sql_var_name; {Perform label}
| GOTO; stmt_label= sql_var_name; {Goto stmt_label}
| GO; TO; stmt_label= sql_var_name; {Goto stmt_label}

(*SQL Stuff*)

let sql_query :=
| s = sql_select; lst = list(select_option); {(s, lst)}

let sql_select:=
| SELECT; x = separated_list(COMMA, sql_complex_literal); {x}

let select_option :=
| FROM; f= from_stm; {From f} 
| ORDER; BY; l=separated_nonempty_list(COMMA, order_by); {OrderBy(l)}
| WHERE; s = search_condition; {Where s}
| GROUP; BY; x = separated_list(COMMA, literal); {GroupBy x}

let order_by:=
| v=literal; option(ASC);{Asc v}
| v=literal; DESC; {Desc v}


let from_stm :=
| lst = separated_nonempty_list(COMMA, table_ref); {lst}

/* let table_ref :=
| table_name = table_ref_aux; AS; correlation_name = literal; {FromLitAs(table_name, correlation_name)}
| t = table_ref_aux;{t}  */

let table_ref := 
| j = qualified_join;{j} 
| t = table_ref_non_rec; {t}

let table_ref_simpl :=
| LPAR; select= sql_query; RPAR; {FromSelect(select)}
| LPAR; t = table_ref; RPAR; {t}
| table_name = literal; {FromLit(table_name)}

let table_ref_non_rec :=
| table_name = table_ref_simpl; AS; correlation_name = literal; {FromLitAs(table_name, correlation_name)}
| t = table_ref_simpl; {t}

let qualified_join :=
| t1=table_ref_non_rec; j=join_type; JOIN; t2=table_ref_non_rec; o=option(qualified_join_option); {Join(t1, j, t2, o)}

let join_type :=
| INNER; {InnerJoin}
| NATURAL; {NaturalJoin}
| LEFT; option(OUTER); {LeftJoin}
| RIGHT; option(OUTER); {RightJoin}

let qualified_join_option := 
| ON; s = search_condition; {JoinOn(s)}
(*| USING; s=separated_nonempty_list(COMMA, sql_var_name); {JoinUsing(s)}*)



let search_condition :=
| s1 = search_condition; OR; s2 = search_condition_aux; {WhereConditionOr(s1, s2)}
| s = search_condition_aux; {s}

let search_condition_aux :=
| s1 = search_condition_aux2; AND; s2 = search_condition_aux; {WhereConditionAnd(s1, s2)}
| s = search_condition_aux2; {s}

let search_condition_aux2 :=
| NOT; s=search_condition_aux2; {WhereConditionNot s}
| LPAR; s=search_condition; RPAR; {s}
| s = predicate; {s}

let predicate :=
| c = comparison_predicate; {WhereConditionCompare c}
| i = in_predicate; {WhereConditionIn i} 

let in_predicate:= (*HGNC_GENE_SYMBOL IN ('MDM2', 'TP53', 'CDKN1A','CCNE1')*)
| l = literal; IN; LPAR; lst = separated_nonempty_list(COMMA, sql_complex_literal); RPAR; {InVarLst(l, lst)}

let comparison_predicate:=
| l = sql_complex_literal; c = compOp; LPAR; sql = sql_query; RPAR; {CompareQuery(l, c, [Sql_query sql])}
| l = sql_complex_literal; c = compOp; l2 =  sql_complex_literal;  {CompareLit(l, c, l2)}

let compOp :=  
| LESS; {Less} (* < *)
| GREAT; {Great} (* > *)
| LESS_EQ; {LessEq} (* <= *)
| GREAT_EQ; {GreatEq} (* >= *)
| EQUAL; {EqualComp} (* = *)
| DIFF; {Diff} (* <> *)



(*For exemple 
SET 
   CID = CID + :VAR1,
   FLD01 = FLD01 + :VAR2,
   FLD02 = CONCAT(FLD02, CAST(:VAR3 AS VARCHAR))
*)
let sql_update :=
  SET; equal_lst = separated_nonempty_list(COMMA, sql_equal); {equal_lst}

let sql_equal :=
  s = sql_var_name; EQUAL; op = sql_op; {(s, op)}

let sql_op := 
| c=sql_complex_literal; o= binop; v=sql_op; {SqlOpBinop(o, c, v)}
| a = sql_complex_literal; { SqlOpLit a }

let sql_complex_literal :=
| v= literal; AS; c=sql_var_name; {SqlCompAs(v, c)}
| v= literal; {SqlCompLit v }
| fun_name=sql_var_name; LPAR; args = separated_list(COMMA, sql_op) ; RPAR;
  {SqlCompFun(fun_name, args)} 
| STAR; {SqlCompStar}

let binop := 
| PLUS; {Add}
| MINUS; {Minus}
| STAR; {Times}
| OR; {Or}


let sql := 
| t = sql_no_simpl_cobol; {t}
| t = cobol_var; {[Sql_var( CobolVar t)]}

let sql_no_simpl_cobol :=
| t = sql_first_token;  x = list(sql_token); {[t] @ x} (*Note for the futur me: a list can be empty*)
| s = sql_query; {[Sql_query s]}

/* let sql_token_after_select:=
| FROM; {Sql_instr "FROM" }
| WHERE; s=search_condition; {Sql_search_condition s}
| ORDER; BY; {Sql_instr"ORDER BY"}
| WHEN; {Sql_instr"WHEN"}
 */

let sql_first_token :=
| t = TOKEN; {Sql_instr t }
| d = DIGITS; {Sql_instr d }
| s = STRING; {Sql_instr ("\""^s^"\"")}
| LPAR ; {Sql_instr "(" }
| RPAR; {Sql_instr ")" }
| NOT; {Sql_instr "NOT" }
| STAR; {Sql_instr "*" }
| SET; {Sql_instr "SET"}
| FOR; {Sql_instr "FOR" }
| BY; {Sql_instr "BY" }
| COUNT_STAR; {Sql_instr "(*)"}
| TABLE; {Sql_instr "TABLE"}
| FROM; {Sql_instr "FROM" }
| WHERE; {Sql_instr"WHERE"}
| ORDER; BY; {Sql_instr"ORDER BY"}
| WHEN; {Sql_instr"WHEN"}

let sql_token_not_first :=
| TO; {Sql_instr "TO"}
| AT; {Sql_instr "AT"}
| IN; {Sql_instr "IN"}
| UPDATE; {Sql_instr "UPDATE" }
| EQUAL; {Sql_instr "=" }
| COMMA; {Sql_instr "," }
(*| SEMICOLON; {Sql_instr ";" }*)
| DOT ; {Sql_instr "." }
| t = cobol_var; {Sql_var( CobolVar t) }


let sql_token := 
| s = sql_first_token; {s}
| s = sql_token_not_first; {s}

%%
