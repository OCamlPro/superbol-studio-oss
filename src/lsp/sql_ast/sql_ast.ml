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
open Cobol_common
(**************************************************************************)
(*                              AST                                       *)
(**************************************************************************)
type sql_token = 
  | Sql_instr of string
  | Sql_var of variable
  | Sql_lit of literal
  | Sql_query of sql_query
  | Sql_equality of sql_equal (*TODO: remove*)
  | Sql_search_condition of search_condition (*TODO: remove*)
and sql_var = string with_loc
and cobol_var_id = string with_loc

and sql_instruction = sql_token list

and variable = 
  | SqlVar of sql_var
  | CobolVar of cobol_var_id

and literal =
  | LiteralVar of variable
  | LiteralNum of string with_loc
  | LiteralStr of string with_loc
  | LiteralDot of string with_loc list

and complex_literal =
  | SqlCompLit of literal
  | SqlCompAs of literal * sql_var (*ex: SMT AS INT*)
  | SqlCompFun of sql_var * sql_op list
  | SqlCompStar

and esql_instuction =
  | At of variable * esql_instuction 
  | Sql of sql_instruction
  | Begin 
  | BeginDeclare
  | EndDeclare
  | StartTransaction
  | Whenever of whenever_condition * whenever_continuation
  | Include of sql_var
  | Connect of connect_syntax
  | Rollback of rb_work_or_tran option * rb_args option
  | Commit of rb_work_or_tran option * bool
  | Savepoint of variable
  | SelectInto of (cobol_var_id) list * sql_select * sql_select_option list (*select and option_select*)
  | DeclareTable of literal * sql_instruction 
  | DeclareCursor of sql_var * sql_query
  | Prepare of sql_var * sql_instruction 
  | ExecuteImmediate of sql_instruction
  | ExecuteIntoUsing of sql_var * (cobol_var_id list) option * (cobol_var_id list) option
  | Disconnect of variable option (*db_id*)
  | DisconnectAll
  | Open of sql_var * (cobol_var_id list) option  (*cursor name*)
  | Close of sql_var (*cursor name*)
  | Fetch of sql_instruction * (cobol_var_id) list
  | Insert of table * (value list)
  | Delete of sql_instruction
  | Update of sql_var * sql_update * (update_arg option)
  | Ignore of sql_instruction

and table = 
  | Table of sql_var
  | TableLst of sql_var * (sql_var list)

and value =
  | ValueNull
  | ValueDefault
  | ValueList of literal list

and rb_work_or_tran = Work | Transaction
and rb_args = Release | To of sql_var

and connect_syntax =
  | Connect_to_idby of {
    dbname : literal; 
    db_conn_id : literal option; 
    username : literal; 
    db_data_source : literal;
    password : literal
  }
  | Connect_to of {
    db_data_source : literal; 
    db_conn_id : literal option; 
    username : literal; 
    password : literal option 
  }
  | Connect_using of {db_data_source: literal}

  | Connect_user of{
    username: literal; 
    password: literal; 
    db_conn_id: literal option; 
    db_data_source: literal option
  }
  | Connect_reset of (literal option)

  (*WHENEVER*)
and whenever_condition = 
  | Not_found
  | SqlError
  | SqlWarning

and whenever_continuation = 
  | Continue
  | Perform of sql_var (*A label in cob program*)
  | Goto of sql_var (*TODO doc*)


and update_arg = 
  | WhereCurrentOf of sql_var 
  | UpdateSql of sql_instruction

(*SQL*)
and sql_query = sql_select * sql_select_option list

and sql_select_option =
  | From of from_stm 
  | Where of search_condition 
  | OrderBy of sql_orderBy list
  | GroupBy of literal list

and from_stm = table_ref list

and table_ref = 
  | FromLitAs of table_ref * literal
  | FromLit of literal
  | FromSelect of sql_query
  | Join of table_ref * join * table_ref * join_option option

and join = InnerJoin | NaturalJoin | LeftJoin | RightJoin
and join_option = 
  | JoinOn of search_condition
  | JoinUsing of sql_var list
and sql_orderBy = Asc of literal | Desc of literal

and sql_select = complex_literal list

and sql_update = (sql_equal) list
and sql_equal = sql_var * sql_op

and sql_op = 
  | SqlOpLit of complex_literal
  | SqlOpBinop of (sql_binop * complex_literal * sql_op)
  
and sql_binop = Add | Minus | Times | Or


and search_condition = 
  | WhereConditionOr of search_condition * search_condition
  | WhereConditionAnd of search_condition * search_condition
  | WhereConditionNot of search_condition
  | WhereConditionCompare of sql_compare
  | WhereConditionIn of sql_condition_in
  
and sql_condition_in =
  | InVarLst of literal * (complex_literal list)

and sql_compare = 
  | CompareQuery of complex_literal * compOperator * sql_instruction
  | CompareLit of complex_literal * compOperator * complex_literal

and compOperator = Less | Great | LessEq | GreatEq | EqualComp | Diff 


(**************************************************************************)
(*                              COMPARE                                   *)
(**************************************************************************)

let compare_option cmp o1 o2 =
  match o1, o2 with
  | Some x1, Some x2 -> cmp x1 x2
  | None, None -> 0
  | _ -> 1

let compare_list cmp l1 l2 =
  try
    List.fold_left2 (fun acc x y -> if acc = 0 then cmp x y else acc) 0 l1 l2
  with Invalid_argument _ -> 1

let rec compare e1 e2 =
  match e1, e2 with
  | At (v1, ei1), At (v2, ei2) ->
      let cmp_var = compare_variable v1 v2 in
      if cmp_var = 0 then compare ei1 ei2 else cmp_var
  | Sql si1, Sql si2 -> compare_sql_instruction si1 si2
  | Begin, Begin -> 0
  | BeginDeclare, BeginDeclare -> 0
  | EndDeclare, EndDeclare -> 0
  | StartTransaction, StartTransaction -> 0
  | Whenever (wc1, wco1), Whenever (wc2, wco2) ->
      let cmp_cond = compare_whenever_condition wc1 wc2 in
      if cmp_cond = 0 then compare_whenever_continuation wco1 wco2 else cmp_cond
  | Include v1, Include v2 -> Stdlib.compare v1 v2
  | Connect cs1, Connect cs2 -> compare_connect_syntax cs1 cs2
  | Rollback (wo1, ra1), Rollback (wo2, ra2) ->
      let cmp_wo = compare_option compare_rb_work_or_tran wo1 wo2 in
      if cmp_wo = 0 then compare_option compare_rb_args ra1 ra2 else cmp_wo
  | Commit (wo1, b1), Commit (wo2, b2) ->
      let cmp_wo = compare_option compare_rb_work_or_tran wo1 wo2 in
      if cmp_wo = 0 then Stdlib.compare b1 b2 else cmp_wo
  | Savepoint v1, Savepoint v2 -> compare_variable v1 v2
  | SelectInto (l1, s1, o1), SelectInto (l2, s2, o2) ->
      let cmp_lst = compare_list Stdlib.compare l1 l2 in
      if cmp_lst = 0 then
        let cmp_sel = compare_sql_select s1 s2 in
        if cmp_sel = 0 then compare_list compare_sql_select_option o1 o2 else cmp_sel
      else cmp_lst
  | DeclareTable (l1, si1), DeclareTable (l2, si2) ->
      let cmp_lit = compare_literal l1 l2 in
      if cmp_lit = 0 then compare_sql_instruction si1 si2 else cmp_lit
  | DeclareCursor (v1, q1), DeclareCursor (v2, q2) ->
      let cmp_var = Stdlib.compare v1 v2 in
      if cmp_var = 0 then compare_sql_query q1 q2 else cmp_var
  | Prepare (v1, si1), Prepare (v2, si2) ->
      let cmp_var = Stdlib.compare v1 v2 in
      if cmp_var = 0 then compare_sql_instruction si1 si2 else cmp_var
  | ExecuteImmediate si1, ExecuteImmediate si2 -> compare_sql_instruction si1 si2
  | ExecuteIntoUsing (v1, l1, l2), ExecuteIntoUsing (v2, l3, l4) ->
      let cmp_var = Stdlib.compare v1 v2 in
      if cmp_var = 0 then
        let cmp_opt1 = compare_option (compare_list Stdlib.compare) l1 l3 in
        if cmp_opt1 = 0 then compare_option (compare_list Stdlib.compare) l2 l4 else cmp_opt1
      else cmp_var
  | Disconnect v1, Disconnect v2 -> compare_option compare_variable v1 v2
  | DisconnectAll, DisconnectAll -> 0
  | Open (v1, l1), Open (v2, l2) ->
      let cmp_var = Stdlib.compare v1 v2 in
      if cmp_var = 0 then compare_option (compare_list Stdlib.compare) l1 l2 else cmp_var
  | Close v1, Close v2 -> Stdlib.compare v1 v2
  | Fetch (si1, l1), Fetch (si2, l2) ->
      let cmp_si = compare_sql_instruction si1 si2 in
      if cmp_si = 0 then compare_list Stdlib.compare l1 l2 else cmp_si
  | Insert (t1, v1), Insert (t2, v2) ->
      let cmp_tbl = compare_table t1 t2 in
      if cmp_tbl = 0 then compare_list compare_value v1 v2 else cmp_tbl
  | Delete si1, Delete si2 -> compare_sql_instruction si1 si2
  | Update (v1, u1, a1), Update (v2, u2, a2) ->
      let cmp_var = Stdlib.compare v1 v2 in
      if cmp_var = 0 then
        let cmp_upd = compare_list compare_sql_equal u1 u2 in
        if cmp_upd = 0 then compare_option compare_update_arg a1 a2 else cmp_upd
      else cmp_var
  | Ignore si1, Ignore si2 -> compare_sql_instruction si1 si2
  | _ -> 1


and compare_sql_token t1 t2 =
  match t1, t2 with
  | Sql_instr s1, Sql_instr s2 -> Stdlib.compare s1 s2
  | Sql_var v1, Sql_var v2 -> compare_variable v1 v2
  | Sql_lit l1, Sql_lit l2 -> compare_literal l1 l2
  | Sql_query q1, Sql_query q2 -> compare_sql_query q1 q2
  | Sql_equality e1, Sql_equality e2 -> compare_sql_equal e1 e2
  | Sql_search_condition sc1, Sql_search_condition sc2 -> compare_search_condition sc1 sc2
  | _ -> 1

and compare_variable v1 v2 =
  match v1, v2 with
  | SqlVar sv1, SqlVar sv2 -> Stdlib.compare sv1 sv2
  | CobolVar cv1, CobolVar cv2 -> Stdlib.compare cv1 cv2
  | _ -> 1

and compare_literal l1 l2 =
  match l1, l2 with
  | LiteralVar v1, LiteralVar v2 -> compare_variable v1 v2
  | LiteralNum n1, LiteralNum n2 -> Stdlib.compare n1 n2
  | LiteralStr s1, LiteralStr s2 -> Stdlib.compare s1 s2
  | LiteralDot d1, LiteralDot d2 -> Stdlib.compare d1 d2
  | _ -> 1

and compare_sql_query (s1, o1) (s2, o2) =
  let cmp_sel = compare_sql_select s1 s2 in
  if cmp_sel = 0 then compare_sql_select_option_list o1 o2 else cmp_sel

and compare_sql_select_option_list l1 l2 =
  compare_list compare_sql_select_option l1 l2

and compare_sql_select l1 l2 =
  compare_list compare_complex_literal l1 l2
and compare_complex_literal c1 c2 =
  match c1, c2 with
  | SqlCompLit l1, SqlCompLit l2 -> compare_literal l1 l2
  | SqlCompAs (l1, v1), SqlCompAs (l2, v2) -> 
      let cmp_lit = compare_literal l1 l2 in
      if cmp_lit = 0 then Stdlib.compare v1 v2 else cmp_lit
  | SqlCompFun (v1, o1), SqlCompFun (v2, o2) ->
      let cmp_var = Stdlib.compare v1 v2 in
      if cmp_var = 0 then compare_list compare_sql_op o1 o2 else cmp_var
  | SqlCompStar, SqlCompStar -> 0
  | _ -> 1

and compare_sql_op o1 o2 =
  match o1, o2 with
  | SqlOpLit l1, SqlOpLit l2 -> compare_complex_literal l1 l2
  | SqlOpBinop (b1, c1, o1), SqlOpBinop (b2, c2, o2) ->
      let cmp_binop = compare_sql_binop b1 b2 in
      if cmp_binop = 0 then
        let cmp_comp = compare_complex_literal c1 c2 in
        if cmp_comp = 0 then compare_sql_op o1 o2 else cmp_comp
      else cmp_binop
  | _ -> 1

and compare_sql_binop b1 b2 =
  match b1, b2 with
  | Add, Add | Minus, Minus | Times, Times | Or, Or -> 0
  | _ -> 1

and compare_sql_equal (v1, o1) (v2, o2) =
  let cmp_var = Stdlib.compare v1 v2 in
  if cmp_var = 0 then compare_sql_op o1 o2 else cmp_var

and compare_search_condition sc1 sc2 =
  match sc1, sc2 with
  | WhereConditionOr (c1, c2), WhereConditionOr (c3, c4)
  | WhereConditionAnd (c1, c2), WhereConditionAnd (c3, c4) ->
      let cmp_c1 = compare_search_condition c1 c3 in
      if cmp_c1 = 0 then compare_search_condition c2 c4 else cmp_c1
  | WhereConditionNot c1, WhereConditionNot c2 ->
      compare_search_condition c1 c2
  | WhereConditionCompare c1, WhereConditionCompare c2 ->
      compare_sql_compare c1 c2
  | WhereConditionIn c1, WhereConditionIn c2 ->
      compare_sql_condition_in c1 c2
  | _ -> 1

and compare_sql_compare c1 c2 =
  match c1, c2 with
  | CompareQuery (l1, o1, i1), CompareQuery (l2, o2, i2) ->
      let cmp_lit = compare_complex_literal l1 l2 in
      if cmp_lit = 0 then
        let cmp_op = compare_compOperator o1 o2 in
        if cmp_op = 0 then compare_sql_instruction i1 i2 else cmp_op
      else cmp_lit
  | CompareLit (l1, o1, l2), CompareLit (l3, o2, l4) ->
      let cmp_lit1 = compare_complex_literal l1 l3 in
      if cmp_lit1 = 0 then
        let cmp_op = compare_compOperator o1 o2 in
        if cmp_op = 0 then compare_complex_literal l2 l4 else cmp_op
      else cmp_lit1
  | _ -> 1

and compare_compOperator o1 o2 =
  match o1, o2 with
  | Less, Less | Great, Great | LessEq, LessEq | GreatEq, GreatEq 
  | EqualComp, EqualComp | Diff, Diff -> 0
  | _ -> 1

and compare_sql_condition_in (InVarLst (l1, cl1)) (InVarLst (l2, cl2)) =
  let cmp_lit = compare_literal l1 l2 in
  if cmp_lit = 0 then compare_list compare_complex_literal cl1 cl2 else cmp_lit

and compare_sql_instruction l1 l2 =
  compare_list compare_sql_token l1 l2

and compare_sql_select_option o1 o2 =
  match o1, o2 with
  | From f1, From f2 -> compare_from_stm f1 f2
  | Where sc1, Where sc2 -> compare_search_condition sc1 sc2
  | OrderBy ol1, OrderBy ol2 -> compare_list compare_sql_orderBy ol1 ol2
  | GroupBy ll1, GroupBy ll2 -> compare_list compare_literal ll1 ll2
  | _ -> 1

and compare_from_stm f1 f2 =
  compare_list compare_table_ref f1 f2

and compare_table_ref tr1 tr2 =
  match tr1, tr2 with
  | FromLitAs (r1, l1), FromLitAs (r2, l2) ->
      let cmp_ref = compare_table_ref r1 r2 in
      if cmp_ref = 0 then compare_literal l1 l2 else cmp_ref
  | FromLit l1, FromLit l2 -> compare_literal l1 l2
  | FromSelect q1, FromSelect q2 -> compare_sql_query q1 q2
  | Join (r1, j1, r2, o1), Join (r3, j2, r4, o2) ->
      let cmp_ref1 = compare_table_ref r1 r3 in
      if cmp_ref1 = 0 then
        let cmp_join = compare_join j1 j2 in
        if cmp_join = 0 then
          let cmp_ref2 = compare_table_ref r2 r4 in
          if cmp_ref2 = 0 then compare_option compare_join_option o1 o2 else cmp_ref2
        else cmp_join
      else cmp_ref1
  | _ -> 1

and compare_join j1 j2 =
  match j1, j2 with
  | InnerJoin, InnerJoin | NaturalJoin, NaturalJoin 
  | LeftJoin, LeftJoin | RightJoin, RightJoin -> 0
  | _ -> 1

and compare_join_option jo1 jo2 =
  match jo1, jo2 with
  | JoinOn sc1, JoinOn sc2 -> compare_search_condition sc1 sc2
  | JoinUsing l1, JoinUsing l2 -> compare_list Stdlib.compare l1 l2
  | _ -> 1

and compare_sql_orderBy o1 o2 =
  match o1, o2 with
  | Asc l1, Asc l2 -> compare_literal l1 l2
  | Desc l1, Desc l2 -> compare_literal l1 l2
  | _ -> 1  
and compare_table t1 t2 =
  match t1, t2 with
  | Table v1, Table v2 -> Stdlib.compare v1 v2
  | TableLst (v1, l1), TableLst (v2, l2) ->
      let cmp_var = Stdlib.compare v1 v2 in
      if cmp_var = 0 then compare_list Stdlib.compare l1 l2 else cmp_var
  | _ -> 1

and compare_value v1 v2 =
  match v1, v2 with
  | ValueNull, ValueNull -> 0
  | ValueDefault, ValueDefault -> 0
  | ValueList l1, ValueList l2 -> compare_list compare_literal l1 l2
  | _ -> 1

and compare_rb_work_or_tran w1 w2 =
  match w1, w2 with
  | Work, Work | Transaction, Transaction -> 0
  | _ -> 1

and compare_rb_args a1 a2 =
  match a1, a2 with
  | Release, Release -> 0
  | To v1, To v2 -> Stdlib.compare v1 v2
  | _ -> 1

(* Comparaison pour update_arg *)
and compare_update_arg a1 a2 =
  match a1, a2 with
  | WhereCurrentOf v1, WhereCurrentOf v2 -> Stdlib.compare v1 v2
  | UpdateSql si1, UpdateSql si2 -> compare_sql_instruction si1 si2
  | _ -> 1

and compare_connect_syntax cs1 cs2 =
  match cs1, cs2 with
  | Connect_to_idby { dbname = d1; db_conn_id = dc1; username = u1; db_data_source = ds1; password = p1 },
    Connect_to_idby { dbname = d2; db_conn_id = dc2; username = u2; db_data_source = ds2; password = p2 } ->
      let cmp_dbname = compare_literal d1 d2 in
      if cmp_dbname = 0 then
        let cmp_conn_id = compare_option compare_literal dc1 dc2 in
        if cmp_conn_id = 0 then
          let cmp_username = compare_literal u1 u2 in
          if cmp_username = 0 then
            let cmp_data_source = compare_literal ds1 ds2 in
            if cmp_data_source = 0 then compare_literal p1 p2 else cmp_data_source
          else cmp_username
        else cmp_conn_id
      else cmp_dbname
  | Connect_to { db_data_source = ds1; db_conn_id = dc1; username = u1; password = p1 },
    Connect_to { db_data_source = ds2; db_conn_id = dc2; username = u2; password = p2 } ->
      let cmp_data_source = compare_literal ds1 ds2 in
      if cmp_data_source = 0 then
        let cmp_conn_id = compare_option compare_literal dc1 dc2 in
        if cmp_conn_id = 0 then
          let cmp_username = compare_literal u1 u2 in
          if cmp_username = 0 then compare_option compare_literal p1 p2 else cmp_username
        else cmp_conn_id
      else cmp_data_source
  | Connect_using { db_data_source = ds1 }, Connect_using { db_data_source = ds2 } ->
      compare_literal ds1 ds2
  | Connect_user { username = u1; password = p1; db_conn_id = dc1; db_data_source = ds1 },
    Connect_user { username = u2; password = p2; db_conn_id = dc2; db_data_source = ds2 } ->
      let cmp_username = compare_literal u1 u2 in
      if cmp_username = 0 then
        let cmp_password = compare_literal p1 p2 in
        if cmp_password = 0 then
          let cmp_conn_id = compare_option compare_literal dc1 dc2 in
          if cmp_conn_id = 0 then compare_option compare_literal ds1 ds2 else cmp_conn_id
        else cmp_password
      else cmp_username
  | Connect_reset l1, Connect_reset l2 -> compare_option compare_literal l1 l2
  | _ -> 1

and compare_whenever_condition wc1 wc2 =
  match wc1, wc2 with
  | Not_found, Not_found | SqlError, SqlError | SqlWarning, SqlWarning -> 0
  | _ -> 1

and compare_whenever_continuation wc1 wc2 =
  match wc1, wc2 with
  | Continue, Continue -> 0
  | Perform v1, Perform v2 -> Stdlib.compare v1 v2
  | Goto v1, Goto v2 -> Stdlib.compare v1 v2
  | _ -> 1


(**************************************************************************)
(*                              PRETTY PRINT                              *)
(**************************************************************************)

let rec list_comma (fmt : Format.formatter) (g : 'a list * (Format.formatter -> 'a -> unit)) : unit = 
  let (x, f) = g in 
  match x with
  | [] -> Format.fprintf fmt ""
  | [ele] -> Format.fprintf fmt "%a " f ele
  | ele::t -> Format.fprintf fmt "%a, %a" f ele list_comma (t, f)

  (*Todo: Declaration are a separate case because else  
   "WORKING-STORAGE SECTION.
    EXEC SQL 
      INCLUDE EMPREC 
    END-EXEC"

  becomes "WORKING-STORAGE SECTION.EXEC SQL INCLUDE EMPREC END-EXEC"
     *)
let rec pp fmt x = 
  match (x) with
  | BeginDeclare -> Format.fprintf fmt "\n EXEC SQL BEGIN DECLARE SECTION END-EXEC. \n"
  | EndDeclare -> Format.fprintf fmt "\n EXEC SQL END DECLARE SECTION END-EXEC. \n"
  | Include i -> Format.fprintf fmt "\n EXEC SQL INCLUDE %s END-EXEC. \n" i.payload
  | _ -> Format.fprintf fmt "EXEC SQL %a END-EXEC" pp_esql x



  and pp_esql fmt x = 
  match x with
  | At (v, instr) -> Format.fprintf fmt "AT %a %a" pp_var v pp_esql instr
  | Sql instr -> pp_sql fmt instr 
  | Begin -> Format.fprintf fmt "BEGIN"
  | BeginDeclare -> Format.fprintf fmt "BEGIN DECLARE SECTION"
  | EndDeclare -> Format.fprintf fmt "END DECLARE SECTION"
  | StartTransaction -> Format.fprintf fmt "START TRANSACTION"
  | Whenever (c, k) -> 
    Format.fprintf fmt "WHENEVER %a %a" 
    pp_whenever_condtion c 
    pp_whenever_continuation k
  | Include i -> Format.fprintf fmt "INCLUDE %s" i.payload
  | Connect c -> Format.fprintf fmt "CONNECT %a" pp_connect c
  | Rollback (rb_work_or_tran, rb_args) ->
    Format.fprintf fmt "ROLLBACK %a %a" 
    pp_some_rb_work_or_tran rb_work_or_tran
    pp_rb_args rb_args
  | Commit(rb_work_or_tran, bool) -> 
    let s= match bool with
      | true -> "RELEASE"
      | false -> ""
    in
    Format.fprintf fmt "COMMIT %a %s" 
    pp_some_rb_work_or_tran rb_work_or_tran
    s
  | Savepoint s -> Format.fprintf fmt "SAVEPOINT %a" pp_var s
  | SelectInto (into, sql, sql2) -> Format.fprintf fmt "SELECT %a INTO %a %a" 
    pp_select_lst sql
    pp_cob_lst into
    pp_select_options_lst sql2
  | DeclareTable (var, sql) -> Format.fprintf fmt "DECLARE %a TABLE %a" pp_lit var pp_sql sql
  | DeclareCursor (var, sql) -> Format.fprintf fmt "DECLARE %s CURSOR FOR %a" var.payload pp_sql_query sql
  | Prepare (str, sql) -> Format.fprintf fmt "PREPARE %s FROM %a" str.payload pp_sql sql
  | ExecuteImmediate sql -> Format.fprintf fmt "EXECUTE IMMEDIATE %a" pp_sql sql
  | ExecuteIntoUsing (var, into, using) -> 
    Format.fprintf fmt "EXECUTE %s %a %a" 
    var.payload 
    pp_some_cob_lst (into, "INTO")
    pp_some_cob_lst (using, "USING")

  | Disconnect sdbname -> Format.fprintf fmt "DISCONNECT %a" pp_some_var (sdbname, "")
  | DisconnectAll -> Format.fprintf fmt "DISCONNECT ALL"
  | Open (cursor, lst) -> Format.fprintf fmt "OPEN %s %a" cursor.payload pp_some_cob_lst (lst, "USING")
  | Close cursor -> Format.fprintf fmt "CLOSE %s" cursor.payload
  | Fetch (sql, var) -> Format.fprintf fmt "FETCH %a INTO %a" 
    pp_sql sql
    pp_cob_lst var
  | Insert (tab, v) -> Format.fprintf fmt "INSERT INTO %a VALUES (%a)" pp_table tab pp_value v
  | Delete sql -> Format.fprintf fmt "DELETE %a" pp_sql sql
  | Update (table, equallst, swhere) -> 
    Format.fprintf fmt "UPDATE %s SET %a %a" 
    table.payload 
    pp_sql_update equallst 
    pp_where_arg swhere
  | Ignore lst -> Format.fprintf fmt "IGNORE %a" pp_sql lst

and pp_table fmt x =
  match x with 
  | Table t -> Format.fprintf fmt "%s" t.payload
  | TableLst (t, lst) -> 
      let f  = pp_sql_var in  
      let pp_aux fmt lst = list_comma fmt (lst, f) in
    Format.fprintf fmt "%s(%a)" t.payload pp_aux lst

and pp_sql_var fmt x = Format.fprintf fmt "%s" x.payload
and pp_value fmt x = list_comma fmt (x, (pp_one_value))

and pp_one_value fmt x =
  match x with
  | ValueDefault -> Format.fprintf fmt "DEFAULT"
  | ValueNull -> Format.fprintf fmt "NULL"
  | ValueList l -> Format.fprintf fmt "(%a)" pp_list_lit l

and pp_where_arg fmt = function
| Some x -> (
  match x with
  | WhereCurrentOf swhere -> Format.fprintf fmt "WHERE CURRENT OF %s" swhere.payload 
  | UpdateSql sql -> pp_sql fmt sql)
| None -> Format.fprintf fmt ""

and pp_sql_update_aux fmt (var, op) = 
  Format.fprintf fmt "%s = %a " var.payload pp_sql_op op

and pp_sql_update fmt x = 
  List.iter (pp_sql_update_aux fmt) x  

and pp_sql_op fmt = function
| SqlOpBinop (op, sql1, sql2) -> Format.fprintf fmt "%a %s %a" pp_complex_literal sql1 (pp_binop op) pp_sql_op sql2
| SqlOpLit (l) -> Format.fprintf fmt "%a" pp_complex_literal l

and pp_sql_some_condition fmt = function
| Some s -> Format.fprintf fmt "WHERE %a" pp_sql_condition s 
| None -> Format.fprintf fmt ""

and pp_sql_condition fmt = function 
| WhereConditionAnd (s1, s2) -> Format.fprintf fmt "%a AND %a" pp_sql_condition s1 pp_sql_condition s2
| WhereConditionOr (s1, s2) -> Format.fprintf fmt "%a OR %a" pp_sql_condition s1 pp_sql_condition s2
| WhereConditionNot s -> Format.fprintf fmt "Not %a" pp_sql_condition s
| WhereConditionCompare c -> 
  let rec pp_compare fmt = function 
    | CompareLit (l1, c, l2) -> Format.fprintf fmt "%a %s %a" pp_complex_literal l1 (comp_op_to_string c) pp_complex_literal l2
    | CompareQuery (l1, c, s) -> Format.fprintf fmt "%a %s %a" pp_complex_literal l1 (comp_op_to_string c) pp_sql s

    and comp_op_to_string = function 
    | Less -> "<" 
    | Great -> ">"
    | LessEq -> "<="
    | GreatEq -> ">="
    | EqualComp -> "="
    | Diff -> "<>"
  in
  Format.fprintf fmt "%a" pp_compare c
  | WhereConditionIn s -> Format.fprintf fmt "%a" pp_condition_in s

and pp_condition_in fmt x =
let pp_aux fmt lst = list_comma fmt (lst, pp_complex_literal) in
match x with
| InVarLst (l, vlist) -> Format.fprintf fmt "%a IN %a" pp_lit l pp_aux vlist 



and pp_complex_literal fmt = function
| SqlCompLit v -> Format.fprintf fmt "%a" pp_lit v
| SqlCompAs (l, v)  ->  Format.fprintf fmt "%a AS %s" pp_lit l v.payload 
| SqlCompFun (funName, args) -> 
  let pp_args fmt lst = list_comma fmt (lst, pp_sql_op)
  in
  Format.fprintf fmt "%s(%a)" funName.payload pp_args args 
| SqlCompStar -> Format.fprintf fmt "*"

and pp_binop = function
| Add -> "+"
| Minus -> "-"
| Times -> "*"
| Or -> "||"
and pp_some_cob_lst fmt = function
| (Some x, s) -> Format.fprintf fmt "%s %a" s pp_cob_lst x
| (None, _) -> Format.fprintf fmt ""
and pp_cob_lst fmt x =  list_comma fmt (x, pp_cob_var)

and pp_cob_var fmt x = Format.fprintf fmt ":%s" x.payload
and pp_some_rb_work_or_tran fmt = function
  | Some p ->  pp_rb_work_or_tran fmt p
  | None -> Format.fprintf fmt "" 
  
and pp_rb_work_or_tran fmt = function
  | Work -> Format.fprintf fmt "WORK"
  | Transaction -> Format.fprintf fmt "TRANSACTION"

and pp_rb_args fmt = function
  | Some Release -> Format.fprintf fmt "RELEASE"
  | Some To (variable) -> Format.fprintf fmt "TO SAVEPOINT %s" variable.payload
  | None -> Format.fprintf fmt ""

and pp_connect fmt c =
  match c with
  | Connect_to_idby {dbname ;db_conn_id ;username ;db_data_source; password} ->
    Format.fprintf fmt "TO %a %a USER %a USING %a IDENTIFIED BY %a"
    pp_lit dbname 
    pp_some_lit (db_conn_id, "AS" )
    pp_lit username 
    pp_lit db_data_source
    pp_lit password

  | Connect_to {db_data_source ;db_conn_id ;username ;password} ->
    Format.fprintf fmt "TO %a %a USER %a %a"
    pp_lit db_data_source 
    pp_some_lit (db_conn_id, "AS" )
    pp_lit username 
    pp_some_lit (password, "USING" )

  | Connect_using {db_data_source} -> 
    Format.fprintf fmt "USING %a"
    pp_lit db_data_source 

  | Connect_user{username; password; db_conn_id; db_data_source} ->
    Format.fprintf fmt "%a IDENTIFIED BY %a %a %a"
    pp_lit username 
    pp_lit password 
    pp_some_lit (db_conn_id, "AT" )
    pp_some_lit (db_data_source, "USING" )

  | Connect_reset (name)-> 
    Format.fprintf fmt "RESET%a" pp_some_lit (name, " " )

and pp_whenever_condtion fmt x =
  match x with
  | Not_found -> Format.fprintf fmt "NOT FOUND"
  | SqlError-> Format.fprintf fmt "SQLERROR"
  | SqlWarning-> Format.fprintf fmt "SQLWARNING"

and pp_whenever_continuation fmt x =
  match x with
  | Continue -> Format.fprintf fmt "CONTINUE"
  | Perform (label) -> Format.fprintf fmt "PERFORM %s" label.payload
  | Goto (stmt_label) -> Format.fprintf fmt "GOTO %s" stmt_label.payload

and pp_some_sql fmt = function
  | Some p ->  pp_sql fmt p
  | None -> Format.fprintf fmt "" 
and  pp_sql fmt x =  Format.fprintf fmt " %a " pp_sql_rec x

and pp_sql_rec fmt x =  List.iter (Format.fprintf fmt "%a " pp_one_token) x
and pp_one_token fmt = function
| Sql_instr(s) -> Format.fprintf fmt "%s" s
| Sql_var(c) -> Format.fprintf fmt "%a" pp_var c
| Sql_lit l -> Format.fprintf fmt "%a" pp_lit l
| Sql_query s -> Format.fprintf fmt "%a" pp_sql_query s
| Sql_equality e -> Format.fprintf fmt "%a" pp_sql_update_aux e
| Sql_search_condition c -> Format.fprintf fmt "%a" pp_sql_condition c

and pp_sql_query fmt (s, o) = 
  Format.fprintf fmt "SELECT %a %a" 
  pp_select_lst s 
  pp_select_options_lst o

and pp_select_options_lst fmt lst =
  let pp_one_option fmt = function 
  | From f -> Format.fprintf fmt "FROM %a" pp_from f
  | Where w -> Format.fprintf fmt "WHERE %a" pp_sql_condition w
  | OrderBy ob -> Format.fprintf fmt "ORDER BY %a" pp_orderBy ob
  | GroupBy gb-> Format.fprintf fmt "GROUP BY %a" pp_group_by gb
  in
  List.iter (Format.fprintf fmt "%a" pp_one_option) lst

and pp_from fmt f= list_comma fmt (f, pp_table_ref)

and pp_table_ref fmt = function
| FromLit l -> Format.fprintf fmt "%a" pp_lit l
| FromLitAs (l, a) -> Format.fprintf fmt "%a AS %a" pp_table_ref l pp_lit a
| FromSelect (s) -> Format.fprintf fmt "(%a)" pp_sql_query s
| Join (tr1, join, tr2, opt) ->
    Format.fprintf fmt "%a %s JOIN %a %a"
    pp_table_ref tr1
    (str_join join)
    pp_table_ref tr2
    pp_table_opt_option opt

and str_join = function
| InnerJoin -> "INNER" 
| NaturalJoin -> "NATURAL" 
| LeftJoin -> "LEFT" 
| RightJoin -> "RIGHT"

and pp_table_opt_option fmt = function
| Some w -> pp_table_opt fmt w
| None -> Format.fprintf fmt ""

and pp_table_opt fmt = function
| JoinOn sc -> Format.fprintf fmt "ON %a" pp_sql_condition sc
| JoinUsing lstvar -> 
    let pp_aux fmt x = list_comma fmt (x, pp_sql_var)
  in
  Format.fprintf fmt "USING %a" pp_aux lstvar


and pp_group_by fmt x = Format.fprintf fmt "GROUP BY %a ASC" pp_list_lit x

and pp_orderBy fmt x = 
  let pp_aux fmt =function
  | Asc v -> Format.fprintf fmt "ORDER BY %a ASC" pp_lit v 
  | Desc v -> Format.fprintf fmt "ORDER BY %a DESC" pp_lit v
  in 
  list_comma fmt (x, pp_aux)


and pp_select_lst fmt l = list_comma fmt (l, pp_complex_literal)

and pp_some_var fmt (x, s) =
  match x with
  | Some v -> Format.fprintf fmt "%s %a" s pp_var v
  | None -> Format.fprintf fmt ""
and pp_var fmt x =
  match x with
  | SqlVar v -> Format.fprintf fmt "%s" v.payload
  | CobolVar c -> Format.fprintf fmt ":%s" c.payload

and pp_some_lit fmt (x, s)= 
match x with
| Some v -> Format.fprintf fmt "%s %a" s pp_lit v
| None -> Format.fprintf fmt ""

and pp_list_lit fmt x = list_comma fmt (x, pp_lit)

and pp_lit fmt x = 
  match x with
  | LiteralNum n -> Format.fprintf fmt "%s" n.payload
  | LiteralStr n -> Format.fprintf fmt "%s" n.payload
  | LiteralVar n -> Format.fprintf fmt "%a" pp_var n
  | LiteralDot lst -> 
    let rec pp_aux fmt x = 
      match x with
      | [] -> Format.fprintf fmt ""
      | [ele] -> Format.fprintf fmt "%s" ele.payload
      | ele::t -> Format.fprintf fmt "%s.%a" ele.payload pp_aux t
    in
    pp_aux fmt lst