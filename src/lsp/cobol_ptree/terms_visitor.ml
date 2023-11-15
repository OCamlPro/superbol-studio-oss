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

open Common
open Numericals
open Terms

open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)

(* --- *)

class ['a] folder = object
  inherit ['a] Cobol_common.Visitor.Fold.folder
  method fold_name: (name, 'a) fold = default
  method fold_name': (name with_loc, 'a) fold = default
  method fold_ident: (ident, 'a) fold = default
  method fold_qualname: (qualname, 'a) fold = default
  method fold_qualident: (qualident, 'a) fold = default
  method fold_address: (address, 'a) fold = default
  method fold_counter: (counter, 'a) fold = default
  method fold_counter_kind: (counter_kind, 'a) fold = default
  method fold_inline_call: (inline_call, 'a) fold = default
  method fold_inline_invocation: (inline_invocation, 'a) fold = default
  method fold_effective_arg: (effective_arg, 'a) fold = default
  method fold_object_view: (object_view, 'a) fold = default
  method fold_object_view_spec: (object_view_spec, 'a) fold = default
  method fold_object_ref: (object_ref, 'a) fold = default
  method fold_subscript: (subscript, 'a) fold = default
  method fold_refmod: (refmod, 'a) fold = default
  method fold_sign: (sign, 'a) fold = default
  method fold_signz: (signz, 'a) fold = default
  method fold_boolean: (boolean, 'a) fold = default
  method fold_alphanum_string: (alphanum_string, 'a) fold = default
  method fold_alphanum: (alphanum, 'a) fold = default
  method fold_national: (national, 'a) fold = default
  (* method fold_strlit_figurative: (strlit_ figurative, 'a) fold = default *)
  (* method fold_intlit: (int_ term, 'a) fold = default *)
  method fold_int_figurative: (int_ figurative, 'a) fold = default
  method fold_any_figurative: 'k. ('k figurative, 'a) fold = default
  (* method fold_figurative: 'k. ('k figurative, 'a) fold = default *)
  (* method fold_numlit: (numlit, 'a) fold = default *)
  (* method fold_strlit: (strlit, 'a) fold = default *)
  (* method fold_literal: (literal, 'a) fold = default *)
  (* method fold_ident_or_alphanum: (ident_or_alphanum, 'a) fold = default *)
  (* method fold_ident_or_literal: (ident_or_literal, 'a) fold = default *)
  method fold_floating: (floating, 'a) fold = default
  method fold_fixed: (fixed, 'a) fold = default
  method fold_integer: (integer, 'a) fold = default
  method fold_integer': (integer with_loc, 'a) fold = default
  method fold_binop: (binop, 'a) fold = default
  method fold_unop: (unop, 'a) fold = default
  method fold_expr: (expression, 'a) fold = default
  method fold_class: (class_, 'a) fold = default
  method fold_cond: 'k. ('k cond, 'a) fold = default
  method fold_simple_cond: (simple_condition, 'a) fold = default
  method fold_flat_combined_relation: (flat_combined_relation, 'a) fold = default
  method fold_logop: (logop, 'a) fold = default
  method fold_relop: (relop, 'a) fold = default
  method fold_rounding_mode: (rounding_mode, 'a) fold = default
  method fold_rounding: (rounding, 'a) fold = default
  method fold_rounded_ident: (rounded_ident, 'a) fold = default
  (* method fold_constant_value: (constant_value, 'a) fold = default *)
  (* method fold_constant: (constant, 'a) fold = default *)
  (* method fold_constant': (constant with_loc, 'a) fold = default *)

end

let fold_name (v: _ #folder) = leaf v#fold_name
let fold_name_opt (v: _ #folder) = fold_option ~fold:fold_name v
let fold_name_list (v: _ #folder) = fold_list ~fold:fold_name v
let fold_name' (v: _ #folder) =
  handle' v#fold_name' v ~fold:fold_name

let fold_name'_opt (v: _ #folder) = fold_option ~fold:fold_name' v
let fold_name'_list (v: _ #folder) = fold_list ~fold:fold_name' v

let fold_binop (v: _ #folder) = leaf v#fold_binop
let fold_unop (v: _ #folder) = leaf v#fold_unop
let fold_logop (v: _ #folder) = leaf v#fold_logop
let fold_relop (v: _ #folder) = leaf v#fold_relop
let fold_sign (v: _ #folder) = leaf v#fold_sign
let fold_signz (v: _ #folder) = leaf v#fold_signz
let fold_counter_kind (v: _ #folder) = leaf v#fold_counter_kind

let fold_fixed (v: _ #folder) = leaf v#fold_fixed
let fold_floating (v: _ #folder) = leaf v#fold_floating
let fold_integer (v: _ #folder) = leaf v#fold_integer
let fold_integer_opt (v: _ #folder) = fold_option ~fold:fold_integer v
let fold_integer' (v: _ #folder) =
  handle' v#fold_integer' v ~fold:fold_integer
let fold_integer'_opt (v: _ #folder) = fold_option ~fold:fold_integer' v

let fold_boolean (v: _ #folder) = leaf v#fold_boolean

let fold_alphanum (v: _ #folder) =
  handle v#fold_alphanum ~continue:(fun (Alphanum s) ->
    handle v#fold_alphanum_string ~continue:(fun (s, _) ->
      fold_string v s) s)

let fold_national (v: _ #folder) =
  handle v#fold_national ~continue:(fun (National s) -> fold_string v s)

let fold_counter (v: _ #folder) =
  handle v#fold_counter
    ~continue:begin fun { counter_kind; counter_name } x -> x
      >> fold_counter_kind v counter_kind
      >> fold_name'_opt v counter_name
    end

let fold_object_ref (v: _ #folder) =
  handle v#fold_object_ref
    ~continue:begin function
      | ExceptionObject | Null | Self -> Fun.id
      | Super s -> fold_name'_opt v s
    end

let rec fold_literal (v: _ #folder) : literal -> 'a -> 'a = function
  | Boolean b -> fold_boolean v b
  | Fixed _
  | Floating _
  | Integer _ as n -> fold_numlit v n
  | National _ as n -> fold_national v n
  | NumFig f  -> fold_int_figurative v f
  | Fig f -> fold_any_figurative v f
  | Alphanum _
  | StrConcat _ as s -> fold_strlit v s
  | Concat _ as s -> fold_nonnumlit v s

and fold_intlit (v: _ #folder) : int_ term -> 'a -> 'a = function
  | Integer i -> fold_integer v i
  | NumFig f -> fold_int_figurative v f

and fold_numlit (v: _ #folder) : numlit -> 'a -> 'a = function
  | Fixed f -> fold_fixed v f
  | Floating f -> fold_floating v f
  | Integer _
  | NumFig _ as i -> fold_intlit v i

and fold_nonnumlit (v: _ #folder) : nonnumlit -> 'a -> 'a = function
  | Alphanum _ as a -> fold_alphanum v a
  | Boolean b -> fold_boolean v b
  | National _ as n -> fold_national v n
  | Fig f -> fold_any_figurative v f
  | StrConcat _ as s -> fold_strlit v s
  | Concat _ as n -> fold_nonnumlit v n

and fold_int_figurative (v: _ #folder) =
  leaf v#fold_int_figurative

and fold_any_figurative (v: _ #folder) =
  handle v#fold_any_figurative
    ~continue:begin function
      | Zero | Space | Quote | LowValue | HighValue -> Fun.id
      | All n -> fold_nonnumlit v n
    end

and fold_strlit (v: _ #folder) : strlit -> 'a -> 'a = function
  | Alphanum _ as a -> fold_alphanum v a
  | National _ as n -> fold_national v n
  | Fig f -> fold_any_figurative v f
  | StrConcat (s, s') -> fun x -> x >> fold_strlit v s >> fold_strlit v s'

and fold_ident (v: _ #folder) =
  handle v#fold_ident
    ~continue:begin function
      | Address ai -> fold_address v ai
      | Counter c -> fold_counter v c
      | InlineCall fi -> fold_inline_call v fi
      | InlineInvoke ii -> fold_inline_invocation v ii
      | ObjectView ov -> fold_object_view v ov
      | ObjectRef po -> fold_object_ref v po
      | QualIdent qi -> fold_qualident v qi
      | RefMod (i, r) -> fun x -> x
        >> fold_ident v (UPCAST.base_ident_with_refmod i)
        >> fold_refmod v r
    end

and fold_qualident (v: _ #folder) =
  handle v#fold_qualident
    ~continue:begin fun { ident_name; ident_subscripts } x -> x
      >> fold_qualname v ident_name
      >> fold_list ~fold:fold_subscript v ident_subscripts
    end

and fold_qualname (v: _ #folder) =
  handle v#fold_qualname
    ~continue:begin function
      | Name n -> fold_name' v n
      | Qual (n, qn) -> fun x -> x >> fold_name' v n >> fold_qualname v qn
    end

and fold_subscript (v: _ #folder) =
  handle v#fold_subscript
    ~continue:begin function
      | SubSAll ->
          Fun.id
      | SubSExpr e ->
          fold_expr v e
      | SubSIdx (n, s, l) ->
          fun x -> x >> fold_name' v n >> fold_sign v s >> fold_integer v l
    end

and fold_refmod (v: _ #folder) =
  handle v#fold_refmod
    ~continue:begin fun { refmod_left; refmod_length } x -> x
      >> fold_expr v refmod_left
      >> fold_option ~fold:fold_expr v refmod_length
    end

and fold_address (v: _ #folder) =
  handle v#fold_address
    ~continue:begin function
      | DataAddress i -> fold_ident v i
      | ProgAddress i -> fold_ident_or_literal v i
    end

and fold_inline_call (v: _ #folder) =
  handle v#fold_inline_call
    ~continue:begin fun { call_fun; call_args } x -> x
      >> fold_name' v call_fun
      >> fold_list ~fold:fold_effective_arg v call_args
    end

and fold_inline_invocation (v: _ #folder) =
  handle v#fold_inline_invocation
    ~continue:begin fun { invoke_class; invoke_meth; invoke_args } x -> x
      >> fold_ident v invoke_class
      >> fold_literal v invoke_meth
      >> fold_list ~fold:fold_effective_arg v invoke_args
    end

and fold_effective_arg (v: _ #folder) =
  handle v#fold_effective_arg
    ~continue:begin function
      | ArgExpr e -> fold_expr v e
      | ArgOmitted -> Fun.id
    end

and fold_object_view (v: _ #folder) =
  handle v#fold_object_view
    ~continue:begin fun { object_view_ident; object_view_spec } x -> x
      >> fold_ident v object_view_ident
      >> fold_object_view_spec v object_view_spec
    end

and fold_object_view_spec (v: _ #folder) =
  handle v#fold_object_view_spec
    ~continue:begin function
      | ObjViewAmbiguous n
      | ObjViewFactory n
      | ObjViewOnly n
      | ObjViewFactoryOnly n -> fold_name' v n
      | ObjViewUniversal -> Fun.id
    end

and fold_expr (v: _ #folder) =
  handle v#fold_expr
    ~continue:begin fun e x -> match e with
      | Atom a -> x >> fold_ident_or_literal v a
      | Unop (o, e) -> x
          >> fold_unop v o
          >> fold_expr v e
      | Binop (e, o, e') -> x
          >> fold_expr v e
          >> fold_binop v o
          >> fold_expr v e'
    end

and fold_ident_or_literal (v: _ #folder) : ident_or_literal -> 'a -> 'a = function
  | Address _
  | Counter _
  | InlineCall _
  | InlineInvoke _
  | ObjectView _
  | ObjectRef _
  | QualIdent _
  | RefMod _ as i -> fold_ident v i
  | Alphanum _
  | Boolean _
  | Fixed _
  | Floating _
  | Integer _
  | National _
  | NumFig _
  | Fig _
  | StrConcat _
  | Concat _ as l -> fold_literal v l

let fold_class (v: _ #folder) =
  handle v#fold_class
    ~continue:begin function
      | AlphabetOrClass n -> fold_name' v n
      | Alphabetic
      | AlphabeticLower
      | AlphabeticUpper
      | ClassBoolean
      | FarthestFromZero
      | FloatInfinity
      | FloatNotANumber
      | FloatNotANumberQuiet
      | FloatNotANumberSignaling
      | InArithmeticRange
      | NearestToZero
      | ClassNumeric -> Fun.id
    end

let rec fold_cond: type k. _ #folder -> k cond -> _ = fun v ->
  handle v#fold_cond
    ~continue:begin fun (c: k cond) x -> match c with
      | Expr _ | Omitted _ | Relation _
      | Abbrev _ | ClassCond _ | SignCond _ as c -> x
          >> fold_simple_cond v c
      | Not c -> x
          >> fold_cond v c
      | Logop (c, l, d) -> x
          >> fold_cond v c
          >> fold_logop v l
          >> fold_cond v d
    end

and fold_simple_cond (v: _ #folder) =
  handle v#fold_simple_cond
    ~continue:begin fun c x -> match c with
      | Expr e | Omitted e -> x
          >> fold_expr v e
      | Relation rel -> x
          >> fold_binary_relation v rel
      | Abbrev (_n, rel, o, comb) -> x
          >> fold_binary_relation v rel
          >> fold_logop v o
          >> fold_flat_combined_relation v comb
      | ClassCond (e, c) -> x
          >> fold_expr v e
          >> fold_class v c
      | SignCond (e, s) -> x
          >> fold_expr v e
          >> fold_signz v s
    end

and fold_binary_relation (v: _ #folder) (e, r, f) x = x
  >> fold_expr v e
  >> fold_relop v r
  >> fold_expr v f

and fold_flat_combined_relation (v: _ #folder) =
  handle v#fold_flat_combined_relation
    ~continue:begin fun c x -> match c with
      | FlatAmbiguous (r, e) -> x
          >> fold_option ~fold:fold_relop v r
          >> fold_expr v e
      | FlatNotExpr e -> x
          >> fold_expr v e
      | FlatRel (neg, rel) -> x
          >> fold_bool v neg
          >> fold_binary_relation v rel
      | FlatOther c -> x
          >> fold_cond v c
      | FlatComb (c1, o, c2) -> x
          >> fold_flat_combined_relation v c1
          >> fold_logop v o
          >> fold_flat_combined_relation v c2
    end

let fold_expression = fold_expr                                      (* alias *)
let fold_condition = fold_cond                                       (* alias *)

let fold_ident_or_alphanum (v: _ #folder) : ident_or_alphanum -> 'a -> 'a = function
  | Alphanum _ as a -> fold_alphanum v a
  | Address _
  | Counter _
  | InlineCall _
  | InlineInvoke _
  | ObjectView _
  | ObjectRef _
  | QualIdent _
  | RefMod _ as i -> fold_ident v i

let fold_ident_or_intlit (v: _ #folder) : ident_or_intlit -> 'a -> 'a = function
  | Address _
  | Counter _
  | InlineCall _
  | InlineInvoke _
  | ObjectView _
  | ObjectRef _
  | QualIdent _
  | RefMod _ as i -> fold_ident v i
  | Integer _ | NumFig _ as i -> fold_intlit v i

let fold_ident_or_numlit (v: _ #folder) : ident_or_numlit -> 'a -> 'a = function
  | Address _
  | Counter _
  | InlineCall _
  | InlineInvoke _
  | ObjectView _
  | ObjectRef _
  | QualIdent _
  | RefMod _ as i -> fold_ident v i
  | Fixed _ | Floating _
  | Integer _ | NumFig _ as i -> fold_numlit v i

let fold_ident_or_nonnum (v: _ #folder) : ident_or_nonnum -> 'a -> 'a = function
  | Address _
  | Counter _
  | InlineCall _
  | InlineInvoke _
  | ObjectView _
  | ObjectRef _
  | QualIdent _
  | RefMod _ as i -> fold_ident v i
  | Alphanum _
  | Boolean _
  | National _
  | Concat _
  | StrConcat _
  | Fig _ as f -> fold_literal v f

let fold_ident_or_strlit (v: _ #folder) : ident_or_strlit -> 'a -> 'a = function
  | Address _
  | Counter _
  | InlineCall _
  | InlineInvoke _
  | ObjectView _
  | ObjectRef _
  | QualIdent _
  | RefMod _ as i -> fold_ident v i
  | Alphanum _
  | National _
  | Fig _
  | StrConcat _ as s -> fold_strlit v s

let fold_qualname_or_alphanum (v: _ #folder) : qualname_or_alphanum -> _ = function
  | Name _ | Qual _ as qn -> fold_qualname v qn
  | Alphanum _ as a -> fold_alphanum v a

let fold_qualname_or_intlit (v: _ #folder) : qualname_or_intlit -> _ = function
  | Name _ | Qual _ as qn -> fold_qualname v qn
  | Integer _ | NumFig _ as i -> fold_intlit v i

let fold_qualname' (v: _ #folder) = fold' ~fold:fold_qualname v
let fold_qualname_opt (v: _ #folder) = fold_option ~fold:fold_qualname v
let fold_qualname'_opt (v: _ #folder) = fold_option ~fold:fold_qualname' v
let fold_strlit_opt (v: _ #folder) = fold_option ~fold:fold_strlit v
let fold_literal_opt (v: _ #folder) = fold_option ~fold:fold_literal v
let fold_ident' (v: _ #folder) = fold' ~fold:fold_ident v
let fold_ident'_opt (v: _ #folder) = fold_option ~fold:fold_ident' v

let fold_rounding_mode (v: _ #folder) =
  leaf v#fold_rounding_mode

let fold_rounding (v: _ #folder) =
  handle v#fold_rounding
    ~continue:begin function
      | RoundingMode m -> fold_rounding_mode v m
      | RoundingNotAny | RoundingDefault -> Fun.id
    end

let fold_rounded_ident (v: _ #folder) =
  handle v#fold_rounded_ident
    ~continue: begin fun { rounded_ident; rounded_rounding } x -> x
      >> fold_ident v rounded_ident
      >> fold_rounding v rounded_rounding
    end

let fold_rounded_idents (v: _ #folder) =
  fold_list ~fold:fold_rounded_ident v
