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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Cobol_common.Platform.TYPES
open Cobol_data.Types

open Preproc_diagnostics

module ENV = Preproc_env
module VAR = ENV.VAR
module OUT = Preproc_outputs
module NEL = Cobol_common.Basics.NEL
module LIST = Cobol_common.Basics.LIST

module TYPES = struct
  type context = frame list
  and frame =
    | If_condition of condition_frame
  and condition_frame =
    {
      condition: Compdir_tree.boolexpr with_loc;
      emitting: bool;
      if_loc: srcloc;
      else_loc: srcloc option;
    }
end
include TYPES

let empty_context: context = []

let top_context: context -> frame option = function
  | c :: _ -> Some c
  | [] -> None

let emitting: context -> bool = function
  | [] -> true
  | If_condition { emitting; _ } :: _ -> emitting


(* --- *)


let err t e = Preproc_diagnostics.add_error e t
let error e = err Preproc_diagnostics.none e
let warn t w = Preproc_diagnostics.add_warning w t
let warning w = warn Preproc_diagnostics.none w

let undefined ~loc var =
  warning @@ Undefine_of_unknown_env_variable { loc; var }

let redefinition ~loc var ~prev_def_src =
  warning @@ Redefinition_of_env_variable { loc; var; prev_def_src }

let unexpected ~loc stuff =
  error @@ Unexpected { loc; stuff }

let warn_unexpected t ~loc stuff =
  warn t @@ Unexpected_warning { loc; stuff }

let warn_undefined t ~loc stuff =
  warn t @@ Undefined_warning { loc; stuff }

(* >>DEFINE / >>SET *)


let on_define_off ~loc var ~(env: ENV.t) =
  if ENV.mem_preproc_var' var env
  then OUT.result (ENV.undefine_preproc_var var env)
  else OUT.result env ~diags:(undefined ~loc var)


let definition_value ~platform ~var ~(value: Compdir_tree.definition_value with_loc) =
  match ~&value with
  | Literal_definition Alphanum l ->
      Ok (ENV.alphanum_literal_value l)
  | Literal_definition Boolean l ->
      Ok (ENV.boolean_literal_value l)
  | Literal_definition Numeric l ->
      Ok (ENV.numeric_literal_value l)
  | Parameter_definition ->                                    (* [sys.getenv] *)
      match platform.getenv_opt @@ ENV.VAR.to_uppercase_string ~&var with
      | Some value ->
          Ok (Cobol_common.Srcloc.with_src ~src:Process_environment @@
              ENV.Alphanum (Cobol_data.Value.plain_alphanum value))
      | None ->
          Error `UNDEFINED


let ppvar_def def var =
  Preproc_trace.Variable_definition { loc = ~@var; var = ~&var;
                                      def = Preproc_var def }

let on_define ~platform ~loc Compdir_tree.{ var; value; override } ~env =
  match definition_value ~platform ~var ~value with
  | Error `UNDEFINED ->                                      (* keep undefined *)
      OUT.result (env, [])
  | Ok v ->
      try
        let env, def = ENV.define_preproc_var ~loc var v ~override env in
        OUT.result (env, [ppvar_def def var])
      with ENV.REDEFINITION { prev_def_src } ->
        OUT.result (env, []) ~diags:(redefinition ~loc var ~prev_def_src)


(* Conditionals *)


let var_eval ?def var =
  Preproc_trace.Variable_evaluation { loc = ~@var; var = ~&var; def }


let eval_term (term: Compdir_tree.term) env : (ENV.value, _) result * _ list =
  match term with
  | Variable var ->
      (match ENV.var_definition_of ~var env with
       | Ok (Preproc_var d as def) ->
           Ok d.src_payload.compvar_value.src_payload, [var_eval var ~def]
       | Ok (Compilation_var d as def) ->
           Ok d.src_payload.compvar_value.src_payload, [var_eval var ~def]
       | Error Undefined as e ->
           e, [var_eval var])
  | Literal Alphanum a ->
      Ok (ENV.alphanum_literal_value' a), []
  | Literal Boolean b ->
      Ok (ENV.boolean_literal_value' b), []
  | Literal Numeric f ->
      Ok (ENV.numeric_literal_value' f), []


exception TYPE_MISMATCH of ENV.value * ENV.value


type matching_operands =
  | Alpha of (Cobol_data.Types.alphanum_value as 'a) * 'a
  | Bool of (Cobol_data.Types.boolean_value as 'b) * 'b
  | Num of (Cobol_data.Types.fixed_value as 'c) * 'c


let operands (a: ENV.value) (b: ENV.value) : matching_operands =
  match a, b with
  | Alphanum a, Alphanum b -> Alpha (a, b)
  | Boolean a, Boolean b -> Bool (a, b)
  | Numeric a, Numeric b -> Num (a, b)
  | a, b -> raise @@ TYPE_MISMATCH (a, b)


let eval_condition ~(operator: Compdir_tree.condition_operator) a b =
  match operands a b, operator with
  | Alpha (a, b), Eq -> Cobol_data.Value.compare_alphanums a b = 0
  | Alpha (a, b), Ne -> Cobol_data.Value.compare_alphanums a b <> 0
  | Alpha (a, b), Le
  | Alpha (b, a), Ge -> Cobol_data.Value.compare_alphanums a b <= 0
  | Alpha (a, b), Lt
  | Alpha (b, a), Gt -> Cobol_data.Value.compare_alphanums a b < 0
  | Bool (a, b), Eq -> Z.equal a.bool_bits b.bool_bits
  | Bool (a, b), Ne -> not (Z.equal a.bool_bits b.bool_bits)
  | Bool (a, b), Le
  | Bool (b, a), Ge -> Z.leq a.bool_bits b.bool_bits
  | Bool (a, b), Lt
  | Bool (b, a), Gt -> Z.lt a.bool_bits b.bool_bits
  | Num (a, b), Eq -> Q.equal a b
  | Num (a, b), Ne -> not (Q.equal a b)
  | Num (a, b), Le
  | Num (b, a), Ge -> Q.leq a b
  | Num (a, b), Lt
  | Num (b, a), Gt -> Q.lt a b


let eval_defined_condition var polarity env =
  (* CHECKME: check whether DEFINED applies on 78-level items (compil. vars) *)
  match ENV.var_definition_of (* ~try_compil_vars:false *) ~var env with
  | Ok def ->
      OUT.result (polarity, [var_eval var ~def])                   (* var_use *)
  | Error Undefined ->
      OUT.result (not polarity, [var_eval var])                      (* var_use *)

let eval_set_condition ~loc var polarity env =
  let diags = Preproc_diagnostics.none in
  let def =
    (* CHECKME: check whether SET applies on 78-level items (compil. vars) *)
    match ENV.var_definition_of (* ~try_compil_vars:false *) ~var env with
    | Ok def ->
        Some def
    | Error Undefined ->
        None
  in
  let set, diags =
    match def with
    | None ->
        false, diags
    | Some Preproc_var def | Some Compilation_var def ->
        match def.src_payload.compvar_value.src_payload with
        | Boolean b ->
            not (Z.equal b.bool_bits Z.zero), diags
        | Alphanum _ | Numeric _ ->            (* CHECKME: not on non-booleans *)
            let item = Set_condition_directive { assumed_set = false } in
            false, warn diags @@ Ignored { loc; item }
  in
  OUT.result (set = polarity, [var_eval var ?def]) ~diags

let eval_value_condition ~loc var polarity env =
  let diags = Preproc_diagnostics.none in
  let[@local] var_value (Preproc_env.(Preproc_var d |
                                      Compilation_var d) as def) =
    match d.src_payload.compvar_value.src_payload with
    | Boolean b ->
        OUT.result (Z.(equal zero) b.bool_bits != polarity, [var_eval var ~def])
    | Alphanum _ | Numeric _ as value ->
        let stuff = Variable_type_in_compdir_condition { value } in
        OUT.result ~diags:(warn_unexpected diags ~loc stuff)
          (false, [var_eval var ~def])
  in
  match ENV.var_definition_of ~var env with
  | Ok def ->
      var_value def
  | Error Undefined ->
      let stuff = Variable_in_compdir_condition { var } in
      OUT.result ~diags:(warn_undefined diags ~loc:~@var stuff)
        (false, [var_eval var])

let eval_constant_conditions ~loc l r polarity operator env =
  let l, log1 = eval_term l env
  and r, log2 = eval_term r env in
  let log = LIST.append log1 log2 in
  match l, r with
  | Error Undefined, Ok _
  | Ok _, Error Undefined
  | Error Undefined, Error Undefined ->
      OUT.result (false, log)                             (* ignore undefined *)
  | Ok l, Ok r ->
      try
        OUT.result (eval_condition ~operator l r = polarity, log)
      with TYPE_MISMATCH (left, right) ->
        let diags = Preproc_diagnostics.none in
        let stuff = Types_in_compdir_condition { left; right } in
        let diags = warn diags @@ Incompatible { loc; stuff } in
        OUT.result ~diags (false, log)

let eval_boolexpr env
  : Compdir_tree.boolexpr with_loc -> (bool * _) OUT.with_diags = fun e ->
  match ~&e with
  | Defined_condition { var; polarity } ->
      eval_defined_condition var polarity env
  | Set_condition { var; polarity } ->
      eval_set_condition ~loc:~@e var polarity env
  | Value_condition { var; polarity } ->
      eval_value_condition ~loc:~@e var polarity env
  | Constant_condition { left_operand = l; right_operand = r;
                         polarity; operator } ->
      eval_constant_conditions ~loc:~@e l r polarity operator env


let on_if ~loc:if_loc ~condition ~env context =
  OUT.map_result (eval_boolexpr env condition)
    ~f:begin fun (cond, log) ->
      If_condition { condition; emitting = cond && emitting context;
                     if_loc; else_loc = None } :: context,
      log
    end


let on_else ~loc context : context OUT.with_diags =
  match context with
  | If_condition ({ else_loc = None; _ } as frame) :: parent_ctxt ->
      let emitting = not frame.emitting && emitting parent_ctxt in
      OUT.result (If_condition { frame with else_loc = Some loc;
                                            emitting } :: parent_ctxt)
  | If_condition { else_loc = Some _; if_loc = initial_if_loc; _ } :: _ ->
      let suggestion = EndIf_compiler_directive_missing { initial_if_loc } in
      OUT.result context
        ~diags:(unexpected ~loc @@
                Else_compiler_directive { suggestion = Some suggestion })
  | _ ->
      OUT.result context
        ~diags:(unexpected ~loc @@
                Else_compiler_directive { suggestion = None })


let on_elif ~loc ~condition ~env context : (context * _) OUT.with_diags =
  (* We always evaluate the condition to gather logs. *)
  (* CHECKME: whether that's useful... *)
  let eval_result = eval_boolexpr env condition in
  OUT.more_result eval_result ~f:begin fun (cond, log) ->
    match context with
    | If_condition ({ else_loc = None; _ } as frame) :: parent_ctxt ->
        let emitting = not frame.emitting && cond && emitting parent_ctxt in
        let context = If_condition { frame with emitting } :: parent_ctxt in
        OUT.result (context, log)
    | If_condition { else_loc = Some _; if_loc = initial_if_loc; _ } :: _ ->
        let suggestion = EndIf_compiler_directive_missing { initial_if_loc } in
        OUT.result (context, log)
          ~diags:(unexpected ~loc @@
                  Elif_compiler_directive { suggestion = Some suggestion })
    | _ ->
        OUT.result (context, log)
          ~diags:(unexpected ~loc @@
                  Elif_compiler_directive { suggestion = None })
  end

let on_endif ~loc : context -> context OUT.with_diags = function
  | If_condition _ :: context ->                                         (* pop *)
      OUT.result context
  | context ->
      OUT.result context ~diags:(unexpected ~loc EndIf_compiler_directive)


(* Misc. *)


let flush_contexts ~loc : context -> context * diagnostics =
  let rec flush_context diags = function
    | [] ->
        [], diags
    | If_condition { if_loc; _ } :: tl ->
        let stuff = If_compiler_directive { suggested_endif_loc = loc } in
        let error = Unterminated { loc = if_loc; stuff } in
        flush_context (add_error error diags) tl
  in
  flush_context Preproc_diagnostics.none


(* --- *)
