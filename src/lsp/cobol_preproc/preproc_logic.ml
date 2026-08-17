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


let on_define ~platform ~loc Compdir_tree.{ var; value; override } ~env =
  let open struct exception KEEP_UNDEFINED end in
  try
    let value =
      match ~&value with
      | Literal_definition Alphanum l ->
          ENV.alphanum_literal_value l
      | Literal_definition Boolean l ->
          ENV.boolean_literal_value l
      | Literal_definition Numeric l ->
          ENV.numeric_literal_value l
      | Parameter_definition ->                                (* [sys.getenv] *)
          let v = ENV.VAR.to_uppercase_string ~&var in
          match platform.getenv_opt v with
          | Some value ->
              let alphanum = Cobol_data.Value.alphanum_of_string value in
              Alphanum { src_payload = alphanum;
                         src = Process_environment }
          | None -> raise KEEP_UNDEFINED
    in
    OUT.result (ENV.define_preproc_var ~loc var value ~override env)
  with
  | KEEP_UNDEFINED ->
      OUT.result env
  | ENV.REDEFINITION { prev_def_src } ->
      OUT.result env ~diags:(redefinition ~loc var ~prev_def_src)


(* Conditionals *)


let compvar_eval ?def var =
  Preproc_trace.Variable_evaluation { loc = ~@var; var = ~&var; def }


let eval_term (term: Compdir_tree.term) env : (ENV.value, _) result * _ list =
  match term with
  | Variable var ->
      (match ENV.preproc_var_definition_of ~var env with
       | Ok def -> Ok def.src_payload.compvar_value, [compvar_eval var ~def]
       | Error `UNDEFINED as e -> e, [compvar_eval var])
  | Literal Alphanum a ->
      Ok (ENV.alphanum_literal_value a), []
  | Literal Boolean b ->
      Ok (ENV.boolean_literal_value b), []
  | Literal Numeric f ->
      Ok (ENV.numeric_literal_value f), []


exception TYPE_MISMATCH of ENV.value * ENV.value


type matching_operands =
  | Alpha of (Cobol_data.Types.alphanum_value as 'a) * 'a
  | Bool of (Cobol_data.Types.boolean_value as 'b) * 'b
  | Num of (Cobol_data.Types.fixed_value as 'c) * 'c


let operands (a: ENV.value) (b: ENV.value) : matching_operands =
  match a, b with
  | Alphanum a, Alphanum b -> Alpha (a.src_payload, b.src_payload)
  | Boolean a, Boolean b -> Bool (a.src_payload, b.src_payload)
  | Numeric a, Numeric b -> Num (a.src_payload, b.src_payload)
  | a, b -> raise @@ TYPE_MISMATCH (a, b)


let eval_condition ~(operator: Compdir_tree.condition_operator) a b =
  match operands a b, operator with
  | Alpha (a, b), Eq -> a = b
  | Alpha (a, b), Ne -> a <> b
  | Alpha (a, b), Le
  | Alpha (b, a), Ge -> String.compare a.str b.str <= 0
  | Alpha (a, b), Lt
  | Alpha (b, a), Gt -> String.compare a.str b.str < 0
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
  match ENV.preproc_var_definition_of ~try_compil_vars:false ~var env with
  | Ok def ->
      OUT.result (polarity, [compvar_eval var ~def])                  (* use! *)
  | Error `UNDEFINED ->
      OUT.result (not polarity, [compvar_eval var])                     (* use! *)

let eval_set_conditon ~loc var polarity env =
  let diags = Preproc_diagnostics.none in
  let item = Set_condition_directive { assumed_set = false } in
  let def =
    match ENV.preproc_var_definition_of ~try_compil_vars:false ~var env with
    | Ok def ->
        Some def
    | Error `UNDEFINED ->
        None
  in
  OUT.result (not polarity, [compvar_eval var ?def])
    ~diags:(warn diags @@ Ignored { loc; item })

let eval_value_condition ~loc var polarity env =
  let diags = Preproc_diagnostics.none in
  match ENV.preproc_var_definition_of ~var env with
  | Ok ({ src_payload = { compvar_value = Boolean b; _ }; _ } as def) ->
      OUT.result
        (Z.(equal zero) b.src_payload.bool_bits != polarity,
         [compvar_eval var ~def])
  | Ok ({ src_payload = { compvar_value = (Alphanum _ | Numeric _ as value); _ };
          _ } as def) ->
      let stuff = Variable_type_in_compdir_condition { value } in
      OUT.result ~diags:(warn_unexpected diags ~loc stuff)
        (false, [compvar_eval var ~def])
  | Error `UNDEFINED ->
      let stuff = Variable_in_compdir_condition { var } in
      OUT.result ~diags:(warn_undefined diags ~loc:~@var stuff)
        (false, [compvar_eval var])

let eval_constant_conditions ~loc l r polarity operator env =
  let l, log1 = eval_term l env
  and r, log2 = eval_term r env in
  let log = LIST.append log1 log2 in
  match l, r with
  | Error `UNDEFINED, Ok _
  | Ok _, Error `UNDEFINED
  | Error `UNDEFINED, Error `UNDEFINED ->
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
      eval_set_conditon ~loc:~@e var polarity env
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
