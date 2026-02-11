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

open Preproc_diagnostics

module ENV = Preproc_env
module VAR = ENV.VAR
module NEL = Cobol_common.Basics.NEL
module OUT = Preproc_outputs

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

let redefinition ~loc var ~prev_def_loc =
  warning @@ Redefinition_of_env_variable { loc; var; prev_def_loc }

let unexpected ~loc stuff =
  error @@ Unexpected { loc; stuff }

let warn_unexpected t ~loc stuff =
  warn t @@ Unexpected_warning { loc; stuff }

let warn_undefined t ~loc stuff =
  warn t @@ Undefined_warning { loc; stuff }

(* >>DEFINE / >>SET *)


let on_define_off ~loc var ~(env: ENV.t) =
  if ENV.mem' var env
  then OUT.result (ENV.undefine var env)
  else OUT.result env ~diags:(undefined ~loc var)


let on_define ~loc Compdir_tree.{ var; value; override } ~env =
  let open struct exception KEEP_UNDEFINED end in
  try
    let value = match ~&value with
      | Literal_definition Alphanum l ->
          ENV.Alphanum { pp_payload = ~&l;
                         pp_loc = Source_location ~@l }
      | Literal_definition Boolean l ->
          ENV.Boolean { pp_payload = ~&l.bool_value;
                        pp_loc = Source_location ~@l }
      | Literal_definition Numeric l ->
          ENV.Numeric { pp_payload = ~&l.fixed_value;
                        pp_loc = Source_location ~@l }
      | Parameter_definition ->                                (* [sys.getenv] *)
          let v = ENV.VAR.to_uppercase_string ~&var in
          match Sys.getenv_opt v with
          | Some value -> ENV.Alphanum { pp_payload = value;
                                         pp_loc = Process_environment }
          | None -> raise KEEP_UNDEFINED
    in
    OUT.result (ENV.define ~loc var value ~override env)
  with
  | KEEP_UNDEFINED ->
      OUT.result env
  | ENV.REDEFINITION { prev_def_loc } ->
      OUT.result env ~diags:(redefinition ~loc var ~prev_def_loc)


(* Conditionals *)


let eval_term: Compdir_tree.term -> ENV.t -> ENV.value = fun term env ->
  match term with
  | Variable var ->
      (ENV.definition_of ~var env).def_value
  | Literal Alphanum a ->
      Alphanum { pp_payload = ~&a;
                 pp_loc = Source_location ~@a }
  | Literal Boolean b ->
      Boolean { pp_payload = ~&b.bool_value;
                pp_loc = Source_location ~@b }
  | Literal Numeric f ->
      Numeric { pp_payload = ~&f.fixed_value;
                pp_loc = Source_location ~@f }


exception TYPE_MISMATCH of ENV.value * ENV.value


type matching_operands =
  | Alpha of (Cobol_data.Value.alphanum as 'a) * 'a
  | Bool of (Cobol_data.Value.boolean as 'b) * 'b
  | Num of (Cobol_data.Value.fixed as 'c) * 'c


let operands (a: ENV.value) (b: ENV.value) : matching_operands =
  match a, b with
  | Alphanum a, Alphanum b -> Alpha (a.pp_payload, b.pp_payload)
  | Boolean a, Boolean b -> Bool (a.pp_payload, b.pp_payload)
  | Numeric a, Numeric b -> Num (a.pp_payload, b.pp_payload)
  | a, b -> raise @@ TYPE_MISMATCH (a, b)


let eval_condition ~(operator: Compdir_tree.condition_operator) a b =
  match operands a b, operator with
  | Alpha (a, b), Eq -> a = b
  | Alpha (a, b), Ne -> a <> b
  | Alpha (a, b), Le
  | Alpha (b, a), Ge -> String.compare a b <= 0
  | Alpha (a, b), Lt
  | Alpha (b, a), Gt -> String.compare a b < 0
  | Bool (a, b), Eq -> Z.equal a.bool_value b.bool_value
  | Bool (a, b), Ne -> not (Z.equal a.bool_value b.bool_value)
  | Bool (a, b), Le
  | Bool (b, a), Ge -> Z.leq a.bool_value b.bool_value
  | Bool (a, b), Lt
  | Bool (b, a), Gt -> Z.lt a.bool_value b.bool_value
  | Num (a, b), Eq -> Q.equal a b
  | Num (a, b), Ne -> not (Q.equal a b)
  | Num (a, b), Le
  | Num (b, a), Ge -> Q.leq a b
  | Num (a, b), Lt
  | Num (b, a), Gt -> Q.lt a b


let eval_boolexpr env
  : Compdir_tree.boolexpr with_loc -> bool OUT.with_diags = fun e ->
  let diags = Preproc_diagnostics.none in
  match ~&e with
  | Defined_condition { var; polarity } ->
      OUT.result (ENV.mem' var env = polarity)
  | Set_condition { var = _; polarity } ->
      OUT.result (not polarity)
        ~diags:(warn diags (Ignored { loc = ~@e;
                                      item = Compiler_set_condition }))
            
  | Value_condition { var; polarity } ->
      begin
        match (ENV.definition_of ~var env).def_value with
        | Boolean b ->
            OUT.result (Z.(equal zero) b.pp_payload.bool_value != polarity)
        | Alphanum _ | Numeric _ as value ->
            let stuff = Variable_type_in_compdir_condition { value } in
            OUT.result ~diags:(warn_unexpected diags ~loc:~@e stuff) false
        | exception ENV.UNDEFINED var ->
            let stuff = Variable_in_compdir_condition { var } in
            OUT.result ~diags:(warn_undefined diags ~loc:~@var stuff) false
      end
  | Constant_condition { left_operand = l; right_operand = r;
                         polarity; operator } ->
      let l = try Ok (eval_term l env) with ENV.UNDEFINED _var -> Error `Undef
      and r = try Ok (eval_term r env) with ENV.UNDEFINED _var -> Error `Undef in
      begin
        try OUT.result @@ match l, r with
          | Ok l, Ok r -> eval_condition ~operator l r = polarity
          | Error `Undef, Ok _ | Ok _, Error `Undef
          | Error `Undef, Error `Undef -> false            (* ignore undefined *)
        with TYPE_MISMATCH (left, right) ->
          let stuff = Types_in_compdir_condition { left; right } in
          let diags = warn diags @@ Incompatible { loc = ~@e; stuff } in
          OUT.result ~diags false
      end


let on_if ~loc:if_loc ~condition ~env context =
  OUT.map_result (eval_boolexpr env condition)
    ~f:(fun cond -> If_condition { condition; emitting = cond && emitting context;
                                   if_loc; else_loc = None } :: context)


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


let on_elif ~loc ~condition ~env context : context OUT.with_diags =
  match context with
  | If_condition ({ else_loc = None; _ } as frame) :: parent_ctxt ->
      OUT.map_result (eval_boolexpr env condition)
        ~f:begin fun cond ->
          let emitting = not frame.emitting && cond && emitting parent_ctxt in
          If_condition { frame with emitting } :: parent_ctxt
        end
  | If_condition { else_loc = Some _; if_loc = initial_if_loc; _ } :: _ ->
      let suggestion = EndIf_compiler_directive_missing { initial_if_loc } in
      OUT.result context
        ~diags:(unexpected ~loc @@
                Elif_compiler_directive { suggestion = Some suggestion })
  | _ ->
      OUT.result context
        ~diags:(unexpected ~loc @@
                Elif_compiler_directive { suggestion = None })


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
