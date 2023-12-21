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

open PTree_types

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Terms_visitor
open Data_descr_visitor
open Misc_sections_visitor
open Data_division_visitor
open Proc_division_visitor

let todo    x = Cobol_common.Visitor.todo    __FILE__ x
let partial x = Cobol_common.Visitor.partial __FILE__ x

(* --- *)

class virtual ['a] folder = object
  inherit ['a] Misc_sections_visitor.folder
  inherit! ['a] Data_division_visitor.folder
  inherit! ['a] Proc_division_visitor.folder
  method fold_compilation_group: (compilation_group, 'a) fold = default
  method fold_control_division': (control_division with_loc, 'a) fold = default
  method fold_program_unit             : (program_unit                 , 'a) fold = default
  method fold_program_unit'            : (program_unit with_loc        , 'a) fold = default
  method fold_function_unit            : (function_unit                , 'a) fold = default
  method fold_function_unit'           : (function_unit with_loc       , 'a) fold = default
  method fold_method_definition        : (method_definition            , 'a) fold = default
  method fold_factory_definition       : (factory_definition           , 'a) fold = default
  method fold_instance_definition      : (instance_definition          , 'a) fold = default
  method fold_class_definition'        : (class_definition with_loc    , 'a) fold = default
  method fold_interface_definition'    : (interface_definition with_loc, 'a) fold = default
  method fold_compilation_unit'        : (compilation_unit with_loc    , 'a) fold = default
  (* Additonal methods: *)
  method fold_method_kind              : (method_kind                  , 'a) fold = default
  method fold_nested_programs          : (program_unit with_loc list   , 'a) fold = default
end

let todo    x = todo    __MODULE__ x
let partial x = partial __MODULE__ x

let fold_options_paragraph'_opt (v: _ #folder) =
  fold_option ~fold:fold_options_paragraph' v

let fold_environment_division'_opt (v: _ #folder) =
  fold_option ~fold:fold_environment_division' v

let fold_data_division'_opt (v: _ #folder) =
  fold_option ~fold:fold_data_division' v

let fold_procedure_division_opt (v: _ #folder) =
  fold_option ~fold:fold_procedure_division v

let fold_procedure_division'_opt (v: _ #folder) =
  fold_option ~fold:fold_procedure_division' v

let rec fold_program_unit (v: _ #folder) =
  handle v#fold_program_unit
    ~continue:begin fun { program_name; program_as; program_level;
                          program_options; program_env; program_data;
                          program_proc; program_end_name } x -> x
      >> (fun x -> match program_level with
          | ProgramPrototype -> x
              >> fold_name' v program_name
              >> fold_strlit_opt v program_as
          | ProgramDefinition { (* has_identification_division_header; *)
              preliminary_informational_paragraphs = infos0;
              supplementary_informational_paragraphs = infos1;
              kind; _ } -> ignore kind; x
              (* >> fold_bool v has_identification_division_header *)
              >> fold_informational_paragraphs v infos0
              >> fold_name' v program_name
              >> fold_strlit_opt v program_as
              >> fold_informational_paragraphs v infos1)
      >> fold_options_paragraph'_opt v program_options
      >> fold_environment_division'_opt v program_env
      >> fold_data_division'_opt v program_data
      >> fold_procedure_division'_opt v program_proc
      >> fold_program_level v program_level
      >> fold_name'_opt v program_end_name                  (* XXX: useful? *)
    end

and fold_nested_programs (v: _ #folder) =
  handle v#fold_nested_programs
    ~continue:(fold_with_loc_list v ~fold:fold_program_unit)

and fold_program_level (v: _ #folder) =
  (* handle v#fold_program_level *)
  (*   ~continue: *)begin function
      | ProgramPrototype -> Fun.id
      | ProgramDefinition { nested_programs; _ } ->
          fold_nested_programs v nested_programs
    end

let fold_program_unit' (v: _#folder) =
  handle' v#fold_program_unit' ~fold:fold_program_unit v

let fold_function_unit (v: _#folder) =
  handle v#fold_function_unit
    ~continue:begin fun { function_name; function_as; function_is_proto;
                          function_options; function_env; function_data;
                          function_proc; function_end_name } x -> x
      >> fold_name' v function_name
      >> fold_strlit_opt v function_as
      >> fold_bool v function_is_proto                      (* XXX: useful? *)
      >> fold_options_paragraph'_opt v function_options
      >> fold_environment_division'_opt v function_env
      >> fold_data_division'_opt v function_data
      >> fold_procedure_division_opt v function_proc
      >> fold_name' v function_end_name                     (* XXX: useful? *)
    end

let fold_function_unit' (v: _#folder) =
  handle' v#fold_function_unit' ~fold:fold_function_unit v

let fold_method_kind (v: _#folder) =
  handle v#fold_method_kind
    ~continue:begin function
      | NamedMethod { as_ } -> fold_strlit_opt v as_
      | PropertyMethod { kind } -> fold_property_kind v kind
    end

let fold_method_definition (v: _#folder) =
  handle v#fold_method_definition
    ~continue:begin fun { method_name; method_kind;
                          method_override; method_final;
                          method_options; method_env; method_data;
                          method_proc; method_end_name } x -> x
      >> fold_name' v method_name
      >> fold_method_kind v method_kind
      >> fold_bool v method_override
      >> fold_bool v method_final
      >> fold_options_paragraph'_opt v method_options
      >> fold_environment_division'_opt v method_env
      >> fold_data_division'_opt v method_data
      >> fold_procedure_division_opt v method_proc
      >> fold_name' v method_end_name                       (* XXX: useful? *)
    end

let fold_factory_definition (v: _#folder) =
  handle v#fold_factory_definition
    ~continue:begin fun { factory_implements; factory_options; factory_env;
                          factory_data; factory_methods } x -> x
      >> fold_name'_list v factory_implements
      >> fold_options_paragraph'_opt v factory_options
      >> fold_environment_division'_opt v factory_env
      >> fold_data_division'_opt v factory_data
      >> fold_option v factory_methods
        ~fold:(fold_with_loc_list ~fold:fold_method_definition)
    end

let fold_instance_definition (v: _#folder) =
  handle v#fold_instance_definition
    ~continue:begin fun { instance_implements; instance_options; instance_env;
                          instance_data; instance_methods } x ->
      ignore instance_env;
      partial __LINE__ "fold_instance_definition" ();
      x
      >> fold_name'_list v instance_implements
      >> fold_options_paragraph'_opt v instance_options
      >> fold_data_division'_opt v instance_data
      >> fold_option v instance_methods
        ~fold:(fold_with_loc_list ~fold:fold_method_definition)
    end

let fold_class_definition' (v: _#folder) =
  handle v#fold_class_definition'
    ~continue:(todo __LINE__ "fold_class_definition")

let fold_interface_definition' (v: _#folder) =
  handle v#fold_interface_definition'
    ~continue:(todo __LINE__ "fold_interface_definition")

let fold_compilation_unit' (v: _#folder) =
  handle v#fold_compilation_unit'
    ~continue:begin fun { payload; loc } -> match payload with
      | Program             d -> fold_program_unit' v (d &@ loc)
      | Function            d -> fold_function_unit' v (d &@ loc)
      | ClassDefinition     d -> fold_class_definition' v (d &@ loc)
      | InterfaceDefinition d -> fold_interface_definition' v (d &@ loc)
    end

let fold_control_division' (v: _#folder) =
  handle' v#fold_control_division'
    ~fold:(fun _ _ x -> x)

let fold_compilation_group (v: _#folder) =
  handle v#fold_compilation_group
    ~continue:begin fun { control_division; compilation_units } x -> x
      >> fold_option ~fold:(fold' ~fold:(fun _ _ -> Fun.id)) v control_division
      >> fold_list ~fold:fold_compilation_unit' v compilation_units
    end
