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
open Terms
open Misc_sections
open Env_division
open Data_descr
open Data_division
open Proc_division

type program_unit =
  {
    program_name: name_or_literal with_loc;
    program_as: strlit option;
    program_level: program_level;
    program_options: options_paragraph with_loc option;
    program_env: environment_division with_loc option;
    program_data: data_division with_loc option;
    program_proc: procedure_division with_loc option;
    program_end_name: name_or_literal with_loc option
  }

and program_level =
  | ProgramDefinition of
      {       (* Note: more general than before (allows nested prototypes): *)
        mode: program_mode option;
        has_identification_division_header: bool;
        preliminary_informational_paragraphs:
          informational_paragraphs         (* GC extension (before PROGRAM-ID) *)
          [@compare fun _ _ -> 0];
        supplementary_informational_paragraphs:
          informational_paragraphs
          [@compare fun _ _ -> 0];                         (* ~COB85, -COB2002 *)
        nested_programs: program_unit with_loc list;
      }
  | ProgramPrototype

and program_mode =
  {
    prog_is_common : bool;    (* only within a nested program *)
    prog_kind : program_kind with_loc option
  }

and program_kind =
  | Initial
  | Recursive
[@@deriving ord]

let pp_program_kind ppf = function
  | Initial ->
      Fmt.pf ppf "INITIAL"
  | Recursive ->
      Fmt.pf ppf "RECURSIVE"

let pp_program_mode ppf { prog_is_common;
                          prog_kind } =
  match prog_kind with
  | None when prog_is_common   -> Fmt.pf ppf "COMMON"
  | None                       -> ()
  | Some k when prog_is_common -> Fmt.pf ppf "%a COMMON" (pp_with_loc pp_program_kind) k;
  | Some k                     -> (pp_with_loc pp_program_kind) ppf k

let rec pp_program_unit ppf { program_name;
                              program_as;
                              program_level;
                              program_options;
                              program_env;
                              program_data;
                              program_proc;
                              program_end_name } =
  let has_identification_division_header,
      preliminary_info, supplementary_info,
      nested_programs,
      mode =
    match program_level with
    | ProgramDefinition { has_identification_division_header = p;
                          preliminary_informational_paragraphs = ip0;
                          supplementary_informational_paragraphs = ip1;
                          nested_programs;
                          mode } ->
        p, Some ip0, Some ip1, nested_programs, Some mode
    | ProgramPrototype ->
        false, None, None, [], None
  in
  if has_identification_division_header then
    Fmt.pf ppf "@[IDENTIFICATION@ DIVISION@].@\n";
  Fmt.(option pp_informational_paragraphs) ppf preliminary_info;
  Fmt.pf ppf "@[PROGRAM-ID.@ %a" (pp_with_loc pp_name_or_literal) program_name;
  Fmt.(option (any "@ AS " ++ pp_strlit)) ppf program_as;
  Fmt.(option (option (sp ++ pp_program_mode))
         ~none:(any "@ PROTOTYPE")) ppf mode;
  Fmt.pf ppf ".@]@\n";
  Fmt.(option pp_informational_paragraphs) ppf supplementary_info;
  Fmt.(option (sp ++ pp_with_loc pp_options_paragraph)) ppf program_options;
  Fmt.(option (sp ++ pp_with_loc pp_environment_division)) ppf program_env;
  Fmt.(option (sp ++ pp_with_loc pp_data_division)) ppf program_data;
  Fmt.(option (sp ++ pp_with_loc pp_procedure_division)) ppf program_proc;
  Fmt.(list ~sep:sp (pp_with_loc pp_program_unit)) ppf nested_programs;
  Fmt.(option (any "@ @[END PROGRAM@ " ++ pp_with_loc pp_name_or_literal ++ any ".@]"))
    ppf program_end_name

type function_unit =
  {
    function_name: name_or_literal with_loc;
    function_as: strlit option;
    function_is_proto: bool;
    function_options: options_paragraph with_loc option;
    function_env: environment_division with_loc option;
    function_data: data_division with_loc option;
    function_proc: procedure_division with_loc option;
    function_end_name: name_or_literal with_loc;
  }
[@@deriving ord]

let pp_id_paragraph
    ?(end_ = false) ?end_name
    popen pclose ppf pp_header header sections
  =
  Fmt.pf ppf "@[%s.%a@]" popen pp_header header;
  List.iter (Fmt.(option (sp ++ fun ppf fp -> fp ppf ())) ppf) sections;
  if end_ && Option.is_none end_name
  then Fmt.pf ppf "@ @[END %s.@]" pclose
  else Fmt.(option (any "@ @[END " ++ const string pclose ++
                    any " " ++ pp_with_loc pp_name_or_literal ++
                    any ".@]")) ppf end_name

let pp_function_id_paragraph ppf (n, fas, is_proto) =
  Fmt.pf ppf "%a%a%a"
    (pp_with_loc pp_name_or_literal) n
    Fmt.(option (any "@ AS " ++ pp_strlit)) fas
    (if is_proto then Fmt.any "@ PROTOTYPE" else Fmt.nop) ()

let pp_function_unit ppf
    { function_name = n; function_as = fas; function_is_proto = is_proto;
      function_options = opts; function_env = env; function_data = data;
      function_proc = proc; function_end_name = en }
  =
  pp_id_paragraph ~end_name:en "FUNCTION-ID" "FUNCTION" ppf
    Fmt.(sp ++ pp_function_id_paragraph ++ any ".") (n, fas, is_proto)
    Fmt.[ Option.map (const (pp_with_loc pp_options_paragraph)) opts;
          Option.map (const (pp_with_loc pp_environment_division)) env;
          Option.map (const (pp_with_loc pp_data_division)) data;
          Option.map (const (pp_with_loc pp_procedure_division)) proc ]

type method_definitions = method_definition with_loc list

and method_definition =
  {
    method_name: name with_loc;
    method_kind: method_kind;
    method_override: bool;
    method_final: bool;
    method_options: options_paragraph with_loc option;
    method_env: environment_division with_loc option;
    method_data: data_division with_loc option;
    method_proc: procedure_division with_loc option;
    method_end_name: name with_loc;
  }

and method_kind =
  | NamedMethod of { as_: strlit option }
  | PropertyMethod of { kind: property_kind }
[@@deriving ord]

let pp_method_kind ppf = function
  | NamedMethod { as_ } -> Fmt.(option (any "@ AS " ++ pp_strlit)) ppf as_
  | PropertyMethod { kind } -> pp_property_kind ppf kind

let pp_method_id_paragraph ppf (n, k, o, f) =
 Fmt.pf ppf "%a %a" pp_name' n pp_method_kind k;
  if o then Fmt.pf ppf " OVERRIDE";
  if f then Fmt.pf ppf " FINAL"

let pp_method_definition ppf
    { method_name = n; method_kind = k; method_override = o; method_final = f;
      method_options = opts; method_env = env; method_data = data;
      method_proc = proc; method_end_name = en } =
  let open Cobol_common.Srcloc.INFIX in
  pp_id_paragraph ~end_name:(Name en &@<- en) "METHOD-ID" "METHOD" ppf
    Fmt.(sp ++ pp_method_id_paragraph ++ any ".") (n, k, o, f)
    Fmt.[ Option.map (const (pp_with_loc pp_options_paragraph)) opts;
          Option.map (const (pp_with_loc pp_environment_division)) env;
          Option.map (const (pp_with_loc pp_data_division)) data;
          Option.map (const (pp_with_loc pp_procedure_division)) proc ]

type factory_definition = (* Note: could be merged with instance_definition *)
  {
    factory_implements: name with_loc list;
    factory_options: options_paragraph with_loc option;
    factory_env: environment_division with_loc option;
    factory_data: data_division with_loc option;
    factory_methods: method_definitions with_loc option;
  }
[@@deriving ord]

let pp_object_procedure_division: method_definitions Pretty.printer =
  Fmt.(any "PROCEDURE DIVISION." ++
       (list ~sep:nop (sp ++ pp_with_loc pp_method_definition)))

let pp_implements ppf = function
  | [] -> ()
  | names ->
      Fmt.(list ~sep:sp (any "IMPLEMENTS " ++ pp_name')) ppf names

let pp_factory_definition ppf
    { factory_implements = impl; factory_options = opts; factory_env = env;
      factory_data = data; factory_methods = meths } =
  pp_id_paragraph ~end_:true "FACTORY" "FACTORY" ppf
    pp_implements impl
    Fmt.[ Option.map (const (pp_with_loc pp_options_paragraph)) opts;
          Option.map (const (pp_with_loc pp_environment_division)) env;
          Option.map (const (pp_with_loc pp_data_division)) data;
          Option.map (const (pp_with_loc pp_object_procedure_division)) meths ]

type instance_definition =
  {
    instance_implements: name with_loc list;
    instance_options: options_paragraph with_loc option;
    instance_env: environment_division with_loc option;
    instance_data: data_division with_loc option;
    instance_methods: method_definitions with_loc option;
  }
[@@deriving ord]

let pp_instance_definition ppf
    { instance_implements = impl; instance_options = opts; instance_env = env;
      instance_data = data; instance_methods = meths } =
  pp_id_paragraph ~end_:true "OBJECT" "OBJECT" ppf
    pp_implements impl
    Fmt.[ Option.map (const (pp_with_loc pp_options_paragraph)) opts;
          Option.map (const (pp_with_loc pp_environment_division)) env;
          Option.map (const (pp_with_loc pp_data_division)) data;
          Option.map (const (pp_with_loc pp_object_procedure_division)) meths ]


type class_definition =
  {
    class_name: name_or_literal with_loc;
    class_as: strlit option;
    class_final: bool;
    class_inherits: name with_loc list;
    class_usings: name with_loc list;
    class_options: options_paragraph with_loc option;
    class_env: environment_division with_loc option;
    class_factory: factory_definition option;
    class_instance: instance_definition option;
    class_end_name: name_or_literal with_loc;
  }
[@@deriving ord]

let pp_class_id_paragraph ppf (cn, cas, f, inh, us) =
  pp_with_loc pp_name_or_literal ppf cn;
  Fmt.(option (any "@ AS " ++ pp_strlit)) ppf cas;
  if f
  then Fmt.pf ppf " FINAL";
  if inh != []
  then Fmt.pf ppf "@ INHERITS %a" Fmt.(list ~sep:sp pp_name') inh;
  if us != []
  then Fmt.pf ppf "@ USING %a" Fmt.(list ~sep:sp pp_name') inh;
  Fmt.pf ppf "."

let pp_class_definition ppf
    { class_name = cn; class_as = cas; class_final = f; class_inherits = inh;
      class_usings = us; class_options = opts; class_env = env;
      class_factory = fac; class_instance = inst; class_end_name = en }
  =
  pp_id_paragraph ~end_name:en "CLASS-ID" "CLASS" ppf
    Fmt.(sp ++ pp_class_id_paragraph) (cn, cas, f, inh, us)
    Fmt.[ Option.map (const (pp_with_loc pp_options_paragraph)) opts;
          Option.map (const (pp_with_loc pp_environment_division)) env;
          Option.map (const pp_factory_definition) fac;
          Option.map (const pp_instance_definition) inst ]


type interface_definition =
  {
    interface_name: name_or_literal with_loc;
    interface_as: strlit option;
    interface_inherits: name with_loc list;
    interface_usings: name with_loc list;
    interface_options: options_paragraph with_loc option;
    interface_env: environment_division with_loc option;
    interface_methods: method_definitions with_loc option;
    interface_end_name: name_or_literal with_loc;
  }
[@@deriving ord]

let pp_interface_id_paragraph ppf (n, a, inh, us) =
  Fmt.pf ppf "%a%a%a%a"
    (pp_with_loc pp_name_or_literal) n
    Fmt.(option (any "@ AS " ++ pp_strlit)) a
    Fmt.(if inh == [] then nop
         else any "@ INHERITS " ++ list ~sep:sp pp_name') inh
    Fmt.(if us == [] then nop
         else any "@ USING " ++ list ~sep:sp pp_name') us

let pp_interface_definition ppf
    { interface_name = n; interface_as = a; interface_inherits = inh;
      interface_usings = us; interface_options = opts; interface_env = env;
      interface_methods = meths; interface_end_name = en } =
  pp_id_paragraph ~end_name:en "INTERFACE-ID" "INTERFACE" ppf
    Fmt.(sp ++ pp_interface_id_paragraph ++ any ".") (n, a, inh, us)
    Fmt.[ Option.map (const (pp_with_loc pp_options_paragraph)) opts;
          Option.map (const (pp_with_loc pp_environment_division)) env;
          Option.map (const (pp_with_loc pp_object_procedure_division)) meths ]

type compilation_unit =
  | Program of program_unit
  | Function of function_unit
  | ClassDefinition of class_definition
  | InterfaceDefinition of interface_definition
[@@deriving ord]

let pp_compilation_unit ppf = function
  | Program pu -> pp_program_unit ppf pu
  | Function fu -> pp_function_unit ppf fu
  | ClassDefinition cd -> pp_class_definition ppf cd
  | InterfaceDefinition id -> pp_interface_definition ppf id

type control_division = unit                               (* nothing for now *)
[@@deriving ord]

type compilation_group =
  {
    control_division: control_division with_loc option;
    compilation_units: compilation_unit with_loc list;
  }
[@@deriving ord]

let pp_compilation_group ppf { control_division; compilation_units } =
  if control_division <> None
  then Fmt.pf ppf "CONTROL DIVISION.@;";
  Fmt.(list ~sep:sp (Fmt.vbox @@ pp_with_loc pp_compilation_unit))
    ppf compilation_units

let show_compilation_group = Fmt.to_to_string pp_compilation_group

(* TODO: this will work with references, but not for literals. We
   should make it work also for them, though we should probably wait
   to get an example from real life. This function should be moved
   somewhere else. *)
let program_name : name_or_literal -> string =
  function name_or_literal ->
  Pretty.to_string "%a" pp_name_or_literal name_or_literal

let program_name' : name_or_literal with_loc -> string with_loc =
  function name_or_literal ->
  let loc = name_or_literal.loc in
  let payload = program_name name_or_literal.payload in
  { loc ; payload }
