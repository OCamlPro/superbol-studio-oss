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

open Cobol_common                                          (* Srcloc, Visitor *)
open Cobol_common.Srcloc.INFIX
open Cobol_parser.Tokens

module TOKTYP = struct
  type t = { index: int; name: string }
  let all: t list ref = ref []
  let mk: string -> t =
    let idx = ref 0 in
    fun name ->
      let res = { index = !idx; name } in
      incr idx;
      all := res :: !all;
      res
  let type_     = mk "type"
  let operator  = mk "operator"
  let variable  = mk "variable"
  let function_ = mk "function"
  let parameter = mk "parameter"
  let keyword   = mk "keyword"
  let string    = mk "string"
  let number    = mk "number"
  let namespace = mk "namespace"
  let decorator = mk "decorator"
  (* let modifier  = mk "modifier" *)
  let comment   = mk "comment"
  (* "class"; *)
  (* "enum"; *)
  (* "interface"; *)
  (* "struct"; *)
  (* "typeParameter"; *)
  (* "property"; *)
  (* "enumMember"; *)
  (* "event"; *)
  (* "method"; *)
  let macro     = mk "macro"
  (* "regexp"; *)
  let all =
    List.sort (fun a b -> b.index - a.index) !all |>
    List.rev_map (fun a -> a.name)
end

module TOKMOD = struct
  type t = { mask: int; name: string }
  let all: t list ref = ref []
  let mk: string -> t =
    let mask = ref 0b1 in
    fun name ->
      let res = { mask = !mask; name } in
      mask := !mask lsl 1;
      all := res :: !all;
      res
  let declaration    = mk "declaration"
  let definition     = mk "definition"
  let readonly       = mk "readonly"
  let modification   = mk "modification"
  (*"static";
    "deprecated";
    "abstract";
    "async";
    "documentation";*)
  let all =
    List.sort (fun a b -> b.mask - a.mask) !all |>
    List.rev_map (fun a -> a.name)
  type set = { flags: int }
  let none = { flags = 0b0 }
  let one { mask; _ } = { flags = mask }
  let union: t list -> set =
    List.fold_left (fun set m -> { flags = set.flags lor m.mask }) none
end

type token_type = TOKTYP.t
type token_modifiers = TOKMOD.set

let token_types = TOKTYP.all
let token_modifiers = TOKMOD.all

type semtok = {
  line: int;
  start: int;
  length: int;
  toktyp: token_type;
  tokmods: token_modifiers;
}

let semtok ?(tokmods = TOKMOD.none) toktyp lexloc =
  let range = Lsp_position.range_of_lexloc lexloc in
  let line = range.start.line in
  let start = range.start.character in
  let length = range.end_.character - start in
  { line; start; length; toktyp; tokmods }

let single_line_lexlocs_in ~filename =
  Srcloc.shallow_single_line_lexlocs_in ~ignore_invalid_filename:true ~filename

let acc_semtoks ~filename ?range ?tokmods toktyp loc acc =
  List.fold_left begin fun acc lexloc -> match range with
    | Some r when not (Lsp_position.intersects_lexloc r lexloc) -> acc
    | _ -> semtok toktyp ?tokmods lexloc :: acc
  end acc @@ single_line_lexlocs_in ~filename loc

type token_category =
  | ProgramName
  | ParagraphName
  | ProcName
  | Parameter
  | DataDecl
  | DataLevel
  | Var
  | VarModif
  | ReportName
  | ExceptionName
  | MnemonicName
  | FileName


(* TODO: incrementally build a map that associates locations in filename with
   token types and modifiers, and then extract the (sorted) list at the end.  In
   this way, we don't need to worry about the order in which parse-tree elements
   are visited.  If really needed, the accumulator may carry some context
   information that can be used in generic methods like `fold_name'`. *)
let semtoks_from_ptree ~filename ?range ptree =
  let open Cobol_ptree.Visitor in
  (* let open Cobol_ptree.Terms_visitor in *)
  (* let open Cobol_ptree.Operands_visitor in *)
  let open Cobol_common.Visitor in

  let acc_semtoks category loc acc =
    let toktyp, tokmods = match category with
      | ProgramName -> TOKTYP.string, TOKMOD.(union [definition; readonly])
      | ParagraphName -> TOKTYP.function_, TOKMOD.(one definition)
      | ProcName -> TOKTYP.function_, TOKMOD.none
      | Parameter -> TOKTYP.parameter, TOKMOD.none
      | DataDecl -> TOKTYP.variable, TOKMOD.(one declaration)
      | DataLevel -> TOKTYP.decorator, TOKMOD.none
      | Var -> TOKTYP.variable, TOKMOD.none
      | VarModif -> TOKTYP.variable, TOKMOD.(one modification)
      | ReportName
      | ExceptionName
      | MnemonicName
      | FileName -> TOKTYP.variable, TOKMOD.(one readonly)
    in
    acc_semtoks ~filename ?range ~tokmods toktyp loc acc
  in
  let add_name' name category acc =
    acc_semtoks category ~@name acc
  in
  let rec add_qualname (qn: Cobol_ptree.qualname) toktyp acc =
    match qn with
    | Name name ->
        add_name' name toktyp acc
    | Qual (name, qn) ->
        add_name' name toktyp acc |>
        add_qualname qn toktyp
  in
  let add_ident (id: Cobol_ptree.ident) toktyp acc =
    match id with
    | QualIdent {ident_name; _} -> add_qualname ident_name toktyp acc
    | _ -> acc (* TODO *)
  in
  let add_ident' id = add_ident ~&id in
  let add_list add_fun l toktyp acc =
    List.fold_left (fun acc n -> add_fun n toktyp acc) acc l
  in
  let add_option add_fun v toktyp acc =
    match v with
    | None -> acc
    | Some v -> add_fun v toktyp acc
  in

  Cobol_ptree.Visitor.fold_compilation_group (object (self)
    inherit [semtok List.t] Cobol_ptree.Visitor.folder

    (* program-name *)
    method! fold_program_unit {program_name; _} acc = acc
      |> add_name' program_name ProgramName
      |> Visitor.do_children
    (* we call do_children, so we must ensure that
       the fold_name' does nothing,
       otherwise, there will be token overlap.

       Or we can override this method fold_program_unit to explicitly
       fold its every child and return Visitor.skip_children x.
       But by doing that for every method(which we need to override),
       we have to write a great amount of code... like rewriting
       the code of Cobol_ast.

    *)

    (*TODO: File/Report section*)

    method! fold_name' n acc =
      Visitor.skip_children @@ add_name' n Var acc

    (* data-name *)
    method! fold_data_name data_name acc =
      match data_name with
      | DataName n -> acc
          |> add_name' n DataDecl
          |> Visitor.skip_children
      | _ ->
          Visitor.do_children acc

    method! fold_rename_item {rename_level; rename_to;
                              rename_renamed; rename_through } acc = acc
      (*|> Visitor.do_children*)
      (* We can remove the code below and return do_children directly*)
      |> fold_data_level' self rename_level
      |> add_name' rename_to DataDecl
      |> Cobol_ptree.Visitor.fold_qualname' self rename_renamed
      |> Cobol_ptree.Visitor.fold_qualname'_opt self rename_through
      |> Visitor.skip_children

    method! fold_condition_name_item { condition_name_level;
                                       condition_name;
                                       condition_name_values;
                                       condition_name_alphabet;
                                       condition_name_when_false } acc = acc
      |> fold_data_level' self condition_name_level
      |> add_name' condition_name DataDecl
      |> fold_list ~fold:fold_condition_name_value self condition_name_values
      |> fold_name'_opt self condition_name_alphabet
      |> fold_literal_opt self condition_name_when_false
      |> Visitor.skip_children

    (* method! fold_data_clause dc acc = *)
    (*   match dc with *)
    (*   | DataRedefines name -> acc *)
    (*       |> add_name' name Var *)
    (*       |> Visitor.skip_children *)
    (*   | _ -> *)
    (*       Visitor.skip_children acc (\*Not implmented*\) *)

    (* data-level *)
    (* TODO: condition_name ??*)
    method! fold_data_level' dl acc = acc
      |> add_name' dl DataLevel
      |> Visitor.skip_children

    (* paragraph name *)
    method! fold_paragraph { paragraph_name; paragraph_is_section;
                             paragraph_segment; paragraph_sentences } acc =
      ignore paragraph_is_section; acc
      |> add_option add_name' paragraph_name ParagraphName
      (*|> Visitor.do_children*)
      |> fold_integer_opt self paragraph_segment
      |> fold_list ~fold:fold_statements' self paragraph_sentences
      |> Visitor.skip_children

    (* procedure using *)
    method! fold_using_by_reference { using_by_reference;
                                      using_by_reference_optional } acc = acc
      |> add_name' using_by_reference Parameter
      (*|> Visitor.do_children*)
      |> fold_bool self using_by_reference_optional
      |> Visitor.skip_children

    method! fold_using_clause = function
      | UsingByReference _ ->
          Visitor.do_children
      | UsingByValue l -> fun x -> x
        |> add_list add_name' l Parameter
        |> Visitor.skip_children

    (* inline call of function *)
    method! fold_inline_call { call_fun; call_args } acc = acc
      |> add_name' call_fun ProcName
      |> fold_list ~fold:fold_effective_arg self call_args
      |> Visitor.skip_children

    (* Statement *)
    (* distinguish
       1 variable
       2 variable modified
       3 procedure-name
       4 report-name/file-name/exception-name/mnemonic-name *)
    (*TODO: maybe finer analysis*)

    method! fold_accept' {payload = accept_stmt; _} acc =
      match accept_stmt with
      | AcceptFromDevice { item; device_item } -> acc
          |> add_name' device_item MnemonicName
          (*|> Visitor.do_children*)
          |> fold_ident' self item
          |> Visitor.skip_children
      | AcceptGeneric _
      | AcceptTemporal _
      | AcceptMsgCount _
      | AcceptAtScreen _
      | AcceptFromEnv _ ->
          Visitor.do_children acc

    method! fold_allocate' {payload = { allocate_kind;
                                        allocate_initialized;
                                        allocate_returning }; _ } acc = acc
      |> fold_allocate_kind self allocate_kind
      |> fold_bool self allocate_initialized
      |> add_option add_ident' allocate_returning VarModif
      |> Visitor.skip_children

    (*TODO: Alter *)

    method! fold_call' {payload = { call_prefix; call_using;
                                    call_returning;
                                    call_error_handler }; _} acc = acc
      |> fold_call_prefix self call_prefix
      |> fold_list ~fold:fold_call_using_clause' self call_using
      |> add_option add_ident' call_returning VarModif
      |> fold_option ~fold:fold_call_error_handler self call_error_handler
      |> Visitor.skip_children

    (*TODO: Cancel *)

    method! fold_close_phrase { close_item; close_format} acc = acc
      |> add_name' close_item FileName
      (*|> Visitor.do_children*)
      |> fold_option ~fold:fold_close_format self close_format
      |> Visitor.skip_children

    (* Add Compute Divide Multiply Subtract *)
    method! fold_rounded_ident { rounded_ident; rounded_rounding } acc = acc
      |> add_ident rounded_ident VarModif
      |> fold_rounding self rounded_rounding
      |> Visitor.skip_children

    method! fold_delete' {payload = { delete_targets; delete_retry;
                                      delete_on_invalid_key }; _} acc = acc
      |> add_name' delete_targets FileName
      (*|> Visitor.do_children*)
      |> fold_option ~fold:fold_retry_clause self delete_retry
      |> fold_dual_handler self delete_on_invalid_key
      |> Visitor.skip_children

    method! fold_display' { payload; _ } acc =
      Visitor.do_children_and_then acc @@ match payload with
      | DisplayDevice { upon = Some { payload = DisplayUponName n; _ }; _ } ->
          add_name' n MnemonicName
      | _ ->
          Fun.id

    (*TODO: Exit *)

    method! fold_free' names acc = acc
      |> add_list add_name' ~&names VarModif
      |> Visitor.skip_children

    method! fold_generate' name acc = acc
      |> add_name' ~&name VarModif
      |> Visitor.skip_children

    method! fold_goto' {payload = goto_target; _} acc = acc
      |> add_qualname goto_target ProcName
      |> Visitor.skip_children

    method! fold_goto_depending' {payload = { goto_depending_targets;
                                              goto_depending_on }; _} acc = acc
      |> add_list add_qualname goto_depending_targets ProcName
      |> fold_ident self goto_depending_on
      |> Visitor.skip_children

    method! fold_initialize' {payload = { init_items; init_filler; init_category;
                                          init_replacings; init_to_default }; _} acc = acc
      |> add_list add_ident init_items VarModif
      |> fold_bool self init_filler
      |> fold_option ~fold:fold_init_category self init_category
      |> fold_list ~fold:fold_init_replacing self init_replacings
      |> fold_bool self init_to_default
      |> Visitor.skip_children

    method! fold_initiate' { payload = names; _} acc = acc
      |> add_list add_name' names ReportName
      |> Visitor.skip_children

    method! fold_tallying { tallying_target; tallying_clauses } acc = acc
      |> add_qualname tallying_target.ident_name VarModif
      |> fold_list ~fold:fold_tallying_clause' self tallying_clauses
      |> Visitor.skip_children

    method! fold_inspect' { payload = { inspect_item; inspect_spec }; _} acc = acc
      |> add_ident inspect_item VarModif
      |> fold_inspect_spec self inspect_spec
      |> Visitor.skip_children

    (*TODO: Invoke *)

    method! fold_move' {payload = move_stmt; _} acc =
      match move_stmt with
      | MoveSimple { from; to_ } -> acc
          |> fold_ident_or_literal self from
          |> add_list add_ident to_ VarModif
          |> Visitor.skip_children
      | MoveCorresponding { from; to_ } -> acc
          |> fold_ident self from
          |> add_list add_ident to_ VarModif
          |> Visitor.skip_children

    method! fold_named_file_option {named_file_name;
                                    named_file_option} acc = acc
      |> add_name' named_file_name FileName
      (*|> Visitor.do_children*)
      |> fold_option ~fold:fold_file_option self named_file_option
      |> Visitor.skip_children

    method! fold_varying_phrase { varying_ident; varying_from;
                                  varying_by; varying_until } acc = acc
      |> add_ident varying_ident VarModif
      |> fold_ident_or_numlit self varying_from
      |> fold_option ~fold:fold_ident_or_numlit self varying_by
      |> fold_condition self varying_until
      |> Visitor.skip_children

    method! fold_raise' {payload = raise_stmt; _} acc =
      match raise_stmt with
      | RaiseIdent _ -> Visitor.do_children acc
      | RaiseException name -> acc
          |> add_name' name ExceptionName
          |> Visitor.skip_children

    method! fold_read' {payload = { read_file; read_direction;
                                    read_into; read_lock_behavior;
                                    read_lock; read_key;
                                    read_error_handler }; _} acc = acc
      |> add_name' read_file FileName
      |> fold_option ~fold:fold_read_direction self read_direction
      |> add_option add_ident read_into VarModif
      |> fold_option ~fold:fold_read_lock_behavior self read_lock_behavior
      |> fold_option ~fold:fold_bool self read_lock
      |> fold_option ~fold:fold_qualname self read_key
      |> fold_option ~fold:fold_read_error_handler self read_error_handler
      |> Visitor.skip_children

    (* TODO: RELEASE *)

    method! fold_resume' {payload = qn; _} acc = acc
      |> add_qualname qn ProcName
      |> Visitor.skip_children

    method! fold_return' {payload = {return_file; return_into;
                                     return_at_end}; _} acc = acc
      |> add_name' return_file FileName
      |> add_option add_ident' return_into VarModif
      |> fold_dual_handler self return_at_end
      |> Visitor.skip_children

    method! fold_write_target write_target acc =
      match write_target with
      | WriteTargetName _ -> Visitor.do_children acc
      | WriteTargetFile name -> acc
          |> add_name' name FileName
          |> Visitor.skip_children

    method! fold_search' { payload = { search_item;
                                       search_varying;
                                       search_at_end;
                                       search_when_clauses }; _ } acc = acc
      |> fold_qualname self search_item
      |> add_option add_ident search_varying VarModif
      |> fold_statements self search_at_end
      |> fold_list ~fold:fold_search_when_clause' self search_when_clauses
      |> Visitor.skip_children

    method! fold_set_switch_spec {set_switch_targets;
                                  set_switch_value } acc = acc
      |> add_list add_ident set_switch_targets MnemonicName (*TODO: ident??*)
      |> fold_on_off self set_switch_value
      |> Visitor.skip_children

    method! fold_set_condition_spec { set_condition_targets;
                                      set_condition_value } acc = acc
      |> add_list add_ident set_condition_targets VarModif
      |> fold_bool self set_condition_value
      |> Visitor.skip_children

    method! fold_set' {payload = set_stmt;_ } acc =
      match set_stmt with
      | SetAmbiguous { targets; set_method; value} -> acc
          |> add_list add_ident targets VarModif
          |> fold_set_ambiguous_method self set_method
          |> fold_expression self value
          |> Visitor.skip_children
      | SetSwitch _ (*TODO*)
      | SetCondition _
      | SetAttribute _
      | SetLocale _
      | SetSavedException ->
          Visitor.do_children acc
      | SetSaveLocale { target; locale } -> acc
          |> add_ident target VarModif
          |> fold_set_save_locale self locale
          |> Visitor.skip_children
      | SetFloatContent { targets; content; sign } -> acc
          |> add_list add_ident targets VarModif
          |> fold_float_content self content
          |> fold_option ~fold:fold_sign self sign
          |> Visitor.skip_children

    method! fold_start' {payload = {start_file; start_position;
                                    start_on_invalid_key }; _} acc = acc
      |> add_name' start_file FileName
      (*|> Visitor.do_children*)
      |> fold_option ~fold:fold_start_position self start_position
      |> fold_dual_handler self start_on_invalid_key
      |> Visitor.skip_children

    method! fold_string_stmt' {payload = {string_sources;
                                          string_target;
                                          string_pointer;
                                          string_on_overflow}; _} acc = acc
      |> fold_list ~fold:fold_string_source self string_sources
      |> add_ident string_target VarModif
      |> fold_option ~fold:fold_ident self string_pointer
      |> fold_dual_handler self string_on_overflow
      |> Visitor.skip_children

    method! fold_terminate' {payload = names; _} acc = acc
      |> add_list add_name' names ReportName
      |> Visitor.skip_children

    method! fold_unlock' {payload = { unlock_file; unlock_record }; _} acc = acc
      |> add_name' unlock_file FileName
      (*|> Visitor.do_children*)
      |> fold_bool self unlock_record
      |> Visitor.skip_children

    method! fold_unstring_target { unstring_target;
                                   unstring_target_delimiter;
                                   unstring_target_count} acc = acc
      |> add_ident unstring_target VarModif
      |> add_option add_ident unstring_target_delimiter VarModif
      |> add_option add_ident unstring_target_count VarModif
      |> Visitor.skip_children

    (*TODO: Validate *)
    (*TODO: Merge, Sort*)

    (* (\* All qualname not colored yet will be marked as normal variable *\) *)
    (* method! fold_qualname qn acc = acc *)
    (*   |> add_qualname qn Var *)
    (*   |> Visitor.skip_children *)

  end) ptree [] |> List.rev

let semtoks_of_comments ~filename ?range rev_comments =
  rev_comments |>
  List.fold_left begin fun acc -> function
    | Cobol_preproc.Text.{ comment_loc = s, _ as lexloc; _ }
      when s.Lexing.pos_fname = filename &&
           Option.fold range
             ~some:(fun r -> Lsp_position.intersects_lexloc r lexloc)
             ~none:true ->
        semtok TOKTYP.comment lexloc :: acc
    | _ ->
        acc
  end []

let semtoks_of_ignored ~filename ?range rev_ignored =
  (* Decorate like comments, for lack of a better suited token type in the set
     of available ones.  This could be improved later with some client-side code
     or configuration.  *)
  rev_ignored |>
  List.fold_left begin fun acc ((s, _ ) as lexloc) ->
    if s.Lexing.pos_fname = filename &&
       Option.fold range
         ~some:(fun r -> Lsp_position.intersects_lexloc r lexloc)
         ~none:true
    then semtok TOKTYP.comment lexloc :: acc
    else acc
  end []

let semtoks_of_preproc_statements ~filename ?range pplog =
  List.rev @@ List.fold_left begin fun acc -> function
    | Cobol_preproc.Trace.FileCopy { copyloc = loc; _ }
    | Cobol_preproc.Trace.Replace { replloc = loc }
    | Cobol_preproc.Trace.CompilerDirective { loc; _ } ->
        acc_semtoks ~filename ?range TOKTYP.macro loc acc
    | Cobol_preproc.Trace.Replacement _ ->
        acc
  end [] (Cobol_preproc.Trace.events pplog)

(** [semtoks_of_non_ambigious_tokens ~filename tokens] returns tokens that do
    not need to have more analyzing to get their type. *)
let semtoks_of_non_ambigious_tokens ~filename ?range tokens =
  List.rev @@ List.fold_left begin fun acc { payload = token; loc } ->
    let semtok_infos = match token with
      | WORD _ | WORD_IN_AREA_A _ -> None
      | ALPHANUM _ | ALPHANUM_PREFIX _ ->
          Some (TOKTYP.string, TOKMOD.none)
      | BOOLIT _
      | HEXLIT _ | NULLIT _
      | NATLIT _ | SINTLIT _
      | FIXEDLIT _ | FLOATLIT _
      | DIGITS _
      | EIGHTY_EIGHT ->
          Some (TOKTYP.number, TOKMOD.none)
      | PICTURE_STRING _ ->
          Some (TOKTYP.type_, TOKMOD.(one declaration))
      | AMPERSAND | ASTERISK | COLON | DASH_SIGN | DOUBLE_ASTERISK | DOUBLE_COLON
      | EQ | GE | GT | LE | LPAR | LT | NE | PLUS_SIGN | RPAR | SLASH ->
          Some (TOKTYP.operator, TOKMOD.none)
      | PARAGRAPH | STATEMENT | PROGRAM |SECTION | DIVISION ->
          Some (TOKTYP.namespace, TOKMOD.none)
      | _ ->
          Some (TOKTYP.keyword, TOKMOD.none)
    in
    match semtok_infos with
    | None ->
        acc
    | Some (toktyp, tokmods) ->
        acc_semtoks ~filename ?range ~tokmods toktyp loc acc
  end [] tokens

let compare_semtoks first second =
  let cmp = Stdlib.compare first.line second.line in
  if cmp = 0
  then Stdlib.compare first.start second.start
  else cmp

let relative_semtoks semtoks =
  let data = Array.make (5 * List.length semtoks) 0 in
  ignore @@ List.fold_left begin fun (i, prev_line, prev_start) semtok ->
    data.(5 * i + 0) <- semtok.line - prev_line;
    data.(5 * i + 1) <- semtok.start -
                        if semtok.line = prev_line then prev_start else 0;
    data.(5 * i + 2) <- semtok.length;
    data.(5 * i + 3) <- semtok.toktyp.index;
    data.(5 * i + 4) <- semtok.tokmods.flags;
    (succ i, semtok.line, semtok.start)
  end (0, 0, 0) semtoks;
  data

let ensure_sorted name ~filename cmp l =
  let rec unsorted_pair = function
    | [] | [_] -> None
    | x :: (y :: _ as tl) when cmp x y <= 0 -> unsorted_pair tl
    | x ::  y :: _ -> Some (x, y)
  in
  match unsorted_pair l with
  | None -> l
  | Some (x, y) ->
      Pretty.error "@[<2>** Internal@ note:@ semantic@ tokens@ in@ %s@ are@ \
                    not@ sorted.@ Two@ offenders@ are:@]@\n%a%a@." name
        Srcloc.pp_raw_loc (filename,
                           (x.line + 1, x.start),
                           (x.line + 1, x.start + x.length))
        Srcloc.pp_raw_loc (filename,
                           (y.line + 1, y.start),
                           (y.line + 1, y.start + y.length));
      List.fast_sort cmp l


let data ~filename ~range ~tokens ~pplog
    ~rev_comments ~rev_ignored ~ptree : int array =
  let semtoks1 = semtoks_of_non_ambigious_tokens ~filename ?range tokens in
  let semtoks2 = semtoks_from_ptree              ~filename ?range ptree in
  let semtoks3 = semtoks_of_comments             ~filename ?range rev_comments in
  let semtoks4 = semtoks_of_ignored              ~filename ?range rev_ignored in
  let semtoks5 = semtoks_of_preproc_statements   ~filename ?range pplog in
  (* NB: In *principle* all those lists are already sorted w.r.t lexical
     locations in [filename].  We just check that for now and raise a warning,
     in case. *)
  (* let semtoks1 = List.fast_sort compare_semtoks semtoks1 *)
  (* and semtoks2 = List.fast_sort compare_semtoks semtoks2 *)
  (* and semtoks3 = List.fast_sort compare_semtoks semtoks3 in *)
  let semtoks1 = ensure_sorted "nonambiguous" ~filename compare_semtoks semtoks1
  and semtoks2 = ensure_sorted "ptree"        ~filename compare_semtoks semtoks2
  and semtoks3 = ensure_sorted "comments"     ~filename compare_semtoks semtoks3
  and semtoks4 = ensure_sorted "ignored"      ~filename compare_semtoks semtoks4
  and semtoks5 = ensure_sorted "preproc"      ~filename compare_semtoks semtoks5 in
  relative_semtoks
    List.(merge compare_semtoks semtoks1 @@
          merge compare_semtoks semtoks2 @@
          merge compare_semtoks semtoks3 @@
          merge compare_semtoks semtoks4 @@ semtoks5)
