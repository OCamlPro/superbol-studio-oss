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

(** Non-branching statements *)

open Common
open Terms
open Operands


(*
ALLOCATE exp CHARACTERS INIT? (RET id)?
ALLOCATE id INIT? (RET id)?
*)

(* ALLOCATE *)
type allocate_stmt =
  {
    allocate_kind: allocate_kind;
    allocate_initialized: bool;
    allocate_returning: ident with_loc option;
  }
[@@deriving ord]

and allocate_kind =
  | AllocateCharacters of expression (*arith exp that evaluates to int (may be rounded)*)
  | AllocateDataItem of name with_loc
[@@deriving ord]

let pp_allocate_kind ppf = function
  | AllocateCharacters e ->
    Fmt.pf ppf "%a@ CHARACTERS" pp_expression e
  | AllocateDataItem n ->
    pp_name ppf n.payload

let pp_allocate_stmt ppf
  { allocate_kind = ak; allocate_initialized = ai; allocate_returning = ar }
=
  Fmt.pf ppf "ALLOCATE@ %a" pp_allocate_kind ak;
  if ai then Fmt.pf ppf "@ INITIALIZED";
  Option.iter (Fmt.pf ppf "@ RETURNING@ %a" (pp_with_loc pp_ident)) ar

(*
ALTER id TO PROCEED? id
*)

(* ALTER *)
type alter_stmt =
  alter_operands with_loc list
[@@deriving ord]

and alter_operands =
  {
    alter_source: procedure_name with_loc;
    alter_target: procedure_name with_loc;
  }
[@@deriving ord]

let pp_alter_operands ppf { alter_source; alter_target } =
  Fmt.pf ppf "%a@ TO@ %a"
    pp_procedure_name' alter_source
    pp_procedure_name' alter_target

let pp_alter_stmt ppf aos =
  Fmt.(pf ppf "ALTER@ %a" (list ~sep:sp (pp_with_loc pp_alter_operands)) aos)

(* CLOSE *)
type close_stmt = close_phrase list [@@deriving ord]
and close_phrase =
  {
    close_item: name with_loc;
    close_format: close_format option;
  }
[@@deriving ord]

and close_format =
  | CloseUnitReel of bool (* for removal *)
  | CloseWithLock
  | CloseWithNoRewind
[@@deriving ord]

let pp_close_format ppf = function
  | CloseUnitReel for_removal ->
    Fmt.pf ppf "REEL";
    if for_removal then Fmt.pf ppf "@ FOR@ REMOVAL"
  | CloseWithLock -> Fmt.pf ppf "WITH@ LOCK"
  | CloseWithNoRewind -> Fmt.pf ppf "With@ NO@ REWIND"

let pp_close_phrase ppf { close_item; close_format } =
  pp_with_loc pp_name ppf close_item;
  Fmt.(option (sp ++ pp_close_format)) ppf close_format

let pp_close_stmt ppf s =
  Fmt.(pf ppf "CLOSE@ %a" (list ~sep:sp pp_close_phrase) s)

(* ENTER *)
type enter_stmt =
  {
    enter_language: name with_loc;
    enter_routine: name with_loc option;
  }
[@@deriving ord]

let pp_enter_stmt ppf { enter_language = lang; enter_routine = rout } =
  Fmt.pf ppf "ENTER@ %a%a"
    (pp_with_loc pp_name) lang
    Fmt.(option (sp ++ pp_with_loc pp_name)) rout

(* ENTRY *)
type entry_by_clause =
  | EntryByReference of name with_loc list
  | EntryByValue of name with_loc list
[@@deriving ord]

type entry_stmt =
  | EntrySimple of alphanum with_loc
  | EntryUsing of
    {
      entry_name: alphanum with_loc;
      entry_by_clauses: entry_by_clause list;
    }
  | EntryForGoTo of alphanum with_loc
[@@deriving ord]

let pp_entry_by_clause ppf = function
  | EntryByReference ns ->
      Fmt.pf ppf "BY REFERENCE %a" Fmt.(list ~sep:sp pp_name') ns
  | EntryByValue ns ->
      Fmt.pf ppf "BY VALUE %a" Fmt.(list ~sep:sp pp_name') ns

let pp_entry_stmt ppf = function
  | EntrySimple name ->
    Fmt.pf ppf "ENTRY@ %a" (pp_with_loc pp_alphanum) name
  | EntryUsing { entry_name; entry_by_clauses } ->
    Fmt.pf ppf "ENTRY@ %a%a"
      (pp_with_loc pp_alphanum) entry_name
      Fmt.(list ~sep:nop (sp ++ pp_entry_by_clause)) entry_by_clauses
  | EntryForGoTo entry_name ->
    Fmt.pf ppf "ENTRY@ FOR GO TO %a"
      (pp_with_loc pp_alphanum) entry_name

(* RETURNING/GIVING x *)
type returning =
  | ReturningScalar of scalar
  | ReturningAddress of qualident
  | ReturningInt of
    {
      value : literal;
      size : literal option;
    }
[@@deriving ord]

let pp_returning ppf = function
  | ReturningScalar s ->
    pp_scalar ppf s
  | ReturningAddress n ->
    Fmt.pf ppf "RETURNING@ ADDRESS@ OF@ %a"
    pp_qualident n
  | ReturningInt { value = v; size = None } ->
    Fmt.pf ppf "RETURNING@ %a" pp_literal v
  | ReturningInt { value = v; size = Some v' } ->
    Fmt.pf ppf "RETURNING@ %a WITH SIZE %a"
    pp_literal v pp_literal v'

type program_exit_status =
  | ExitDefault
  | ExitReturning of returning
  | ExitRaising of raising
[@@deriving ord]

let pp_program_exit_status ppf = function
  | ExitDefault -> ()
  | ExitReturning x -> Fmt.(sp ++ pp_returning) ppf x
  | ExitRaising r -> Fmt.(sp ++ pp_raising) ppf r

(* EXIT *)
type exit_stmt =
  | ExitSimple
  | ExitProgram of program_exit_status
  | ExitMethod of raising option
  | ExitFunction of raising option
  | ExitPerform of bool
  | ExitParagraph
  | ExitSection
[@@deriving ord]

let pp_exit_stmt ppf = function
  | ExitSimple -> Fmt.pf ppf "EXIT"
  | ExitProgram s ->
    Fmt.pf ppf "EXIT PROGRAM%a" pp_program_exit_status s
  | ExitMethod ro ->
    Fmt.pf ppf "EXIT METHOD%a" Fmt.(option (sp ++ pp_raising)) ro
  | ExitFunction ro ->
    Fmt.pf ppf "EXIT FUNCTION%a" Fmt.(option (sp ++ pp_raising)) ro
  | ExitPerform cycle ->
    Fmt.pf ppf "EXIT PERFORM";
    if cycle then Fmt.pf ppf " CYCLE";
  | ExitParagraph -> Fmt.pf ppf "EXIT PARAGRAPH"
  | ExitSection -> Fmt.pf ppf "EXIT SECTION"

(* INITIALIZE *)
type initialize_stmt =
  {
    init_items: ident list;
    init_filler: bool;
    init_category: init_category option;
    init_replacings: init_replacing list;
    init_to_default: bool;
  }
[@@deriving ord]

and init_category =
  | InitAll
  | InitCategory of init_data_category
[@@deriving ord]

and init_replacing =
  {
    init_replacing_category: init_data_category;
    init_replacing_replacement_item: ident_or_literal;
  }
[@@deriving ord]

and init_data_category =
  | InitCategoryAlphabetic
  | InitCategoryAlphanumeric
  | InitCategoryAlphanumericEdited
  | InitCategoryBoolean
  | InitCategoryDataPointer
  | InitCategoryFunctionPointer
  | InitCategoryProcedurePointer
  | InitCategoryNational
  | InitCategoryNationalEdited
  | InitCategoryNumeric
  | InitCategoryNumericEdited
  | InitCategoryObjectReference
  | InitCategoryProgramPointer
[@@deriving ord]

let pp_init_data_category ppf = function
  | InitCategoryAlphabetic -> Fmt.pf ppf "ALPHABETIC"
  | InitCategoryAlphanumeric -> Fmt.pf ppf "ALPHANUMERIC"
  | InitCategoryAlphanumericEdited -> Fmt.pf ppf "ALPHANUMERIC-EDITED"
  | InitCategoryBoolean -> Fmt.pf ppf "BOOLEAN"
  | InitCategoryDataPointer -> Fmt.pf ppf "DATA-POINTER"
  | InitCategoryFunctionPointer -> Fmt.pf ppf "FUNCTION-POINTER"
  | InitCategoryProcedurePointer -> Fmt.pf ppf "PROCEDURE-POINTER"
  | InitCategoryNational -> Fmt.pf ppf "NATIONAL"
  | InitCategoryNationalEdited -> Fmt.pf ppf "NATIONAL-EDITED"
  | InitCategoryNumeric -> Fmt.pf ppf "NUMERIC"
  | InitCategoryNumericEdited -> Fmt.pf ppf "NUMERIC-EDITED"
  | InitCategoryObjectReference -> Fmt.pf ppf "OBJECT-REFERENCE"
  | InitCategoryProgramPointer -> Fmt.pf ppf "PROGRAM-POINTER"

let pp_init_category ppf = function
  | InitAll -> Fmt.pf ppf "ALL"
  | InitCategory dc -> pp_init_data_category ppf dc

let pp_init_replacing ppf { init_replacing_category = c; init_replacing_replacement_item = ri } =
  Fmt.pf ppf "%a@ DATA@ BY@ %a" pp_init_data_category c pp_ident_or_literal ri

let pp_initialize_stmt ppf
  { init_items = items
  ; init_filler = filler
  ; init_category = cat
  ; init_replacings = r
  ; init_to_default = to_default }
=
  Fmt.pf ppf "INITIALIZE@ %a%t%a"
    Fmt.(list ~sep:sp pp_ident) items
    (if filler then Fmt.fmt "@ FILLER" else Fmt.fmt "")
    Fmt.(option (sp ++ pp_init_category ++ sp ++ const string "VALUE")) cat;
  if r != [] then
    Fmt.pf ppf "@ REPLACING@ %a" Fmt.(list ~sep:sp pp_init_replacing) r;
  if to_default then
    Fmt.pf ppf "@ DEFAULT"



(* INVOKE *)
type invoke_stmt =
  {
    invoke_target: ident;
    invoke_method: ident_or_strlit;
    invoke_using: call_using_clause with_loc list;
    invoke_returning: ident with_loc option;
    (* error_htypeler: error_htypeler;  (* seen on IBM Cobol *)  *)
  }
[@@deriving ord]

let pp_invoke_stmt ppf
  { invoke_target = t; invoke_method = m; invoke_using = u; invoke_returning = r}
=
  Fmt.pf ppf "INVOKE@ %a@ %a" pp_ident t pp_ident_or_strlit m;
  if u != [] then
    Fmt.pf ppf "@ USING@ %a"
      Fmt.(list ~sep:sp (pp_with_loc pp_call_using_clause)) u;
  Fmt.(option (sp ++ pp_with_loc pp_ident)) ppf r


(* INSPECT *)
type inspect_stmt =
  {
    inspect_item: ident;
    inspect_spec: inspect_spec;
  }
[@@deriving ord]

and inspect_spec =
  | InspectTallying of tallying list
  | InspectReplacing of replacing list
  | InspectBoth of tallying list * replacing list
  | InspectConverting of converting
[@@deriving ord]

and tallying =
  {
    tallying_target: qualident;
    tallying_clauses: tallying_clause with_loc list;
  }
[@@deriving ord]

and tallying_clause =
  | TallyingCharacters of inspect_where list
  | TallyingRange of tallying_range * tallying_spec list
[@@deriving ord]

and tallying_range =
  | TallyAll
  | TallyLeading
[@@deriving ord]

and tallying_spec =
  {
    tallying_item: ident_or_nonnum;
    tallying_where: inspect_where list;
  }
[@@deriving ord]

and replacing = replacing_clause with_loc
[@@deriving ord]

and replacing_clause =
  | ReplacingCharacters of
      {
        replacement: ident_or_nonnum;
        where: inspect_where list;
      }
  | ReplacingRange of replacing_range * replacing_range_spec list
[@@deriving ord]

and replacing_range =
  | ReplaceAll
  | ReplaceLeading
  | ReplaceFirst
[@@deriving ord]

and replacing_range_spec =
  {
    replacing_item: ident_or_nonnum;
    replacing_by: ident_or_nonnum;
    replacing_where: inspect_where list;
  }
[@@deriving ord]

and converting =
  {
    converting_from: ident_or_nonnum;
    converting_to: ident_or_nonnum;
    converting_where: inspect_where list;
  }
[@@deriving ord]

and inspect_where = inspect_direction * inspect_reference
[@@deriving ord]

and inspect_direction =
  | InspectAfter
  | InspectBefore
[@@deriving ord]

and inspect_reference = ident_or_nonnum nel
[@@deriving ord]

let pp_replacing_range ppf = function
  | ReplaceAll -> Fmt.pf ppf "ALL"
  | ReplaceLeading -> Fmt.pf ppf "LEADING"
  | ReplaceFirst -> Fmt.pf ppf "FIRST"

let pp_inspect_direction ppf = function
  | InspectAfter -> Fmt.pf ppf "AFTER"
  | InspectBefore -> Fmt.pf ppf "BEFORE"

let pp_inspect_where =
  Fmt.(pair
    ~sep:sp pp_inspect_direction
      Pretty.(NEL.pp ~fopen:(Simple.string " ")
                     ~fsep:(Pretty.Simple.string "@ OR@ ")
                     ~fclose:(Simple.string " ")
                      pp_ident_or_nonnum))

let pp_converting ppf {
  converting_from = cf; converting_to = ct; converting_where = cw
} =
  Fmt.pf ppf "CONVERTING@ %a@ TO@ %a%a"
    pp_ident_or_nonnum cf
    pp_ident_or_nonnum ct
    Fmt.(if cw == [] then any "" else sp ++ list ~sep:sp pp_inspect_where) cw

let pp_replacing_range_spec ppf {
  replacing_item = ri; replacing_by = rb; replacing_where = rw } =
  Fmt.pf ppf "%a@ BY@ %a%a"
    pp_ident_or_nonnum ri
    pp_ident_or_nonnum rb
    Fmt.(if rw == [] then any "" else sp ++ list ~sep:sp pp_inspect_where) rw

let pp_replacing_clause ppf = function
  | ReplacingCharacters { replacement; where } ->
    Fmt.pf ppf "CHARACTERS@ BY@ %a%a"
      pp_ident_or_nonnum replacement
      Fmt.(if where == [] then any "" else sp ++ list ~sep:sp pp_inspect_where) where
  | ReplacingRange (rr, rrss) ->
    Fmt.pf ppf "%a@ %a"
      pp_replacing_range rr
      Fmt.(list ~sep:sp pp_replacing_range_spec) rrss

let pp_replacing = pp_with_loc pp_replacing_clause

let pp_tallying_range ppf = function
  | TallyAll -> Fmt.pf ppf "ALL"
  | TallyLeading -> Fmt.pf ppf "LEADING"

let pp_tallying_spec ppf { tallying_item = ti; tallying_where = tw } =
  Fmt.pf ppf "%a%a"
    pp_ident_or_nonnum ti
    Fmt.(if tw == [] then any "" else sp ++ list ~sep:sp pp_inspect_where) tw

let pp_tallying_clause ppf = function
  | TallyingCharacters iws ->
    Fmt.pf ppf "CHARACTERS@ %a"
      Fmt.(list ~sep:sp pp_inspect_where) iws
  | TallyingRange (r, tss) ->
    Fmt.pf ppf "%a@ %a"
      pp_tallying_range r
      Fmt.(list ~sep:sp pp_tallying_spec) tss

let pp_tallying ppf { tallying_target = tt; tallying_clauses = tcs } =
  Fmt.pf ppf "%a@ FOR@ %a"
    pp_qualident tt
    Fmt.(list ~sep:sp (pp_with_loc pp_tallying_clause)) tcs

let pp_inspect_spec ppf = function
  | InspectTallying ts ->
    Fmt.pf ppf "TALLYING@ %a" Fmt.(list ~sep:sp pp_tallying) ts
  | InspectReplacing rs ->
    Fmt.pf ppf "REPLACING@ %a"
      Fmt.(list ~sep:sp (pp_with_loc pp_replacing_clause)) rs
  | InspectBoth (ts, rs) ->
    Fmt.pf ppf "TALLYING@ %a@ REPLACING@ %a"
      Fmt.(list ~sep:sp pp_tallying) ts
      Fmt.(list ~sep:sp (pp_with_loc pp_replacing_clause)) rs
  | InspectConverting c -> pp_converting ppf c

let pp_inspect_stmt ppf { inspect_item = ii; inspect_spec = is } =
  Fmt.pf ppf "INSPECT@ %a@ %a" pp_ident ii pp_inspect_spec is

(* MERGE *)
type merge_stmt =
  {
    merge_file: name with_loc;
    merge_keys: Data_descr.sort_spec list;
    merge_collating: alphabet_specification option;
    merge_using: name with_loc list;
    merge_target: merge_or_sort_target;
  }
[@@deriving ord]

and merge_or_sort_target =
  | OutputProcedure of procedure_name with_loc procedure_range
  | Giving of name with_loc list
[@@deriving ord]

let pp_merge_or_sort_target ppf = function
  | OutputProcedure pr ->
    Fmt.pf ppf "OUTPUT@ PROCEDURE@ IS@ %a"
      (pp_procedure_range pp_procedure_name') pr
  | Giving ns ->
    Fmt.pf ppf "GIVING@ %a" Fmt.(list ~sep:sp (pp_with_loc pp_name)) ns

let pp_merge_stmt ppf { merge_file = mf;
                        merge_keys = mk;
                        merge_collating = mc;
                        merge_using = mu;
                        merge_target = mt;
                      } =
  Fmt.pf ppf "MERGE@ %a%a%a%a@ %a"
    (pp_with_loc pp_name) mf
    Fmt.(
      if mk == [] then
        any ""
      else
        sp ++ list ~sep:sp (const string "ON" ++ sp ++ Data_descr.pp_sort_spec )
    ) mk
    Fmt.(option (
        sp ++ const words "COLLATING SEQUENCE" ++ sp ++
        pp_alphabet_specification)
      ) mc
    Fmt.(
      if mu == [] then
        any ""
      else
        sp ++ const string "USING " ++ box (list ~sep:sp (pp_with_loc pp_name))
    ) mu
    pp_merge_or_sort_target mt


(* MOVE *)
type move_stmt =              (* TODO: maybe split in two distinct statements *)
  | MoveSimple of
      {
        from: scalar;
        to_: ident list;
      }
  | MoveCorresponding of
      {
        from: ident;
        to_: ident list;
      }
[@@deriving ord]

let pp_move_stmt ppf = function
  | MoveSimple { from; to_ } ->
    Fmt.pf ppf "MOVE@ %a@ TO@ %a"
      pp_scalar from
      Fmt.(list ~sep:sp pp_ident) to_
  | MoveCorresponding { from; to_ } ->
    Fmt.pf ppf "MOVE CORRESPONDING@ %a@ TO@ %a"
      pp_ident from
      Fmt.(list ~sep:sp pp_ident) to_


(* OPEN *)
type open_stmt = open_phrase list [@@deriving ord]
and open_phrase =
  {
    open_mode: open_mode;
    open_sharing: sharing_mode option;
    open_retry: retry_clause option;
    open_files: named_file_option list;
  }
[@@deriving ord]

and named_file_option =
  {
    named_file_name: name with_loc;
    named_file_option: file_option option;
  }
[@@deriving ord]

let pp_named_file_option ppf { named_file_name = nfn; named_file_option = nfo } =
  pp_with_loc pp_name ppf nfn;
  Fmt.(option (sp ++ pp_file_option)) ppf nfo

let pp_open_phrase ppf { open_mode = om; open_sharing = oso;
                         open_retry = oro; open_files = ofl } =
  pp_open_mode ppf om;
  Fmt.(option (sp ++ pp_sharing_mode)) ppf oso;
  Fmt.(option (sp ++ pp_retry_clause)) ppf oro;
  Fmt.pf ppf "@ %a" Fmt.(list ~sep:sp pp_named_file_option) ofl

let pp_open_stmt =
  Fmt.(const string "OPEN" ++ sp ++ list ~sep:sp pp_open_phrase)

(* RELEASE *)
type release_stmt =
  {
    release_item: name with_loc;
    release_from: ident_or_literal option;
  }
[@@deriving ord]

let pp_release_stmt ppf { release_item = ri; release_from = rf } =
  Fmt.pf ppf "RELEASE@ %a%a"
    (pp_with_loc pp_name) ri
    Fmt.(option (sp ++ const string "FROM" ++ sp ++ pp_ident_or_literal)) rf


(* SEND *)
type send_stmt =
  {
    send_name: name with_loc;
    send_operands: send_operands;
  }
[@@deriving ord]

and send_operands =
  | SendSimple of
      {
        from: ident;
      }
  | SendWith of
      {
        from: ident option;
        ending_indicator: message_ending_indicator;
        advancing: advancing_phrase option;
        replace: bool;
      }
[@@deriving ord]

and message_ending_indicator =
  | EndingIndicator of ident
  | EndingIndicatorESI
  | EndingIndicatorEMI
  | EndingIndicatorEGI
[@@deriving ord]

let pp_message_ending_indicator ppf = function
  | EndingIndicator i -> pp_ident ppf i
  | EndingIndicatorESI -> Fmt.pf ppf "ESI"
  | EndingIndicatorEMI -> Fmt.pf ppf "EMI"
  | EndingIndicatorEGI -> Fmt.pf ppf "EGI"

let pp_send_operands ppf = function
  | SendSimple { from } ->
    Fmt.pf ppf "FROM@ %a" pp_ident from
  | SendWith { from = f; ending_indicator = ei; advancing = apo; replace } ->
    Fmt.(option (const string "FROM" ++ sp ++ pp_ident ++ sp)) ppf f;
    pp_message_ending_indicator ppf ei;
    Fmt.(option (sp ++ pp_advancing_phrase)) ppf apo;
    if replace then Fmt.pf ppf "@ REPLACING LINE"

let pp_send_stmt ppf { send_name; send_operands } =
  Fmt.pf ppf "SEND@ %a@ %a"
    (pp_with_loc pp_name) send_name
    pp_send_operands send_operands


(* SET *)

  (*
 SET ADDR/id... UPBY/DOWNBY/TO id/exp/int

 SET (id... TO ON/OFF)...
 SET (id... TO TRUE/FALSE)...

 SET id ATTRIBUTE
 SET id TO LOCALE

 SET LOCALE
 SET LAST EXCEPTION
 SET CONTENT
 *)
type set_stmt =
  | SetAmbiguous of
      {
        targets: ident list;
        set_method: set_ambiguous_method;
        value: expression;
      }
  | SetSwitch of
      set_switch_spec list
  | SetCondition of
      set_condition_spec list
  | SetEntry of {
      targets : ident list ;
      value : ident_or_nonnum ;
    }
  | SetAttribute of
      {
        name: name with_loc;
        attribute_switches: set_attribute_switch list;
      }
  | SetSaveLocale of
      {
        target: ident;
        locale: set_save_locale;
      }
  | SetLocale of
      {
        target: set_locale_target;
        source: set_locale_source;
      }
  | SetSavedException
  | SetFloatContent of
      {
        targets: ident list;
        content: float_content;
        sign: sign option;
      }
  | SetEnvironment of
      {
        variable: ident_or_literal;
        value: ident_or_literal;
      }
[@@deriving ord]

and set_switch_spec =
  {
    set_switch_targets: ident list;
    set_switch_value: on_off;
  }
[@@deriving ord]

and set_condition_spec =
  {
    set_condition_targets: ident list;
    set_condition_value: bool;
  }
[@@deriving ord]

let pp_set_condition_spec ppf { set_condition_targets; set_condition_value } =
  Fmt.(list ~sep:sp pp_ident) ppf set_condition_targets;
  Fmt.pf ppf "@ TO@ ";
  Fmt.pf ppf (if set_condition_value then "TRUE" else "FALSE")

let pp_set_switch_spec ppf { set_switch_targets; set_switch_value } =
  Fmt.(list ~sep:sp pp_ident) ppf set_switch_targets;
  Fmt.pf ppf "@ TO@ ";
  pp_on_off ppf set_switch_value

let pp_set_stmt ppf ss =
  Fmt.pf ppf "SET@ ";
  match ss with
  | SetAmbiguous { targets; set_method; value } ->
    Fmt.pf ppf "%a@ %a@ %a"
      Fmt.(list ~sep:sp pp_ident) targets
      pp_set_ambiguous_method set_method
      pp_expression value
  | SetSwitch sssl ->
    Fmt.(list ~sep:sp pp_set_switch_spec) ppf sssl
  | SetCondition scsl ->
    Fmt.(list ~sep:sp pp_set_condition_spec) ppf scsl
  | SetAttribute { name = n; attribute_switches = ass } ->
    Fmt.pf ppf "%a@ %a"
      (pp_with_loc pp_name) n
      Fmt.(list ~sep:sp pp_set_attribute_switch) ass
  | SetSaveLocale { target; locale } ->
    Fmt.pf ppf "%a@ %a"
      pp_ident target pp_set_save_locale locale
  | SetLocale { target; source } ->
    Fmt.pf ppf "LOCALE@ %a@ TO@ %a"
      pp_set_locale_target target
      pp_set_locale_source source
  | SetSavedException ->
    Fmt.words ppf "LAST EXCEPTION TO OFF"
  | SetFloatContent { targets; content; sign } ->
    Fmt.pf ppf "CONTENT@ %a@ TO@ %a%a"
      Fmt.(list ~sep:sp pp_ident) targets
      pp_float_content content
      Fmt.(option (sp ++ pp_sign)) sign
  | SetEnvironment { variable; value } ->                         (* GnuCOBOL *)
    Fmt.pf ppf "ENVIRONMENT@ %a@ TO@ %a"
      pp_ident_or_literal variable
      pp_ident_or_literal value
  | SetEntry { targets ; value } ->
    Fmt.pf ppf "%a@ TO@ ENTRY@ %a"
      Fmt.(list ~sep:sp pp_ident) targets
      pp_ident_or_nonnum value

(* SORT *)
type sort_stmt =
  | SortFile of
      {
        file: qualident;
        keys: Data_descr.sort_spec list;                         (* Not empty *)
        duplicate_in_order: bool;
        collating: alphabet_specification option;
        source: sort_source;
        target: merge_or_sort_target;
      }
  | SortTable of
      {
        table: qualident;
        keys: Data_descr.sort_spec list;                      (* Can be empty *)
        duplicate_in_order: bool;
        collating: alphabet_specification option;
      }
[@@deriving ord]

and sort_source =                                                (* SORT only *)
  | SortInputProcedure of procedure_name with_loc procedure_range
  | SortUsing of name with_loc list
[@@deriving ord]

let pp_sort_source ppf = function
  | SortInputProcedure pr ->
    Fmt.pf ppf "INPUT@ PROCEDURE@ %a"
      (pp_procedure_range pp_procedure_name') pr
  | SortUsing ns ->
    Fmt.pf ppf "USING@ %a"
      Fmt.(list ~sep:sp (pp_with_loc pp_name)) ns

let pp_sort_stmt ppf = function
  | SortFile { file; keys; duplicate_in_order; collating; source; target } ->
    Fmt.pf ppf "SORT@ %a@ %a%a%a@ %a@ %a"
      pp_qualident file
      Fmt.(list ~sep:sp Data_descr.pp_sort_spec) keys
      Fmt.(if duplicate_in_order then any "@ DUPLICATES" else nop) ()
      Fmt.(option (any "@ COLLATING SEQUENCE@ " ++
                   pp_alphabet_specification)) collating
      pp_sort_source source
      pp_merge_or_sort_target target
  | SortTable { table; keys; duplicate_in_order; collating } ->
    Fmt.pf ppf "SORT@ %a" pp_qualident table;
    if keys != [] then Fmt.(sp ++ list ~sep:sp Data_descr.pp_sort_spec) ppf keys;
    if duplicate_in_order then Fmt.pf ppf "@ DUPLICATES";
    Fmt.(option (any "@ COLLATING SEQUENCE@ " ++
                 pp_alphabet_specification)) ppf collating


(* STOP *)
type stop_stmt =
  | StopArg of stop_arg option
  | StopRun of stop_run_return option
  | StopError
  | StopThread of qualident option
[@@deriving ord]

and stop_arg =
  | StopWithQualIdent of qualident
  | StopWithLiteral of literal

and stop_run_return =
  | StopReturning of returning
  | StopWithStatus of stop_run_status
[@@deriving ord]

and stop_run_status =
  {
    status_kind: status_kind;
    status_value: scalar option;
  }
[@@deriving ord]

and status_kind =
  | StatusError
  | StatusNormal
[@@deriving ord]

let pp_status_kind ppf = function
  | StatusError -> Fmt.pf ppf "ERROR"
  | StatusNormal -> Fmt.pf ppf "NORMAL"

let pp_stop_run_status ppf { status_kind; status_value } =
  Fmt.pf ppf "WITH@ %a@ STATUS@ %a"
    pp_status_kind status_kind
    Fmt.(option pp_scalar) status_value

let pp_stop_stmt ppf = function
  | StopArg None -> Fmt.pf ppf "STOP"
  | StopArg Some StopWithQualIdent i -> Fmt.pf ppf "STOP %a" pp_qualident i
  | StopArg Some StopWithLiteral l -> Fmt.pf ppf "STOP %a" pp_literal l
  | StopRun None -> Fmt.pf ppf "STOP RUN"
  | StopRun Some StopReturning r ->
    Fmt.pf ppf "STOP@ RUN%a"
      Fmt.(sp ++ pp_returning) r
  | StopRun Some StopWithStatus s ->
    Fmt.pf ppf "STOP@ RUN%a"
      Fmt.(sp ++ pp_stop_run_status) s
  | StopError -> Fmt.pf ppf "STOP ERROR"
  | StopThread None -> Fmt.pf ppf "STOP THREAD"
  | StopThread Some i -> Fmt.pf ppf "STOP THREAD %a" pp_qualident i

type terminate_stmt =
  name with_loc list
[@@deriving ord]

let pp_terminate_stmt =
  Fmt.(any "TERMINATE@ " ++ list ~sep:sp (pp_with_loc pp_name))


(* TRANSFORM *)

type transform_stmt =
  {
    transform_ident: ident with_loc;
    transform_from: ident_or_nonnum with_loc;
    transform_to: ident_or_nonnum with_loc;
  }
[@@deriving ord]

let pp_transform_stmt ppf { transform_ident; transform_from; transform_to } =
  Fmt.pf ppf "TRANSFORM@ %a@ FROM@ %a@ TO@ %a"
    (pp_with_loc pp_ident) transform_ident
    (pp_with_loc pp_ident_or_nonnum) transform_from
    (pp_with_loc pp_ident_or_nonnum) transform_to


(* UNLOCK *)
type unlock_stmt =
  {
    unlock_file: name with_loc;
    unlock_record: bool;
  }
[@@deriving ord]

let pp_unlock_stmt ppf { unlock_file; unlock_record } =
  Fmt.pf ppf "UNLOCK@ %a" (pp_with_loc pp_name) unlock_file;
  if unlock_record then Fmt.pf ppf "@ RECORDS"
