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

open Terms
open Operands


(* -------------------- IDENTIFICATION DIVISION (EXTRA) -------------------- *)

type informational_paragraphs =
  informational_paragraph with_loc list
[@@deriving ord]

and informational_paragraph =
  informational_paragraph_header * comment_entry with_loc

and informational_paragraph_header =
  | Author
  | DateCompiled
  | DateModified
  | DateWritten
  | Installation
  | Remarks
  | Security

and comment_entry = string list


(* ------------------------- ENVIRONMENT DIVISION -------------------------- *)
type environment_division =
  {
    env_configuration: configuration_section with_loc option;
    env_input_output: input_output_section with_loc option;
  }
[@@deriving ord]

(* ------------- ENVIRONMENT DIVISION / CONFIGURATION SECTION -------------- *)
and configuration_section =
  {
    source_computer_paragraph: source_computer_paragraph with_loc option;
    object_computer_paragraph: object_computer_paragraph with_loc option;
    special_names_paragraph: special_names_paragraph with_loc option;
    repository_paragraph: repository_paragraph with_loc option;   (* +COB2002 *)
  }

(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / SOURCE-COMPUTER PARAGRAPH *)
and source_computer_paragraph =
  source_computer option

and source_computer =
  {
    source_computer_name: name with_loc;
    source_computer_with_debugging_mode: bool;
  }

(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / OBJECT-COMPUTER PARAGRAPH *)
and object_computer_paragraph =
  object_computer option

and object_computer =
  {
    object_computer_name: name with_loc;
    object_computer_clauses: object_computer_clause with_loc list;
  }

and object_computer_clause =
  | ComputerMemorySize of                                 (* ~COB85, -COB2002 *)
      integer * memory_size_unit
  | ComputerCharClassification of                                 (* +COB2002 *)
      {
        alphanumeric: locale option;              (* at least one must be set *)
        national: locale option
      }
  | ComputerProgCollatingSeq of alphabet_specification    (* COB85 != COB2002 *)
  | ComputerSegmentLimit of integer                       (* -COB2002 *)

and memory_size_unit =
  | MemoryWords
  | MemoryCharacters
  | MemoryModules

and locale =
  | CharClassificationName of name with_loc
  | CharClassificationLocale
  | CharClassificationSystemDefault
  | CharClassificationUserDefault

and alphabet_specification =                      (* At least one is required *)
  {
    alphanumeric: name with_loc option;
    national: name with_loc option;
  }
(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / SPECIAL-NAMES PARAGRAPH *)
and special_names_paragraph =
  special_names_clause with_loc list

and special_names_clause =
  | AlphabetName of                                       (* Multiple allowed *)
      {
        alphabet_name: name with_loc;
        category: alphanumeric_or_national;
        characters: character_set;
      }
  | ClassName of                                          (* Multiple allowed *)
      {
        class_name: name with_loc;
        category: alphanumeric_or_national;
        characters: character_range list;
        source_charset: name with_loc option;
      }
  | CRTStatus of name with_loc                         (* +COB2002 *)
  | CurrencySign of                                    (* Multiple in COB2002 *)
      {
        sign: strlit;
        picture_symbol: strlit option;
      }
  | Cursor of name with_loc                                       (* +COB2002 *)
  | DecimalPointIsComma
  | DynLenStruct of                        (* +COB2002 *) (* Multiple allowed *)
      {
        name: name with_loc;
        kind: dyn_len_struct_kind;
      }
  | SpecialNameLocale of                   (* +COB2002 *) (* Multiple allowed *)
      {
        locale_name: name with_loc;
        external_name: name_or_string;
      }
  | MnemonicName of                        (* Multiple allowed *)
      {
        implementor_name: name with_loc;
        mnemonic_name: name with_loc option;
        status: status_switch option;
      }
  | SymbolicChars of                                      (* Multiple allowed *)
      {
        category: alphanumeric_or_national;
        characters: (name with_loc list * integer list) list; (* same lengths *)
        source_charset: name with_loc option;
      }
  | OrderTable of                                                 (* +COB2002 *)
      {
        ordering_name: name with_loc;
        cultural_ordering: strlit;
      }

and alphanumeric_or_national =
  | Alphanumeric
  | National

and character_range =
  | SingleCharacter of strlit_or_intlit
  | CharacterRange of range_items

and characters_range =
  | CharactersRange of character_range
  | CharactersList of strlit_or_intlit list                    (* non-trivial *)

and range_items =
  {
    start_item: strlit_or_intlit;
    end_item: strlit_or_intlit;
  }

and character_set =
  | CharSetLocale of name with_loc option     (* +COB2002 *)
  | CharSetNative                             (* +COB2002 *)
  | CharSetStandard_1                         (* Alphanum only *)
  | CharSetStandard_2                         (* Alphanum only *)
  | CharSetUCS_4                              (* +COB2002 *) (* National only *)
  | CharSetUTF_8                              (* +COB2002 *) (* National only *)
  | CharSetUTF_16                             (* +COB2002 *) (* National only *)
  | CharSetCharacters of characters_range list                   (* non-empty *)

and dyn_len_struct_kind =
  | DynLenPrefixed of { signed: bool; short: bool }
  | DynLenDelimited
  | DynLenPhysical of name with_loc

and status_switch =
  | StatusSwitchOn of name with_loc
  | StatusSwitchOff of name with_loc
  | StatusSwitch of { on_: name with_loc; off: name with_loc }

(* ENVIRONMENT DIVISION / CONFIGURATION SECTION / REPOSITORY PARAGRAPH *)
and repository_paragraph =
  specifier list

and specifier =                                       (* TODO: inline records *)
  | ClassSpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
        expands: expands option;
      }
  | InterfaceSpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
        expands: expands option;
      }
  | UserFunctionSpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
      }
  | IntrinsicFunctionSpecifier of
      name with_loc list                                         (* non-empty *)
  | IntrinsicFunctionAllSpecifier
  | ProgramSpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
      }
  | PropertySpecifier of
      {
        name: name with_loc;
        external_name: strlit option;
      }

and expands =
  {
    expands_name: name with_loc;
    expands_using: name with_loc list; (* non-empty *)
  }


(* -------------- ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION -------------- *)
and input_output_section =
  {
    file_control_paragraph: file_control_paragraph with_loc option; (* COB85: mandatory *)
    io_control_paragraph: io_control_paragraph with_loc option;
  }

(* - ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION / FILE-CONTROL PARAGRAPH -- *)
and file_control_paragraph =
  select list

and select =
  {
    select_optional: bool;
    select_name: name with_loc;
    select_clauses: select_clause with_loc list;
  }

and select_clause =
  | SelectAssign of
      {
        to_: name_or_alphanum list;
        using: name with_loc option;
      }
  | SelectAccessMode of access_mode
  | SelectAlternateRecordKey of
      {
        key: qualname;
        source: name with_loc list;
        with_duplicates: bool;
      }
  (* | SelectCollatingSequence of collating_sequence_clause *)
  | SelectCollatingSequenceOfFile of
      alphabet_specification                       (* +COB2002 *) (* Multiple *)
  | SelectCollatingSequenceOfKey of                (* +COB2002 *) (* Multiple *)
      {
        keys: name with_loc list;
        alphabet: name with_loc;
      }
  | SelectStatus of qualname
  | SelectLockMode of                                             (* +COB2002 *)
      {
        mode: lock_mode;
        with_lock: with_lock;
      }
  | SelectOrganization of organization
  | SelectPaddingCharacter of qualname_or_alphanum                (* -COB2002 *)
  | SelectRecordDelimiter of record_delimiter
  | SelectRecordKey of
      {
        key: qualname;
        source: name with_loc list;
      }
  | SelectRelativeKey of name with_loc
  | SelectReserve of integer
  | SelectSharing of sharing_mode                                 (* +COB2002 *)

and access_mode =
  | AccessModeDynamic
  | AccessModeRandom
  | AccessModeSequential

and lock_mode =
  | LockManual
  | LockAutomatic

and with_lock =
  | WithLockNone
  | WithLock of { multiple: bool }

and organization =
  | OrganizationIndexed
  | OrganizationRelative
  | OrganizationSequential

and record_delimiter =
  | Standard_1

(* -- ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION / I-O-CONTROL PARAGRAPH -- *)
and io_control_paragraph =
  io_control_entry option

and io_control_entry =
  {
    io_control_rerun_clauses: rerun_clause with_loc list;         (* -COB2002 *)
    io_control_same_area_clauses: same_area_clause with_loc list;
    io_control_multiple_file_clauses: multiple_file_clause with_loc list; (* -COB2002 *)
  }

and rerun_clause =
  {
    rerun_on: name with_loc option;
    rerun_every: rerun_frequency;
  }

and rerun_frequency =
  | RerunEndOf of name with_loc
  | RerunRecords of integer * name with_loc
  | RerunClockUnits of integer
  | RerunCond of name with_loc

and same_area_clause =
  {
    same_area_source: area_source;
    same_area_file_name: name with_loc;
    same_area_file_names: name with_loc list; (* non-empty *)
  }

and area_source =
  | AreaSourceFile
  | AreaSourceRecord
  | AreaSourceSortMerge

and multiple_file_clause =
  file_portion list

and file_portion =
  {
    file_portion_name: name with_loc;
    file_portion_position: integer option;
  }

let pp_file_portion ppf { file_portion_name = fpn; file_portion_position = fpp } =
  pp_with_loc pp_integer ppf fpn;
  Option.iter (Fmt.pf ppf " POSITION %a" pp_integer) fpp

let pp_multiple_file_clause : multiple_file_clause Fmt.t =
  Fmt.(any "MULTIPLE FILE " ++ list ~sep:(any " ") pp_file_portion)

let pp_area_source_opt ppf = function
  | AreaSourceFile -> ()
  | AreaSourceRecord -> Fmt.pf ppf " RECORD"
  | AreaSourceSortMerge -> Fmt.pf ppf " SORT"

let pp_same_area_clause ppf
  { same_area_source = s; same_area_file_name = fn; same_area_file_names = fns }
=
  Fmt.pf ppf "SAME%a %a %a"
    pp_area_source_opt s
    pp_name' fn
    Fmt.(list ~sep:(any " ") pp_name') fns

let pp_rerun_frequency ppf = function
  | RerunEndOf n -> Fmt.pf ppf "END UNIT %a" pp_name' n
  | RerunRecords (i, id) -> Fmt.pf ppf "%a RECORDS %a" pp_integer i pp_name' id
  | RerunClockUnits i -> Fmt.pf ppf "%a CLOCK-UNITS" pp_integer i
  | RerunCond n -> pp_name' ppf n

let pp_rerun_clause ppf { rerun_on; rerun_every } =
  Fmt.pf ppf "RERUN%a %a"
    Fmt.(option (any " ON " ++ pp_name')) rerun_on
    pp_rerun_frequency rerun_every

let pp_io_control_entry ppf {
  io_control_rerun_clauses = rrcs;
  io_control_same_area_clauses = sacs;
  io_control_multiple_file_clauses = mfcs;
} =
  Fmt.(list ~sep:sp (pp_with_loc pp_rerun_clause)) ppf rrcs;
  if rrcs != [] && (sacs != [] || mfcs != []) then
    Fmt.sp ppf ();
  Fmt.(list ~sep:sp (pp_with_loc pp_same_area_clause)) ppf sacs;
  if sacs != [] && mfcs != [] then
    Fmt.sp ppf ();
  Fmt.(list ~sep:sp (pp_with_loc pp_multiple_file_clause)) ppf mfcs;
  Fmt.pf ppf "."

let pp_io_control_paragraph ppf iceo =
  Fmt.pf ppf "I-O-CONTROL.%a"
    Fmt.(option (sp ++ box pp_io_control_entry)) iceo

let pp_expands ppf { expands_name; expands_using } =
  Fmt.pf ppf "EXPANDS %a USING %a"
    pp_name' expands_name
    Fmt.(list ~sep:sp pp_name' ) expands_using

let pp_status_switch ppf = function
  | StatusSwitchOn n -> Fmt.pf ppf "ON %a" pp_name' n
  | StatusSwitchOff n -> Fmt.pf ppf "OFF %a" pp_name' n
  | StatusSwitch { on_; off } ->
    Fmt.pf ppf "ON %a OFF %a" pp_name' on_ pp_name' off

let pp_dyn_len_struct_kind ppf = function
  | DynLenPrefixed { signed; short } ->
    if signed then Fmt.pf ppf "SIGNED ";
    if short then Fmt.pf ppf "SHORT ";
    Fmt.pf ppf "PREFIXED"
  | DynLenDelimited -> Fmt.pf ppf "DELIMITED"
  | DynLenPhysical n -> pp_name' ppf n

let pp_range_items ppf { start_item; end_item } =
  Fmt.pf ppf "%a THROUGH %a"
    pp_strlit_or_intlit start_item
    pp_strlit_or_intlit end_item

let pp_character_classification pp ppf (alphanumeric, national) =
  match alphanumeric, national with
  | Some a, Some n ->
    Fmt.pf ppf "FOR ALPHANUMERIC %a@ FOR NATIONAL %a" pp a pp n
  | Some a, None ->
    Fmt.pf ppf "FOR ALPHANUMERIC %a" pp a
  | None, Some n ->
    Fmt.pf ppf "FOR NATIONAL %a" pp n
  | None, None -> (* Type invariant *) assert false

let pp_alphabet_specification ppf { alphanumeric; national } =
  pp_character_classification pp_name' ppf (alphanumeric, national)

let pp_locale ppf = function
  | CharClassificationName n -> pp_name' ppf n
  | CharClassificationLocale -> Fmt.pf ppf "LOCALE"
  | CharClassificationSystemDefault -> Fmt.pf ppf "SYSTEM-DEFAULT"
  | CharClassificationUserDefault -> Fmt.pf ppf "USER-DEFAULT"

let pp_memory_size_unit ppf = function
  | MemoryWords -> Fmt.pf ppf "WORDS"
  | MemoryCharacters -> Fmt.pf ppf "CHARACTERS"
  | MemoryModules -> Fmt.pf ppf "MODULES"

let pp_object_computer_clause ppf = function
  | ComputerMemorySize (n, u) ->
    Fmt.pf ppf "MEMORY %a %a" pp_integer n pp_memory_size_unit u
  | ComputerCharClassification { alphanumeric; national } ->
    pp_character_classification pp_locale ppf (alphanumeric, national)
  | ComputerProgCollatingSeq aspec ->
    Fmt.pf ppf "SEQUENCE %a" pp_alphabet_specification aspec
  | ComputerSegmentLimit i ->
    Fmt.pf ppf "SEGMENT-LIMIT %a" pp_integer i

let pp_object_computer ppf
  { object_computer_name = ocn; object_computer_clauses = occs }
=
  Fmt.pf ppf "%a%a."
    pp_name' ocn
    Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_object_computer_clause)) occs

let pp_object_computer_paragraph : object_computer_paragraph Fmt.t =
  Fmt.(any "OBJECT-COMPUTER." ++ Fmt.option (sp ++ box pp_object_computer))

let pp_source_computer ppf
  { source_computer_name = scn; source_computer_with_debugging_mode = wdb }
=
  pp_name' ppf scn;
  if wdb then Fmt.pf ppf "@ WITH DEBUGGING MODE";
  Fmt.pf ppf "."

let pp_source_computer_paragraph : source_computer_paragraph Fmt.t =
  Fmt.(any "SOURCE-COMPUTER." ++ Fmt.option (sp ++ box pp_source_computer))

let pp_specifier_aux kind ppf n en ex =
  Fmt.pf ppf "%s %a%a%a" kind
    pp_name' n
    Fmt.(option (any "@ AS " ++ pp_strlit)) en
    Fmt.(option (sp ++ pp_expands)) ex

let pp_specifier ppf = function
  | ClassSpecifier { name; external_name = en; expands = ex } ->
    pp_specifier_aux "CLASS" ppf name en ex
  | InterfaceSpecifier { name; external_name = en; expands = ex } ->
    pp_specifier_aux "INTERFACE" ppf name en ex
  | UserFunctionSpecifier { name; external_name = en } ->
    pp_specifier_aux "FUNCTION" ppf name en None
  | IntrinsicFunctionSpecifier ns ->
    Fmt.pf ppf "FUNCTION %a INTRINSIC"
      Fmt.(list ~sep:sp pp_name') ns
  | IntrinsicFunctionAllSpecifier ->
    Fmt.pf ppf "FUNCTION ALL INTRINSIC"
  | ProgramSpecifier { name; external_name = en } ->
    pp_specifier_aux "PROGRAM" ppf name en None
  | PropertySpecifier { name; external_name = en } ->
    pp_specifier_aux "PROPERTY" ppf name en None

let pp_repository_paragraph ppf rps =
  Fmt.pf ppf "REPOSITORY.";
  Fmt.(list ~sep:nop (sp ++ pp_specifier ++ any ".")) ppf rps

let pp_alphanumeric_or_national ppf = function
  | Alphanumeric -> Fmt.pf ppf "ALPHANUMERIC"
  | National -> Fmt.pf ppf "NATIONAL"

let pp_alphanumeric_or_national_opt ppf = function
  | Alphanumeric -> ()
  | National -> Fmt.pf ppf " NATIONAL"

let pp_character_range ppf = function
  | SingleCharacter c -> pp_strlit_or_intlit ppf c
  | CharacterRange ri -> pp_range_items ppf ri

let pp_characters_range ppf = function
  | CharactersRange rng -> pp_character_range ppf rng
  | CharactersList xs ->
    Fmt.(list ~sep:(any " ALSO ") pp_strlit_or_intlit) ppf xs

let pp_character_set ppf = function
  | CharSetLocale n ->
    Fmt.pf ppf "LOCALE%a"
      Fmt.(option (sp ++ pp_name')) n
  | CharSetNative -> Fmt.pf ppf "NATIVE"
  | CharSetStandard_1 -> Fmt.pf ppf "STANDARD-1"
  | CharSetStandard_2 -> Fmt.pf ppf "STANDARD-2"
  | CharSetUCS_4 -> Fmt.pf ppf "UCS-4"
  | CharSetUTF_8 -> Fmt.pf ppf "UTF-8"
  | CharSetUTF_16 -> Fmt.pf ppf "UTF-16"
  | CharSetCharacters crs -> Fmt.(list ~sep:sp pp_characters_range) ppf crs

let pp_special_names_clause ppf = function
  | AlphabetName { alphabet_name = an; category = c; characters = cs } ->
    Fmt.pf ppf "ALPHABET %a%a IS %a"
      pp_name' an
      pp_alphanumeric_or_national_opt c
      pp_character_set cs
  | ClassName {
    class_name = cn; category = c; characters = cs; source_charset = sc
    } ->
      Fmt.pf ppf "CLASS %a %a IS %a%a"
        pp_name' cn
        pp_alphanumeric_or_national c
        Fmt.(list ~sep:nop (sp ++ pp_character_range)) cs
        Fmt.(option (any " IN " ++ pp_name')) sc
  | CRTStatus n ->
    Fmt.pf ppf "CRT STATUS IS %a" pp_name' n
  | CurrencySign { sign; picture_symbol = ps } ->
    Fmt.pf ppf "CURRENCY %a%a" pp_strlit sign
      Fmt.(option (any " PICTURE " ++ pp_strlit ++ any " SYMBOL")) ps
  | Cursor n -> Fmt.pf ppf "CURSOR %a" pp_name' n
  | DecimalPointIsComma -> Fmt.pf ppf "DECIMAL-POINT IS COMMA"
  | DynLenStruct { name; kind } ->
    Fmt.pf ppf "DYNAMIC LENGTH %a IS %a" pp_name' name pp_dyn_len_struct_kind kind
  | SpecialNameLocale { locale_name = ln; external_name = en } ->
    Fmt.pf ppf "LOCALE %a IS %a" pp_name' ln pp_name_or_string en
  | MnemonicName { implementor_name = n; mnemonic_name = mn; status = s} ->
    Fmt.pf ppf "%a%a%a"
      pp_name' n
      Fmt.(option (any " IS " ++ pp_name')) mn
      Fmt.(option (any " " ++ pp_status_switch)) s
  | SymbolicChars { category = c; characters = cs; source_charset = sc} ->
    let pp_names = Fmt.(list ~sep:sp pp_name') in
    let pp_integers = Fmt.(list ~sep:sp pp_integer) in
    Fmt.pf ppf "SYMBOLIC %a%a%a"
      pp_alphanumeric_or_national_opt c
      Fmt.(sp ++ list ~sep:sp (pair ~sep:(any " IS ") pp_names pp_integers)) cs
      Fmt.(option (any " IN " ++ pp_name')) sc
  | OrderTable { ordering_name = on; cultural_ordering = co } ->
    Fmt.pf ppf "ORDER TABLE %a IS %a" pp_name' on pp_strlit co

let pp_special_names_paragraph ppf sncs =
  Fmt.pf ppf "SPECIAL-NAMES.";
  match sncs with
  | [] -> ()
  | _ ->
    Fmt.pf ppf "%a."
      Fmt.(list ~sep:sp (pp_with_loc pp_special_names_clause)) sncs

let pp_configuration_section ppf
  { source_computer_paragraph = scp; object_computer_paragraph = ocp;
    special_names_paragraph = snp; repository_paragraph = rp }
=
  Fmt.pf ppf "CONFIGURATION SECTION.%a%a%a%a"
    Fmt.(option (sp ++ pp_with_loc pp_source_computer_paragraph)) scp
    Fmt.(option (sp ++ pp_with_loc pp_object_computer_paragraph)) ocp
    Fmt.(option (sp ++ pp_with_loc pp_special_names_paragraph)) snp
    Fmt.(option (sp ++ pp_with_loc pp_repository_paragraph)) rp

let pp_record_delimiter ppf = function
  | Standard_1 -> Fmt.pf ppf "STANDARD-1"

let pp_organization ppf = function
  | OrganizationIndexed -> Fmt.pf ppf "INDEXED"
  | OrganizationRelative -> Fmt.pf ppf "RELATIVE"
  | OrganizationSequential -> Fmt.pf ppf "SEQUENTIAL"

let pp_with_lock ppf = function
  | WithLockNone -> ()
  | WithLock { multiple } ->
    Fmt.pf ppf "WITH LOCK ON ";
    if multiple then
      Fmt.pf ppf "MULTIPLE RECORDS"
    else
      Fmt.pf ppf "RECORD"

let pp_lock_mode ppf = function
  | LockManual -> Fmt.pf ppf "MANUAL"
  | LockAutomatic -> Fmt.pf ppf "AUTOMATIC"

let pp_access_mode ppf = function
  | AccessModeDynamic -> Fmt.pf ppf "DYNAMIC"
  | AccessModeRandom -> Fmt.pf ppf "RANDOM"
  | AccessModeSequential -> Fmt.pf ppf "SEQUENTIAL"

let pp_select_clause ppf = function
  | SelectAssign { to_; using } ->
    Fmt.pf ppf "@[ASSIGN%a@]%a"
      Fmt.(list ~sep:nop (sp ++ pp_name_or_alphanum)) to_
      Fmt.(option (any "@ USING " ++ pp_name')) using
  | SelectAccessMode am ->
    Fmt.pf ppf "ACCESS %a" pp_access_mode am
  | SelectAlternateRecordKey { key; source; with_duplicates } ->
    Fmt.pf ppf "ALTERNATE RECORD %a%t%a%t"
      pp_qualname key
      (if source != [] then Fmt.fmt " SOURCE " else Fmt.fmt "")
      Fmt.(list ~sep:sp pp_name') source
      (if with_duplicates then Fmt.fmt " WITH DUPLICATES" else Fmt.fmt "")
  | SelectCollatingSequenceOfFile aspec ->
    Fmt.pf ppf "SEQUENCE %a" pp_alphabet_specification aspec
  | SelectCollatingSequenceOfKey { keys; alphabet } ->
    Fmt.pf ppf "SEQUENCE OF %a IS %a"
      Fmt.(list ~sep:sp pp_name') keys
      pp_name' alphabet
  | SelectStatus n -> Fmt.pf ppf "STATUS %a" pp_qualname n
  | SelectLockMode { mode; with_lock } ->
    Fmt.pf ppf "LOCK %a" pp_lock_mode mode;
    if with_lock != WithLockNone then
      Fmt.pf ppf " %a" pp_with_lock with_lock
  | SelectOrganization org -> pp_organization ppf org
  | SelectPaddingCharacter qoa ->
    Fmt.pf ppf "PADDING %a" pp_qualname_or_alphanum qoa
  | SelectRecordDelimiter rd ->
    Fmt.pf ppf "RECORD DELIMITER %a" pp_record_delimiter rd
  | SelectRecordKey { key; source } ->
    Fmt.pf ppf "RECORD %a%a"
      pp_qualname key
      Pretty.(list ~fopen:"SOURCE " ~fsep:"@ " ~fclose:"" ~fempty:""
                pp_name') source
  | SelectRelativeKey n ->
    Fmt.pf ppf "RELATIVE %a" pp_name' n
  | SelectReserve n -> Fmt.pf ppf "RESERVE %a" pp_integer n
  | SelectSharing sm -> Fmt.pf ppf "SHARING %a" pp_sharing_mode sm

let pp_select ppf { select_optional; select_name; select_clauses } =
  Fmt.pf ppf "@[SELECT ";
  if select_optional then
    Fmt.pf ppf "OPTIONAL ";
  Fmt.pf ppf "%a%a@]."
    pp_name' select_name
    Fmt.(list ~sep:nop (sp ++ pp_with_loc pp_select_clause)) select_clauses

let pp_file_control_paragraph ppf = function
  | [] -> Fmt.pf ppf "FILE-CONTROL."
  | xs ->
    Fmt.(any "FILE-CONTROL.@ " ++ box (list ~sep:sp pp_select)) ppf xs

let pp_input_output_section ppf
  { file_control_paragraph = fcp ; io_control_paragraph = icp }
=
  Fmt.pf ppf "INPUT-OUTPUT SECTION.";
  Fmt.(option (sp ++ pp_with_loc pp_file_control_paragraph)) ppf fcp;
  Fmt.(option (sp ++ pp_with_loc pp_io_control_paragraph)) ppf icp

let pp_environment_division ppf
  { env_configuration = ec; env_input_output = eio }
=
  Fmt.pf ppf "ENVIRONMENT DIVISION.%a%a"
    Fmt.(option (sp ++ pp_with_loc pp_configuration_section)) ec
    Fmt.(option (sp ++ pp_with_loc pp_input_output_section)) eio

type options_paragraph =
  options_clause with_loc list
[@@deriving ord]

and options_clause =
  | Arithmetic of arithmetic_mode
  | DefaultRoundedMode of rounding_mode
  | EntryConvention of entry_convention
  | FloatBinaryDefault of Data_descr.endianness_mode
  | FloatDecimalDefault of Data_descr.encoding_endianness               (* 1+ *)
  | IntermediateRounding of rounding_mode (* not all are valid (TODO:
                                             restriction with type param) *)

and arithmetic_mode =
  | Native
  | Standard        (* ~COB2002 *)
  | StandardBinary
  | StandardDecimal

(* Other conventions may be defined by the implementor *)
and entry_convention =
  | COBOL

(* --- *)

let pp_entry_convention ppf = function
  | COBOL -> Fmt.pf ppf "COBOL"

let pp_arithmetic_mode ppf = function
  | Native -> Fmt.pf ppf "NATIVE"
  | Standard -> Fmt.pf ppf "STANDARD"
  | StandardBinary -> Fmt.pf ppf "STANDARD-BINARY"
  | StandardDecimal -> Fmt.pf ppf "STANDARD-DECIMAL"

let pp_options_clause ppf = function
  | Arithmetic am ->
      Fmt.pf ppf "ARITHMETIC %a" pp_arithmetic_mode am
  | DefaultRoundedMode rm ->
      Fmt.pf ppf "DEFAULT ROUNDED MODE %a" pp_rounding_mode rm
  | EntryConvention ec ->
      Fmt.pf ppf "ENTRY-CONVENTION %a" pp_entry_convention ec
  | FloatBinaryDefault em ->
      Fmt.pf ppf "FLOAT-BINARY %a" Data_descr.pp_endianness_mode em
  | FloatDecimalDefault ee ->
      Fmt.pf ppf "FLOAT-DECIMAL %a" Data_descr.pp_encoding_endianness ee
  | IntermediateRounding rm ->
      Fmt.pf ppf "INTERMEDIATE-ROUNDING %a" pp_rounding_mode rm

let pp_options_paragraph : options_paragraph Fmt.t =
  Fmt.(any "OPTIONS.@ " ++
       box (list ~sep:sp (pp_with_loc pp_options_clause)) ++
       any ".")


(* --- *)

let pp_informational_paragraph_header ppf = function
  | Author -> Fmt.pf ppf "AUTHOR"
  | DateCompiled -> Fmt.pf ppf "DATE-COMPILED"
  | DateModified -> Fmt.pf ppf "DATE-MODIFIED"
  | DateWritten -> Fmt.pf ppf "DATE-WRITTEN"
  | Installation -> Fmt.pf ppf "INSTALLATION"
  | Remarks -> Fmt.pf ppf "REMARKS"
  | Security -> Fmt.pf ppf "SECURITY"

let pp_comment_entry: comment_entry Pretty.printer =
  Fmt.(list ~sep:sp string)

let pp_informational_paragraph: informational_paragraph Pretty.printer =
  Fmt.(any "@[<4>" ++                       (* <- indent by 4 to avoid Area A *)
       pair ~sep:(any ".@ ")
         pp_informational_paragraph_header
         (pp_with_loc pp_comment_entry) ++
       any "@]")

let pp_informational_paragraphs: informational_paragraphs Pretty.printer =
  Fmt.(list ~sep:(any "@\n")                                (* force newlines *)
         (pp_with_loc pp_informational_paragraph))
