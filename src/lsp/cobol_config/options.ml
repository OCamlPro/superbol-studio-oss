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

open Types

(** This module contains all the option available *)

(* int options *)
let tab_stops: int list value =
  Value.int_list ~name:"tab-width"
    "Number@ of@ spaces@ that@ are@ assumed@ for@ tabs."

let text_column: int value =
  Value.int ~name:"text-column"
    "Right@ margin@ column@ number@ for@ fixed-form@ reference-format."

let pic_length: int value =
  Value.int ~name:"pic-length"
    "Maximum@ size@ of@ a@ picture@ string."

let word_length: int value =
  Value.int ~name:"word-length"
    "Maximum@ word-length @ for@ COBOL@ (=@ programmer@ defined)@ words."

let literal_length: int value =
  Value.int ~name:"literal-length"
    "Maximum@ literal@ size@ in@ general."

let numeric_literal_length: int value =
  Value.int ~name:"numeric-literal-length"
    "Maximum@ numeric@ literal@ size."

(* any options *)
let defaultbyte: defaultbyte value =
  let parse: string -> defaultbyte = function
    | "init" -> Init
    | "none" -> None
    | s ->
        if String.length s = 1 then
          Char (s.[0])
        else
          begin match int_of_string_opt s with
            | Some i when i >= 0 && i <= 255 ->
                Char (Char.chr i)
            | _ -> invalid_arg s
          end
  in
  Value.def ~parse ~name:"defaultbyte"
    "Default@ initialization@ for@ field@ without@ Value."

let standard_define: standard value =
  let parse: string -> standard = fun s ->
    match int_of_string_opt s with
    | Some 0 -> GnuCOBOL
    | Some 1 -> MicroFocus
    | Some 2 -> IBM
    | Some 3 -> MVS
    | Some 4 -> BS2000
    | Some 5 -> ACU
    | Some 6 -> RM
    | Some 7 -> STD85
    | Some 8 -> STD2002
    | Some 9 -> STD2014
    | _ -> invalid_arg s
  in
  Value.def ~parse ~name:"standard-define"
    "Used@ standard"

let format_of_string s =
  match String.lowercase_ascii s with
  | "fixed" -> SF SFFixed
  | "free" -> SF SFFree
  | "cobol85" -> SF SFCOBOL85
  | "variable" -> SF SFVariable
  | "xopen" -> SF SFXOpen
  | "xcard" -> SF SFxCard
  | "crt" -> SF SFCRT
  | "terminal" -> SF SFTrm
  | "cobolx" -> SF SFCOBOLX
  | "auto" -> Auto
  | _ -> invalid_arg s

let all_format_names =
  [
    "auto";
    "fixed";
    "free";
    "cobol85";
    "variable";
    "xopen";
    "xcard";
    "crt";
    "terminal";
    "cobolx";
  ]

let string_of_format = function
  | SF SFCOBOLX -> "cobolx"
  | SF SFCRT -> "crt"
  | SF SFFixed -> "fixed"
  | SF SFCOBOL85 -> "cobol85"
  | SF SFFree -> "free"
  | SF SFTrm -> "terminal"
  | SF SFVariable -> "variable"
  | SF SFXOpen -> "xopen"
  | SF SFxCard -> "xcard"
  | Auto -> "auto"

let format: source_format_spec value =
  let parse: string -> source_format_spec = format_of_string in
  Value.def ~parse ~name:"format"
    "Default@ reference@ format."

let binary_size: binary_size value =
  let parse: string -> binary_size = function
    | "2-4-8" -> B_2_4_8
    | "1-2-4-8" -> B_1_2_4_8
    | "1--8" -> B_1__8
    | s -> invalid_arg s
  in
  Value.def ~parse ~name:"binary-size"
    "Binary@ byte@ size@ -@ defines@ the@ allocated@ bytes@ according@ to@ PIC."

let binary_byteorder: binary_byteorder value =
  let parse: string -> binary_byteorder = fun s ->
    match String.lowercase_ascii s with
    | "native" -> Native
    | "big-endian" -> Big_endian
    | _ -> invalid_arg s
  in
  Value.def ~parse ~name:"binary-byteorder"
    "Binary@ byte@ order."

let assign_clause: assign_clause value =
  let parse: string -> assign_clause = fun s ->
    match String.lowercase_ascii s with
    | "dynamic" -> Dynamic
    | "external" -> External
    | "ibm" -> IBM
    | "mf" -> MF
    | _ -> invalid_arg s
  in
  Value.def ~parse ~name:"assign-clause"
    "How@ to@ interpret@ 'ASSIGN@ word':@ as@ 'ASSIGN@ EXTERNAL@ word'@ or@ 'ASSIGN@ DYNAMIC@ word'"

let screen_section_rules: screen_section_rules value =
  let parse: string -> screen_section_rules = fun s ->
    match String.lowercase_ascii s with
    | "acu" -> ACU
    | "gc" -> GC
    | "mf" -> MF
    | "rm" -> RM
    | "std" -> STD
    | "xopen" -> XOPEN
    | _ -> invalid_arg s
  in
  Value.def ~parse ~name:"screen-section-rules"
    "Which@ compiler's@ rules@ to@ apply@ to@ SCREEN@ SECTION@ item@ clauses."

let dpc_in_data: dpc_in_data value =
  let parse: string -> dpc_in_data = fun s ->
    match String.lowercase_ascii s with
    | "none" -> None
    | "xml" -> XML
    | "json" -> Json
    | "all" -> All
    | _ -> invalid_arg s
  in
  Value.def ~parse ~name:"dpc-in-data"
    "Wether@ DECIMAL@ POINT@ IS@ COMMA@ has@ effect@ in@ XML/JSON@ GENERATE."

(* boolean options *)
let filename_mapping: bool value =
  Value.bool ~name:"filename-mapping"
    "resolve@ file@ names@ at@ run@ time@ using@ environment@ variables"

let pretty_display: bool value =
  Value.bool ~name:"pretty-display"
    "alternate@ formatting@ of@ numeric@ fields"

let binary_truncate: bool value =
  Value.bool ~name:"binary-truncate"
    "numeric@ truncation@ according@ to@ ANSI"

let complex_odo: bool value =
  Value.bool ~name:"complex-odo"
    "allow@ non-standard@ OCCURS@ DEPENDING@ ON@ syntax"

let odoslide: bool value =
  Value.bool ~name:"odoslide"
    "adjust@ items@ following@ OCCURS@ DEPENDING@ (implies@ complex-odo)"

let indirect_redefines: bool value =
  Value.bool ~name:"indirect-redefines"
    "allow@ REDEFINES@ to@ other@ than@ last@ equal@ level@ number"

let relax_syntax_checks: bool value =
  Value.bool ~name:"relax-syntax-checks"
    "allow@ certain@ syntax@ variation@ (e.g.@ REDEFINES@ position)"

let ref_mod_zero_length: bool value =
  Value.bool ~name:"ref-mod-zero-length"
    "allow@ zero@ length@ reference-modification@ (only@ changed@ with@ EC-BOUND-REF-MOD@ active)"

let relax_level_hierarchy: bool value =
  Value.bool ~name:"relax-level-hierarchy"
    "allow@ non-matching@ level@ numbers"

let select_working: bool value =
  Value.bool ~name:"select-working"
    "require@ ASSING@ UNSING@ items@ to@ be@ in@ WORKING-STORAGE"

let local_implies_recursive: bool value =
  Value.bool ~name:"local-implies-recursive"
    "LOCAL-STORAGE@ SECTION@ implies@ RECURSIVE@ attribute"

let sticky_linkage: bool value =
  Value.bool ~name:"sticky-linkage"
    "LINKAGE@ SECTION@ items@ remain@ allocated@ between@ invocations"

let move_ibm: bool value =
  Value.bool ~name:"move-ibm"
    "MOVE@ operates@ as@ on@ IBM@ (left@ to@ right,@ byte@ by@ byte)"

let perform_osvs: bool value =
  Value.bool ~name:"perform-osvs"
    "exit@ point@ of@ any@ currently@ executing@ perfom@ is@ recognized@ if@ reached"

let arithmetic_osvs: bool value =
  Value.bool ~name:"arithmetic-osvs"
    "limit@ precision@ in@ intermediate@ result@ to@ precision@ of@ final@ result@ (less@ accurate)"

let hostsign: bool value =
  Value.bool ~name:"hostsign"
    "allow@ hexadecimal@ values@ 'F' for@ NUMERIC@ test@ of@ signed@ PACKED@ DECIMAL@ field"

let program_name_redefinition: bool value =
  Value.bool ~name:"program-name-redefinition"
    "program@ names@ dont't@ lead@ to@ reserved@ identifier"

let accept_update: bool value =
  Value.bool ~name:"accept-update"
    "set@ WITH@ UPDATE@ clause@ as@ default@ for@ ACCEPT@ dest-item,@ instead@ of@ WITH@ NO@ UPDATE"

let accept_auto: bool value =
  Value.bool ~name:"accept-auto"
    "set@ WITH@ AUTO@ clause@ as@ default@ for@ ACCEPT@ dest-item,@ instead@ of@ WIHT@ TAB"

let console_is_crt: bool value =
  Value.bool ~name:"console-is-crt"
    "assume@ CONSOLE@ IS@ CRT@ if@ not@ set@ otherwise"

let no_echo_means_secure: bool value =
  Value.bool ~name:"no-echo-means-secure"
    "NO-ECHO@ hides@ input@ with@ asterisks@ like@ SECURE"

let line_col_zero_default: bool value =
  Value.bool ~name:"line-col-zero-default"
    "assume@ a@ field@ DISPLAY@ starts@ at@ LINE@ 0@ COL@ 0@ (i.e.@ at@ the@ cursor),@ not LINE@ 1@ COL@ 1"

let display_special_fig_consts: bool value =
  Value.bool ~name:"display-special-fig-consts"
    "special@ behaviour@ of@ DISPLAY@ SPACE/ALL@ X'01'/ALL@ X'02'/ALL X'07'"

let binary_comp_1: bool value =
  Value.bool ~name:"binary-comp-1"
    "COMP-1@ is@ a@ 16-bit@ signed@ integer"

let numeric_pointer: bool value =
  Value.bool ~name:"numeric-pointer"
    "POINTER@ is@ 64-bit@ unsigned@ integer"

let move_non_numeric_lit_to_numeric_is_zero: bool value =
  Value.bool ~name:"move-non-numeric-lit-to-numeric-is-zero"
    "imply@ zero@ in@ move@ of@ non-numeric@ literal@ to@ numeric@ items"

let implicit_assign_dynamic_var: bool value =
  Value.bool ~name:"implicit-assign-dynamic-var"
    "implicitly@ define@ a@ variable@ if@ an@ ASSIGN@ DYNAMIC@ does@ not@ match@ any@ data@ item"

let device_mnemonics: bool value =
  Value.bool ~name:"device-mnemonics"
    "specifying@ device@ by@ mnemonic"

let xml_parse_xmlss: bool value =
  Value.bool ~name:"xml-parse-xmlss"
    "XML@ PARSE@ XMLSS"

let areacheck: bool value =
  Value.bool ~name:"areacheck"
    "check@ contents@ of@ Area@ A@ (when@ reference@ format@ supports@ Area@ A enforcement)@\n\
     @[@[enabled@ checks@ include:@]@\n\
     @[*@ division,@ section,@ paragraph@ names,@ level@ indicators@ (FD,@ SD,@ RD,@ \
     and@ CD),@\n@[and@ toplevel@ numbers@ (01@ and@ 77)@ must@ start@ in@ Area@ A;@]@]@\n\
     @[*@ statements@ must@ not@ start@ in@ Area A; and@]@\n\
     @[*@ separator@ periods@ must@ not@ be@ within@ Area@ A@]@]"

let ebcdic_symbolic_characters: bool value =
  Value.bool ~name:"ebcdic-symbolic-characters"
    "EBCDIC symbolic characters in literals (\" \"135,151,151\"bar\"195, \
     194\"Z\" for \" foobarBAZ\")"

(* support options *)
(*NOTE: GnuCOBOL option name is "partial-replace-when-literal-src"*)
let safe_partial_replacing_when_src_literal: [`Safe | `Unsafe] feature =
  let open Cobol_dialect.Types in
  let of_level : unit support -> [`Safe | `Unsafe] support = function
    | Ok () -> Ok `Safe
    | Warning () -> Warning `Safe
    | Archaic () -> Archaic `Safe
    | Obsolete () -> Obsolete `Safe
    | Skip -> Ok `Unsafe                                (* special semantics *)
    | Ignore -> Ignore
    | Error -> Error
    | Unconformable -> Unconformable
  in
  Cobol_dialect.Feature.def ~of_level ~name:"partial-replace-when-literal-src"
    ~short:"partial@ replacing@ with@ literal@ source@ operands"
    "Enable@ partial@ replacings@ (COPY...REPLACING/REPLACE@ \
     LEADING/TRAILING)@ with@ literal@ source@ operands, and@ apply@ them@ \
     even@ when@ they@ replace@ with@ spaces@ only."

let comment_paragraphs: unit feature =
  Cobol_dialect.Feature.unit ~name:"comment-paragraphs"
    ~short:"comment@ paragraph@ in@ IDENTIFICATION@ DIVISION"
    "Comment@ paragraphs@ in@ IDENTIFICATION@ DIVISION@ (AUTHOR,@ \
     DATE-WRITTEN,@ ...)."

let control_division: unit feature =
  Cobol_dialect.Feature.unit ~name:"control-division"
    "CONTROL@ DIVISION"

let memory_size_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"memory-size-clause"
    "MEMORY-SIZE@ clause"

let multiple_file_tape_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"multiple-file-tape-clause"
    "MULTIPLE-FILE-TAPE@ clause"

let label_records_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"label-records-clause"
    "LABEL_RECORDS@ clause"

let value_of_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"value-of-clause"
    "Value-OF@ clause"

let data_records_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"data-records-clause"
    "DATA-RECORDS@ clause"

let top_level_occurs_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"top-level-occurs-clause"
    "OCCURS@ clause@ on@ top-level"

let same_as_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"same-as-clause"
    "SAME@ AS@ clause"

let type_to_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"type-to-clause"
    "TYPE@ TO@ clause"

let usage_type: unit feature =
  Cobol_dialect.Feature.unit ~name:"usage-type"
    "USAGE@ type-name"

let synchronized_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"synchronized-clause"
    "SYNCHRONIZED@ clause"

let sync_left_right: unit feature =
  Cobol_dialect.Feature.unit ~name:"sync-left-right"
    "LEFT/RIGHT@ phrases@ in@ SYNCHRONIZED@ clause"

let special_names_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"special-names-clause"
    "SPECIAL-NAMES@ clause"

let goto_statement_without_name: unit feature =
  Cobol_dialect.Feature.unit ~name:"goto-statement-without-name"
    "GO@ TO@ statement without name"

let stop_literal_statement: unit feature =
  Cobol_dialect.Feature.unit ~name:"stop-literal-statement"
    "STOP-literal@ statement"

let stop_identifier_statement: unit feature =
  Cobol_dialect.Feature.unit ~name:"stop-identifier-statement"
    "STOP-identifier@ statement"

let stop_error_statement: unit feature =
  Cobol_dialect.Feature.unit ~name:"stop-error-statement"
    "STOP-error@ statement"

let debugging_mode: unit feature =
  Cobol_dialect.Feature.unit ~name:"debugging-mode"
    "DEBUGGING@ MODE@ and@ debugging@ indicator"

let use_for_debugging: unit feature =
  Cobol_dialect.Feature.unit ~name:"use-for-debugging"
    "USE@ FOR@ DEBUGGING"

let padding_character_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"padding-character-clause"
    "PADDING@ CHARACTER@ clause"

let next_sentence_phrase: unit feature =
  Cobol_dialect.Feature.unit ~name:"next-sentence-phrase"
    "NEXT@ SENTENCE@ phrase"

let listing_statements: unit feature =
  Cobol_dialect.Feature.unit ~name:"listing-statements"
    "listing-directive@ statements@ EJECT,@ SKIP1,@ SKIP2,@ SKIP3"

let title_statement: unit feature =
  Cobol_dialect.Feature.unit ~name:"title-statement"
    "listing-directive@ statement@ TITLE"

let entry_statement: unit feature =
  Cobol_dialect.Feature.unit ~name:"entry-statement"
    "ENTRY@ statement"

let move_noninteger_to_alphanumeric: unit feature =
  Cobol_dialect.Feature.unit ~name:"move-noninteger-to-alphanumeric"
    "move@ noninteger@ to@ alphanumeric"

let move_figurative_constant_to_numeric: unit feature =
  Cobol_dialect.Feature.unit ~name:"move-figurative-constant-to-numeric"
    "move@ figurative@ constants@ to@ numeric"

let move_figurative_space_to_numeric: unit feature =
  Cobol_dialect.Feature.unit ~name:"move-figurative-space-to-numeric"
    "move@ figurative@ constant@ SPACE@ to@ numeric"

let move_figurative_quote_to_numeric: unit feature =
  Cobol_dialect.Feature.unit ~name:"move-figurative-quote-to-numeric"
    "move@ figurative@ constant@ QUOTE@ to@ numeric"

let odo_without_to: unit feature =
  Cobol_dialect.Feature.unit ~name:"odo-without-to"
    "OCCURS@ DEPENDING@ ON@ without@ to"

let section_segments: unit feature =
  Cobol_dialect.Feature.unit ~name:"section-segments"
    "section@ segments"

let alter_statement: unit feature =
  Cobol_dialect.Feature.unit ~name:"alter-statement"
    "ALTER@ statement"

let call_overflow: unit feature =
  Cobol_dialect.Feature.unit ~name:"call-overflow"
    "OVERFLOW@ clause@ for@ CALL"

let numeric_boolean: unit feature =
  Cobol_dialect.Feature.unit ~name:"numeric-boolean"
    "boolean@ literal@ (B'1010')"

let hexadecimal_boolean: unit feature =
  Cobol_dialect.Feature.unit ~name:"hexadecimal-boolean"
    "hexadecimal-boolean@ literal@ (BX'A')"

let national_literals: unit feature =
  Cobol_dialect.Feature.unit ~name:"national-literals"
    "national@ literals@ (N'UTF-16 string')"

let hexadecimal_national_literals: unit feature =
  Cobol_dialect.Feature.unit ~name:"hexadecimal-national-literals"
    "hexadecimal-national@ literals@ (NX'265E')"

let national_character_literals: unit feature =
  Cobol_dialect.Feature.unit ~name:"national-character-literals"
    "non-standard@ national@ literals@ (NC'UTF-16 string')"

let hp_octal_literals: unit feature =
  Cobol_dialect.Feature.unit ~name:"hp-octal-literals"
    "HP@ COBOL@ octal@ literals (%%377)"

let acu_literals: unit feature =
  Cobol_dialect.Feature.unit ~name:"acu-literals"
    "ACUCOBOL-G@ literals@ (#B #O #H #X)"

let word_continuation: unit feature =
  Cobol_dialect.Feature.unit ~name:"word-continuation"
    "continuation@ of@ COBOL@ words"

let not_exception_before_exception: unit feature =
  Cobol_dialect.Feature.unit ~name:"not-exception-before-exception"
    "NOT@ ON@ EXCEPTION@ before@ ON@ EXCEPTION"

let accept_display_extensions: unit feature =
  Cobol_dialect.Feature.unit ~name:"accept-display-extensions"
    "extensions@ to@ ACCEPT@ and@ DISPLAY"

let larger_redefines: unit feature =
  Cobol_dialect.Feature.unit ~name:"larger-redefines"
    "allow@ larger@ REDEFINES@ items"

let symbolic_constant: unit feature =
  Cobol_dialect.Feature.unit ~name:"symbolic-constant"
    "constants@ defined@ in@ SPECIAL-NAMES"

let constant_78: unit feature =
  Cobol_dialect.Feature.unit ~name:"constant-78"
    ~short:"constant@ with@ level@ 78@ item"
    "constant@ with@ level@ 78@ item@ (note:@ has@ left@ to@ right@ precedence@ in@ expressions)"

let constant_01: unit feature =
  Cobol_dialect.Feature.unit ~name:"constant-01"
    "constant@ wiht@ level@ 01@ CONSTANT@ AS/FROM@ item (COBOL@ 2002+)"

let perform_varying_without_by: unit feature =
  Cobol_dialect.Feature.unit ~name:"perform-varying-without-by"
    "PERFORM@ VARYING@ without@ BY@ phrase@ (implies@ BY@ 1)"

let reference_out_of_declaratives: unit feature =
  Cobol_dialect.Feature.unit ~name:"reference-out-of-declaratives"
    "references@ to@ sections@ not@ in@ DECLARATIVES@ from@ within@ DECLARATIVES"

let program_prototypes: unit feature =
  Cobol_dialect.Feature.unit ~name:"program-prototypes"
    "CALL/CANCEL@ with@ program-prototype-name"

let call_convention_mnemonic: unit feature =
  Cobol_dialect.Feature.unit ~name:"call-convention-mnemonic"
    "specifying@ call-convention@ by@ mnemonic"

let call_convention_linkage: unit feature =
  Cobol_dialect.Feature.unit ~name:"call-convention-linkage"
    "specifying@ call-convention@ by@ WITH@ ...@ LINKAGE"

let numeric_value_for_edited_item: unit feature =
  Cobol_dialect.Feature.unit ~name:"numeric-value-for-edited-item"
    "numeric@ literals@ in@ Value@ clause@ of@ numeric-edited@ items"

let incorrect_conf_sec_order: unit feature =
  Cobol_dialect.Feature.unit ~name:"incorrect-conf-sec-order"
    "incorrect@ order@ of@ CONFIGURATION@ SECTION@ paragraphs@ (OpenCOBOL/GnuCOBOL@ extension)"

let define_constant_directive: unit feature =
  Cobol_dialect.Feature.unit ~name:"define-constant-directive"
    "allow@ >>@ DEFINE@ CONSTANT@ var@ AS@ literal@ (OpenCOBOL/GnuCOBOL@ extension)"

let free_redefines_position: unit feature =
  Cobol_dialect.Feature.unit ~name:"free-redefines-position"
    "REDEFINES@ clause@ not@ following@ entry-name@ in@ definition"

let records_mismatch_record_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"records-mismatch-record-clause"
    "record@ sizes@ does@ not@ match@ RECORD@ clause"

let record_delimiter: unit feature =
  Cobol_dialect.Feature.unit ~name:"record-delimiter"
    "RECORD@ DELIMITER@ clause"

let sequential_delimiters: unit feature =
  Cobol_dialect.Feature.unit ~name:"sequential-delimiters"
    "BINARY-SEQUENTIAL@ and @ LINE-SEQUENTIAL@ phrases@ in@ RECORD@ DELIMITER"

let record_delim_with_fixed_recs: unit feature =
  Cobol_dialect.Feature.unit ~name:"record-delim-with-fixed-recs"
    "RECORD@ DELIMITER@ clause@ on@ file@ with@ fixed-length@ records"

let missing_statement: unit feature =
  Cobol_dialect.Feature.unit ~name:"missing-statement"
    "missing@ statement@ (e.g.@ empty@ IF/PERFORM)"

let missing_period: unit feature =
  Cobol_dialect.Feature.unit ~name:"missing-period"
    ~short:"missing@ period@ in@ PROCEDURE@ DIVISION"
    "missing@ period@ in@ PROCEDURE@ DIVISION@ (when@ reference@ format@ supports@ Area@ A@ enforcement)"

let zero_length_literals: unit feature =
  Cobol_dialect.Feature.unit ~name:"zero-length-literals"
    "zero-length@ literals,@ e.g.@ ''@ and@ \"\""

let xml_generate_extra_phrases: unit feature =
  Cobol_dialect.Feature.unit ~name:"xml-generate-extra-phrases"
    "XML@ GENERATE's@ phrases@ other@ thant@ COUNT@ IN"

let continue_after: unit feature =
  Cobol_dialect.Feature.unit ~name:"continue-after"
    "AFTER@ phrase@ in@ CONTINUE@ statement"

let goto_entry: unit feature =
  Cobol_dialect.Feature.unit ~name:"goto-entry"
    "ENTRY@ FOR@ GO@ TO@ and@ GO@ TO@ ENTRY@ statements"

let assign_variable: unit feature =
  Cobol_dialect.Feature.unit ~name:"assign-variable"
    "ASSIGN@ [TO]@ variable@ in@ SELECT"

let assign_using_variable: unit feature =
  Cobol_dialect.Feature.unit ~name:"assign-using-variable"
    "ASSIGN@ USING/VARYING variable@ in@ SELECT"

let assign_ext_dyn: unit feature =
  Cobol_dialect.Feature.unit ~name:"assign-ext-dyn"
    "ASSIGN@ EXTERNAL/DYNAMIC@ in@ SELECT"

let assign_disk_from: unit feature =
  Cobol_dialect.Feature.unit ~name:"assign-disk-from"
    "ASSIGN@ DISK@ FROM@ variable@ in@ SELECT"

let vsam_status: unit feature =
  Cobol_dialect.Feature.unit ~name:"vsam-status"
    "VSAM@ status@ in@ FILE@ STATUS"

let self_call_recursive: unit feature =
  Cobol_dialect.Feature.unit ~name:"self-call-recursive"
    "CALL@ to@ own@ PROGRAM-ID@ implies@ RECURSIVE@ attribute"

let record_contains_depending_clause: unit feature =
  Cobol_dialect.Feature.unit ~name:"record-contains-depending-clause"
    "DEPENDING@ clause@ in@ RECORD@ CONTAINS"

let picture_l: unit feature =
  Cobol_dialect.Feature.unit ~name:"picture-l"
    "PICTURE@ string@ with@ 'L'@ character"
