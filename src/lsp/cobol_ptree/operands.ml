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

(* Used in ENVIRONMENT, and SORT/MERGE statements *)

type alphabet_specification =                      (* At least one is required *)
  {
    alphanumeric: name with_loc option;
    national: name with_loc option;
  }
[@@deriving ord]

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

(* ACCEPT, DISPLAY *)
type position =
  | LinePosition of ident_or_intlit
  | ColumnPosition of ident_or_intlit
  | LineColumnPosition of ident_or_intlit * ident_or_intlit
  | CompoundPosition of ident_or_intlit                                 (* MF *)
[@@deriving ord]

let pp_position ppf = function
  | LinePosition i -> Fmt.pf ppf "LINE %a" pp_ident_or_intlit i
  | ColumnPosition i -> Fmt.pf ppf "COL %a" pp_ident_or_intlit i
  | LineColumnPosition (l, c) ->
    Fmt.pf ppf "LINE %a@ COL %a" pp_ident_or_intlit l pp_ident_or_intlit c
  | CompoundPosition i -> Fmt.pf ppf "%a" pp_ident_or_intlit i

(* CALL, INVOKE *)

type call_using_clause =
  {
    call_using_by: call_using_by option;
    call_using_expr: expression option;(* [@compare
    fun a b -> Option.compare compare_expression a b] *)                (** OMITTED if [None] *)
  }
[@@deriving ord]

and call_using_by =
  | CallUsingByReference
  | CallUsingByContent
  | CallUsingByValue
[@@deriving ord]

let pp_call_using_by ppf = function
  | CallUsingByReference -> Fmt.pf ppf "BY REFERENCE"
  | CallUsingByContent -> Fmt.pf ppf "BY CONTENT"
  | CallUsingByValue -> Fmt.pf ppf "BY VALUE"

let pp_call_using_clause ppf { call_using_by = cub; call_using_expr = cue } =
  Fmt.(option (pp_call_using_by ++ sp)) ppf cub;
  Fmt.(option ~none:(const string "OMITTED") pp_expression) ppf cue


(* DELETE, OPEN, REWRITE, WRITE, READ (through on_lock_or_retry) *)
type retry_clause =
  | RetryNTimes of expression (* arith *)
  | RetryForNSeconds of expression (* arith *)
  | RetryForever
[@@deriving ord]

let pp_retry_clause ppf rc =
  Fmt.pf ppf "RETRY";
  match rc with
  | RetryNTimes e -> Fmt.pf ppf "@ @[%a@]@ TIMES" pp_expression e
  | RetryForNSeconds e ->Fmt.pf ppf " FOR@ @[%a@]@ SECONDS" pp_expression e
  | RetryForever -> Fmt.pf ppf " FOREVER"


(* ENABLE, DISABLE *)
(* MCS: Message Control System *)

type mcs_command_operands =
  {
    mcs_command_kind: mcs_kind;
    mcs_command_target: name with_loc;
    mcs_command_key: ident_or_alphanum option;
  }
[@@deriving ord]

and mcs_kind =
  | MCSInput
  | MCSOutput
  | MCSInputOutput
[@@deriving ord]

let pp_mcs_kind ppf = function
  | MCSInput -> Fmt.pf ppf "INPUT"
  | MCSOutput -> Fmt.pf ppf "OUTPUT"
  | MCSInputOutput -> Fmt.pf ppf "I-O TERMINAL"

let pp_modifier_opt modifier pp =
  Fmt.(option (sp ++ const string modifier ++ const string " " ++ pp))

let pp_mcs_command_operands
  ppf { mcs_command_kind = k; mcs_command_target = t; mcs_command_key = key } =
  Fmt.pf ppf "%a@ %a%a"
    pp_mcs_kind k
    (pp_with_loc pp_name) t
    (pp_modifier_opt "KEY" pp_ident_or_alphanum) key


(* SEND, WRITE, PERFORM *)
type stage =
  | After
  | Before
[@@deriving ord]

let pp_stage ppf = function
  | After -> Fmt.pf ppf "AFTER"
  | Before -> Fmt.pf ppf "BEFORE"

(* REWRITE, WRITE *)
type write_target =
  | WriteTargetName of qualname
  | WriteTargetFile of name with_loc
[@@deriving ord]

let pp_write_target ppf = function
  | WriteTargetName n -> pp_qualname ppf n
  | WriteTargetFile n -> Fmt.pf ppf "FILE@ %a" (pp_with_loc pp_name) n


(*
ACCEPT id END_ACC (device or screen)
ACCEPT id FROM DATE/DAY... END_ACC (temporal)
ACCEPT id MESSAGE? COUNT END_ACC (message count?)
ACCEPT id AT ... ON EXCEPT ... END_ACC (screen)
*)

(* ACCEPT *)
type date_time =
  | Date of bool
  | Day of bool
  | DayOfWeek
  | Time
[@@deriving ord]

let pp_date_time ppf = function
  | Date b -> Fmt.pf ppf "DATE"; if b then Fmt.pf ppf " YYYYMMDD"
  | Day b -> Fmt.pf ppf "DAY"; if b then Fmt.pf ppf " YYYYDDD"
  | DayOfWeek -> Fmt.pf ppf "DAY-OF-WEEK"
  | Time -> Fmt.pf ppf "TIME"



(* ADD, SUBTRACT *)
type basic_arithmetic_operands =
  | ArithSimple of
      {
        sources: ident_or_numlit list;
        targets: rounded_idents;
      }
  | ArithGiving of
      {
        sources: ident_or_numlit list;
        to_or_from_item: ident_or_numlit;
        targets: rounded_idents;
      }
  | ArithCorresponding of
      {
        source: qualname;
        target: rounded_ident;
      }
[@@deriving ord]

let pp_arithmetic_operands ?modifier ~sep pp_lhs pp_rhs ppf (args, body) =
  let pp_lhs = Fmt.box pp_lhs and pp_rhs = Fmt.box pp_rhs in
  Fmt.pf ppf "%a@;<1 2>%a"
    Fmt.(option (any " " ++ string)) modifier
    Fmt.(box (pair ~sep:Fmt.(sp ++ const string sep ++ sp) pp_lhs pp_rhs)) args;
  Fmt.(list ~sep:nop (any "@ " ++ box ~indent:2 (fun ppf pf -> pf ppf ()))) ppf body

let pp_giving targets =
  [ Fmt.(any "GIVING@ " ++ const (box pp_rounded_idents) targets) ]

let pp_basic_arithmetic_operands ?(sep = "TO") ppf bao =
  let pp_sources = Fmt.(list ~sep:sp pp_ident_or_numlit) in
  match bao with
  | ArithSimple { sources; targets } ->
    pp_arithmetic_operands ~sep pp_sources pp_rounded_idents
      ppf ((sources, targets), [])
  | ArithGiving { sources; to_or_from_item; targets } ->
    pp_arithmetic_operands ~sep pp_sources pp_ident_or_numlit
      ppf ((sources, to_or_from_item), pp_giving targets)
  | ArithCorresponding { source; target } ->
    pp_arithmetic_operands ~modifier:"CORRESPONDING" ~sep
      pp_qualname pp_rounded_ident
      ppf ((source, target), [])


(*
DIVIDE id/lit INTO id_rounded+ ON_SIZE...? END_DIV

DIVIDE id/lit INTO id/lit GIVING... ON_SIZE...? END_DIV
DIVIDE id/lit INTO id/lit GIVING... REMAINDER... ON_SIZE...? END_DIV

DIVIDE id/lit BY id/lit GIVING... ON_SIZE...? END_DIV
DIVIDE id/lit BY id/lit GIVING... REMAINDER... ON_SIZE...? END_DIV
*)

(* DIVIDE *)
type divide_operands =
  | DivideInto of
      {
        divisor: ident_or_numlit;
        dividends: rounded_idents;                          (* non-empty *)
      }
  | DivideGiving of
      {
        divisor: ident_or_numlit;
        dividend: ident_or_numlit;
        giving: rounded_idents;
        into: bool;                         (* "INTO" if true, "by" otherwise *)
        remainder: ident option;
      }
[@@deriving ord]

let pp_remainder_opt = function
  | Some remainder -> [ Fmt.(any "REMAINDER@ " ++ const pp_ident remainder) ]
  | None -> []

let pp_divide_operands ppf = function
  | DivideInto { divisor; dividends } ->
    pp_arithmetic_operands ~sep:"INTO" pp_ident_or_numlit pp_rounded_idents
      ppf ((divisor, dividends), [])
  | DivideGiving { divisor; dividend; giving; into; remainder } ->
    let pair = if into then divisor, dividend else dividend, divisor in
    pp_arithmetic_operands ~sep:(if into then "INTO" else "BY")
      pp_ident_or_numlit pp_ident_or_numlit
      ppf (pair, pp_giving giving @ pp_remainder_opt remainder)


(* EVALUATE *)
type selection_subject =
  | Subject of condition
  | SubjectConst of bool
[@@deriving ord]

let pp_selection_subject ppf = function
  | Subject c -> pp_condition ppf c
  | SubjectConst b -> Fmt.pf ppf (if b then "TRUE" else "FALSE")

type selection_object =
  | SelCond of condition (* ident / literal / expression *)
  | SelRange of
      {
        negated: bool;
        start: expression;
        stop: expression;
        alphabet: name with_loc option;
      }
  | SelRelation of
      {
        relation: relop;
        expr: expression;
      }
  | SelClassCond of
      {
        negated: bool;
        class_specifier: class_;
      }
  | SelSignCond of
      {
        negated: bool;
        sign_specifier: signz;
      }
  | SelOmitted of
      {
        negated: bool;
      }
  | SelConst of bool
  | SelAny
[@@deriving ord]

let pp_selection_object ppf = function
  | SelCond c -> pp_condition ppf c
  | SelRange { negated; start; stop; alphabet } ->
    if negated then Fmt.pf ppf "NOT@ ";
    Fmt.pf ppf "%a@ THROUGH@ %a%a"
      pp_expression start
      pp_expression stop
      Fmt.(option (sp ++ const string "IN" ++ sp ++ pp_with_loc pp_name)) alphabet
  | SelRelation { relation; expr } ->
    Fmt.pf ppf "%a@ %a" pp_relop relation pp_expression expr
  | SelClassCond { negated; class_specifier = cs } ->
    if negated then Fmt.pf ppf "NOT@ ";
    pp_class_ ppf cs
  | SelSignCond { negated; sign_specifier } ->
    if negated then Fmt.pf ppf "NOT@ ";
    pp_signz ppf sign_specifier
  | SelOmitted { negated } ->
    if negated then Fmt.pf ppf "NOT@ ";
    Fmt.pf ppf "OMITTED"
  | SelConst b -> Fmt.pf ppf (if b then "TRUE" else "FALSE")
  | SelAny -> Fmt.pf ppf "ANY"


(* MULTIPLY *)
type multiply_operands =
  | MultiplyBy of
      {
        multiplier: ident_or_numlit;
        multiplicand: rounded_idents;
      }
  | MultiplyGiving of
      {
        multiplier: ident_or_numlit;
        multiplicand: ident_or_numlit;
        targets: rounded_idents;
      }
[@@deriving ord]

let pp_multiply_operands ppf = function
  | MultiplyBy { multiplier; multiplicand = mc } ->
    pp_arithmetic_operands ~sep:"BY"
      pp_ident_or_numlit pp_rounded_idents
      ppf ((multiplier, mc), [])
  | MultiplyGiving { multiplier = a; multiplicand = b; targets = c } ->
    pp_arithmetic_operands ~sep:"BY"
      pp_ident_or_numlit pp_ident_or_numlit
      ppf ((a, b), pp_giving c)


(* OPEN *)
type open_mode =
  | OpenInput
  | OpenOutput
  | OpenInputOutput
  | OpenExtend
[@@deriving ord]

let pp_open_mode ppf = function
  | OpenInput -> Fmt.pf ppf "INPUT"
  | OpenOutput -> Fmt.pf ppf "OUTPUT"
  | OpenInputOutput -> Fmt.pf ppf "I-O"
  | OpenExtend -> Fmt.pf ppf "EXTEND"

type sharing_mode =
  | SharingAllOther
  | SharingNoOther
  | SharingReadOnly
[@@deriving ord]

let pp_sharing_mode ppf = function
  | SharingAllOther -> Fmt.pf ppf "ALL OTHER"
  | SharingNoOther -> Fmt.pf ppf "NO OTHER"
  | SharingReadOnly -> Fmt.pf ppf "READ ONLY"


type file_option =
  | FileOptReversed
  | FileOptWithNoRewind
[@@deriving ord]

let pp_file_option ppf = function
  | FileOptReversed -> Fmt.pf ppf "REVERSED"
  | FileOptWithNoRewind -> Fmt.pf ppf "WITH NO REWIND"


(* RAISE *)
type raise_operand =
  | RaiseIdent of ident
  | RaiseException of name with_loc
[@@deriving ord]

let pp_raise_operand ppf = function
  | RaiseIdent i -> Fmt.pf ppf "RAISE@ %a" pp_ident i
  | RaiseException nwl ->
    Fmt.pf ppf "RAISE@ EXCEPTION@ %a" (pp_with_loc pp_name) nwl


(* EXIT & GO BACK *)
type raising =
  | RaisingIdent of ident                (* CHECKME: Can ident be a subscript?*)
  | RaisingException of name with_loc
  | RaisingLastException
[@@deriving ord]

let pp_raising ppf r =
  Fmt.pf ppf "RAISING";
  match r with
  | RaisingIdent i -> Fmt.pf ppf "@ %a" pp_ident i
  | RaisingException n -> Fmt.pf ppf " EXCEPTION@ %a" (pp_with_loc pp_name) n
  | RaisingLastException -> Fmt.pf ppf "LAST EXCEPTION"

(* READ *)
type read_direction =
  | ReadNext
  | ReadPrevious
[@@deriving ord]

let pp_read_direction ppf = function
  | ReadNext -> Fmt.string ppf "NEXT"
  | ReadPrevious -> Fmt.string ppf "PREVIOUS"

type read_lock_behavior =
  | ReadAdvancingOnLock
  | ReadIgnoringLock
  | ReadRetry of retry_clause
[@@deriving ord]

let pp_read_lock_behavior ppf = function
  | ReadAdvancingOnLock -> Fmt.pf ppf "ADVANCING ON LOCK"
  | ReadIgnoringLock -> Fmt.pf ppf "IGNORING LOCK"
  | ReadRetry rc -> pp_retry_clause ppf rc


(* RECEIVE *)
type mcs_awaiting_item =
  | MCSMessage
  | MCSSegment
[@@deriving ord]

let pp_mcs_awaiting_item ppf = function
  | MCSMessage -> Fmt.pf ppf "MESSAGE"
  | MCSSegment -> Fmt.pf ppf "SEGMENT"


(* SEARCH *)
type search_condition =
  | IsEqual of
      {
        data_item: qualident;
        condition: expression;
      }
  | Cond of qualident
[@@deriving ord]

let pp_search_condition ppf = function
  | IsEqual { data_item = di; condition = c } ->
    Fmt.pf ppf "%a@ EQUAL@ %a" pp_qualident di pp_expression c
  | Cond qi -> pp_qualident ppf qi


(* SEND, WRITE *)
type advancing_phrase =
  | AdvancingLines of
      {
        stage: stage;
        lines: ident_or_intlit;
        ambiguous: bool;
      }
  | AdvancingPage of
      {
        stage: stage;
      }
[@@deriving ord]

let pp_advancing_phrase ppf = function
  | AdvancingLines { stage; lines; ambiguous } ->
    Fmt.pf ppf "%a@ ADVANCING@ %a" pp_stage stage pp_ident_or_intlit lines;
    if not ambiguous then Fmt.pf ppf "@ LINES"
  | AdvancingPage { stage } ->
    Fmt.pf ppf "%a@ ADVANCING@ PAGE" pp_stage stage


(* SET *)
type set_attribute_switch =
  {
    set_attribute: screen_attribute;
    set_attribute_switch_value: on_off;
  }
[@@deriving ord]

and screen_attribute =
  | ScreenBell
  | ScreenBlink
  | ScreenHighlight
  | ScreenLowlight
  | ScreenReverseVideo
  | ScreenUnderline
[@@deriving ord]

and set_ambiguous_method =
  | SetMethodUp
  | SetMethodDown
  | SetMethodTo
[@@deriving ord]

and on_off =
  | On
  | Off
[@@deriving ord]

and locale_category =
  | LcAll
  | LcCollate
  | LcCtype
  | LcMessages
  | LcMonetary
  | LcNumeric
  | LcTime
[@@deriving ord]

and set_save_locale =
  | SetSaveLocaleLcAll
  | SetSaveLocaleUserDefault
[@@deriving ord]

and set_locale_target =
  | SetLocaleTarget of locale_category
  | SetLocaleTargetUserDefault
[@@deriving ord]

and set_locale_source =
  | SetLocaleSource of ident
  | SetLocaleSourceUserDefault
  | SetLocaleSourceSystemDefault
[@@deriving ord]

and float_content =
  | FarthestFromZero of bool
  | NearestToZero of bool
  | FloatInfinity
  | FloatNotANumber
  | FloatNotANumberSignaling
[@@deriving ord]

let pp_screen_attribute ppf = function
  | ScreenBell -> Fmt.pf ppf "BELL"
  | ScreenBlink -> Fmt.pf ppf "BLINK"
  | ScreenHighlight -> Fmt.pf ppf "HIGHLIGHT"
  | ScreenLowlight -> Fmt.pf ppf "LOWLIGHT"
  | ScreenReverseVideo -> Fmt.pf ppf "REVERSE-VIDEO"
  | ScreenUnderline -> Fmt.pf ppf "UNDERLINE"

let pp_on_off ppf = function
  | On -> Fmt.pf ppf "ON"
  | Off -> Fmt.pf ppf "OFF"

let pp_set_attribute_switch ppf { set_attribute = a; set_attribute_switch_value = v } =
  Fmt.pf ppf "%a@ %a" pp_screen_attribute a pp_on_off v

let pp_set_ambiguous_method ppf = function
  | SetMethodUp -> Fmt.pf ppf "UP BY"
  | SetMethodDown -> Fmt.pf ppf "DOWN BY"
  | SetMethodTo -> Fmt.pf ppf "TO"

let pp_locale_category ppf = function
  | LcAll -> Fmt.pf ppf "LC_ALL"
  | LcCollate -> Fmt.pf ppf "LC_COLLATE"
  | LcCtype -> Fmt.pf ppf "LC_CTYPE"
  | LcMessages -> Fmt.pf ppf "LC_MESSAGES"
  | LcMonetary -> Fmt.pf ppf "LC_MONETARY"
  | LcNumeric -> Fmt.pf ppf "LC_NUMERIC"
  | LcTime -> Fmt.pf ppf "LC_TIME"

let pp_set_save_locale ppf = function
  | SetSaveLocaleLcAll -> Fmt.pf ppf "LC_ALL"
  | SetSaveLocaleUserDefault -> Fmt.pf ppf "USER_DEFAULT"

let pp_set_locale_target ppf = function
  | SetLocaleTarget lc -> pp_locale_category ppf lc
  | SetLocaleTargetUserDefault -> Fmt.pf ppf "USER_DEFAULT"

let pp_set_locale_source ppf = function
  | SetLocaleSource i -> pp_ident ppf i
  | SetLocaleSourceUserDefault -> Fmt.pf ppf "USER_DEFAULT"
  | SetLocaleSourceSystemDefault -> Fmt.pf ppf "SYSTEM_DEFAULT"

let pp_float_content ppf = function
  | FarthestFromZero iar ->
    Fmt.pf ppf "FARTHEST-FROM-ZERO";
    if iar then Fmt.pf ppf "@ IN-ARITHMETIC-RANGE"
  | NearestToZero iar ->
    Fmt.pf ppf "NEAREST-TO-ZERO";
    if iar then Fmt.pf ppf "@ IN-ARITHMETIC-RANGE"
  | FloatInfinity -> Fmt.pf ppf "FLOAT-INFINITY"
  | FloatNotANumber -> Fmt.pf ppf "FLOAT-NOT-A-NUMBER"
  | FloatNotANumberSignaling -> Fmt.pf ppf "FLOAT-NOT-A-NUMBER-SIGNALING"


(* START *)
type start_position =
  | StartPositionFirst
  | StartPositionLast
  | StartPositionKey of
      {
        operator: relop;
        name: qualname;
        length: expression option;
      }
  (* any relop except IS NOT EQUAL TO or IS NOT = *)
[@@deriving ord]

let pp_start_position ppf = function
  | StartPositionFirst -> Fmt.pf ppf "FIRST"
  | StartPositionLast -> Fmt.pf ppf "LAST"
  | StartPositionKey { operator = op; name = n; length = l } ->
    Fmt.pf ppf "KEY@ %a@ %a%a"
      pp_relop op
      pp_qualname n
      Fmt.(option (sp ++ const words "WITH LENGTH" ++ pp_expression)) l


(* STRING *)
type string_source =
  {
    string_source: ident_or_nonnum;
    string_delimiter: string_delimiter option;
  }
[@@deriving ord]

and string_delimiter =
  | StringDelimiter of ident_or_nonnum
  | StringDelimiterSize
[@@deriving ord]

let pp_string_delimiter ppf = function
  | StringDelimiter i -> pp_ident_or_nonnum ppf i
  | StringDelimiterSize -> Fmt.pf ppf "SIZE"

let pp_string_source ppf { string_source = ss; string_delimiter = sd } =
  pp_ident_or_nonnum ppf ss;
  match sd with
  | Some sd -> Fmt.pf ppf "@ DELIMITED@ BY@ %a" pp_string_delimiter sd
  | None -> ()


(* UNSTRING *)
type unstring_delimiter =
  {
    unstring_delimiter: ident_or_strlit;
    unstring_delimiter_by_all: bool;
  }
[@@deriving ord]

let pp_unstring_delimiter ppf { unstring_delimiter = i; unstring_delimiter_by_all = b } =
  if b then Fmt.pf ppf "ALL@ ";
  pp_ident_or_strlit ppf i

type unstring_target =
  {
    unstring_target: ident;
    unstring_target_delimiter: ident option;
    unstring_target_count: ident option;
  }
[@@deriving ord]

let pp_unstring_target ppf
  { unstring_target = i
  ; unstring_target_delimiter = d
  ; unstring_target_count = c }
=
  Fmt.pf ppf "%a%a%a"
    pp_ident i
    Fmt.(option (sp ++ const string "DELIMITER" ++ sp ++ pp_ident)) d
    Fmt.(option (sp ++ const string "COUNT" ++ sp ++ pp_ident)) c

(* --- generics --- *)

type 'a procedure_range =
  {
    procedure_start: 'a;
    procedure_end: 'a option;
  }
[@@deriving ord]

let pp_procedure_range pp ppf { procedure_start; procedure_end } =
  pp ppf procedure_start;
  Fmt.(option (sp ++ const string "THROUGH" ++ sp ++ pp)) ppf procedure_end
