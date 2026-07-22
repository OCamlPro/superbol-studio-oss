(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module IntMap = Map.Make (Int)
module StrMap = Map.Make (String)
module StrSet = Set.Make (String)

module SmartBuffer = struct

  type t = {
    mutable data  : Bytes.t;
    mutable start : int;
    mutable end_  : int;
  }

  let create ?(size=4096) () =
    { data = Bytes.create size; start = 0; end_ = 0 }

  let ensure_capacity b n =
    let free = Bytes.length b.data - b.end_ in
    if free < n then
      let used = b.end_ - b.start in
      if n < b.start && b.start > Bytes.length b.data / 2 then
        Bytes.blit b.data b.start b.data 0 used
      else
        begin
          let new_len = max (Bytes.length b.data * 2) (used + n) in
          let new_buf = Bytes.create new_len in
          Bytes.blit b.data b.start new_buf 0 used;
          b.data <- new_buf;
        end;
      b.start <- 0;
      b.end_ <- used

  let append b s =
    let n = String.length s in
    ensure_capacity b n;
    Bytes.blit_string s 0 b.data b.end_ n;
    b.end_ <- b.end_ + n

  let rec foreach_line b f =
    match Bytes.index_from_opt b.data b.start '\n' with
    | Some i when i < b.end_ ->
        let line_start = b.start in
        let line_len = i - line_start in
        f b.data line_start line_len;
        b.start <- i + 1;
        foreach_line b f
    | _ -> ()

end

module Handles = struct

  type 'a t = {
    mutable map : 'a IntMap.t;
    mutable next : int;
    start : int;
  }

  let create start =
    { map = IntMap.empty; next = start; start }

  let reset handles =
    handles.map <- IntMap.empty;
    handles.next <- handles.start

  let set handles value =
    let handle = handles.next in
    handles.next <- handles.next + 1;
    handles.map <- IntMap.add handle value handles.map;
    handle

  let get handle handles =
    IntMap.find handle handles.map

  let get_opt handle handles =
    IntMap.find_opt handle handles.map

end

module GdbTty = struct

  type t =
    | Bool of bool
    | String of string (* vscode, xterm, gnome-terminal, xfce4-terminal, konsole, external *)
    | Other of Ojs.t
  [@@js.union] [@@js]

  let t_of_js js_val =
    match Ojs.type_of js_val with
    | "boolean" -> Bool ([%js.to: bool] js_val)
    | "string" -> String ([%js.to: string] js_val)
    | _ -> Other (js_val)

end


module Breakpoint = struct

  type location =
    | FileLine of { file : string; line : int }
    | Function of string

  type t = {
    location : location;
    condition : string option;
    countCondition : string option;
  }

  module Map = Map.Make (struct
      type nonrec t = t
      let compare = Stdlib.compare
    end)

  module Set = Set.Make (struct
      type nonrec t = t
      let compare = Stdlib.compare
    end)

end

module Thread = struct

  type t = {
    id : int;
    targetId : int;
    name : string option;
  }

end

module Stack = struct

  type t = {
    level : int;
    function_ : string;
    file : string;
    line : int;
    lineContents : string;
  }

end

module Symbol = struct

  type t = {
    line : int;
    name : string;
    type_ : string;
    description : string
  }

end

module FileSymbols = struct

  type t = {
    filename : string;
    fullname : string;
    symbols : Symbol.t list;
  }

end

module LocalizedSymbol = struct

  type t = {
    symbol : Symbol.t;
    filename : string;
  }

  let ofFileSymbols (fileSymbols : FileSymbols.t) =
    List.map (fun symbol ->
        { symbol; filename = fileSymbols.fullname (* filename *) }
      ) fileSymbols.symbols

end

module CobFlag = struct

  type t =
    | UNKNOWN of int
    | HAVE_SIGN
    | SIGN_SEPARATE
    | SIGN_LEADING
    | BLANK_ZERO
    | JUSTIFIED
    | BINARY_SWAP
    | REAL_BINARY
    | IS_POINTER
    | NO_SIGN_NIBBLE
    | IS_FP
    | REAL_SIGN
    | BINARY_TRUNC
    | CONSTANT

  let maxFlag = 0x1000

  let ofInt i =
    match i with
    | 0x0001 -> HAVE_SIGN
    | 0x0002 -> SIGN_SEPARATE
    | 0x0004 -> SIGN_LEADING
    | 0x0008 -> BLANK_ZERO
    | 0x0010 -> JUSTIFIED
    | 0x0020 -> BINARY_SWAP
    | 0x0040 -> REAL_BINARY
    | 0x0080 -> IS_POINTER
    | 0x0100 -> NO_SIGN_NIBBLE
    | 0x0200 -> IS_FP
    | 0x0400 -> REAL_SIGN
    | 0x0800 -> BINARY_TRUNC
    | 0x1000 -> CONSTANT
    | _      -> UNKNOWN i

  module Set = struct
    include Set.Make (struct
        type nonrec t = t
        let compare = Stdlib.compare end
      )
    let ofInt i =
      let rec loop flags i' =
        if i' > maxFlag then flags
        else
          let flags =
            if i land i' = 0 then flags
            else add (ofInt i') flags
          in
          loop flags (i' * 2)
      in
      loop empty 1
  end

end

module CobolFieldDataParser = struct

  let reRepeatTime =
  Str.regexp_case_fold
    {|\("\,[ 	]\|^\)'\([ 	]\|0\)'[ 	]\<repeats[ 	]\([0-9]+\)[ 	]times\>|}

  (* NOTE: this is not specific to filelds *)
  let parseRepeat value =
    if Util.matches reRepeatTime value then
      let size = Util.groupInt value 3 in
      let char = (Util.group value 2).[0] in
      let repl = String.make size char ^ "\"" in
      let value = Str.replace_first reRepeatTime repl value in
      if value.[0] = '"' then Some (value)
      else Some ("\"" ^ value)
    else
      Some (value)

  (* Depending on how value is obtained, it can look like this:
       0x5555555884c0 <b_17> "042"
       0x55555558b690 <b_18> ' ' <repeats 200 times>...
       0x5555555884c0 "042"
       0x55555558b690 ' ' <repeats 200 times>...
     This function removes the prefix (including the optional <b_N>,
     and expands the count, if any.
     NOTE: value might end with ... if overflow max repeats  *)
  let parse value =
    match String.index_opt value ' ' with
    | None ->
        None (* NOTE: this should really never happen *)
    | Some (i) ->
        let value = Util.stringSubToEnd value (i + 1) in
        let value_opt =
          if String.length value > 0 && value.[0] = '<' then
            String.index_opt value ' ' |>
            Option.map (fun i -> Util.stringSubToEnd value (i + 1))
          else
            Some (value)
        in
        match value_opt with
        | None ->
            None (* NOTE: this should really never happen *)
        | Some (value) ->
            parseRepeat value
end

module NumericValueParser = struct

  let encodeSign value flags negative =
    if CobFlag.Set.mem HAVE_SIGN flags then
      if CobFlag.Set.mem SIGN_SEPARATE flags then
        let sign = if negative then "-" else "+" in
        if CobFlag.Set.mem SIGN_LEADING flags then sign ^ value
        else (* SIGN TRAILING *) value ^ sign
      else (* SIGN NON-SEPARATE *)
        if negative then
          let sign_pos, start, end_, leading =
            if CobFlag.Set.mem SIGN_LEADING flags then 0, 1, 0, true
            else (* SIGN TRAILING *) String.length value - 1, 0, 1, false
          in
          let digit = value.[sign_pos] in
          (* TODO: handle EBCDIC sign *)
          let c =
            Char.chr (Char.code 'p' + Char.code digit - Char.code '0') in
          let value = Util.stringSub value start end_ in
          let value =
            if leading then Printf.sprintf "%c%s" c value
            else Printf.sprintf "%s%c" value c
          in
          value
        else
          value
    else
      value

  (* Converts a numeric display value (for instance 123.456) to its internal
     representation (for instance 1234) according to field size & scale *)
  let format value fieldSize scale flags =
    let len = String.length value in
    if len <= 0 then
      "0"
    else
      let negative, value =
        if value.[0] = '-' then true, Util.stringSubToEnd value 1
        else if value.[0] = '+' then false, Util.stringSubToEnd value 1
        else false, value
      in
      let integer, decimals =
        match String.split_on_char '.' value with
        | integer :: decimals :: _ -> integer, decimals
        | integer :: [] -> integer, ""
        | [] -> assert false (* never empty *)
      in
      let value =
        if scale < 0 then
          String.sub integer 0 (max 0 (String.length integer + scale))
        else if scale > 0 then
          integer ^ String.sub decimals 0 scale
        else
          integer
      in
      let signed = CobFlag.Set.mem HAVE_SIGN flags in
      let sepSign = signed && CobFlag.Set.mem SIGN_SEPARATE flags in
      let digits = if sepSign then fieldSize - 1 else fieldSize in
      let diff = digits - String.length value in
      let value =
        if diff > 0 then
          String.make diff '0' ^ value
        else if diff < 0 then
          Util.stringSubToEnd value (abs diff)
        else
          value
      in
      encodeSign value flags negative

  let extractSign value flags =
    if CobFlag.Set.mem HAVE_SIGN flags then
      if CobFlag.Set.mem SIGN_SEPARATE flags then
        let sign_pos, start, end_ =
          if CobFlag.Set.mem SIGN_LEADING flags then 0, 1, 0
          else (* SIGN TRAILING *) String.length value - 1, 0, 1
        in
        (if value.[sign_pos] = '-' then "-" else ""),
        Util.stringSub value start end_
      else (* SIGN NON-SEPARATE *)
        let sign_pos, start, end_, leading =
          if CobFlag.Set.mem SIGN_LEADING flags then 0, 1, 0, true
          else (* SIGN TRAILING *) String.length value - 1, 0, 1, false
        in
        let sign = value.[sign_pos] in
        (* TODO: handle EBCDIC sign *)
        if sign >= 'p' then
          let c = Char.chr (Char.code '0' + Char.code sign - Char.code 'p') in
          let value = Util.stringSub value start end_ in
          let value =
            if leading then Printf.sprintf "%c%s" c value
            else Printf.sprintf "%s%c" value c
          in
          "-", value
        else
          "", value
    else
      "", value


  (* Converts a numeric display value from its internal representation (for
     instance 1234p) to its human-readble equivalent (for instance -12.340) *)
  (* NOTE: seems the quotes are always present *)
  let parse value fieldSize scale flags =
    let len = String.length value in
    if len > 0 && value.[0] = '"' && fieldSize > 0 then
      let value = Util.stringSub value 1 1 in
      let sign, value = extractSign value flags in
      let len = String.length value in
      let prefix =
        if len < scale then String.make (scale - len) '0'
        else ""
      in
      let suffix =
        if scale < 0 then String.make (-scale) '0'
        else ""
      in
      let value = prefix ^ value ^ suffix in
      let len = String.length value in
      let integer = Util.stringSub value 0 (abs scale) in
      let decimal = Util.stringSub value (len - abs scale) 0 in
      if decimal = "" then sign ^ integer
      else sign ^ integer ^ "." ^ decimal
    else
      value
end

module AlphanumericValueParser = struct

  (* Converts an alphanumeric display value to its internal representation
     according to field size *)
  let format value fieldSize =
    let len = String.length value in
    if len <= 0 then
      ""
    else
      let start = if value.[0] = '"' then 1 else 0 in
      let end_ = if value.[len-1] = '"' then 1 else 0 in
      let value = Util.stringSub value start end_ in
      let len = String.length value in
      let diff = fieldSize - len in
      if diff > 0 then
        value ^ (String.make diff ' ')
      else if diff < 0 then
        String.sub value 0 fieldSize
      else
        value

  (* Converts an alphanumeric display value from its internal representation
     to its human-readable equivalent. This just amounts to trimming the
     string and ensuring it is between quotes. *)
  let parse value fieldSize =
    let len = String.length value in
    let start, size =
      if len > 0 && value.[0] = '"' && fieldSize > 0 then
        1, min fieldSize (len - 2)
      else
        0, min fieldSize len
    in
    (* TODO: this is what the original code does, but we
       might not want to trim, at least on the right side... *)
    "\"" ^ (String.trim (String.sub value start size)) ^ "\""

end

module VariableType = struct

  type t =
    | UNKNOWN
    | GROUP
    | BOOLEAN
    | NUMERIC_DISPLAY
    | NUMERIC_BINARY
    | NUMERIC_PACKED
    | NUMERIC_FLOAT
    | NUMERIC_DOUBLE
    | NUMERIC_L_DOUBLE
    | NUMERIC_FP_DEC64
    | NUMERIC_FP_DEC128
    | NUMERIC_FP_BIN32
    | NUMERIC_FP_BIN64
    | NUMERIC_FP_BIN128
    | NUMERIC_COMP5
    | NUMERIC_EDITED
    | ALNUM
    | ALPHANUMERIC
    | ALPHANUMERIC_ALL
    | ALPHANUMERIC_EDITED
    | NATIONAL
    | NATIONAL_EDITED
    | INTEGER
    | BIT (* internal use *)
    | POINTER (* internal use *)

  let ofInt i =
    match i with
    | 0x00 -> UNKNOWN
    | 0x01 -> GROUP
    | 0x02 -> BOOLEAN
    | 0x10 -> NUMERIC_DISPLAY
    | 0x11 -> NUMERIC_BINARY
    | 0x12 -> NUMERIC_PACKED
    | 0x13 -> NUMERIC_FLOAT
    | 0x14 -> NUMERIC_DOUBLE
    | 0x15 -> NUMERIC_L_DOUBLE
    | 0x16 -> NUMERIC_FP_DEC64
    | 0x17 -> NUMERIC_FP_DEC128
    | 0x18 -> NUMERIC_FP_BIN32
    | 0x19 -> NUMERIC_FP_BIN64
    | 0x1A -> NUMERIC_FP_BIN128
    | 0x1B -> NUMERIC_COMP5
    | 0x24 -> NUMERIC_EDITED
    | 0x20 -> ALNUM
    | 0x21 -> ALPHANUMERIC
    | 0x22 -> ALPHANUMERIC_ALL
    | 0x23 -> ALPHANUMERIC_EDITED
    | 0x40 -> NATIONAL
    | 0x41 -> NATIONAL_EDITED
    | _    -> UNKNOWN

  let ofString s =
    match s with
    | "cob_u8_t" -> GROUP
    | "int"      -> INTEGER
    | _          -> UNKNOWN

  let toString t =
    match t with
    | UNKNOWN             -> "unknown"
    | GROUP               -> "group"
    | BOOLEAN             -> "boolean"
    | NUMERIC_DISPLAY     -> "numeric display"
    | NUMERIC_BINARY      -> "numeric binary"
    | NUMERIC_PACKED      -> "numeric packed"
    | NUMERIC_FLOAT       -> "numeric float"
    | NUMERIC_DOUBLE      -> "numeric double"
    | NUMERIC_L_DOUBLE    -> "numeric long double"
    | NUMERIC_FP_DEC64    -> "numeric fpdec64"
    | NUMERIC_FP_DEC128   -> "numeric fpdec128"
    | NUMERIC_FP_BIN32    -> "numeric fpbin32"
    | NUMERIC_FP_BIN64    -> "numeric fpbin64"
    | NUMERIC_FP_BIN128   -> "numeric fpbin128"
    | NUMERIC_COMP5       -> "numeric comp5"
    | NUMERIC_EDITED      -> "numeric edited"
    | ALNUM               -> "alnum"
    | ALPHANUMERIC        -> "alphanumeric"
    | ALPHANUMERIC_ALL    -> "alphanumeric all"
    | ALPHANUMERIC_EDITED -> "alphanumeric edited"
    | NATIONAL            -> "national"
    | NATIONAL_EDITED     -> "national edited"
    | INTEGER             -> "integer"
    | BIT                 -> "bit"
    | POINTER             -> "pointer"

end

module VariableDetail = struct

  type t = {
    type_ : string;
    name : string;
    value : string;
  }

end

module Attribute = struct

  type t = {
    cName : string;
    type_ : VariableType.t;
    digits : int;
    scale : int;
    flags : CobFlag.Set.t;
  }

  type type_arg =
    | TypeInt of int
    | TypeStr of string

  let create cName type_ digits scale int_flags =
    let type_ =
      match type_ with
      | TypeInt i -> VariableType.ofInt i
      | TypeStr s -> VariableType.ofString s
    in
    let flags = CobFlag.Set.ofInt int_flags in
    { cName; type_; digits; scale; flags }

  let has a flag =
    CobFlag.Set.mem flag a.flags

  let getDetails (a : t) size =
    let open VariableType in
    let open VariableDetail in
    match a.type_ with
    | BOOLEAN ->
        BIT, [ { type_ = "number"; name = "digits";
                 value = string_of_int a.digits } ]
    | NUMERIC_BINARY when has a IS_POINTER ->
        POINTER, [ { type_ = "number"; name = "digits";
                     value = string_of_int a.digits } ]
    | NUMERIC_DISPLAY
    | NUMERIC_PACKED
    | NUMERIC_BINARY
    | NUMERIC_FLOAT
    | NUMERIC_DOUBLE
    | NUMERIC_EDITED ->
        a.type_, [
          { type_ = "boolean"; name = "signed";
            value = string_of_bool (has a HAVE_SIGN) };
          { type_ = "number"; name = "digits";
            value = string_of_int a.digits };
          { type_ = "number"; name = "scale";
            value = string_of_int a.scale }
        ]
    | GROUP
    | ALNUM
    | ALPHANUMERIC_ALL
    | ALPHANUMERIC
    | NATIONAL ->
        a.type_, [ { type_ = "number"; name = "size";
                     value = string_of_int size } ]
    | UNKNOWN
    | NUMERIC_L_DOUBLE
    | NUMERIC_FP_DEC64 | NUMERIC_FP_DEC128
    | NUMERIC_FP_BIN32 | NUMERIC_FP_BIN64 | NUMERIC_FP_BIN128
    | NUMERIC_COMP5
    | ALPHANUMERIC_EDITED
    | NATIONAL_EDITED
    | INTEGER
    | BIT
    | POINTER ->
        a.type_, []

  let format a value fieldSize =
    if value = "" then
      None
    else
      match a.type_ with
      | NUMERIC_DISPLAY ->
          Some (NumericValueParser.format value fieldSize a.scale a.flags)
      | GROUP
      | NUMERIC_EDITED
      | ALPHANUMERIC | ALPHANUMERIC_EDITED
      | NATIONAL | NATIONAL_EDITED ->
          Some (AlphanumericValueParser.format value fieldSize)
      | UNKNOWN
      | BOOLEAN
      | NUMERIC_BINARY | NUMERIC_COMP5 | NUMERIC_PACKED
      | NUMERIC_FLOAT | NUMERIC_DOUBLE | NUMERIC_L_DOUBLE
      | NUMERIC_FP_DEC64 | NUMERIC_FP_DEC128
      | NUMERIC_FP_BIN32 | NUMERIC_FP_BIN64 | NUMERIC_FP_BIN128
      | ALNUM | ALPHANUMERIC_ALL
      | INTEGER
      | BIT
      | POINTER ->
          Log.error ["Unsupported type for format: "; String.escaped value];
          None

  (* When value is obtained from a field (either using f_N.data
     or cob_get_field_str(f_N), it is prefixed by 0x... ; otherwise
     if it is raw storage (b_N), it has no prefix. It may also be
     suffixed by a repeat count, and also by "...".
     This function allows to remove the prefix when present,
     expand the repeat count, and remove the final "..." *)
  let parseCommon value =
    begin
      if value = "" then
        None
      else if String.starts_with ~prefix:"0x" value then
        CobolFieldDataParser.parse value
      else
        CobolFieldDataParser.parseRepeat value
    end |>
    Option.map (fun value ->
        if String.ends_with ~suffix:"..." value then
          String.sub value 0 (String.length value - 3)
        else
          value
      )

  (* Only called by DebuggerVariable.setValue, which is called when
     processing stack variables, or when updating variable description
     (in this case, only when cob_get_field_str is missing) *)
  let parse a value fieldSize =
    Option.bind (parseCommon value) (fun value ->
        match a.type_ with
        | NUMERIC_DISPLAY ->
            Some (NumericValueParser.parse value fieldSize a.scale a.flags)
        | NUMERIC_EDITED
        | ALNUM | ALPHANUMERIC
        | ALPHANUMERIC_ALL | ALPHANUMERIC_EDITED
        | NATIONAL | NATIONAL_EDITED ->
            Some (AlphanumericValueParser.parse value fieldSize)
        | INTEGER
        | GROUP ->
            Some (value)
        | UNKNOWN
        | BOOLEAN
        | NUMERIC_BINARY | NUMERIC_COMP5 | NUMERIC_PACKED
        | NUMERIC_FLOAT | NUMERIC_DOUBLE | NUMERIC_L_DOUBLE
        | NUMERIC_FP_DEC64 | NUMERIC_FP_DEC128
        | NUMERIC_FP_BIN32 | NUMERIC_FP_BIN64 | NUMERIC_FP_BIN128
        | BIT
        | POINTER ->
            Log.emit ["Unsupported type for parse: "; String.escaped value];
            None
  )

  let reLeadingZeroes =
    Str.regexp {|^[-+]?0*|}

  let removeLeadingZeroes value =
    if String.length value <= 0 ||
       String.starts_with ~prefix:"0x" value ||
       String.starts_with ~prefix:"\"" value then
      value
    else
      if Util.matches reLeadingZeroes value then
        let sign =
          if String.starts_with ~prefix:"-" value then "-"
          else ""
        in
        let fixed_val = Str.replace_first reLeadingZeroes "" value in
        if String.length fixed_val <= 0 ||
           String.starts_with ~prefix:"." fixed_val then
          sign ^ "0" ^ fixed_val
        else
          sign ^ fixed_val
      else
        value

  (* Only called by DebuggerVariable.setValuePreformatted, which is called when
     updating variable description when cob_get_field_str is present *)
  let parsePreformatted a value =
    Option.bind (parseCommon value) (fun value ->
        match a.type_ with
        | BOOLEAN
        | NUMERIC_DISPLAY
        | NUMERIC_BINARY | NUMERIC_COMP5 | NUMERIC_PACKED
        | NUMERIC_FLOAT | NUMERIC_DOUBLE | NUMERIC_L_DOUBLE
        | NUMERIC_FP_DEC64 | NUMERIC_FP_DEC128
        | NUMERIC_FP_BIN32 | NUMERIC_FP_BIN64 | NUMERIC_FP_BIN128 ->
            Some (removeLeadingZeroes (Util.stringSub value 1 1))
        | UNKNOWN
        | GROUP
        | NUMERIC_EDITED
        | ALNUM | ALPHANUMERIC
        | ALPHANUMERIC_ALL | ALPHANUMERIC_EDITED
        | NATIONAL | NATIONAL_EDITED
        | INTEGER
        | BIT
        | POINTER ->
            Some (value);
  )

end

module DebuggerVariable = struct

  type t = {
    cobName : string;
    cName : string;
    funName : string; (* actually namespace : <global> or function name *)
    rootCFile : string;
    isField : bool;
    attribute : Attribute.t;
    size : int;
    mutable value : string;
    mutable parent : t option;
    mutable children : t StrMap.t;
    displayableType : string;
    details : VariableDetail.t list;
  }

  let create cobName cName funName rootCFile isField attribute size =
    let type_, details = Attribute.getDetails attribute size in
    { cobName; cName; funName; rootCFile;
      isField; attribute; size; value = "unset";
      parent = None; children = StrMap.empty;
      displayableType = VariableType.toString type_; details }

  let addChild dv child =
    assert (child.parent = None);
    child.parent <- Some (dv);
    dv.children <- StrMap.add child.cobName child dv.children

  let hasChildren dv =
    StrMap.cardinal dv.children > 0

  (* Update the variable's value.
     Called mainly on raw storage (locals/stack), and also
     on fields when cob_get_field_str is absent.
     Called by Mi2.getLocalStorageVariables and Mi2.evalVariable. *)
  let setValue dv value =
    dv.value <- Option.value ~default:"null"
        (Attribute.parse dv.attribute value dv.size)

  (* Update the variable's value, assuming preformatted input.
     Called on fields processed with cob_get_field_str, when present.
     Called by Mi2.evalVariable. *)
  let setValuePreformatted dv value =
    dv.value <- Option.value ~default:"null"
        (Attribute.parsePreformatted dv.attribute value)

  (* Format a value so that it can be stored.
     Used for raw storage (b_N), and also on fields
     when cob_put_field_str is absent (f_N.data).
     Called by Mi2.setDebuggerVariable. *)
  let formatValue dv value =
    Option.value ~default:""
      (Attribute.format dv.attribute value dv.size)

end
