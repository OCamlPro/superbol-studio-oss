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

open Types

type line = {
  cobFile : string;
  cobLine : int;
  cFile : string;
  cLine : int;
  rootCFile : string;
  funName : string;
  endPerformLine : int;
  cobolLine : string; (* used by stack trace to show cobol code *)
}

type t = {
  cwd : string;
  sourceDirs : string list;
  cToCob : line IntMap.t StrMap.t;
  cobToC : line IntMap.t StrMap.t;
  varsByCobol : DebuggerVariable.t StrMap.t;
  varsByC : DebuggerVariable.t StrMap.t;
  attributes : Attribute.t StrMap.t;
  dataStorages : DebuggerVariable.t StrMap.t;
  loadedLibs : StrSet.t;
}

let empty = {
  cwd = ""; sourceDirs = [];
  cToCob = StrMap.empty;
  cobToC = StrMap.empty;
  varsByCobol = StrMap.empty;
  varsByC = StrMap.empty;
  attributes = StrMap.empty;
  dataStorages = StrMap.empty;
  loadedLibs = StrSet.empty
}

let globalNamespace = "<global>"

let scope a b = a ^ "." ^ b (* for namespaces, attributes and COBOL idents *)

let cScope a b = a ^ "::" ^ b (* for C variables *)

let addLine_aux map keyFile keyLine line =
  StrMap.update keyFile (function
      | None -> Some (IntMap.singleton keyLine line)
      | Some imap -> Some (
          IntMap.update keyLine (function
              | None -> Some line
              | Some l -> Some (if l = line then l else line)
            ) imap)
    ) map

let addLine sm line =
  { sm with
    cToCob = addLine_aux sm.cToCob line.cFile line.cLine line;
    cobToC = addLine_aux sm.cobToC line.cobFile line.cobLine line }

let filterLines_aux map f =
  StrMap.filter_map (fun _keyFile imap ->
      let imap' = IntMap.filter (fun _keyLine line -> f line) imap in
      if IntMap.is_empty imap' then None
      else Some (imap')
    ) map

let filterLines sm f =
  { sm with
    cToCob = filterLines_aux sm.cToCob f;
    cobToC = filterLines_aux sm.cobToC f }

(* On Win32, if a filename exists both as uppercase and lowercase,
   use the lowercase name, otherwise use the name as provided *)
let normalizeExistingFilename f =
  if f <> "" && Node.Process.platform = "win32" then
    let lc = String.lowercase_ascii f in
    let uc = String.uppercase_ascii f in
    if Sys.file_exists lc && Sys.file_exists uc then lc
    else f
  else f

(* returns only the filename (no path) *)
let getCFile f =
  Filename.remove_extension (Filename.basename f) ^ ".c"

let cobEncodeInvalidChars s =
  (* TODO: mimick libcob:encode_invalid_char *)
  Str.global_replace (Str.regexp_string "-") "__" s

let lookupLineInCobolFile =
  let currentCobolFilename = ref "" in
  let currentCobolFileContents = ref [||] in
  fun fileCobol lineCobol ->
    if fileCobol <> "" && !currentCobolFilename <> fileCobol then
      begin
        currentCobolFilename := fileCobol;
        let fileContents =
          try Util.foldFileLines (fun lines l -> l :: lines) [] fileCobol
          with _ -> []
        in
        currentCobolFileContents := Array.of_list (List.rev fileContents)
      end;
    try !currentCobolFileContents.(lineCobol - 1)
    with _ -> ""

let reFileCobol = Str.regexp_case_fold
    {|/\*[ 	]Generated from[ 	]+\([0-9a-z_/. 	\:-]+\)[ 	]+\*/|}

let reFunction = Str.regexp_case_fold
    {|/\*[ 	]Program[ 	]local[ 	]variables[ 	]for[ 	]'\(.*\)'[ 	]\*/|}

let reProcedure = Str.regexp_case_fold
    {|/\*[ 	]Line:[ 	]\([0-9]+\)\([ 	]+:[ 	]Entry[ 	]\)?|}

let reProcedureFix = Str.regexp_case_fold
    {|#line[ 	]\([0-9]+\)[ 	]".*\.c"|}

let reAttribute = Str.regexp_case_fold
    {|static[ 	]const[ 	]cob_field_attr[ 	]\(a_[0-9]+\).*{\(0x[0-9]+\),[ 	]*\([0-9-]*\),[ 	]*\([0-9-]*\),[ 	]*\(0x[0-9][0-9][0-9][0-9]\),.*|}

let reDataStorage = Str.regexp_case_fold
    {|static[ 	]+\([^ 	]*\)[ 	]+\(b_[0-9]+\)\(;\|\[[0-9]+\]\).*/\*[ 	]+\([0-9a-z_ 	-]+\)[ 	]+\*/|}

let reField = Str.regexp_case_fold
    {|static[ 	]+cob_field[ 	]+\([0-9a-z_]+\)[ 	]+=[ 	]+{\([0-9]+\),[ 	]+\([0-9a-z_]+\).+\&\(a_[0-9]+\).*/\*[ 	]+\([0-9a-z_\-]+\)[ 	]+\*/|}

let reFileInclude = Str.regexp_case_fold
    {|#include[ 	]+"\([0-9a-z_. 	-]+\)"|}

let reSubroutine = Str.regexp_case_fold
    {|[ 	]Perform[ 	]|}

let reFramePtr = Str.regexp_case_fold
    {|frame_ptr--;|}

let rec create cwd cobFiles sourceDirs =

  let cwd = Node.Fs.realpathSync (Node.Path.resolve [cwd]) in

  let sourceDirs =
    List.filter_map (fun sd ->
        let sd = Node.Path.resolve [cwd; sd] in
        if Sys.file_exists sd then
          Some (normalizeExistingFilename (Node.Fs.realpathSync sd))
        else
          None
      ) sourceDirs
  in

  let sourceDirs = sourceDirs @ [cwd] in

  let sm = { empty with cwd; sourceDirs } in

  List.fold_left (fun sm cobolFile ->
      let cFile_opt = lookupSourceFile sm (getCFile cobolFile) in
      match cFile_opt with
      | Some cFile -> register sm cFile
      | None -> sm
    ) sm cobFiles

and lookupSourceFile sm file =
  List.find_map (fun sd ->
      let filePath = Filename.concat sd file in
      if Sys.file_exists filePath then
        Some (normalizeExistingFilename filePath)
      else
        None
    ) sm.sourceDirs

and addLib sm libFile =
  if StrSet.mem libFile sm.loadedLibs then
    sm, false
  else
    let sm = { sm with loadedLibs = StrSet.add libFile sm.loadedLibs } in
    let cFile_opt = lookupSourceFile sm (getCFile (libFile)) in
    match cFile_opt with
    | Some cFile -> register sm cFile, true
    | None -> sm, false

and remLib sm libFile =
  if not (StrSet.mem libFile sm.loadedLibs) then
    sm, false
  else
    let sm = { sm with loadedLibs = StrSet.remove libFile sm.loadedLibs } in
    let cFile_opt = lookupSourceFile sm (getCFile (libFile)) in
    match cFile_opt with
    | Some cFile -> unregister sm cFile, true
    | None -> sm, false

and unregister sm cFile =
  let cFile, _cleanedFile = ensureAbsolute sm cFile in
  let sm = filterLines sm (fun l -> l.rootCFile <> cFile) in
  (* TODO: remove variables_by_c and variables_by_cobol *)
  sm

and ensureAbsolute sm cFile =
  let cFile =
    if Filename.is_relative cFile then
      Node.Path.resolve [sm.cwd; cFile]
    else cFile
  in
  let basename = Filename.basename cFile in
  let cleanedFile = Util.removeAfterAndIncluding basename ".c" in
  normalizeExistingFilename cFile, cleanedFile

and register sm cFile =
  parse sm cFile

and parse sm cFile =

    let open struct
      type global_acc = {
        lines: line list;
        funName: string;
        performLine: bool;
      }
      type local_acc = {
        cobFile: string;
        cLine: int;
      }
    end in

    let rec parse_aux sm gacc cFile rootCFile =
      let cFile, cleaned_file = ensureAbsolute sm cFile in
      let rootCFile = if rootCFile = "" then cFile else rootCFile in
      let lacc = { cobFile = ""; cLine = 0 } in
      let sm, gacc, _lacc = Util.foldFileLines (fun (sm, gacc, lacc) line ->
          let sm, gacc, lacc =

            (* debug "processing: %s" line; *)
            (* Handle includes (.c) *)
            if Util.matches reFileInclude line then
              let cFile = Util.group line 1 in
              let cFile =
                Node.Path.resolve [Filename.dirname rootCFile; cFile] in
              let sm, gacc = parse_aux sm gacc cFile rootCFile in
              sm, gacc, lacc


            (* Find Cobol file for given C file (.c, .c.h, .c.l.h) *)
            (* "Generated from dir/prog.cob" *)
            (* Cobol file name often relative but might be absolute *)
            (* NOTE: for main programs, we know the Cobol file in advance. *)
            (* Look for the following:
               c-file-dir/dir/prog.cob   rel
               c-file-dir/prog.cob       rel
               sources/dir/prog.cob      rel
               sources/prog.cob          rel/abs *)
            else if Util.matches reFileCobol line then
              let filename = Util.group line 1 in
              let filebase = Filename.basename filename in
              let cobFile =
                if not (Filename.is_relative filename) then filename
                else
                  let cobFile =
                    Node.Path.resolve [Filename.dirname cFile; filename] in
                  if Sys.file_exists cobFile then cobFile
                  else
                    let cobFile =
                      Node.Path.resolve [Filename.dirname cFile; filebase] in
                    if Sys.file_exists cobFile then cobFile
                    else Util.unoptString (lookupSourceFile sm filename)
              in
              let cobFile =
                if Sys.file_exists cobFile then
                  normalizeExistingFilename cobFile
                else
                  Util.unoptString (lookupSourceFile sm filebase)
              in
              sm, gacc, { lacc with cobFile }


            (* Determine function/program name (.c.l.h) *)
            (* NOTE: we want the name with _ *)
            else if Util.matches reFunction line then
              let funName = cobEncodeInvalidChars (Util.group line 1) in
              let funName = (String.lowercase_ascii funName) ^ "_" in
              sm, { gacc with funName }, lacc


            (* Match instruction lines between C and COBOL (.c) *)
            else if Util.matches reProcedure line &&
                    not (Util.hasGroup 2) then
              let cobLine = Util.groupInt line 1 in
              let performLine = Util.matches reSubroutine line in
              let cobolLine = lookupLineInCobolFile lacc.cobFile cobLine in
              let line = { cobFile = lacc.cobFile; cobLine;
                           cFile; cLine = lacc.cLine + 2;
                           funName = gacc.funName;
                           endPerformLine = -1;
                           rootCFile;
                           cobolLine;
                         } in
              let lines = line :: gacc.lines in
              sm, { gacc with lines; performLine }, lacc


            (* Fixup COBOL/C line (.c) *)
            else if Util.matches reProcedureFix line then
              let lines = Util.mapHd (fun (l : line) ->
                  if l.funName = gacc.funName &&
                     l.cobFile = lacc.cobFile then
                    let cLine = Util.groupInt line 1 in
                    { l with cLine }
                  else
                    l
                ) gacc.lines
              in

              sm, { gacc with lines }, lacc


            (* Handle end of perform blocks (.c) *)
            else if gacc.performLine && Util.matches reFramePtr line then
              let endPerformLine = lacc.cLine + 1 in
              let lines = Util.mapHd (fun l ->
                  { l with endPerformLine }
                ) gacc.lines
              in
              sm, { gacc with lines }, lacc


            (* Handle attributes (.c.h) *)
            (* static const cob_field_attr a_3 = {0x10, 18, 0, 0x0000, NULL}; *)
            else if Util.matches reAttribute line then
              let attrName = Util.group line 1 in (* a_N *)
              let attribute = Attribute.create attrName
                  (TypeInt (Util.groupInt line 2))
                  (Util.groupInt line 3)
                  (Util.groupInt line 4)
                  (Util.groupInt line 5)
              in
              let attributes =
                StrMap.add (scope cleaned_file attrName)
                  attribute sm.attributes in
              { sm with attributes }, gacc, lacc


            (* Handle dataStorage (.c.l.h (local), .c.h (global)) *)
            else if Util.matches reDataStorage line then
              let type_ = Util.group line 1 in
              let cName = Util.group line 2 in (* b_N *)
              let size = Util.group line 3 in
              let cobName = Util.group line 4 in
              let cobNAME = String.uppercase_ascii cobName in
              let size =
                if size = ";" then 1 (* TODO: or 0/-1 ? *)
                else int_of_string (String.sub size 1 (String.length size - 2))
              in
              let global = gacc.funName = "" in
              let namespace, cName, fullCobName =
                if not global then gacc.funName, cName, cobNAME
                else globalNamespace, cScope ("'" ^ cFile ^ "'") cName,
                     scope ("'" ^ cFile ^ "'") cobNAME
              in
              let attribute = Attribute.create "" (TypeStr type_) 0 0 0 in
              let dataStorage =
                DebuggerVariable.create cobName cName namespace
                  rootCFile false attribute size in
              let dataStorages =
                StrMap.add (scope namespace cName)
                  dataStorage sm.dataStorages in
              let varsByC =
                StrMap.add (scope namespace cName)
                  dataStorage sm.varsByC in
              let varsByCobol =
                StrMap.add (scope namespace fullCobName)
                  dataStorage sm.varsByCobol in
              { sm with dataStorages; varsByC; varsByCobol }, gacc, lacc


            (* Handle fields (.c.l.h (local), .c.h (global)) *)
            else if Util.matches reField line then
              let cName = Util.group line 1 in (* f_N *)
              let size = Util.groupInt line 2 in
              let cRecord = Util.group line 3 in (* b_N *)
              let attrName = Util.group line 4 in (* a_N *)
              let cobName = Util.group line 5 in
              let cobNAME = String.uppercase_ascii cobName in
              let global = gacc.funName = "" in
              let namespace, cName, cRecord =
                if not global then gacc.funName, cName, cRecord
                else globalNamespace, cScope ("'" ^ cFile ^ "'") cName,
                     cScope ("'" ^ cFile ^ "'") cRecord
              in
              let attribute =
                StrMap.find_opt (scope cleaned_file attrName) sm.attributes |>
                Option.value ~default:(Attribute.create
                                         attrName (TypeInt 1) 0 0 0)
              in
              let field =
                DebuggerVariable.create cobName cName namespace
                  rootCFile true attribute size in
              let dataStorage_opt =
                StrMap.find_opt (scope namespace cRecord) sm.dataStorages in
              let storageKey =
                match dataStorage_opt with
                | None ->
                    namespace
                | Some (dataStorage) ->
                    DebuggerVariable.addChild dataStorage field;
                    let cobNAME = String.uppercase_ascii dataStorage.cobName in
                    let fullCobName =
                      if not global then cobNAME
                      else scope ("'" ^ cFile ^ "'") cobNAME
                    in
                    scope namespace fullCobName
              in
              let varsByC =
                StrMap.add (scope namespace cName) field sm.varsByC in
              let varsByCobol =
                StrMap.add (scope storageKey cobNAME) field sm.varsByCobol in
              { sm with varsByC; varsByCobol }, gacc, lacc


            else
              sm, gacc, lacc
          in
          sm, gacc, { lacc with cLine = lacc.cLine + 1 }
        ) (sm, gacc, lacc) cFile
      in
      sm, gacc
    in

    let gacc = { lines = []; funName = ""; performLine = false } in
    let sm, gacc = parse_aux sm gacc cFile "" in
    List.fold_left addLine sm gacc.lines

(* Find a local variable by its C name, of the form:
   - progN_.b_N
   - progN_.f_N *)
let findLocalByC sm funName cName =
  StrMap.find_opt (scope funName cName) sm.varsByC

(* Find a global variable by its C name, of the form:
   - <global>.'/path/to/prog.c.h'::b_N
   - <global>.'/path/to/prog.c.h'::f_N *)
let findGlobalByC sm ?cFile cName =
  let cNameFull =
    match cFile with
    | None | Some ("") -> cName
    | Some (cFile) ->
        let cFile = normalizeExistingFilename cFile in
        cScope ("'" ^ cFile ^ "'") cName
  in
  StrMap.find_opt (scope globalNamespace cNameFull) sm.varsByC

(* Find a local variable by its COBOL name, of the form:
   - progN_.COBNAME
   - progN_.COBNAME.COBNAME *)
let findLocalByCobol sm funName cobName =
  let prefix = funName ^ "." in
  let suffix = "." ^ String.uppercase_ascii cobName in
  StrMap.filter (fun k _v ->
      String.starts_with ~prefix k && String.ends_with ~suffix k
    ) sm.varsByCobol |> StrMap.choose_opt |> Option.map snd

(* Find a global variable by its COBOL name, of the form:
   - <global>.'/path/to/prog.c.h'.COBNAME
   - <global>.'/path/to/prog.c.h'.COBNAME.COBNAME *)
let findGlobalByCobol sm ?cFile cobName =
  let prefix =
    match cFile with
    | None | Some ("") -> globalNamespace ^ "."
    | Some (cFile) ->
        let cFile = normalizeExistingFilename cFile in
        scope globalNamespace ("'" ^ cFile ^ "'") ^ "."
  in
  let suffix = "." ^ String.uppercase_ascii cobName in
  StrMap.filter (fun k _v ->
      String.starts_with ~prefix k && String.ends_with ~suffix k
    ) sm.varsByCobol |> StrMap.choose_opt |> Option.map snd

let hasLineCobol sm cFile cLine =
  let cFile = normalizeExistingFilename cFile in
  let cFile =
    if Filename.is_relative cFile then
      Node.Path.join [sm.cwd; cFile]
    else
      cFile
  in
  StrMap.exists (fun cFile' imap ->
      cFile = cFile' && IntMap.mem cLine imap
    ) sm.cToCob

let getLineCobol sm cFile cLine =
  let cFile = normalizeExistingFilename cFile in
  Option.bind (StrMap.find_opt cFile sm.cToCob)
    (fun imap -> IntMap.find_opt cLine imap)

let hasLineSubroutine sm cFile cLine =
  let cFile = normalizeExistingFilename cFile in
  getLineCobol sm cFile cLine |>
  Option.fold ~none:(-1) ~some:(fun l -> l.endPerformLine)

(* let hasLineC sm cobFile cobLine = *)
(*   StrMap.exists (fun cobFile' imap -> *)
(*       cobFile = cobFile' && IntMap.mem cobLine imap *)
(*     ) sm.cobToC *)

let getLineC sm cobFile cobLine =
  let cobFile = normalizeExistingFilename cobFile in
  Option.bind (StrMap.find_opt cobFile sm.cobToC)
    (fun imap -> IntMap.find_opt cobLine imap)

let print fmt sm =
  StrMap.iter (fun file imap ->
      Format.fprintf fmt "File: %s\n" file;
      IntMap.iter (fun line (tline : line) ->
          Format.fprintf fmt "Line %d -> %s:%d (%s, %d)\n"
            line tline.cobFile tline.cobLine
            tline.funName tline.endPerformLine;
        ) imap
    ) sm.cToCob;

  StrMap.iter (fun file imap ->
      Format.fprintf fmt "File: %s\n" file;
      IntMap.iter (fun line (tline : line) ->
          Format.fprintf fmt "Line %d -> %s:%d (%s, %d)\n"
            line tline.cFile tline.cLine
            tline.funName tline.endPerformLine;
        ) imap
    ) sm.cobToC;

  StrMap.iter (fun an (a : Attribute.t) ->
      Format.fprintf fmt "Attr %s = %s\n" an a.cName;
    ) sm.attributes;

  StrMap.iter (fun cn (v : DebuggerVariable.t) ->
      Format.fprintf fmt "C Var %s = %s %s %s / %s\n"
        cn v.cName v.cobName v.funName
        (VariableType.toString v.attribute.type_)
    ) sm.varsByC;

  StrMap.iter (fun cn (v : DebuggerVariable.t) ->
      Format.fprintf fmt "COB Var %s = %s %s %s / %s\n"
        cn v.cName v.cobName v.funName
        (VariableType.toString v.attribute.type_)
    ) sm.varsByCobol

let toString sm =
  Format.asprintf "%a" print sm
