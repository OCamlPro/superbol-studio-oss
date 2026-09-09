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

module Value = struct

  type t =
    | String of string
    | List of v_list
    | Tuple of kv_list
    | Null (* NOTE: not an actual MI2 value *)

  and v_list =
    | Empty
    | Values of t list
    | KeyValues of kv_list

  and kv_list = (string * t) list

  let string ?(def="") v =
    match v with
    | String (s) -> s
    | _ -> def

  let int ?(def=0) v =
    match v with
    | String (s) -> (try int_of_string s with _ -> def)
    | _ -> def

  let opt f v =
    match v with
    | Null -> None
    | _ -> Some (f v)

  let map f v =
    match v with
    | List (Empty) ->
        []
    | List (Values vl) ->
        List.map f vl
    | _ ->
        []

  let fold f acc v =
    match v with
    | List (Empty) ->
        acc
    | List (Values vl) ->
        List.fold_left (fun acc v -> f acc v) acc vl
    | _ ->
        acc

  let kvFold f acc v =
    match v with
    | List (Empty) ->
        acc
    | List (KeyValues kvl)
    | Tuple (kvl) ->
        List.fold_left (fun acc (k, v) -> f acc k v) acc kvl
    | _ ->
        acc

  let kvFilterMap f v =
    match v with
    | List (Empty) ->
        []
    | List (KeyValues kvl)
    | Tuple (kvl) ->
        List.filter_map (fun (k, v) -> f k v) kvl
    | _ ->
        []

  let field k v =
    match v with
    | List (KeyValues kvl)
    | Tuple (kvl) ->
        begin
          match List.assoc_opt k kvl with
          | Some (v) -> v
          | None -> Null
        end
    | _ ->
        Null

  let rec print fmt v =
    match v with
    | String s ->
        Format.fprintf fmt "\"%s\"" s
    | List (Empty) ->
        Format.fprintf fmt "[]"
    | List (Values vl) ->
        Format.fprintf fmt "[%a]" printValueList vl
    | List (KeyValues kvl) ->
        Format.fprintf fmt "[%a]" printKeyValueList kvl
    | Tuple (kvl) ->
        Format.fprintf fmt "{%a}" printKeyValueList kvl
    | Null ->
        Format.fprintf fmt "null"

  and printValueList fmt vl =
    let vl =
      match vl with
      | v :: vl -> print fmt v; vl
      | _ -> vl
    in
    List.iter (fun v ->
        Format.fprintf fmt ",%a" print v
      ) vl

  and printKeyValueList fmt kvl =
    let kvl =
      match kvl with
      | (k, v) :: kvl ->
          Format.fprintf fmt "%s=%a" k print v; kvl
      | _ ->
          kvl
    in
    List.iter (fun (k, v) ->
        Format.fprintf fmt ",%s=%a" k print v
      ) kvl

  let toString v =
    Format.asprintf "%a" print v

end

type async_kind =
  | Exec (* * *)
  | Status (* + *) (* never saw these in actual gdb sessions *)
  | Notify (* = *)

type stream_kind =
  | Console (* ~ *)
  | Target (* @ *)
  | Log (* & *)

type result_class =
  | Done (* followed by a list of results (key/val)*)
  | Running (* followed by nothing *)
  | Connected (* followed by nothing *)
  | Error (* followed by msg=c-string (old versions: just c-string) *)
  | Exit (* followed by nothing *)

type async_class =
  (* Only for async/exec *)
  | Running
  | Stopped
  (* Only for async/notify *)
  | ThreadGroupAdded
  | ThreadGroupRemoved
  | ThreadGroupStarted
  | ThreadGroupExited
  | ThreadCreated
  | ThreadExited
  | ThreadSelected
  | LibraryLoaded
  | LibraryUnloaded
  | TraceframeChange
  | TvsCreated
  | TvsDeleted
  | TvsModified
  | BreakPointCreated
  | BreakPointModified
  | BreakPointDeleted
  | RecordStarted
  | RecordStopped
  | CmdParamChanged
  | MemoryChanged
  | Other of string

type result = {
  token : int option;
  class_ : result_class;
  results : Value.kv_list;
 }

type async = {
  token : int option;
  kind : async_kind;
  class_ : async_class;
  results : Value.kv_list;
 }

type stream = {
  kind : stream_kind;
  output : string;
}

type t =
  | Result of result
  | Async of async
  | Stream of stream

let rfield (r : result) k =
  match List.assoc_opt k r.results with
  | None -> Value.Null
  | Some v -> v

let afield (a : async) k =
  match List.assoc_opt k a.results with
  | None -> Value.Null
  | Some v -> v

let stringOfAsyncKind = function
  | Exec   -> "*"
  | Status -> "+"
  | Notify -> "="

let asyncKindOfString = function
  | "*" -> Exec
  | "+" -> Status
  | "=" -> Notify
  | _ -> failwith "Invalid async kind"

let stringOfStreamKind = function
  | Console -> "~"
  | Target  -> "@"
  | Log     -> "&"

let streamKindOfString = function
  | "~" -> Console
  | "@" -> Target
  | "&" -> Log
  | _ -> failwith "Invalid stream kind"

let stringOfResultClass = function
  | Done      -> "done"
  | Running   -> "running"
  | Connected -> "connected"
  | Error     -> "error"
  | Exit      -> "exit"

let resultClassOfString = function
  | "done"      -> Done
  | "running"   -> Running
  | "connected" -> Connected
  | "error"     -> Error
  | "exit"      -> Exit
  | _ -> failwith "Invalid result class"

let stringOfAsyncClass = function
  | Running            -> "running"
  | Stopped            -> "stopped"
  | ThreadGroupAdded   -> "thread-group-added"
  | ThreadGroupRemoved -> "thread-group-removed"
  | ThreadGroupStarted -> "thread-group-started"
  | ThreadGroupExited  -> "thread-group-exited"
  | ThreadCreated      -> "thread-created"
  | ThreadExited       -> "thread-exited"
  | ThreadSelected     -> "thread-selected"
  | LibraryLoaded      -> "library-loaded"
  | LibraryUnloaded    -> "library-unloaded"
  | TraceframeChange   -> "traceframe-changed"
  | TvsCreated         -> "tvs-created"
  | TvsDeleted         -> "tvs-deleted"
  | TvsModified        -> "tvs-modified"
  | BreakPointCreated  -> "breakpoint-created"
  | BreakPointModified -> "breakpoint-modified"
  | BreakPointDeleted  -> "breakpoint-deleted"
  | RecordStarted      -> "record-started"
  | RecordStopped      -> "record-stopped"
  | CmdParamChanged    -> "cmd-param-changed"
  | MemoryChanged      -> "memory-changed"
  | Other (s)          -> s

let asyncClassOfString = function
  | "running"              -> Running
  | "stopped"              -> Stopped
  | "thread-group-added"   -> ThreadGroupAdded
  | "thread-group-removed" -> ThreadGroupRemoved
  | "thread-group-started" -> ThreadGroupStarted
  | "thread-group-exited"  -> ThreadGroupExited
  | "thread-created"       -> ThreadCreated
  | "thread-exited"        -> ThreadExited
  | "thread-selected"      -> ThreadSelected
  | "library-loaded"       -> LibraryLoaded
  | "library-unloaded"     -> LibraryUnloaded
  | "traceframe-changed"   -> TraceframeChange
  | "tvs-created"          -> TvsCreated
  | "tvs-deleted"          -> TvsDeleted
  | "tvs-modified"         -> TvsModified
  | "breakpoint-created"   -> BreakPointCreated
  | "breakpoint-modified"  -> BreakPointModified
  | "breakpoint-deleted"   -> BreakPointDeleted
  | "record-started"       -> RecordStarted
  | "record-stopped"       -> RecordStopped
  | "cmd-param-changed"    -> CmdParamChanged
  | "memory-changed"       -> MemoryChanged
  | s                      -> Other (s)


let printTokOpt fmt = function
  | Some (tok) -> Format.fprintf fmt "%d" tok
  | None -> ()

let printResults fmt = function
  | [] -> ()
  | res -> Format.fprintf fmt ",%a" Value.printKeyValueList res

let print fmt r =
  match r with
  | Result { token; class_; results } ->
      Format.fprintf fmt "%a^%s%a\n"
        printTokOpt token
        (stringOfResultClass class_) printResults results
  | Async { token; kind; class_; results } ->
      Format.fprintf fmt "%a%s%s%a\n"
        printTokOpt token (stringOfAsyncKind kind)
        (stringOfAsyncClass class_) printResults results
  | Stream { kind; output } ->
      Format.fprintf fmt "%s,\"%s\"\n"
        (stringOfStreamKind kind) output

let toString r =
  Format.asprintf "%a" print r
