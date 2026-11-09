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

open Ezcmd.V2
open EZCMD.TYPES

type dual_switch = [`enable_disable | `with_without | `boolean]
type single_switch = [`use | `force]

let dual_switch ?descr (kind: dual_switch) ~name ~default =
  let switch = ref default in
  let set, unset = match kind with
    | `enable_disable -> "Enable", "Disable"
    | `with_without   -> "With", "Without"
    | `boolean        -> "Set", "Clear"
  in
  let arg_set, arg_unset = match kind with
    | `enable_disable | `boolean -> name, "no-"^name
    | `with_without -> "with-"^name, "without-"^name
  in
  let default = match kind with
    | `enable_disable when default -> "enabled"
    | `enable_disable              -> "disabled"
    | `with_without when default   -> "with"
    | `with_without                -> "without"
    | `boolean when default        -> "true"
    | `boolean                     -> "false"
  in
  let descr = Option.value descr ~default:name in
  switch,
  [
    [arg_set], Arg.Set switch,
    Pretty.string_to EZCMD.info "%s %s (%s by default)" set descr default;

    [arg_unset], Arg.Clear switch,
    Pretty.string_to EZCMD.info "%s %s (%s by default)" unset descr default;
  ]

let single_switch ?descr (kind: single_switch) ~name ~default =
  let switch = ref default in
  let set = match kind with
    | `use when not default -> "Use"
    | `use -> "Don't use"
    | `force when not default -> "Force"
    | `force -> "Don't force"
  in
  let arg_set = match kind with
    | `use when not default -> name
    | `use -> "no-"^name
    | `force when not default -> "force-"^name
    | `force -> "default-"^name
  in
  let descr = Option.value descr ~default:name in
  switch,
  [
    [arg_set], Arg.Set switch,
    Pretty.string_to EZCMD.info "%s %s" set descr;
  ]

let switch ?descr = function
  | #dual_switch as kind -> dual_switch ?descr kind
  | #single_switch as kind -> single_switch ?descr kind

(** Accepts a plain number of bytes, or one with the usual suffix: [500M],
    [2G], [64k].  Powers of 1024, as the sizes this reports are. *)
let size_of_string s =
  let scale, digits = match s.[String.length s - 1] with
    | 'k' | 'K' -> 1024L, String.length s - 1
    | 'M' | 'm' -> 1048576L, String.length s - 1
    | 'G' | 'g' -> 1073741824L, String.length s - 1
    | _ -> 1L, String.length s
  in
  match Int64.of_string_opt (String.sub s 0 digits) with
  | Some n when n >= 0L -> Ok (Int64.mul n scale)
  | Some _ | None -> Error ()

(** [recording args] is [args], along with a function that lists the arguments
    among them that were given, as they would be given on the command line
    (anonymous arguments excepted).  See the interface. *)
let recording (args: arg_list) =
  let recorded = ref [] in
  let record name value =
    let arg = match name, value with
      | n, None when String.length n = 1 -> "-" ^ n
      | n, None -> "--" ^ n
      | n, Some v when String.length n = 1 -> "-" ^ n ^ v
      | n, Some v -> "--" ^ n ^ "=" ^ v
    in
    recorded := arg :: !recorded
  in
  let flag n f = Arg.Unit (fun () -> f (); record n None)
  and with_value n to_string f = fun v -> f v; record n (Some (to_string v)) in
  let float_to_string = Printf.sprintf "%.17g" in
  let wrap = function
    | [], spec -> spec
    | n :: _, spec ->
        match (spec: Arg.spec) with
        | Unit f -> flag n f
        | Set r -> flag n (fun () -> r := true)
        | Clear r -> flag n (fun () -> r := false)
        | Bool f -> Bool (with_value n string_of_bool f)
        | Set_bool r -> Bool (with_value n string_of_bool (( := ) r))
        | String f -> String (with_value n Fun.id f)
        | Set_string r -> String (with_value n Fun.id (( := ) r))
        | File f -> File (with_value n Fun.id f)
        | Int f -> Int (with_value n string_of_int f)
        | Set_int r -> Int (with_value n string_of_int (( := ) r))
        | Float f -> Float (with_value n float_to_string f)
        | Set_float r -> Float (with_value n float_to_string (( := ) r))
        | Symbol (l, f) -> Symbol (l, with_value n Fun.id f)
        | Anon _ | Anons _ -> spec
  in
  (fun () -> List.rev !recorded),
  List.map (fun (names, spec, info) -> names, wrap (names, spec), info) args
