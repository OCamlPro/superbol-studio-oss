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

(** {1 Tree Visitors}

    Tree visitors as implemented here are objects that define one method for
    each type of node to be visited.  Such visitors are associated (and
    implemented using) a set of functions that are visiting starting points.

    {2 Conventions}

    In the module of a visitor [V]:

    - the (currently unique) visitor is defined as a class [folder];

    - the action to be performed when visiting (folding) a type [t] is specified
      using a method [fold_t] of [folder];

    - the starting point for a type [t] is [V.fold_t].

    {b Note:} a type {!type:Srcloc.with_loc} is denoted with a prime ([']).  For
    instance, the starting point for a node of type [t with_loc] will be
    [V.fold_t'].  This convention also guides the naming of utilities
    below.  *)

open Srcloc.TYPES

(** {2 Types}

    The type of actions depends on the kind of visitor.  For now, only a folding
    visitor is available: *)

(** Action to be performed when visiting a node: *)
type 'a folding_action =
  | SkipChildren of 'a  (** do not visit any of the node's children, and continue
                           with the given value *)
  | DoChildren of 'a                      (** visit the node's children *)
  | DoChildrenAndThen of 'a * ('a -> 'a)     (** visit the node's children, and then
                                             call the given function *)

(** {3 Utilities for writing visitors} *)

module INFIX = struct
  (** Temporary binding, until `|>` is properly indented like a monadic operator
      by ocp-indent. *)
  let ( >> ) = ( |> )
end
open INFIX

let in_testsuite = ref false

let report =                        (* to be kept until visitors are complete *)
  let module REPORTED =
    Hashtbl.Make (struct
      type t = string * string * int * string
      let equal = (=)
      let hash = Hashtbl.hash
    end)
  in
  let reported_table = lazy (REPORTED.create 17) in
  fun k file_name module_name line_num func_name ->
    let tbl = Lazy.force reported_table in
    let file_name =
      if !in_testsuite then Filename.basename file_name else file_name in
    let line_num = if !in_testsuite then 0 else line_num in
    if not (REPORTED.mem tbl (file_name, module_name, line_num, func_name))
    then begin
      Pretty.error "@[<2>%s:%u:@ (%s.%s):@ %s@ visitor@ implementation@]@."
        file_name line_num module_name func_name k;
      REPORTED.add tbl (file_name, module_name, line_num, func_name) ()
    end

(** {3 Specific visitors} *)

(** Visitor that accumulates over the nodes of the tree. *)
module Fold = struct

  type 'a action = 'a folding_action
  type ('x, 'a) fold = 'x -> 'a -> 'a folding_action

  (** Some combinators to write more readable folding visitors *)

  let skip_children x = SkipChildren x
  let skip = skip_children                                           (* alias *)
  let do_children x = DoChildren x
  let proceed = do_children                                          (* alias *)
  let do_children_and_then x f = DoChildrenAndThen (x, f)
  let proceed_and_then = do_children_and_then                        (* alias *)

  let default _ = do_children                               (* default action *)

  (** Action handling *)

  (** [handle fold continue node acc] first calls [fold node acc], and then
      behaves according to the action returned. *)
  let handle (fold: 'x -> 'a -> 'a action) ~(continue: 'x -> 'a -> 'a) n x =
    match fold n x with
    | SkipChildren x -> x
    | DoChildren x -> continue n x
    | DoChildrenAndThen (x, f) -> continue n x >> f

  (** [leaf fold node acc] calls [fold node acc] and returns immediately (after
      executing the post action, if [fold] returns {!DoChildrenAndThen}). *)
  let leaf (fold: 'x -> 'a -> 'a action) =
    handle fold ~continue:(fun _ -> Fun.id)

  (** Base folding visitor. *)
  class ['a] folder = object
    method fold_bool: bool -> 'a -> 'a action = default
    method fold_char: char -> 'a -> 'a action = default
    method fold_int: int -> 'a -> 'a action = default
    method fold_string: string -> 'a -> 'a action = default
    method fold_option: 'x. 'x option -> 'a -> 'a action = default
    method fold_list: 'x. 'x list -> 'a -> 'a action = default
    method fold': 'x. 'x with_loc -> 'a -> 'a action = default
  end

  (** Entry points for folding over some basic types. *)

  let fold_bool   (v: _ #folder) = leaf v#fold_bool
  let fold_char   (v: _ #folder) = leaf v#fold_char
  let fold_int    (v: _ #folder) = leaf v#fold_int
  let fold_string (v: _ #folder) = leaf v#fold_string

  (** Generic entry points. *)

  let fold_option ~fold (v: _ #folder) =
    handle v#fold_option
      ~continue:(Option.fold ~none:Fun.id ~some:(fold v))

  let fold_list ~fold (v: _ #folder) =
    handle v#fold_list
      ~continue:(fun l x -> List.fold_left (fun x a -> fold v a x) x l)

  let fold' ~fold (v: _ #folder) =
    handle v#fold' ~continue:(fun { payload; _ } -> fold v payload)

  let fold_string' (v: _ #folder) =
    fold' ~fold:fold_string v

  let fold_string'_opt (v: _ #folder) =
    fold_option ~fold:fold_string' v

  let fold_with_loc_list ~fold (v: _ #folder) =
    fold_list v ~fold:(fold' ~fold)

  (** Helper to shorten definitions for traversal of nodes with source
      locations *)
  (* NOTE: we consider the traversal of `t with_loc` as a whole before the
     generic traversal of `_ with_loc` via [fold'].  Maybe doing it the other
     way round would be more intuitive? *)
  let handle' vfold ~fold (v: _ #folder) =
    handle vfold ~continue:(fold' ~fold v)

  let leaf' vfold =
    handle' vfold ~fold:(fun _ _ -> Fun.id)

  (* --- *)

  (** Reports a missing folding visitor implementation {e once}. *)
  let todo  a b c d _ x = report "missing" a b c d; x

  (** Reports a partial folding visitor implementation {e once}. *)
  let partial a b c d x = report "partial" a b c d; x

end

(* --- *)

(** Folder that carries a context *)
module Fold_with_context = struct

  (* TODO: to be extended to the ASTs if really needed; priority is in completing
     the basic one above first *)

  type 'a action = 'a folding_action

  let do_children _ctx x = DoChildren x
  let proceed = do_children                                          (* alias *)

  let default _ = do_children                               (* default action *)

  (* Action handling *)

  let handle (fold: 'x -> 'c -> 'a -> 'a action) ~(continue: 'x -> 'c -> 'a -> 'a) ctx n x =
    match fold ctx n x with
    | SkipChildren x -> x
    | DoChildren x -> continue ctx n x
    | DoChildrenAndThen (x, f) -> continue ctx n x >> f

  let leaf (fold: 'x -> 'c -> 'a -> 'a action) =
    handle fold ~continue:(fun _ctx _ -> Fun.id)

  class ['a, 'c] folder = object
    method fold_bool: 'c -> bool -> 'a -> 'a action = default
    method fold_char: 'c -> char -> 'a -> 'a action = default
    method fold_int: 'c -> int -> 'a -> 'a action = default
    method fold_string: 'c -> string -> 'a -> 'a action = default
    method fold_option: 'x. 'c -> 'x option -> 'a -> 'a action = default
    method fold_list: 'x. 'c -> 'x list -> 'a -> 'a action = default
  end

  let fold_bool   (v: (_, _) #folder) = leaf v#fold_bool
  let fold_char   (v: (_, _) #folder) = leaf v#fold_char
  let fold_int    (v: (_, _) #folder) = leaf v#fold_int
  let fold_string (v: (_, _) #folder) = leaf v#fold_string
  let fold_option ~fold (v: (_, _) #folder) =
    handle v#fold_option
      ~continue:(Option.fold ~none:(fun _ctx -> Fun.id) ~some:(fold v))
  let fold_list ~fold (v: (_, _) #folder) =
    handle v#fold_list
      ~continue:(fun ctx l x -> List.fold_left (fun x a -> fold v ctx x a) x l)

end

(* --- *)

(* Fold by default. *)
include Fold
