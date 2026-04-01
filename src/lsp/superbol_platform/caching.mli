(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Helpers to construct persistent cache storage where each named individual
    item is stored as a single file.

    I/O operations are not performed via a generic platform.  Instead, they are
    based on {!Ez_file.V1} (indirectly {!Unix}). *)

(** [write_cache_item ?version_tag oc item] dumps a version tag, followed by
    [item], on the stream [oc].  Relies on {!Marshal}. *)
val write_cache_item: ?version_tag:string -> out_channel -> 'a -> unit

(** [read_cache_item ?version_tag ic] reads and checks a version tag from [oc],
    and then reads an [item] from the same stream.  Relies on {!Marshal}. *)
val read_cache_item: ?version_tag:string -> in_channel -> 'a

(** [save_named_item_cache ~cache_dir ~item_name ~write_item item] saves an
    [item] with a unique name [item_name] in a cache file within [cache_dir].
    [write_item] is used to dump the item: it should typically be a call to
    {!write_cache_item}. *)
val save_named_item_cache
  : cache_dir:string
  -> item_name:string
  -> write_item:('a -> out_channel -> unit)
  -> 'a
  -> unit

(** [load_named_item_cache ~cache_dir ~item_name ~read_item] loads an item named
    [item_name] from a cache file in [cache_dir].  [read_item] is used to read
    the item: it should typically be a call to {!read_cache_item}.

    Raises {!Sys_error} in case no item with name [item_name] is found in cache
    directory [cache_dir]. *)
val load_named_item_cache
  : cache_dir:string
  -> item_name:string
  -> read_item:(in_channel -> 'a) -> 'a
