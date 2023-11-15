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

(** This module implements functions to type the COBOL data.*)

module OLD = struct

  module Types = struct
    include Types
    type group = Group.t

    include Compilation_unit.TYPES
    type compilation_units = Compilation_unit.SET.t
    type +'a compilation_units_map = 'a Compilation_unit.MAP.t
  end

  include Env
  module Group = Group
  module Mangling = Mangling
  module Picture = Data_picture
  module Qualmap = Qualmap
  module Compilation_unit = Compilation_unit
end

module Memory = Data_memory
module Types = Data_types
module Item = Data_item
module Picture = Data_picture
module Printer = Data_printer
