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

open Lsp_imports
open Lsp_types

(** [save ~config docs] saves the caches of all the given document's
    projects.

    For any project [p] of which at least one document belongs to [docs], any
    document of [p] that does not belong to [docs] is removed from [p]'s
    cache.

    Some notifications about the saving process may be pushed via
    {!Lsp_io.send_notification}.  May raise some IO-related exceptions
    ([Sys_error], [Failure]). *)
val save
  : params: params
  -> Lsp_document.t URIMap.t
  -> unit

(** [load ~rootdir ~config ~layout] pre-loads cached documents pertaining to a
    project located in [rootdir], with project directory layout [layout].

    All projects in the returned map belong to the same project.  Note this map
    may actually be empty (for instance in case of missing, out-dated cache
    files, or cache files for a different version of the library).

    Some notifications about the loading process may be pushed via
    {!Lsp_io.send_notification}.  May raise some IO-related exceptions
    ([Sys_error], [Failure]). *)
val load
  : rootdir: Superbol_project.rootdir
  -> layout: Superbol_project.layout
  -> params: params
  -> (* Lsp_document.t URIMap.t *)Lsp_project.t option

(** {2 Saving a single document} *)

(** [save_document ~params doc] persists the cache of [doc] alone, and records
    it in the cache of its project so that a later session finds it again.  The
    other documents of that project are left as they are.

    Used when a document is saved by the editor: that is when what the file
    holds and what was parsed are known to agree, which is the condition for a
    cached document to be of any use.

    Some notifications about the saving process may be pushed via
    {!Lsp_io.send_notification}. *)
val save_document
  : params: params
  -> Lsp_document.t
  -> unit

(** {2 Loading a single document} *)

(** [load_document ~project ~params ~doc] attempts to restore the document [doc]
    that the client has just opened, from the cache of [project].  Returns
    [None] whenever its analysis is missing from the cache, or out-dated.

    The analysis may have been stored by another front-end. *)
val load_document
  : project: Lsp_project.t
  -> params: params
  -> doc: Lsp.Types.DidOpenTextDocumentParams.t
  -> Lsp_document.t option

(** {2 Workspace symbols} *)

(** [workspace_symbols ~params ~except project] retrieves the symbols of the
    source files of [project] that are held in its cache, along with their URI,
    except for those files whose (absolute) name satisfies [except] (typically,
    open documents, whose symbols are better computed from their current
    contents).  Files are never analyzed here: those whose symbols are not
    cached are left out.

    Source files are those found under the project's root directory that the
    platform deems so, or that have an extension the project configuration
    assigns a source format to.  Files and directories the platform deems hidden
    are skipped, as are the project's work directory (according to the project
    layout of [params]) and the directories that the [exclude-dirs] setting of
    the project configuration names; links to directories are not followed.  All
    file-system operations go through {!Lsp_platform.record}.

    Results are memoized per file, and retrieved again only when the file
    changes on disk, or when [project] is replaced; changes to copybooks alone
    are not noticed until then. *)
val workspace_symbols
  : params: params
  -> except: (string -> bool)
  -> Lsp_project.t
  -> (Lsp.Uri.t * Lsp_workspace_symbols.t) Seq.t
