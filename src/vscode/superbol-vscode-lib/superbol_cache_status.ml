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


open Vscode

module MONITOR = Superbol_cache_monitor

type state =
  {
    instance: Superbol_instance.t;
    item: StatusBarItem.t;
    mutable refreshing: bool;
    mutable pending: bool;          (** whether to refresh again once done *)
    mutable delayed: Node.Timeout.t option;
  }

let state = ref None

(* Saving a document has the server write its cache: wait for saves to
   settle. *)
let delay_after_save = 2000                                             (* ms *)

(** Shows the totals of the caches of every workspace folder, given the status
    of each of them; the size limit applies to each cache on its own. *)
let show_statuses { item; _ } statuses =
  let caches =
    List.filter_map (function
        | folder, Ok (MONITOR.{ cache = Some _; _ } as status) ->
            Some (folder, status)
        | _ ->
            None) statuses
  in
  match caches with
  | [] ->
      StatusBarItem.hide item          (* no workspace folder, or no caching *)
  | caches ->
      let limit =
        Option.map Superbol_config.bytes_of_size
          (Superbol_config.cache_size_limit ()) in
      let over_limit (status: MONITOR.status) = match limit with
        | Some limit -> status.disk_usage > limit
        | None -> false
      in
      let total = List.fold_left (fun total (_, (s: MONITOR.status)) ->
          total +. s.disk_usage) 0. caches in
      let any_over_limit = List.exists (fun (_, s) -> over_limit s) caches in
      StatusBarItem.set_text item
        ("$(database) " ^ MONITOR.string_of_size total);
      StatusBarItem.set_tooltip item @@ String.concat "\n" @@
      List.map begin fun (folder, (s: MONITOR.status)) ->
        Printf.sprintf "SuperBOL cache of %s: %s (%u entries)%s"
          (WorkspaceFolder.name folder) (MONITOR.string_of_size s.disk_usage)
          s.entries (if over_limit s then ", over the limit" else "")
      end caches @
      (match limit with
       | Some limit ->
           [Printf.sprintf "Size limit of each cache: %s%s"
              (MONITOR.string_of_size limit)
              (if any_over_limit
               then " (caches are pruned when the language server stops)"
               else "")]
       | None ->
           []) @
      ["Click to show their contents."];
      StatusBarItem.set_backgroundColor item
        (if any_over_limit
         then Some (ThemeColor.make ~id:"statusBarItem.warningBackground")
         else None);
      StatusBarItem.show item

let rec refresh_now ({ instance; _ } as s) =
  if s.refreshing then s.pending <- true else begin
    s.refreshing <- true;
    s.pending <- false;
    let cli = Superbol_instance.cli_invocation instance in
    let folders = MONITOR.workspace_folders () in
    let _: unit Promise.t =
      Promise.then_
        (Promise.all @@ Array.of_list @@ List.map begin fun folder ->
            Promise.map (fun status -> folder, status) @@
            MONITOR.status cli ~project_root:(Uri.fsPath
                                                (WorkspaceFolder.uri folder))
          end folders)
        ~fulfilled:begin fun statuses ->
          s.refreshing <- false;
          let statuses = Array.to_list statuses in
          List.iter begin function
            | folder, Error msg ->
                Superbol_printer.log_error
                  "SuperBOL: could not get the status of the cache of %s: %s"
                  (WorkspaceFolder.name folder) msg
            | _, Ok _ ->
                ()
          end statuses;
          (* Should workspace folders have changed meanwhile, the refresh they
             trigger corrects this. *)
          show_statuses s statuses;
          if s.pending then refresh_now s;
          Promise.return ()
        end
    in
    ()
  end

let cancel_delayed s =
  Option.iter Node.clearTimeout s.delayed;
  s.delayed <- None

let refresh_later s ~delay =
  cancel_delayed s;
  s.delayed <- Some (Node.setTimeout (fun () ->
      s.delayed <- None;
      refresh_now s) delay)

let refresh () =
  Option.iter (fun s -> cancel_delayed s; refresh_now s) !state

let affects_status config_change =
  List.exists begin fun section ->
    ConfigurationChangeEvent.affectsConfiguration config_change ~section ()
  end ["superbol.cacheInGlobalStorage"; "superbol.cache.sizeLimit"]

let start instance ~click_command =
  (* On the left, below the side bar. *)
  let item =
    Window.createStatusBarItem ~id:"superbol.cacheStatus" ~alignment:Left ()
  in
  (* Named in menus (e.g. to hide it) on its own, rather than after the
     extension. *)
  StatusBarItem.set_name item "SuperBOL Caches";
  StatusBarItem.set_command item (`String click_command);
  let s = { instance; item; refreshing = false; pending = false;
            delayed = None } in
  state := Some s;
  let subscribe = Superbol_instance.subscribe_disposable instance in
  subscribe (StatusBarItem.disposable item);
  subscribe (Disposable.make ~dispose:(fun () -> cancel_delayed s));
  subscribe @@ Workspace.onDidSaveTextDocument
    ~listener:begin fun document ->
      match Workspace.getWorkspaceFolder ~uri:(TextDocument.uri document) with
      | Some _ -> refresh_later s ~delay:delay_after_save
      | None -> ()
    end ();
  subscribe @@ Workspace.onDidChangeWorkspaceFolders
    ~listener:(fun _ -> refresh ()) ();
  subscribe @@ Workspace.onDidChangeConfiguration
    ~listener:(fun change -> if affects_status change then refresh ()) ();
  (* The server prunes caches when it stops. *)
  Superbol_instance.on_language_server_start instance (fun _ -> refresh ());
  refresh_now s
