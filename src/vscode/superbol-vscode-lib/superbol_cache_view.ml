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
open Promise.Syntax

module MONITOR = Superbol_cache_monitor

let view_id = "superbol.cacheView"
let show_command = "superbol.cache.show"
let refresh_command = "superbol.cache.refresh"
let prune_command = "superbol.cache.prune"
let prune_unused_since_command = "superbol.cache.pruneUnusedSince"

(* --- Grouping entries by the date they were last used --- *)

type bucket =
  {
    since: float;           (** in days: entries unused for at least that... *)
    until: float option;    (** ... and less than that *)
    title: string;
  }

let buckets =
  [ { since =   0.; until = Some   1.; title = "Used within the last day" };
    { since =   1.; until = Some   7.; title = "Last used 1 to 7 days ago" };
    { since =   7.; until = Some  30.; title = "Last used 1 to 4 weeks ago" };
    { since =  30.; until = Some 182.; title = "Last used 1 to 6 months ago" };
    { since = 182.; until = None;      title = "Last used over 6 months ago" } ]

let days ~now (e: MONITOR.entry) = (now -. e.last_used) /. 86400.

let in_bucket ~now { since; until; _ } e =
  let d = days ~now e in
  d >= since && match until with Some u -> d < u | None -> true

(* Size of [entries] and of the values they hold, each value counted once. *)
let size_of (entries: MONITOR.entry list) =
  let values = Hashtbl.create 17 in
  List.fold_left begin fun total (e: MONITOR.entry) ->
    Hashtbl.replace values e.value e.value_size;
    total +. (e.disk_usage -. e.value_size)
  end 0. entries |> fun manifests ->
  Hashtbl.fold (fun _ size total -> total +. size) values manifests

let count n ~one ~many =
  if n = 1 then "1 " ^ one else Printf.sprintf "%u %s" n many

let plural n singular = count n ~one:singular ~many:(singular ^ "s")

let pp_days ppf d =
  if d = 0. then Format.fprintf ppf "any time"
  else if d >= 182. then Format.fprintf ppf "%.0f months" (d /. 30.4)
  else Format.fprintf ppf "%s" (plural (int_of_float d) "day")

let string_of_age seconds =
  if seconds < 60. then "just now"
  else
    let n, unit =
      if seconds < 3600. then seconds /. 60., "minute"
      else if seconds < 86400. then seconds /. 3600., "hour"
      else seconds /. 86400., "day"
    in
    plural (int_of_float n) unit ^ " ago"

(* --- Nodes --- *)

(** Size of the age group or directory holding a node, and number of nodes it
    holds. *)
type parent =
  {
    parent_size: float;
    siblings: int;
  }

(** [index] is the age group a node belongs to. *)
type node =
  | Folder of WorkspaceFolder.t
  | Note of { text: string; icon: string }
  | Bucket of { folder: WorkspaceFolder.t; index: int; bucket: bucket;
                entries: MONITOR.entry list }
  | Dir of { folder: WorkspaceFolder.t; index: int; prefix: string;
             name: string; entries: MONITOR.entry list; parent: parent }
    (** entries whose label starts with [prefix], which ends with [/] *)
  | File of { folder: WorkspaceFolder.t; index: int; label: string;
              name: string; entries: MONITOR.entry list; parent: parent }
  | Entry of { folder: WorkspaceFolder.t; entry: MONITOR.entry }

module EMITTER = EventEmitter.Make (Interop.Js.Option (Interop.Js.String))
module DECORATIONS_EMITTER =
  EventEmitter.Make (Interop.Js.Option (Interop.Js.List (Uri)))
module PROVIDER = TreeDataProvider.Make (Interop.Js.String)

type t =
  {
    instance: Superbol_instance.t;
    emitter: EMITTER.t;
    nodes: (string, node) Hashtbl.t;           (** elements are their ids *)
    listings: (string, (MONITOR.listing, string) result) Hashtbl.t;
    (** per workspace folder, by URI *)
    shares: (string, float) Hashtbl.t;
    (** share of the size of their parent held by nodes drawn since the last
        refresh, by URI *)
    decorations: DECORATIONS_EMITTER.t;
    changed_decorations: (string, Uri.t) Hashtbl.t;
    (** URIs of nodes drawn since VS Code was last told, by string *)
  }

let state = ref None

let folder_key folder = Uri.toString (WorkspaceFolder.uri folder) ()

let project_root folder = Uri.fsPath (WorkspaceFolder.uri folder)

let workspace_folders = MONITOR.workspace_folders

let register t id node = Hashtbl.replace t.nodes id node; id

let listing t folder =
  match Hashtbl.find_opt t.listings (folder_key folder) with
  | Some listing ->
      Promise.return listing
  | None ->
      let+ listing =
        MONITOR.listing (Superbol_instance.cli_invocation t.instance)
          ~project_root:(project_root folder)
      in
      Hashtbl.replace t.listings (folder_key folder) listing;
      listing

(* Tail-recursive [List.map] (it is not, as of OCaml 4.14): nodes may have tens
   of thousands of children, and the stack is small in JavaScript. *)
let map_tr f l = List.rev (List.rev_map f l)

(* --- Directories --- *)

let add_to table key e =
  Hashtbl.replace table key
    (e :: Option.value ~default:[] (Hashtbl.find_opt table key))

let sorted_bindings table =
  List.sort (fun (a, _) (b, _) -> String.compare a b) @@
  Hashtbl.fold (fun key entries acc -> (key, entries) :: acc) table []

(** [split ~prefix entries] groups [entries], whose labels all start with
    [prefix], by the subdirectory of [prefix] they belong to (named with a
    trailing [/]), or else by label for those that are directly in [prefix].
    Absolute labels belong to subdirectory [/] of [""]. *)
let split ~prefix (entries: MONITOR.entry list) =
  let n = String.length prefix in
  let dirs = Hashtbl.create 17 and files = Hashtbl.create 17 in
  List.iter begin fun (e: MONITOR.entry) ->
    match String.index_from_opt e.label n '/' with
    | Some i -> add_to dirs (String.sub e.label n (i + 1 - n)) e
    | None -> add_to files e.label e
  end entries;
  dirs, files

(** [compact ~prefix entries] extends directory [prefix] through every
    subdirectory that is all its parent holds, so that such chains are shown as
    a single node. *)
let rec compact ~prefix entries =
  let dirs, files = split ~prefix entries in
  if Hashtbl.length files = 0 && Hashtbl.length dirs = 1
  then match sorted_bindings dirs with
    | [sub, _] -> compact ~prefix:(prefix ^ sub) entries
    | _ -> prefix
  else prefix

let directory_children t ~folder ~index ~prefix entries =
  let key = folder_key folder in
  let dirs, files = split ~prefix entries in
  let n = String.length prefix in
  let parent = { parent_size = size_of entries;
                 siblings = Hashtbl.length dirs + Hashtbl.length files } in
  List.rev_append (List.rev_map begin fun (sub, entries) ->
    let prefix' = compact ~prefix:(prefix ^ sub) entries in
    let name = match String.sub prefix' n (String.length prefix' - n - 1) with
      | "" -> "/"                            (* the root, of absolute paths *)
      | name -> name
    in
    register t (Printf.sprintf "d|%s|%u|%s" key index prefix')
      (Dir { folder; index; prefix = prefix'; name; entries; parent })
  end (sorted_bindings dirs)) @@          (* directories first, then files *)
  map_tr begin fun (label, entries) ->
    let name = String.sub label n (String.length label - n) in
    register t (Printf.sprintf "l|%s|%u|%s" key index label)
      (File { folder; index; label; name; entries; parent })
  end (sorted_bindings files)

let children t = function
  | None ->
      let folders = workspace_folders () in
      let+ _ = Promise.all (Array.of_list (List.map (listing t) folders)) in
      List.map (fun f -> register t ("f|" ^ folder_key f) (Folder f)) folders
  | Some (Folder folder) ->
      let key = folder_key folder in
      let note text ~icon = [register t ("n|" ^ key) (Note { text; icon })] in
      let+ listing = listing t folder in
      begin match listing with
        | Error msg ->
            note ("Could not read the cache: " ^ msg) ~icon:"error"
        | Ok { totals = { cache = None; _ }; _ } ->
            note "Caching is disabled" ~icon:"info"
        | Ok { items = []; _ } ->
            note "The cache is empty" ~icon:"info"
        | Ok { now; items; _ } ->
            List.concat @@ List.mapi begin fun i bucket ->
              match List.filter (in_bucket ~now bucket) items with
              | [] -> []
              | entries ->
                  [register t (Printf.sprintf "b|%s|%u" key i)
                     (Bucket { folder; index = i; bucket; entries })]
            end buckets
      end
  | Some (Bucket { folder; index; entries; _ }) ->
      Promise.return @@ directory_children t ~folder ~index ~prefix:"" entries
  | Some (Dir { folder; index; prefix; entries; _ }) ->
      Promise.return @@ directory_children t ~folder ~index ~prefix entries
  | Some (File { folder; entries; _ }) ->
      Promise.return @@ map_tr begin fun (entry: MONITOR.entry) ->
        register t (Printf.sprintf "e|%s|%s" (folder_key folder) entry.key)
          (Entry { folder; entry })
      end entries
  | Some (Note _ | Entry _) ->
      Promise.return []

(* --- Items --- *)

let theme_icon id = `ThemeIcon (ThemeIcon.make ~id ())

let item ?description ?tooltip ?icon ?context
    ?(state = TreeItemCollapsibleState.None) label =
  let item =
    TreeItem.make_label ~label:(`String label) ~collapsibleState:state () in
  Option.iter (fun d -> TreeItem.set_description item (`String d)) description;
  Option.iter (fun d -> TreeItem.set_tooltip item (`String d)) tooltip;
  Option.iter (fun i -> TreeItem.set_iconPath item (theme_icon i)) icon;
  Option.iter (TreeItem.set_contextValue item) context;
  item

let entries_text n = count n ~one:"entry" ~many:"entries"

let freed_tooltip ~now ~since items =
  let n, freed = MONITOR.removed_by_age_bound ~now ~older_than:since items in
  if since = 0.
  then Printf.sprintf "Removing every entry frees %s."
      (MONITOR.string_of_size freed)
  else Format.asprintf "Removing entries unused for more than %a frees about \
                        %s (%s)."
      pp_days since (MONITOR.string_of_size freed) (entries_text n)

(* --- Decorations ---

   Files and directories get URIs of a scheme of their own: they keep the icons
   of the file icon theme, but not the decorations of the actual files (e.g.
   diagnostics, version control).  They are colored by size instead. *)

let uri_scheme = "superbol-cache"

(** [node_uri folder ~index path] is the URI of the node for [path] (a label,
    or a directory prefix) within the age group [index] of the cache of
    [folder]. *)
let node_uri folder ~index path =
  let path =
    if Filename.is_relative path
    then Uri.path (WorkspaceFolder.uri folder) ^ "/" ^ path
    else path
  in
  let path =                                     (* no trailing separator *)
    let n = String.length path in
    if n > 1 && path.[n - 1] = '/' then String.sub path 0 (n - 1) else path
  in
  Uri.from ~scheme:uri_scheme ~path ~query:(Printf.sprintf "group=%u" index) ()

(** Colors of nodes holding at least these shares of the size of their
    parent. *)
let size_colors =
  [ 0.5,  "charts.red";
    0.2,  "charts.orange";
    0.05, "charts.yellow" ]

let decoration ~share =
  let tooltip =
    Printf.sprintf "%.1f%% of the size of its parent" (share *. 100.) in
  match List.find_opt (fun (bound, _) -> share >= bound) size_colors with
  | Some (_, color) ->
      Some (FileDecoration.make ~tooltip ~color:(ThemeColor.make ~id:color) ())
  | None ->
      None

(* VS Code asks again for the decoration of a URI only once told it changed: it
   is told about every node drawn, in one batch per tick.  Shares are forgotten
   on refresh without telling it, so it keeps its decorations until nodes are
   drawn again. *)
let decoration_changed t ~key uri =
  if Hashtbl.length t.changed_decorations = 0 then
    ignore @@ Node.setTimeout begin fun () ->
      let uris =
        Hashtbl.fold (fun _ uri uris -> uri :: uris) t.changed_decorations [] in
      Hashtbl.reset t.changed_decorations;
      DECORATIONS_EMITTER.fire t.decorations (Some uris)
    end 0;
  Hashtbl.replace t.changed_decorations key uri

(** Gives the node of [item] the URI of [path], and records the share of the
    size of its [parent] held by [entries], unless it has no siblings. *)
let set_node_uri t item folder ~index ~parent path entries =
  let uri = node_uri folder ~index path in
  TreeItem.set_resourceUri item uri;
  let key = Uri.toString uri () in
  if parent.siblings > 1 && parent.parent_size > 0. then
    Hashtbl.replace t.shares key (size_of entries /. parent.parent_size);
  decoration_changed t ~key uri

let file_uri folder label =
  if Filename.is_relative label then
    let uri = Uri.joinPath (WorkspaceFolder.uri folder) ~pathSegments:[label] in
    if Node.Fs.existsSync (Uri.fsPath uri) then Some uri else None
  else None

let tree_item t = function
  | Folder folder ->
      let description, tooltip =
        match Hashtbl.find_opt t.listings (folder_key folder) with
        | Some (Ok { totals = { cache = Some dir; entries; values;
                                disk_usage; _ }; _ }) ->
            let size = MONITOR.string_of_size disk_usage in
            Some (Printf.sprintf "%s · %s" size (entries_text entries)),
            Some (Printf.sprintf "%s\n%s, %u values: %s" dir
                    (entries_text entries) values size)
        | _ ->
            None, None
      in
      item (WorkspaceFolder.name folder) ?description ?tooltip
        ~icon:"database" ~context:"superbolCacheFolder"
        ~state:TreeItemCollapsibleState.Expanded
  | Note { text; icon } ->
      item text ~icon
  | Bucket { folder; bucket; entries; _ } ->
      let now = match Hashtbl.find_opt t.listings (folder_key folder) with
        | Some (Ok { now; _ }) -> now
        | _ -> MONITOR.now ()
      and all = match Hashtbl.find_opt t.listings (folder_key folder) with
        | Some (Ok { items; _ }) -> items
        | _ -> entries
      in
      item bucket.title ~icon:"history" ~context:"superbolCacheBucket"
        ~state:TreeItemCollapsibleState.Collapsed
        ~description:(Printf.sprintf "%s · %s" (entries_text
                                                  (List.length entries))
                        (MONITOR.string_of_size (size_of entries)))
        ~tooltip:(freed_tooltip ~now ~since:bucket.since all)
  | Dir { folder; index; prefix; name; entries; parent } ->
      let item =
        item name ~state:TreeItemCollapsibleState.Collapsed ~tooltip:prefix
          ~description:(Printf.sprintf "%s · %s"
                          (entries_text (List.length entries))
                          (MONITOR.string_of_size (size_of entries)))
      in
      set_node_uri t item folder ~index ~parent prefix entries;
      (* The icon of the theme for the directory. *)
      TreeItem.set_iconPath item (`ThemeIcon ThemeIcon.folder);
      item
  | File { folder; index; label; name; entries; parent } ->
      let last_used = List.fold_left (fun m (e: MONITOR.entry) ->
          Float.max m e.last_used) 0. entries in
      let item =
        item name ~state:TreeItemCollapsibleState.Collapsed ~tooltip:label
          ~description:(Printf.sprintf "%s · %s · used %s"
                          (entries_text (List.length entries))
                          (MONITOR.string_of_size (size_of entries))
                          (string_of_age (MONITOR.now () -. last_used)))
      in
      set_node_uri t item folder ~index ~parent label entries;
      (* The icon of the theme for the file (rather than a folder, as for any
         item that can be expanded), unless the label is not a path (e.g. that
         of the entry of the project). *)
      TreeItem.set_iconPath item @@
      if String.contains label '/' || Filename.extension label <> ""
      then `ThemeIcon ThemeIcon.file
      else theme_icon "symbol-misc";
      Option.iter begin fun uri ->
        TreeItem.set_command item @@
        Command.create ~title:"Open" ~command:"vscode.open"
          ~arguments:[Uri.t_to_js uri] ()
      end (file_uri folder label);
      item
  | Entry { entry; _ } ->
      item (String.sub entry.key 0 (min 12 (String.length entry.key)))
        ~icon:"circle-small"
        ~description:(Printf.sprintf "%s · %s · used %s"
                        (MONITOR.string_of_size entry.disk_usage)
                        (count entry.dependencies ~one:"dependency"
                           ~many:"dependencies")
                        (string_of_age
                           (MONITOR.now () -. entry.last_used)))
        ~tooltip:(Printf.sprintf "Entry %s\nValue %s (%s, possibly shared \
                                  with other entries)"
                    entry.key entry.value
                    (MONITOR.string_of_size entry.value_size))

(* --- Refreshing --- *)

(* The listing is read before the folder is drawn again, as its item gives
   totals. *)
let refresh_folder t folder =
  Hashtbl.remove t.listings (folder_key folder);
  let+ _ = listing t folder in
  Hashtbl.reset t.shares;
  EMITTER.fire t.emitter (Some ("f|" ^ folder_key folder))

let refresh () =
  Option.iter begin fun t ->
    Hashtbl.reset t.listings;
    Hashtbl.reset t.nodes;
    Hashtbl.reset t.shares;
    EMITTER.fire t.emitter None
  end !state;
  Superbol_cache_status.refresh ()

let show () =
  let _: Ojs.t option Promise.t =
    Commands.executeCommand ~command:(view_id ^ ".focus") ~args:[]
  in
  refresh ()

(* --- Pruning --- *)

let node_of_args t = function
  | arg :: _ when Ojs.type_of arg = "string" ->
      Hashtbl.find_opt t.nodes (Ojs.string_of_js arg)
  | _ ->
      None

let pick_folder () =
  match workspace_folders () with
  | [] -> Promise.return None
  | [folder] -> Promise.return (Some folder)
  | folders ->
      Window.showQuickPickItems ()
        ~choices:(List.map (fun f ->
            QuickPickItem.create ~label:(WorkspaceFolder.name f)
              ~description:(project_root f) (), f) folders)
        ~options:(QuickPickOptions.create
                    ~placeHolder:"Workspace folder whose cache to prune" ())

let trim t folder ?older_than ?max_size () =
  let name = WorkspaceFolder.name folder in
  let task ~progress:_ ~token:_ =
    let* result =
      MONITOR.trim (Superbol_instance.cli_invocation t.instance)
        ~project_root:(project_root folder) ?older_than ?max_size ()
    in
    begin match result with
      | Ok { reclaimed; remaining } ->
          let _: unit option Promise.t =
            Window.showInformationMessage ()
              ~message:(Printf.sprintf
                          "SuperBOL: removed %s from the cache of %s, that \
                           now takes %s.%s" (entries_text reclaimed) name
                          (MONITOR.string_of_size remaining.disk_usage)
                          (if reclaimed = 0 && older_than <> None
                           then "  (Nothing is removed while the cache is \
                                 being pruned by another process.)"
                           else ""))
          in
          ()
      | Error msg ->
          let _: unit option Promise.t =
            Window.showErrorMessage ()
              ~message:(Printf.sprintf "SuperBOL: could not prune the cache \
                                        of %s: %s" name msg)
          in
          ()
    end;
    Superbol_cache_status.refresh ();
    refresh_folder t folder
  in
  Window.withProgress (module Interop.Js.Unit) ~task
    ~options:(ProgressOptions.create
                ~location:(`ProgressLocation ProgressLocation.Notification)
                ~title:(Printf.sprintf "SuperBOL: pruning the cache of %s" name)
                ~cancellable:false ())

type prune_choice =
  | Configured_limits
  | Unused_for of float                                         (* days *)

let prune_folder t folder =
  let* listing = listing t folder in
  let size_limit = Superbol_config.cache_size_limit ()
  and max_age = Superbol_config.cache_max_age () in
  let configured =
    QuickPickItem.create ~label:"$(settings-gear) Apply the configured limits"
      ~description:(String.concat ", " [
          (match size_limit with
           | Some s -> "size limit: " ^ s
           | None -> "no size limit");
          (match max_age with
           | Some d -> Printf.sprintf "maximum age: %s" (plural d "day")
           | None -> "no maximum age") ]) (),
    Configured_limits
  in
  let by_age =
    match listing with
    | Error _ ->
        []
    | Ok { now; items; totals } ->
        List.filter_map begin fun since ->
          match MONITOR.removed_by_age_bound ~now ~older_than:since items with
          | 0, _ -> None
          | n, freed ->
              Some (QuickPickItem.create ()
                      ~label:(Format.asprintf
                                "$(history) Remove entries unused for more \
                                 than %a" pp_days since)
                      ~description:(Printf.sprintf "%s, frees about %s"
                                      (entries_text n)
                                      (MONITOR.string_of_size freed)),
                    Unused_for since)
        end [1.; 7.; 30.; 182.] @
        if items = [] then [] else
          [ QuickPickItem.create ~label:"$(trash) Remove every entry"
              ~description:(Printf.sprintf "%s, %s"
                              (entries_text totals.entries)
                              (MONITOR.string_of_size totals.disk_usage)) (),
            Unused_for 0. ]
  in
  let* choice =
    Window.showQuickPickItems () ~choices:(configured :: by_age)
      ~options:(QuickPickOptions.create
                  ~placeHolder:(Printf.sprintf "How to prune the cache of %s"
                                  (WorkspaceFolder.name folder)) ())
  in
  match choice with
  | None ->
      Promise.return ()
  | Some Configured_limits ->
      trim t folder ?max_size:size_limit
        ?older_than:(Option.map float_of_int max_age) ()
  | Some (Unused_for days) ->
      trim t folder ~older_than:days ()

let prune ~args =
  match !state with
  | None -> ()
  | Some t ->
      let _: unit Promise.t =
        match node_of_args t args with
        | Some (Folder folder) ->
            prune_folder t folder
        | _ ->
            let* folder = pick_folder () in
            match folder with
            | Some folder -> prune_folder t folder
            | None -> Promise.return ()
      in
      ()

let prune_unused_since ~args =
  match !state with
  | None -> ()
  | Some t ->
      match node_of_args t args with
      | Some (Bucket { folder; bucket; _ }) ->
          let n, freed =
            match Hashtbl.find_opt t.listings (folder_key folder) with
            | Some (Ok { now; items; _ }) ->
                MONITOR.removed_by_age_bound ~now ~older_than:bucket.since
                  items
            | _ ->
                0, 0.
          in
          let message =
            if bucket.since = 0.
            then Printf.sprintf "Remove every entry (%s) from the cache of %s?"
                (entries_text n) (WorkspaceFolder.name folder)
            else Format.asprintf "Remove the %s unused for more than %a from \
                                  the cache of %s?  This frees about %s."
                (entries_text n) pp_days bucket.since
                (WorkspaceFolder.name folder) (MONITOR.string_of_size freed)
          in
          let _: unit Promise.t =
            let* choice =
              Window.showWarningMessage () ~message
                ~options:(MessageOptions.create ~modal:true ())
                ~choices:["Remove", ()]
            in
            match choice with
            | Some () -> trim t folder ~older_than:bucket.since ()
            | None -> Promise.return ()
          in
          ()
      | _ ->
          ()

(* --- *)

let start instance =
  let t = { instance; emitter = EMITTER.make ();
            nodes = Hashtbl.create 64; listings = Hashtbl.create 4;
            shares = Hashtbl.create 64;
            decorations = DECORATIONS_EMITTER.make ();
            changed_decorations = Hashtbl.create 64 } in
  state := Some t;
  let provider =
    PROVIDER.create ()
      ~onDidChangeTreeData:(EMITTER.event t.emitter)
      ~getTreeItem:begin fun ~element ->
        match Hashtbl.find_opt t.nodes element with
        | Some node -> `Value (tree_item t node)
        | None -> `Value (item "…")
      end
      ~getChildren:begin fun ?element () ->
        match element with
        | None ->
            `Promise (Promise.map Option.some (children t None))
        | Some id ->
            match Hashtbl.find_opt t.nodes id with
            | Some node ->
                `Promise (Promise.map Option.some (children t (Some node)))
            | None ->
                `Value (Some [])
      end
  in
  let subscribe = Superbol_instance.subscribe_disposable instance in
  subscribe @@
  Window.registerTreeDataProvider (module Interop.Js.String) ~viewId:view_id
    ~treeDataProvider:provider;
  subscribe @@
  Window.registerFileDecorationProvider ~provider:begin
    FileDecorationProvider.create ()
      ~onDidChangeFileDecorations:(DECORATIONS_EMITTER.event t.decorations)
      ~provideFileDecoration:begin fun ~uri ~token:_ ->
        `Value begin
          if Uri.scheme uri <> uri_scheme then None else
            Option.bind (Hashtbl.find_opt t.shares (Uri.toString uri ()))
              (fun share -> decoration ~share)
        end
      end
  end;
  subscribe @@
  Disposable.make ~dispose:begin fun () ->
    EMITTER.dispose t.emitter ();
    DECORATIONS_EMITTER.dispose t.decorations ()
  end;
  subscribe @@ Workspace.onDidChangeWorkspaceFolders
    ~listener:(fun _ -> refresh ()) ()
