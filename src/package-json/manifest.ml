(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

type 'a list_object = ( string * 'a ) list

let list_object_enc encoding :
  ( string * 'a ) list Json_encoding.encoding =
  Json_encoding.custom
    (fun scripts ->
       `O ( List.map (fun (label, field) ->
           label,
           Json_encoding.construct encoding field
         ) scripts )
    )
    (function
        `O scripts ->
        List.map (fun (label, field) ->
            let field =
              try
                Json_encoding.destruct encoding field
              with exn ->
                Printf.eprintf "Exn in destruct label %S\n%!" label;
                raise exn
            in
            label, field
          ) scripts
      | _ -> raise (Json_encoding.Cannot_destruct ([], Exit))
    )
    ~schema:Json_schema.any


type any = Json_repr.ezjsonm [@@deriving json_encoding]


type repository = {
  type_ : string option ; [@key "type"]
  url : string ;
}
[@@deriving json_encoding]

type engines = {
  vscode : string ;
}
[@@deriving json_encoding]

type author = {
  author_name : string ;
  author_email : string ;
}
[@@deriving json_encoding]

type bug = {
  bug_url : string option ;
  bug_email : string option ;
}
[@@deriving json_encoding]

type package = {
  name : string ;
  displayName : string ;
  description : string ;
  license : string option ;
  version : string ;
  repository : repository option ;
  homepage : string option ;
  author : author option ;
  keywords : string list ; [@dft []]
  main : string option ;
  browser : string option ;
  scripts : string list_object ; [@dft []]
  dependencies : string list_object ; [@dft []]
  devDependencies : string list_object ; [@dft []]
  bugs : bug option ;
}
[@@deriving json_encoding]



type breakpoint = {
  language : string ;
}
[@@deriving json_encoding]

type color_defaults = {
  color_dark : string option ;
  color_light : string option ;
  color_highContrast : string option ;
  color_highContrastLight : string option ;
}
[@@deriving json_encoding]

type color = {
  color_id : string ;
  color_description : string ;
  color_defaults : color_defaults ;
}
[@@deriving json_encoding]

type command_icon = {
  icon_light : string ; (* path *)
  icon_dark : string ; (* path *)
}
[@@deriving json_encoding]


let command_icon_enc = Json_encoding.union [
    Json_encoding.case Json_encoding.string
      (fun _icon -> None)
      (fun url -> { icon_light = url ;icon_dark = url })
    ;
    Json_encoding.case command_icon_enc
      (fun icon -> Some icon)
      (fun icon -> icon)
  ]

type command = {
  command_command : string ;
  command_title : string ;
  command_category : string option ;
  command_icon : command_icon option ;
  command_enablement : string option ; (* Javascript condition *)
}
[@@deriving json_encoding]

type property = {
  prop_title : string option ;
  prop_markdownDescription : string option ;
  prop_deprecationMessage : string option ;

  prop_type : any; [@dft `String "string"] (* "array", "boolean", "number", "object", "string" *)
  prop_default : any option ; (* from type *)
  prop_description : string option ;

(*
    application - Settings that apply to all instances of VS Code and can only be configured in user settings.
    machine - Machine specific settings that can be set only in user settings or only in remote settings. For example, an installation path which shouldn't be shared across machines.
    machine-overridable - Machine specific settings that can be overridden by workspace or folder settings.
    window - Windows (instance) specific settings which can be configured in user, workspace, or remote settings. (* default *)
    resource - Resource settings, which apply to files and folders, and can be configured in all settings levels, even folder settings.
    language-overridable - Resource settings that can be overridable at a language level.
*)
  prop_scope : string option ;
    prop_items : any option ;
  prop_uniqueItems : any option ;
  prop_enum : any option ;
  prop_enumDescriptions : any option ;
  prop_minimum : int option ;
  prop_maximum : int option ;
  prop_minItems : int option ;
}
[@@deriving json_encoding]

type configuration = {
  conf_type : string option ; (* should always be None *)
  conf_title : string option ;
  conf_properties : property list_object ; [@dft []]
}
[@@deriving json_encoding]

type selector = {
  filenamePattern : string ; (* glob *)
}
[@@deriving json_encoding]

type customEditor = {
  edit_viewType : string ;
  edit_displayName : string ;
  edit_selector : selector list ;
  edit_priority : string option ; (* "default" or "option" *)
}
[@@deriving json_encoding]

type debugger = {
  debugger_type : string ; (* unique ID *)
  debugger_label : string ; (* user visible name of this debugger in the UI *)

  debugger_program : string option ;
  (* path to the debug adapter that implements the VS Code debug
     protocol against the real debugger or runtime *)

  debugger_runtime : string ;
  (* if the path to the debug adapter is not an executable but needs a
     runtime.*)

  debugger_variables : string list_object ; [@dft []]
  (* introduces substitution variables and binds them to commands
     implemented by the debugger extension. *)

  debugger_languages : string list ; [@dft []]
  (* those languages for which the debug extension could be considered
     the "default debugger". *)

  debugger_configurationAttributes : any option ;
  (* is the schema for launch configuration arguments specific to this
     debugger. Please note that the JSON schema constructs $ref and
     definition are not supported. *)

  debugger_initialConfigurations : any list ; [@dft []]
  (* lists launch configurations that are used to populate an initial
     launch.json. *)

  debugger_configurationSnippets : any list ; [@dft []]
  (* lists launch configurations that are available through
     IntelliSense when editing a launch.json. *)

}
[@@deriving json_encoding]


type grammar = { (* TextMate grammar for syntax highlighting *)
  grammar_language : string option ; (* existing language ID *)
  grammar_scopeName : string ; (* "source.COBOL" *)
  grammar_path : string ; (* path to grammar file *)
  grammar_injectTo : string list ; [@dft []]
  grammar_embeddedLanguages : string list_object ; [@dft []]
}
[@@deriving json_encoding]

type icon = {
  icon_description : string ;
  icon_default : string list_object ;
}
[@@deriving json_encoding]

type iconTheme = {
  iconTheme_id : string ;
  iconTheme_label : string ;
  iconTheme_path : string ;
}
[@@deriving json_encoding]

type jsonValidation = {
  jsonValidation_fileMatch : string ; (* ".json" *)
  jsonValidation_url : string ; (* scheme URL *)
}
[@@deriving json_encoding]

type keybinding = {
  key_command : string ;
  key_key : string ; (* "ctrl+f1" *)
  key_mac : string option ;
  key_when : string option ; (* Javascript condition *)
  key_args : string list_object ; [@dft []]
}
[@@deriving json_encoding]

type language = {
  lang_id : string ;
  lang_extensions : string list ; [@dft []]
  lang_aliases : string list ; [@dft []]
  lang_filenames : string list ; [@dft []]
  lang_filenamePatterns : string list ; [@dft []]
  lang_firstLine : string option ;
  lang_configuration : string option ;
  lang_icon : command_icon option ;
}
[@@deriving json_encoding]

type menu = {
  menu_command : string option ;
  menu_group : string option ;
  menu_when : string option ;
  menu_key : string option ;
  menu_submenu : string option ;
}
[@@deriving json_encoding]

type fileLocation = string list

let fileLocation_enc =
  let open Json_encoding in
  union
    [
      case string
        (function [s] -> Some s | _ -> None)
        (fun s -> [s]) ;
      case (list string)
        (function [_] -> None | list -> Some list)
        (fun s -> s) ;
    ]

type problemPattern = {
  pat_regexp : string ;
  pat_file : int option ;
  pat_line : int option ;
  pat_endLine : int option ;
  pat_column : int option ;
  pat_endColumn : int option ;
  pat_severity : int option ;
  pat_code : int option ;
  pat_message : int option ;
}
[@@deriving json_encoding]

type pattern =
    ProblemPattern of problemPattern
  | ProblemName of string

let pattern_enc =
  let open Json_encoding in
  union
    [
      case string
        (function ProblemName s -> Some s | _ -> None)
        (function s -> ProblemName s) ;
      case problemPattern_enc
        (function ProblemPattern s -> Some s | _ -> None)
        (function s -> ProblemPattern s) ;
    ]

type 'a list_or_one = 'a list

let list_or_one_enc encoding =
  let open Json_encoding in
  union [
    case encoding
      (function [s] -> Some s | _ -> None)
      (fun s -> [s]) ;
    case (list encoding)
      (fun s -> Some s)
      (fun s -> s)
  ]


type problemMatcher = {
  pm_name : string ;
  pm_owner : string option ;
  pm_fileLocation : fileLocation ; [@dft []]
  pm_pattern : pattern list_or_one ;
  pm_source : string option ;
  pm_severity : string option ; (* info, error, warning *)
}
[@@deriving json_encoding]


type contributes = {
    breakpoints : breakpoint list ; [@dft []]
    colors : color list ; [@dft []]
    commands : command list ; [@dft []]
    configuration : configuration option ;
    configurationDefaults : any list_object ; [@dft []]
    customEditors : customEditor list ; [@dft []]
    debuggers : debugger list ; [@dft []]
    grammars : grammar list ; [@dft []]
    icons : icon list_object ; [@dft []]
    iconThemes : iconTheme list ; [@dft []]
    jsonValidation : jsonValidation list ; [@dft []]
    keybindings : keybinding list ; [@dft []]
    languages : language list ; [@dft []]
    menus : menu list list_object ; [@dft []]
    problemMatchers : problemMatcher list ; [@dft []]
    problemPatterns : any option ;
    productIconThemes : any option ;
    resourceLabelFormatters : any option ;
    semanticTokenModifiers : any option ;
    semanticTokenScopes : any option ;
    semanticTokenTypes : any option ;
    snippets : any option ;
    submenus : any option ;
    taskDefinitions : any option ;
    terminal : any option ;
    themes : any option ;
    typescriptServerPlugins : any option ;
    views : any option ;
    viewsContainers : any option ;
    viewsWelcome : any option ;
    walkthroughs : any option ;
  }
  [@@deriving json_encoding]

type vscode = {
  engines : engines ;

  activationEvents : string list ; [@dft []]
  contributes : contributes option ;
  extensionKind :  string list ; [@dft []] (* "ui" or/and "workspace" *)
  extensionPack : string list ; [@dft []]
  extensionDependencies : string list ; [@dft []]
}
[@@deriving json_encoding]

type sponsor = {
  sponsor_url : string ; [@key "url"]
}
[@@deriving json_encoding]

type galleryBanner = {
  gallery_color : string option ;
  gallery_theme : string option ;
}
[@@deriving json_encoding]

type marketplace = {
  (* `Programming Languages`, `Snippets`, `Linters`, `Themes`,
     `Debuggers`, `Formatters`, `Keymaps`, `SCM Providers`, `Other`,
     `Extension Packs`, `Language Packs`, `Data Science`, `Machine
     Learning`, `Visualization`, `Notebooks`, `Education`, `Testing`*)
  publisher : string ;
  categories : string list ; [@dft []]
  icon : string option ;
  preview : bool option ;
  badges : string list ; [@dft []]
  markdown : string option ; (* "github" or "standard" *)
  qna : string option ; (* "marketplace" or URL *)
  sponsor : sponsor option ;
  galleryBanner : galleryBanner option ;
}
[@@deriving json_encoding]

type others = {
  prettier : any option ;
  capabilities : any option ;
  types : any option ;
  metadata : any option ; [@key "__metadata"]
}
[@@deriving json_encoding]

type vscode_package = ( package * vscode ) * ( marketplace * others )

let vscode_package_enc =
  Json_encoding.merge_objs
    ( Json_encoding.merge_objs package_enc vscode_enc )
    ( Json_encoding.merge_objs marketplace_enc others_enc )
