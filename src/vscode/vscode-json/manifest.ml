(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let file = "package.json"
let filename = file

(* Some doc on deriving json_encoding:
   * Field attributes : [@dft default], [@opt],
     [@ddft default] (forced field on construct),
     [@req] (forced option), [@key "keyName"], [@title "titleName"],
     [@description "descr"], [@exclude default],
     [@merge],
     [@camel], [@snake],
     [@set encoding], [@map encoding]
   * Type attributes :
     [@assoc] ( (string * 'a) list encoded as object )
     [@enum]
     [@encoding encoding_name]
     [@wrap "field"] (wrap within an obj1)
   * Variant attributes:
     [@kind "label"], [@kind_label "type"], [@empty]
   * Definition attributes: [@@deriving json_encoding {...}]
     {ignore} : ignore other fields
     {remove_prefix = "prefix"}
     {recursive}
     {title = "title" }
     {description = "description"}
     {schema = "schema"}
     {option = "req/opt/dft" }
     {debug}
     {name = "..._enc" }
     {camel}, {snake}
     {wrap}
*)



type any = Json_repr.ezjsonm [@@deriving json_encoding]
let any s = Ezjsonm.from_string s

type 'a list_or_one = 'a list [@@deriving show]

let pp_any _fmt _ezjsonm = ()

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



type repository = {
  type_ : string option ; [@key "type"]
  url : string ;
}
[@@deriving json_encoding,show]

type engines = {
  vscode : string ;
}
[@@deriving json_encoding,show]
let engines ~vscode = { vscode }


type author = {
  author_name : string ;
  author_email : string option ;
}
[@@deriving json_encoding,show]

let author_enc =
  let open Json_encoding in
  union [
    case string
      (function { author_email = None ; author_name } -> Some author_name
              | _ -> None)
      (fun author_name -> { author_name ; author_email = None }) ;
    case author_enc
      (function { author_email = None ; _ } -> None
              | a -> Some a)
      (fun s -> s) ;
  ]

type bug = {
  bug_url : string option ;
  bug_email : string option ;
}
[@@deriving json_encoding,show]

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
  scripts : (string * string) list [@assoc] ; [@dft []]
  dependencies : (string * string) list [@assoc] ; [@dft []]
  devDependencies : (string * string) list [@assoc] ; [@dft []]
  bugs : bug option ;
}
[@@deriving json_encoding,show]
let package
    ~displayName
    ~description
    ?license
    ~version
    ?repository
    ?homepage
    ?author
    ?( keywords = [] )
    ?main
    ?browser
    ?( scripts = [] )
    ?( dependencies = [] )
    ?( devDependencies = [] )
    ?bugs
    name =
  { name ;
    displayName ;
    description ;
    license ;
    version ;
    repository ;
    homepage ;
    author ;
    keywords ;
    main ;
    browser ;
    scripts ;
    dependencies ;
    devDependencies ;
    bugs
  }


type breakpoint = {
  language : string ;
}
[@@deriving json_encoding,show]

let breakpoint language = { language }

type color_defaults = {
  color_dark : string option ;
  color_light : string option ;
  color_highContrast : string option ;
  color_highContrastLight : string option ;
}
[@@deriving json_encoding,show]

type color = {
  color_id : string ;
  color_description : string ;
  color_defaults : color_defaults ;
}
[@@deriving json_encoding,show]

type command_icon = {
  icon_light : string ; (* path *)
  icon_dark : string ; (* path *)
}
[@@deriving json_encoding,show]


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
[@@deriving json_encoding,show]
let command ~command ~title ?category ?icon ?enablement () =
  {
    command_command = command ;
    command_title = title ;
    command_category = category ;
    command_icon = icon ;
    command_enablement = enablement ;
  }

type property = {
  prop_title : string option ;
  prop_markdownDescription : string option ;
  prop_deprecationMessage : string option ;

  prop_type : any; (* "array", "boolean", "number", "object", "string" *)
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
  prop_uniqueItems : bool option ;
  prop_enum : string list ; [@dft []]
  prop_enumDescriptions : string list ; [@dft []]
  prop_minimum : int option ;
  prop_maximum : int option ;
  prop_minItems : int option ;
}
[@@deriving json_encoding,show]

let property
    ?title: prop_title
    ?markdownDescription: prop_markdownDescription
    ?deprecationMessage: prop_deprecationMessage

    ?type_: (prop_type = `String "string")
    ?default: prop_default
    ?description: prop_description
    ?scope: prop_scope
    ?items: prop_items
    ?uniqueItems: prop_uniqueItems
    ?enum: (prop_enum = [])
    ?enumDescriptions: (prop_enumDescriptions = [])
    ?minimum: prop_minimum
    ?maximum: prop_maximum
    ?minItems: prop_minItems

    name =
  name,
  {
    prop_title ;
    prop_markdownDescription ;
    prop_deprecationMessage ;
    prop_type ;
    prop_default ;
    prop_description ;
    prop_scope ;
    prop_items ;
    prop_uniqueItems ;
    prop_enum ;
    prop_enumDescriptions ;
    prop_minimum ;
    prop_maximum ;
    prop_minItems ;
  }

module PROPERTY = struct

  let bool ?default =
    property
      ~type_:(`String "boolean")
      ?default:(match default with
          | None -> None
          | Some bool -> Some (`Bool bool))

  let int ?default =
    property
      ~type_:(`String "number")
      ?default:(match default with
          | None -> None
          | Some int -> Some (`Float (float_of_int int)))

  let string ?default =
    property
      ~type_:(`String "string")
      ?default:(match default with
          | None -> None
          | Some s -> Some (`String s))

  let null_string ?default =
    property
      ~type_:(`A [`String "string"; `String "null"])
      ?default:(match default with
          | None -> None
          | Some None -> Some `Null
          | Some Some s -> Some (`String s))

  let strings ?default =
    property
      ~type_:(`String "array")
      ?default:(match default with
          | None -> None
          | Some strings ->
            Some (`A (List.map (fun s -> `String s) strings)))
      ~items: (`String "string")

  let array = property ~type_:(`String "array")

  let ints ?default =
    property
      ~type_:(`String "array")
      ?default:(match default with
          | None -> None
          | Some strings ->
            Some (`A (List.map (fun s -> `Float (float_of_string s)) strings)))
      ~items: (`String "number")

  let tabstops ?default =
    property
      ~type_:(`String "array")
      ?default:(match default with
          | None -> None
          | Some strings ->
            Some (`A (List.map (fun s -> `Float (float_of_string s)) strings)))
      ~items: (any {|
{
  "type": "number",
  "title": "tabstops",
  "properties": {
     "tabstop": {
        "type": "number",
        "description": "tabstop"
     }
  }
}
|})

  let null_strings ?default =
    property
      ~type_:(`A [ `String "array"; `String "null" ])
      ?default:(match default with
          | None -> None
          | Some None -> Some `Null
          | Some Some strings ->
            Some (`A (List.map (fun s -> `String s) strings)))
      ~items: (`String "string")

  let enum ?default ~cases =
    property
      ~type_:(`String "string")
      ~enum:cases
      ?default
      ~items:(`String "string")

end

type configuration = {
  conf_type : string option ; (* should always be None *)
  conf_title : string option ;
  conf_properties : ( string * property ) list [@assoc] ; [@dft []]
}
[@@deriving json_encoding,show]
let configuration ?title properties =
  { conf_type = None ;
    conf_title = title ;
    conf_properties = properties }

type selector = {
  filenamePattern : string ; (* glob *)
}
[@@deriving json_encoding,show]

type customEditor = {
  edit_viewType : string ;
  edit_displayName : string ;
  edit_selector : selector list ;
  edit_priority : string option ; (* "default" or "option" *)
}
[@@deriving json_encoding,show]

type debugger = {
  debugger_type : string ; (* unique ID *)
  debugger_label : string ; (* user visible name of this debugger in the UI *)

  debugger_program : string option ;
  (* path to the debug adapter that implements the VS Code debug
     protocol against the real debugger or runtime *)

  debugger_runtime : string option ;
  (* if the path to the debug adapter is not an executable but needs a
     runtime.*)

  debugger_variables : (string * string) list [@assoc] ; [@dft []]
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
[@@deriving json_encoding,show]

let debugger
    ~label:debugger_label
    ?program:debugger_program
    ?runtime:debugger_runtime
    ?variables:(debugger_variables = [])
    ?languages:(debugger_languages = [])
    ?configurationAttributes:debugger_configurationAttributes
    ?initialConfigurations:(debugger_initialConfigurations = [])
    ?configurationSnippets:(debugger_configurationSnippets = [])
    debugger_type
  =
  { debugger_type ;
    debugger_label ;
    debugger_program ;
    debugger_runtime ;
    debugger_variables ;
    debugger_languages ;
    debugger_configurationAttributes ;
    debugger_initialConfigurations ;
    debugger_configurationSnippets
  }

type grammar = { (* TextMate grammar for syntax highlighting *)
  grammar_language : string option ; (* existing language ID *)
  grammar_scopeName : string ; (* "source.COBOL" *)
  grammar_path : string ; (* path to grammar file *)
  grammar_injectTo : string list ; [@dft []]
  grammar_embeddedLanguages : (string * string) list [@assoc] ; [@dft []]
}
[@@deriving json_encoding,show]

type icon = {
  icon_description : string ;
  icon_default : (string * string) list [@assoc] ;
}
[@@deriving json_encoding,show]

type iconTheme = {
  iconTheme_id : string ;
  iconTheme_label : string ;
  iconTheme_path : string ;
}
[@@deriving json_encoding,show]

type jsonValidation = {
  jsonValidation_fileMatch : string ; (* ".json" *)
  jsonValidation_url : string ; (* scheme URL *)
}
[@@deriving json_encoding,show]

let jsonValidation ~fileMatch ~url =
  { jsonValidation_fileMatch = fileMatch;
    jsonValidation_url = url }

type tomlValidation = {
  tomlValidation_fileMatch : string ; (* ".toml" *)
  tomlValidation_url : string ; (* schema URL *)
}
[@@deriving json_encoding,show]

let tomlValidation ~fileMatch ~url =
  { tomlValidation_fileMatch = fileMatch;
    tomlValidation_url = url }

type keybinding = {
  key_command : string ;
  key_key : string ; (* "ctrl+f1" *)
  key_mac : string option ;
  key_when : string option ; (* Javascript condition *)
  key_args : (string * string) list [@assoc] ; [@dft []]
}
[@@deriving json_encoding,show]
let keybinding
    ~key
    ?mac ?when_ ?( args = [] )
    key_command =
  {
    key_command ;
    key_key  = key ;
    key_mac = mac ;
    key_when = when_ ;
    key_args = args ;
  }

type language = {
  lang_id : string ;
  lang_extensions : string list ; [@dft []]
  lang_aliases : string list ; [@dft []]
  lang_filenames : string list ; [@dft []]
  lang_filenamePatterns : string list ; [@dft []]
  lang_firstLine : string option ;
  lang_configuration : string option ; [@dft None](* path *)
  lang_icon : command_icon option ;
}
[@@deriving json_encoding,show]

let language
    ?( extensions = [] )
    ?( aliases = [] )
    ?( filenames = [] )
    ?( filenamePatterns = [] )
    ?firstLine
    ?configuration
    ?icon
    id
  =
  {
    lang_id = id ;
    lang_extensions = extensions ;
    lang_aliases = aliases ;
    lang_filenames = filenames ;
    lang_filenamePatterns = filenamePatterns ;
    lang_firstLine = firstLine ;
    lang_configuration = configuration ;
    lang_icon = icon ;
  }

type menu = {
  menu_command : string option ;
  menu_group : string option ;
  menu_when : string option ;
  menu_key : string option ;
  menu_submenu : string option ;
}
[@@deriving json_encoding,show]
let menu ?command ?group ?when_ ?key ?submenu () =
  {
    menu_command = command ;
    menu_group = group ;
    menu_when = when_ ;
    menu_key = key ;
    menu_submenu = submenu ;
  }


type fileLocation = string list [@@deriving show]

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
  pat_name : string option ; (* only in problemPatterns *)
  pat_regexp : string option ;

  (* group positions within regexp *)
  pat_file : int option ;
  pat_line : int option ;
  pat_endLine : int option ;
  pat_column : int option ;
  pat_endColumn : int option ;
  pat_severity : int option ;
  pat_code : int option ;
  pat_location : int option ;
  pat_message : int option ;

  pat_loop : bool option ;
  pat_patterns : problemPattern list ; [@dft []] (* instead of regexp *)
}
[@@deriving json_encoding {recursive}, show]

let problemPattern
    ?name
    ?file
    ?line
    ?endLine
    ?column
    ?endColumn
    ?severity
    ?code
    ?location
    ?message
    ?loop
    regexp =
  {
    pat_name = name ;
    pat_regexp = regexp ;
    pat_file = file ;
    pat_line = line ;
    pat_endLine = endLine ;
    pat_column = column ;
    pat_endColumn = endColumn ;
    pat_severity = severity ;
    pat_code = code ;
    pat_location = location ;
    pat_message = message ;
    pat_loop = loop ;
    pat_patterns = [] ;
  }

type pattern =
    ProblemPattern of problemPattern
  | ProblemName of string
[@@deriving show]

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

type problemMatcher = {
  pm_name : string ;
  pm_owner : string option ;
  pm_fileLocation : fileLocation ; [@dft []]
  pm_pattern : pattern list_or_one ;
  pm_source : string option ;
  pm_severity : string option ; (* info, error, warning *)
}
[@@deriving json_encoding,show]

let problemMatcher ~name ?owner ?(fileLocation = []) ?( pattern = [])
    ?source ?severity () =
  {
    pm_name = name;
    pm_owner = owner;
    pm_fileLocation = fileLocation;
    pm_pattern = pattern;
    pm_source = source;
    pm_severity = severity;
  }


type productIconTheme = {
  pit_id : string ;
  pit_label : string ;
  pit_path : string ;
}
[@@deriving json_encoding,show]

(* A set of completions for a specific language *)
type snippet = {
  snippet_language : string ;
  snippet_path : string ;
}
[@@deriving json_encoding,show]
let snippet ~language ~path = {
  snippet_language = language ;
  snippet_path = path }

type submenu = {
  submenu_id : string ;
  submenu_label : string ;
}
[@@deriving json_encoding,show]
let submenu ~id ~label =
  { submenu_id = id ; submenu_label = label }

type taskDefinition = {
  task_type : string ;
  task_required : string list ; [@dft []]
  task_properties : ( string * property ) list [@assoc] ; [@dft []]
}
[@@deriving json_encoding,show]
let taskDefinition ?(required = []) ?(properties = []) task_type =
  {
    task_type ;
    task_required = required ;
    task_properties = properties ;
  }

type view = {
  view_id : string ;
  view_name : string ;
  view_when : string option;
  view_icon : string option;
  view_contextualTitle : string option;
}
[@@deriving json_encoding,show]
let view ~id ~name ?when_ ?icon ?contextualTitle () =
  {
    view_id = id ;
    view_name = name ;
    view_when = when_ ;
    view_icon = icon ;
    view_contextualTitle = contextualTitle ;
  }

type viewsContainer = {
  vc_id : string ;
  vc_title : string ;
  vc_icon : string option;
}
[@@deriving json_encoding,show]
let viewsContainer ~id ~title ?icon () =
  { vc_id = id ; vc_title = title ; vc_icon = icon }

type viewsWelcome = {
  vw_view : string ;
  vw_contents : string ;
  vw_when : string option;
}
[@@deriving json_encoding,show]
let viewsWelcome ~view ~contents ?when_ () =
  { vw_view = view ; vw_contents = contents ; vw_when = when_ }

type configurationDefaults =
 ( string * any ) list [@assoc]
[@@deriving json_encoding,show]

type configurationDefault =
  | Default of string
  | Defaults of ( string * any ) list
[@@deriving show]

let configurationDefault_enc =
  let open Json_encoding in
  union [
    case string
      (function Default s -> Some s | _ -> None )
      (fun s -> Default s) ;
    case configurationDefaults_enc
      (function Defaults s -> Some s | _ -> None )
      (fun s -> Defaults s) ;
  ]


type contributes = {
    breakpoints : breakpoint list ; [@dft []]
    colors : color list ; [@dft []]
    commands : command list ; [@dft []]
    configuration : configuration option ;
    configurationDefaults :
      ( string * configurationDefault ) list [@assoc] ; [@dft []]
    customEditors : customEditor list ; [@dft []]
    debuggers : debugger list ; [@dft []]
    grammars : grammar list ; [@dft []]
    icons : icon list [@assoc] ; [@dft []]
    iconThemes : iconTheme list ; [@dft []]
    jsonValidation : jsonValidation list ; [@dft []]
    tomlValidation : tomlValidation list ; [@dft []]
    keybindings : keybinding list ; [@dft []]
    languages : language list ; [@dft []]
    menus : ( string * menu list ) list [@assoc] ; [@dft []]
    problemMatchers : problemMatcher list ; [@dft []]
    problemPatterns : problemPattern list ; [@dft []]
    productIconThemes : productIconTheme list ; [@dft []]
    snippets : snippet list ; [@dft []]
    submenus : submenu list ; [@dft []]
    taskDefinitions : taskDefinition list ; [@dft []]
    views : ( string * view list ) list [@assoc] ; [@dft []]

    (* "activitybar" or "panel" *)
    viewsContainers : ( string * viewsContainer list ) list [@assoc] ; [@dft []]
    viewsWelcome : viewsWelcome list ; [@dft []]

    (* TODO *)
    resourceLabelFormatters : any option ;
    semanticTokenModifiers : any option ;
    semanticTokenScopes : any option ;
    semanticTokenTypes : any option ;
    terminal : any option ;
    themes : any option ;
    typescriptServerPlugins : any option ;
    walkthroughs : any option ;
  }
  [@@deriving json_encoding,show]
let contributes
  ?( breakpoints = [] )
  ?( colors = [] )
  ?( commands = [] )
  ?configuration
  ?( configurationDefaults = [] )
  ?( customEditors = [] )
  ?( debuggers = [] )
  ?( grammars = [] )
  ?( icons = [] )
  ?( iconThemes = [] )
  ?( jsonValidation = [] )
  ?( tomlValidation = [] )
  ?( keybindings = [] )
  ?( languages = [] )
  ?( menus = [] )
  ?( problemMatchers = [] )
  ?( problemPatterns = [] )
  ?( productIconThemes = [] )
  ?( snippets = [] )
  ?( submenus = [] )
  ?( taskDefinitions = [] )
  ?( views = [] )
  ?( viewsContainers = [] )
  ?( viewsWelcome = [] )
  () =
  {
    breakpoints ;
    colors ;
    commands ;
    configuration ;
    configurationDefaults ;
    customEditors ;
    debuggers ;
    grammars ;
    icons ;
    iconThemes ;
    jsonValidation ;
    tomlValidation ;
    keybindings ;
    languages ;
    menus ;
    problemMatchers ;
    problemPatterns ;
    productIconThemes ;
    snippets ;
    submenus ;
    taskDefinitions ;
    views ;
    viewsContainers ;
    viewsWelcome ;
    resourceLabelFormatters = None ;
    semanticTokenModifiers = None ;
    semanticTokenScopes  = None;
    semanticTokenTypes = None ;
    terminal = None ;
    themes = None ;
    typescriptServerPlugins = None ;
    walkthroughs = None ;
  }

type sponsor = {
  sponsor_url : string ; [@key "url"]
}
[@@deriving json_encoding,show]
let sponsor sponsor_url = { sponsor_url }

type galleryBanner = {
  gallery_color : string option ;
  gallery_theme : string option ;
}
[@@deriving json_encoding,show]
let galleryBanner ?color ?theme () =
  { gallery_color = color ; gallery_theme = theme }

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
[@@deriving json_encoding,show]

let marketplace
    ?(categories=[]) ?icon ?preview ?(badges=[])
    ?markdown ?qna ?sponsor ?galleryBanner
    publisher =
  {
    publisher ;
    categories ;
    icon ;
    preview ;
    badges ;
    markdown ;
    qna ;
    sponsor ;
    galleryBanner ;
  }

type vscode = {
  engines : engines ;

  activationEvents : string list ; [@dft []]
  contributes : contributes option ;
  extensionKind :  string list ; [@dft []] (* "ui" or/and "workspace" *)
  extensionPack : string list ; [@dft []]
  extensionDependencies : string list ; [@dft []]

  package : package ; [@merge]
  marketplace : marketplace ; [@merge]
}
[@@deriving json_encoding {ignore}]

let make_engines = engines
let vscode
    ~engines
    ?(activationEvents=[])
    ?contributes
    ?(extensionKind = [])
    ?(extensionPack = [])
    ?(extensionDependencies = [])
    ~marketplace
    package
  =
  {
    engines = make_engines ~vscode:engines ;
    activationEvents ;
    contributes ;
    extensionKind ;
    extensionPack ;
    extensionDependencies ;
    marketplace ;
    package ;
  }
