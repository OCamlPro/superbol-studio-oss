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

let file = "tasks.json"
let filename = ".vscode/" ^ file
let version = "2.0.0"

type 'a string_or =
  | String of string
  | Or of 'a
[@@deriving show]

let string_or_enc encoding =
  let open Json_encoding in
  union [
    case string
      (function
        | String s -> Some s
        | _ -> None)
      (fun s -> String s) ;
    case encoding
      (function
        | Or s -> Some s
        | _ -> None)
      (fun s -> Or s)
  ]

(**
 * A description to when and how run a task.
 *)
type runOptions = {
  (**
   * Controls how variables are evaluated when a task is executed through
   * the Rerun Last Task command.
   * The default is `true`, meaning that variables will be re-evaluated when
   * a task is rerun. When set to `false`, the resolved variable values from
   * the previous run of the task will be used.
   *)
  reevaluateOnRerun :  bool option ;

  (**
   * Specifies when a task is run.
   *
   * Valid values are:
   *   "default": The task will only be run when executed through the Run Task command.
   *   "folderOpen": The task will be run when the containing folder is opened.
   *)
  runOn :  string option ;
}
[@@deriving json_encoding,show]

type problemPattern = {
  (**
   * The regular expression to find a problem in the console output of an
   * executed task.
   *)
  regexp: string;

  (**
   * Whether the pattern matches a problem for the whole file or for a location
   * inside a file.
   *
   * Defaults to "location".
   *)
  kind :  string option ; (* 'file' | 'location' *)

  (**
   * The match group index of the filename.
   *)
  file: int;

  (**
   * The match group index of the problem's location. Valid location
   * patterns are: (line), (line,column) and (startLine,startColumn,endLine,endColumn).
   * If omitted the line and column properties are used.
   *)
  location :  int option ;

  (**
   * The match group index of the problem's line in the source file.
   * Can only be omitted if location is specified.
   *)
  line :  int option ;

  (**
   * The match group index of the problem's column in the source file.
   *)
  column :  int option ;

  (**
   * The match group index of the problem's end line in the source file.
   *
   * Defaults to undefined. No end line is captured.
   *)
  endLine :  int option ;

  (**
   * The match group index of the problem's end column in the source file.
   *
   * Defaults to undefined. No end column is captured.
   *)
  endColumn :  int option ;

  (**
   * The match group index of the problem's severity.
   *
   * Defaults to undefined. In this case the problem matcher's severity
   * is used.
   *)
  severity :  int option ;

  (**
   * The match group index of the problem's code.
   *
   * Defaults to undefined. No code is captured.
   *)
  code :  int option ;

  (**
   * The match group index of the message. Defaults to 0.
   *)
  message: int;

  (**
   * Specifies if the last pattern in a multi line problem matcher should
   * loop as long as it does match a line consequently. Only valid on the
   * last problem pattern in a multi line problem matcher.
   *)
  loop :  bool option ;
}
[@@deriving json_encoding,show]


(**
 * A description to track the start and end of a background task.
 *)
type backgroundMatcher = {
  (**
   * If set to true the watcher is in active mode when the task
   * starts. This is equals of issuing a line that matches the
   * beginPattern.
   *)
  activeOnStart :  bool option ;

  (**
   * If matched in the output the start of a background task is signaled.
   *)
  beginsPattern :  string option ;

  (**
   * If matched in the output the end of a background task is signaled.
   *)
  endsPattern :  string option ;
}
[@@deriving json_encoding,show]


(**
 * A description of a problem matcher that detects problems
 * in build output.
 *)
type problemMatcher = {
  (**
   * The name of a base problem matcher to use. If specified the
   * base problem matcher will be used as a template and properties
   * specified here will replace properties of the base problem
   * matcher
   *)
  base :  string option ;

  (**
   * The owner of the produced VS Code problem. This is typically
   * the identifier of a VS Code language service if the problems are
   * to be merged with the one produced by the language service
   * or 'external'. Defaults to 'external' if omitted.
   *)
  owner :  string option ;

  (**
   * The severity of the VS Code problem produced by this problem matcher.
   *
   * Valid values are:
   *   "error": to produce errors.
   *   "warning": to produce warnings.
   *   "info": to produce infos.
   *
   * The value is used if a pattern doesn't specify a severity match group.
   * Defaults to "error" if omitted.
   *)
  severity :  string option ;

  (**
   * Defines how filename reported in a problem pattern
   * should be read. Valid values are:
   *  - "absolute": the filename is always treated absolute.
   *  - "relative": the filename is always treated relative to
   *    the current working directory. This is the default.
   *  - ["relative", "path value"]: the filename is always
   *    treated relative to the given path value.
   *  - "autodetect": the filename is treated relative to
   *    the current workspace directory, and if the file
   *    does not exist, it is treated as absolute.
   *  - ["autodetect", "path value"]: the filename is treated
   *    relative to the given path value, and if it does not
   *    exist, it is treated as absolute.
   *)
  fileLocation :  string Manifest.list_or_one ; [@dft []]

  (**
   * The name of a predefined problem pattern, the inline definition
   * of a problem pattern or an array of problem patterns to match
   * problems spread over multiple lines.
   *)
  pattern :  problemPattern list string_or option ;

  (**
   * Additional information used to detect when a background task (like a watching task in Gulp)
   * is active.
   *)
  background :  backgroundMatcher option ;
}
[@@deriving json_encoding,show]


type presentationOptions = {
  (**
   * Controls whether the task output is reveal in the user interface.
   * Defaults to `always`.
   *)
  reveal :  string option ; (* 'never' | 'silent' | 'always' *)

  (**
   * Controls whether the command associated with the task is echoed
   * in the user interface. Defaults to `true`.
   *)
  echo :  bool ; [@dft true]

  (**
   * Controls whether the panel showing the task output is taking focus.
   * Defaults to `false`.
   *)
  focus :  bool ; [@dft false]

  (**
   * Controls if the task panel is used for this task only (dedicated),
   * shared between tasks (shared) or if a new panel is created on
   * every task execution (new). Defaults to `shared`.
   *)
  panel : string option ; (*  'shared' | 'dedicated' | 'new' *)

  (**
   * Controls whether to show the `Terminal will be reused by tasks,
   * press any key to close it` message.
   *)
  showReuseMessage :  bool option ;

  (**
   * Controls whether the terminal is cleared before this task is run.
   * Defaults to `false`.
   *)
  clear :  bool ; [@dft false]

  (**
   * Controls whether the task is executed in a specific terminal
   * group using split panes. Tasks in the same group (specified by a string value)
   * will use split terminals to present instead of a new terminal panel.
   *)
  group :  string option ;
}
[@@deriving json_encoding,show]

type groupDescription = {
    kind : string option ; (* 'build' | 'test' *)
    isDefault : bool
    }
[@@deriving json_encoding,show]


(**
 * The description of a task.
 *)
type taskDescription = {
  (**
   * The task's name
   *)
  label : string option;

  (**
   * The type of a custom task. Tasks of type "shell" are executed
   * inside a shell (e.g. bash, cmd, powershell, ...)
   *)
  type_ : string ; (* 'shell' | 'process'; *) [@key "type"]

  (**
   * The command to execute. If the type is "shell" it should be the full
   * command line including any additional arguments passed to the command.
   *)
  command : string;

  (**
   * Whether the executed command is kept alive and runs in the background.
   *)
  isBackground :  bool option ;

  (**
   * Additional arguments passed to the command. Should be used if type
   * is "process".
   *)
  args :  string list ; [@dft []]

  (**
   * Defines the group to which this task belongs. Also supports to mark
   * a task as the default task in a group.
   *)
  group : groupDescription string_or ; (* 'build' | 'test' *)

  (**
   * The presentation options.
   *)
  presentation :  presentationOptions option ;

  (**
   * The problem matcher(s) to use to capture problems in the tasks
   * output.
   *)
  problemMatcher :  problemMatcher string_or Manifest.list_or_one ; [@dft []]

  (**
   * Defines when and how a task is run.
   *)
  runOptions :  runOptions option ;


  dependsOn : string Manifest.list_or_one ; [@dft []]
  dependsOrder : string option ;
}
[@@deriving json_encoding,show]


type shellDescription = {
    (**
     * The shell to use.
     *)
    executable: string;

    (**
     * The arguments to be passed to the shell executable to run in command mode
     * (e.g ['-c'] for bash or ['/S', '/C'] for cmd.exe).
     *)
    args :  string list ; [@dft []]
  }
[@@deriving json_encoding,show]

(**
 * Options to be passed to the external program or shell
 *)
type commandOptions = {
  (**
   * The current working directory of the executed program or shell.
   * If omitted the current workspace's root is used.
   *)
  cwd: string option; (* "${workspaceFolder}/dir1/dir2/dir3/" *)

  (**
   * The environment of the executed program or shell. If omitted
   * the parent process' environment is used.
   *)
  env : ( string * string ) list [@assoc] ; [@dft []]

  (**
   * Configuration of the shell when task type is `shell`
   *)
  shell: shellDescription option ;
}
[@@deriving json_encoding,show]

type baseTaskConfiguration = {
  (**
   * The type of a custom task. Tasks of type "shell" are executed
   * inside a shell (e.g. bash, cmd, powershell, ...)
   *)
  type_: string option ; (* 'shell' | 'process' *) [@key "type"]

  (**
   * The command to be executed. Can be an external program or a shell
   * command.
   *)
  command: string option;

  (**
   * Specifies whether a global command is a background task.
   *)
  isBackground: bool option;

  (**
   * The command options used when the command is executed. Can be omitted.
   *)
  options: commandOptions option;

  (**
   * The arguments passed to the command. Can be omitted.
   *)
  args: string list; [@dft []]

  (**
   * The presentation options.
   *)
  (* TODO presentation :  PresentationOptions option ; *)

  (**
   * The problem matcher to be used if a global command is executed (e.g. no tasks
   * are defined). A tasks.json file can either contain a global problemMatcher
   * property or a tasks property but not both.
   *)
  (* TODO problemMatcher :  string | ProblemMatcher | (string | ProblemMatcher)[] option ; *)

  (**
   * The configuration of the available tasks. A tasks.json file can either
   * contain a global problemMatcher property or a tasks property but not both.
   *)
  tasks: taskDescription list; [@dft []]
}
[@@deriving json_encoding,show]

type taskConfiguration = {
  base : baseTaskConfiguration ; [@merge]
  (**
   * The configuration's version number
   *)
  version: string ;

  (**
   * Windows specific task configuration
   *)
  windows: baseTaskConfiguration option;

  (**
   * macOS specific task configuration
   *)
  osx: baseTaskConfiguration option;

  (**
   * Linux specific task configuration
   *)
  linux: baseTaskConfiguration option;
}
[@@deriving json_encoding,show]

let encoding = taskConfiguration_enc
let pp = pp_taskConfiguration
