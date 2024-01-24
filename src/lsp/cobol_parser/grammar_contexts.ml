(* Caution this file was automatically generated from grammar.cmly; do not edit *)
module TH = Text_lexer.TokenHandles
type context =
  | Accept_stmt
  | Allocate_stmt
  | Alphabet_clause
  | Arithmetic_clause
  | Class_specifier
  | Column_clause
  | Constant
  | Currency_clause
  | Default_clause
  | Dynlen_struct_clause
  | Entry_convention_clause
  | Erase_clause
  | Exit_stmt
  | Factory_paragraph
  | Float_binary_clause
  | Float_decimal_clause
  | Function_specifier
  | Interface_specifier
  | Intermediate_rounding_clause
  | Line_clause
  | Lock_mode_clause
  | Object_computer_paragraph
  | Object_paragraph
  | Occurs_clause
  | Options_paragrahp
  | Options_paragraph
  | Options_pragraph
  | Perform_stmt
  | Program_id_paragraph
  | Read_statement
  | Read_stmt
  | Resume_stmt
  | Retry_phrase
  | Rounded_phrase
  | Screen_descr_entry
  | Screnn_descr_entry
  | Set_attribute_stmt
  | Set_stmt
  | Sharing_clause
  | Sharing_phrase
  | Stop_stmt
  | Typedef_clause
  | Usage_clause
  | Validate_status_clause
type t = context

type context_tokens =
  {
    accept_stmt: TH.t;
    allocate_stmt: TH.t;
    alphabet_clause: TH.t;
    arithmetic_clause: TH.t;
    class_specifier: TH.t;
    column_clause: TH.t;
    constant: TH.t;
    currency_clause: TH.t;
    default_clause: TH.t;
    dynlen_struct_clause: TH.t;
    entry_convention_clause: TH.t;
    erase_clause: TH.t;
    exit_stmt: TH.t;
    factory_paragraph: TH.t;
    float_binary_clause: TH.t;
    float_decimal_clause: TH.t;
    function_specifier: TH.t;
    interface_specifier: TH.t;
    intermediate_rounding_clause: TH.t;
    line_clause: TH.t;
    lock_mode_clause: TH.t;
    object_computer_paragraph: TH.t;
    object_paragraph: TH.t;
    occurs_clause: TH.t;
    options_paragrahp: TH.t;
    options_paragraph: TH.t;
    options_pragraph: TH.t;
    perform_stmt: TH.t;
    program_id_paragraph: TH.t;
    read_statement: TH.t;
    read_stmt: TH.t;
    resume_stmt: TH.t;
    retry_phrase: TH.t;
    rounded_phrase: TH.t;
    screen_descr_entry: TH.t;
    screnn_descr_entry: TH.t;
    set_attribute_stmt: TH.t;
    set_stmt: TH.t;
    sharing_clause: TH.t;
    sharing_phrase: TH.t;
    stop_stmt: TH.t;
    typedef_clause: TH.t;
    usage_clause: TH.t;
    validate_status_clause: TH.t;
  }

type handles =
  {
    context_tokens: context_tokens;
    context_sensitive_tokens: TH.t;
    context_sensitive_tokens_unimplemented: TH.t;
  }

let init ~handle_of_token =
  let open TH in
  let empty =
    {
      accept_stmt = empty;
      allocate_stmt = empty;
      alphabet_clause = empty;
      arithmetic_clause = empty;
      class_specifier = empty;
      column_clause = empty;
      constant = empty;
      currency_clause = empty;
      default_clause = empty;
      dynlen_struct_clause = empty;
      entry_convention_clause = empty;
      erase_clause = empty;
      exit_stmt = empty;
      factory_paragraph = empty;
      float_binary_clause = empty;
      float_decimal_clause = empty;
      function_specifier = empty;
      interface_specifier = empty;
      intermediate_rounding_clause = empty;
      line_clause = empty;
      lock_mode_clause = empty;
      object_computer_paragraph = empty;
      object_paragraph = empty;
      occurs_clause = empty;
      options_paragrahp = empty;
      options_paragraph = empty;
      options_pragraph = empty;
      perform_stmt = empty;
      program_id_paragraph = empty;
      read_statement = empty;
      read_stmt = empty;
      resume_stmt = empty;
      retry_phrase = empty;
      rounded_phrase = empty;
      screen_descr_entry = empty;
      screnn_descr_entry = empty;
      set_attribute_stmt = empty;
      set_stmt = empty;
      sharing_clause = empty;
      sharing_phrase = empty;
      stop_stmt = empty;
      typedef_clause = empty;
      usage_clause = empty;
      validate_status_clause = empty;
    }
  in
  let accept_stmt t c = { c with accept_stmt = add t c.accept_stmt } in
  let allocate_stmt t c = { c with allocate_stmt = add t c.allocate_stmt } in
  let alphabet_clause t c = { c with alphabet_clause = add t c.alphabet_clause } in
  let arithmetic_clause t c = { c with arithmetic_clause = add t c.arithmetic_clause } in
  let class_specifier t c = { c with class_specifier = add t c.class_specifier } in
  let column_clause t c = { c with column_clause = add t c.column_clause } in
  let constant t c = { c with constant = add t c.constant } in
  let currency_clause t c = { c with currency_clause = add t c.currency_clause } in
  let default_clause t c = { c with default_clause = add t c.default_clause } in
  let dynlen_struct_clause t c = { c with dynlen_struct_clause = add t c.dynlen_struct_clause } in
  let entry_convention_clause t c = { c with entry_convention_clause = add t c.entry_convention_clause } in
  let erase_clause t c = { c with erase_clause = add t c.erase_clause } in
  let exit_stmt t c = { c with exit_stmt = add t c.exit_stmt } in
  let factory_paragraph t c = { c with factory_paragraph = add t c.factory_paragraph } in
  let float_binary_clause t c = { c with float_binary_clause = add t c.float_binary_clause } in
  let float_decimal_clause t c = { c with float_decimal_clause = add t c.float_decimal_clause } in
  let function_specifier t c = { c with function_specifier = add t c.function_specifier } in
  let interface_specifier t c = { c with interface_specifier = add t c.interface_specifier } in
  let intermediate_rounding_clause t c = { c with intermediate_rounding_clause = add t c.intermediate_rounding_clause } in
  let line_clause t c = { c with line_clause = add t c.line_clause } in
  let lock_mode_clause t c = { c with lock_mode_clause = add t c.lock_mode_clause } in
  let object_computer_paragraph t c = { c with object_computer_paragraph = add t c.object_computer_paragraph } in
  let object_paragraph t c = { c with object_paragraph = add t c.object_paragraph } in
  let occurs_clause t c = { c with occurs_clause = add t c.occurs_clause } in
  let options_paragrahp t c = { c with options_paragrahp = add t c.options_paragrahp } in
  let options_paragraph t c = { c with options_paragraph = add t c.options_paragraph } in
  let options_pragraph t c = { c with options_pragraph = add t c.options_pragraph } in
  let perform_stmt t c = { c with perform_stmt = add t c.perform_stmt } in
  let program_id_paragraph t c = { c with program_id_paragraph = add t c.program_id_paragraph } in
  let read_statement t c = { c with read_statement = add t c.read_statement } in
  let read_stmt t c = { c with read_stmt = add t c.read_stmt } in
  let resume_stmt t c = { c with resume_stmt = add t c.resume_stmt } in
  let retry_phrase t c = { c with retry_phrase = add t c.retry_phrase } in
  let rounded_phrase t c = { c with rounded_phrase = add t c.rounded_phrase } in
  let screen_descr_entry t c = { c with screen_descr_entry = add t c.screen_descr_entry } in
  let screnn_descr_entry t c = { c with screnn_descr_entry = add t c.screnn_descr_entry } in
  let set_attribute_stmt t c = { c with set_attribute_stmt = add t c.set_attribute_stmt } in
  let set_stmt t c = { c with set_stmt = add t c.set_stmt } in
  let sharing_clause t c = { c with sharing_clause = add t c.sharing_clause } in
  let sharing_phrase t c = { c with sharing_phrase = add t c.sharing_phrase } in
  let stop_stmt t c = { c with stop_stmt = add t c.stop_stmt } in
  let typedef_clause t c = { c with typedef_clause = add t c.typedef_clause } in
  let usage_clause t c = { c with usage_clause = add t c.usage_clause } in
  let validate_status_clause t c = { c with validate_status_clause = add t c.validate_status_clause } in
  let specs = Grammar_tokens.[
    ACTION, [];
    ACTIVATING, [];
    ACTIVE_X, [];
    ACTUAL, [];
    ADJUSTABLE_COLUMNS, [];
    ALIGNMENT, [];
    ALLOWING, [];
    ANUM, [];
    APPLY, [];
    ARITHMETIC, [options_paragraph];
    ASCII, [];
    ATTRIBUTE, [set_stmt];
    ATTRIBUTES, [];
    AUTO, [screen_descr_entry];
    AUTOMATIC, [lock_mode_clause];
    AUTO_DECIMAL, [];
    AUTO_SPIN, [];
    AWAY_FROM_ZERO, [rounded_phrase];
    BACKGROUND_COLOR, [screen_descr_entry];
    BACKWARD, [];
    BAR, [];
    BELL, [screen_descr_entry; set_attribute_stmt];
    BINARY_ENCODING, [usage_clause; set_attribute_stmt];
    BITMAP, [];
    BITMAP_END, [];
    BITMAP_HANDLE, [];
    BITMAP_NUMBER, [];
    BITMAP_START, [];
    BITMAP_TIMER, [];
    BITMAP_TRAILING, [];
    BITMAP_TRANSPARENT_COLOR, [];
    BITMAP_WIDTH, [];
    BLINK, [screen_descr_entry; set_attribute_stmt];
    BOX, [];
    BOXED, [];
    BULK_ADDITION, [];
    BUSY, [];
    BUTTONS, [];
    BYTE, [];
    BYTE_LENGTH, [constant];
    C, [];
    CALENDAR_FONT, [];
    CANCEL_BUTTON, [];
    CAPACITY, [occurs_clause];
    CARD_PUNCH, [];
    CARD_READER, [];
    CASSETTE, [];
    CCOL, [];
    CELL, [];
    CELL_COLOR, [];
    CELL_DATA, [];
    CELL_FONT, [];
    CELL_PROTECTION, [];
    CENTER, [column_clause];
    CENTERED, [];
    CENTERED_HEADINGS, [];
    CENTURY_DATE, [];
    CHANGED, [];
    CHECK_BOX, [];
    CLASSIFICATION, [object_computer_paragraph];
    CLEAR_SELECTION, [];
    CLINE, [];
    CLINES, [];
    COBOL, [entry_convention_clause];
    COLORS, [];
    COLUMN_COLOR, [];
    COLUMN_DIVIDERS, [];
    COLUMN_FONT, [];
    COLUMN_HEADINGS, [];
    COLUMN_PROTECTION, [];
    COMBO_BOX, [];
    CONVERSION, [];
    COPY_SELECTION, [];
    CORE_INDEX, [];
    CSIZE, [];
    CURRENT, [];
    CURSOR_COL, [];
    CURSOR_COLOR, [];
    CURSOR_FRAME_WIDTH, [];
    CURSOR_ROW, [];
    CURSOR_X, [];
    CURSOR_Y, [];
    CUSTOM_PRINT_TEMPLATE, [];
    CYCLE, [];
    CYL_INDEX, [];
    CYL_OVERFLOW, [];
    DASHED, [];
    DATA_COLUMNS, [];
    DATA_TYPES, [];
    DATE_ENTRY, [];
    DECIMAL_ENCODING, [usage_clause; float_decimal_clause];
    DEFAULT_BUTTON, [];
    DISC, [];
    DISK, [];
    DISP, [];
    DISPLAY_COLUMNS, [];
    DISPLAY_FORMAT, [];
    DIVIDERS, [];
    DIVIDER_COLOR, [];
    DOTDASH, [];
    DOTTED, [];
    DRAG_COLOR, [];
    DROP_DOWN, [];
    DROP_LIST, [];
    EBCDIC, [];
    ELEMENT, [];
    ENCODING, [];
    ENCRYPTION, [];
    END_COLOR, [];
    END_MODIFY, [];
    ENGRAVED, [];
    ENSURE_VISIBLE, [];
    ENTRY_CONVENTION, [options_paragraph];
    ENTRY_FIELD, [];
    ENTRY_REASON, [];
    EOL, [erase_clause];
    EOS, [erase_clause];
    ERASE, [screen_descr_entry];
    ESCAPE_BUTTON, [];
    EVENT_LIST, [];
    EVERY, [];
    EXCEPTION_VALUE, [];
    EXPAND, [];
    EXPANDS, [class_specifier; interface_specifier];
    EXTENDED_SEARCH, [];
    EXTERN, [];
    F, [];
    FH__FCD, [];
    FH__KEYDEF, [];
    FILE_LIMIT, [];
    FILE_LIMITS, [];
    FILE_NAME, [];
    FILE_POS, [];
    FILL_COLOR, [];
    FILL_COLOR2, [];
    FILL_PERCENT, [];
    FINISH_REASON, [];
    FIXED_WIDTH, [];
    FLAT, [];
    FLAT_BUTTONS, [];
    FLOAT_BINARY, [options_paragraph];
    FLOAT_DECIMAL, [options_paragrahp];
    FLOAT_NOT_A_NUMBER, [];
    FOREGROUND_COLOR, [screen_descr_entry];
    FOREVER, [retry_phrase; perform_stmt];
    FRAME, [];
    FRAMED, [];
    FULL, [screnn_descr_entry];
    FULL_HEIGHT, [];
    GO_BACK, [];
    GO_FORWARD, [];
    GO_HOME, [];
    GO_SEARCH, [];
    GRAPHICAL, [];
    GRID, [];
    GROUP_VALUE, [];
    HAS_CHILDREN, [];
    HEADING_COLOR, [];
    HEADING_DIVIDER_COLOR, [];
    HEADING_FONT, [];
    HEAVY, [];
    HEIGHT_IN_CELLS, [];
    HEX, [];
    HIGHLIGHT, [screen_descr_entry];
    HIGH_COLOR, [];
    HIGH_ORDER_LEFT, [float_binary_clause;
    float_decimal_clause];
    HIGH_ORDER_RIGHT, [float_binary_clause; float_decimal_clause];
    HOT_TRACK, [];
    HSCROLL, [];
    HSCROLL_POS, [];
    ICON, [];
    IGNORING, [read_statement];
    IMPLEMENTS, [factory_paragraph; object_paragraph];
    INDEPENDENT, [];
    INITIALIZED, [allocate_stmt; occurs_clause];
    INSERTION_INDEX, [];
    INSERT_ROWS, [];
    INTERMEDIATE, [options_pragraph];
    INTRINSIC, [function_specifier];
    ITEM, [];
    ITEM_TEXT, [];
    ITEM_TO_ADD, [];
    ITEM_TO_DELETE, [];
    ITEM_TO_EMPTY, [];
    ITEM_VALUE, [];
    KEYBOARD, [];
    LABEL_OFFSET, [];
    LARGE_OFFSET, [];
    LAST_ROW, [];
    LAYOUT_DATA, [];
    LC_ALL, [set_stmt];
    LC_COLLATE, [set_stmt];
    LC_CTYPE, [set_stmt];
    LC_MESSAGES, [set_stmt];
    LC_MONETARY, [set_stmt];
    LC_NUMERIC, [set_stmt];
    LC_TIME, [set_stmt];
    LEADING_SHIFT, [];
    LEAVE, [];
    LEFT_TEXT, [];
    LINES_AT_ROOT, [];
    LINE_SEQUENTIAL, [];
    LIST_BOX, [];
    LOC, [];
    LOCATION, [];
    LOCK_HOLDING, [];
    LONG_DATE, [];
    LOWER, [];
    LOWERED, [];
    LOWLIGHT, [screen_descr_entry; set_attribute_stmt];
    LOW_COLOR, [];
    MAGNETIC_TAPE, [];
    MANUAL, [lock_mode_clause];
    MASS_UPDATE, [];
    MASTER_INDEX, [];
    MAX_LINES, [];
    MAX_PROGRESS, [];
    MAX_TEXT, [];
    MAX_VAL, [];
    MEMORY, [];
    MICROSECOND_TIME, [];
    MIN_VAL, [];
    MODULES, [];
    MULTILINE, [];
    NAME, [];
    NAMED, [];
    NAMESPACE, [];
    NAMESPACE_PREFIX, [];
    NAT, [];
    NAVIGATE_URL, [];
    NEAREST_AWAY_FROM_ZERO, [intermediate_rounding_clause;
    rounded_phrase];
    NEAREST_EVEN, [intermediate_rounding_clause;
    rounded_phrase];
    NEAREST_TOWARD_ZERO, [intermediate_rounding_clause;
    rounded_phrase];
    NEXT_ITEM, [];
    NOMINAL, [];
    NONE, [default_clause];
    NONNUMERIC, [];
    NORMAL, [stop_stmt];
    NOTAB, [];
    NOTIFY, [];
    NOTIFY_CHANGE, [];
    NOTIFY_DBLCLICK, [];
    NOTIFY_SELCHANGE, [];
    NO_AUTOSEL, [];
    NO_AUTO_DEFAULT, [];
    NO_BOX, [];
    NO_DIVIDERS, [];
    NO_F4, [];
    NO_FOCUS, [];
    NO_GROUP_TAB, [];
    NO_KEY_LETTER, [];
    NO_SEARCH, [];
    NO_UPDOWN, [];
    NUMBERS, [column_clause; line_clause];
    NUM_COL_HEADINGS, [];
    NUM_ROWS, [];
    OK_BUTTON, [];
    ONLY, [sharing_clause; sharing_phrase; usage_clause];
    OTHERS, [];
    OVERLAP_LEFT, [];
    OVERLAP_TOP, [];
    PAGED, [];
    PAGE_SETUP, [];
    PARAGRAPH, [exit_stmt];
    PARENT, [];
    PARSE, [];
    PASCAL, [];
    PASSWORD, [];
    PERMANENT, [];
    PIXEL, [];
    PLACEMENT, [];
    POP_UP, [];
    POS, [];
    POSITION_SHIFT, [];
    PREFIXED, [dynlen_struct_clause];
    PREVIOUS, [read_stmt];
    PRINT, [];
    PRINTER, [];
    PRINTER_1, [];
    PRINT_NO_PROMPT, [];
    PRINT_PREVIEW, [];
    PROCESSING, [];
    PROGRESS, [];
    PROHIBITED, [intermediate_rounding_clause; rounded_phrase];
    PROPERTIES, [];
    PROTECTED, [];
    PUSH_BUTTON, [];
    QUERY_INDEX, [];
    RADIO_BUTTON, [];
    RAISED, [];
    READERS, [];
    READ_ONLY, [];
    RECORD_DATA, [];
    RECORD_OVERFLOW, [];
    RECORD_TO_ADD, [];
    RECORD_TO_DELETE, [];
    RECURSIVE, [program_id_paragraph];
    REFRESH, [];
    REGION_COLOR, [];
    RELATION, [validate_status_clause];
    REORG_CRITERIA, [];
    REQUIRED, [screen_descr_entry];
    REREAD, [];
    RERUN, [];
    RESET_GRID, [];
    RESET_LIST, [];
    RESET_TABS, [];
    REVERSE_VIDEO, [screen_descr_entry; set_attribute_stmt];
    RIGHT_ALIGN, [];
    RIMMED, [];
    ROUNDING, [options_paragraph];
    ROW_COLOR, [];
    ROW_COLOR_PATTERN, [];
    ROW_DIVIDERS, [];
    ROW_FONT, [];
    ROW_HEADINGS, [];
    ROW_PROTECTION, [];
    S, [];
    SAVE_AS, [];
    SAVE_AS_NO_PROMPT, [];
    SCROLL, [];
    SCROLL_BAR, [];
    SEARCH_OPTIONS, [];
    SEARCH_TEXT, [];
    SECONDS, [retry_phrase];
    SECURE, [screen_descr_entry];
    SELECTION_INDEX, [];
    SELECTION_TEXT, [];
    SELECT_ALL, [];
    SELF_ACT, [];
    SEPARATION, [];
    SHADING, [];
    SHADOW, [];
    SHORT, [dynlen_struct_clause];
    SHOW_LINES, [];
    SHOW_NONE, [];
    SHOW_SEL_ALWAYS, [];
    SIGNED, [dynlen_struct_clause; usage_clause];
    SORT_ORDER, [];
    SPINNER, [];
    SQUARE, [];
    STACK, [];
    STANDARD_BINARY, [arithmetic_clause];
    STANDARD_DECIMAL, [arithmetic_clause];
    START_X, [];
    START_Y, [];
    STATEMENT, [resume_stmt];
    STATIC, [];
    STATIC_LIST, [];
    STATUS_BAR, [];
    STATUS_TEXT, [];
    STDCALL, [];
    STEP, [occurs_clause];
    STRONG, [typedef_clause];
    STRUCTURE, [dynlen_struct_clause];
    STYLE, [];
    SYMBOL, [currency_clause];
    SYSTEM_INFO, [];
    TAB, [];
    TAB_TO_ADD, [];
    TAB_TO_DELETE, [];
    TAPE, [];
    TEMPORARY, [];
    TERMINAL_INFO, [];
    TERMINATION_VALUE, [];
    THREEDIMENSIONAL, [];
    THUMB_POSITION, [];
    TILED_HEADINGS, [];
    TIME_OUT, [];
    TITLE, [];
    TITLE_POSITION, [];
    TOP_LEVEL, [];
    TOWARD_GREATER, [rounded_phrase];
    TOWARD_LESSER, [rounded_phrase];
    TRACK, [];
    TRACKS, [];
    TRACK_AREA, [];
    TRACK_LIMIT, [];
    TRAILING_SHIFT, [];
    TRANSPARENT, [];
    TREE_VIEW, [];
    TRUNCATION, [intermediate_rounding_clause; rounded_phrase];
    U, [];
    UCS_4, [alphabet_clause];
    UNBOUNDED, [];
    UNDERLINE, [screen_descr_entry; set_attribute_stmt];
    UNFRAMED, [];
    UNSIGNED, [usage_clause];
    UNSORTED, [];
    UPDATERS, [];
    UPPER, [];
    USER, [];
    USE_ALT, [];
    USE_RETURN, [];
    USE_TAB, [];
    UTF_16, [alphabet_clause];
    UTF_8, [alphabet_clause];
    V, [];
    VALIDATING, [];
    VALUE_FORMAT, [];
    VARIABLE, [];
    VERTICAL, [];
    VERY_HEAVY, [];
    VIRTUAL_WIDTH, [];
    VPADDING, [];
    VSCROLL, [];
    VSCROLL_BAR, [];
    VSCROLL_POS, [];
    VTOP, [];
    WEB_BROWSER, [];
    WIDTH, [];
    WIDTH_IN_CELLS, [];
    WRAP, [];
    WRITERS, [];
    WRITE_ONLY, [];
    WRITE_VERIFY, [];
    X, [];
    XML_DECLARATION, [];
    XML_SCHEMA, [];
    Y, [];
    YYYYDDD, [accept_stmt];
    YYYYMMDD, [accept_stmt];
    ZERO_FILL, [];
    ]
  in
  let context_tokens,
      context_sensitive_tokens,
      context_sensitive_tokens_unimplemented =
    List.fold_left (fun (acc, cstoks, unimpl) (t, add_contexts) ->
      let h = handle_of_token t in
      List.fold_left (fun acc f -> f h acc) acc add_contexts,
      TH.add h cstoks,
      if add_contexts = [] then TH.add h unimpl else unimpl)
    (empty, TH.empty, TH.empty) specs
  in
  { context_tokens;
    context_sensitive_tokens;
    context_sensitive_tokens_unimplemented }

let tokens_of_context context_tokens : t -> TH.t = function
  | Accept_stmt -> context_tokens.accept_stmt
  | Allocate_stmt -> context_tokens.allocate_stmt
  | Alphabet_clause -> context_tokens.alphabet_clause
  | Arithmetic_clause -> context_tokens.arithmetic_clause
  | Class_specifier -> context_tokens.class_specifier
  | Column_clause -> context_tokens.column_clause
  | Constant -> context_tokens.constant
  | Currency_clause -> context_tokens.currency_clause
  | Default_clause -> context_tokens.default_clause
  | Dynlen_struct_clause -> context_tokens.dynlen_struct_clause
  | Entry_convention_clause -> context_tokens.entry_convention_clause
  | Erase_clause -> context_tokens.erase_clause
  | Exit_stmt -> context_tokens.exit_stmt
  | Factory_paragraph -> context_tokens.factory_paragraph
  | Float_binary_clause -> context_tokens.float_binary_clause
  | Float_decimal_clause -> context_tokens.float_decimal_clause
  | Function_specifier -> context_tokens.function_specifier
  | Interface_specifier -> context_tokens.interface_specifier
  | Intermediate_rounding_clause -> context_tokens.intermediate_rounding_clause
  | Line_clause -> context_tokens.line_clause
  | Lock_mode_clause -> context_tokens.lock_mode_clause
  | Object_computer_paragraph -> context_tokens.object_computer_paragraph
  | Object_paragraph -> context_tokens.object_paragraph
  | Occurs_clause -> context_tokens.occurs_clause
  | Options_paragrahp -> context_tokens.options_paragrahp
  | Options_paragraph -> context_tokens.options_paragraph
  | Options_pragraph -> context_tokens.options_pragraph
  | Perform_stmt -> context_tokens.perform_stmt
  | Program_id_paragraph -> context_tokens.program_id_paragraph
  | Read_statement -> context_tokens.read_statement
  | Read_stmt -> context_tokens.read_stmt
  | Resume_stmt -> context_tokens.resume_stmt
  | Retry_phrase -> context_tokens.retry_phrase
  | Rounded_phrase -> context_tokens.rounded_phrase
  | Screen_descr_entry -> context_tokens.screen_descr_entry
  | Screnn_descr_entry -> context_tokens.screnn_descr_entry
  | Set_attribute_stmt -> context_tokens.set_attribute_stmt
  | Set_stmt -> context_tokens.set_stmt
  | Sharing_clause -> context_tokens.sharing_clause
  | Sharing_phrase -> context_tokens.sharing_phrase
  | Stop_stmt -> context_tokens.stop_stmt
  | Typedef_clause -> context_tokens.typedef_clause
  | Usage_clause -> context_tokens.usage_clause
  | Validate_status_clause -> context_tokens.validate_status_clause

