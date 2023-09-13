(* Caution this file was automatically generated from grammar.cmly; do not edit *)
module TH = Text_lexer.TokenHandles
type context = TH.t

type t = context
type contexts =
  {
    accept_stmt: t;
    allocate_stmt: t;
    alphabet_clause: t;
    arithmetic_clause: t;
    class_specifier: t;
    column_clause: t;
    constant: t;
    currency_clause: t;
    default_clause: t;
    dynlen_struct_clause: t;
    entry_convention_clause: t;
    erase_clause: t;
    exit_stmt: t;
    factory_paragraph: t;
    float_binary_clause: t;
    float_decimal_clause: t;
    function_specifier: t;
    interface_specifier: t;
    intermediate_rounding_clause: t;
    line_clause: t;
    lock_mode_clause: t;
    object_computer_paragraph: t;
    object_paragraph: t;
    occurs_clause: t;
    options_paragrahp: t;
    options_paragraph: t;
    options_pragraph: t;
    program_id_paragraph: t;
    read_statement: t;
    read_stmt: t;
    resume_stmt: t;
    retry_phrase: t;
    rounded_phrase: t;
    screen_descr_entry: t;
    screnn_descr_entry: t;
    set_attribute_stmt: t;
    set_stmt: t;
    sharing_clause: t;
    sharing_phrase: t;
    stop_stmt: t;
    typedef_clause: t;
    usage_clause: t;
    validate_status_clause: t;
  }

let all, sensitive_tokens, sensitive_tokens_unimplemented =
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
    FOREVER, [retry_phrase];
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
    PAGE, [];
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
  List.fold_left (fun (acc, cstoks, unimpl) (t, add_contexts) ->
    let h = Text_lexer.handle_of_token t in
    List.fold_left (fun acc f -> f h acc) acc add_contexts,
    TH.add h cstoks,
    if add_contexts = [] then TH.add h unimpl else unimpl)
  (empty, TH.empty, TH.empty) specs

let accept_stmt = all.accept_stmt
let allocate_stmt = all.allocate_stmt
let alphabet_clause = all.alphabet_clause
let arithmetic_clause = all.arithmetic_clause
let class_specifier = all.class_specifier
let column_clause = all.column_clause
let constant = all.constant
let currency_clause = all.currency_clause
let default_clause = all.default_clause
let dynlen_struct_clause = all.dynlen_struct_clause
let entry_convention_clause = all.entry_convention_clause
let erase_clause = all.erase_clause
let exit_stmt = all.exit_stmt
let factory_paragraph = all.factory_paragraph
let float_binary_clause = all.float_binary_clause
let float_decimal_clause = all.float_decimal_clause
let function_specifier = all.function_specifier
let interface_specifier = all.interface_specifier
let intermediate_rounding_clause = all.intermediate_rounding_clause
let line_clause = all.line_clause
let lock_mode_clause = all.lock_mode_clause
let object_computer_paragraph = all.object_computer_paragraph
let object_paragraph = all.object_paragraph
let occurs_clause = all.occurs_clause
let options_paragrahp = all.options_paragrahp
let options_paragraph = all.options_paragraph
let options_pragraph = all.options_pragraph
let program_id_paragraph = all.program_id_paragraph
let read_statement = all.read_statement
let read_stmt = all.read_stmt
let resume_stmt = all.resume_stmt
let retry_phrase = all.retry_phrase
let rounded_phrase = all.rounded_phrase
let screen_descr_entry = all.screen_descr_entry
let screnn_descr_entry = all.screnn_descr_entry
let set_attribute_stmt = all.set_attribute_stmt
let set_stmt = all.set_stmt
let sharing_clause = all.sharing_clause
let sharing_phrase = all.sharing_phrase
let stop_stmt = all.stop_stmt
let typedef_clause = all.typedef_clause
let usage_clause = all.usage_clause
let validate_status_clause = all.validate_status_clause

