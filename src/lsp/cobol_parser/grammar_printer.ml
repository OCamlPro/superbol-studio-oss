open Grammar
open Grammar_tokens

let print_symbol = function
  | MenhirInterpreter.X (MenhirInterpreter.T T_error) -> "error"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ZERO_FILL) -> "ZERO_FILL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ZERO) -> "ZERO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_YYYYMMDD) -> "YYYYMMDD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_YYYYDDD) -> "YYYYDDD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_Y) -> "Y"
  | MenhirInterpreter.X (MenhirInterpreter.T T_XOR) -> "XOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_XML_TEXT) -> "XML_TEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_XML_SCHEMA) -> "XML_SCHEMA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_XML_NTEXT) -> "XML_NTEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_XML_EVENT) -> "XML_EVENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_XML_DECLARATION) -> "XML_DECLARATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_XML) -> "XML"
  | MenhirInterpreter.X (MenhirInterpreter.T T_X) -> "X"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WRITING) -> "WRITING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WRITE_VERIFY) -> "WRITE_VERIFY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WRITE_ONLY) -> "WRITE_ONLY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WRITERS) -> "WRITERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WRITE) -> "WRITE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WRAP) -> "WRAP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WORKING_STORAGE) -> "WORKING_STORAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WORD_IN_AREA_A) -> "WORD_IN_AREA_A"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WORDS) -> "WORDS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WORD) -> "WORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WITH_DATA) -> "WITH_DATA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WITH) -> "WITH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WINDOW) -> "WINDOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WIDTH_IN_CELLS) -> "WIDTH_IN_CELLS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WIDTH) -> "WIDTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WIDE) -> "WIDE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WHILE) -> "WHILE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WHEN_COMPILED) -> "WHEN_COMPILED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WHEN) -> "WHEN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WEB_BROWSER) -> "WEB_BROWSER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_WAIT) -> "WAIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VTOP) -> "VTOP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VSCROLL_POS) -> "VSCROLL_POS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VSCROLL_BAR) -> "VSCROLL_BAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VSCROLL) -> "VSCROLL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VPADDING) -> "VPADDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VOLATILE) -> "VOLATILE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VLR) -> "VLR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VISIBLE) -> "VISIBLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VIRTUAL_WIDTH) -> "VIRTUAL_WIDTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VIRTUAL) -> "VIRTUAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VIA) -> "VIA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VERY_HEAVY) -> "VERY_HEAVY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VERTICAL) -> "VERTICAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VERSION) -> "VERSION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VARYING) -> "VARYING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VARIANT) -> "VARIANT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VARIABLE) -> "VARIABLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VARBINARY) -> "VARBINARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VALUE_FORMAT) -> "VALUE_FORMAT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VALUES) -> "VALUES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VALUE) -> "VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VALIDATING) -> "VALIDATING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VALIDATE_STATUS) -> "VALIDATE_STATUS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VALIDATE) -> "VALIDATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_VALID) -> "VALID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_V) -> "V"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UTF_8) -> "UTF_8"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UTF_16) -> "UTF_16"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USING) -> "USING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USE_TAB) -> "USE_TAB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USE_RETURN) -> "USE_RETURN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USE_ALT) -> "USE_ALT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USER_WHITE) -> "USER_WHITE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USER_GRAY) -> "USER_GRAY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USER_DEFAULT) -> "USER_DEFAULT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USER_COLORS) -> "USER_COLORS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USER) -> "USER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USE) -> "USE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_USAGE) -> "USAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UPPER) -> "UPPER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UPON) -> "UPON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UPDATERS) -> "UPDATERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UPDATE) -> "UPDATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UP) -> "UP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNUSED__) -> "UNUSED__"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNTIL) -> "UNTIL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNSTRING) -> "UNSTRING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNSORTED) -> "UNSORTED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNSIGNED_SHORT) -> "UNSIGNED_SHORT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNSIGNED_LONG) -> "UNSIGNED_LONG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNSIGNED_INT) -> "UNSIGNED_INT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNSIGNED) -> "UNSIGNED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNSEQUAL) -> "UNSEQUAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNLOCK) -> "UNLOCK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNIVERSAL) -> "UNIVERSAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNIT) -> "UNIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNFRAMED) -> "UNFRAMED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNEQUAL) -> "UNEQUAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNDERLINE) -> "UNDERLINE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UNBOUNDED) -> "UNBOUNDED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UFF) -> "UFF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_UCS_4) -> "UCS_4"
  | MenhirInterpreter.X (MenhirInterpreter.T T_U) -> "U"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TYPEDEF) -> "TYPEDEF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TYPE) -> "TYPE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRUNCATION) -> "TRUNCATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRUE) -> "TRUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRIMMED) -> "TRIMMED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TREE_VIEW) -> "TREE_VIEW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRANSPARENT_COLOR) -> "TRANSPARENT_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRANSPARENT) -> "TRANSPARENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRANSFORM) -> "TRANSFORM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRANSACTION_STATUS) -> "TRANSACTION_STATUS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRANSACTION) -> "TRANSACTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRAILING_SIGN) -> "TRAILING_SIGN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRAILING_SHIFT) -> "TRAILING_SHIFT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRAILING) -> "TRAILING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRADITIONAL_FONT) -> "TRADITIONAL_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRACK_THUMB) -> "TRACK_THUMB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRACK_LIMIT) -> "TRACK_LIMIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRACK_AREA) -> "TRACK_AREA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRACKS) -> "TRACKS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRACK) -> "TRACK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TRACE) -> "TRACE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TOWARD_LESSER) -> "TOWARD_LESSER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TOWARD_GREATER) -> "TOWARD_GREATER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TOTALING) -> "TOTALING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TOTALED) -> "TOTALED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TOP_LEVEL) -> "TOP_LEVEL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TOP) -> "TOP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TOOL_BAR) -> "TOOL_BAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TO) -> "TO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TITLE_POSITION) -> "TITLE_POSITION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TITLE_BAR) -> "TITLE_BAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TITLE) -> "TITLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TIME_RECORD) -> "TIME_RECORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TIME_OUT) -> "TIME_OUT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TIME_OF_DAY) -> "TIME_OF_DAY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TIMESTAMP_RECORD) -> "TIMESTAMP_RECORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TIMESTAMP_OFFSET_RECORD) -> "TIMESTAMP_OFFSET_RECORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TIMESTAMP_OFFSET) -> "TIMESTAMP_OFFSET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TIMESTAMP) -> "TIMESTAMP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TIMES) -> "TIMES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TIME) -> "TIME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TILED_HEADINGS) -> "TILED_HEADINGS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THUMB_POSITION) -> "THUMB_POSITION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THROUGH) -> "THROUGH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THREEDIMENSIONAL) -> "THREEDIMENSIONAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THREAD_POINTER) -> "THREAD_POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THREAD_LOCAL_STORAGE) -> "THREAD_LOCAL_STORAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THREAD_LOCAL) -> "THREAD_LOCAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THREADS) -> "THREADS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THREAD) -> "THREAD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THEN) -> "THEN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_THAN) -> "THAN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TEXT) -> "TEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TEST) -> "TEST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TERMINATION_VALUE) -> "TERMINATION_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TERMINATE) -> "TERMINATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TERMINAL_X) -> "TERMINAL_X"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TERMINAL_INFO) -> "TERMINAL_INFO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TERMINAL_3) -> "TERMINAL_3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TERMINAL_2) -> "TERMINAL_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TERMINAL_1) -> "TERMINAL_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TERMINAL_0) -> "TERMINAL_0"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TERMINAL) -> "TERMINAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TEMPORARY) -> "TEMPORARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TEMP) -> "TEMP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TAPE) -> "TAPE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TALLYING) -> "TALLYING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TALLY) -> "TALLY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TAB_TO_DELETE) -> "TAB_TO_DELETE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TAB_TO_ADD) -> "TAB_TO_ADD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TAB_CONTROL) -> "TAB_CONTROL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TABLE) -> "TABLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_TAB) -> "TAB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSTEM_OFFSET) -> "SYSTEM_OFFSET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSTEM_INFO) -> "SYSTEM_INFO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSTEM_DEFAULT) -> "SYSTEM_DEFAULT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSTEM) -> "SYSTEM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSOUT_X) -> "SYSOUT_X"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSOUT_3) -> "SYSOUT_3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSOUT_2) -> "SYSOUT_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSOUT_1) -> "SYSOUT_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSOUT_0) -> "SYSOUT_0"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSIN_X) -> "SYSIN_X"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSIN_3) -> "SYSIN_3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSIN_2) -> "SYSIN_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSIN_1) -> "SYSIN_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYSIN_0) -> "SYSIN_0"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYNCHRONIZED) -> "SYNCHRONIZED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYMBOLIC) -> "SYMBOLIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SYMBOL) -> "SYMBOL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SWITCH) -> "SWITCH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUPPRESS) -> "SUPPRESS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUPER) -> "SUPER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUM) -> "SUM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUFFIXING) -> "SUFFIXING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUB_SCHEMA) -> "SUB_SCHEMA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUB_QUEUE_3) -> "SUB_QUEUE_3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUB_QUEUE_2) -> "SUB_QUEUE_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUB_QUEUE_1) -> "SUB_QUEUE_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUBWINDOW) -> "SUBWINDOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUBTRACT) -> "SUBTRACT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SUBFILE) -> "SUBFILE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STYLE) -> "STYLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STRUCTURE) -> "STRUCTURE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STRONG_NAME) -> "STRONG_NAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STRONG) -> "STRONG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STRING) -> "STRING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STOP_BROWSER) -> "STOP_BROWSER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STOP) -> "STOP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STEP) -> "STEP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STDCALL) -> "STDCALL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STATUS_TEXT) -> "STATUS_TEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STATUS_BAR) -> "STATUS_BAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STATUS) -> "STATUS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STATION) -> "STATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STATIC_LIST) -> "STATIC_LIST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STATIC) -> "STATIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STATEMENT) -> "STATEMENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_START_Y) -> "START_Y"
  | MenhirInterpreter.X (MenhirInterpreter.T T_START_X) -> "START_X"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STARTING) -> "STARTING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_START) -> "START"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STANDARD_DECIMAL) -> "STANDARD_DECIMAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STANDARD_BINARY) -> "STANDARD_BINARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STANDARD_2) -> "STANDARD_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STANDARD_1) -> "STANDARD_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STANDARD) -> "STANDARD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_STACK) -> "STACK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SSF) -> "SSF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SQUARE) -> "SQUARE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SQL_ROWID) -> "SQL_ROWID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SQL_NCLOB) -> "SQL_NCLOB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SQL_CURSOR) -> "SQL_CURSOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SQL_CLOB) -> "SQL_CLOB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SQL_BLOB) -> "SQL_BLOB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SQL_BFILE) -> "SQL_BFILE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SQLIMS) -> "SQLIMS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SQL) -> "SQL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SPINNER) -> "SPINNER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SPECIAL_NAMES) -> "SPECIAL_NAMES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SPACE_FILL) -> "SPACE_FILL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SPACE) -> "SPACE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SOURCE_COMPUTER) -> "SOURCE_COMPUTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SOURCES) -> "SOURCES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SOURCE) -> "SOURCE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT_WORK) -> "SORT_WORK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT_RETURN) -> "SORT_RETURN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT_ORDER) -> "SORT_ORDER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT_MODE_SIZE) -> "SORT_MODE_SIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT_MESSAGE) -> "SORT_MESSAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT_MERGE) -> "SORT_MERGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT_FILE_SIZE) -> "SORT_FILE_SIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT_CORE_SIZE) -> "SORT_CORE_SIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT_CONTROL) -> "SORT_CONTROL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SORT) -> "SORT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SMALL_FONT) -> "SMALL_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SLASH) -> "/"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SKIP3) -> "SKIP3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SKIP2) -> "SKIP2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SKIP1) -> "SKIP1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SIZE) -> "SIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SINTLIT) -> "SINTLIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SIGNED_SHORT) -> "SIGNED_SHORT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SIGNED_LONG) -> "SIGNED_LONG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SIGNED_INT) -> "SIGNED_INT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SIGNED) -> "SIGNED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SIGN) -> "SIGN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHOW_SEL_ALWAYS) -> "SHOW_SEL_ALWAYS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHOW_NONE) -> "SHOW_NONE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHOW_LINES) -> "SHOW_LINES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHORT_DATE) -> "SHORT_DATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHORT) -> "SHORT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHIFT_OUT) -> "SHIFT_OUT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHIFT_IN) -> "SHIFT_IN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHARING) -> "SHARING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHADOW) -> "SHADOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SHADING) -> "SHADING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SET) -> "SET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SERVICE) -> "SERVICE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEQUENTIAL) -> "SEQUENTIAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEQUENCE) -> "SEQUENCE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEPARATION) -> "SEPARATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEPARATE) -> "SEPARATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SENTENCE) -> "SENTENCE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEND) -> "SEND"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEMAPHORE_POINTER) -> "SEMAPHORE_POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SELF_ACT) -> "SELF_ACT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SELFCLASS) -> "SELFCLASS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SELF) -> "SELF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SELECT_ALL) -> "SELECT_ALL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SELECTIVE) -> "SELECTIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SELECTION_TEXT) -> "SELECTION_TEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SELECTION_INDEX) -> "SELECTION_INDEX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SELECTION) -> "SELECTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SELECT) -> "SELECT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEGMENT_LIMIT) -> "SEGMENT_LIMIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEGMENT) -> "SEGMENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEEK) -> "SEEK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SECURITY) -> "SECURITY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SECURE) -> "SECURE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SECTION) -> "SECTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SECONDS) -> "SECONDS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SECONDARY) -> "SECONDARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEARCH_TEXT) -> "SEARCH_TEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEARCH_OPTIONS) -> "SEARCH_OPTIONS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SEARCH) -> "SEARCH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SD) -> "SD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SCROLL_BAR) -> "SCROLL_BAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SCROLL) -> "SCROLL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SCREEN) -> "SCREEN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SAVE_AS_NO_PROMPT) -> "SAVE_AS_NO_PROMPT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SAVE_AS) -> "SAVE_AS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SARF) -> "SARF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_SAME) -> "SAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_S) -> "S"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RUN) -> "RUN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RPAR) -> ")"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROW_PROTECTION) -> "ROW_PROTECTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROW_HEADINGS) -> "ROW_HEADINGS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROW_FONT) -> "ROW_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROW_DIVIDERS) -> "ROW_DIVIDERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROW_COLOR_PATTERN) -> "ROW_COLOR_PATTERN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROW_COLOR) -> "ROW_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROWID) -> "ROWID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROUNDING) -> "ROUNDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROUNDED) -> "ROUNDED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROLLING) -> "ROLLING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ROLLBACK) -> "ROLLBACK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RIMMED) -> "RIMMED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RIGHT_JUSTIFY) -> "RIGHT_JUSTIFY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RIGHT_ALIGN) -> "RIGHT_ALIGN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RIGHT) -> "RIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RH) -> "RH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RF) -> "RF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REWRITE) -> "REWRITE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REWIND) -> "REWIND"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REVERSE_VIDEO) -> "REVERSE_VIDEO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REVERSED) -> "REVERSED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REVERSE) -> "REVERSE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RETURN_UNSIGNED) -> "RETURN_UNSIGNED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RETURN_CODE) -> "RETURN_CODE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RETURNING) -> "RETURNING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RETURN) -> "RETURN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RETRY) -> "RETRY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RETENTION) -> "RETENTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESUME) -> "RESUME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESTRICTED) -> "RESTRICTED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESIZABLE) -> "RESIZABLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESIDENT) -> "RESIDENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESET_TABS) -> "RESET_TABS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESET_SET_LOCATOR) -> "RESET_SET_LOCATOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESET_LIST) -> "RESET_LIST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESET_GRID) -> "RESET_GRID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESET) -> "RESET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RESERVE) -> "RESERVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RERUN) -> "RERUN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REREAD) -> "REREAD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REQUIRED) -> "REQUIRED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REPOSITORY) -> "REPOSITORY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REPORTS) -> "REPORTS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REPORTING) -> "REPORTING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REPORT) -> "REPORT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REPLACING) -> "REPLACING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REPLACED) -> "REPLACED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REPLACE) -> "REPLACE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REPEATED) -> "REPEATED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REORG_CRITERIA) -> "REORG_CRITERIA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RENAMES) -> "RENAMES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REMOVAL) -> "REMOVAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REMARKS) -> "REMARKS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REMAINDER) -> "REMAINDER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RELOAD) -> "RELOAD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RELEASE) -> "RELEASE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RELATIVE) -> "RELATIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RELATION) -> "RELATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REGION_COLOR) -> "REGION_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REFRESH) -> "REFRESH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REFERENCES) -> "REFERENCES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REFERENCE) -> "REFERENCE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REEL) -> "REEL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REDEFINITION) -> "REDEFINITION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_REDEFINES) -> "REDEFINES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECURSIVE) -> "RECURSIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECORD_TO_DELETE) -> "RECORD_TO_DELETE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECORD_TO_ADD) -> "RECORD_TO_ADD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECORD_POSITION) -> "RECORD_POSITION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECORD_OVERFLOW) -> "RECORD_OVERFLOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECORD_DATA) -> "RECORD_DATA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECORDS) -> "RECORDS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECORDING) -> "RECORDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECORD) -> "RECORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECEIVED) -> "RECEIVED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RECEIVE) -> "RECEIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_READ_ONLY) -> "READ_ONLY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_READY) -> "READY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_READING) -> "READING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_READERS) -> "READERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_READ) -> "READ"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RD) -> "RD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RANGE) -> "RANGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RANDOM) -> "RANDOM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RAISING) -> "RAISING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RAISED) -> "RAISED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RAISE) -> "RAISE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_RADIO_BUTTON) -> "RADIO_BUTTON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_QUOTE) -> "QUOTE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_QUEUED) -> "QUEUED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_QUEUE) -> "QUEUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_QUERY_INDEX) -> "QUERY_INDEX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PUSH_BUTTON) -> "PUSH_BUTTON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PURGE) -> "PURGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PUBLIC) -> "PUBLIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROTOTYPE) -> "PROTOTYPE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROTECTED) -> "PROTECTED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROPERTY) -> "PROPERTY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROPERTIES) -> "PROPERTIES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROMPT) -> "PROMPT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROHIBITED) -> "PROHIBITED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROGRESS) -> "PROGRESS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROGRAM_POINTER) -> "PROGRAM_POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROGRAM_ID) -> "PROGRAM_ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROGRAM) -> "PROGRAM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROCESS_AREA) -> "PROCESS_AREA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROCESSING) -> "PROCESSING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROCESS) -> "PROCESS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROCEED) -> "PROCEED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROCEDURE_POINTER) -> "PROCEDURE_POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROCEDURE_NAME) -> "PROCEDURE_NAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROCEDURES) -> "PROCEDURES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PROCEDURE) -> "PROCEDURE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRIVATE) -> "PRIVATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRIORITY) -> "PRIORITY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRIOR) -> "PRIOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRINT_PREVIEW) -> "PRINT_PREVIEW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRINT_NO_PROMPT) -> "PRINT_NO_PROMPT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRINT_CONTROL) -> "PRINT_CONTROL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRINTING) -> "PRINTING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRINTER_1) -> "PRINTER_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRINTER) -> "PRINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRINT) -> "PRINT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRIMARY) -> "PRIMARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PREVIOUS) -> "PREVIOUS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PRESENT) -> "PRESENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PREFIXING) -> "PREFIXING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PREFIXED) -> "PREFIXED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_POSITIVE) -> "POSITIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_POSITION_SHIFT) -> "POSITION_SHIFT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_POSITIONING) -> "POSITIONING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_POSITION) -> "POSITION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_POS) -> "POS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_POP_UP) -> "POP_UP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_POINTER_32) -> "POINTER_32"
  | MenhirInterpreter.X (MenhirInterpreter.T T_POINTER) -> "POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PLUS_SIGN) -> "+"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PLUS) -> "PLUS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PLACEMENT) -> "PLACEMENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PIXEL) -> "PIXEL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PICTURE_STRING) -> "PICTURE_STRING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PICTURE) -> "PICTURE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PHYSICAL) -> "PHYSICAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PH) -> "PH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PF) -> "PF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PERMANENT) -> "PERMANENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PERIOD) -> "."
  | MenhirInterpreter.X (MenhirInterpreter.T T_PERFORM) -> "PERFORM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PASSWORD) -> "PASSWORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PASCAL) -> "PASCAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PARSE) -> "PARSE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PARENT) -> "PARENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PARAGRAPH) -> "PARAGRAPH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PANEL_WIDTHS) -> "PANEL_WIDTHS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PANEL_TEXT) -> "PANEL_TEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PANEL_STYLE) -> "PANEL_STYLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PANEL_INDEX) -> "PANEL_INDEX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PAGE_SIZE) -> "PAGE_SIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PAGE_SETUP) -> "PAGE_SETUP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PAGE_COUNTER) -> "PAGE_COUNTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PAGED) -> "PAGED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PAGE) -> "PAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PADDING) -> "PADDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_PACKED_DECIMAL) -> "PACKED_DECIMAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_O_FILL) -> "O_FILL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OVERRIDING) -> "OVERRIDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OVERRIDE) -> "OVERRIDE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OVERLINE) -> "OVERLINE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OVERLAP_TOP) -> "OVERLAP_TOP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OVERLAP_LEFT) -> "OVERLAP_LEFT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OVERLAPPED) -> "OVERLAPPED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OVERFLOW) -> "OVERFLOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OUTPUT) -> "OUTPUT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OTHERWISE) -> "OTHERWISE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OTHERS) -> "OTHERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OTHER) -> "OTHER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ORGANIZATION) -> "ORGANIZATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ORDER) -> "ORDER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OR) -> "OR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OPTIONS) -> "OPTIONS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OPTIONAL) -> "OPTIONAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OPERATIONAL) -> "OPERATIONAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OPEN) -> "OPEN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OOSTACKPTR) -> "OOSTACKPTR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ON_SIZE_ERROR) -> "ON_SIZE_ERROR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ON_OVERFLOW) -> "ON_OVERFLOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ON_EXCEPTION) -> "ON_EXCEPTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ONLY) -> "ONLY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ON) -> "ON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OMITTED) -> "OMITTED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OK_BUTTON) -> "OK_BUTTON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OFF) -> "OFF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OF) -> "OF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OCCURS) -> "OCCURS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OBJECT_STORAGE) -> "OBJECT_STORAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OBJECT_REFERENCE) -> "OBJECT_REFERENCE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OBJECT_PROGRAM) -> "OBJECT_PROGRAM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OBJECT_ID) -> "OBJECT_ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OBJECT_COMPUTER) -> "OBJECT_COMPUTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_OBJECT) -> "OBJECT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NUM_ROW_HEADINGS) -> "NUM_ROW_HEADINGS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NUM_ROWS) -> "NUM_ROWS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NUM_COL_HEADINGS) -> "NUM_COL_HEADINGS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NUMERIC_FILL) -> "NUMERIC_FILL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NUMERIC_EDITED) -> "NUMERIC_EDITED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NUMERIC) -> "NUMERIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NUMBERS) -> "NUMBERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NUMBER) -> "NUMBER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NULLS) -> "NULLS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NULLIT) -> "NULLIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NULL) -> "NULL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_UPDOWN) -> "NO_UPDOWN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_TAB) -> "NO_TAB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_SEARCH) -> "NO_SEARCH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_KEY_LETTER) -> "NO_KEY_LETTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_GROUP_TAB) -> "NO_GROUP_TAB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_FOCUS) -> "NO_FOCUS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_F4) -> "NO_F4"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_ECHO) -> "NO_ECHO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_DIVIDERS) -> "NO_DIVIDERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_DATA) -> "NO_DATA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_CLOSE) -> "NO_CLOSE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_CELL_DRAG) -> "NO_CELL_DRAG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_BOX) -> "NO_BOX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_AUTO_DEFAULT) -> "NO_AUTO_DEFAULT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO_AUTOSEL) -> "NO_AUTOSEL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOT_ON_SIZE_ERROR) -> "NOT_ON_SIZE_ERROR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOT_ON_OVERFLOW) -> "NOT_ON_OVERFLOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOT_ON_EXCEPTION) -> "NOT_ON_EXCEPTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOT_INVALID_KEY) -> "NOT_INVALID_KEY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOT_AT_EOP) -> "NOT_AT_EOP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOT_AT_END) -> "NOT_AT_END"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOTIFY_SELCHANGE) -> "NOTIFY_SELCHANGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOTIFY_DBLCLICK) -> "NOTIFY_DBLCLICK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOTIFY_CHANGE) -> "NOTIFY_CHANGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOTIFY) -> "NOTIFY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOTHING) -> "NOTHING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOTE) -> "NOTE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOTAB) -> "NOTAB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOT) -> "NOT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NORMAL) -> "NORMAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NONNUMERIC) -> "NONNUMERIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NONE) -> "NONE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NOMINAL) -> "NOMINAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NO) -> "NO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NEXT_PAGE) -> "NEXT_PAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NEXT_ITEM) -> "NEXT_ITEM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NEXT) -> "NEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NEW) -> "NEW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NET_EVENT_LIST) -> "NET_EVENT_LIST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NESTED) -> "NESTED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NEGATIVE) -> "NEGATIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NEAREST_TO_ZERO) -> "NEAREST_TO_ZERO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NEAREST_TOWARD_ZERO) -> "NEAREST_TOWARD_ZERO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NEAREST_EVEN) -> "NEAREST_EVEN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NEAREST_AWAY_FROM_ZERO) -> "NEAREST_AWAY_FROM_ZERO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NE) -> "<>"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NCLOB) -> "NCLOB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NCHAR) -> "NCHAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NAVIGATE_URL) -> "NAVIGATE_URL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NATLIT) -> "NATLIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NATIVE) -> "NATIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NATIONAL_EDITED) -> "NATIONAL_EDITED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NATIONAL) -> "NATIONAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NAT) -> "NAT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NAMESPACE_PREFIX) -> "NAMESPACE_PREFIX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NAMESPACE) -> "NAMESPACE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NAMED) -> "NAMED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_NAME) -> "NAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MUTEX_POINTER) -> "MUTEX_POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MULTIPLY) -> "MULTIPLY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MULTIPLE) -> "MULTIPLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MULTILINE) -> "MULTILINE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MOVE) -> "MOVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MORE_LABELS) -> "MORE_LABELS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MONITOR_POINTER) -> "MONITOR_POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MODULES) -> "MODULES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MODULE) -> "MODULE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MODIFY) -> "MODIFY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MODIFIED) -> "MODIFIED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MODELESS) -> "MODELESS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MODE) -> "MODE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MODAL) -> "MODAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MIN_WIDTH) -> "MIN_WIDTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MIN_VALUE) -> "MIN_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MIN_VAL) -> "MIN_VAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MIN_SIZE) -> "MIN_SIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MIN_LINES) -> "MIN_LINES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MIN_HEIGHT) -> "MIN_HEIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MINUS) -> "MINUS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MICROSECOND_TIME) -> "MICROSECOND_TIME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_METHOD_ID) -> "METHOD_ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_METHOD) -> "METHOD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_META_CLASS) -> "META_CLASS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MESSAGE_TAG) -> "MESSAGE_TAG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MESSAGES) -> "MESSAGES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MESSAGE) -> "MESSAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MERGE) -> "MERGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MENU) -> "MENU"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MEMORY) -> "MEMORY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MEDIUM_FONT) -> "MEDIUM_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MDI_FRAME) -> "MDI_FRAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MDI_CHILD) -> "MDI_CHILD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MAX_WIDTH) -> "MAX_WIDTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MAX_VALUE) -> "MAX_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MAX_VAL) -> "MAX_VAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MAX_TEXT) -> "MAX_TEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MAX_SIZE) -> "MAX_SIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MAX_PROGRESS) -> "MAX_PROGRESS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MAX_LINES) -> "MAX_LINES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MAX_HEIGHT) -> "MAX_HEIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MASTER_INDEX) -> "MASTER_INDEX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MASS_UPDATE) -> "MASS_UPDATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MANUAL) -> "MANUAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_MAGNETIC_TAPE) -> "MAGNETIC_TAPE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LT) -> "<"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LPAR) -> "("
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOW_VALUE) -> "LOW_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOW_COLOR) -> "LOW_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOWLIGHT) -> "LOWLIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOWEST_VALUE) -> "LOWEST_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOWERED) -> "LOWERED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOWER) -> "LOWER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOW) -> "LOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LONG_VARCHAR) -> "LONG_VARCHAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LONG_VARBINARY) -> "LONG_VARBINARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LONG_DATE) -> "LONG_DATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOCK_HOLDING) -> "LOCK_HOLDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOCKS) -> "LOCKS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOCK) -> "LOCK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOCATION) -> "LOCATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOCAL_STORAGE) -> "LOCAL_STORAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOCALE) -> "LOCALE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LOC) -> "LOC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LM_RESIZE) -> "LM_RESIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LIST_BOX) -> "LIST_BOX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINKAGE) -> "LINKAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINK) -> "LINK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINE_SEQUENTIAL) -> "LINE_SEQUENTIAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINE_COUNTER) -> "LINE_COUNTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINES_PER_PAGE) -> "LINES_PER_PAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINES_AT_ROOT) -> "LINES_AT_ROOT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINES) -> "LINES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINE) -> "LINE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINAGE_COUNTER) -> "LINAGE_COUNTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LINAGE) -> "LINAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LIN) -> "LIN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LIMITS) -> "LIMITS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LIMIT) -> "LIMIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LIKE) -> "LIKE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LIBRARY) -> "LIBRARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LESS) -> "LESS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LENGTH) -> "LENGTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LEFT_TEXT) -> "LEFT_TEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LEFT_JUSTIFY) -> "LEFT_JUSTIFY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LEFTLINE) -> "LEFTLINE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LEFT) -> "LEFT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LEAVE) -> "LEAVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LEADING_SHIFT) -> "LEADING_SHIFT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LEADING) -> "LEADING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LE) -> "<="
  | MenhirInterpreter.X (MenhirInterpreter.T T_LC_TIME) -> "LC_TIME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LC_NUMERIC) -> "LC_NUMERIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LC_MONETARY) -> "LC_MONETARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LC_MESSAGES) -> "LC_MESSAGES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LC_CTYPE) -> "LC_CTYPE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LC_COLLATE) -> "LC_COLLATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LC_ALL) -> "LC_ALL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LAYOUT_MANAGER) -> "LAYOUT_MANAGER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LAYOUT_DATA) -> "LAYOUT_DATA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LAST_ROW) -> "LAST_ROW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LAST) -> "LAST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LARGE_OFFSET) -> "LARGE_OFFSET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LARGE_FONT) -> "LARGE_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LABEL_OFFSET) -> "LABEL_OFFSET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_LABEL) -> "LABEL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_KEY_LOCATION) -> "KEY_LOCATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_KEYED) -> "KEYED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_KEYBOARD) -> "KEYBOARD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_KEY) -> "KEY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_KEPT) -> "KEPT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_KANJI) -> "KANJI"
  | MenhirInterpreter.X (MenhirInterpreter.T T_JUSTIFIED) -> "JUSTIFIED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_JSON_STATUS) -> "JSON_STATUS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_JSON_CODE) -> "JSON_CODE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_JSON) -> "JSON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_JOINING) -> "JOINING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_JNIENVPTR) -> "JNIENVPTR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_JAVA) -> "JAVA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_JAPANESE) -> "JAPANESE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_I_O_CONTROL) -> "I_O_CONTROL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_I_O) -> "I_O"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ITEM_VALUE) -> "ITEM_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ITEM_TO_EMPTY) -> "ITEM_TO_EMPTY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ITEM_TO_DELETE) -> "ITEM_TO_DELETE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ITEM_TO_ADD) -> "ITEM_TO_ADD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ITEM_TEXT) -> "ITEM_TEXT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ITEM_ID) -> "ITEM_ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ITEM_BOLD) -> "ITEM_BOLD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ITEM) -> "ITEM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IS_TYPEDEF) -> "IS_TYPEDEF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IS_GLOBAL) -> "IS_GLOBAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IS_EXTERNAL) -> "IS_EXTERNAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IS) -> "IS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IN_ARITHMETIC_RANGE) -> "IN_ARITHMETIC_RANGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INVOKING) -> "INVOKING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INVOKED) -> "INVOKED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INVOKE) -> "INVOKE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INVALID_KEY) -> "INVALID_KEY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INVALID) -> "INVALID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INTRINSIC) -> "INTRINSIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INTO) -> "INTO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INTERVENING_) -> "INTERVENING_"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INTERVAL_TIMER) -> "INTERVAL_TIMER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INTERMEDIATE) -> "INTERMEDIATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INTERFACE_ID) -> "INTERFACE_ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INTERFACE) -> "INTERFACE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INSTANCE) -> "INSTANCE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INSTALLATION) -> "INSTALLATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INSPECT) -> "INSPECT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INSERT_ROWS) -> "INSERT_ROWS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INSERTION_INDEX) -> "INSERTION_INDEX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INSERT) -> "INSERT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INQUIRE) -> "INQUIRE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INPUT_OUTPUT) -> "INPUT_OUTPUT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INPUT) -> "INPUT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INITIATE) -> "INITIATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INITIAL_VALUE) -> "INITIAL_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INITIALIZED) -> "INITIALIZED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INITIALIZE) -> "INITIALIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INITIAL) -> "INITIAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INHERITS) -> "INHERITS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INHERITING) -> "INHERITING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INFO_WORD) -> "INFO_WORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INDICATORS) -> "INDICATORS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INDICATOR) -> "INDICATOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INDICATE) -> "INDICATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INDIC) -> "INDIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INDEX_2) -> "INDEX_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INDEX_1) -> "INDEX_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INDEXED) -> "INDEXED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INDEX) -> "INDEX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_INDEPENDENT) -> "INDEPENDENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IN) -> "IN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IMPLEMENTS) -> "IMPLEMENTS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IMP) -> "IMP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IGNORING) -> "IGNORING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IGNORE) -> "IGNORE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IF) -> "IF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IDS_II) -> "IDS_II"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IDENTIFIED) -> "IDENTIFIED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_IDENTIFICATION) -> "IDENTIFICATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ID) -> "ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ICON) -> "ICON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HSCROLL_POS) -> "HSCROLL_POS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HSCROLL) -> "HSCROLL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HOT_TRACK) -> "HOT_TRACK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HORIZONTAL) -> "HORIZONTAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HIGH_VALUE) -> "HIGH_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HIGH_ORDER_RIGHT) -> "HIGH_ORDER_RIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HIGH_ORDER_LEFT) -> "HIGH_ORDER_LEFT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HIGH_COLOR) -> "HIGH_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HIGHLIGHT) -> "HIGHLIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HIGHEST_VALUE) -> "HIGHEST_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HIGH) -> "HIGH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HIDDEN_DATA) -> "HIDDEN_DATA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HEXLIT) -> "HEXLIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HEX) -> "HEX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HELP_ID) -> "HELP_ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HEIGHT_IN_CELLS) -> "HEIGHT_IN_CELLS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HEIGHT) -> "HEIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HEAVY) -> "HEAVY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HEADING_FONT) -> "HEADING_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HEADING_DIVIDER_COLOR) -> "HEADING_DIVIDER_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HEADING_COLOR) -> "HEADING_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HEADING) -> "HEADING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HAS_CHILDREN) -> "HAS_CHILDREN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_HANDLE) -> "HANDLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GT) -> ">"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GROUP_VALUE) -> "GROUP_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GROUP_USAGE) -> "GROUP_USAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GROUP) -> "GROUP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GRIP) -> "GRIP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GRID) -> "GRID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GREATER) -> "GREATER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GRAPHICAL) -> "GRAPHICAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GO_SEARCH) -> "GO_SEARCH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GO_HOME) -> "GO_HOME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GO_FORWARD) -> "GO_FORWARD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GO_BACK) -> "GO_BACK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GOBACK) -> "GOBACK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GO) -> "GO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GLOBAL) -> "GLOBAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GIVING) -> "GIVING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GET) -> "GET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GENERATE) -> "GENERATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_GE) -> ">="
  | MenhirInterpreter.X (MenhirInterpreter.T T_GCOS) -> "GCOS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FUNCTION_POINTER) -> "FUNCTION_POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FUNCTION_ID) -> "FUNCTION_ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FUNCTION) -> "FUNCTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FULL_HEIGHT) -> "FULL_HEIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FULL) -> "FULL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FROM) -> "FROM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FREE) -> "FREE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FRAMED) -> "FRAMED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FRAME) -> "FRAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FORMAT) -> "FORMAT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FOREVER) -> "FOREVER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FOREGROUND_COLOR) -> "FOREGROUND_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FOR) -> "FOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FOOTING) -> "FOOTING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FONT) -> "FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLR) -> "FLR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_SHORT) -> "FLOAT_SHORT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_NOT_A_NUMBER_SIGNALING) -> "FLOAT_NOT_A_NUMBER_SIGNALING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_NOT_A_NUMBER_QUIET) -> "FLOAT_NOT_A_NUMBER_QUIET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_NOT_A_NUMBER) -> "FLOAT_NOT_A_NUMBER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_LONG) -> "FLOAT_LONG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_INFINITY) -> "FLOAT_INFINITY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_EXTENDED) -> "FLOAT_EXTENDED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_DECIMAL_34) -> "FLOAT_DECIMAL_34"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_DECIMAL_16) -> "FLOAT_DECIMAL_16"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_DECIMAL) -> "FLOAT_DECIMAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_BINARY_64) -> "FLOAT_BINARY_64"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_BINARY_32) -> "FLOAT_BINARY_32"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_BINARY_128) -> "FLOAT_BINARY_128"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT_BINARY) -> "FLOAT_BINARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOATLIT) -> "FLOATLIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOATING) -> "FLOATING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLOAT) -> "FLOAT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLAT_BUTTONS) -> "FLAT_BUTTONS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FLAT) -> "FLAT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FIXED_WIDTH) -> "FIXED_WIDTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FIXED_FONT) -> "FIXED_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FIXEDLIT) -> "FIXEDLIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FIXED) -> "FIXED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FIRST) -> "FIRST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FINISH_REASON) -> "FINISH_REASON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FINALLY) -> "FINALLY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FINAL) -> "FINAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILL_PERCENT) -> "FILL_PERCENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILL_COLOR2) -> "FILL_COLOR2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILL_COLOR) -> "FILL_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILLER) -> "FILLER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILE_PREFIX) -> "FILE_PREFIX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILE_POS) -> "FILE_POS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILE_PATH) -> "FILE_PATH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILE_NAME) -> "FILE_NAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILE_LIMITS) -> "FILE_LIMITS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILE_LIMIT) -> "FILE_LIMIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILE_ID) -> "FILE_ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILE_CONTROL) -> "FILE_CONTROL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILES) -> "FILES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FILE) -> "FILE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FIELD_TERMINATOR) -> "FIELD_TERMINATOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FH__KEYDEF) -> "FH__KEYDEF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FH__FCD) -> "FH__FCD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FD) -> "FD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FARTHEST_FROM_ZERO) -> "FARTHEST_FROM_ZERO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FALSE) -> "FALSE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_FACTORY) -> "FACTORY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_F) -> "F"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXTERNAL_FORM) -> "EXTERNAL_FORM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXTERNALLY_DESCRIBED_KEY) -> "EXTERNALLY_DESCRIBED_KEY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXTERNAL) -> "EXTERNAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXTERN) -> "EXTERN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXTENDED_SEARCH) -> "EXTENDED_SEARCH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXTEND) -> "EXTEND"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXPANDS) -> "EXPANDS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXPAND) -> "EXPAND"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXIT) -> "EXIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXHIBIT) -> "EXHIBIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXECUTE) -> "EXECUTE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXEC) -> "EXEC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXCLUSIVE_OR) -> "EXCLUSIVE_OR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXCLUSIVE) -> "EXCLUSIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXCLUDE_EVENT_LIST) -> "EXCLUDE_EVENT_LIST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXCESS_3) -> "EXCESS_3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXCEPTION_VALUE) -> "EXCEPTION_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXCEPTION_OBJECT) -> "EXCEPTION_OBJECT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXCEPTION) -> "EXCEPTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXCEEDS) -> "EXCEEDS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EXAMINE) -> "EXAMINE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EVERY) -> "EVERY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EVENT_POINTER) -> "EVENT_POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EVENT_LIST) -> "EVENT_LIST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EVENT) -> "EVENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EVALUATE) -> "EVALUATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ESI) -> "ESI"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ESCAPE_BUTTON) -> "ESCAPE_BUTTON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ESCAPE) -> "ESCAPE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ERROR) -> "ERROR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ERASE) -> "ERASE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EQUAL) -> "EQUAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EQ) -> "="
  | MenhirInterpreter.X (MenhirInterpreter.T T_EOS) -> "EOS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EOP) -> "EOP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EOL) -> "EOL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EOF) -> "EOF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EO) -> "EO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENVIRONMENT_VALUE) -> "ENVIRONMENT_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENVIRONMENT_NAME) -> "ENVIRONMENT_NAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENVIRONMENT) -> "ENVIRONMENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENTRY_REASON) -> "ENTRY_REASON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENTRY_FIELD) -> "ENTRY_FIELD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENTRY_CONVENTION) -> "ENTRY_CONVENTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENTRY) -> "ENTRY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENTER) -> "ENTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENSURE_VISIBLE) -> "ENSURE_VISIBLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENGRAVED) -> "ENGRAVED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_XML) -> "END_XML"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_WRITE) -> "END_WRITE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_WAIT) -> "END_WAIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_USE) -> "END_USE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_UNSTRING) -> "END_UNSTRING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_SUBTRACT) -> "END_SUBTRACT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_STRING) -> "END_STRING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_START) -> "END_START"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_SET) -> "END_SET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_SEND) -> "END_SEND"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_SEARCH) -> "END_SEARCH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_REWRITE) -> "END_REWRITE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_RETURN) -> "END_RETURN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_REPLACE) -> "END_REPLACE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_RECEIVE) -> "END_RECEIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_READ) -> "END_READ"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_PERFORM) -> "END_PERFORM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_ON) -> "END_ON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_OF_PAGE) -> "END_OF_PAGE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_MULTIPLY) -> "END_MULTIPLY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_MOVE) -> "END_MOVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_MODIFY) -> "END_MODIFY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_JSON) -> "END_JSON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_INVOKE) -> "END_INVOKE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_IF) -> "END_IF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_EXEC) -> "END_EXEC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_EVALUATE) -> "END_EVALUATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_ENABLE) -> "END_ENABLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_DIVIDE) -> "END_DIVIDE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_DISPLAY) -> "END_DISPLAY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_DISABLE) -> "END_DISABLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_DELETE) -> "END_DELETE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_COPY) -> "END_COPY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_COMPUTE) -> "END_COMPUTE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_COLOR) -> "END_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_CHAIN) -> "END_CHAIN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_CALL) -> "END_CALL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_ADD) -> "END_ADD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END_ACCEPT) -> "END_ACCEPT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENDING) -> "ENDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_END) -> "END"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENCRYPTION) -> "ENCRYPTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENCODING) -> "ENCODING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENABLED) -> "ENABLED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ENABLE) -> "ENABLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EMI) -> "EMI"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ELSE) -> "ELSE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ELEMENT) -> "ELEMENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EJECT) -> "EJECT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EIGHTY_EIGHT) -> "EIGHTY_EIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EGI) -> "EGI"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EGCS) -> "EGCS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EGC) -> "EGC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EDITING) -> "EDITING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ECHO) -> "ECHO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EC) -> "EC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_EBCDIC) -> "EBCDIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DYNAMIC) -> "DYNAMIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DUPLICATES) -> "DUPLICATES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DROP_LIST) -> "DROP_LIST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DROP_DOWN) -> "DROP_DOWN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DROP) -> "DROP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DRAW) -> "DRAW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DRAG_COLOR) -> "DRAG_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DOWN) -> "DOWN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DOUBLE_COLON) -> "::"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DOUBLE_ASTERISK) -> "**"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DOUBLE) -> "DOUBLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DOT_DASH) -> "DOT_DASH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DOTTED) -> "DOTTED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DOTDASH) -> "DOTDASH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DIVISION) -> "DIVISION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DIVIDER_COLOR) -> "DIVIDER_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DIVIDERS) -> "DIVIDERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DIVIDE) -> "DIVIDE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISPLAY_ST) -> "DISPLAY_ST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISPLAY_FORMAT) -> "DISPLAY_FORMAT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISPLAY_COLUMNS) -> "DISPLAY_COLUMNS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISPLAY_4) -> "DISPLAY_4"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISPLAY_3) -> "DISPLAY_3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISPLAY_2) -> "DISPLAY_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISPLAY_1) -> "DISPLAY_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISPLAY) -> "DISPLAY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISP) -> "DISP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISK) -> "DISK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISJOINING) -> "DISJOINING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISCONNECT) -> "DISCONNECT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISC) -> "DISC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DISABLE) -> "DISABLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DIGITS) -> "DIGITS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DETAIL) -> "DETAIL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DESTROY) -> "DESTROY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DESTINATION) -> "DESTINATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DESCRIPTOR) -> "DESCRIPTOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DESCENDING) -> "DESCENDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEPENDING) -> "DEPENDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DELIMITER) -> "DELIMITER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DELIMITED) -> "DELIMITED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DELETE) -> "DELETE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEFINITION) -> "DEFINITION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEFAULT_FONT) -> "DEFAULT_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEFAULT_BUTTON) -> "DEFAULT_BUTTON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEFAULT) -> "DEFAULT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DECLARE) -> "DECLARE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DECLARATIVES) -> "DECLARATIVES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DECIMAL_POINT) -> "DECIMAL_POINT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DECIMAL_ENCODING) -> "DECIMAL_ENCODING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEBUG_SUB_3) -> "DEBUG_SUB_3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEBUG_SUB_2) -> "DEBUG_SUB_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEBUG_SUB_1) -> "DEBUG_SUB_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEBUG_NAME) -> "DEBUG_NAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEBUG_LINE) -> "DEBUG_LINE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEBUG_ITEM) -> "DEBUG_ITEM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEBUG_CONTENTS) -> "DEBUG_CONTENTS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEBUGGING) -> "DEBUGGING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DEBUG) -> "DEBUG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DBCS) -> "DBCS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DBCLOB_LOCATOR) -> "DBCLOB_LOCATOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DBCLOB_FILE) -> "DBCLOB_FILE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DBCLOB) -> "DBCLOB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DAY_OF_WEEK) -> "DAY_OF_WEEK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DAY_AND_TIME) -> "DAY_AND_TIME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DAY) -> "DAY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATE_WRITTEN) -> "DATE_WRITTEN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATE_RECORD) -> "DATE_RECORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATE_MODIFIED) -> "DATE_MODIFIED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATE_ENTRY) -> "DATE_ENTRY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATE_COMPILED) -> "DATE_COMPILED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATE_AND_TIME) -> "DATE_AND_TIME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATE) -> "DATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATA_TYPES) -> "DATA_TYPES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATA_RECORDS) -> "DATA_RECORDS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATA_RECORD) -> "DATA_RECORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATA_POINTER) -> "DATA_POINTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATA_COLUMNS) -> "DATA_COLUMNS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DATA) -> "DATA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DASH_SIGN) -> "-"
  | MenhirInterpreter.X (MenhirInterpreter.T T_DASHED) -> "DASHED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CYL_OVERFLOW) -> "CYL_OVERFLOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CYL_INDEX) -> "CYL_INDEX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CYCLE) -> "CYCLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CUSTOM_PRINT_TEMPLATE) -> "CUSTOM_PRINT_TEMPLATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURSOR_Y) -> "CURSOR_Y"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURSOR_X) -> "CURSOR_X"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURSOR_ROW) -> "CURSOR_ROW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURSOR_FRAME_WIDTH) -> "CURSOR_FRAME_WIDTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURSOR_COLOR) -> "CURSOR_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURSOR_COL) -> "CURSOR_COL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURSOR) -> "CURSOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURRENT_DATE) -> "CURRENT_DATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURRENT) -> "CURRENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CURRENCY) -> "CURRENCY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CULTURE) -> "CULTURE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CS_GENERAL) -> "CS_GENERAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CS_BASIC) -> "CS_BASIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CSP) -> "CSP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CSIZE) -> "CSIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CRT_UNDER) -> "CRT_UNDER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CRT) -> "CRT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CREATE) -> "CREATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COUNT_TRAILING) -> "COUNT_TRAILING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COUNT_MIN) -> "COUNT_MIN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COUNT_MAX) -> "COUNT_MAX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COUNT_LEADLING) -> "COUNT_LEADLING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COUNT) -> "COUNT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CORRESPONDING) -> "CORRESPONDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CORE_INDEX) -> "CORE_INDEX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COPY_SELECTION) -> "COPY_SELECTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COPY) -> "COPY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONVERTING) -> "CONVERTING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONVERT) -> "CONVERT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONVERSION) -> "CONVERSION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONTROL_AREA) -> "CONTROL_AREA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONTROLS_UNCROPPED) -> "CONTROLS_UNCROPPED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONTROLS) -> "CONTROLS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONTROL) -> "CONTROL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONTINUE) -> "CONTINUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONTENT_OF) -> "CONTENT_OF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONTENT) -> "CONTENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONTAINS) -> "CONTAINS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONSTRUCTOR) -> "CONSTRUCTOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONSTANT_RECORD) -> "CONSTANT_RECORD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONSTANT) -> "CONSTANT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONSOLE_3) -> "CONSOLE_3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONSOLE_2) -> "CONSOLE_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONSOLE_1) -> "CONSOLE_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONSOLE_0) -> "CONSOLE_0"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONNECT) -> "CONNECT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONFIGURATION) -> "CONFIGURATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CONDITION) -> "CONDITION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COM_REG) -> "COM_REG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_X) -> "COMP_X"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_N) -> "COMP_N"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_9) -> "COMP_9"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_7) -> "COMP_7"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_6) -> "COMP_6"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_5) -> "COMP_5"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_4) -> "COMP_4"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_3) -> "COMP_3"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_2) -> "COMP_2"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_15) -> "COMP_15"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_14) -> "COMP_14"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_13) -> "COMP_13"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_12) -> "COMP_12"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_11) -> "COMP_11"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_10) -> "COMP_10"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_1) -> "COMP_1"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP_0) -> "COMP_0"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMPUTE) -> "COMPUTE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMPUTATIONAL_7) -> "COMPUTATIONAL_7"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMPUTATIONAL_14) -> "COMPUTATIONAL_14"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMPUTATIONAL_13) -> "COMPUTATIONAL_13"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMPUTATIONAL_12) -> "COMPUTATIONAL_12"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMPUTATIONAL_11) -> "COMPUTATIONAL_11"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMPRESSION) -> "COMPRESSION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMPLEMENTARY) -> "COMPLEMENTARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMPLE) -> "COMPLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMP) -> "COMP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMMUNICATION) -> "COMMUNICATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMMON) -> "COMMON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMMITMENT) -> "COMMITMENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMMIT) -> "COMMIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMMENT_ENTRY) -> "COMMENT_ENTRY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMMAND_LINE) -> "COMMAND_LINE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMMA) -> "COMMA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COMBO_BOX) -> "COMBO_BOX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLUMN_PROTECTION) -> "COLUMN_PROTECTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLUMN_HEADINGS) -> "COLUMN_HEADINGS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLUMN_FONT) -> "COLUMN_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLUMN_DIVIDERS) -> "COLUMN_DIVIDERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLUMN_COLOR) -> "COLUMN_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLUMNS) -> "COLUMNS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLUMN) -> "COLUMN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLORS) -> "COLORS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLOR) -> "COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLON) -> ":"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COLLATING) -> "COLLATING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COL) -> "COL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COERCION) -> "COERCION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CODE_SET) -> "CODE_SET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CODE) -> "CODE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_COBOL) -> "COBOL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLOSE) -> "CLOSE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLOCK_UNITS) -> "CLOCK_UNITS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLOB_LOCATOR) -> "CLOB_LOCATOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLOB_FILE) -> "CLOB_FILE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLOB) -> "CLOB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLINES) -> "CLINES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLINE) -> "CLINE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLEAR_SELECTION) -> "CLEAR_SELECTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLASS_OBJECT) -> "CLASS_OBJECT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLASS_NAME) -> "CLASS_NAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLASS_ID) -> "CLASS_ID"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLASS_CONTROL) -> "CLASS_CONTROL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLASSIFICATION) -> "CLASSIFICATION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CLASS) -> "CLASS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CICS) -> "CICS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHECK_BOX) -> "CHECK_BOX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHECKPOINT_FILE) -> "CHECKPOINT_FILE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHECK) -> "CHECK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHAR_VARYING) -> "CHAR_VARYING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHART) -> "CHART"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHARACTERS) -> "CHARACTERS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHARACTER) -> "CHARACTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHAR) -> "CHAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHANGED) -> "CHANGED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHAINING) -> "CHAINING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CHAIN) -> "CHAIN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CH) -> "CH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CF) -> "CF"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CENTURY_DAY) -> "CENTURY_DAY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CENTURY_DATE) -> "CENTURY_DATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CENTERED_HEADINGS) -> "CENTERED_HEADINGS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CENTERED) -> "CENTERED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CENTER) -> "CENTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CELL_PROTECTION) -> "CELL_PROTECTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CELL_FONT) -> "CELL_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CELL_DATA) -> "CELL_DATA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CELL_COLOR) -> "CELL_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CELL) -> "CELL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CD) -> "CD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CCOL) -> "CCOL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CBL) -> "CBL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CATALOGUE_NAME) -> "CATALOGUE_NAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CATALOGUED) -> "CATALOGUED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CASSETTE) -> "CASSETTE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CASE_SENSITIVE) -> "CASE_SENSITIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CASE_INSENSITIVE) -> "CASE_INSENSITIVE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CARD_READER) -> "CARD_READER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CARD_PUNCH) -> "CARD_PUNCH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CAPACITY) -> "CAPACITY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CANCEL_BUTTON) -> "CANCEL_BUTTON"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CANCEL) -> "CANCEL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CALLED) -> "CALLED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CALL) -> "CALL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_CALENDAR_FONT) -> "CALENDAR_FONT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_C) -> "C"
  | MenhirInterpreter.X (MenhirInterpreter.T T_B_XOR) -> "B_XOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_B_SHIFT_RC) -> "B_SHIFT_RC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_B_SHIFT_R) -> "B_SHIFT_R"
  | MenhirInterpreter.X (MenhirInterpreter.T T_B_SHIFT_LC) -> "B_SHIFT_LC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_B_SHIFT_L) -> "B_SHIFT_L"
  | MenhirInterpreter.X (MenhirInterpreter.T T_B_OR) -> "B_OR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_B_NOT) -> "B_NOT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_B_EXOR) -> "B_EXOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_B_AND) -> "B_AND"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BYTE_LENGTH) -> "BYTE_LENGTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BYTES) -> "BYTES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BYTE) -> "BYTE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BY) -> "BY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BUTTONS) -> "BUTTONS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BUSY) -> "BUSY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BULK_ADDITION) -> "BULK_ADDITION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BSN) -> "BSN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BROWSING) -> "BROWSING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BOXED) -> "BOXED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BOX) -> "BOX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BOTTOM) -> "BOTTOM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BOOLIT) -> "BOOLIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BOOLEAN) -> "BOOLEAN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BLOCK) -> "BLOCK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BLOB_LOCATOR) -> "BLOB_LOCATOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BLOB_FILE) -> "BLOB_FILE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BLOB) -> "BLOB"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BLINK) -> "BLINK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BLANK) -> "BLANK"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITS) -> "BITS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_WIDTH) -> "BITMAP_WIDTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_TRANSPARENT_COLOR) -> "BITMAP_TRANSPARENT_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_TRAILING) -> "BITMAP_TRAILING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_TIMER) -> "BITMAP_TIMER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_START) -> "BITMAP_START"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_SCALE) -> "BITMAP_SCALE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_RAW_WIDTH) -> "BITMAP_RAW_WIDTH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_RAW_HEIGHT) -> "BITMAP_RAW_HEIGHT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_NUMBER) -> "BITMAP_NUMBER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_HANDLE) -> "BITMAP_HANDLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP_END) -> "BITMAP_END"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BITMAP) -> "BITMAP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BIT) -> "BIT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BIND) -> "BIND"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BINARY_SHORT) -> "BINARY_SHORT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BINARY_SEQUENTIAL) -> "BINARY_SEQUENTIAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BINARY_LONG) -> "BINARY_LONG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BINARY_ENCODING) -> "BINARY_ENCODING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BINARY_DOUBLE) -> "BINARY_DOUBLE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BINARY_C_LONG) -> "BINARY_C_LONG"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BINARY_CHAR) -> "BINARY_CHAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BINARY) -> "BINARY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BELL) -> "BELL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BEGINNING) -> "BEGINNING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BEFORE) -> "BEFORE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BECOMES) -> "BECOMES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BASIS) -> "BASIS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BASED) -> "BASED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BAR) -> "BAR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BACKWARD) -> "BACKWARD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BACKGROUND_STANDARD) -> "BACKGROUND_STANDARD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BACKGROUND_LOW) -> "BACKGROUND_LOW"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BACKGROUND_HIGH) -> "BACKGROUND_HIGH"
  | MenhirInterpreter.X (MenhirInterpreter.T T_BACKGROUND_COLOR) -> "BACKGROUND_COLOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AX_EVENT_LIST) -> "AX_EVENT_LIST"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AWAY_FROM_ZERO) -> "AWAY_FROM_ZERO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AUTO_SPIN) -> "AUTO_SPIN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AUTO_RESIZE) -> "AUTO_RESIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AUTO_MINIMIZE) -> "AUTO_MINIMIZE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AUTO_HYPHEN_SKIP) -> "AUTO_HYPHEN_SKIP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AUTO_DECIMAL) -> "AUTO_DECIMAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AUTOMATIC) -> "AUTOMATIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AUTO) -> "AUTO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AUTHOR) -> "AUTHOR"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AT_EOP) -> "AT_EOP"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AT_END) -> "AT_END"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ATTRIBUTES) -> "ATTRIBUTES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ATTRIBUTE) -> "ATTRIBUTE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AT) -> "AT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ASTERISK) -> "*"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ASSIGN) -> "ASSIGN"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ASSEMBLY_NAME) -> "ASSEMBLY_NAME"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ASCII) -> "ASCII"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ASCENDING) -> "ASCENDING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ASA) -> "ASA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AS) -> "AS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ARITHMETIC) -> "ARITHMETIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ARGUMENT_VALUE) -> "ARGUMENT_VALUE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ARGUMENT_NUMBER) -> "ARGUMENT_NUMBER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AREA_VALUES) -> "AREA_VALUES"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AREAS) -> "AREAS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AREA) -> "AREA"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ARE) -> "ARE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_APPLY) -> "APPLY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ANYCASE) -> "ANYCASE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ANY) -> "ANY"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ANUM) -> "ANUM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ANSI) -> "ANSI"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AND) -> "AND"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AMPERSAND) -> "&"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALTERNATE) -> "ALTERNATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALTERING) -> "ALTERING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALTER) -> "ALTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALSO) -> "ALSO"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALPHANUM_PREFIX) -> "ALPHANUM_PREFIX"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALPHANUMERIC_EDITED) -> "ALPHANUMERIC_EDITED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALPHANUMERIC) -> "ALPHANUMERIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALPHANUM) -> "ALPHANUM"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALPHABETIC_UPPER) -> "ALPHABETIC_UPPER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALPHABETIC_LOWER) -> "ALPHABETIC_LOWER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALPHABETIC) -> "ALPHABETIC"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALPHABET) -> "ALPHABET"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALLOWING) -> "ALLOWING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALLOCATE) -> "ALLOCATE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALL) -> "ALL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALIGNMENT) -> "ALIGNMENT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALIGNED) -> "ALIGNED"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ALIAS) -> "ALIAS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_AFTER) -> "AFTER"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ADVANCING) -> "ADVANCING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ADJUSTABLE_COLUMNS) -> "ADJUSTABLE_COLUMNS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ADDRESS) -> "ADDRESS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ADD) -> "ADD"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ACTUAL) -> "ACTUAL"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ACTIVE_X) -> "ACTIVE_X"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ACTIVE_CLASS) -> "ACTIVE_CLASS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ACTIVATING) -> "ACTIVATING"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ACTION) -> "ACTION"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ACQUIRE) -> "ACQUIRE"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ACCESS) -> "ACCESS"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ACCEPT) -> "ACCEPT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ABSTRACT) -> "ABSTRACT"
  | MenhirInterpreter.X (MenhirInterpreter.T T_ABSENT) -> "ABSENT"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_write_target) -> "write_target"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_write_statement) -> "write_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_working_storage_section) -> "working_storage_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_word_or_terminal) -> "<word or TERMINAL>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_test) -> "with_test"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_status) -> "with_status"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_no_advancing) -> "with_no_advancing"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_lock_clause) -> "with_lock_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_lock) -> "with_lock"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_key) -> "with_key"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_data) -> "with_data"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_when_selection_objects) -> "when_selection_objects"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_when_phrase) -> "when_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_when_other) -> "when_other"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_when_clause) -> "when_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_varying_phrase) -> "varying_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_varying_clause) -> "varying_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_value_of_clause) -> "value_of_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_validation_stage) -> "validation_stage"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_validation_clause) -> "validation_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_validate_status_clause) -> "validate_status_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_validate_statement) -> "validate_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_using_clause) -> "using_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_using_by) -> "using_by"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_use_statement) -> "use_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_use_after_exception) -> "use_after_exception"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_usage_clause) -> "usage_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_usage) -> "usage"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_upon) -> "upon"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_up_down) -> "up_down"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_unstring_target) -> "unstring_target"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_unstring_statement) -> "unstring_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_unstring_delimiters) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_unlock_statement) -> "unlock_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_unconditional_action) -> "unconditional_action"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_typedef_clause) -> "typedef_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_transform_statement) -> "transform_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_then_replacing) -> "then_replacing"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_terminate_statement) -> "terminate_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_tallying_for) -> "tallying_for"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_tallying) -> "tallying"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_synchronized_clause) -> "synchronized_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_symbolic_characters_clause) -> "symbolic_characters_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_suppress_statement) -> "suppress_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sum_phrase) -> "sum_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sum_operands) -> "sum_operands"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sum_clause) -> "sum_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_subtract_statement) -> "subtract_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_subscripts) -> "<subscripts>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_subscript_following) -> "<subscript>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_subscript_first) -> "<subscript>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_structure_kind) -> "structure_kind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_string_statement) -> "string_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_string_or_int_literal) -> "string_or_int_literal"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_string_literal_no_all) -> "<string literal>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_string_literal) -> "<string literal>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_stop_statement) -> "stop_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_stop_kind) -> "stop_kind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_step_phrase) -> "step_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_status_switch) -> "status_switch"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_start_statement) -> "start_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_standalone_condition) -> "standalone_condition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_specifier) -> "specifier"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_special_names_paragraph) -> "special_names_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_special_names_clause) -> "special_names_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_source_string) -> "source_string"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_source_operands) -> "source_operands"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_source_destination_clauses) -> "source_destination_clauses"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_source_destination_clause) -> "source_destination_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_source_computer_paragraph) -> "source_computer_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_source_clause) -> "source_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sort_statement) -> "sort_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sort_merge_file_descr_clause) -> "sort_merge_file_descr_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signedness_) -> "signedness_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sign_condition_no_zero) -> "sign_condition_no_zero"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sign_condition) -> "sign_condition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sign_clause) -> "sign_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sign) -> "sign"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sharing_phrase) -> "sharing_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sharing_mode) -> "sharing_mode"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sharing_clause) -> "sharing_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_set_statement) -> "set_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_set_attribute_switches) -> "set_attribute_switches"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sentence) -> "sentence"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_send_statement) -> "send_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_selection_subjects) -> "selection_subjects"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_selection_subject) -> "selection_subject"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_selection_objects) -> "selection_objects"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_selection_object) -> "selection_object"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_select_when_clause) -> "select_when_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_select_clause) -> "select_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_select) -> "select"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_segment_limit_clause) -> "segment_limit_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_section_paragraphs) -> "section_paragraphs"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_section_paragraph) -> "section_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_search_statement) -> "search_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_search_condition) -> "search_condition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_section) -> "screen_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_occurs_clause) -> "screen_occurs_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_line_column_clause) -> "screen_line_column_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_line_clause) -> "screen_line_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_descr_entry) -> "screen_descr_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_descr_clause) -> "screen_descr_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_column_clause) -> "screen_column_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_attribute_on_off) -> "screen_attribute_on_off"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_attribute_name) -> "screen_attribute_name"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_attribute_clauses) -> "screen_attribute_clauses"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_screen_attribute_clause) -> "screen_attribute_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_same_as_clause) -> "same_as_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_same_area_clause) -> "same_area_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_s_delimited_by) -> "s_delimited_by"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rounding_mode) -> "rounding_mode"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rounded_phrase_opt) -> "rounded_phrase_opt"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rounded_phrase) -> "rounded_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rounded_ident) -> "rounded_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rounded_clause) -> "rounded_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_working_storage_section_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_with_test_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_with_status_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_step_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_signedness_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_sign_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_sharing_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_screen_section_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_s_delimited_by_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_returning_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_retry_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_report_section_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_read_direction_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_raising_exception_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_procedure_division_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_picture_locale_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_option_TO__name__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_86_qualname__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_44_property_kind__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_43_integer__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_38_integer__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_37_integer__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_34_integer__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_33_integer__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_32_qualname_or_integer__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_30_qualname_or_integer__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_14_string_literal__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_101_ident__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_100_ident__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_VARYING_ident__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_USING_name__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_TO_loc_integer___) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_THROUGH_string_or_int_literal__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_THROUGH_qualified_procedure_name__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_THROUGH_procedure_name__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_REMAINDER_ident__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_POSITION_integer__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_ON_name__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_INTO_loc_ident___) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_INTO_ident__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_IN_name__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_FROM_loc_integer___) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_FROM_ident_or_literal__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_FROM_expression__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_BY_ident_or_numeric__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_BY_expression__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_pf_AS_string_literal__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_perform_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_object_reference_kind_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_object_procedure_division_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_name_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_lock_or_retry_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_locale_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_local_storage_section_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_upon__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_special_names_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_source_computer_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_repository_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_program_procedure_division__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_procedure_division__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_options_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_object_computer_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_io_control_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_input_output_section__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_file_control_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_environment_division__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_data_division__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_loc_configuration_section__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_linkage_section_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_integer_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_instance_definition_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_file_section_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_expression_no_all_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_expands_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_entry_name_clause_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_endianness_mode_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_depending_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_communication_section_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_collating_sequence_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_close_format_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_capacity_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ro_advancing_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnell_rev_tallying_) -> "rnell_rev_tallying_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnell_rev___anonymous_91_) -> "rnell_rev___anonymous_91_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnell_rev___anonymous_90_) -> "rnell_rev___anonymous_90_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnell_rev___anonymous_89_) -> "rnell_rev___anonymous_89_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnell_rev___anonymous_88_) -> "rnell_rev___anonymous_88_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_when_selection_objects_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_validation_stage_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_use_after_exception_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_unstring_target_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_subscript_following_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_specifier_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_screen_attribute_on_off_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_rounded_ident_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_qualname_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_qualified_procedure_name_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_pf_ALSO_string_or_int_literal__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_open_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_on_key_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_name_or_alphanum_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_name_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_using_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_using_by__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_tallying_for__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_special_names_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_sentence__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_select_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_section_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_replacing_phrase__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_options_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_literal__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_decl_section_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_literal_through_literal_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_literal_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_line_position_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_integer_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_or_string_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_or_numeric_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_or_literal_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_by_after_before_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_file_with_opt_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_debug_target_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_column_position_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rnel_argument_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_select_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_pf_FILE_name__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_name_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_result_imperative_statement__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_sort_merge_file_descr_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_sentence__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_section_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_screen_descr_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_same_area_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_rerun_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_report_group_descr_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_report_descr_entry__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_report_descr_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_object_computer_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_multiple_file_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_method_definition__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_informational_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_file_or_sort_merge_descr_entry__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_file_descr_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_data_descr_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_constant_or_screen_descr_entry__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_constant_or_report_group_descr_entry__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_constant_or_data_descr_entry__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_communication_descr_entry__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_loc_communication_descr_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_key_is_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_inspect_where_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rl_entry_name_clause_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rewrite_statement) -> "rewrite_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reversed_or_no_rewind_opt) -> "reversed_or_no_rewind_opt"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_returning) -> "returning"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_return_statement) -> "return_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_retry_phrase) -> "retry_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_resume_statement) -> "resume_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_reserve_clause) -> "reserve_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rerun_frequency) -> "rerun_frequency"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rerun_clause) -> "rerun_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_repository_paragraph) -> "repository_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_value_clause) -> "report_value_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_type_clause) -> "report_type_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_section) -> "report_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_screen_usage_clause) -> "report_screen_usage_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_occurs_clause) -> "report_occurs_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_line_clause) -> "report_line_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_group_descr_entry) -> "report_group_descr_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_group_descr_clause) -> "report_group_descr_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_descr_entry) -> "report_descr_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_descr_clause) -> "report_descr_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_data_name_or_final) -> "report_data_name_or_final"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_column_clause) -> "report_column_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_report_clause) -> "report_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_replacing_phrase) -> "replacing_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_relop) -> "<relational arithmetic operator>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_release_statement) -> "release_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_relative_key_clause) -> "relative_key_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_relation_condition) -> "relation_condition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_redefines_clause) -> "redefines_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_record_key_clause) -> "record_key_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_record_delimiter_clause) -> "record_delimiter_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_record_delimiter) -> "record_delimiter"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_record_clause) -> "record_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_receive_statement) -> "receive_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_read_statement) -> "read_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_read_direction) -> "read_direction"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_range_expression) -> "range_expression"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_raising_phrase) -> "raising_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_raising_exception) -> "raising_exception"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_raise_statement) -> "raise_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_qualnames) -> "qualnames"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_qualname_or_literal) -> "qualname_or_literal"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_qualname_or_integer) -> "qualname_or_integer"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_qualname_or_alphanum) -> "qualname_or_alphanum"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_qualname) -> "<qualified name>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_qualified_procedure_name) -> "qualified_procedure_name"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_purge_statement) -> "purge_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_property_clause) -> "property_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_program_prototype_id_paragraph) -> "program_prototype_id_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_program_prototype) -> "program_prototype"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_program_procedure_division) -> "program_procedure_division"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_program_kind) -> "program_kind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_program_definition_no_end) -> "program_definition_no_end"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_program_definition_identification) -> "program_definition_identification"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_program_definition_id_paragraph) -> "program_definition_id_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_program_definition) -> "program_definition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_program_collating_sequence_clause) -> "program_collating_sequence_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_procedure_name_decl) -> "procedure_name_decl"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_procedure_name) -> "procedure_name"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_procedure_division) -> "procedure_division"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_present_when_clause) -> "present_when_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_position) -> "position"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_plus_or_minus) -> "plus_or_minus"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_picture_locale_phrase) -> "<locale phrase>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_picture_clause) -> "<picture clause>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_perform_statement) -> "perform_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_perform_phrase) -> "perform_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_partial_expression) -> "partial_expression"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_page_line_col) -> "page_line_col"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_page_limit_clause) -> "page_limit_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_padding_character_clause) -> "padding_character_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_output_or_giving) -> "output_or_giving"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_organization_clause) -> "organization_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_organization) -> "organization"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_order_table_clause) -> "order_table_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_options_paragraph) -> "options_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_options_clause) -> "options_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_optional_arguments_list) -> "optional_arguments_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_working_storage_section_) -> "option_working_storage_section_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_with_test_) -> "option_with_test_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_with_status_) -> "option_with_status_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_step_phrase_) -> "option_step_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_signedness_) -> "option_signedness_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_sign_) -> "option_sign_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_sharing_phrase_) -> "option_sharing_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_screen_section_) -> "option_screen_section_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_s_delimited_by_) -> "option_s_delimited_by_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_returning_) -> "option_returning_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_retry_phrase_) -> "option_retry_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_report_section_) -> "option_report_section_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_read_direction_) -> "option_read_direction_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_raising_exception_) -> "option_raising_exception_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_procedure_division_) -> "option_procedure_division_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_picture_locale_phrase_) -> "option_picture_locale_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_option_TO__name__) -> "option_pf_option_TO__name__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_option_IS__name__) -> "option_pf_option_IS__name__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_86_qualname__) -> "option_pf___anonymous_86_qualname__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_44_property_kind__) -> "option_pf___anonymous_44_property_kind__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_43_integer__) -> "option_pf___anonymous_43_integer__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_38_integer__) -> "option_pf___anonymous_38_integer__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_37_integer__) -> "option_pf___anonymous_37_integer__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_34_integer__) -> "option_pf___anonymous_34_integer__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_33_integer__) -> "option_pf___anonymous_33_integer__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_32_qualname_or_integer__) -> "option_pf___anonymous_32_qualname_or_integer__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_30_qualname_or_integer__) -> "option_pf___anonymous_30_qualname_or_integer__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_14_string_literal__) -> "option_pf___anonymous_14_string_literal__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_101_ident__) -> "option_pf___anonymous_101_ident__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_100_ident__) -> "option_pf___anonymous_100_ident__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_VARYING_ident__) -> "option_pf_VARYING_ident__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_USING_name__) -> "option_pf_USING_name__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_TO_loc_integer___) -> "option_pf_TO_loc_integer___"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_THROUGH_string_or_int_literal__) -> "option_pf_THROUGH_string_or_int_literal__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_THROUGH_qualified_procedure_name__) -> "option_pf_THROUGH_qualified_procedure_name__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_THROUGH_procedure_name__) -> "option_pf_THROUGH_procedure_name__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_REMAINDER_ident__) -> "option_pf_REMAINDER_ident__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_POSITION_integer__) -> "option_pf_POSITION_integer__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_ON_name__) -> "option_pf_ON_name__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_INTO_loc_ident___) -> "option_pf_INTO_loc_ident___"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_INTO_ident__) -> "option_pf_INTO_ident__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_IN_name__) -> "option_pf_IN_name__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_FROM_loc_integer___) -> "option_pf_FROM_loc_integer___"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_FROM_ident_or_literal__) -> "option_pf_FROM_ident_or_literal__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_FROM_expression__) -> "option_pf_FROM_expression__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_BY_ident_or_numeric__) -> "option_pf_BY_ident_or_numeric__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_BY_expression__) -> "option_pf_BY_expression__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_pf_AS_string_literal__) -> "option_pf_AS_string_literal__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_perform_phrase_) -> "option_perform_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_or__NUMBER_NUMBERS__) -> "option_or__NUMBER_NUMBERS__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_or__LINE_LINES__) -> "option_or__LINE_LINES__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_or__IS_ARE__) -> "option_or__IS_ARE__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_or__AREA_AREAS__) -> "option_or__AREA_AREAS__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_object_reference_kind_) -> "option_object_reference_kind_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_object_procedure_division_) -> "option_object_procedure_division_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_name_) -> "option_name_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_mr___anonymous_0__) -> "option_mr___anonymous_0__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_lock_or_retry_) -> "option_lock_or_retry_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_locale_phrase_) -> "option_locale_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_local_storage_section_) -> "option_local_storage_section_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_upon__) -> "option_loc_upon__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_special_names_paragraph__) -> "option_loc_special_names_paragraph__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_source_computer_paragraph__) -> "option_loc_source_computer_paragraph__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_repository_paragraph__) -> "option_loc_repository_paragraph__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_program_procedure_division__) -> "option_loc_program_procedure_division__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_program_definition_no_end__) -> "option_loc_program_definition_no_end__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_procedure_division__) -> "option_loc_procedure_division__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_options_paragraph__) -> "option_loc_options_paragraph__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_object_computer_paragraph__) -> "option_loc_object_computer_paragraph__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_io_control_paragraph__) -> "option_loc_io_control_paragraph__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_input_output_section__) -> "option_loc_input_output_section__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_file_control_paragraph__) -> "option_loc_file_control_paragraph__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_environment_division__) -> "option_loc_environment_division__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_data_division__) -> "option_loc_data_division__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_control_division__) -> "option_loc_control_division__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_loc_configuration_section__) -> "option_loc_configuration_section__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_linkage_section_) -> "option_linkage_section_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_limit_is__) -> "option_limit_is__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_io_control_entry_) -> "option_io_control_entry_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_integer_) -> "option_integer_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_instance_definition_) -> "option_instance_definition_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_file_section_) -> "option_file_section_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_expression_no_all_) -> "option_expression_no_all_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_expands_phrase_) -> "option_expands_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_entry_name_clause_) -> "option_entry_name_clause_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_endianness_mode_) -> "option_endianness_mode_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_depending_phrase_) -> "option_depending_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_default_section_) -> "option_default_section_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_default_display_clause_) -> "option_default_display_clause_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_default_accept_clause_) -> "option_default_accept_clause_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_communication_section_) -> "option_communication_section_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_collating_sequence_phrase_) -> "option_collating_sequence_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_close_format_) -> "option_close_format_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_capacity_phrase_) -> "option_capacity_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_call_using_by_) -> "option_call_using_by_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_advancing_phrase_) -> "option_advancing_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option__assign_external__) -> "option__assign_external__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_78_) -> "option___anonymous_78_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_74_) -> "option___anonymous_74_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_73_) -> "option___anonymous_73_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_59_) -> "option___anonymous_59_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_57_) -> "option___anonymous_57_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_39_) -> "option___anonymous_39_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_25_) -> "option___anonymous_25_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_24_) -> "option___anonymous_24_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_22_) -> "option___anonymous_22_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_1_) -> "option___anonymous_1_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_WITH_) -> "option_WITH_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_WHEN_) -> "option_WHEN_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_TO_) -> "option_TO_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_TIMES_) -> "option_TIMES_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_THEN_) -> "option_THEN_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_THAN_) -> "option_THAN_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_TERMINAL_) -> "option_TERMINAL_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_TAPE_) -> "option_TAPE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_SYMBOLIC_) -> "option_SYMBOLIC_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_STRUCTURE_) -> "option_STRUCTURE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_STATUS_) -> "option_STATUS_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_SIZE_) -> "option_SIZE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_SIGN_) -> "option_SIGN_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_SET_) -> "option_SET_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_RIGHT_) -> "option_RIGHT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_REFERENCES_) -> "option_REFERENCES_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_RECORD_) -> "option_RECORD_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_PROGRAM_) -> "option_PROGRAM_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_PROCEDURE_) -> "option_PROCEDURE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_PRINTING_) -> "option_PRINTING_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_PERIOD_) -> "option_PERIOD_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_OTHER_) -> "option_OTHER_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_ORDER_) -> "option_ORDER_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_ON_) -> "option_ON_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_OF_) -> "option_OF_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_NUMBER_) -> "option_NUMBER_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_MODE_) -> "option_MODE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_MESSAGE_) -> "option_MESSAGE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_LINES_) -> "option_LINES_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_LINE_) -> "option_LINE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_LENGTH_) -> "option_LENGTH_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_LEFT_) -> "option_LEFT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_KEY_) -> "option_KEY_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_IS_) -> "option_IS_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_INITIAL_) -> "option_INITIAL_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_INDICATE_) -> "option_INDICATE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_IN_) -> "option_IN_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_FROM_) -> "option_FROM_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_FOR_) -> "option_FOR_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_FILE_) -> "option_FILE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_EVERY_) -> "option_EVERY_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_END_) -> "option_END_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_DEFAULT_) -> "option_DEFAULT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_DATA_) -> "option_DATA_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_CONTAINS_) -> "option_CONTAINS_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_COLLATING_) -> "option_COLLATING_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_CHARACTERS_) -> "option_CHARACTERS_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_CHARACTER_) -> "option_CHARACTER_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_BY_) -> "option_BY_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_AT_) -> "option_AT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_AS_) -> "option_AS_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_AREA_) -> "option_AREA_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_ARE_) -> "option_ARE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_option_ADVANCING_) -> "option_ADVANCING_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_open_statement) -> "open_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_open_phrase) -> "open_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_open_mode) -> "open_mode"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_on_overflow) -> "on_overflow"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_on_or_off) -> "on_or_off"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_on_key) -> "on_key"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_on_exception) -> "on_exception"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_occurs_fixed_clause) -> "occurs_fixed_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_occurs_dynamic_clause) -> "occurs_dynamic_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_occurs_depending_clause) -> "occurs_depending_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_object_view) -> "object_view"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_object_reference_kind) -> "object_reference_kind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_object_ref) -> "object_ref"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_object_procedure_division) -> "object_procedure_division"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_object_paragraph) -> "object_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_object_computer_paragraph) -> "object_computer_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_object_computer_clause) -> "object_computer_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_numeric_literal) -> "<numeric literal>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ntl_name_) -> "ntl_name_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ntl_arithmetic_term_) -> "ntl_arithmetic_term_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonrel_condition) -> "nonrel_condition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonnumeric_literal_no_all) -> "nonnumeric_literal_no_all"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonnumeric_literal) -> "nonnumeric_literal"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_next_group_clause) -> "next_group_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nell_rev_when_phrase_) -> "nell_rev_when_phrase_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nell_rev_source_string_) -> "nell_rev_source_string_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nell_rev_name_) -> "nell_rev_name_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nell_rev_loc_result_imperative_statement__) -> "nell_rev_loc_result_imperative_statement__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nell_rev_loc_when_clause__) -> "nell_rev_loc_when_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nell_rev___anonymous_70_) -> "nell_rev___anonymous_70_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_when_selection_objects_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_validation_stage_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_use_after_exception_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_unstring_target_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_sum_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_subscript_following_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_specifier_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_screen_attribute_on_off_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_rounded_ident_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_qualname_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_qualified_procedure_name_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_pf_ALSO_string_or_int_literal__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_open_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_on_key_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_name_or_alphanum_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_name_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_using_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_using_by__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_tallying_for__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_special_names_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_source_destination_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_sentence__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_select_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_section_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_screen_attribute_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_replacing_phrase__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_options_clause__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_literal__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc_decl_section_paragraph__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_loc___anonymous_72__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_literal_through_literal_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_literal_phrase_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_line_position_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_integer_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_ident_or_string_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_ident_or_numeric_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_ident_or_literal_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_ident_by_after_before_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_ident_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_file_with_opt_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_debug_target_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_column_position_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel_argument_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_84_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_80_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_50_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_48_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_42_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_29_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_21_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_16_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_13_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_names_or_open_mode) -> "names_or_open_mode"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_names) -> "names"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_name_or_string) -> "name_or_string"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_name_or_alphanum) -> "name_or_alphanum"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_name) -> "<word>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_multiply_statement) -> "multiply_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_multiple_file_clause) -> "multiple_file_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_move_statement) -> "move_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mnemonic_name_suffix) -> "mnemonic_name_suffix"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mnemonic_name_clause) -> "mnemonic_name_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_85_) -> "midrule___anonymous_85_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_77_) -> "midrule___anonymous_77_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_on_overflow_NOT_ON_OVERFLOW__) -> "midrule___anonymous_76_on_overflow_NOT_ON_OVERFLOW__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_on_exception_NOT_ON_EXCEPTION__) -> "midrule___anonymous_76_on_exception_NOT_ON_EXCEPTION__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_at_eop_NOT_AT_EOP__) -> "midrule___anonymous_76_at_eop_NOT_AT_EOP__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_at_end_NOT_AT_END__) -> "midrule___anonymous_76_at_end_NOT_AT_END__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_ON_SIZE_ERROR_NOT_ON_SIZE_ERROR__) -> "midrule___anonymous_76_ON_SIZE_ERROR_NOT_ON_SIZE_ERROR__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_INVALID_KEY_NOT_INVALID_KEY__) -> "midrule___anonymous_76_INVALID_KEY_NOT_INVALID_KEY__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_68_) -> "midrule___anonymous_68_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_67_) -> "midrule___anonymous_67_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_66_) -> "midrule___anonymous_66_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_65_) -> "midrule___anonymous_65_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_64_) -> "midrule___anonymous_64_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_62_) -> "midrule___anonymous_62_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_61_) -> "midrule___anonymous_61_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_58_) -> "midrule___anonymous_58_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_55_) -> "midrule___anonymous_55_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_54_) -> "midrule___anonymous_54_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_53_) -> "midrule___anonymous_53_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_52_) -> "midrule___anonymous_52_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_51_) -> "midrule___anonymous_51_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_40_) -> "midrule___anonymous_40_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_35_) -> "midrule___anonymous_35_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_28_) -> "midrule___anonymous_28_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_27_) -> "midrule___anonymous_27_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_15_) -> "midrule___anonymous_15_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_0_) -> "midrule___anonymous_0_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_method_identification) -> "method_identification"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_method_id_paragraph) -> "method_id_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_method_definition) -> "method_definition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_message_or_segment) -> "message_or_segment"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_merge_statement) -> "merge_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_memory_size_unit) -> "memory_size_unit"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_memory_size_clause) -> "memory_size_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mcs_kind) -> "mcs_kind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mcs_command) -> "mcs_command"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption_sf_rnel_loc_options_clause___PERIOD__) -> "loption_sf_rnel_loc_options_clause___PERIOD__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption_pf___anonymous_26_nel_name___) -> "loption_pf___anonymous_26_nel_name___"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption_pf___anonymous_20_names__) -> "loption_pf___anonymous_20_names__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption_pf___anonymous_17_names__) -> "loption_pf___anonymous_17_names__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption_pf_USING_rnel_loc_using_by____) -> "loption_pf_USING_rnel_loc_using_by____"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption_pf_UPON_names__) -> "loption_pf_UPON_names__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption_pf_ON_rnel_validation_stage___) -> "loption_pf_ON_rnel_validation_stage___"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption_indexed_by_) -> "loption_indexed_by_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption_declaratives_) -> "loption_declaratives_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_9_) -> "loption___anonymous_9_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_8_) -> "loption___anonymous_8_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_7_) -> "loption___anonymous_7_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_6_) -> "loption___anonymous_6_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_5_) -> "loption___anonymous_5_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_49_) -> "loption___anonymous_49_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_4_) -> "loption___anonymous_4_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lock_or_retry) -> "lock_or_retry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lock_mode_clause) -> "lock_mode_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lock_mode) -> "lock_mode"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_locale_value_or_ident) -> "locale_value_or_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_locale_phrase) -> "locale_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_locale_or_default) -> "locale_or_default"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_locale_or_ambiguous) -> "locale_or_ambiguous"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_locale_clause) -> "locale_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_locale_category) -> "locale_category"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_local_storage_section) -> "local_storage_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ll_rev_loc_compilation_unit__) -> "ll_rev_loc_compilation_unit__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ll_rev_and_clause_) -> "ll_rev_and_clause_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_literal_through_literal) -> "literal_through_literal"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_literal_phrase) -> "literal_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_literal_int_ident) -> "literal_int_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_literal) -> "<literal>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_select_) -> "list_select_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_pf_FILE_name__) -> "list_pf_FILE_name__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_name_) -> "list_name_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_result_imperative_statement__) -> "list_loc_result_imperative_statement__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_sort_merge_file_descr_clause__) -> "list_loc_sort_merge_file_descr_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_sentence__) -> "list_loc_sentence__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_section_paragraph__) -> "list_loc_section_paragraph__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_screen_descr_clause__) -> "list_loc_screen_descr_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_same_area_clause__) -> "list_loc_same_area_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_rerun_clause__) -> "list_loc_rerun_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_report_group_descr_clause__) -> "list_loc_report_group_descr_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_report_descr_entry__) -> "list_loc_report_descr_entry__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_report_descr_clause__) -> "list_loc_report_descr_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_program_definition__) -> "list_loc_program_definition__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_object_computer_clause__) -> "list_loc_object_computer_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_multiple_file_clause__) -> "list_loc_multiple_file_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_method_definition__) -> "list_loc_method_definition__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_informational_paragraph__) -> "list_loc_informational_paragraph__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_file_or_sort_merge_descr_entry__) -> "list_loc_file_or_sort_merge_descr_entry__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_file_descr_clause__) -> "list_loc_file_descr_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_data_descr_clause__) -> "list_loc_data_descr_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_constant_or_screen_descr_entry__) -> "list_loc_constant_or_screen_descr_entry__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_constant_or_report_group_descr_entry__) -> "list_loc_constant_or_report_group_descr_entry__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_constant_or_data_descr_entry__) -> "list_loc_constant_or_data_descr_entry__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_communication_descr_entry__) -> "list_loc_communication_descr_entry__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_loc_communication_descr_clause__) -> "list_loc_communication_descr_clause__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_key_is_) -> "list_key_is_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_inspect_where_) -> "list_inspect_where_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_list_entry_name_clause_) -> "list_entry_name_clause_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_linkage_section) -> "linkage_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_line_position) -> "line_position"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_line_number) -> "line_number"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_line_header) -> "line_header"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_linage_header) -> "linage_header"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_linage_clause) -> "linage_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lc_all_or_default) -> "lc_all_or_default"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_clause) -> "label_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_l_pf_AFTER_loc_varying_phrase___) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_l_loc___anonymous_79__) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_l___anonymous_99_) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_key_is) -> "key_is"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_justified_clause) -> "justified_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_io_control_paragraph) -> "io_control_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_io_control_entry) -> "io_control_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_invoke_statement) -> "invoke_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_invalid_when_clause) -> "invalid_when_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_intrinsic_function_name) -> "intrinsic_function_name"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_intermediate_rounding_clause) -> "intermediate_rounding_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_interface_specifier) -> "interface_specifier"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_interface_identification) -> "interface_identification"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_interface_id_paragraph) -> "interface_id_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_interface_definition) -> "interface_definition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_integers) -> "integers"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_integer) -> "<integer literal>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_instance_identification) -> "instance_identification"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_instance_definition) -> "instance_definition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_inspect_where) -> "inspect_where"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_inspect_statement) -> "inspect_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_inspect_spec) -> "inspect_spec"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_input_output_section) -> "input_output_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_input_or_using) -> "input_or_using"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_inline_invocation) -> "inline_invocation"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_initiate_statement) -> "initiate_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_initialize_statement) -> "initialize_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_init_data_category) -> "init_data_category"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_informational_paragraphs) -> "informational_paragraphs"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_informational_paragraph) -> "informational_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_info_word) -> "<word>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_indexed_by) -> "indexed_by"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_in_of) -> "in_of"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_imperative_statement) -> "imperative_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_imp_stmts) -> ""
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_if_statement_explicit_term) -> "if_statement_explicit_term"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_if_statement) -> "if_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_if_body) -> "if_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_idents) -> "<identifiers>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_or_string_no_all) -> "ident_or_string_no_all"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_or_string) -> "ident_or_string"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_or_numeric) -> "ident_or_numeric"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_or_nonnumeric_no_all) -> "ident_or_nonnumeric_no_all"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_or_nonnumeric) -> "ident_or_nonnumeric"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_or_nested) -> "ident_or_nested"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_or_literal) -> "<identifier or literal>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_or_integer) -> "ident_or_integer"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_or_alphanum) -> "ident_or_alphanum"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_by_after_before) -> "ident_by_after_before"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_after_before_list) -> "ident_after_before_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident_after_before) -> "ident_after_before"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident) -> "<identifier>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_group_usage_clause) -> "group_usage_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_group_indicate_clause) -> "group_indicate_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_goback_statement) -> "goback_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_go_to_statement) -> "go_to_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_global_clause) -> "global_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generate_statement) -> "generate_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_function_unit) -> "function_unit"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_function_specifier) -> "function_specifier"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_function_name) -> "<function-name>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_function_identification) -> "function_identification"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_function_id_paragraph) -> "function_id_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_from_to_characters_opt) -> "from_to_characters_opt"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_free_statement) -> "free_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_format_clause) -> "format_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_for_alphanumeric_or_national_opt) -> "for_alphanumeric_or_national_opt"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_floatlit) -> "<floating-point literal>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_float_decimal_clause) -> "float_decimal_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_float_content) -> "float_content"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_float_binary_clause) -> "float_binary_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_flat_combination_operand) -> "flat_combination_operand"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fixedlit) -> "<fixed-point literal>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_file_with_opt) -> "file_with_opt"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_file_status_clause) -> "file_status_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_file_section) -> "file_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_file_or_sort_merge_descr_entry) -> "file_or_sort_merge_descr_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_file_descr_clause) -> "file_descr_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_file_control_paragraph) -> "file_control_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_figurative_constant) -> "<figurative constant>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_factory_paragraph) -> "factory_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_factory_identification) -> "factory_identification"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_factory_definition) -> "factory_definition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_external_clause) -> "external_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extended_condition) -> "extended_condition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expression_par_unop) -> "<expression>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expression_no_all) -> "<expression>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expression) -> "<expression>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_unary) -> "expr_unary"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_term_par_unop) -> "expr_term_par_unop"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_term_no_all) -> "expr_term_no_all"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_term) -> "expr_term"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_factor_par_unop) -> "expr_factor_par_unop"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_factor_no_all) -> "expr_factor_no_all"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_factor) -> "expr_factor"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expands_phrase) -> "expands_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_exit_statement) -> "exit_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_exit_spec) -> "exit_spec"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_evaluate_statement) -> "evaluate_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_error_or_no_error) -> "error_or_no_error"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_erase_clause) -> "erase_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_environment_division) -> "environment_division"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_entry_name_clause) -> "entry_name_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_entry_convention_clause) -> "entry_convention_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_enter_statement) -> "enter_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ending_indicator) -> "ending_indicator"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_endianness_mode_) -> "endianness_mode_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_endianness_mode) -> "endianness_mode"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_end_subtract) -> "end_subtract"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_end_search) -> "end_search"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_end_multiply) -> "end_multiply"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_end_divide) -> "end_divide"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_end_display) -> "end_display"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_end_add) -> "end_add"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_end_accept) -> "end_accept"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_encoding_mode) -> "encoding_mode"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_encoding_endianness_opt) -> "encoding_endianness_opt"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_encoding_endianness) -> "encoding_endianness"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_enable_statement) -> "enable_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_else_phrase) -> "else_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_elementary_string_or_int_literal) -> "elementary_string_or_int_literal"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_elementary_nonnumeric_literal) -> "elementary_nonnumeric_literal"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_dynamic_length_structure_clause) -> "dynamic_length_structure_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_dynamic_length_clause) -> "dynamic_length_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_divide_statement) -> "divide_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_display_statement) -> "display_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_disable_statement) -> "disable_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_destination_clause) -> "destination_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_depending_phrase) -> "depending_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_delete_statement) -> "delete_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_default_section_clauses) -> "default_section_clauses"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_default_section) -> "default_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_default_display_clause) -> "default_display_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_default_clause) -> "default_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_default_accept_clause) -> "default_accept_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_declaratives) -> "declaratives"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_decl_section_paragraph) -> "decl_section_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_decimal_point_clause) -> "decimal_point_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_debug_target) -> "debug_target"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_date_day_time) -> "date_day_time"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_data_value_clause) -> "data_value_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_data_type_clause) -> "data_type_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_data_occurs_clause) -> "data_occurs_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_data_division) -> "data_division"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_data_descr_entry) -> "data_descr_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_data_descr_clause) -> "data_descr_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_data_clause) -> "data_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_cursor_clause) -> "cursor_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_currency_sign_clause) -> "currency_sign_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_cs_national) -> "cs_national"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_cs_alphanumeric) -> "cs_alphanumeric"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_crt_status_clause) -> "crt_status_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_counter) -> "counter"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_control_division) -> "control_division"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_control_clause) -> "control_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_continue_statement) -> "continue_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant_value_length) -> "constant_value_length"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant_spec) -> "constant_spec"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant_record_clause) -> "constant_record_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant_or_screen_descr_entry) -> "constant_or_screen_descr_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant_or_report_group_descr_entry) -> "constant_or_report_group_descr_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant_or_data_descr_entry) -> "constant_or_data_descr_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant_level) -> "constant_level"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant) -> "constant"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_configuration_section) -> "configuration_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_condition) -> "condition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_compute_statement) -> "compute_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_complex_condition) -> "complex_condition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_compilation_unit) -> "compilation_unit"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_compilation_group) -> "compilation_group"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_communication_section) -> "communication_section"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_communication_descr_entry) -> "communication_descr_entry"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_communication_descr_clause) -> "communication_descr_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_comment_entry) -> "<comment entry>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_column_position) -> "column_position"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_column_number) -> "column_number"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_column_header) -> "column_header"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_collating_sequence_phrase) -> "collating_sequence_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_collating_sequence_clause) -> "collating_sequence_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_code_set_clause) -> "code_set_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_code_clause) -> "code_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_close_statement) -> "close_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_close_format) -> "close_format"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_specifier) -> "class_specifier"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_name_clause) -> "class_name_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_identification) -> "class_identification"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_id_paragraph) -> "class_id_paragraph"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_definition) -> "class_definition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_condition_no_ident) -> "class_condition_no_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_condition) -> "class_condition"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_clause) -> "class_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_) -> "class_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_character_set) -> "character_set"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_character_classification_clause) -> "character_classification_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_character_classification) -> "character_classification"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_cc_national) -> "cc_national"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_cc_alphanumeric) -> "cc_alphanumeric"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_category_to_value) -> "category_to_value"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_capacity_phrase) -> "capacity_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_cancel_statement) -> "cancel_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_call_using_by) -> "call_using_by"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_call_statement) -> "call_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_call_prefix) -> "call_prefix"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_or__RECORD_RECORDS__) -> "boption_or__RECORD_RECORDS__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_or__LINE_LINES__) -> "boption_or__LINE_LINES__"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_87_) -> "boption___anonymous_87_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_81_) -> "boption___anonymous_81_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_71_) -> "boption___anonymous_71_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_60_) -> "boption___anonymous_60_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_56_) -> "boption___anonymous_56_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_47_) -> "boption___anonymous_47_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_46_) -> "boption___anonymous_46_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_45_) -> "boption___anonymous_45_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_41_) -> "boption___anonymous_41_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_3_) -> "boption___anonymous_3_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_18_) -> "boption___anonymous_18_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_12_) -> "boption___anonymous_12_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_11_) -> "boption___anonymous_11_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_102_) -> "boption___anonymous_102_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_10_) -> "boption___anonymous_10_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_YYYYMMDD_) -> "boption_YYYYMMDD_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_YYYYDDD_) -> "boption_YYYYDDD_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_STRONG_) -> "boption_STRONG_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_SIGNED_) -> "boption_SIGNED_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_SHORT_) -> "boption_SHORT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_OVERRIDE_) -> "boption_OVERRIDE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_OPTIONAL_) -> "boption_OPTIONAL_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_ONLY_) -> "boption_ONLY_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_NOT_) -> "boption_NOT_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_MULTIPLE_) -> "boption_MULTIPLE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_IN_ARITHMETIC_RANGE_) -> "boption_IN_ARITHMETIC_RANGE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_INITIALIZED_) -> "boption_INITIALIZED_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_INITIAL_) -> "boption_INITIAL_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_GLOBAL_) -> "boption_GLOBAL_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_CYCLE_) -> "boption_CYCLE_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boption_ALL_) -> "boption_ALL_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_boollit) -> "boollit"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_block_contains_clause) -> "block_contains_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_blank_when_zero_clause) -> "blank_when_zero_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_blank_clause) -> "blank_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_based_clause) -> "based_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_atomic_expression_no_all) -> "<atomic expression>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_atomic_expression) -> "<atomic expression>"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_at_eop) -> "at_eop"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_at_end) -> "at_end"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_assign_clause) -> "assign_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_as__strlit_) -> "as__strlit_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_arithmetic_term_no_all) -> "arithmetic_term_no_all"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_arithmetic_term) -> "arithmetic_term"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_arithmetic_mode) -> "arithmetic_mode"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_arithmetic_clause) -> "arithmetic_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_argument) -> "argument"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_area_source) -> "area_source"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_any_length_clause) -> "any_length_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_clause) -> "and_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_alternate_record_key_clause) -> "alternate_record_key_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_alter_statement) -> "alter_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_alphabet_specification) -> "alphabet_specification"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_alphabet_name_clause) -> "alphabet_name_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_allocate_statement) -> "allocate_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_alignment) -> "alignment"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_aligned_clause) -> "aligned_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_after_or_before) -> "after_or_before"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_advancing_phrase) -> "advancing_phrase"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_address) -> "address"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_add_statement) -> "add_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_access_mode_clause) -> "access_mode_clause"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_access_mode) -> "access_mode"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_accept_statement) -> "accept_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N__assign_external_) -> "_assign_external_"

let print_value (type a) : a MenhirInterpreter.symbol -> a -> string = function
  | MenhirInterpreter.T T_error -> (fun _ -> "error")
  | MenhirInterpreter.T T_ZERO_FILL -> (fun _ -> "ZERO_FILL")
  | MenhirInterpreter.T T_ZERO -> (fun _ -> "ZERO")
  | MenhirInterpreter.T T_YYYYMMDD -> (fun _ -> "YYYYMMDD")
  | MenhirInterpreter.T T_YYYYDDD -> (fun _ -> "YYYYDDD")
  | MenhirInterpreter.T T_Y -> (fun _ -> "Y")
  | MenhirInterpreter.T T_XOR -> (fun _ -> "XOR")
  | MenhirInterpreter.T T_XML_TEXT -> (fun _ -> "XML_TEXT")
  | MenhirInterpreter.T T_XML_SCHEMA -> (fun _ -> "XML_SCHEMA")
  | MenhirInterpreter.T T_XML_NTEXT -> (fun _ -> "XML_NTEXT")
  | MenhirInterpreter.T T_XML_EVENT -> (fun _ -> "XML_EVENT")
  | MenhirInterpreter.T T_XML_DECLARATION -> (fun _ -> "XML_DECLARATION")
  | MenhirInterpreter.T T_XML -> (fun _ -> "XML")
  | MenhirInterpreter.T T_X -> (fun _ -> "X")
  | MenhirInterpreter.T T_WRITING -> (fun _ -> "WRITING")
  | MenhirInterpreter.T T_WRITE_VERIFY -> (fun _ -> "WRITE_VERIFY")
  | MenhirInterpreter.T T_WRITE_ONLY -> (fun _ -> "WRITE_ONLY")
  | MenhirInterpreter.T T_WRITERS -> (fun _ -> "WRITERS")
  | MenhirInterpreter.T T_WRITE -> (fun _ -> "WRITE")
  | MenhirInterpreter.T T_WRAP -> (fun _ -> "WRAP")
  | MenhirInterpreter.T T_WORKING_STORAGE -> (fun _ -> "WORKING_STORAGE")
  | MenhirInterpreter.T T_WORD_IN_AREA_A -> (fun _ -> "WORD_IN_AREA_A")
  | MenhirInterpreter.T T_WORDS -> (fun _ -> "WORDS")
  | MenhirInterpreter.T T_WORD -> (fun _ -> "WORD")
  | MenhirInterpreter.T T_WITH_DATA -> (fun _ -> "WITH_DATA")
  | MenhirInterpreter.T T_WITH -> (fun _ -> "WITH")
  | MenhirInterpreter.T T_WINDOW -> (fun _ -> "WINDOW")
  | MenhirInterpreter.T T_WIDTH_IN_CELLS -> (fun _ -> "WIDTH_IN_CELLS")
  | MenhirInterpreter.T T_WIDTH -> (fun _ -> "WIDTH")
  | MenhirInterpreter.T T_WIDE -> (fun _ -> "WIDE")
  | MenhirInterpreter.T T_WHILE -> (fun _ -> "WHILE")
  | MenhirInterpreter.T T_WHEN_COMPILED -> (fun _ -> "WHEN_COMPILED")
  | MenhirInterpreter.T T_WHEN -> (fun _ -> "WHEN")
  | MenhirInterpreter.T T_WEB_BROWSER -> (fun _ -> "WEB_BROWSER")
  | MenhirInterpreter.T T_WAIT -> (fun _ -> "WAIT")
  | MenhirInterpreter.T T_VTOP -> (fun _ -> "VTOP")
  | MenhirInterpreter.T T_VSCROLL_POS -> (fun _ -> "VSCROLL_POS")
  | MenhirInterpreter.T T_VSCROLL_BAR -> (fun _ -> "VSCROLL_BAR")
  | MenhirInterpreter.T T_VSCROLL -> (fun _ -> "VSCROLL")
  | MenhirInterpreter.T T_VPADDING -> (fun _ -> "VPADDING")
  | MenhirInterpreter.T T_VOLATILE -> (fun _ -> "VOLATILE")
  | MenhirInterpreter.T T_VLR -> (fun _ -> "VLR")
  | MenhirInterpreter.T T_VISIBLE -> (fun _ -> "VISIBLE")
  | MenhirInterpreter.T T_VIRTUAL_WIDTH -> (fun _ -> "VIRTUAL_WIDTH")
  | MenhirInterpreter.T T_VIRTUAL -> (fun _ -> "VIRTUAL")
  | MenhirInterpreter.T T_VIA -> (fun _ -> "VIA")
  | MenhirInterpreter.T T_VERY_HEAVY -> (fun _ -> "VERY_HEAVY")
  | MenhirInterpreter.T T_VERTICAL -> (fun _ -> "VERTICAL")
  | MenhirInterpreter.T T_VERSION -> (fun _ -> "VERSION")
  | MenhirInterpreter.T T_VARYING -> (fun _ -> "VARYING")
  | MenhirInterpreter.T T_VARIANT -> (fun _ -> "VARIANT")
  | MenhirInterpreter.T T_VARIABLE -> (fun _ -> "VARIABLE")
  | MenhirInterpreter.T T_VARBINARY -> (fun _ -> "VARBINARY")
  | MenhirInterpreter.T T_VALUE_FORMAT -> (fun _ -> "VALUE_FORMAT")
  | MenhirInterpreter.T T_VALUES -> (fun _ -> "VALUES")
  | MenhirInterpreter.T T_VALUE -> (fun _ -> "VALUE")
  | MenhirInterpreter.T T_VALIDATING -> (fun _ -> "VALIDATING")
  | MenhirInterpreter.T T_VALIDATE_STATUS -> (fun _ -> "VALIDATE_STATUS")
  | MenhirInterpreter.T T_VALIDATE -> (fun _ -> "VALIDATE")
  | MenhirInterpreter.T T_VALID -> (fun _ -> "VALID")
  | MenhirInterpreter.T T_V -> (fun _ -> "V")
  | MenhirInterpreter.T T_UTF_8 -> (fun _ -> "UTF_8")
  | MenhirInterpreter.T T_UTF_16 -> (fun _ -> "UTF_16")
  | MenhirInterpreter.T T_USING -> (fun _ -> "USING")
  | MenhirInterpreter.T T_USE_TAB -> (fun _ -> "USE_TAB")
  | MenhirInterpreter.T T_USE_RETURN -> (fun _ -> "USE_RETURN")
  | MenhirInterpreter.T T_USE_ALT -> (fun _ -> "USE_ALT")
  | MenhirInterpreter.T T_USER_WHITE -> (fun _ -> "USER_WHITE")
  | MenhirInterpreter.T T_USER_GRAY -> (fun _ -> "USER_GRAY")
  | MenhirInterpreter.T T_USER_DEFAULT -> (fun _ -> "USER_DEFAULT")
  | MenhirInterpreter.T T_USER_COLORS -> (fun _ -> "USER_COLORS")
  | MenhirInterpreter.T T_USER -> (fun _ -> "USER")
  | MenhirInterpreter.T T_USE -> (fun _ -> "USE")
  | MenhirInterpreter.T T_USAGE -> (fun _ -> "USAGE")
  | MenhirInterpreter.T T_UPPER -> (fun _ -> "UPPER")
  | MenhirInterpreter.T T_UPON -> (fun _ -> "UPON")
  | MenhirInterpreter.T T_UPDATERS -> (fun _ -> "UPDATERS")
  | MenhirInterpreter.T T_UPDATE -> (fun _ -> "UPDATE")
  | MenhirInterpreter.T T_UP -> (fun _ -> "UP")
  | MenhirInterpreter.T T_UNUSED__ -> (fun _ -> "UNUSED__")
  | MenhirInterpreter.T T_UNTIL -> (fun _ -> "UNTIL")
  | MenhirInterpreter.T T_UNSTRING -> (fun _ -> "UNSTRING")
  | MenhirInterpreter.T T_UNSORTED -> (fun _ -> "UNSORTED")
  | MenhirInterpreter.T T_UNSIGNED_SHORT -> (fun _ -> "UNSIGNED_SHORT")
  | MenhirInterpreter.T T_UNSIGNED_LONG -> (fun _ -> "UNSIGNED_LONG")
  | MenhirInterpreter.T T_UNSIGNED_INT -> (fun _ -> "UNSIGNED_INT")
  | MenhirInterpreter.T T_UNSIGNED -> (fun _ -> "UNSIGNED")
  | MenhirInterpreter.T T_UNSEQUAL -> (fun _ -> "UNSEQUAL")
  | MenhirInterpreter.T T_UNLOCK -> (fun _ -> "UNLOCK")
  | MenhirInterpreter.T T_UNIVERSAL -> (fun _ -> "UNIVERSAL")
  | MenhirInterpreter.T T_UNIT -> (fun _ -> "UNIT")
  | MenhirInterpreter.T T_UNFRAMED -> (fun _ -> "UNFRAMED")
  | MenhirInterpreter.T T_UNEQUAL -> (fun _ -> "UNEQUAL")
  | MenhirInterpreter.T T_UNDERLINE -> (fun _ -> "UNDERLINE")
  | MenhirInterpreter.T T_UNBOUNDED -> (fun _ -> "UNBOUNDED")
  | MenhirInterpreter.T T_UFF -> (fun _ -> "UFF")
  | MenhirInterpreter.T T_UCS_4 -> (fun _ -> "UCS_4")
  | MenhirInterpreter.T T_U -> (fun _ -> "U")
  | MenhirInterpreter.T T_TYPEDEF -> (fun _ -> "TYPEDEF")
  | MenhirInterpreter.T T_TYPE -> (fun _ -> "TYPE")
  | MenhirInterpreter.T T_TRUNCATION -> (fun _ -> "TRUNCATION")
  | MenhirInterpreter.T T_TRUE -> (fun _ -> "TRUE")
  | MenhirInterpreter.T T_TRIMMED -> (fun _ -> "TRIMMED")
  | MenhirInterpreter.T T_TREE_VIEW -> (fun _ -> "TREE_VIEW")
  | MenhirInterpreter.T T_TRANSPARENT_COLOR -> (fun _ -> "TRANSPARENT_COLOR")
  | MenhirInterpreter.T T_TRANSPARENT -> (fun _ -> "TRANSPARENT")
  | MenhirInterpreter.T T_TRANSFORM -> (fun _ -> "TRANSFORM")
  | MenhirInterpreter.T T_TRANSACTION_STATUS -> (fun _ -> "TRANSACTION_STATUS")
  | MenhirInterpreter.T T_TRANSACTION -> (fun _ -> "TRANSACTION")
  | MenhirInterpreter.T T_TRAILING_SIGN -> (fun _ -> "TRAILING_SIGN")
  | MenhirInterpreter.T T_TRAILING_SHIFT -> (fun _ -> "TRAILING_SHIFT")
  | MenhirInterpreter.T T_TRAILING -> (fun _ -> "TRAILING")
  | MenhirInterpreter.T T_TRADITIONAL_FONT -> (fun _ -> "TRADITIONAL_FONT")
  | MenhirInterpreter.T T_TRACK_THUMB -> (fun _ -> "TRACK_THUMB")
  | MenhirInterpreter.T T_TRACK_LIMIT -> (fun _ -> "TRACK_LIMIT")
  | MenhirInterpreter.T T_TRACK_AREA -> (fun _ -> "TRACK_AREA")
  | MenhirInterpreter.T T_TRACKS -> (fun _ -> "TRACKS")
  | MenhirInterpreter.T T_TRACK -> (fun _ -> "TRACK")
  | MenhirInterpreter.T T_TRACE -> (fun _ -> "TRACE")
  | MenhirInterpreter.T T_TOWARD_LESSER -> (fun _ -> "TOWARD_LESSER")
  | MenhirInterpreter.T T_TOWARD_GREATER -> (fun _ -> "TOWARD_GREATER")
  | MenhirInterpreter.T T_TOTALING -> (fun _ -> "TOTALING")
  | MenhirInterpreter.T T_TOTALED -> (fun _ -> "TOTALED")
  | MenhirInterpreter.T T_TOP_LEVEL -> (fun _ -> "TOP_LEVEL")
  | MenhirInterpreter.T T_TOP -> (fun _ -> "TOP")
  | MenhirInterpreter.T T_TOOL_BAR -> (fun _ -> "TOOL_BAR")
  | MenhirInterpreter.T T_TO -> (fun _ -> "TO")
  | MenhirInterpreter.T T_TITLE_POSITION -> (fun _ -> "TITLE_POSITION")
  | MenhirInterpreter.T T_TITLE_BAR -> (fun _ -> "TITLE_BAR")
  | MenhirInterpreter.T T_TITLE -> (fun _ -> "TITLE")
  | MenhirInterpreter.T T_TIME_RECORD -> (fun _ -> "TIME_RECORD")
  | MenhirInterpreter.T T_TIME_OUT -> (fun _ -> "TIME_OUT")
  | MenhirInterpreter.T T_TIME_OF_DAY -> (fun _ -> "TIME_OF_DAY")
  | MenhirInterpreter.T T_TIMESTAMP_RECORD -> (fun _ -> "TIMESTAMP_RECORD")
  | MenhirInterpreter.T T_TIMESTAMP_OFFSET_RECORD -> (fun _ -> "TIMESTAMP_OFFSET_RECORD")
  | MenhirInterpreter.T T_TIMESTAMP_OFFSET -> (fun _ -> "TIMESTAMP_OFFSET")
  | MenhirInterpreter.T T_TIMESTAMP -> (fun _ -> "TIMESTAMP")
  | MenhirInterpreter.T T_TIMES -> (fun _ -> "TIMES")
  | MenhirInterpreter.T T_TIME -> (fun _ -> "TIME")
  | MenhirInterpreter.T T_TILED_HEADINGS -> (fun _ -> "TILED_HEADINGS")
  | MenhirInterpreter.T T_THUMB_POSITION -> (fun _ -> "THUMB_POSITION")
  | MenhirInterpreter.T T_THROUGH -> (fun _ -> "THROUGH")
  | MenhirInterpreter.T T_THREEDIMENSIONAL -> (fun _ -> "THREEDIMENSIONAL")
  | MenhirInterpreter.T T_THREAD_POINTER -> (fun _ -> "THREAD_POINTER")
  | MenhirInterpreter.T T_THREAD_LOCAL_STORAGE -> (fun _ -> "THREAD_LOCAL_STORAGE")
  | MenhirInterpreter.T T_THREAD_LOCAL -> (fun _ -> "THREAD_LOCAL")
  | MenhirInterpreter.T T_THREADS -> (fun _ -> "THREADS")
  | MenhirInterpreter.T T_THREAD -> (fun _ -> "THREAD")
  | MenhirInterpreter.T T_THEN -> (fun _ -> "THEN")
  | MenhirInterpreter.T T_THAN -> (fun _ -> "THAN")
  | MenhirInterpreter.T T_TEXT -> (fun _ -> "TEXT")
  | MenhirInterpreter.T T_TEST -> (fun _ -> "TEST")
  | MenhirInterpreter.T T_TERMINATION_VALUE -> (fun _ -> "TERMINATION_VALUE")
  | MenhirInterpreter.T T_TERMINATE -> (fun _ -> "TERMINATE")
  | MenhirInterpreter.T T_TERMINAL_X -> (fun _ -> "TERMINAL_X")
  | MenhirInterpreter.T T_TERMINAL_INFO -> (fun _ -> "TERMINAL_INFO")
  | MenhirInterpreter.T T_TERMINAL_3 -> (fun _ -> "TERMINAL_3")
  | MenhirInterpreter.T T_TERMINAL_2 -> (fun _ -> "TERMINAL_2")
  | MenhirInterpreter.T T_TERMINAL_1 -> (fun _ -> "TERMINAL_1")
  | MenhirInterpreter.T T_TERMINAL_0 -> (fun _ -> "TERMINAL_0")
  | MenhirInterpreter.T T_TERMINAL -> (fun _ -> "TERMINAL")
  | MenhirInterpreter.T T_TEMPORARY -> (fun _ -> "TEMPORARY")
  | MenhirInterpreter.T T_TEMP -> (fun _ -> "TEMP")
  | MenhirInterpreter.T T_TAPE -> (fun _ -> "TAPE")
  | MenhirInterpreter.T T_TALLYING -> (fun _ -> "TALLYING")
  | MenhirInterpreter.T T_TALLY -> (fun _ -> "TALLY")
  | MenhirInterpreter.T T_TAB_TO_DELETE -> (fun _ -> "TAB_TO_DELETE")
  | MenhirInterpreter.T T_TAB_TO_ADD -> (fun _ -> "TAB_TO_ADD")
  | MenhirInterpreter.T T_TAB_CONTROL -> (fun _ -> "TAB_CONTROL")
  | MenhirInterpreter.T T_TABLE -> (fun _ -> "TABLE")
  | MenhirInterpreter.T T_TAB -> (fun _ -> "TAB")
  | MenhirInterpreter.T T_SYSTEM_OFFSET -> (fun _ -> "SYSTEM_OFFSET")
  | MenhirInterpreter.T T_SYSTEM_INFO -> (fun _ -> "SYSTEM_INFO")
  | MenhirInterpreter.T T_SYSTEM_DEFAULT -> (fun _ -> "SYSTEM_DEFAULT")
  | MenhirInterpreter.T T_SYSTEM -> (fun _ -> "SYSTEM")
  | MenhirInterpreter.T T_SYSOUT_X -> (fun _ -> "SYSOUT_X")
  | MenhirInterpreter.T T_SYSOUT_3 -> (fun _ -> "SYSOUT_3")
  | MenhirInterpreter.T T_SYSOUT_2 -> (fun _ -> "SYSOUT_2")
  | MenhirInterpreter.T T_SYSOUT_1 -> (fun _ -> "SYSOUT_1")
  | MenhirInterpreter.T T_SYSOUT_0 -> (fun _ -> "SYSOUT_0")
  | MenhirInterpreter.T T_SYSIN_X -> (fun _ -> "SYSIN_X")
  | MenhirInterpreter.T T_SYSIN_3 -> (fun _ -> "SYSIN_3")
  | MenhirInterpreter.T T_SYSIN_2 -> (fun _ -> "SYSIN_2")
  | MenhirInterpreter.T T_SYSIN_1 -> (fun _ -> "SYSIN_1")
  | MenhirInterpreter.T T_SYSIN_0 -> (fun _ -> "SYSIN_0")
  | MenhirInterpreter.T T_SYNCHRONIZED -> (fun _ -> "SYNCHRONIZED")
  | MenhirInterpreter.T T_SYMBOLIC -> (fun _ -> "SYMBOLIC")
  | MenhirInterpreter.T T_SYMBOL -> (fun _ -> "SYMBOL")
  | MenhirInterpreter.T T_SWITCH -> (fun _ -> "SWITCH")
  | MenhirInterpreter.T T_SUPPRESS -> (fun _ -> "SUPPRESS")
  | MenhirInterpreter.T T_SUPER -> (fun _ -> "SUPER")
  | MenhirInterpreter.T T_SUM -> (fun _ -> "SUM")
  | MenhirInterpreter.T T_SUFFIXING -> (fun _ -> "SUFFIXING")
  | MenhirInterpreter.T T_SUB_SCHEMA -> (fun _ -> "SUB_SCHEMA")
  | MenhirInterpreter.T T_SUB_QUEUE_3 -> (fun _ -> "SUB_QUEUE_3")
  | MenhirInterpreter.T T_SUB_QUEUE_2 -> (fun _ -> "SUB_QUEUE_2")
  | MenhirInterpreter.T T_SUB_QUEUE_1 -> (fun _ -> "SUB_QUEUE_1")
  | MenhirInterpreter.T T_SUBWINDOW -> (fun _ -> "SUBWINDOW")
  | MenhirInterpreter.T T_SUBTRACT -> (fun _ -> "SUBTRACT")
  | MenhirInterpreter.T T_SUBFILE -> (fun _ -> "SUBFILE")
  | MenhirInterpreter.T T_STYLE -> (fun _ -> "STYLE")
  | MenhirInterpreter.T T_STRUCTURE -> (fun _ -> "STRUCTURE")
  | MenhirInterpreter.T T_STRONG_NAME -> (fun _ -> "STRONG_NAME")
  | MenhirInterpreter.T T_STRONG -> (fun _ -> "STRONG")
  | MenhirInterpreter.T T_STRING -> (fun _ -> "STRING")
  | MenhirInterpreter.T T_STOP_BROWSER -> (fun _ -> "STOP_BROWSER")
  | MenhirInterpreter.T T_STOP -> (fun _ -> "STOP")
  | MenhirInterpreter.T T_STEP -> (fun _ -> "STEP")
  | MenhirInterpreter.T T_STDCALL -> (fun _ -> "STDCALL")
  | MenhirInterpreter.T T_STATUS_TEXT -> (fun _ -> "STATUS_TEXT")
  | MenhirInterpreter.T T_STATUS_BAR -> (fun _ -> "STATUS_BAR")
  | MenhirInterpreter.T T_STATUS -> (fun _ -> "STATUS")
  | MenhirInterpreter.T T_STATION -> (fun _ -> "STATION")
  | MenhirInterpreter.T T_STATIC_LIST -> (fun _ -> "STATIC_LIST")
  | MenhirInterpreter.T T_STATIC -> (fun _ -> "STATIC")
  | MenhirInterpreter.T T_STATEMENT -> (fun _ -> "STATEMENT")
  | MenhirInterpreter.T T_START_Y -> (fun _ -> "START_Y")
  | MenhirInterpreter.T T_START_X -> (fun _ -> "START_X")
  | MenhirInterpreter.T T_STARTING -> (fun _ -> "STARTING")
  | MenhirInterpreter.T T_START -> (fun _ -> "START")
  | MenhirInterpreter.T T_STANDARD_DECIMAL -> (fun _ -> "STANDARD_DECIMAL")
  | MenhirInterpreter.T T_STANDARD_BINARY -> (fun _ -> "STANDARD_BINARY")
  | MenhirInterpreter.T T_STANDARD_2 -> (fun _ -> "STANDARD_2")
  | MenhirInterpreter.T T_STANDARD_1 -> (fun _ -> "STANDARD_1")
  | MenhirInterpreter.T T_STANDARD -> (fun _ -> "STANDARD")
  | MenhirInterpreter.T T_STACK -> (fun _ -> "STACK")
  | MenhirInterpreter.T T_SSF -> (fun _ -> "SSF")
  | MenhirInterpreter.T T_SQUARE -> (fun _ -> "SQUARE")
  | MenhirInterpreter.T T_SQL_ROWID -> (fun _ -> "SQL_ROWID")
  | MenhirInterpreter.T T_SQL_NCLOB -> (fun _ -> "SQL_NCLOB")
  | MenhirInterpreter.T T_SQL_CURSOR -> (fun _ -> "SQL_CURSOR")
  | MenhirInterpreter.T T_SQL_CLOB -> (fun _ -> "SQL_CLOB")
  | MenhirInterpreter.T T_SQL_BLOB -> (fun _ -> "SQL_BLOB")
  | MenhirInterpreter.T T_SQL_BFILE -> (fun _ -> "SQL_BFILE")
  | MenhirInterpreter.T T_SQLIMS -> (fun _ -> "SQLIMS")
  | MenhirInterpreter.T T_SQL -> (fun _ -> "SQL")
  | MenhirInterpreter.T T_SPINNER -> (fun _ -> "SPINNER")
  | MenhirInterpreter.T T_SPECIAL_NAMES -> (fun _ -> "SPECIAL_NAMES")
  | MenhirInterpreter.T T_SPACE_FILL -> (fun _ -> "SPACE_FILL")
  | MenhirInterpreter.T T_SPACE -> (fun _ -> "SPACE")
  | MenhirInterpreter.T T_SOURCE_COMPUTER -> (fun _ -> "SOURCE_COMPUTER")
  | MenhirInterpreter.T T_SOURCES -> (fun _ -> "SOURCES")
  | MenhirInterpreter.T T_SOURCE -> (fun _ -> "SOURCE")
  | MenhirInterpreter.T T_SORT_WORK -> (fun _ -> "SORT_WORK")
  | MenhirInterpreter.T T_SORT_RETURN -> (fun _ -> "SORT_RETURN")
  | MenhirInterpreter.T T_SORT_ORDER -> (fun _ -> "SORT_ORDER")
  | MenhirInterpreter.T T_SORT_MODE_SIZE -> (fun _ -> "SORT_MODE_SIZE")
  | MenhirInterpreter.T T_SORT_MESSAGE -> (fun _ -> "SORT_MESSAGE")
  | MenhirInterpreter.T T_SORT_MERGE -> (fun _ -> "SORT_MERGE")
  | MenhirInterpreter.T T_SORT_FILE_SIZE -> (fun _ -> "SORT_FILE_SIZE")
  | MenhirInterpreter.T T_SORT_CORE_SIZE -> (fun _ -> "SORT_CORE_SIZE")
  | MenhirInterpreter.T T_SORT_CONTROL -> (fun _ -> "SORT_CONTROL")
  | MenhirInterpreter.T T_SORT -> (fun _ -> "SORT")
  | MenhirInterpreter.T T_SMALL_FONT -> (fun _ -> "SMALL_FONT")
  | MenhirInterpreter.T T_SLASH -> (fun _ -> "/")
  | MenhirInterpreter.T T_SKIP3 -> (fun _ -> "SKIP3")
  | MenhirInterpreter.T T_SKIP2 -> (fun _ -> "SKIP2")
  | MenhirInterpreter.T T_SKIP1 -> (fun _ -> "SKIP1")
  | MenhirInterpreter.T T_SIZE -> (fun _ -> "SIZE")
  | MenhirInterpreter.T T_SINTLIT -> (fun _ -> "SINTLIT")
  | MenhirInterpreter.T T_SIGNED_SHORT -> (fun _ -> "SIGNED_SHORT")
  | MenhirInterpreter.T T_SIGNED_LONG -> (fun _ -> "SIGNED_LONG")
  | MenhirInterpreter.T T_SIGNED_INT -> (fun _ -> "SIGNED_INT")
  | MenhirInterpreter.T T_SIGNED -> (fun _ -> "SIGNED")
  | MenhirInterpreter.T T_SIGN -> (fun _ -> "SIGN")
  | MenhirInterpreter.T T_SHOW_SEL_ALWAYS -> (fun _ -> "SHOW_SEL_ALWAYS")
  | MenhirInterpreter.T T_SHOW_NONE -> (fun _ -> "SHOW_NONE")
  | MenhirInterpreter.T T_SHOW_LINES -> (fun _ -> "SHOW_LINES")
  | MenhirInterpreter.T T_SHORT_DATE -> (fun _ -> "SHORT_DATE")
  | MenhirInterpreter.T T_SHORT -> (fun _ -> "SHORT")
  | MenhirInterpreter.T T_SHIFT_OUT -> (fun _ -> "SHIFT_OUT")
  | MenhirInterpreter.T T_SHIFT_IN -> (fun _ -> "SHIFT_IN")
  | MenhirInterpreter.T T_SHARING -> (fun _ -> "SHARING")
  | MenhirInterpreter.T T_SHADOW -> (fun _ -> "SHADOW")
  | MenhirInterpreter.T T_SHADING -> (fun _ -> "SHADING")
  | MenhirInterpreter.T T_SET -> (fun _ -> "SET")
  | MenhirInterpreter.T T_SERVICE -> (fun _ -> "SERVICE")
  | MenhirInterpreter.T T_SEQUENTIAL -> (fun _ -> "SEQUENTIAL")
  | MenhirInterpreter.T T_SEQUENCE -> (fun _ -> "SEQUENCE")
  | MenhirInterpreter.T T_SEPARATION -> (fun _ -> "SEPARATION")
  | MenhirInterpreter.T T_SEPARATE -> (fun _ -> "SEPARATE")
  | MenhirInterpreter.T T_SENTENCE -> (fun _ -> "SENTENCE")
  | MenhirInterpreter.T T_SEND -> (fun _ -> "SEND")
  | MenhirInterpreter.T T_SEMAPHORE_POINTER -> (fun _ -> "SEMAPHORE_POINTER")
  | MenhirInterpreter.T T_SELF_ACT -> (fun _ -> "SELF_ACT")
  | MenhirInterpreter.T T_SELFCLASS -> (fun _ -> "SELFCLASS")
  | MenhirInterpreter.T T_SELF -> (fun _ -> "SELF")
  | MenhirInterpreter.T T_SELECT_ALL -> (fun _ -> "SELECT_ALL")
  | MenhirInterpreter.T T_SELECTIVE -> (fun _ -> "SELECTIVE")
  | MenhirInterpreter.T T_SELECTION_TEXT -> (fun _ -> "SELECTION_TEXT")
  | MenhirInterpreter.T T_SELECTION_INDEX -> (fun _ -> "SELECTION_INDEX")
  | MenhirInterpreter.T T_SELECTION -> (fun _ -> "SELECTION")
  | MenhirInterpreter.T T_SELECT -> (fun _ -> "SELECT")
  | MenhirInterpreter.T T_SEGMENT_LIMIT -> (fun _ -> "SEGMENT_LIMIT")
  | MenhirInterpreter.T T_SEGMENT -> (fun _ -> "SEGMENT")
  | MenhirInterpreter.T T_SEEK -> (fun _ -> "SEEK")
  | MenhirInterpreter.T T_SECURITY -> (fun _ -> "SECURITY")
  | MenhirInterpreter.T T_SECURE -> (fun _ -> "SECURE")
  | MenhirInterpreter.T T_SECTION -> (fun _ -> "SECTION")
  | MenhirInterpreter.T T_SECONDS -> (fun _ -> "SECONDS")
  | MenhirInterpreter.T T_SECONDARY -> (fun _ -> "SECONDARY")
  | MenhirInterpreter.T T_SEARCH_TEXT -> (fun _ -> "SEARCH_TEXT")
  | MenhirInterpreter.T T_SEARCH_OPTIONS -> (fun _ -> "SEARCH_OPTIONS")
  | MenhirInterpreter.T T_SEARCH -> (fun _ -> "SEARCH")
  | MenhirInterpreter.T T_SD -> (fun _ -> "SD")
  | MenhirInterpreter.T T_SCROLL_BAR -> (fun _ -> "SCROLL_BAR")
  | MenhirInterpreter.T T_SCROLL -> (fun _ -> "SCROLL")
  | MenhirInterpreter.T T_SCREEN -> (fun _ -> "SCREEN")
  | MenhirInterpreter.T T_SAVE_AS_NO_PROMPT -> (fun _ -> "SAVE_AS_NO_PROMPT")
  | MenhirInterpreter.T T_SAVE_AS -> (fun _ -> "SAVE_AS")
  | MenhirInterpreter.T T_SARF -> (fun _ -> "SARF")
  | MenhirInterpreter.T T_SAME -> (fun _ -> "SAME")
  | MenhirInterpreter.T T_S -> (fun _ -> "S")
  | MenhirInterpreter.T T_RUN -> (fun _ -> "RUN")
  | MenhirInterpreter.T T_RPAR -> (fun _ -> ")")
  | MenhirInterpreter.T T_ROW_PROTECTION -> (fun _ -> "ROW_PROTECTION")
  | MenhirInterpreter.T T_ROW_HEADINGS -> (fun _ -> "ROW_HEADINGS")
  | MenhirInterpreter.T T_ROW_FONT -> (fun _ -> "ROW_FONT")
  | MenhirInterpreter.T T_ROW_DIVIDERS -> (fun _ -> "ROW_DIVIDERS")
  | MenhirInterpreter.T T_ROW_COLOR_PATTERN -> (fun _ -> "ROW_COLOR_PATTERN")
  | MenhirInterpreter.T T_ROW_COLOR -> (fun _ -> "ROW_COLOR")
  | MenhirInterpreter.T T_ROWID -> (fun _ -> "ROWID")
  | MenhirInterpreter.T T_ROUNDING -> (fun _ -> "ROUNDING")
  | MenhirInterpreter.T T_ROUNDED -> (fun _ -> "ROUNDED")
  | MenhirInterpreter.T T_ROLLING -> (fun _ -> "ROLLING")
  | MenhirInterpreter.T T_ROLLBACK -> (fun _ -> "ROLLBACK")
  | MenhirInterpreter.T T_RIMMED -> (fun _ -> "RIMMED")
  | MenhirInterpreter.T T_RIGHT_JUSTIFY -> (fun _ -> "RIGHT_JUSTIFY")
  | MenhirInterpreter.T T_RIGHT_ALIGN -> (fun _ -> "RIGHT_ALIGN")
  | MenhirInterpreter.T T_RIGHT -> (fun _ -> "RIGHT")
  | MenhirInterpreter.T T_RH -> (fun _ -> "RH")
  | MenhirInterpreter.T T_RF -> (fun _ -> "RF")
  | MenhirInterpreter.T T_REWRITE -> (fun _ -> "REWRITE")
  | MenhirInterpreter.T T_REWIND -> (fun _ -> "REWIND")
  | MenhirInterpreter.T T_REVERSE_VIDEO -> (fun _ -> "REVERSE_VIDEO")
  | MenhirInterpreter.T T_REVERSED -> (fun _ -> "REVERSED")
  | MenhirInterpreter.T T_REVERSE -> (fun _ -> "REVERSE")
  | MenhirInterpreter.T T_RETURN_UNSIGNED -> (fun _ -> "RETURN_UNSIGNED")
  | MenhirInterpreter.T T_RETURN_CODE -> (fun _ -> "RETURN_CODE")
  | MenhirInterpreter.T T_RETURNING -> (fun _ -> "RETURNING")
  | MenhirInterpreter.T T_RETURN -> (fun _ -> "RETURN")
  | MenhirInterpreter.T T_RETRY -> (fun _ -> "RETRY")
  | MenhirInterpreter.T T_RETENTION -> (fun _ -> "RETENTION")
  | MenhirInterpreter.T T_RESUME -> (fun _ -> "RESUME")
  | MenhirInterpreter.T T_RESTRICTED -> (fun _ -> "RESTRICTED")
  | MenhirInterpreter.T T_RESIZABLE -> (fun _ -> "RESIZABLE")
  | MenhirInterpreter.T T_RESIDENT -> (fun _ -> "RESIDENT")
  | MenhirInterpreter.T T_RESET_TABS -> (fun _ -> "RESET_TABS")
  | MenhirInterpreter.T T_RESET_SET_LOCATOR -> (fun _ -> "RESET_SET_LOCATOR")
  | MenhirInterpreter.T T_RESET_LIST -> (fun _ -> "RESET_LIST")
  | MenhirInterpreter.T T_RESET_GRID -> (fun _ -> "RESET_GRID")
  | MenhirInterpreter.T T_RESET -> (fun _ -> "RESET")
  | MenhirInterpreter.T T_RESERVE -> (fun _ -> "RESERVE")
  | MenhirInterpreter.T T_RERUN -> (fun _ -> "RERUN")
  | MenhirInterpreter.T T_REREAD -> (fun _ -> "REREAD")
  | MenhirInterpreter.T T_REQUIRED -> (fun _ -> "REQUIRED")
  | MenhirInterpreter.T T_REPOSITORY -> (fun _ -> "REPOSITORY")
  | MenhirInterpreter.T T_REPORTS -> (fun _ -> "REPORTS")
  | MenhirInterpreter.T T_REPORTING -> (fun _ -> "REPORTING")
  | MenhirInterpreter.T T_REPORT -> (fun _ -> "REPORT")
  | MenhirInterpreter.T T_REPLACING -> (fun _ -> "REPLACING")
  | MenhirInterpreter.T T_REPLACED -> (fun _ -> "REPLACED")
  | MenhirInterpreter.T T_REPLACE -> (fun _ -> "REPLACE")
  | MenhirInterpreter.T T_REPEATED -> (fun _ -> "REPEATED")
  | MenhirInterpreter.T T_REORG_CRITERIA -> (fun _ -> "REORG_CRITERIA")
  | MenhirInterpreter.T T_RENAMES -> (fun _ -> "RENAMES")
  | MenhirInterpreter.T T_REMOVAL -> (fun _ -> "REMOVAL")
  | MenhirInterpreter.T T_REMARKS -> (fun _ -> "REMARKS")
  | MenhirInterpreter.T T_REMAINDER -> (fun _ -> "REMAINDER")
  | MenhirInterpreter.T T_RELOAD -> (fun _ -> "RELOAD")
  | MenhirInterpreter.T T_RELEASE -> (fun _ -> "RELEASE")
  | MenhirInterpreter.T T_RELATIVE -> (fun _ -> "RELATIVE")
  | MenhirInterpreter.T T_RELATION -> (fun _ -> "RELATION")
  | MenhirInterpreter.T T_REGION_COLOR -> (fun _ -> "REGION_COLOR")
  | MenhirInterpreter.T T_REFRESH -> (fun _ -> "REFRESH")
  | MenhirInterpreter.T T_REFERENCES -> (fun _ -> "REFERENCES")
  | MenhirInterpreter.T T_REFERENCE -> (fun _ -> "REFERENCE")
  | MenhirInterpreter.T T_REEL -> (fun _ -> "REEL")
  | MenhirInterpreter.T T_REDEFINITION -> (fun _ -> "REDEFINITION")
  | MenhirInterpreter.T T_REDEFINES -> (fun _ -> "REDEFINES")
  | MenhirInterpreter.T T_RECURSIVE -> (fun _ -> "RECURSIVE")
  | MenhirInterpreter.T T_RECORD_TO_DELETE -> (fun _ -> "RECORD_TO_DELETE")
  | MenhirInterpreter.T T_RECORD_TO_ADD -> (fun _ -> "RECORD_TO_ADD")
  | MenhirInterpreter.T T_RECORD_POSITION -> (fun _ -> "RECORD_POSITION")
  | MenhirInterpreter.T T_RECORD_OVERFLOW -> (fun _ -> "RECORD_OVERFLOW")
  | MenhirInterpreter.T T_RECORD_DATA -> (fun _ -> "RECORD_DATA")
  | MenhirInterpreter.T T_RECORDS -> (fun _ -> "RECORDS")
  | MenhirInterpreter.T T_RECORDING -> (fun _ -> "RECORDING")
  | MenhirInterpreter.T T_RECORD -> (fun _ -> "RECORD")
  | MenhirInterpreter.T T_RECEIVED -> (fun _ -> "RECEIVED")
  | MenhirInterpreter.T T_RECEIVE -> (fun _ -> "RECEIVE")
  | MenhirInterpreter.T T_READ_ONLY -> (fun _ -> "READ_ONLY")
  | MenhirInterpreter.T T_READY -> (fun _ -> "READY")
  | MenhirInterpreter.T T_READING -> (fun _ -> "READING")
  | MenhirInterpreter.T T_READERS -> (fun _ -> "READERS")
  | MenhirInterpreter.T T_READ -> (fun _ -> "READ")
  | MenhirInterpreter.T T_RD -> (fun _ -> "RD")
  | MenhirInterpreter.T T_RANGE -> (fun _ -> "RANGE")
  | MenhirInterpreter.T T_RANDOM -> (fun _ -> "RANDOM")
  | MenhirInterpreter.T T_RAISING -> (fun _ -> "RAISING")
  | MenhirInterpreter.T T_RAISED -> (fun _ -> "RAISED")
  | MenhirInterpreter.T T_RAISE -> (fun _ -> "RAISE")
  | MenhirInterpreter.T T_RADIO_BUTTON -> (fun _ -> "RADIO_BUTTON")
  | MenhirInterpreter.T T_QUOTE -> (fun _ -> "QUOTE")
  | MenhirInterpreter.T T_QUEUED -> (fun _ -> "QUEUED")
  | MenhirInterpreter.T T_QUEUE -> (fun _ -> "QUEUE")
  | MenhirInterpreter.T T_QUERY_INDEX -> (fun _ -> "QUERY_INDEX")
  | MenhirInterpreter.T T_PUSH_BUTTON -> (fun _ -> "PUSH_BUTTON")
  | MenhirInterpreter.T T_PURGE -> (fun _ -> "PURGE")
  | MenhirInterpreter.T T_PUBLIC -> (fun _ -> "PUBLIC")
  | MenhirInterpreter.T T_PROTOTYPE -> (fun _ -> "PROTOTYPE")
  | MenhirInterpreter.T T_PROTECTED -> (fun _ -> "PROTECTED")
  | MenhirInterpreter.T T_PROPERTY -> (fun _ -> "PROPERTY")
  | MenhirInterpreter.T T_PROPERTIES -> (fun _ -> "PROPERTIES")
  | MenhirInterpreter.T T_PROMPT -> (fun _ -> "PROMPT")
  | MenhirInterpreter.T T_PROHIBITED -> (fun _ -> "PROHIBITED")
  | MenhirInterpreter.T T_PROGRESS -> (fun _ -> "PROGRESS")
  | MenhirInterpreter.T T_PROGRAM_POINTER -> (fun _ -> "PROGRAM_POINTER")
  | MenhirInterpreter.T T_PROGRAM_ID -> (fun _ -> "PROGRAM_ID")
  | MenhirInterpreter.T T_PROGRAM -> (fun _ -> "PROGRAM")
  | MenhirInterpreter.T T_PROCESS_AREA -> (fun _ -> "PROCESS_AREA")
  | MenhirInterpreter.T T_PROCESSING -> (fun _ -> "PROCESSING")
  | MenhirInterpreter.T T_PROCESS -> (fun _ -> "PROCESS")
  | MenhirInterpreter.T T_PROCEED -> (fun _ -> "PROCEED")
  | MenhirInterpreter.T T_PROCEDURE_POINTER -> (fun _ -> "PROCEDURE_POINTER")
  | MenhirInterpreter.T T_PROCEDURE_NAME -> (fun _ -> "PROCEDURE_NAME")
  | MenhirInterpreter.T T_PROCEDURES -> (fun _ -> "PROCEDURES")
  | MenhirInterpreter.T T_PROCEDURE -> (fun _ -> "PROCEDURE")
  | MenhirInterpreter.T T_PRIVATE -> (fun _ -> "PRIVATE")
  | MenhirInterpreter.T T_PRIORITY -> (fun _ -> "PRIORITY")
  | MenhirInterpreter.T T_PRIOR -> (fun _ -> "PRIOR")
  | MenhirInterpreter.T T_PRINT_PREVIEW -> (fun _ -> "PRINT_PREVIEW")
  | MenhirInterpreter.T T_PRINT_NO_PROMPT -> (fun _ -> "PRINT_NO_PROMPT")
  | MenhirInterpreter.T T_PRINT_CONTROL -> (fun _ -> "PRINT_CONTROL")
  | MenhirInterpreter.T T_PRINTING -> (fun _ -> "PRINTING")
  | MenhirInterpreter.T T_PRINTER_1 -> (fun _ -> "PRINTER_1")
  | MenhirInterpreter.T T_PRINTER -> (fun _ -> "PRINTER")
  | MenhirInterpreter.T T_PRINT -> (fun _ -> "PRINT")
  | MenhirInterpreter.T T_PRIMARY -> (fun _ -> "PRIMARY")
  | MenhirInterpreter.T T_PREVIOUS -> (fun _ -> "PREVIOUS")
  | MenhirInterpreter.T T_PRESENT -> (fun _ -> "PRESENT")
  | MenhirInterpreter.T T_PREFIXING -> (fun _ -> "PREFIXING")
  | MenhirInterpreter.T T_PREFIXED -> (fun _ -> "PREFIXED")
  | MenhirInterpreter.T T_POSITIVE -> (fun _ -> "POSITIVE")
  | MenhirInterpreter.T T_POSITION_SHIFT -> (fun _ -> "POSITION_SHIFT")
  | MenhirInterpreter.T T_POSITIONING -> (fun _ -> "POSITIONING")
  | MenhirInterpreter.T T_POSITION -> (fun _ -> "POSITION")
  | MenhirInterpreter.T T_POS -> (fun _ -> "POS")
  | MenhirInterpreter.T T_POP_UP -> (fun _ -> "POP_UP")
  | MenhirInterpreter.T T_POINTER_32 -> (fun _ -> "POINTER_32")
  | MenhirInterpreter.T T_POINTER -> (fun _ -> "POINTER")
  | MenhirInterpreter.T T_PLUS_SIGN -> (fun _ -> "+")
  | MenhirInterpreter.T T_PLUS -> (fun _ -> "PLUS")
  | MenhirInterpreter.T T_PLACEMENT -> (fun _ -> "PLACEMENT")
  | MenhirInterpreter.T T_PIXEL -> (fun _ -> "PIXEL")
  | MenhirInterpreter.T T_PICTURE_STRING -> (fun _ -> "PICTURE_STRING")
  | MenhirInterpreter.T T_PICTURE -> (fun _ -> "PICTURE")
  | MenhirInterpreter.T T_PHYSICAL -> (fun _ -> "PHYSICAL")
  | MenhirInterpreter.T T_PH -> (fun _ -> "PH")
  | MenhirInterpreter.T T_PF -> (fun _ -> "PF")
  | MenhirInterpreter.T T_PERMANENT -> (fun _ -> "PERMANENT")
  | MenhirInterpreter.T T_PERIOD -> (fun _ -> ".")
  | MenhirInterpreter.T T_PERFORM -> (fun _ -> "PERFORM")
  | MenhirInterpreter.T T_PASSWORD -> (fun _ -> "PASSWORD")
  | MenhirInterpreter.T T_PASCAL -> (fun _ -> "PASCAL")
  | MenhirInterpreter.T T_PARSE -> (fun _ -> "PARSE")
  | MenhirInterpreter.T T_PARENT -> (fun _ -> "PARENT")
  | MenhirInterpreter.T T_PARAGRAPH -> (fun _ -> "PARAGRAPH")
  | MenhirInterpreter.T T_PANEL_WIDTHS -> (fun _ -> "PANEL_WIDTHS")
  | MenhirInterpreter.T T_PANEL_TEXT -> (fun _ -> "PANEL_TEXT")
  | MenhirInterpreter.T T_PANEL_STYLE -> (fun _ -> "PANEL_STYLE")
  | MenhirInterpreter.T T_PANEL_INDEX -> (fun _ -> "PANEL_INDEX")
  | MenhirInterpreter.T T_PAGE_SIZE -> (fun _ -> "PAGE_SIZE")
  | MenhirInterpreter.T T_PAGE_SETUP -> (fun _ -> "PAGE_SETUP")
  | MenhirInterpreter.T T_PAGE_COUNTER -> (fun _ -> "PAGE_COUNTER")
  | MenhirInterpreter.T T_PAGED -> (fun _ -> "PAGED")
  | MenhirInterpreter.T T_PAGE -> (fun _ -> "PAGE")
  | MenhirInterpreter.T T_PADDING -> (fun _ -> "PADDING")
  | MenhirInterpreter.T T_PACKED_DECIMAL -> (fun _ -> "PACKED_DECIMAL")
  | MenhirInterpreter.T T_O_FILL -> (fun _ -> "O_FILL")
  | MenhirInterpreter.T T_OVERRIDING -> (fun _ -> "OVERRIDING")
  | MenhirInterpreter.T T_OVERRIDE -> (fun _ -> "OVERRIDE")
  | MenhirInterpreter.T T_OVERLINE -> (fun _ -> "OVERLINE")
  | MenhirInterpreter.T T_OVERLAP_TOP -> (fun _ -> "OVERLAP_TOP")
  | MenhirInterpreter.T T_OVERLAP_LEFT -> (fun _ -> "OVERLAP_LEFT")
  | MenhirInterpreter.T T_OVERLAPPED -> (fun _ -> "OVERLAPPED")
  | MenhirInterpreter.T T_OVERFLOW -> (fun _ -> "OVERFLOW")
  | MenhirInterpreter.T T_OUTPUT -> (fun _ -> "OUTPUT")
  | MenhirInterpreter.T T_OTHERWISE -> (fun _ -> "OTHERWISE")
  | MenhirInterpreter.T T_OTHERS -> (fun _ -> "OTHERS")
  | MenhirInterpreter.T T_OTHER -> (fun _ -> "OTHER")
  | MenhirInterpreter.T T_ORGANIZATION -> (fun _ -> "ORGANIZATION")
  | MenhirInterpreter.T T_ORDER -> (fun _ -> "ORDER")
  | MenhirInterpreter.T T_OR -> (fun _ -> "OR")
  | MenhirInterpreter.T T_OPTIONS -> (fun _ -> "OPTIONS")
  | MenhirInterpreter.T T_OPTIONAL -> (fun _ -> "OPTIONAL")
  | MenhirInterpreter.T T_OPERATIONAL -> (fun _ -> "OPERATIONAL")
  | MenhirInterpreter.T T_OPEN -> (fun _ -> "OPEN")
  | MenhirInterpreter.T T_OOSTACKPTR -> (fun _ -> "OOSTACKPTR")
  | MenhirInterpreter.T T_ON_SIZE_ERROR -> (fun _ -> "ON_SIZE_ERROR")
  | MenhirInterpreter.T T_ON_OVERFLOW -> (fun _ -> "ON_OVERFLOW")
  | MenhirInterpreter.T T_ON_EXCEPTION -> (fun _ -> "ON_EXCEPTION")
  | MenhirInterpreter.T T_ONLY -> (fun _ -> "ONLY")
  | MenhirInterpreter.T T_ON -> (fun _ -> "ON")
  | MenhirInterpreter.T T_OMITTED -> (fun _ -> "OMITTED")
  | MenhirInterpreter.T T_OK_BUTTON -> (fun _ -> "OK_BUTTON")
  | MenhirInterpreter.T T_OFF -> (fun _ -> "OFF")
  | MenhirInterpreter.T T_OF -> (fun _ -> "OF")
  | MenhirInterpreter.T T_OCCURS -> (fun _ -> "OCCURS")
  | MenhirInterpreter.T T_OBJECT_STORAGE -> (fun _ -> "OBJECT_STORAGE")
  | MenhirInterpreter.T T_OBJECT_REFERENCE -> (fun _ -> "OBJECT_REFERENCE")
  | MenhirInterpreter.T T_OBJECT_PROGRAM -> (fun _ -> "OBJECT_PROGRAM")
  | MenhirInterpreter.T T_OBJECT_ID -> (fun _ -> "OBJECT_ID")
  | MenhirInterpreter.T T_OBJECT_COMPUTER -> (fun _ -> "OBJECT_COMPUTER")
  | MenhirInterpreter.T T_OBJECT -> (fun _ -> "OBJECT")
  | MenhirInterpreter.T T_NUM_ROW_HEADINGS -> (fun _ -> "NUM_ROW_HEADINGS")
  | MenhirInterpreter.T T_NUM_ROWS -> (fun _ -> "NUM_ROWS")
  | MenhirInterpreter.T T_NUM_COL_HEADINGS -> (fun _ -> "NUM_COL_HEADINGS")
  | MenhirInterpreter.T T_NUMERIC_FILL -> (fun _ -> "NUMERIC_FILL")
  | MenhirInterpreter.T T_NUMERIC_EDITED -> (fun _ -> "NUMERIC_EDITED")
  | MenhirInterpreter.T T_NUMERIC -> (fun _ -> "NUMERIC")
  | MenhirInterpreter.T T_NUMBERS -> (fun _ -> "NUMBERS")
  | MenhirInterpreter.T T_NUMBER -> (fun _ -> "NUMBER")
  | MenhirInterpreter.T T_NULLS -> (fun _ -> "NULLS")
  | MenhirInterpreter.T T_NULLIT -> (fun _ -> "NULLIT")
  | MenhirInterpreter.T T_NULL -> (fun _ -> "NULL")
  | MenhirInterpreter.T T_NO_UPDOWN -> (fun _ -> "NO_UPDOWN")
  | MenhirInterpreter.T T_NO_TAB -> (fun _ -> "NO_TAB")
  | MenhirInterpreter.T T_NO_SEARCH -> (fun _ -> "NO_SEARCH")
  | MenhirInterpreter.T T_NO_KEY_LETTER -> (fun _ -> "NO_KEY_LETTER")
  | MenhirInterpreter.T T_NO_GROUP_TAB -> (fun _ -> "NO_GROUP_TAB")
  | MenhirInterpreter.T T_NO_FOCUS -> (fun _ -> "NO_FOCUS")
  | MenhirInterpreter.T T_NO_F4 -> (fun _ -> "NO_F4")
  | MenhirInterpreter.T T_NO_ECHO -> (fun _ -> "NO_ECHO")
  | MenhirInterpreter.T T_NO_DIVIDERS -> (fun _ -> "NO_DIVIDERS")
  | MenhirInterpreter.T T_NO_DATA -> (fun _ -> "NO_DATA")
  | MenhirInterpreter.T T_NO_CLOSE -> (fun _ -> "NO_CLOSE")
  | MenhirInterpreter.T T_NO_CELL_DRAG -> (fun _ -> "NO_CELL_DRAG")
  | MenhirInterpreter.T T_NO_BOX -> (fun _ -> "NO_BOX")
  | MenhirInterpreter.T T_NO_AUTO_DEFAULT -> (fun _ -> "NO_AUTO_DEFAULT")
  | MenhirInterpreter.T T_NO_AUTOSEL -> (fun _ -> "NO_AUTOSEL")
  | MenhirInterpreter.T T_NOT_ON_SIZE_ERROR -> (fun _ -> "NOT_ON_SIZE_ERROR")
  | MenhirInterpreter.T T_NOT_ON_OVERFLOW -> (fun _ -> "NOT_ON_OVERFLOW")
  | MenhirInterpreter.T T_NOT_ON_EXCEPTION -> (fun _ -> "NOT_ON_EXCEPTION")
  | MenhirInterpreter.T T_NOT_INVALID_KEY -> (fun _ -> "NOT_INVALID_KEY")
  | MenhirInterpreter.T T_NOT_AT_EOP -> (fun _ -> "NOT_AT_EOP")
  | MenhirInterpreter.T T_NOT_AT_END -> (fun _ -> "NOT_AT_END")
  | MenhirInterpreter.T T_NOTIFY_SELCHANGE -> (fun _ -> "NOTIFY_SELCHANGE")
  | MenhirInterpreter.T T_NOTIFY_DBLCLICK -> (fun _ -> "NOTIFY_DBLCLICK")
  | MenhirInterpreter.T T_NOTIFY_CHANGE -> (fun _ -> "NOTIFY_CHANGE")
  | MenhirInterpreter.T T_NOTIFY -> (fun _ -> "NOTIFY")
  | MenhirInterpreter.T T_NOTHING -> (fun _ -> "NOTHING")
  | MenhirInterpreter.T T_NOTE -> (fun _ -> "NOTE")
  | MenhirInterpreter.T T_NOTAB -> (fun _ -> "NOTAB")
  | MenhirInterpreter.T T_NOT -> (fun _ -> "NOT")
  | MenhirInterpreter.T T_NORMAL -> (fun _ -> "NORMAL")
  | MenhirInterpreter.T T_NONNUMERIC -> (fun _ -> "NONNUMERIC")
  | MenhirInterpreter.T T_NONE -> (fun _ -> "NONE")
  | MenhirInterpreter.T T_NOMINAL -> (fun _ -> "NOMINAL")
  | MenhirInterpreter.T T_NO -> (fun _ -> "NO")
  | MenhirInterpreter.T T_NEXT_PAGE -> (fun _ -> "NEXT_PAGE")
  | MenhirInterpreter.T T_NEXT_ITEM -> (fun _ -> "NEXT_ITEM")
  | MenhirInterpreter.T T_NEXT -> (fun _ -> "NEXT")
  | MenhirInterpreter.T T_NEW -> (fun _ -> "NEW")
  | MenhirInterpreter.T T_NET_EVENT_LIST -> (fun _ -> "NET_EVENT_LIST")
  | MenhirInterpreter.T T_NESTED -> (fun _ -> "NESTED")
  | MenhirInterpreter.T T_NEGATIVE -> (fun _ -> "NEGATIVE")
  | MenhirInterpreter.T T_NEAREST_TO_ZERO -> (fun _ -> "NEAREST_TO_ZERO")
  | MenhirInterpreter.T T_NEAREST_TOWARD_ZERO -> (fun _ -> "NEAREST_TOWARD_ZERO")
  | MenhirInterpreter.T T_NEAREST_EVEN -> (fun _ -> "NEAREST_EVEN")
  | MenhirInterpreter.T T_NEAREST_AWAY_FROM_ZERO -> (fun _ -> "NEAREST_AWAY_FROM_ZERO")
  | MenhirInterpreter.T T_NE -> (fun _ -> "<>")
  | MenhirInterpreter.T T_NCLOB -> (fun _ -> "NCLOB")
  | MenhirInterpreter.T T_NCHAR -> (fun _ -> "NCHAR")
  | MenhirInterpreter.T T_NAVIGATE_URL -> (fun _ -> "NAVIGATE_URL")
  | MenhirInterpreter.T T_NATLIT -> (fun _ -> "NATLIT")
  | MenhirInterpreter.T T_NATIVE -> (fun _ -> "NATIVE")
  | MenhirInterpreter.T T_NATIONAL_EDITED -> (fun _ -> "NATIONAL_EDITED")
  | MenhirInterpreter.T T_NATIONAL -> (fun _ -> "NATIONAL")
  | MenhirInterpreter.T T_NAT -> (fun _ -> "NAT")
  | MenhirInterpreter.T T_NAMESPACE_PREFIX -> (fun _ -> "NAMESPACE_PREFIX")
  | MenhirInterpreter.T T_NAMESPACE -> (fun _ -> "NAMESPACE")
  | MenhirInterpreter.T T_NAMED -> (fun _ -> "NAMED")
  | MenhirInterpreter.T T_NAME -> (fun _ -> "NAME")
  | MenhirInterpreter.T T_MUTEX_POINTER -> (fun _ -> "MUTEX_POINTER")
  | MenhirInterpreter.T T_MULTIPLY -> (fun _ -> "MULTIPLY")
  | MenhirInterpreter.T T_MULTIPLE -> (fun _ -> "MULTIPLE")
  | MenhirInterpreter.T T_MULTILINE -> (fun _ -> "MULTILINE")
  | MenhirInterpreter.T T_MOVE -> (fun _ -> "MOVE")
  | MenhirInterpreter.T T_MORE_LABELS -> (fun _ -> "MORE_LABELS")
  | MenhirInterpreter.T T_MONITOR_POINTER -> (fun _ -> "MONITOR_POINTER")
  | MenhirInterpreter.T T_MODULES -> (fun _ -> "MODULES")
  | MenhirInterpreter.T T_MODULE -> (fun _ -> "MODULE")
  | MenhirInterpreter.T T_MODIFY -> (fun _ -> "MODIFY")
  | MenhirInterpreter.T T_MODIFIED -> (fun _ -> "MODIFIED")
  | MenhirInterpreter.T T_MODELESS -> (fun _ -> "MODELESS")
  | MenhirInterpreter.T T_MODE -> (fun _ -> "MODE")
  | MenhirInterpreter.T T_MODAL -> (fun _ -> "MODAL")
  | MenhirInterpreter.T T_MIN_WIDTH -> (fun _ -> "MIN_WIDTH")
  | MenhirInterpreter.T T_MIN_VALUE -> (fun _ -> "MIN_VALUE")
  | MenhirInterpreter.T T_MIN_VAL -> (fun _ -> "MIN_VAL")
  | MenhirInterpreter.T T_MIN_SIZE -> (fun _ -> "MIN_SIZE")
  | MenhirInterpreter.T T_MIN_LINES -> (fun _ -> "MIN_LINES")
  | MenhirInterpreter.T T_MIN_HEIGHT -> (fun _ -> "MIN_HEIGHT")
  | MenhirInterpreter.T T_MINUS -> (fun _ -> "MINUS")
  | MenhirInterpreter.T T_MICROSECOND_TIME -> (fun _ -> "MICROSECOND_TIME")
  | MenhirInterpreter.T T_METHOD_ID -> (fun _ -> "METHOD_ID")
  | MenhirInterpreter.T T_METHOD -> (fun _ -> "METHOD")
  | MenhirInterpreter.T T_META_CLASS -> (fun _ -> "META_CLASS")
  | MenhirInterpreter.T T_MESSAGE_TAG -> (fun _ -> "MESSAGE_TAG")
  | MenhirInterpreter.T T_MESSAGES -> (fun _ -> "MESSAGES")
  | MenhirInterpreter.T T_MESSAGE -> (fun _ -> "MESSAGE")
  | MenhirInterpreter.T T_MERGE -> (fun _ -> "MERGE")
  | MenhirInterpreter.T T_MENU -> (fun _ -> "MENU")
  | MenhirInterpreter.T T_MEMORY -> (fun _ -> "MEMORY")
  | MenhirInterpreter.T T_MEDIUM_FONT -> (fun _ -> "MEDIUM_FONT")
  | MenhirInterpreter.T T_MDI_FRAME -> (fun _ -> "MDI_FRAME")
  | MenhirInterpreter.T T_MDI_CHILD -> (fun _ -> "MDI_CHILD")
  | MenhirInterpreter.T T_MAX_WIDTH -> (fun _ -> "MAX_WIDTH")
  | MenhirInterpreter.T T_MAX_VALUE -> (fun _ -> "MAX_VALUE")
  | MenhirInterpreter.T T_MAX_VAL -> (fun _ -> "MAX_VAL")
  | MenhirInterpreter.T T_MAX_TEXT -> (fun _ -> "MAX_TEXT")
  | MenhirInterpreter.T T_MAX_SIZE -> (fun _ -> "MAX_SIZE")
  | MenhirInterpreter.T T_MAX_PROGRESS -> (fun _ -> "MAX_PROGRESS")
  | MenhirInterpreter.T T_MAX_LINES -> (fun _ -> "MAX_LINES")
  | MenhirInterpreter.T T_MAX_HEIGHT -> (fun _ -> "MAX_HEIGHT")
  | MenhirInterpreter.T T_MASTER_INDEX -> (fun _ -> "MASTER_INDEX")
  | MenhirInterpreter.T T_MASS_UPDATE -> (fun _ -> "MASS_UPDATE")
  | MenhirInterpreter.T T_MANUAL -> (fun _ -> "MANUAL")
  | MenhirInterpreter.T T_MAGNETIC_TAPE -> (fun _ -> "MAGNETIC_TAPE")
  | MenhirInterpreter.T T_LT -> (fun _ -> "<")
  | MenhirInterpreter.T T_LPAR -> (fun _ -> "(")
  | MenhirInterpreter.T T_LOW_VALUE -> (fun _ -> "LOW_VALUE")
  | MenhirInterpreter.T T_LOW_COLOR -> (fun _ -> "LOW_COLOR")
  | MenhirInterpreter.T T_LOWLIGHT -> (fun _ -> "LOWLIGHT")
  | MenhirInterpreter.T T_LOWEST_VALUE -> (fun _ -> "LOWEST_VALUE")
  | MenhirInterpreter.T T_LOWERED -> (fun _ -> "LOWERED")
  | MenhirInterpreter.T T_LOWER -> (fun _ -> "LOWER")
  | MenhirInterpreter.T T_LOW -> (fun _ -> "LOW")
  | MenhirInterpreter.T T_LONG_VARCHAR -> (fun _ -> "LONG_VARCHAR")
  | MenhirInterpreter.T T_LONG_VARBINARY -> (fun _ -> "LONG_VARBINARY")
  | MenhirInterpreter.T T_LONG_DATE -> (fun _ -> "LONG_DATE")
  | MenhirInterpreter.T T_LOCK_HOLDING -> (fun _ -> "LOCK_HOLDING")
  | MenhirInterpreter.T T_LOCKS -> (fun _ -> "LOCKS")
  | MenhirInterpreter.T T_LOCK -> (fun _ -> "LOCK")
  | MenhirInterpreter.T T_LOCATION -> (fun _ -> "LOCATION")
  | MenhirInterpreter.T T_LOCAL_STORAGE -> (fun _ -> "LOCAL_STORAGE")
  | MenhirInterpreter.T T_LOCALE -> (fun _ -> "LOCALE")
  | MenhirInterpreter.T T_LOC -> (fun _ -> "LOC")
  | MenhirInterpreter.T T_LM_RESIZE -> (fun _ -> "LM_RESIZE")
  | MenhirInterpreter.T T_LIST_BOX -> (fun _ -> "LIST_BOX")
  | MenhirInterpreter.T T_LINKAGE -> (fun _ -> "LINKAGE")
  | MenhirInterpreter.T T_LINK -> (fun _ -> "LINK")
  | MenhirInterpreter.T T_LINE_SEQUENTIAL -> (fun _ -> "LINE_SEQUENTIAL")
  | MenhirInterpreter.T T_LINE_COUNTER -> (fun _ -> "LINE_COUNTER")
  | MenhirInterpreter.T T_LINES_PER_PAGE -> (fun _ -> "LINES_PER_PAGE")
  | MenhirInterpreter.T T_LINES_AT_ROOT -> (fun _ -> "LINES_AT_ROOT")
  | MenhirInterpreter.T T_LINES -> (fun _ -> "LINES")
  | MenhirInterpreter.T T_LINE -> (fun _ -> "LINE")
  | MenhirInterpreter.T T_LINAGE_COUNTER -> (fun _ -> "LINAGE_COUNTER")
  | MenhirInterpreter.T T_LINAGE -> (fun _ -> "LINAGE")
  | MenhirInterpreter.T T_LIN -> (fun _ -> "LIN")
  | MenhirInterpreter.T T_LIMITS -> (fun _ -> "LIMITS")
  | MenhirInterpreter.T T_LIMIT -> (fun _ -> "LIMIT")
  | MenhirInterpreter.T T_LIKE -> (fun _ -> "LIKE")
  | MenhirInterpreter.T T_LIBRARY -> (fun _ -> "LIBRARY")
  | MenhirInterpreter.T T_LESS -> (fun _ -> "LESS")
  | MenhirInterpreter.T T_LENGTH -> (fun _ -> "LENGTH")
  | MenhirInterpreter.T T_LEFT_TEXT -> (fun _ -> "LEFT_TEXT")
  | MenhirInterpreter.T T_LEFT_JUSTIFY -> (fun _ -> "LEFT_JUSTIFY")
  | MenhirInterpreter.T T_LEFTLINE -> (fun _ -> "LEFTLINE")
  | MenhirInterpreter.T T_LEFT -> (fun _ -> "LEFT")
  | MenhirInterpreter.T T_LEAVE -> (fun _ -> "LEAVE")
  | MenhirInterpreter.T T_LEADING_SHIFT -> (fun _ -> "LEADING_SHIFT")
  | MenhirInterpreter.T T_LEADING -> (fun _ -> "LEADING")
  | MenhirInterpreter.T T_LE -> (fun _ -> "<=")
  | MenhirInterpreter.T T_LC_TIME -> (fun _ -> "LC_TIME")
  | MenhirInterpreter.T T_LC_NUMERIC -> (fun _ -> "LC_NUMERIC")
  | MenhirInterpreter.T T_LC_MONETARY -> (fun _ -> "LC_MONETARY")
  | MenhirInterpreter.T T_LC_MESSAGES -> (fun _ -> "LC_MESSAGES")
  | MenhirInterpreter.T T_LC_CTYPE -> (fun _ -> "LC_CTYPE")
  | MenhirInterpreter.T T_LC_COLLATE -> (fun _ -> "LC_COLLATE")
  | MenhirInterpreter.T T_LC_ALL -> (fun _ -> "LC_ALL")
  | MenhirInterpreter.T T_LAYOUT_MANAGER -> (fun _ -> "LAYOUT_MANAGER")
  | MenhirInterpreter.T T_LAYOUT_DATA -> (fun _ -> "LAYOUT_DATA")
  | MenhirInterpreter.T T_LAST_ROW -> (fun _ -> "LAST_ROW")
  | MenhirInterpreter.T T_LAST -> (fun _ -> "LAST")
  | MenhirInterpreter.T T_LARGE_OFFSET -> (fun _ -> "LARGE_OFFSET")
  | MenhirInterpreter.T T_LARGE_FONT -> (fun _ -> "LARGE_FONT")
  | MenhirInterpreter.T T_LABEL_OFFSET -> (fun _ -> "LABEL_OFFSET")
  | MenhirInterpreter.T T_LABEL -> (fun _ -> "LABEL")
  | MenhirInterpreter.T T_KEY_LOCATION -> (fun _ -> "KEY_LOCATION")
  | MenhirInterpreter.T T_KEYED -> (fun _ -> "KEYED")
  | MenhirInterpreter.T T_KEYBOARD -> (fun _ -> "KEYBOARD")
  | MenhirInterpreter.T T_KEY -> (fun _ -> "KEY")
  | MenhirInterpreter.T T_KEPT -> (fun _ -> "KEPT")
  | MenhirInterpreter.T T_KANJI -> (fun _ -> "KANJI")
  | MenhirInterpreter.T T_JUSTIFIED -> (fun _ -> "JUSTIFIED")
  | MenhirInterpreter.T T_JSON_STATUS -> (fun _ -> "JSON_STATUS")
  | MenhirInterpreter.T T_JSON_CODE -> (fun _ -> "JSON_CODE")
  | MenhirInterpreter.T T_JSON -> (fun _ -> "JSON")
  | MenhirInterpreter.T T_JOINING -> (fun _ -> "JOINING")
  | MenhirInterpreter.T T_JNIENVPTR -> (fun _ -> "JNIENVPTR")
  | MenhirInterpreter.T T_JAVA -> (fun _ -> "JAVA")
  | MenhirInterpreter.T T_JAPANESE -> (fun _ -> "JAPANESE")
  | MenhirInterpreter.T T_I_O_CONTROL -> (fun _ -> "I_O_CONTROL")
  | MenhirInterpreter.T T_I_O -> (fun _ -> "I_O")
  | MenhirInterpreter.T T_ITEM_VALUE -> (fun _ -> "ITEM_VALUE")
  | MenhirInterpreter.T T_ITEM_TO_EMPTY -> (fun _ -> "ITEM_TO_EMPTY")
  | MenhirInterpreter.T T_ITEM_TO_DELETE -> (fun _ -> "ITEM_TO_DELETE")
  | MenhirInterpreter.T T_ITEM_TO_ADD -> (fun _ -> "ITEM_TO_ADD")
  | MenhirInterpreter.T T_ITEM_TEXT -> (fun _ -> "ITEM_TEXT")
  | MenhirInterpreter.T T_ITEM_ID -> (fun _ -> "ITEM_ID")
  | MenhirInterpreter.T T_ITEM_BOLD -> (fun _ -> "ITEM_BOLD")
  | MenhirInterpreter.T T_ITEM -> (fun _ -> "ITEM")
  | MenhirInterpreter.T T_IS_TYPEDEF -> (fun _ -> "IS_TYPEDEF")
  | MenhirInterpreter.T T_IS_GLOBAL -> (fun _ -> "IS_GLOBAL")
  | MenhirInterpreter.T T_IS_EXTERNAL -> (fun _ -> "IS_EXTERNAL")
  | MenhirInterpreter.T T_IS -> (fun _ -> "IS")
  | MenhirInterpreter.T T_IN_ARITHMETIC_RANGE -> (fun _ -> "IN_ARITHMETIC_RANGE")
  | MenhirInterpreter.T T_INVOKING -> (fun _ -> "INVOKING")
  | MenhirInterpreter.T T_INVOKED -> (fun _ -> "INVOKED")
  | MenhirInterpreter.T T_INVOKE -> (fun _ -> "INVOKE")
  | MenhirInterpreter.T T_INVALID_KEY -> (fun _ -> "INVALID_KEY")
  | MenhirInterpreter.T T_INVALID -> (fun _ -> "INVALID")
  | MenhirInterpreter.T T_INTRINSIC -> (fun _ -> "INTRINSIC")
  | MenhirInterpreter.T T_INTO -> (fun _ -> "INTO")
  | MenhirInterpreter.T T_INTERVENING_ -> (fun _ -> "INTERVENING_")
  | MenhirInterpreter.T T_INTERVAL_TIMER -> (fun _ -> "INTERVAL_TIMER")
  | MenhirInterpreter.T T_INTERMEDIATE -> (fun _ -> "INTERMEDIATE")
  | MenhirInterpreter.T T_INTERFACE_ID -> (fun _ -> "INTERFACE_ID")
  | MenhirInterpreter.T T_INTERFACE -> (fun _ -> "INTERFACE")
  | MenhirInterpreter.T T_INSTANCE -> (fun _ -> "INSTANCE")
  | MenhirInterpreter.T T_INSTALLATION -> (fun _ -> "INSTALLATION")
  | MenhirInterpreter.T T_INSPECT -> (fun _ -> "INSPECT")
  | MenhirInterpreter.T T_INSERT_ROWS -> (fun _ -> "INSERT_ROWS")
  | MenhirInterpreter.T T_INSERTION_INDEX -> (fun _ -> "INSERTION_INDEX")
  | MenhirInterpreter.T T_INSERT -> (fun _ -> "INSERT")
  | MenhirInterpreter.T T_INQUIRE -> (fun _ -> "INQUIRE")
  | MenhirInterpreter.T T_INPUT_OUTPUT -> (fun _ -> "INPUT_OUTPUT")
  | MenhirInterpreter.T T_INPUT -> (fun _ -> "INPUT")
  | MenhirInterpreter.T T_INITIATE -> (fun _ -> "INITIATE")
  | MenhirInterpreter.T T_INITIAL_VALUE -> (fun _ -> "INITIAL_VALUE")
  | MenhirInterpreter.T T_INITIALIZED -> (fun _ -> "INITIALIZED")
  | MenhirInterpreter.T T_INITIALIZE -> (fun _ -> "INITIALIZE")
  | MenhirInterpreter.T T_INITIAL -> (fun _ -> "INITIAL")
  | MenhirInterpreter.T T_INHERITS -> (fun _ -> "INHERITS")
  | MenhirInterpreter.T T_INHERITING -> (fun _ -> "INHERITING")
  | MenhirInterpreter.T T_INFO_WORD -> (fun _ -> "INFO_WORD")
  | MenhirInterpreter.T T_INDICATORS -> (fun _ -> "INDICATORS")
  | MenhirInterpreter.T T_INDICATOR -> (fun _ -> "INDICATOR")
  | MenhirInterpreter.T T_INDICATE -> (fun _ -> "INDICATE")
  | MenhirInterpreter.T T_INDIC -> (fun _ -> "INDIC")
  | MenhirInterpreter.T T_INDEX_2 -> (fun _ -> "INDEX_2")
  | MenhirInterpreter.T T_INDEX_1 -> (fun _ -> "INDEX_1")
  | MenhirInterpreter.T T_INDEXED -> (fun _ -> "INDEXED")
  | MenhirInterpreter.T T_INDEX -> (fun _ -> "INDEX")
  | MenhirInterpreter.T T_INDEPENDENT -> (fun _ -> "INDEPENDENT")
  | MenhirInterpreter.T T_IN -> (fun _ -> "IN")
  | MenhirInterpreter.T T_IMPLEMENTS -> (fun _ -> "IMPLEMENTS")
  | MenhirInterpreter.T T_IMP -> (fun _ -> "IMP")
  | MenhirInterpreter.T T_IGNORING -> (fun _ -> "IGNORING")
  | MenhirInterpreter.T T_IGNORE -> (fun _ -> "IGNORE")
  | MenhirInterpreter.T T_IF -> (fun _ -> "IF")
  | MenhirInterpreter.T T_IDS_II -> (fun _ -> "IDS_II")
  | MenhirInterpreter.T T_IDENTIFIED -> (fun _ -> "IDENTIFIED")
  | MenhirInterpreter.T T_IDENTIFICATION -> (fun _ -> "IDENTIFICATION")
  | MenhirInterpreter.T T_ID -> (fun _ -> "ID")
  | MenhirInterpreter.T T_ICON -> (fun _ -> "ICON")
  | MenhirInterpreter.T T_HSCROLL_POS -> (fun _ -> "HSCROLL_POS")
  | MenhirInterpreter.T T_HSCROLL -> (fun _ -> "HSCROLL")
  | MenhirInterpreter.T T_HOT_TRACK -> (fun _ -> "HOT_TRACK")
  | MenhirInterpreter.T T_HORIZONTAL -> (fun _ -> "HORIZONTAL")
  | MenhirInterpreter.T T_HIGH_VALUE -> (fun _ -> "HIGH_VALUE")
  | MenhirInterpreter.T T_HIGH_ORDER_RIGHT -> (fun _ -> "HIGH_ORDER_RIGHT")
  | MenhirInterpreter.T T_HIGH_ORDER_LEFT -> (fun _ -> "HIGH_ORDER_LEFT")
  | MenhirInterpreter.T T_HIGH_COLOR -> (fun _ -> "HIGH_COLOR")
  | MenhirInterpreter.T T_HIGHLIGHT -> (fun _ -> "HIGHLIGHT")
  | MenhirInterpreter.T T_HIGHEST_VALUE -> (fun _ -> "HIGHEST_VALUE")
  | MenhirInterpreter.T T_HIGH -> (fun _ -> "HIGH")
  | MenhirInterpreter.T T_HIDDEN_DATA -> (fun _ -> "HIDDEN_DATA")
  | MenhirInterpreter.T T_HEXLIT -> (fun _ -> "HEXLIT")
  | MenhirInterpreter.T T_HEX -> (fun _ -> "HEX")
  | MenhirInterpreter.T T_HELP_ID -> (fun _ -> "HELP_ID")
  | MenhirInterpreter.T T_HEIGHT_IN_CELLS -> (fun _ -> "HEIGHT_IN_CELLS")
  | MenhirInterpreter.T T_HEIGHT -> (fun _ -> "HEIGHT")
  | MenhirInterpreter.T T_HEAVY -> (fun _ -> "HEAVY")
  | MenhirInterpreter.T T_HEADING_FONT -> (fun _ -> "HEADING_FONT")
  | MenhirInterpreter.T T_HEADING_DIVIDER_COLOR -> (fun _ -> "HEADING_DIVIDER_COLOR")
  | MenhirInterpreter.T T_HEADING_COLOR -> (fun _ -> "HEADING_COLOR")
  | MenhirInterpreter.T T_HEADING -> (fun _ -> "HEADING")
  | MenhirInterpreter.T T_HAS_CHILDREN -> (fun _ -> "HAS_CHILDREN")
  | MenhirInterpreter.T T_HANDLE -> (fun _ -> "HANDLE")
  | MenhirInterpreter.T T_GT -> (fun _ -> ">")
  | MenhirInterpreter.T T_GROUP_VALUE -> (fun _ -> "GROUP_VALUE")
  | MenhirInterpreter.T T_GROUP_USAGE -> (fun _ -> "GROUP_USAGE")
  | MenhirInterpreter.T T_GROUP -> (fun _ -> "GROUP")
  | MenhirInterpreter.T T_GRIP -> (fun _ -> "GRIP")
  | MenhirInterpreter.T T_GRID -> (fun _ -> "GRID")
  | MenhirInterpreter.T T_GREATER -> (fun _ -> "GREATER")
  | MenhirInterpreter.T T_GRAPHICAL -> (fun _ -> "GRAPHICAL")
  | MenhirInterpreter.T T_GO_SEARCH -> (fun _ -> "GO_SEARCH")
  | MenhirInterpreter.T T_GO_HOME -> (fun _ -> "GO_HOME")
  | MenhirInterpreter.T T_GO_FORWARD -> (fun _ -> "GO_FORWARD")
  | MenhirInterpreter.T T_GO_BACK -> (fun _ -> "GO_BACK")
  | MenhirInterpreter.T T_GOBACK -> (fun _ -> "GOBACK")
  | MenhirInterpreter.T T_GO -> (fun _ -> "GO")
  | MenhirInterpreter.T T_GLOBAL -> (fun _ -> "GLOBAL")
  | MenhirInterpreter.T T_GIVING -> (fun _ -> "GIVING")
  | MenhirInterpreter.T T_GET -> (fun _ -> "GET")
  | MenhirInterpreter.T T_GENERATE -> (fun _ -> "GENERATE")
  | MenhirInterpreter.T T_GE -> (fun _ -> ">=")
  | MenhirInterpreter.T T_GCOS -> (fun _ -> "GCOS")
  | MenhirInterpreter.T T_FUNCTION_POINTER -> (fun _ -> "FUNCTION_POINTER")
  | MenhirInterpreter.T T_FUNCTION_ID -> (fun _ -> "FUNCTION_ID")
  | MenhirInterpreter.T T_FUNCTION -> (fun _ -> "FUNCTION")
  | MenhirInterpreter.T T_FULL_HEIGHT -> (fun _ -> "FULL_HEIGHT")
  | MenhirInterpreter.T T_FULL -> (fun _ -> "FULL")
  | MenhirInterpreter.T T_FROM -> (fun _ -> "FROM")
  | MenhirInterpreter.T T_FREE -> (fun _ -> "FREE")
  | MenhirInterpreter.T T_FRAMED -> (fun _ -> "FRAMED")
  | MenhirInterpreter.T T_FRAME -> (fun _ -> "FRAME")
  | MenhirInterpreter.T T_FORMAT -> (fun _ -> "FORMAT")
  | MenhirInterpreter.T T_FOREVER -> (fun _ -> "FOREVER")
  | MenhirInterpreter.T T_FOREGROUND_COLOR -> (fun _ -> "FOREGROUND_COLOR")
  | MenhirInterpreter.T T_FOR -> (fun _ -> "FOR")
  | MenhirInterpreter.T T_FOOTING -> (fun _ -> "FOOTING")
  | MenhirInterpreter.T T_FONT -> (fun _ -> "FONT")
  | MenhirInterpreter.T T_FLR -> (fun _ -> "FLR")
  | MenhirInterpreter.T T_FLOAT_SHORT -> (fun _ -> "FLOAT_SHORT")
  | MenhirInterpreter.T T_FLOAT_NOT_A_NUMBER_SIGNALING -> (fun _ -> "FLOAT_NOT_A_NUMBER_SIGNALING")
  | MenhirInterpreter.T T_FLOAT_NOT_A_NUMBER_QUIET -> (fun _ -> "FLOAT_NOT_A_NUMBER_QUIET")
  | MenhirInterpreter.T T_FLOAT_NOT_A_NUMBER -> (fun _ -> "FLOAT_NOT_A_NUMBER")
  | MenhirInterpreter.T T_FLOAT_LONG -> (fun _ -> "FLOAT_LONG")
  | MenhirInterpreter.T T_FLOAT_INFINITY -> (fun _ -> "FLOAT_INFINITY")
  | MenhirInterpreter.T T_FLOAT_EXTENDED -> (fun _ -> "FLOAT_EXTENDED")
  | MenhirInterpreter.T T_FLOAT_DECIMAL_34 -> (fun _ -> "FLOAT_DECIMAL_34")
  | MenhirInterpreter.T T_FLOAT_DECIMAL_16 -> (fun _ -> "FLOAT_DECIMAL_16")
  | MenhirInterpreter.T T_FLOAT_DECIMAL -> (fun _ -> "FLOAT_DECIMAL")
  | MenhirInterpreter.T T_FLOAT_BINARY_64 -> (fun _ -> "FLOAT_BINARY_64")
  | MenhirInterpreter.T T_FLOAT_BINARY_32 -> (fun _ -> "FLOAT_BINARY_32")
  | MenhirInterpreter.T T_FLOAT_BINARY_128 -> (fun _ -> "FLOAT_BINARY_128")
  | MenhirInterpreter.T T_FLOAT_BINARY -> (fun _ -> "FLOAT_BINARY")
  | MenhirInterpreter.T T_FLOATLIT -> (fun _ -> "FLOATLIT")
  | MenhirInterpreter.T T_FLOATING -> (fun _ -> "FLOATING")
  | MenhirInterpreter.T T_FLOAT -> (fun _ -> "FLOAT")
  | MenhirInterpreter.T T_FLAT_BUTTONS -> (fun _ -> "FLAT_BUTTONS")
  | MenhirInterpreter.T T_FLAT -> (fun _ -> "FLAT")
  | MenhirInterpreter.T T_FIXED_WIDTH -> (fun _ -> "FIXED_WIDTH")
  | MenhirInterpreter.T T_FIXED_FONT -> (fun _ -> "FIXED_FONT")
  | MenhirInterpreter.T T_FIXEDLIT -> (fun _ -> "FIXEDLIT")
  | MenhirInterpreter.T T_FIXED -> (fun _ -> "FIXED")
  | MenhirInterpreter.T T_FIRST -> (fun _ -> "FIRST")
  | MenhirInterpreter.T T_FINISH_REASON -> (fun _ -> "FINISH_REASON")
  | MenhirInterpreter.T T_FINALLY -> (fun _ -> "FINALLY")
  | MenhirInterpreter.T T_FINAL -> (fun _ -> "FINAL")
  | MenhirInterpreter.T T_FILL_PERCENT -> (fun _ -> "FILL_PERCENT")
  | MenhirInterpreter.T T_FILL_COLOR2 -> (fun _ -> "FILL_COLOR2")
  | MenhirInterpreter.T T_FILL_COLOR -> (fun _ -> "FILL_COLOR")
  | MenhirInterpreter.T T_FILLER -> (fun _ -> "FILLER")
  | MenhirInterpreter.T T_FILE_PREFIX -> (fun _ -> "FILE_PREFIX")
  | MenhirInterpreter.T T_FILE_POS -> (fun _ -> "FILE_POS")
  | MenhirInterpreter.T T_FILE_PATH -> (fun _ -> "FILE_PATH")
  | MenhirInterpreter.T T_FILE_NAME -> (fun _ -> "FILE_NAME")
  | MenhirInterpreter.T T_FILE_LIMITS -> (fun _ -> "FILE_LIMITS")
  | MenhirInterpreter.T T_FILE_LIMIT -> (fun _ -> "FILE_LIMIT")
  | MenhirInterpreter.T T_FILE_ID -> (fun _ -> "FILE_ID")
  | MenhirInterpreter.T T_FILE_CONTROL -> (fun _ -> "FILE_CONTROL")
  | MenhirInterpreter.T T_FILES -> (fun _ -> "FILES")
  | MenhirInterpreter.T T_FILE -> (fun _ -> "FILE")
  | MenhirInterpreter.T T_FIELD_TERMINATOR -> (fun _ -> "FIELD_TERMINATOR")
  | MenhirInterpreter.T T_FH__KEYDEF -> (fun _ -> "FH__KEYDEF")
  | MenhirInterpreter.T T_FH__FCD -> (fun _ -> "FH__FCD")
  | MenhirInterpreter.T T_FD -> (fun _ -> "FD")
  | MenhirInterpreter.T T_FARTHEST_FROM_ZERO -> (fun _ -> "FARTHEST_FROM_ZERO")
  | MenhirInterpreter.T T_FALSE -> (fun _ -> "FALSE")
  | MenhirInterpreter.T T_FACTORY -> (fun _ -> "FACTORY")
  | MenhirInterpreter.T T_F -> (fun _ -> "F")
  | MenhirInterpreter.T T_EXTERNAL_FORM -> (fun _ -> "EXTERNAL_FORM")
  | MenhirInterpreter.T T_EXTERNALLY_DESCRIBED_KEY -> (fun _ -> "EXTERNALLY_DESCRIBED_KEY")
  | MenhirInterpreter.T T_EXTERNAL -> (fun _ -> "EXTERNAL")
  | MenhirInterpreter.T T_EXTERN -> (fun _ -> "EXTERN")
  | MenhirInterpreter.T T_EXTENDED_SEARCH -> (fun _ -> "EXTENDED_SEARCH")
  | MenhirInterpreter.T T_EXTEND -> (fun _ -> "EXTEND")
  | MenhirInterpreter.T T_EXPANDS -> (fun _ -> "EXPANDS")
  | MenhirInterpreter.T T_EXPAND -> (fun _ -> "EXPAND")
  | MenhirInterpreter.T T_EXIT -> (fun _ -> "EXIT")
  | MenhirInterpreter.T T_EXHIBIT -> (fun _ -> "EXHIBIT")
  | MenhirInterpreter.T T_EXECUTE -> (fun _ -> "EXECUTE")
  | MenhirInterpreter.T T_EXEC -> (fun _ -> "EXEC")
  | MenhirInterpreter.T T_EXCLUSIVE_OR -> (fun _ -> "EXCLUSIVE_OR")
  | MenhirInterpreter.T T_EXCLUSIVE -> (fun _ -> "EXCLUSIVE")
  | MenhirInterpreter.T T_EXCLUDE_EVENT_LIST -> (fun _ -> "EXCLUDE_EVENT_LIST")
  | MenhirInterpreter.T T_EXCESS_3 -> (fun _ -> "EXCESS_3")
  | MenhirInterpreter.T T_EXCEPTION_VALUE -> (fun _ -> "EXCEPTION_VALUE")
  | MenhirInterpreter.T T_EXCEPTION_OBJECT -> (fun _ -> "EXCEPTION_OBJECT")
  | MenhirInterpreter.T T_EXCEPTION -> (fun _ -> "EXCEPTION")
  | MenhirInterpreter.T T_EXCEEDS -> (fun _ -> "EXCEEDS")
  | MenhirInterpreter.T T_EXAMINE -> (fun _ -> "EXAMINE")
  | MenhirInterpreter.T T_EVERY -> (fun _ -> "EVERY")
  | MenhirInterpreter.T T_EVENT_POINTER -> (fun _ -> "EVENT_POINTER")
  | MenhirInterpreter.T T_EVENT_LIST -> (fun _ -> "EVENT_LIST")
  | MenhirInterpreter.T T_EVENT -> (fun _ -> "EVENT")
  | MenhirInterpreter.T T_EVALUATE -> (fun _ -> "EVALUATE")
  | MenhirInterpreter.T T_ESI -> (fun _ -> "ESI")
  | MenhirInterpreter.T T_ESCAPE_BUTTON -> (fun _ -> "ESCAPE_BUTTON")
  | MenhirInterpreter.T T_ESCAPE -> (fun _ -> "ESCAPE")
  | MenhirInterpreter.T T_ERROR -> (fun _ -> "ERROR")
  | MenhirInterpreter.T T_ERASE -> (fun _ -> "ERASE")
  | MenhirInterpreter.T T_EQUAL -> (fun _ -> "EQUAL")
  | MenhirInterpreter.T T_EQ -> (fun _ -> "=")
  | MenhirInterpreter.T T_EOS -> (fun _ -> "EOS")
  | MenhirInterpreter.T T_EOP -> (fun _ -> "EOP")
  | MenhirInterpreter.T T_EOL -> (fun _ -> "EOL")
  | MenhirInterpreter.T T_EOF -> (fun _ -> "EOF")
  | MenhirInterpreter.T T_EO -> (fun _ -> "EO")
  | MenhirInterpreter.T T_ENVIRONMENT_VALUE -> (fun _ -> "ENVIRONMENT_VALUE")
  | MenhirInterpreter.T T_ENVIRONMENT_NAME -> (fun _ -> "ENVIRONMENT_NAME")
  | MenhirInterpreter.T T_ENVIRONMENT -> (fun _ -> "ENVIRONMENT")
  | MenhirInterpreter.T T_ENTRY_REASON -> (fun _ -> "ENTRY_REASON")
  | MenhirInterpreter.T T_ENTRY_FIELD -> (fun _ -> "ENTRY_FIELD")
  | MenhirInterpreter.T T_ENTRY_CONVENTION -> (fun _ -> "ENTRY_CONVENTION")
  | MenhirInterpreter.T T_ENTRY -> (fun _ -> "ENTRY")
  | MenhirInterpreter.T T_ENTER -> (fun _ -> "ENTER")
  | MenhirInterpreter.T T_ENSURE_VISIBLE -> (fun _ -> "ENSURE_VISIBLE")
  | MenhirInterpreter.T T_ENGRAVED -> (fun _ -> "ENGRAVED")
  | MenhirInterpreter.T T_END_XML -> (fun _ -> "END_XML")
  | MenhirInterpreter.T T_END_WRITE -> (fun _ -> "END_WRITE")
  | MenhirInterpreter.T T_END_WAIT -> (fun _ -> "END_WAIT")
  | MenhirInterpreter.T T_END_USE -> (fun _ -> "END_USE")
  | MenhirInterpreter.T T_END_UNSTRING -> (fun _ -> "END_UNSTRING")
  | MenhirInterpreter.T T_END_SUBTRACT -> (fun _ -> "END_SUBTRACT")
  | MenhirInterpreter.T T_END_STRING -> (fun _ -> "END_STRING")
  | MenhirInterpreter.T T_END_START -> (fun _ -> "END_START")
  | MenhirInterpreter.T T_END_SET -> (fun _ -> "END_SET")
  | MenhirInterpreter.T T_END_SEND -> (fun _ -> "END_SEND")
  | MenhirInterpreter.T T_END_SEARCH -> (fun _ -> "END_SEARCH")
  | MenhirInterpreter.T T_END_REWRITE -> (fun _ -> "END_REWRITE")
  | MenhirInterpreter.T T_END_RETURN -> (fun _ -> "END_RETURN")
  | MenhirInterpreter.T T_END_REPLACE -> (fun _ -> "END_REPLACE")
  | MenhirInterpreter.T T_END_RECEIVE -> (fun _ -> "END_RECEIVE")
  | MenhirInterpreter.T T_END_READ -> (fun _ -> "END_READ")
  | MenhirInterpreter.T T_END_PERFORM -> (fun _ -> "END_PERFORM")
  | MenhirInterpreter.T T_END_ON -> (fun _ -> "END_ON")
  | MenhirInterpreter.T T_END_OF_PAGE -> (fun _ -> "END_OF_PAGE")
  | MenhirInterpreter.T T_END_MULTIPLY -> (fun _ -> "END_MULTIPLY")
  | MenhirInterpreter.T T_END_MOVE -> (fun _ -> "END_MOVE")
  | MenhirInterpreter.T T_END_MODIFY -> (fun _ -> "END_MODIFY")
  | MenhirInterpreter.T T_END_JSON -> (fun _ -> "END_JSON")
  | MenhirInterpreter.T T_END_INVOKE -> (fun _ -> "END_INVOKE")
  | MenhirInterpreter.T T_END_IF -> (fun _ -> "END_IF")
  | MenhirInterpreter.T T_END_EXEC -> (fun _ -> "END_EXEC")
  | MenhirInterpreter.T T_END_EVALUATE -> (fun _ -> "END_EVALUATE")
  | MenhirInterpreter.T T_END_ENABLE -> (fun _ -> "END_ENABLE")
  | MenhirInterpreter.T T_END_DIVIDE -> (fun _ -> "END_DIVIDE")
  | MenhirInterpreter.T T_END_DISPLAY -> (fun _ -> "END_DISPLAY")
  | MenhirInterpreter.T T_END_DISABLE -> (fun _ -> "END_DISABLE")
  | MenhirInterpreter.T T_END_DELETE -> (fun _ -> "END_DELETE")
  | MenhirInterpreter.T T_END_COPY -> (fun _ -> "END_COPY")
  | MenhirInterpreter.T T_END_COMPUTE -> (fun _ -> "END_COMPUTE")
  | MenhirInterpreter.T T_END_COLOR -> (fun _ -> "END_COLOR")
  | MenhirInterpreter.T T_END_CHAIN -> (fun _ -> "END_CHAIN")
  | MenhirInterpreter.T T_END_CALL -> (fun _ -> "END_CALL")
  | MenhirInterpreter.T T_END_ADD -> (fun _ -> "END_ADD")
  | MenhirInterpreter.T T_END_ACCEPT -> (fun _ -> "END_ACCEPT")
  | MenhirInterpreter.T T_ENDING -> (fun _ -> "ENDING")
  | MenhirInterpreter.T T_END -> (fun _ -> "END")
  | MenhirInterpreter.T T_ENCRYPTION -> (fun _ -> "ENCRYPTION")
  | MenhirInterpreter.T T_ENCODING -> (fun _ -> "ENCODING")
  | MenhirInterpreter.T T_ENABLED -> (fun _ -> "ENABLED")
  | MenhirInterpreter.T T_ENABLE -> (fun _ -> "ENABLE")
  | MenhirInterpreter.T T_EMI -> (fun _ -> "EMI")
  | MenhirInterpreter.T T_ELSE -> (fun _ -> "ELSE")
  | MenhirInterpreter.T T_ELEMENT -> (fun _ -> "ELEMENT")
  | MenhirInterpreter.T T_EJECT -> (fun _ -> "EJECT")
  | MenhirInterpreter.T T_EIGHTY_EIGHT -> (fun _ -> "EIGHTY_EIGHT")
  | MenhirInterpreter.T T_EGI -> (fun _ -> "EGI")
  | MenhirInterpreter.T T_EGCS -> (fun _ -> "EGCS")
  | MenhirInterpreter.T T_EGC -> (fun _ -> "EGC")
  | MenhirInterpreter.T T_EDITING -> (fun _ -> "EDITING")
  | MenhirInterpreter.T T_ECHO -> (fun _ -> "ECHO")
  | MenhirInterpreter.T T_EC -> (fun _ -> "EC")
  | MenhirInterpreter.T T_EBCDIC -> (fun _ -> "EBCDIC")
  | MenhirInterpreter.T T_DYNAMIC -> (fun _ -> "DYNAMIC")
  | MenhirInterpreter.T T_DUPLICATES -> (fun _ -> "DUPLICATES")
  | MenhirInterpreter.T T_DROP_LIST -> (fun _ -> "DROP_LIST")
  | MenhirInterpreter.T T_DROP_DOWN -> (fun _ -> "DROP_DOWN")
  | MenhirInterpreter.T T_DROP -> (fun _ -> "DROP")
  | MenhirInterpreter.T T_DRAW -> (fun _ -> "DRAW")
  | MenhirInterpreter.T T_DRAG_COLOR -> (fun _ -> "DRAG_COLOR")
  | MenhirInterpreter.T T_DOWN -> (fun _ -> "DOWN")
  | MenhirInterpreter.T T_DOUBLE_COLON -> (fun _ -> "::")
  | MenhirInterpreter.T T_DOUBLE_ASTERISK -> (fun _ -> "**")
  | MenhirInterpreter.T T_DOUBLE -> (fun _ -> "DOUBLE")
  | MenhirInterpreter.T T_DOT_DASH -> (fun _ -> "DOT_DASH")
  | MenhirInterpreter.T T_DOTTED -> (fun _ -> "DOTTED")
  | MenhirInterpreter.T T_DOTDASH -> (fun _ -> "DOTDASH")
  | MenhirInterpreter.T T_DIVISION -> (fun _ -> "DIVISION")
  | MenhirInterpreter.T T_DIVIDER_COLOR -> (fun _ -> "DIVIDER_COLOR")
  | MenhirInterpreter.T T_DIVIDERS -> (fun _ -> "DIVIDERS")
  | MenhirInterpreter.T T_DIVIDE -> (fun _ -> "DIVIDE")
  | MenhirInterpreter.T T_DISPLAY_ST -> (fun _ -> "DISPLAY_ST")
  | MenhirInterpreter.T T_DISPLAY_FORMAT -> (fun _ -> "DISPLAY_FORMAT")
  | MenhirInterpreter.T T_DISPLAY_COLUMNS -> (fun _ -> "DISPLAY_COLUMNS")
  | MenhirInterpreter.T T_DISPLAY_4 -> (fun _ -> "DISPLAY_4")
  | MenhirInterpreter.T T_DISPLAY_3 -> (fun _ -> "DISPLAY_3")
  | MenhirInterpreter.T T_DISPLAY_2 -> (fun _ -> "DISPLAY_2")
  | MenhirInterpreter.T T_DISPLAY_1 -> (fun _ -> "DISPLAY_1")
  | MenhirInterpreter.T T_DISPLAY -> (fun _ -> "DISPLAY")
  | MenhirInterpreter.T T_DISP -> (fun _ -> "DISP")
  | MenhirInterpreter.T T_DISK -> (fun _ -> "DISK")
  | MenhirInterpreter.T T_DISJOINING -> (fun _ -> "DISJOINING")
  | MenhirInterpreter.T T_DISCONNECT -> (fun _ -> "DISCONNECT")
  | MenhirInterpreter.T T_DISC -> (fun _ -> "DISC")
  | MenhirInterpreter.T T_DISABLE -> (fun _ -> "DISABLE")
  | MenhirInterpreter.T T_DIGITS -> (fun _ -> "DIGITS")
  | MenhirInterpreter.T T_DETAIL -> (fun _ -> "DETAIL")
  | MenhirInterpreter.T T_DESTROY -> (fun _ -> "DESTROY")
  | MenhirInterpreter.T T_DESTINATION -> (fun _ -> "DESTINATION")
  | MenhirInterpreter.T T_DESCRIPTOR -> (fun _ -> "DESCRIPTOR")
  | MenhirInterpreter.T T_DESCENDING -> (fun _ -> "DESCENDING")
  | MenhirInterpreter.T T_DEPENDING -> (fun _ -> "DEPENDING")
  | MenhirInterpreter.T T_DELIMITER -> (fun _ -> "DELIMITER")
  | MenhirInterpreter.T T_DELIMITED -> (fun _ -> "DELIMITED")
  | MenhirInterpreter.T T_DELETE -> (fun _ -> "DELETE")
  | MenhirInterpreter.T T_DEFINITION -> (fun _ -> "DEFINITION")
  | MenhirInterpreter.T T_DEFAULT_FONT -> (fun _ -> "DEFAULT_FONT")
  | MenhirInterpreter.T T_DEFAULT_BUTTON -> (fun _ -> "DEFAULT_BUTTON")
  | MenhirInterpreter.T T_DEFAULT -> (fun _ -> "DEFAULT")
  | MenhirInterpreter.T T_DECLARE -> (fun _ -> "DECLARE")
  | MenhirInterpreter.T T_DECLARATIVES -> (fun _ -> "DECLARATIVES")
  | MenhirInterpreter.T T_DECIMAL_POINT -> (fun _ -> "DECIMAL_POINT")
  | MenhirInterpreter.T T_DECIMAL_ENCODING -> (fun _ -> "DECIMAL_ENCODING")
  | MenhirInterpreter.T T_DEBUG_SUB_3 -> (fun _ -> "DEBUG_SUB_3")
  | MenhirInterpreter.T T_DEBUG_SUB_2 -> (fun _ -> "DEBUG_SUB_2")
  | MenhirInterpreter.T T_DEBUG_SUB_1 -> (fun _ -> "DEBUG_SUB_1")
  | MenhirInterpreter.T T_DEBUG_NAME -> (fun _ -> "DEBUG_NAME")
  | MenhirInterpreter.T T_DEBUG_LINE -> (fun _ -> "DEBUG_LINE")
  | MenhirInterpreter.T T_DEBUG_ITEM -> (fun _ -> "DEBUG_ITEM")
  | MenhirInterpreter.T T_DEBUG_CONTENTS -> (fun _ -> "DEBUG_CONTENTS")
  | MenhirInterpreter.T T_DEBUGGING -> (fun _ -> "DEBUGGING")
  | MenhirInterpreter.T T_DEBUG -> (fun _ -> "DEBUG")
  | MenhirInterpreter.T T_DBCS -> (fun _ -> "DBCS")
  | MenhirInterpreter.T T_DBCLOB_LOCATOR -> (fun _ -> "DBCLOB_LOCATOR")
  | MenhirInterpreter.T T_DBCLOB_FILE -> (fun _ -> "DBCLOB_FILE")
  | MenhirInterpreter.T T_DBCLOB -> (fun _ -> "DBCLOB")
  | MenhirInterpreter.T T_DAY_OF_WEEK -> (fun _ -> "DAY_OF_WEEK")
  | MenhirInterpreter.T T_DAY_AND_TIME -> (fun _ -> "DAY_AND_TIME")
  | MenhirInterpreter.T T_DAY -> (fun _ -> "DAY")
  | MenhirInterpreter.T T_DATE_WRITTEN -> (fun _ -> "DATE_WRITTEN")
  | MenhirInterpreter.T T_DATE_RECORD -> (fun _ -> "DATE_RECORD")
  | MenhirInterpreter.T T_DATE_MODIFIED -> (fun _ -> "DATE_MODIFIED")
  | MenhirInterpreter.T T_DATE_ENTRY -> (fun _ -> "DATE_ENTRY")
  | MenhirInterpreter.T T_DATE_COMPILED -> (fun _ -> "DATE_COMPILED")
  | MenhirInterpreter.T T_DATE_AND_TIME -> (fun _ -> "DATE_AND_TIME")
  | MenhirInterpreter.T T_DATE -> (fun _ -> "DATE")
  | MenhirInterpreter.T T_DATA_TYPES -> (fun _ -> "DATA_TYPES")
  | MenhirInterpreter.T T_DATA_RECORDS -> (fun _ -> "DATA_RECORDS")
  | MenhirInterpreter.T T_DATA_RECORD -> (fun _ -> "DATA_RECORD")
  | MenhirInterpreter.T T_DATA_POINTER -> (fun _ -> "DATA_POINTER")
  | MenhirInterpreter.T T_DATA_COLUMNS -> (fun _ -> "DATA_COLUMNS")
  | MenhirInterpreter.T T_DATA -> (fun _ -> "DATA")
  | MenhirInterpreter.T T_DASH_SIGN -> (fun _ -> "-")
  | MenhirInterpreter.T T_DASHED -> (fun _ -> "DASHED")
  | MenhirInterpreter.T T_CYL_OVERFLOW -> (fun _ -> "CYL_OVERFLOW")
  | MenhirInterpreter.T T_CYL_INDEX -> (fun _ -> "CYL_INDEX")
  | MenhirInterpreter.T T_CYCLE -> (fun _ -> "CYCLE")
  | MenhirInterpreter.T T_CUSTOM_PRINT_TEMPLATE -> (fun _ -> "CUSTOM_PRINT_TEMPLATE")
  | MenhirInterpreter.T T_CURSOR_Y -> (fun _ -> "CURSOR_Y")
  | MenhirInterpreter.T T_CURSOR_X -> (fun _ -> "CURSOR_X")
  | MenhirInterpreter.T T_CURSOR_ROW -> (fun _ -> "CURSOR_ROW")
  | MenhirInterpreter.T T_CURSOR_FRAME_WIDTH -> (fun _ -> "CURSOR_FRAME_WIDTH")
  | MenhirInterpreter.T T_CURSOR_COLOR -> (fun _ -> "CURSOR_COLOR")
  | MenhirInterpreter.T T_CURSOR_COL -> (fun _ -> "CURSOR_COL")
  | MenhirInterpreter.T T_CURSOR -> (fun _ -> "CURSOR")
  | MenhirInterpreter.T T_CURRENT_DATE -> (fun _ -> "CURRENT_DATE")
  | MenhirInterpreter.T T_CURRENT -> (fun _ -> "CURRENT")
  | MenhirInterpreter.T T_CURRENCY -> (fun _ -> "CURRENCY")
  | MenhirInterpreter.T T_CULTURE -> (fun _ -> "CULTURE")
  | MenhirInterpreter.T T_CS_GENERAL -> (fun _ -> "CS_GENERAL")
  | MenhirInterpreter.T T_CS_BASIC -> (fun _ -> "CS_BASIC")
  | MenhirInterpreter.T T_CSP -> (fun _ -> "CSP")
  | MenhirInterpreter.T T_CSIZE -> (fun _ -> "CSIZE")
  | MenhirInterpreter.T T_CRT_UNDER -> (fun _ -> "CRT_UNDER")
  | MenhirInterpreter.T T_CRT -> (fun _ -> "CRT")
  | MenhirInterpreter.T T_CREATE -> (fun _ -> "CREATE")
  | MenhirInterpreter.T T_COUNT_TRAILING -> (fun _ -> "COUNT_TRAILING")
  | MenhirInterpreter.T T_COUNT_MIN -> (fun _ -> "COUNT_MIN")
  | MenhirInterpreter.T T_COUNT_MAX -> (fun _ -> "COUNT_MAX")
  | MenhirInterpreter.T T_COUNT_LEADLING -> (fun _ -> "COUNT_LEADLING")
  | MenhirInterpreter.T T_COUNT -> (fun _ -> "COUNT")
  | MenhirInterpreter.T T_CORRESPONDING -> (fun _ -> "CORRESPONDING")
  | MenhirInterpreter.T T_CORE_INDEX -> (fun _ -> "CORE_INDEX")
  | MenhirInterpreter.T T_COPY_SELECTION -> (fun _ -> "COPY_SELECTION")
  | MenhirInterpreter.T T_COPY -> (fun _ -> "COPY")
  | MenhirInterpreter.T T_CONVERTING -> (fun _ -> "CONVERTING")
  | MenhirInterpreter.T T_CONVERT -> (fun _ -> "CONVERT")
  | MenhirInterpreter.T T_CONVERSION -> (fun _ -> "CONVERSION")
  | MenhirInterpreter.T T_CONTROL_AREA -> (fun _ -> "CONTROL_AREA")
  | MenhirInterpreter.T T_CONTROLS_UNCROPPED -> (fun _ -> "CONTROLS_UNCROPPED")
  | MenhirInterpreter.T T_CONTROLS -> (fun _ -> "CONTROLS")
  | MenhirInterpreter.T T_CONTROL -> (fun _ -> "CONTROL")
  | MenhirInterpreter.T T_CONTINUE -> (fun _ -> "CONTINUE")
  | MenhirInterpreter.T T_CONTENT_OF -> (fun _ -> "CONTENT_OF")
  | MenhirInterpreter.T T_CONTENT -> (fun _ -> "CONTENT")
  | MenhirInterpreter.T T_CONTAINS -> (fun _ -> "CONTAINS")
  | MenhirInterpreter.T T_CONSTRUCTOR -> (fun _ -> "CONSTRUCTOR")
  | MenhirInterpreter.T T_CONSTANT_RECORD -> (fun _ -> "CONSTANT_RECORD")
  | MenhirInterpreter.T T_CONSTANT -> (fun _ -> "CONSTANT")
  | MenhirInterpreter.T T_CONSOLE_3 -> (fun _ -> "CONSOLE_3")
  | MenhirInterpreter.T T_CONSOLE_2 -> (fun _ -> "CONSOLE_2")
  | MenhirInterpreter.T T_CONSOLE_1 -> (fun _ -> "CONSOLE_1")
  | MenhirInterpreter.T T_CONSOLE_0 -> (fun _ -> "CONSOLE_0")
  | MenhirInterpreter.T T_CONNECT -> (fun _ -> "CONNECT")
  | MenhirInterpreter.T T_CONFIGURATION -> (fun _ -> "CONFIGURATION")
  | MenhirInterpreter.T T_CONDITION -> (fun _ -> "CONDITION")
  | MenhirInterpreter.T T_COM_REG -> (fun _ -> "COM_REG")
  | MenhirInterpreter.T T_COMP_X -> (fun _ -> "COMP_X")
  | MenhirInterpreter.T T_COMP_N -> (fun _ -> "COMP_N")
  | MenhirInterpreter.T T_COMP_9 -> (fun _ -> "COMP_9")
  | MenhirInterpreter.T T_COMP_7 -> (fun _ -> "COMP_7")
  | MenhirInterpreter.T T_COMP_6 -> (fun _ -> "COMP_6")
  | MenhirInterpreter.T T_COMP_5 -> (fun _ -> "COMP_5")
  | MenhirInterpreter.T T_COMP_4 -> (fun _ -> "COMP_4")
  | MenhirInterpreter.T T_COMP_3 -> (fun _ -> "COMP_3")
  | MenhirInterpreter.T T_COMP_2 -> (fun _ -> "COMP_2")
  | MenhirInterpreter.T T_COMP_15 -> (fun _ -> "COMP_15")
  | MenhirInterpreter.T T_COMP_14 -> (fun _ -> "COMP_14")
  | MenhirInterpreter.T T_COMP_13 -> (fun _ -> "COMP_13")
  | MenhirInterpreter.T T_COMP_12 -> (fun _ -> "COMP_12")
  | MenhirInterpreter.T T_COMP_11 -> (fun _ -> "COMP_11")
  | MenhirInterpreter.T T_COMP_10 -> (fun _ -> "COMP_10")
  | MenhirInterpreter.T T_COMP_1 -> (fun _ -> "COMP_1")
  | MenhirInterpreter.T T_COMP_0 -> (fun _ -> "COMP_0")
  | MenhirInterpreter.T T_COMPUTE -> (fun _ -> "COMPUTE")
  | MenhirInterpreter.T T_COMPUTATIONAL_7 -> (fun _ -> "COMPUTATIONAL_7")
  | MenhirInterpreter.T T_COMPUTATIONAL_14 -> (fun _ -> "COMPUTATIONAL_14")
  | MenhirInterpreter.T T_COMPUTATIONAL_13 -> (fun _ -> "COMPUTATIONAL_13")
  | MenhirInterpreter.T T_COMPUTATIONAL_12 -> (fun _ -> "COMPUTATIONAL_12")
  | MenhirInterpreter.T T_COMPUTATIONAL_11 -> (fun _ -> "COMPUTATIONAL_11")
  | MenhirInterpreter.T T_COMPRESSION -> (fun _ -> "COMPRESSION")
  | MenhirInterpreter.T T_COMPLEMENTARY -> (fun _ -> "COMPLEMENTARY")
  | MenhirInterpreter.T T_COMPLE -> (fun _ -> "COMPLE")
  | MenhirInterpreter.T T_COMP -> (fun _ -> "COMP")
  | MenhirInterpreter.T T_COMMUNICATION -> (fun _ -> "COMMUNICATION")
  | MenhirInterpreter.T T_COMMON -> (fun _ -> "COMMON")
  | MenhirInterpreter.T T_COMMITMENT -> (fun _ -> "COMMITMENT")
  | MenhirInterpreter.T T_COMMIT -> (fun _ -> "COMMIT")
  | MenhirInterpreter.T T_COMMENT_ENTRY -> (fun _ -> "COMMENT_ENTRY")
  | MenhirInterpreter.T T_COMMAND_LINE -> (fun _ -> "COMMAND_LINE")
  | MenhirInterpreter.T T_COMMA -> (fun _ -> "COMMA")
  | MenhirInterpreter.T T_COMBO_BOX -> (fun _ -> "COMBO_BOX")
  | MenhirInterpreter.T T_COLUMN_PROTECTION -> (fun _ -> "COLUMN_PROTECTION")
  | MenhirInterpreter.T T_COLUMN_HEADINGS -> (fun _ -> "COLUMN_HEADINGS")
  | MenhirInterpreter.T T_COLUMN_FONT -> (fun _ -> "COLUMN_FONT")
  | MenhirInterpreter.T T_COLUMN_DIVIDERS -> (fun _ -> "COLUMN_DIVIDERS")
  | MenhirInterpreter.T T_COLUMN_COLOR -> (fun _ -> "COLUMN_COLOR")
  | MenhirInterpreter.T T_COLUMNS -> (fun _ -> "COLUMNS")
  | MenhirInterpreter.T T_COLUMN -> (fun _ -> "COLUMN")
  | MenhirInterpreter.T T_COLORS -> (fun _ -> "COLORS")
  | MenhirInterpreter.T T_COLOR -> (fun _ -> "COLOR")
  | MenhirInterpreter.T T_COLON -> (fun _ -> ":")
  | MenhirInterpreter.T T_COLLATING -> (fun _ -> "COLLATING")
  | MenhirInterpreter.T T_COL -> (fun _ -> "COL")
  | MenhirInterpreter.T T_COERCION -> (fun _ -> "COERCION")
  | MenhirInterpreter.T T_CODE_SET -> (fun _ -> "CODE_SET")
  | MenhirInterpreter.T T_CODE -> (fun _ -> "CODE")
  | MenhirInterpreter.T T_COBOL -> (fun _ -> "COBOL")
  | MenhirInterpreter.T T_CLOSE -> (fun _ -> "CLOSE")
  | MenhirInterpreter.T T_CLOCK_UNITS -> (fun _ -> "CLOCK_UNITS")
  | MenhirInterpreter.T T_CLOB_LOCATOR -> (fun _ -> "CLOB_LOCATOR")
  | MenhirInterpreter.T T_CLOB_FILE -> (fun _ -> "CLOB_FILE")
  | MenhirInterpreter.T T_CLOB -> (fun _ -> "CLOB")
  | MenhirInterpreter.T T_CLINES -> (fun _ -> "CLINES")
  | MenhirInterpreter.T T_CLINE -> (fun _ -> "CLINE")
  | MenhirInterpreter.T T_CLEAR_SELECTION -> (fun _ -> "CLEAR_SELECTION")
  | MenhirInterpreter.T T_CLASS_OBJECT -> (fun _ -> "CLASS_OBJECT")
  | MenhirInterpreter.T T_CLASS_NAME -> (fun _ -> "CLASS_NAME")
  | MenhirInterpreter.T T_CLASS_ID -> (fun _ -> "CLASS_ID")
  | MenhirInterpreter.T T_CLASS_CONTROL -> (fun _ -> "CLASS_CONTROL")
  | MenhirInterpreter.T T_CLASSIFICATION -> (fun _ -> "CLASSIFICATION")
  | MenhirInterpreter.T T_CLASS -> (fun _ -> "CLASS")
  | MenhirInterpreter.T T_CICS -> (fun _ -> "CICS")
  | MenhirInterpreter.T T_CHECK_BOX -> (fun _ -> "CHECK_BOX")
  | MenhirInterpreter.T T_CHECKPOINT_FILE -> (fun _ -> "CHECKPOINT_FILE")
  | MenhirInterpreter.T T_CHECK -> (fun _ -> "CHECK")
  | MenhirInterpreter.T T_CHAR_VARYING -> (fun _ -> "CHAR_VARYING")
  | MenhirInterpreter.T T_CHART -> (fun _ -> "CHART")
  | MenhirInterpreter.T T_CHARACTERS -> (fun _ -> "CHARACTERS")
  | MenhirInterpreter.T T_CHARACTER -> (fun _ -> "CHARACTER")
  | MenhirInterpreter.T T_CHAR -> (fun _ -> "CHAR")
  | MenhirInterpreter.T T_CHANGED -> (fun _ -> "CHANGED")
  | MenhirInterpreter.T T_CHAINING -> (fun _ -> "CHAINING")
  | MenhirInterpreter.T T_CHAIN -> (fun _ -> "CHAIN")
  | MenhirInterpreter.T T_CH -> (fun _ -> "CH")
  | MenhirInterpreter.T T_CF -> (fun _ -> "CF")
  | MenhirInterpreter.T T_CENTURY_DAY -> (fun _ -> "CENTURY_DAY")
  | MenhirInterpreter.T T_CENTURY_DATE -> (fun _ -> "CENTURY_DATE")
  | MenhirInterpreter.T T_CENTERED_HEADINGS -> (fun _ -> "CENTERED_HEADINGS")
  | MenhirInterpreter.T T_CENTERED -> (fun _ -> "CENTERED")
  | MenhirInterpreter.T T_CENTER -> (fun _ -> "CENTER")
  | MenhirInterpreter.T T_CELL_PROTECTION -> (fun _ -> "CELL_PROTECTION")
  | MenhirInterpreter.T T_CELL_FONT -> (fun _ -> "CELL_FONT")
  | MenhirInterpreter.T T_CELL_DATA -> (fun _ -> "CELL_DATA")
  | MenhirInterpreter.T T_CELL_COLOR -> (fun _ -> "CELL_COLOR")
  | MenhirInterpreter.T T_CELL -> (fun _ -> "CELL")
  | MenhirInterpreter.T T_CD -> (fun _ -> "CD")
  | MenhirInterpreter.T T_CCOL -> (fun _ -> "CCOL")
  | MenhirInterpreter.T T_CBL -> (fun _ -> "CBL")
  | MenhirInterpreter.T T_CATALOGUE_NAME -> (fun _ -> "CATALOGUE_NAME")
  | MenhirInterpreter.T T_CATALOGUED -> (fun _ -> "CATALOGUED")
  | MenhirInterpreter.T T_CASSETTE -> (fun _ -> "CASSETTE")
  | MenhirInterpreter.T T_CASE_SENSITIVE -> (fun _ -> "CASE_SENSITIVE")
  | MenhirInterpreter.T T_CASE_INSENSITIVE -> (fun _ -> "CASE_INSENSITIVE")
  | MenhirInterpreter.T T_CARD_READER -> (fun _ -> "CARD_READER")
  | MenhirInterpreter.T T_CARD_PUNCH -> (fun _ -> "CARD_PUNCH")
  | MenhirInterpreter.T T_CAPACITY -> (fun _ -> "CAPACITY")
  | MenhirInterpreter.T T_CANCEL_BUTTON -> (fun _ -> "CANCEL_BUTTON")
  | MenhirInterpreter.T T_CANCEL -> (fun _ -> "CANCEL")
  | MenhirInterpreter.T T_CALLED -> (fun _ -> "CALLED")
  | MenhirInterpreter.T T_CALL -> (fun _ -> "CALL")
  | MenhirInterpreter.T T_CALENDAR_FONT -> (fun _ -> "CALENDAR_FONT")
  | MenhirInterpreter.T T_C -> (fun _ -> "C")
  | MenhirInterpreter.T T_B_XOR -> (fun _ -> "B_XOR")
  | MenhirInterpreter.T T_B_SHIFT_RC -> (fun _ -> "B_SHIFT_RC")
  | MenhirInterpreter.T T_B_SHIFT_R -> (fun _ -> "B_SHIFT_R")
  | MenhirInterpreter.T T_B_SHIFT_LC -> (fun _ -> "B_SHIFT_LC")
  | MenhirInterpreter.T T_B_SHIFT_L -> (fun _ -> "B_SHIFT_L")
  | MenhirInterpreter.T T_B_OR -> (fun _ -> "B_OR")
  | MenhirInterpreter.T T_B_NOT -> (fun _ -> "B_NOT")
  | MenhirInterpreter.T T_B_EXOR -> (fun _ -> "B_EXOR")
  | MenhirInterpreter.T T_B_AND -> (fun _ -> "B_AND")
  | MenhirInterpreter.T T_BYTE_LENGTH -> (fun _ -> "BYTE_LENGTH")
  | MenhirInterpreter.T T_BYTES -> (fun _ -> "BYTES")
  | MenhirInterpreter.T T_BYTE -> (fun _ -> "BYTE")
  | MenhirInterpreter.T T_BY -> (fun _ -> "BY")
  | MenhirInterpreter.T T_BUTTONS -> (fun _ -> "BUTTONS")
  | MenhirInterpreter.T T_BUSY -> (fun _ -> "BUSY")
  | MenhirInterpreter.T T_BULK_ADDITION -> (fun _ -> "BULK_ADDITION")
  | MenhirInterpreter.T T_BSN -> (fun _ -> "BSN")
  | MenhirInterpreter.T T_BROWSING -> (fun _ -> "BROWSING")
  | MenhirInterpreter.T T_BOXED -> (fun _ -> "BOXED")
  | MenhirInterpreter.T T_BOX -> (fun _ -> "BOX")
  | MenhirInterpreter.T T_BOTTOM -> (fun _ -> "BOTTOM")
  | MenhirInterpreter.T T_BOOLIT -> (fun _ -> "BOOLIT")
  | MenhirInterpreter.T T_BOOLEAN -> (fun _ -> "BOOLEAN")
  | MenhirInterpreter.T T_BLOCK -> (fun _ -> "BLOCK")
  | MenhirInterpreter.T T_BLOB_LOCATOR -> (fun _ -> "BLOB_LOCATOR")
  | MenhirInterpreter.T T_BLOB_FILE -> (fun _ -> "BLOB_FILE")
  | MenhirInterpreter.T T_BLOB -> (fun _ -> "BLOB")
  | MenhirInterpreter.T T_BLINK -> (fun _ -> "BLINK")
  | MenhirInterpreter.T T_BLANK -> (fun _ -> "BLANK")
  | MenhirInterpreter.T T_BITS -> (fun _ -> "BITS")
  | MenhirInterpreter.T T_BITMAP_WIDTH -> (fun _ -> "BITMAP_WIDTH")
  | MenhirInterpreter.T T_BITMAP_TRANSPARENT_COLOR -> (fun _ -> "BITMAP_TRANSPARENT_COLOR")
  | MenhirInterpreter.T T_BITMAP_TRAILING -> (fun _ -> "BITMAP_TRAILING")
  | MenhirInterpreter.T T_BITMAP_TIMER -> (fun _ -> "BITMAP_TIMER")
  | MenhirInterpreter.T T_BITMAP_START -> (fun _ -> "BITMAP_START")
  | MenhirInterpreter.T T_BITMAP_SCALE -> (fun _ -> "BITMAP_SCALE")
  | MenhirInterpreter.T T_BITMAP_RAW_WIDTH -> (fun _ -> "BITMAP_RAW_WIDTH")
  | MenhirInterpreter.T T_BITMAP_RAW_HEIGHT -> (fun _ -> "BITMAP_RAW_HEIGHT")
  | MenhirInterpreter.T T_BITMAP_NUMBER -> (fun _ -> "BITMAP_NUMBER")
  | MenhirInterpreter.T T_BITMAP_HANDLE -> (fun _ -> "BITMAP_HANDLE")
  | MenhirInterpreter.T T_BITMAP_END -> (fun _ -> "BITMAP_END")
  | MenhirInterpreter.T T_BITMAP -> (fun _ -> "BITMAP")
  | MenhirInterpreter.T T_BIT -> (fun _ -> "BIT")
  | MenhirInterpreter.T T_BIND -> (fun _ -> "BIND")
  | MenhirInterpreter.T T_BINARY_SHORT -> (fun _ -> "BINARY_SHORT")
  | MenhirInterpreter.T T_BINARY_SEQUENTIAL -> (fun _ -> "BINARY_SEQUENTIAL")
  | MenhirInterpreter.T T_BINARY_LONG -> (fun _ -> "BINARY_LONG")
  | MenhirInterpreter.T T_BINARY_ENCODING -> (fun _ -> "BINARY_ENCODING")
  | MenhirInterpreter.T T_BINARY_DOUBLE -> (fun _ -> "BINARY_DOUBLE")
  | MenhirInterpreter.T T_BINARY_C_LONG -> (fun _ -> "BINARY_C_LONG")
  | MenhirInterpreter.T T_BINARY_CHAR -> (fun _ -> "BINARY_CHAR")
  | MenhirInterpreter.T T_BINARY -> (fun _ -> "BINARY")
  | MenhirInterpreter.T T_BELL -> (fun _ -> "BELL")
  | MenhirInterpreter.T T_BEGINNING -> (fun _ -> "BEGINNING")
  | MenhirInterpreter.T T_BEFORE -> (fun _ -> "BEFORE")
  | MenhirInterpreter.T T_BECOMES -> (fun _ -> "BECOMES")
  | MenhirInterpreter.T T_BASIS -> (fun _ -> "BASIS")
  | MenhirInterpreter.T T_BASED -> (fun _ -> "BASED")
  | MenhirInterpreter.T T_BAR -> (fun _ -> "BAR")
  | MenhirInterpreter.T T_BACKWARD -> (fun _ -> "BACKWARD")
  | MenhirInterpreter.T T_BACKGROUND_STANDARD -> (fun _ -> "BACKGROUND_STANDARD")
  | MenhirInterpreter.T T_BACKGROUND_LOW -> (fun _ -> "BACKGROUND_LOW")
  | MenhirInterpreter.T T_BACKGROUND_HIGH -> (fun _ -> "BACKGROUND_HIGH")
  | MenhirInterpreter.T T_BACKGROUND_COLOR -> (fun _ -> "BACKGROUND_COLOR")
  | MenhirInterpreter.T T_AX_EVENT_LIST -> (fun _ -> "AX_EVENT_LIST")
  | MenhirInterpreter.T T_AWAY_FROM_ZERO -> (fun _ -> "AWAY_FROM_ZERO")
  | MenhirInterpreter.T T_AUTO_SPIN -> (fun _ -> "AUTO_SPIN")
  | MenhirInterpreter.T T_AUTO_RESIZE -> (fun _ -> "AUTO_RESIZE")
  | MenhirInterpreter.T T_AUTO_MINIMIZE -> (fun _ -> "AUTO_MINIMIZE")
  | MenhirInterpreter.T T_AUTO_HYPHEN_SKIP -> (fun _ -> "AUTO_HYPHEN_SKIP")
  | MenhirInterpreter.T T_AUTO_DECIMAL -> (fun _ -> "AUTO_DECIMAL")
  | MenhirInterpreter.T T_AUTOMATIC -> (fun _ -> "AUTOMATIC")
  | MenhirInterpreter.T T_AUTO -> (fun _ -> "AUTO")
  | MenhirInterpreter.T T_AUTHOR -> (fun _ -> "AUTHOR")
  | MenhirInterpreter.T T_AT_EOP -> (fun _ -> "AT_EOP")
  | MenhirInterpreter.T T_AT_END -> (fun _ -> "AT_END")
  | MenhirInterpreter.T T_ATTRIBUTES -> (fun _ -> "ATTRIBUTES")
  | MenhirInterpreter.T T_ATTRIBUTE -> (fun _ -> "ATTRIBUTE")
  | MenhirInterpreter.T T_AT -> (fun _ -> "AT")
  | MenhirInterpreter.T T_ASTERISK -> (fun _ -> "*")
  | MenhirInterpreter.T T_ASSIGN -> (fun _ -> "ASSIGN")
  | MenhirInterpreter.T T_ASSEMBLY_NAME -> (fun _ -> "ASSEMBLY_NAME")
  | MenhirInterpreter.T T_ASCII -> (fun _ -> "ASCII")
  | MenhirInterpreter.T T_ASCENDING -> (fun _ -> "ASCENDING")
  | MenhirInterpreter.T T_ASA -> (fun _ -> "ASA")
  | MenhirInterpreter.T T_AS -> (fun _ -> "AS")
  | MenhirInterpreter.T T_ARITHMETIC -> (fun _ -> "ARITHMETIC")
  | MenhirInterpreter.T T_ARGUMENT_VALUE -> (fun _ -> "ARGUMENT_VALUE")
  | MenhirInterpreter.T T_ARGUMENT_NUMBER -> (fun _ -> "ARGUMENT_NUMBER")
  | MenhirInterpreter.T T_AREA_VALUES -> (fun _ -> "AREA_VALUES")
  | MenhirInterpreter.T T_AREAS -> (fun _ -> "AREAS")
  | MenhirInterpreter.T T_AREA -> (fun _ -> "AREA")
  | MenhirInterpreter.T T_ARE -> (fun _ -> "ARE")
  | MenhirInterpreter.T T_APPLY -> (fun _ -> "APPLY")
  | MenhirInterpreter.T T_ANYCASE -> (fun _ -> "ANYCASE")
  | MenhirInterpreter.T T_ANY -> (fun _ -> "ANY")
  | MenhirInterpreter.T T_ANUM -> (fun _ -> "ANUM")
  | MenhirInterpreter.T T_ANSI -> (fun _ -> "ANSI")
  | MenhirInterpreter.T T_AND -> (fun _ -> "AND")
  | MenhirInterpreter.T T_AMPERSAND -> (fun _ -> "&")
  | MenhirInterpreter.T T_ALTERNATE -> (fun _ -> "ALTERNATE")
  | MenhirInterpreter.T T_ALTERING -> (fun _ -> "ALTERING")
  | MenhirInterpreter.T T_ALTER -> (fun _ -> "ALTER")
  | MenhirInterpreter.T T_ALSO -> (fun _ -> "ALSO")
  | MenhirInterpreter.T T_ALPHANUM_PREFIX -> (fun _ -> "ALPHANUM_PREFIX")
  | MenhirInterpreter.T T_ALPHANUMERIC_EDITED -> (fun _ -> "ALPHANUMERIC_EDITED")
  | MenhirInterpreter.T T_ALPHANUMERIC -> (fun _ -> "ALPHANUMERIC")
  | MenhirInterpreter.T T_ALPHANUM -> (fun _ -> "ALPHANUM")
  | MenhirInterpreter.T T_ALPHABETIC_UPPER -> (fun _ -> "ALPHABETIC_UPPER")
  | MenhirInterpreter.T T_ALPHABETIC_LOWER -> (fun _ -> "ALPHABETIC_LOWER")
  | MenhirInterpreter.T T_ALPHABETIC -> (fun _ -> "ALPHABETIC")
  | MenhirInterpreter.T T_ALPHABET -> (fun _ -> "ALPHABET")
  | MenhirInterpreter.T T_ALLOWING -> (fun _ -> "ALLOWING")
  | MenhirInterpreter.T T_ALLOCATE -> (fun _ -> "ALLOCATE")
  | MenhirInterpreter.T T_ALL -> (fun _ -> "ALL")
  | MenhirInterpreter.T T_ALIGNMENT -> (fun _ -> "ALIGNMENT")
  | MenhirInterpreter.T T_ALIGNED -> (fun _ -> "ALIGNED")
  | MenhirInterpreter.T T_ALIAS -> (fun _ -> "ALIAS")
  | MenhirInterpreter.T T_AFTER -> (fun _ -> "AFTER")
  | MenhirInterpreter.T T_ADVANCING -> (fun _ -> "ADVANCING")
  | MenhirInterpreter.T T_ADJUSTABLE_COLUMNS -> (fun _ -> "ADJUSTABLE_COLUMNS")
  | MenhirInterpreter.T T_ADDRESS -> (fun _ -> "ADDRESS")
  | MenhirInterpreter.T T_ADD -> (fun _ -> "ADD")
  | MenhirInterpreter.T T_ACTUAL -> (fun _ -> "ACTUAL")
  | MenhirInterpreter.T T_ACTIVE_X -> (fun _ -> "ACTIVE_X")
  | MenhirInterpreter.T T_ACTIVE_CLASS -> (fun _ -> "ACTIVE_CLASS")
  | MenhirInterpreter.T T_ACTIVATING -> (fun _ -> "ACTIVATING")
  | MenhirInterpreter.T T_ACTION -> (fun _ -> "ACTION")
  | MenhirInterpreter.T T_ACQUIRE -> (fun _ -> "ACQUIRE")
  | MenhirInterpreter.T T_ACCESS -> (fun _ -> "ACCESS")
  | MenhirInterpreter.T T_ACCEPT -> (fun _ -> "ACCEPT")
  | MenhirInterpreter.T T_ABSTRACT -> (fun _ -> "ABSTRACT")
  | MenhirInterpreter.T T_ABSENT -> (fun _ -> "ABSENT")
  | MenhirInterpreter.N MenhirInterpreter.N_write_target -> (fun _ -> "write_target")
  | MenhirInterpreter.N MenhirInterpreter.N_write_statement -> (fun _ -> "write_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_working_storage_section -> (fun _ -> "working_storage_section")
  | MenhirInterpreter.N MenhirInterpreter.N_word_or_terminal -> (fun _ -> "<word or TERMINAL>")
  | MenhirInterpreter.N MenhirInterpreter.N_with_test -> (fun _ -> "with_test")
  | MenhirInterpreter.N MenhirInterpreter.N_with_status -> (fun _ -> "with_status")
  | MenhirInterpreter.N MenhirInterpreter.N_with_no_advancing -> (fun _ -> "with_no_advancing")
  | MenhirInterpreter.N MenhirInterpreter.N_with_lock_clause -> (fun _ -> "with_lock_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_with_lock -> (fun _ -> "with_lock")
  | MenhirInterpreter.N MenhirInterpreter.N_with_key -> (fun _ -> "with_key")
  | MenhirInterpreter.N MenhirInterpreter.N_with_data -> (fun _ -> "with_data")
  | MenhirInterpreter.N MenhirInterpreter.N_when_selection_objects -> (fun _ -> "when_selection_objects")
  | MenhirInterpreter.N MenhirInterpreter.N_when_phrase -> (fun _ -> "when_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_when_other -> (fun _ -> "when_other")
  | MenhirInterpreter.N MenhirInterpreter.N_when_clause -> (fun _ -> "when_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_varying_phrase -> (fun _ -> "varying_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_varying_clause -> (fun _ -> "varying_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_value_of_clause -> (fun _ -> "value_of_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_validation_stage -> (fun _ -> "validation_stage")
  | MenhirInterpreter.N MenhirInterpreter.N_validation_clause -> (fun _ -> "validation_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_validate_status_clause -> (fun _ -> "validate_status_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_validate_statement -> (fun _ -> "validate_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_using_clause -> (fun _ -> "using_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_using_by -> (fun _ -> "using_by")
  | MenhirInterpreter.N MenhirInterpreter.N_use_statement -> (fun _ -> "use_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_use_after_exception -> (fun _ -> "use_after_exception")
  | MenhirInterpreter.N MenhirInterpreter.N_usage_clause -> (fun _ -> "usage_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_usage -> (fun _ -> "usage")
  | MenhirInterpreter.N MenhirInterpreter.N_upon -> (fun _ -> "upon")
  | MenhirInterpreter.N MenhirInterpreter.N_up_down -> (fun _ -> "up_down")
  | MenhirInterpreter.N MenhirInterpreter.N_unstring_target -> (fun _ -> "unstring_target")
  | MenhirInterpreter.N MenhirInterpreter.N_unstring_statement -> (fun _ -> "unstring_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_unstring_delimiters -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_unlock_statement -> (fun _ -> "unlock_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_unconditional_action -> (fun _ -> "unconditional_action")
  | MenhirInterpreter.N MenhirInterpreter.N_typedef_clause -> (fun _ -> "typedef_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_transform_statement -> (fun _ -> "transform_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_then_replacing -> (fun _ -> "then_replacing")
  | MenhirInterpreter.N MenhirInterpreter.N_terminate_statement -> (fun _ -> "terminate_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_tallying_for -> (fun _ -> "tallying_for")
  | MenhirInterpreter.N MenhirInterpreter.N_tallying -> (fun _ -> "tallying")
  | MenhirInterpreter.N MenhirInterpreter.N_synchronized_clause -> (fun _ -> "synchronized_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_symbolic_characters_clause -> (fun _ -> "symbolic_characters_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_suppress_statement -> (fun _ -> "suppress_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_sum_phrase -> (fun _ -> "sum_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_sum_operands -> (fun _ -> "sum_operands")
  | MenhirInterpreter.N MenhirInterpreter.N_sum_clause -> (fun _ -> "sum_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_subtract_statement -> (fun _ -> "subtract_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_subscripts -> (fun _ -> "<subscripts>")
  | MenhirInterpreter.N MenhirInterpreter.N_subscript_following -> (fun _ -> "<subscript>")
  | MenhirInterpreter.N MenhirInterpreter.N_subscript_first -> (fun _ -> "<subscript>")
  | MenhirInterpreter.N MenhirInterpreter.N_structure_kind -> (fun _ -> "structure_kind")
  | MenhirInterpreter.N MenhirInterpreter.N_string_statement -> (fun _ -> "string_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_string_or_int_literal -> (fun _ -> "string_or_int_literal")
  | MenhirInterpreter.N MenhirInterpreter.N_string_literal_no_all -> (fun _ -> "<string literal>")
  | MenhirInterpreter.N MenhirInterpreter.N_string_literal -> (fun _ -> "<string literal>")
  | MenhirInterpreter.N MenhirInterpreter.N_stop_statement -> (fun _ -> "stop_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_stop_kind -> (fun _ -> "stop_kind")
  | MenhirInterpreter.N MenhirInterpreter.N_step_phrase -> (fun _ -> "step_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_status_switch -> (fun _ -> "status_switch")
  | MenhirInterpreter.N MenhirInterpreter.N_start_statement -> (fun _ -> "start_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_standalone_condition -> (fun _ -> "standalone_condition")
  | MenhirInterpreter.N MenhirInterpreter.N_specifier -> (fun _ -> "specifier")
  | MenhirInterpreter.N MenhirInterpreter.N_special_names_paragraph -> (fun _ -> "special_names_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_special_names_clause -> (fun _ -> "special_names_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_source_string -> (fun _ -> "source_string")
  | MenhirInterpreter.N MenhirInterpreter.N_source_operands -> (fun _ -> "source_operands")
  | MenhirInterpreter.N MenhirInterpreter.N_source_destination_clauses -> (fun _ -> "source_destination_clauses")
  | MenhirInterpreter.N MenhirInterpreter.N_source_destination_clause -> (fun _ -> "source_destination_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_source_computer_paragraph -> (fun _ -> "source_computer_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_source_clause -> (fun _ -> "source_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_sort_statement -> (fun _ -> "sort_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_sort_merge_file_descr_clause -> (fun _ -> "sort_merge_file_descr_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_signedness_ -> (fun _ -> "signedness_")
  | MenhirInterpreter.N MenhirInterpreter.N_sign_condition_no_zero -> (fun _ -> "sign_condition_no_zero")
  | MenhirInterpreter.N MenhirInterpreter.N_sign_condition -> (fun _ -> "sign_condition")
  | MenhirInterpreter.N MenhirInterpreter.N_sign_clause -> (fun _ -> "sign_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_sign -> (fun _ -> "sign")
  | MenhirInterpreter.N MenhirInterpreter.N_sharing_phrase -> (fun _ -> "sharing_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_sharing_mode -> (fun _ -> "sharing_mode")
  | MenhirInterpreter.N MenhirInterpreter.N_sharing_clause -> (fun _ -> "sharing_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_set_statement -> (fun _ -> "set_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_set_attribute_switches -> (fun _ -> "set_attribute_switches")
  | MenhirInterpreter.N MenhirInterpreter.N_sentence -> (fun _ -> "sentence")
  | MenhirInterpreter.N MenhirInterpreter.N_send_statement -> (fun _ -> "send_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_selection_subjects -> (fun _ -> "selection_subjects")
  | MenhirInterpreter.N MenhirInterpreter.N_selection_subject -> (fun _ -> "selection_subject")
  | MenhirInterpreter.N MenhirInterpreter.N_selection_objects -> (fun _ -> "selection_objects")
  | MenhirInterpreter.N MenhirInterpreter.N_selection_object -> (fun _ -> "selection_object")
  | MenhirInterpreter.N MenhirInterpreter.N_select_when_clause -> (fun _ -> "select_when_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_select_clause -> (fun _ -> "select_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_select -> (fun _ -> "select")
  | MenhirInterpreter.N MenhirInterpreter.N_segment_limit_clause -> (fun _ -> "segment_limit_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_section_paragraphs -> (fun _ -> "section_paragraphs")
  | MenhirInterpreter.N MenhirInterpreter.N_section_paragraph -> (fun _ -> "section_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_search_statement -> (fun _ -> "search_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_search_condition -> (fun _ -> "search_condition")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_section -> (fun _ -> "screen_section")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_occurs_clause -> (fun _ -> "screen_occurs_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_line_column_clause -> (fun _ -> "screen_line_column_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_line_clause -> (fun _ -> "screen_line_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_descr_entry -> (fun _ -> "screen_descr_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_descr_clause -> (fun _ -> "screen_descr_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_column_clause -> (fun _ -> "screen_column_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_attribute_on_off -> (fun _ -> "screen_attribute_on_off")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_attribute_name -> (fun _ -> "screen_attribute_name")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_attribute_clauses -> (fun _ -> "screen_attribute_clauses")
  | MenhirInterpreter.N MenhirInterpreter.N_screen_attribute_clause -> (fun _ -> "screen_attribute_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_same_as_clause -> (fun _ -> "same_as_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_same_area_clause -> (fun _ -> "same_area_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_s_delimited_by -> (fun _ -> "s_delimited_by")
  | MenhirInterpreter.N MenhirInterpreter.N_rounding_mode -> (fun _ -> "rounding_mode")
  | MenhirInterpreter.N MenhirInterpreter.N_rounded_phrase_opt -> (fun _ -> "rounded_phrase_opt")
  | MenhirInterpreter.N MenhirInterpreter.N_rounded_phrase -> (fun _ -> "rounded_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_rounded_ident -> (fun _ -> "rounded_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_rounded_clause -> (fun _ -> "rounded_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_working_storage_section_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_with_test_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_with_status_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_step_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_signedness_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_sign_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_sharing_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_screen_section_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_s_delimited_by_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_returning_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_retry_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_report_section_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_read_direction_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_raising_exception_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_procedure_division_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_picture_locale_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_option_TO__name__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_86_qualname__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_44_property_kind__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_43_integer__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_38_integer__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_37_integer__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_34_integer__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_33_integer__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_32_qualname_or_integer__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_30_qualname_or_integer__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_14_string_literal__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_101_ident__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf___anonymous_100_ident__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_VARYING_ident__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_USING_name__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_TO_loc_integer___ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_THROUGH_string_or_int_literal__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_THROUGH_qualified_procedure_name__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_THROUGH_procedure_name__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_REMAINDER_ident__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_POSITION_integer__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_ON_name__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_INTO_loc_ident___ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_INTO_ident__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_IN_name__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_FROM_loc_integer___ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_FROM_ident_or_literal__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_FROM_expression__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_BY_ident_or_numeric__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_BY_expression__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_pf_AS_string_literal__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_perform_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_object_reference_kind_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_object_procedure_division_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_name_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_lock_or_retry_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_locale_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_local_storage_section_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_upon__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_special_names_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_source_computer_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_repository_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_program_procedure_division__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_procedure_division__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_options_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_object_computer_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_io_control_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_input_output_section__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_file_control_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_environment_division__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_data_division__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_loc_configuration_section__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_linkage_section_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_integer_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_instance_definition_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_file_section_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_expression_no_all_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_expands_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_entry_name_clause_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_endianness_mode_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_depending_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_communication_section_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_collating_sequence_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_close_format_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_capacity_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_ro_advancing_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnell_rev_tallying_ -> (fun _ -> "rnell_rev_tallying_")
  | MenhirInterpreter.N MenhirInterpreter.N_rnell_rev___anonymous_91_ -> (fun _ -> "rnell_rev___anonymous_91_")
  | MenhirInterpreter.N MenhirInterpreter.N_rnell_rev___anonymous_90_ -> (fun _ -> "rnell_rev___anonymous_90_")
  | MenhirInterpreter.N MenhirInterpreter.N_rnell_rev___anonymous_89_ -> (fun _ -> "rnell_rev___anonymous_89_")
  | MenhirInterpreter.N MenhirInterpreter.N_rnell_rev___anonymous_88_ -> (fun _ -> "rnell_rev___anonymous_88_")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_when_selection_objects_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_validation_stage_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_use_after_exception_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_unstring_target_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_subscript_following_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_specifier_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_screen_attribute_on_off_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_rounded_ident_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_qualname_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_qualified_procedure_name_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_pf_ALSO_string_or_int_literal__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_open_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_on_key_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_name_or_alphanum_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_name_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_using_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_using_by__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_tallying_for__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_special_names_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_sentence__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_select_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_section_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_replacing_phrase__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_options_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_literal__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_loc_decl_section_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_literal_through_literal_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_literal_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_line_position_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_integer_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_or_string_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_or_numeric_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_or_literal_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_by_after_before_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_ident_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_file_with_opt_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_debug_target_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_column_position_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rnel_argument_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_select_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_pf_FILE_name__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_name_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_result_imperative_statement__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_sort_merge_file_descr_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_sentence__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_section_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_screen_descr_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_same_area_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_rerun_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_report_group_descr_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_report_descr_entry__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_report_descr_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_object_computer_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_multiple_file_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_method_definition__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_informational_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_file_or_sort_merge_descr_entry__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_file_descr_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_data_descr_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_constant_or_screen_descr_entry__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_constant_or_report_group_descr_entry__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_constant_or_data_descr_entry__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_communication_descr_entry__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_loc_communication_descr_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_key_is_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_inspect_where_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rl_entry_name_clause_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_rewrite_statement -> (fun _ -> "rewrite_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_reversed_or_no_rewind_opt -> (fun _ -> "reversed_or_no_rewind_opt")
  | MenhirInterpreter.N MenhirInterpreter.N_returning -> (fun _ -> "returning")
  | MenhirInterpreter.N MenhirInterpreter.N_return_statement -> (fun _ -> "return_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_retry_phrase -> (fun _ -> "retry_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_resume_statement -> (fun _ -> "resume_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_reserve_clause -> (fun _ -> "reserve_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_rerun_frequency -> (fun _ -> "rerun_frequency")
  | MenhirInterpreter.N MenhirInterpreter.N_rerun_clause -> (fun _ -> "rerun_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_repository_paragraph -> (fun _ -> "repository_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_report_value_clause -> (fun _ -> "report_value_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_report_type_clause -> (fun _ -> "report_type_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_report_section -> (fun _ -> "report_section")
  | MenhirInterpreter.N MenhirInterpreter.N_report_screen_usage_clause -> (fun _ -> "report_screen_usage_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_report_occurs_clause -> (fun _ -> "report_occurs_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_report_line_clause -> (fun _ -> "report_line_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_report_group_descr_entry -> (fun _ -> "report_group_descr_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_report_group_descr_clause -> (fun _ -> "report_group_descr_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_report_descr_entry -> (fun _ -> "report_descr_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_report_descr_clause -> (fun _ -> "report_descr_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_report_data_name_or_final -> (fun _ -> "report_data_name_or_final")
  | MenhirInterpreter.N MenhirInterpreter.N_report_column_clause -> (fun _ -> "report_column_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_report_clause -> (fun _ -> "report_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_replacing_phrase -> (fun _ -> "replacing_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_relop -> (fun _ -> "<relational arithmetic operator>")
  | MenhirInterpreter.N MenhirInterpreter.N_release_statement -> (fun _ -> "release_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_relative_key_clause -> (fun _ -> "relative_key_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_relation_condition -> (fun _ -> "relation_condition")
  | MenhirInterpreter.N MenhirInterpreter.N_redefines_clause -> (fun _ -> "redefines_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_record_key_clause -> (fun _ -> "record_key_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_record_delimiter_clause -> (fun _ -> "record_delimiter_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_record_delimiter -> (fun _ -> "record_delimiter")
  | MenhirInterpreter.N MenhirInterpreter.N_record_clause -> (fun _ -> "record_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_receive_statement -> (fun _ -> "receive_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_read_statement -> (fun _ -> "read_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_read_direction -> (fun _ -> "read_direction")
  | MenhirInterpreter.N MenhirInterpreter.N_range_expression -> (fun _ -> "range_expression")
  | MenhirInterpreter.N MenhirInterpreter.N_raising_phrase -> (fun _ -> "raising_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_raising_exception -> (fun _ -> "raising_exception")
  | MenhirInterpreter.N MenhirInterpreter.N_raise_statement -> (fun _ -> "raise_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_qualnames -> (fun _ -> "qualnames")
  | MenhirInterpreter.N MenhirInterpreter.N_qualname_or_literal -> (fun _ -> "qualname_or_literal")
  | MenhirInterpreter.N MenhirInterpreter.N_qualname_or_integer -> (fun _ -> "qualname_or_integer")
  | MenhirInterpreter.N MenhirInterpreter.N_qualname_or_alphanum -> (fun _ -> "qualname_or_alphanum")
  | MenhirInterpreter.N MenhirInterpreter.N_qualname -> (fun _ -> "<qualified name>")
  | MenhirInterpreter.N MenhirInterpreter.N_qualified_procedure_name -> (fun _ -> "qualified_procedure_name")
  | MenhirInterpreter.N MenhirInterpreter.N_purge_statement -> (fun _ -> "purge_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_property_clause -> (fun _ -> "property_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_program_prototype_id_paragraph -> (fun _ -> "program_prototype_id_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_program_prototype -> (fun _ -> "program_prototype")
  | MenhirInterpreter.N MenhirInterpreter.N_program_procedure_division -> (fun _ -> "program_procedure_division")
  | MenhirInterpreter.N MenhirInterpreter.N_program_kind -> (fun _ -> "program_kind")
  | MenhirInterpreter.N MenhirInterpreter.N_program_definition_no_end -> (fun _ -> "program_definition_no_end")
  | MenhirInterpreter.N MenhirInterpreter.N_program_definition_identification -> (fun _ -> "program_definition_identification")
  | MenhirInterpreter.N MenhirInterpreter.N_program_definition_id_paragraph -> (fun _ -> "program_definition_id_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_program_definition -> (fun _ -> "program_definition")
  | MenhirInterpreter.N MenhirInterpreter.N_program_collating_sequence_clause -> (fun _ -> "program_collating_sequence_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_procedure_name_decl -> (fun _ -> "procedure_name_decl")
  | MenhirInterpreter.N MenhirInterpreter.N_procedure_name -> (fun _ -> "procedure_name")
  | MenhirInterpreter.N MenhirInterpreter.N_procedure_division -> (fun _ -> "procedure_division")
  | MenhirInterpreter.N MenhirInterpreter.N_present_when_clause -> (fun _ -> "present_when_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_position -> (fun _ -> "position")
  | MenhirInterpreter.N MenhirInterpreter.N_plus_or_minus -> (fun _ -> "plus_or_minus")
  | MenhirInterpreter.N MenhirInterpreter.N_picture_locale_phrase -> (fun _ -> "<locale phrase>")
  | MenhirInterpreter.N MenhirInterpreter.N_picture_clause -> (fun _ -> "<picture clause>")
  | MenhirInterpreter.N MenhirInterpreter.N_perform_statement -> (fun _ -> "perform_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_perform_phrase -> (fun _ -> "perform_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_partial_expression -> (fun _ -> "partial_expression")
  | MenhirInterpreter.N MenhirInterpreter.N_page_line_col -> (fun _ -> "page_line_col")
  | MenhirInterpreter.N MenhirInterpreter.N_page_limit_clause -> (fun _ -> "page_limit_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_padding_character_clause -> (fun _ -> "padding_character_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_output_or_giving -> (fun _ -> "output_or_giving")
  | MenhirInterpreter.N MenhirInterpreter.N_organization_clause -> (fun _ -> "organization_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_organization -> (fun _ -> "organization")
  | MenhirInterpreter.N MenhirInterpreter.N_order_table_clause -> (fun _ -> "order_table_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_options_paragraph -> (fun _ -> "options_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_options_clause -> (fun _ -> "options_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_optional_arguments_list -> (fun _ -> "optional_arguments_list")
  | MenhirInterpreter.N MenhirInterpreter.N_option_working_storage_section_ -> (fun _ -> "option_working_storage_section_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_with_test_ -> (fun _ -> "option_with_test_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_with_status_ -> (fun _ -> "option_with_status_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_step_phrase_ -> (fun _ -> "option_step_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_signedness_ -> (fun _ -> "option_signedness_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_sign_ -> (fun _ -> "option_sign_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_sharing_phrase_ -> (fun _ -> "option_sharing_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_screen_section_ -> (fun _ -> "option_screen_section_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_s_delimited_by_ -> (fun _ -> "option_s_delimited_by_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_returning_ -> (fun _ -> "option_returning_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_retry_phrase_ -> (fun _ -> "option_retry_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_report_section_ -> (fun _ -> "option_report_section_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_read_direction_ -> (fun _ -> "option_read_direction_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_raising_exception_ -> (fun _ -> "option_raising_exception_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_procedure_division_ -> (fun _ -> "option_procedure_division_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_picture_locale_phrase_ -> (fun _ -> "option_picture_locale_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_option_TO__name__ -> (fun _ -> "option_pf_option_TO__name__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_option_IS__name__ -> (fun _ -> "option_pf_option_IS__name__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_86_qualname__ -> (fun _ -> "option_pf___anonymous_86_qualname__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_44_property_kind__ -> (fun _ -> "option_pf___anonymous_44_property_kind__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_43_integer__ -> (fun _ -> "option_pf___anonymous_43_integer__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_38_integer__ -> (fun _ -> "option_pf___anonymous_38_integer__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_37_integer__ -> (fun _ -> "option_pf___anonymous_37_integer__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_34_integer__ -> (fun _ -> "option_pf___anonymous_34_integer__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_33_integer__ -> (fun _ -> "option_pf___anonymous_33_integer__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_32_qualname_or_integer__ -> (fun _ -> "option_pf___anonymous_32_qualname_or_integer__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_30_qualname_or_integer__ -> (fun _ -> "option_pf___anonymous_30_qualname_or_integer__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_14_string_literal__ -> (fun _ -> "option_pf___anonymous_14_string_literal__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_101_ident__ -> (fun _ -> "option_pf___anonymous_101_ident__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf___anonymous_100_ident__ -> (fun _ -> "option_pf___anonymous_100_ident__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_VARYING_ident__ -> (fun _ -> "option_pf_VARYING_ident__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_USING_name__ -> (fun _ -> "option_pf_USING_name__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_TO_loc_integer___ -> (fun _ -> "option_pf_TO_loc_integer___")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_THROUGH_string_or_int_literal__ -> (fun _ -> "option_pf_THROUGH_string_or_int_literal__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_THROUGH_qualified_procedure_name__ -> (fun _ -> "option_pf_THROUGH_qualified_procedure_name__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_THROUGH_procedure_name__ -> (fun _ -> "option_pf_THROUGH_procedure_name__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_REMAINDER_ident__ -> (fun _ -> "option_pf_REMAINDER_ident__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_POSITION_integer__ -> (fun _ -> "option_pf_POSITION_integer__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_ON_name__ -> (fun _ -> "option_pf_ON_name__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_INTO_loc_ident___ -> (fun _ -> "option_pf_INTO_loc_ident___")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_INTO_ident__ -> (fun _ -> "option_pf_INTO_ident__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_IN_name__ -> (fun _ -> "option_pf_IN_name__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_FROM_loc_integer___ -> (fun _ -> "option_pf_FROM_loc_integer___")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_FROM_ident_or_literal__ -> (fun _ -> "option_pf_FROM_ident_or_literal__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_FROM_expression__ -> (fun _ -> "option_pf_FROM_expression__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_BY_ident_or_numeric__ -> (fun _ -> "option_pf_BY_ident_or_numeric__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_BY_expression__ -> (fun _ -> "option_pf_BY_expression__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_pf_AS_string_literal__ -> (fun _ -> "option_pf_AS_string_literal__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_perform_phrase_ -> (fun _ -> "option_perform_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_or__NUMBER_NUMBERS__ -> (fun _ -> "option_or__NUMBER_NUMBERS__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_or__LINE_LINES__ -> (fun _ -> "option_or__LINE_LINES__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_or__IS_ARE__ -> (fun _ -> "option_or__IS_ARE__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_or__AREA_AREAS__ -> (fun _ -> "option_or__AREA_AREAS__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_object_reference_kind_ -> (fun _ -> "option_object_reference_kind_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_object_procedure_division_ -> (fun _ -> "option_object_procedure_division_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_name_ -> (fun _ -> "option_name_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_mr___anonymous_0__ -> (fun _ -> "option_mr___anonymous_0__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_lock_or_retry_ -> (fun _ -> "option_lock_or_retry_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_locale_phrase_ -> (fun _ -> "option_locale_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_local_storage_section_ -> (fun _ -> "option_local_storage_section_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_upon__ -> (fun _ -> "option_loc_upon__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_special_names_paragraph__ -> (fun _ -> "option_loc_special_names_paragraph__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_source_computer_paragraph__ -> (fun _ -> "option_loc_source_computer_paragraph__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_repository_paragraph__ -> (fun _ -> "option_loc_repository_paragraph__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_program_procedure_division__ -> (fun _ -> "option_loc_program_procedure_division__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_program_definition_no_end__ -> (fun _ -> "option_loc_program_definition_no_end__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_procedure_division__ -> (fun _ -> "option_loc_procedure_division__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_options_paragraph__ -> (fun _ -> "option_loc_options_paragraph__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_object_computer_paragraph__ -> (fun _ -> "option_loc_object_computer_paragraph__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_io_control_paragraph__ -> (fun _ -> "option_loc_io_control_paragraph__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_input_output_section__ -> (fun _ -> "option_loc_input_output_section__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_file_control_paragraph__ -> (fun _ -> "option_loc_file_control_paragraph__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_environment_division__ -> (fun _ -> "option_loc_environment_division__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_data_division__ -> (fun _ -> "option_loc_data_division__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_control_division__ -> (fun _ -> "option_loc_control_division__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_loc_configuration_section__ -> (fun _ -> "option_loc_configuration_section__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_linkage_section_ -> (fun _ -> "option_linkage_section_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_limit_is__ -> (fun _ -> "option_limit_is__")
  | MenhirInterpreter.N MenhirInterpreter.N_option_io_control_entry_ -> (fun _ -> "option_io_control_entry_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_integer_ -> (fun _ -> "option_integer_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_instance_definition_ -> (fun _ -> "option_instance_definition_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_file_section_ -> (fun _ -> "option_file_section_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_expression_no_all_ -> (fun _ -> "option_expression_no_all_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_expands_phrase_ -> (fun _ -> "option_expands_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_entry_name_clause_ -> (fun _ -> "option_entry_name_clause_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_endianness_mode_ -> (fun _ -> "option_endianness_mode_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_depending_phrase_ -> (fun _ -> "option_depending_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_default_section_ -> (fun _ -> "option_default_section_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_default_display_clause_ -> (fun _ -> "option_default_display_clause_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_default_accept_clause_ -> (fun _ -> "option_default_accept_clause_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_communication_section_ -> (fun _ -> "option_communication_section_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_collating_sequence_phrase_ -> (fun _ -> "option_collating_sequence_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_close_format_ -> (fun _ -> "option_close_format_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_capacity_phrase_ -> (fun _ -> "option_capacity_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_call_using_by_ -> (fun _ -> "option_call_using_by_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_advancing_phrase_ -> (fun _ -> "option_advancing_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_option__assign_external__ -> (fun _ -> "option__assign_external__")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_78_ -> (fun _ -> "option___anonymous_78_")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_74_ -> (fun _ -> "option___anonymous_74_")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_73_ -> (fun _ -> "option___anonymous_73_")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_59_ -> (fun _ -> "option___anonymous_59_")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_57_ -> (fun _ -> "option___anonymous_57_")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_39_ -> (fun _ -> "option___anonymous_39_")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_25_ -> (fun _ -> "option___anonymous_25_")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_24_ -> (fun _ -> "option___anonymous_24_")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_22_ -> (fun _ -> "option___anonymous_22_")
  | MenhirInterpreter.N MenhirInterpreter.N_option___anonymous_1_ -> (fun _ -> "option___anonymous_1_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_WITH_ -> (fun _ -> "option_WITH_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_WHEN_ -> (fun _ -> "option_WHEN_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_TO_ -> (fun _ -> "option_TO_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_TIMES_ -> (fun _ -> "option_TIMES_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_THEN_ -> (fun _ -> "option_THEN_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_THAN_ -> (fun _ -> "option_THAN_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_TERMINAL_ -> (fun _ -> "option_TERMINAL_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_TAPE_ -> (fun _ -> "option_TAPE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_SYMBOLIC_ -> (fun _ -> "option_SYMBOLIC_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_STRUCTURE_ -> (fun _ -> "option_STRUCTURE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_STATUS_ -> (fun _ -> "option_STATUS_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_SIZE_ -> (fun _ -> "option_SIZE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_SIGN_ -> (fun _ -> "option_SIGN_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_SET_ -> (fun _ -> "option_SET_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_RIGHT_ -> (fun _ -> "option_RIGHT_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_REFERENCES_ -> (fun _ -> "option_REFERENCES_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_RECORD_ -> (fun _ -> "option_RECORD_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_PROGRAM_ -> (fun _ -> "option_PROGRAM_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_PROCEDURE_ -> (fun _ -> "option_PROCEDURE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_PRINTING_ -> (fun _ -> "option_PRINTING_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_PERIOD_ -> (fun _ -> "option_PERIOD_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_OTHER_ -> (fun _ -> "option_OTHER_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_ORDER_ -> (fun _ -> "option_ORDER_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_ON_ -> (fun _ -> "option_ON_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_OF_ -> (fun _ -> "option_OF_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_NUMBER_ -> (fun _ -> "option_NUMBER_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_MODE_ -> (fun _ -> "option_MODE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_MESSAGE_ -> (fun _ -> "option_MESSAGE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_LINES_ -> (fun _ -> "option_LINES_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_LINE_ -> (fun _ -> "option_LINE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_LENGTH_ -> (fun _ -> "option_LENGTH_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_LEFT_ -> (fun _ -> "option_LEFT_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_KEY_ -> (fun _ -> "option_KEY_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_IS_ -> (fun _ -> "option_IS_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_INITIAL_ -> (fun _ -> "option_INITIAL_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_INDICATE_ -> (fun _ -> "option_INDICATE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_IN_ -> (fun _ -> "option_IN_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_FROM_ -> (fun _ -> "option_FROM_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_FOR_ -> (fun _ -> "option_FOR_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_FILE_ -> (fun _ -> "option_FILE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_EVERY_ -> (fun _ -> "option_EVERY_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_END_ -> (fun _ -> "option_END_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_DEFAULT_ -> (fun _ -> "option_DEFAULT_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_DATA_ -> (fun _ -> "option_DATA_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_CONTAINS_ -> (fun _ -> "option_CONTAINS_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_COLLATING_ -> (fun _ -> "option_COLLATING_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_CHARACTERS_ -> (fun _ -> "option_CHARACTERS_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_CHARACTER_ -> (fun _ -> "option_CHARACTER_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_BY_ -> (fun _ -> "option_BY_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_AT_ -> (fun _ -> "option_AT_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_AS_ -> (fun _ -> "option_AS_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_AREA_ -> (fun _ -> "option_AREA_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_ARE_ -> (fun _ -> "option_ARE_")
  | MenhirInterpreter.N MenhirInterpreter.N_option_ADVANCING_ -> (fun _ -> "option_ADVANCING_")
  | MenhirInterpreter.N MenhirInterpreter.N_open_statement -> (fun _ -> "open_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_open_phrase -> (fun _ -> "open_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_open_mode -> (fun _ -> "open_mode")
  | MenhirInterpreter.N MenhirInterpreter.N_on_overflow -> (fun _ -> "on_overflow")
  | MenhirInterpreter.N MenhirInterpreter.N_on_or_off -> (fun _ -> "on_or_off")
  | MenhirInterpreter.N MenhirInterpreter.N_on_key -> (fun _ -> "on_key")
  | MenhirInterpreter.N MenhirInterpreter.N_on_exception -> (fun _ -> "on_exception")
  | MenhirInterpreter.N MenhirInterpreter.N_occurs_fixed_clause -> (fun _ -> "occurs_fixed_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_occurs_dynamic_clause -> (fun _ -> "occurs_dynamic_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_occurs_depending_clause -> (fun _ -> "occurs_depending_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_object_view -> (fun _ -> "object_view")
  | MenhirInterpreter.N MenhirInterpreter.N_object_reference_kind -> (fun _ -> "object_reference_kind")
  | MenhirInterpreter.N MenhirInterpreter.N_object_ref -> (fun _ -> "object_ref")
  | MenhirInterpreter.N MenhirInterpreter.N_object_procedure_division -> (fun _ -> "object_procedure_division")
  | MenhirInterpreter.N MenhirInterpreter.N_object_paragraph -> (fun _ -> "object_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_object_computer_paragraph -> (fun _ -> "object_computer_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_object_computer_clause -> (fun _ -> "object_computer_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_numeric_literal -> (fun _ -> "<numeric literal>")
  | MenhirInterpreter.N MenhirInterpreter.N_ntl_name_ -> (fun _ -> "ntl_name_")
  | MenhirInterpreter.N MenhirInterpreter.N_ntl_arithmetic_term_ -> (fun _ -> "ntl_arithmetic_term_")
  | MenhirInterpreter.N MenhirInterpreter.N_nonrel_condition -> (fun _ -> "nonrel_condition")
  | MenhirInterpreter.N MenhirInterpreter.N_nonnumeric_literal_no_all -> (fun _ -> "nonnumeric_literal_no_all")
  | MenhirInterpreter.N MenhirInterpreter.N_nonnumeric_literal -> (fun _ -> "nonnumeric_literal")
  | MenhirInterpreter.N MenhirInterpreter.N_next_group_clause -> (fun _ -> "next_group_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_nell_rev_when_phrase_ -> (fun _ -> "nell_rev_when_phrase_")
  | MenhirInterpreter.N MenhirInterpreter.N_nell_rev_source_string_ -> (fun _ -> "nell_rev_source_string_")
  | MenhirInterpreter.N MenhirInterpreter.N_nell_rev_name_ -> (fun _ -> "nell_rev_name_")
  | MenhirInterpreter.N MenhirInterpreter.N_nell_rev_loc_result_imperative_statement__ -> (fun _ -> "nell_rev_loc_result_imperative_statement__")
  | MenhirInterpreter.N MenhirInterpreter.N_nell_rev_loc_when_clause__ -> (fun _ -> "nell_rev_loc_when_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_nell_rev___anonymous_70_ -> (fun _ -> "nell_rev___anonymous_70_")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_when_selection_objects_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_validation_stage_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_use_after_exception_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_unstring_target_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_sum_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_subscript_following_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_specifier_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_screen_attribute_on_off_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_rounded_ident_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_qualname_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_qualified_procedure_name_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_pf_ALSO_string_or_int_literal__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_open_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_on_key_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_name_or_alphanum_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_name_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_using_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_using_by__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_tallying_for__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_special_names_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_source_destination_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_sentence__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_select_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_section_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_screen_attribute_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_replacing_phrase__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_options_clause__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_literal__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc_decl_section_paragraph__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_loc___anonymous_72__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_literal_through_literal_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_literal_phrase_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_line_position_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_integer_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_ident_or_string_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_ident_or_numeric_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_ident_or_literal_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_ident_by_after_before_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_ident_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_file_with_opt_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_debug_target_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_column_position_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel_argument_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_84_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_80_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_50_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_48_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_42_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_29_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_21_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_16_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_nel___anonymous_13_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_names_or_open_mode -> (fun _ -> "names_or_open_mode")
  | MenhirInterpreter.N MenhirInterpreter.N_names -> (fun _ -> "names")
  | MenhirInterpreter.N MenhirInterpreter.N_name_or_string -> (fun _ -> "name_or_string")
  | MenhirInterpreter.N MenhirInterpreter.N_name_or_alphanum -> (fun _ -> "name_or_alphanum")
  | MenhirInterpreter.N MenhirInterpreter.N_name -> (fun _ -> "<word>")
  | MenhirInterpreter.N MenhirInterpreter.N_multiply_statement -> (fun _ -> "multiply_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_multiple_file_clause -> (fun _ -> "multiple_file_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_move_statement -> (fun _ -> "move_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_mnemonic_name_suffix -> (fun _ -> "mnemonic_name_suffix")
  | MenhirInterpreter.N MenhirInterpreter.N_mnemonic_name_clause -> (fun _ -> "mnemonic_name_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_85_ -> (fun _ -> "midrule___anonymous_85_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_77_ -> (fun _ -> "midrule___anonymous_77_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_on_overflow_NOT_ON_OVERFLOW__ -> (fun _ -> "midrule___anonymous_76_on_overflow_NOT_ON_OVERFLOW__")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_on_exception_NOT_ON_EXCEPTION__ -> (fun _ -> "midrule___anonymous_76_on_exception_NOT_ON_EXCEPTION__")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_at_eop_NOT_AT_EOP__ -> (fun _ -> "midrule___anonymous_76_at_eop_NOT_AT_EOP__")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_at_end_NOT_AT_END__ -> (fun _ -> "midrule___anonymous_76_at_end_NOT_AT_END__")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_ON_SIZE_ERROR_NOT_ON_SIZE_ERROR__ -> (fun _ -> "midrule___anonymous_76_ON_SIZE_ERROR_NOT_ON_SIZE_ERROR__")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_76_INVALID_KEY_NOT_INVALID_KEY__ -> (fun _ -> "midrule___anonymous_76_INVALID_KEY_NOT_INVALID_KEY__")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_68_ -> (fun _ -> "midrule___anonymous_68_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_67_ -> (fun _ -> "midrule___anonymous_67_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_66_ -> (fun _ -> "midrule___anonymous_66_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_65_ -> (fun _ -> "midrule___anonymous_65_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_64_ -> (fun _ -> "midrule___anonymous_64_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_62_ -> (fun _ -> "midrule___anonymous_62_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_61_ -> (fun _ -> "midrule___anonymous_61_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_58_ -> (fun _ -> "midrule___anonymous_58_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_55_ -> (fun _ -> "midrule___anonymous_55_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_54_ -> (fun _ -> "midrule___anonymous_54_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_53_ -> (fun _ -> "midrule___anonymous_53_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_52_ -> (fun _ -> "midrule___anonymous_52_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_51_ -> (fun _ -> "midrule___anonymous_51_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_40_ -> (fun _ -> "midrule___anonymous_40_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_35_ -> (fun _ -> "midrule___anonymous_35_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_28_ -> (fun _ -> "midrule___anonymous_28_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_27_ -> (fun _ -> "midrule___anonymous_27_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_15_ -> (fun _ -> "midrule___anonymous_15_")
  | MenhirInterpreter.N MenhirInterpreter.N_midrule___anonymous_0_ -> (fun _ -> "midrule___anonymous_0_")
  | MenhirInterpreter.N MenhirInterpreter.N_method_identification -> (fun _ -> "method_identification")
  | MenhirInterpreter.N MenhirInterpreter.N_method_id_paragraph -> (fun _ -> "method_id_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_method_definition -> (fun _ -> "method_definition")
  | MenhirInterpreter.N MenhirInterpreter.N_message_or_segment -> (fun _ -> "message_or_segment")
  | MenhirInterpreter.N MenhirInterpreter.N_merge_statement -> (fun _ -> "merge_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_memory_size_unit -> (fun _ -> "memory_size_unit")
  | MenhirInterpreter.N MenhirInterpreter.N_memory_size_clause -> (fun _ -> "memory_size_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_mcs_kind -> (fun _ -> "mcs_kind")
  | MenhirInterpreter.N MenhirInterpreter.N_mcs_command -> (fun _ -> "mcs_command")
  | MenhirInterpreter.N MenhirInterpreter.N_loption_sf_rnel_loc_options_clause___PERIOD__ -> (fun _ -> "loption_sf_rnel_loc_options_clause___PERIOD__")
  | MenhirInterpreter.N MenhirInterpreter.N_loption_pf___anonymous_26_nel_name___ -> (fun _ -> "loption_pf___anonymous_26_nel_name___")
  | MenhirInterpreter.N MenhirInterpreter.N_loption_pf___anonymous_20_names__ -> (fun _ -> "loption_pf___anonymous_20_names__")
  | MenhirInterpreter.N MenhirInterpreter.N_loption_pf___anonymous_17_names__ -> (fun _ -> "loption_pf___anonymous_17_names__")
  | MenhirInterpreter.N MenhirInterpreter.N_loption_pf_USING_rnel_loc_using_by____ -> (fun _ -> "loption_pf_USING_rnel_loc_using_by____")
  | MenhirInterpreter.N MenhirInterpreter.N_loption_pf_UPON_names__ -> (fun _ -> "loption_pf_UPON_names__")
  | MenhirInterpreter.N MenhirInterpreter.N_loption_pf_ON_rnel_validation_stage___ -> (fun _ -> "loption_pf_ON_rnel_validation_stage___")
  | MenhirInterpreter.N MenhirInterpreter.N_loption_indexed_by_ -> (fun _ -> "loption_indexed_by_")
  | MenhirInterpreter.N MenhirInterpreter.N_loption_declaratives_ -> (fun _ -> "loption_declaratives_")
  | MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_9_ -> (fun _ -> "loption___anonymous_9_")
  | MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_8_ -> (fun _ -> "loption___anonymous_8_")
  | MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_7_ -> (fun _ -> "loption___anonymous_7_")
  | MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_6_ -> (fun _ -> "loption___anonymous_6_")
  | MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_5_ -> (fun _ -> "loption___anonymous_5_")
  | MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_49_ -> (fun _ -> "loption___anonymous_49_")
  | MenhirInterpreter.N MenhirInterpreter.N_loption___anonymous_4_ -> (fun _ -> "loption___anonymous_4_")
  | MenhirInterpreter.N MenhirInterpreter.N_lock_or_retry -> (fun _ -> "lock_or_retry")
  | MenhirInterpreter.N MenhirInterpreter.N_lock_mode_clause -> (fun _ -> "lock_mode_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_lock_mode -> (fun _ -> "lock_mode")
  | MenhirInterpreter.N MenhirInterpreter.N_locale_value_or_ident -> (fun _ -> "locale_value_or_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_locale_phrase -> (fun _ -> "locale_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_locale_or_default -> (fun _ -> "locale_or_default")
  | MenhirInterpreter.N MenhirInterpreter.N_locale_or_ambiguous -> (fun _ -> "locale_or_ambiguous")
  | MenhirInterpreter.N MenhirInterpreter.N_locale_clause -> (fun _ -> "locale_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_locale_category -> (fun _ -> "locale_category")
  | MenhirInterpreter.N MenhirInterpreter.N_local_storage_section -> (fun _ -> "local_storage_section")
  | MenhirInterpreter.N MenhirInterpreter.N_ll_rev_loc_compilation_unit__ -> (fun _ -> "ll_rev_loc_compilation_unit__")
  | MenhirInterpreter.N MenhirInterpreter.N_ll_rev_and_clause_ -> (fun _ -> "ll_rev_and_clause_")
  | MenhirInterpreter.N MenhirInterpreter.N_literal_through_literal -> (fun _ -> "literal_through_literal")
  | MenhirInterpreter.N MenhirInterpreter.N_literal_phrase -> (fun _ -> "literal_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_literal_int_ident -> (fun _ -> "literal_int_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_literal -> (fun _ -> "<literal>")
  | MenhirInterpreter.N MenhirInterpreter.N_list_select_ -> (fun _ -> "list_select_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_pf_FILE_name__ -> (fun _ -> "list_pf_FILE_name__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_name_ -> (fun _ -> "list_name_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_result_imperative_statement__ -> (fun _ -> "list_loc_result_imperative_statement__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_sort_merge_file_descr_clause__ -> (fun _ -> "list_loc_sort_merge_file_descr_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_sentence__ -> (fun _ -> "list_loc_sentence__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_section_paragraph__ -> (fun _ -> "list_loc_section_paragraph__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_screen_descr_clause__ -> (fun _ -> "list_loc_screen_descr_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_same_area_clause__ -> (fun _ -> "list_loc_same_area_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_rerun_clause__ -> (fun _ -> "list_loc_rerun_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_report_group_descr_clause__ -> (fun _ -> "list_loc_report_group_descr_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_report_descr_entry__ -> (fun _ -> "list_loc_report_descr_entry__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_report_descr_clause__ -> (fun _ -> "list_loc_report_descr_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_program_definition__ -> (fun _ -> "list_loc_program_definition__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_object_computer_clause__ -> (fun _ -> "list_loc_object_computer_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_multiple_file_clause__ -> (fun _ -> "list_loc_multiple_file_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_method_definition__ -> (fun _ -> "list_loc_method_definition__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_informational_paragraph__ -> (fun _ -> "list_loc_informational_paragraph__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_file_or_sort_merge_descr_entry__ -> (fun _ -> "list_loc_file_or_sort_merge_descr_entry__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_file_descr_clause__ -> (fun _ -> "list_loc_file_descr_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_data_descr_clause__ -> (fun _ -> "list_loc_data_descr_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_constant_or_screen_descr_entry__ -> (fun _ -> "list_loc_constant_or_screen_descr_entry__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_constant_or_report_group_descr_entry__ -> (fun _ -> "list_loc_constant_or_report_group_descr_entry__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_constant_or_data_descr_entry__ -> (fun _ -> "list_loc_constant_or_data_descr_entry__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_communication_descr_entry__ -> (fun _ -> "list_loc_communication_descr_entry__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_loc_communication_descr_clause__ -> (fun _ -> "list_loc_communication_descr_clause__")
  | MenhirInterpreter.N MenhirInterpreter.N_list_key_is_ -> (fun _ -> "list_key_is_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_inspect_where_ -> (fun _ -> "list_inspect_where_")
  | MenhirInterpreter.N MenhirInterpreter.N_list_entry_name_clause_ -> (fun _ -> "list_entry_name_clause_")
  | MenhirInterpreter.N MenhirInterpreter.N_linkage_section -> (fun _ -> "linkage_section")
  | MenhirInterpreter.N MenhirInterpreter.N_line_position -> (fun _ -> "line_position")
  | MenhirInterpreter.N MenhirInterpreter.N_line_number -> (fun _ -> "line_number")
  | MenhirInterpreter.N MenhirInterpreter.N_line_header -> (fun _ -> "line_header")
  | MenhirInterpreter.N MenhirInterpreter.N_linage_header -> (fun _ -> "linage_header")
  | MenhirInterpreter.N MenhirInterpreter.N_linage_clause -> (fun _ -> "linage_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_lc_all_or_default -> (fun _ -> "lc_all_or_default")
  | MenhirInterpreter.N MenhirInterpreter.N_label_clause -> (fun _ -> "label_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_l_pf_AFTER_loc_varying_phrase___ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_l_loc___anonymous_79__ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_l___anonymous_99_ -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_key_is -> (fun _ -> "key_is")
  | MenhirInterpreter.N MenhirInterpreter.N_justified_clause -> (fun _ -> "justified_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_io_control_paragraph -> (fun _ -> "io_control_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_io_control_entry -> (fun _ -> "io_control_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_invoke_statement -> (fun _ -> "invoke_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_invalid_when_clause -> (fun _ -> "invalid_when_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_intrinsic_function_name -> (fun _ -> "intrinsic_function_name")
  | MenhirInterpreter.N MenhirInterpreter.N_intermediate_rounding_clause -> (fun _ -> "intermediate_rounding_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_interface_specifier -> (fun _ -> "interface_specifier")
  | MenhirInterpreter.N MenhirInterpreter.N_interface_identification -> (fun _ -> "interface_identification")
  | MenhirInterpreter.N MenhirInterpreter.N_interface_id_paragraph -> (fun _ -> "interface_id_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_interface_definition -> (fun _ -> "interface_definition")
  | MenhirInterpreter.N MenhirInterpreter.N_integers -> (fun _ -> "integers")
  | MenhirInterpreter.N MenhirInterpreter.N_integer -> (fun _ -> "<integer literal>")
  | MenhirInterpreter.N MenhirInterpreter.N_instance_identification -> (fun _ -> "instance_identification")
  | MenhirInterpreter.N MenhirInterpreter.N_instance_definition -> (fun _ -> "instance_definition")
  | MenhirInterpreter.N MenhirInterpreter.N_inspect_where -> (fun _ -> "inspect_where")
  | MenhirInterpreter.N MenhirInterpreter.N_inspect_statement -> (fun _ -> "inspect_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_inspect_spec -> (fun _ -> "inspect_spec")
  | MenhirInterpreter.N MenhirInterpreter.N_input_output_section -> (fun _ -> "input_output_section")
  | MenhirInterpreter.N MenhirInterpreter.N_input_or_using -> (fun _ -> "input_or_using")
  | MenhirInterpreter.N MenhirInterpreter.N_inline_invocation -> (fun _ -> "inline_invocation")
  | MenhirInterpreter.N MenhirInterpreter.N_initiate_statement -> (fun _ -> "initiate_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_initialize_statement -> (fun _ -> "initialize_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_init_data_category -> (fun _ -> "init_data_category")
  | MenhirInterpreter.N MenhirInterpreter.N_informational_paragraphs -> (fun _ -> "informational_paragraphs")
  | MenhirInterpreter.N MenhirInterpreter.N_informational_paragraph -> (fun _ -> "informational_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_info_word -> (fun _ -> "<word>")
  | MenhirInterpreter.N MenhirInterpreter.N_indexed_by -> (fun _ -> "indexed_by")
  | MenhirInterpreter.N MenhirInterpreter.N_in_of -> (fun _ -> "in_of")
  | MenhirInterpreter.N MenhirInterpreter.N_imperative_statement -> (fun _ -> "imperative_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_imp_stmts -> (fun _ -> "")
  | MenhirInterpreter.N MenhirInterpreter.N_if_statement_explicit_term -> (fun _ -> "if_statement_explicit_term")
  | MenhirInterpreter.N MenhirInterpreter.N_if_statement -> (fun _ -> "if_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_if_body -> (fun _ -> "if_body")
  | MenhirInterpreter.N MenhirInterpreter.N_idents -> (fun _ -> "<identifiers>")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_or_string_no_all -> (fun _ -> "ident_or_string_no_all")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_or_string -> (fun _ -> "ident_or_string")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_or_numeric -> (fun _ -> "ident_or_numeric")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_or_nonnumeric_no_all -> (fun _ -> "ident_or_nonnumeric_no_all")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_or_nonnumeric -> (fun _ -> "ident_or_nonnumeric")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_or_nested -> (fun _ -> "ident_or_nested")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_or_literal -> (fun _ -> "<identifier or literal>")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_or_integer -> (fun _ -> "ident_or_integer")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_or_alphanum -> (fun _ -> "ident_or_alphanum")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_by_after_before -> (fun _ -> "ident_by_after_before")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_after_before_list -> (fun _ -> "ident_after_before_list")
  | MenhirInterpreter.N MenhirInterpreter.N_ident_after_before -> (fun _ -> "ident_after_before")
  | MenhirInterpreter.N MenhirInterpreter.N_ident -> (fun _ -> "<identifier>")
  | MenhirInterpreter.N MenhirInterpreter.N_group_usage_clause -> (fun _ -> "group_usage_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_group_indicate_clause -> (fun _ -> "group_indicate_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_goback_statement -> (fun _ -> "goback_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_go_to_statement -> (fun _ -> "go_to_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_global_clause -> (fun _ -> "global_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_generate_statement -> (fun _ -> "generate_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_function_unit -> (fun _ -> "function_unit")
  | MenhirInterpreter.N MenhirInterpreter.N_function_specifier -> (fun _ -> "function_specifier")
  | MenhirInterpreter.N MenhirInterpreter.N_function_name -> (fun _ -> "<function-name>")
  | MenhirInterpreter.N MenhirInterpreter.N_function_identification -> (fun _ -> "function_identification")
  | MenhirInterpreter.N MenhirInterpreter.N_function_id_paragraph -> (fun _ -> "function_id_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_from_to_characters_opt -> (fun _ -> "from_to_characters_opt")
  | MenhirInterpreter.N MenhirInterpreter.N_free_statement -> (fun _ -> "free_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_format_clause -> (fun _ -> "format_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_for_alphanumeric_or_national_opt -> (fun _ -> "for_alphanumeric_or_national_opt")
  | MenhirInterpreter.N MenhirInterpreter.N_floatlit -> (fun _ -> "<floating-point literal>")
  | MenhirInterpreter.N MenhirInterpreter.N_float_decimal_clause -> (fun _ -> "float_decimal_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_float_content -> (fun _ -> "float_content")
  | MenhirInterpreter.N MenhirInterpreter.N_float_binary_clause -> (fun _ -> "float_binary_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_flat_combination_operand -> (fun _ -> "flat_combination_operand")
  | MenhirInterpreter.N MenhirInterpreter.N_fixedlit -> (fun _ -> "<fixed-point literal>")
  | MenhirInterpreter.N MenhirInterpreter.N_file_with_opt -> (fun _ -> "file_with_opt")
  | MenhirInterpreter.N MenhirInterpreter.N_file_status_clause -> (fun _ -> "file_status_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_file_section -> (fun _ -> "file_section")
  | MenhirInterpreter.N MenhirInterpreter.N_file_or_sort_merge_descr_entry -> (fun _ -> "file_or_sort_merge_descr_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_file_descr_clause -> (fun _ -> "file_descr_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_file_control_paragraph -> (fun _ -> "file_control_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_figurative_constant -> (fun _ -> "<figurative constant>")
  | MenhirInterpreter.N MenhirInterpreter.N_factory_paragraph -> (fun _ -> "factory_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_factory_identification -> (fun _ -> "factory_identification")
  | MenhirInterpreter.N MenhirInterpreter.N_factory_definition -> (fun _ -> "factory_definition")
  | MenhirInterpreter.N MenhirInterpreter.N_external_clause -> (fun _ -> "external_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_extended_condition -> (fun _ -> "extended_condition")
  | MenhirInterpreter.N MenhirInterpreter.N_expression_par_unop -> (fun _ -> "<expression>")
  | MenhirInterpreter.N MenhirInterpreter.N_expression_no_all -> (fun _ -> "<expression>")
  | MenhirInterpreter.N MenhirInterpreter.N_expression -> (fun _ -> "<expression>")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_unary -> (fun _ -> "expr_unary")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_term_par_unop -> (fun _ -> "expr_term_par_unop")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_term_no_all -> (fun _ -> "expr_term_no_all")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_term -> (fun _ -> "expr_term")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_factor_par_unop -> (fun _ -> "expr_factor_par_unop")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_factor_no_all -> (fun _ -> "expr_factor_no_all")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_factor -> (fun _ -> "expr_factor")
  | MenhirInterpreter.N MenhirInterpreter.N_expands_phrase -> (fun _ -> "expands_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_exit_statement -> (fun _ -> "exit_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_exit_spec -> (fun _ -> "exit_spec")
  | MenhirInterpreter.N MenhirInterpreter.N_evaluate_statement -> (fun _ -> "evaluate_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_error_or_no_error -> (fun _ -> "error_or_no_error")
  | MenhirInterpreter.N MenhirInterpreter.N_erase_clause -> (fun _ -> "erase_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_environment_division -> (fun _ -> "environment_division")
  | MenhirInterpreter.N MenhirInterpreter.N_entry_name_clause -> (fun _ -> "entry_name_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_entry_convention_clause -> (fun _ -> "entry_convention_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_enter_statement -> (fun _ -> "enter_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_ending_indicator -> (fun _ -> "ending_indicator")
  | MenhirInterpreter.N MenhirInterpreter.N_endianness_mode_ -> (fun _ -> "endianness_mode_")
  | MenhirInterpreter.N MenhirInterpreter.N_endianness_mode -> (fun _ -> "endianness_mode")
  | MenhirInterpreter.N MenhirInterpreter.N_end_subtract -> (fun _ -> "end_subtract")
  | MenhirInterpreter.N MenhirInterpreter.N_end_search -> (fun _ -> "end_search")
  | MenhirInterpreter.N MenhirInterpreter.N_end_multiply -> (fun _ -> "end_multiply")
  | MenhirInterpreter.N MenhirInterpreter.N_end_divide -> (fun _ -> "end_divide")
  | MenhirInterpreter.N MenhirInterpreter.N_end_display -> (fun _ -> "end_display")
  | MenhirInterpreter.N MenhirInterpreter.N_end_add -> (fun _ -> "end_add")
  | MenhirInterpreter.N MenhirInterpreter.N_end_accept -> (fun _ -> "end_accept")
  | MenhirInterpreter.N MenhirInterpreter.N_encoding_mode -> (fun _ -> "encoding_mode")
  | MenhirInterpreter.N MenhirInterpreter.N_encoding_endianness_opt -> (fun _ -> "encoding_endianness_opt")
  | MenhirInterpreter.N MenhirInterpreter.N_encoding_endianness -> (fun _ -> "encoding_endianness")
  | MenhirInterpreter.N MenhirInterpreter.N_enable_statement -> (fun _ -> "enable_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_else_phrase -> (fun _ -> "else_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_elementary_string_or_int_literal -> (fun _ -> "elementary_string_or_int_literal")
  | MenhirInterpreter.N MenhirInterpreter.N_elementary_nonnumeric_literal -> (fun _ -> "elementary_nonnumeric_literal")
  | MenhirInterpreter.N MenhirInterpreter.N_dynamic_length_structure_clause -> (fun _ -> "dynamic_length_structure_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_dynamic_length_clause -> (fun _ -> "dynamic_length_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_divide_statement -> (fun _ -> "divide_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_display_statement -> (fun _ -> "display_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_disable_statement -> (fun _ -> "disable_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_destination_clause -> (fun _ -> "destination_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_depending_phrase -> (fun _ -> "depending_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_delete_statement -> (fun _ -> "delete_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_default_section_clauses -> (fun _ -> "default_section_clauses")
  | MenhirInterpreter.N MenhirInterpreter.N_default_section -> (fun _ -> "default_section")
  | MenhirInterpreter.N MenhirInterpreter.N_default_display_clause -> (fun _ -> "default_display_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_default_clause -> (fun _ -> "default_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_default_accept_clause -> (fun _ -> "default_accept_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_declaratives -> (fun _ -> "declaratives")
  | MenhirInterpreter.N MenhirInterpreter.N_decl_section_paragraph -> (fun _ -> "decl_section_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_decimal_point_clause -> (fun _ -> "decimal_point_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_debug_target -> (fun _ -> "debug_target")
  | MenhirInterpreter.N MenhirInterpreter.N_date_day_time -> (fun _ -> "date_day_time")
  | MenhirInterpreter.N MenhirInterpreter.N_data_value_clause -> (fun _ -> "data_value_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_data_type_clause -> (fun _ -> "data_type_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_data_occurs_clause -> (fun _ -> "data_occurs_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_data_division -> (fun _ -> "data_division")
  | MenhirInterpreter.N MenhirInterpreter.N_data_descr_entry -> (fun _ -> "data_descr_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_data_descr_clause -> (fun _ -> "data_descr_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_data_clause -> (fun _ -> "data_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_cursor_clause -> (fun _ -> "cursor_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_currency_sign_clause -> (fun _ -> "currency_sign_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_cs_national -> (fun _ -> "cs_national")
  | MenhirInterpreter.N MenhirInterpreter.N_cs_alphanumeric -> (fun _ -> "cs_alphanumeric")
  | MenhirInterpreter.N MenhirInterpreter.N_crt_status_clause -> (fun _ -> "crt_status_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_counter -> (fun _ -> "counter")
  | MenhirInterpreter.N MenhirInterpreter.N_control_division -> (fun _ -> "control_division")
  | MenhirInterpreter.N MenhirInterpreter.N_control_clause -> (fun _ -> "control_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_continue_statement -> (fun _ -> "continue_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_constant_value_length -> (fun _ -> "constant_value_length")
  | MenhirInterpreter.N MenhirInterpreter.N_constant_spec -> (fun _ -> "constant_spec")
  | MenhirInterpreter.N MenhirInterpreter.N_constant_record_clause -> (fun _ -> "constant_record_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_constant_or_screen_descr_entry -> (fun _ -> "constant_or_screen_descr_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_constant_or_report_group_descr_entry -> (fun _ -> "constant_or_report_group_descr_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_constant_or_data_descr_entry -> (fun _ -> "constant_or_data_descr_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_constant_level -> (fun _ -> "constant_level")
  | MenhirInterpreter.N MenhirInterpreter.N_constant -> (fun _ -> "constant")
  | MenhirInterpreter.N MenhirInterpreter.N_configuration_section -> (fun _ -> "configuration_section")
  | MenhirInterpreter.N MenhirInterpreter.N_condition -> (fun _ -> "condition")
  | MenhirInterpreter.N MenhirInterpreter.N_compute_statement -> (fun _ -> "compute_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_complex_condition -> (fun _ -> "complex_condition")
  | MenhirInterpreter.N MenhirInterpreter.N_compilation_unit -> (fun _ -> "compilation_unit")
  | MenhirInterpreter.N MenhirInterpreter.N_compilation_group -> (fun _ -> "compilation_group")
  | MenhirInterpreter.N MenhirInterpreter.N_communication_section -> (fun _ -> "communication_section")
  | MenhirInterpreter.N MenhirInterpreter.N_communication_descr_entry -> (fun _ -> "communication_descr_entry")
  | MenhirInterpreter.N MenhirInterpreter.N_communication_descr_clause -> (fun _ -> "communication_descr_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_comment_entry -> (fun _ -> "<comment entry>")
  | MenhirInterpreter.N MenhirInterpreter.N_column_position -> (fun _ -> "column_position")
  | MenhirInterpreter.N MenhirInterpreter.N_column_number -> (fun _ -> "column_number")
  | MenhirInterpreter.N MenhirInterpreter.N_column_header -> (fun _ -> "column_header")
  | MenhirInterpreter.N MenhirInterpreter.N_collating_sequence_phrase -> (fun _ -> "collating_sequence_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_collating_sequence_clause -> (fun _ -> "collating_sequence_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_code_set_clause -> (fun _ -> "code_set_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_code_clause -> (fun _ -> "code_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_close_statement -> (fun _ -> "close_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_close_format -> (fun _ -> "close_format")
  | MenhirInterpreter.N MenhirInterpreter.N_class_specifier -> (fun _ -> "class_specifier")
  | MenhirInterpreter.N MenhirInterpreter.N_class_name_clause -> (fun _ -> "class_name_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_class_identification -> (fun _ -> "class_identification")
  | MenhirInterpreter.N MenhirInterpreter.N_class_id_paragraph -> (fun _ -> "class_id_paragraph")
  | MenhirInterpreter.N MenhirInterpreter.N_class_definition -> (fun _ -> "class_definition")
  | MenhirInterpreter.N MenhirInterpreter.N_class_condition_no_ident -> (fun _ -> "class_condition_no_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_class_condition -> (fun _ -> "class_condition")
  | MenhirInterpreter.N MenhirInterpreter.N_class_clause -> (fun _ -> "class_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_class_ -> (fun _ -> "class_")
  | MenhirInterpreter.N MenhirInterpreter.N_character_set -> (fun _ -> "character_set")
  | MenhirInterpreter.N MenhirInterpreter.N_character_classification_clause -> (fun _ -> "character_classification_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_character_classification -> (fun _ -> "character_classification")
  | MenhirInterpreter.N MenhirInterpreter.N_cc_national -> (fun _ -> "cc_national")
  | MenhirInterpreter.N MenhirInterpreter.N_cc_alphanumeric -> (fun _ -> "cc_alphanumeric")
  | MenhirInterpreter.N MenhirInterpreter.N_category_to_value -> (fun _ -> "category_to_value")
  | MenhirInterpreter.N MenhirInterpreter.N_capacity_phrase -> (fun _ -> "capacity_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_cancel_statement -> (fun _ -> "cancel_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_call_using_by -> (fun _ -> "call_using_by")
  | MenhirInterpreter.N MenhirInterpreter.N_call_statement -> (fun _ -> "call_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_call_prefix -> (fun _ -> "call_prefix")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_or__RECORD_RECORDS__ -> (fun _ -> "boption_or__RECORD_RECORDS__")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_or__LINE_LINES__ -> (fun _ -> "boption_or__LINE_LINES__")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_87_ -> (fun _ -> "boption___anonymous_87_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_81_ -> (fun _ -> "boption___anonymous_81_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_71_ -> (fun _ -> "boption___anonymous_71_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_60_ -> (fun _ -> "boption___anonymous_60_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_56_ -> (fun _ -> "boption___anonymous_56_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_47_ -> (fun _ -> "boption___anonymous_47_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_46_ -> (fun _ -> "boption___anonymous_46_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_45_ -> (fun _ -> "boption___anonymous_45_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_41_ -> (fun _ -> "boption___anonymous_41_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_3_ -> (fun _ -> "boption___anonymous_3_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_18_ -> (fun _ -> "boption___anonymous_18_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_12_ -> (fun _ -> "boption___anonymous_12_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_11_ -> (fun _ -> "boption___anonymous_11_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_102_ -> (fun _ -> "boption___anonymous_102_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption___anonymous_10_ -> (fun _ -> "boption___anonymous_10_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_YYYYMMDD_ -> (fun _ -> "boption_YYYYMMDD_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_YYYYDDD_ -> (fun _ -> "boption_YYYYDDD_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_STRONG_ -> (fun _ -> "boption_STRONG_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_SIGNED_ -> (fun _ -> "boption_SIGNED_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_SHORT_ -> (fun _ -> "boption_SHORT_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_OVERRIDE_ -> (fun _ -> "boption_OVERRIDE_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_OPTIONAL_ -> (fun _ -> "boption_OPTIONAL_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_ONLY_ -> (fun _ -> "boption_ONLY_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_NOT_ -> (fun _ -> "boption_NOT_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_MULTIPLE_ -> (fun _ -> "boption_MULTIPLE_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_IN_ARITHMETIC_RANGE_ -> (fun _ -> "boption_IN_ARITHMETIC_RANGE_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_INITIALIZED_ -> (fun _ -> "boption_INITIALIZED_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_INITIAL_ -> (fun _ -> "boption_INITIAL_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_GLOBAL_ -> (fun _ -> "boption_GLOBAL_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_CYCLE_ -> (fun _ -> "boption_CYCLE_")
  | MenhirInterpreter.N MenhirInterpreter.N_boption_ALL_ -> (fun _ -> "boption_ALL_")
  | MenhirInterpreter.N MenhirInterpreter.N_boollit -> (fun _ -> "boollit")
  | MenhirInterpreter.N MenhirInterpreter.N_block_contains_clause -> (fun _ -> "block_contains_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_blank_when_zero_clause -> (fun _ -> "blank_when_zero_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_blank_clause -> (fun _ -> "blank_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_based_clause -> (fun _ -> "based_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_atomic_expression_no_all -> (fun _ -> "<atomic expression>")
  | MenhirInterpreter.N MenhirInterpreter.N_atomic_expression -> (fun _ -> "<atomic expression>")
  | MenhirInterpreter.N MenhirInterpreter.N_at_eop -> (fun _ -> "at_eop")
  | MenhirInterpreter.N MenhirInterpreter.N_at_end -> (fun _ -> "at_end")
  | MenhirInterpreter.N MenhirInterpreter.N_assign_clause -> (fun _ -> "assign_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_as__strlit_ -> (fun _ -> "as__strlit_")
  | MenhirInterpreter.N MenhirInterpreter.N_arithmetic_term_no_all -> (fun _ -> "arithmetic_term_no_all")
  | MenhirInterpreter.N MenhirInterpreter.N_arithmetic_term -> (fun _ -> "arithmetic_term")
  | MenhirInterpreter.N MenhirInterpreter.N_arithmetic_mode -> (fun _ -> "arithmetic_mode")
  | MenhirInterpreter.N MenhirInterpreter.N_arithmetic_clause -> (fun _ -> "arithmetic_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_argument -> (fun _ -> "argument")
  | MenhirInterpreter.N MenhirInterpreter.N_area_source -> (fun _ -> "area_source")
  | MenhirInterpreter.N MenhirInterpreter.N_any_length_clause -> (fun _ -> "any_length_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_and_clause -> (fun _ -> "and_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_alternate_record_key_clause -> (fun _ -> "alternate_record_key_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_alter_statement -> (fun _ -> "alter_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_alphabet_specification -> (fun _ -> "alphabet_specification")
  | MenhirInterpreter.N MenhirInterpreter.N_alphabet_name_clause -> (fun _ -> "alphabet_name_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_allocate_statement -> (fun _ -> "allocate_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_alignment -> (fun _ -> "alignment")
  | MenhirInterpreter.N MenhirInterpreter.N_aligned_clause -> (fun _ -> "aligned_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_after_or_before -> (fun _ -> "after_or_before")
  | MenhirInterpreter.N MenhirInterpreter.N_advancing_phrase -> (fun _ -> "advancing_phrase")
  | MenhirInterpreter.N MenhirInterpreter.N_address -> (fun _ -> "address")
  | MenhirInterpreter.N MenhirInterpreter.N_add_statement -> (fun _ -> "add_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_access_mode_clause -> (fun _ -> "access_mode_clause")
  | MenhirInterpreter.N MenhirInterpreter.N_access_mode -> (fun _ -> "access_mode")
  | MenhirInterpreter.N MenhirInterpreter.N_accept_statement -> (fun _ -> "accept_statement")
  | MenhirInterpreter.N MenhirInterpreter.N__assign_external_ -> (fun _ -> "_assign_external_")

let print_token = function
  | ZERO_FILL -> print_value (MenhirInterpreter.T T_ZERO_FILL) ()
  | ZERO -> print_value (MenhirInterpreter.T T_ZERO) ()
  | YYYYMMDD -> print_value (MenhirInterpreter.T T_YYYYMMDD) ()
  | YYYYDDD -> print_value (MenhirInterpreter.T T_YYYYDDD) ()
  | Y -> print_value (MenhirInterpreter.T T_Y) ()
  | XOR -> print_value (MenhirInterpreter.T T_XOR) ()
  | XML_TEXT -> print_value (MenhirInterpreter.T T_XML_TEXT) ()
  | XML_SCHEMA -> print_value (MenhirInterpreter.T T_XML_SCHEMA) ()
  | XML_NTEXT -> print_value (MenhirInterpreter.T T_XML_NTEXT) ()
  | XML_EVENT -> print_value (MenhirInterpreter.T T_XML_EVENT) ()
  | XML_DECLARATION -> print_value (MenhirInterpreter.T T_XML_DECLARATION) ()
  | XML -> print_value (MenhirInterpreter.T T_XML) ()
  | X -> print_value (MenhirInterpreter.T T_X) ()
  | WRITING -> print_value (MenhirInterpreter.T T_WRITING) ()
  | WRITE_VERIFY -> print_value (MenhirInterpreter.T T_WRITE_VERIFY) ()
  | WRITE_ONLY -> print_value (MenhirInterpreter.T T_WRITE_ONLY) ()
  | WRITERS -> print_value (MenhirInterpreter.T T_WRITERS) ()
  | WRITE -> print_value (MenhirInterpreter.T T_WRITE) ()
  | WRAP -> print_value (MenhirInterpreter.T T_WRAP) ()
  | WORKING_STORAGE -> print_value (MenhirInterpreter.T T_WORKING_STORAGE) ()
  | WORD_IN_AREA_A v -> print_value (MenhirInterpreter.T T_WORD_IN_AREA_A) v
  | WORDS -> print_value (MenhirInterpreter.T T_WORDS) ()
  | WORD v -> print_value (MenhirInterpreter.T T_WORD) v
  | WITH_DATA -> print_value (MenhirInterpreter.T T_WITH_DATA) ()
  | WITH -> print_value (MenhirInterpreter.T T_WITH) ()
  | WINDOW -> print_value (MenhirInterpreter.T T_WINDOW) ()
  | WIDTH_IN_CELLS -> print_value (MenhirInterpreter.T T_WIDTH_IN_CELLS) ()
  | WIDTH -> print_value (MenhirInterpreter.T T_WIDTH) ()
  | WIDE -> print_value (MenhirInterpreter.T T_WIDE) ()
  | WHILE -> print_value (MenhirInterpreter.T T_WHILE) ()
  | WHEN_COMPILED -> print_value (MenhirInterpreter.T T_WHEN_COMPILED) ()
  | WHEN -> print_value (MenhirInterpreter.T T_WHEN) ()
  | WEB_BROWSER -> print_value (MenhirInterpreter.T T_WEB_BROWSER) ()
  | WAIT -> print_value (MenhirInterpreter.T T_WAIT) ()
  | VTOP -> print_value (MenhirInterpreter.T T_VTOP) ()
  | VSCROLL_POS -> print_value (MenhirInterpreter.T T_VSCROLL_POS) ()
  | VSCROLL_BAR -> print_value (MenhirInterpreter.T T_VSCROLL_BAR) ()
  | VSCROLL -> print_value (MenhirInterpreter.T T_VSCROLL) ()
  | VPADDING -> print_value (MenhirInterpreter.T T_VPADDING) ()
  | VOLATILE -> print_value (MenhirInterpreter.T T_VOLATILE) ()
  | VLR -> print_value (MenhirInterpreter.T T_VLR) ()
  | VISIBLE -> print_value (MenhirInterpreter.T T_VISIBLE) ()
  | VIRTUAL_WIDTH -> print_value (MenhirInterpreter.T T_VIRTUAL_WIDTH) ()
  | VIRTUAL -> print_value (MenhirInterpreter.T T_VIRTUAL) ()
  | VIA -> print_value (MenhirInterpreter.T T_VIA) ()
  | VERY_HEAVY -> print_value (MenhirInterpreter.T T_VERY_HEAVY) ()
  | VERTICAL -> print_value (MenhirInterpreter.T T_VERTICAL) ()
  | VERSION -> print_value (MenhirInterpreter.T T_VERSION) ()
  | VARYING -> print_value (MenhirInterpreter.T T_VARYING) ()
  | VARIANT -> print_value (MenhirInterpreter.T T_VARIANT) ()
  | VARIABLE -> print_value (MenhirInterpreter.T T_VARIABLE) ()
  | VARBINARY -> print_value (MenhirInterpreter.T T_VARBINARY) ()
  | VALUE_FORMAT -> print_value (MenhirInterpreter.T T_VALUE_FORMAT) ()
  | VALUES -> print_value (MenhirInterpreter.T T_VALUES) ()
  | VALUE -> print_value (MenhirInterpreter.T T_VALUE) ()
  | VALIDATING -> print_value (MenhirInterpreter.T T_VALIDATING) ()
  | VALIDATE_STATUS -> print_value (MenhirInterpreter.T T_VALIDATE_STATUS) ()
  | VALIDATE -> print_value (MenhirInterpreter.T T_VALIDATE) ()
  | VALID -> print_value (MenhirInterpreter.T T_VALID) ()
  | V -> print_value (MenhirInterpreter.T T_V) ()
  | UTF_8 -> print_value (MenhirInterpreter.T T_UTF_8) ()
  | UTF_16 -> print_value (MenhirInterpreter.T T_UTF_16) ()
  | USING -> print_value (MenhirInterpreter.T T_USING) ()
  | USE_TAB -> print_value (MenhirInterpreter.T T_USE_TAB) ()
  | USE_RETURN -> print_value (MenhirInterpreter.T T_USE_RETURN) ()
  | USE_ALT -> print_value (MenhirInterpreter.T T_USE_ALT) ()
  | USER_WHITE -> print_value (MenhirInterpreter.T T_USER_WHITE) ()
  | USER_GRAY -> print_value (MenhirInterpreter.T T_USER_GRAY) ()
  | USER_DEFAULT -> print_value (MenhirInterpreter.T T_USER_DEFAULT) ()
  | USER_COLORS -> print_value (MenhirInterpreter.T T_USER_COLORS) ()
  | USER -> print_value (MenhirInterpreter.T T_USER) ()
  | USE -> print_value (MenhirInterpreter.T T_USE) ()
  | USAGE -> print_value (MenhirInterpreter.T T_USAGE) ()
  | UPPER -> print_value (MenhirInterpreter.T T_UPPER) ()
  | UPON -> print_value (MenhirInterpreter.T T_UPON) ()
  | UPDATERS -> print_value (MenhirInterpreter.T T_UPDATERS) ()
  | UPDATE -> print_value (MenhirInterpreter.T T_UPDATE) ()
  | UP -> print_value (MenhirInterpreter.T T_UP) ()
  | UNUSED__ -> print_value (MenhirInterpreter.T T_UNUSED__) ()
  | UNTIL -> print_value (MenhirInterpreter.T T_UNTIL) ()
  | UNSTRING -> print_value (MenhirInterpreter.T T_UNSTRING) ()
  | UNSORTED -> print_value (MenhirInterpreter.T T_UNSORTED) ()
  | UNSIGNED_SHORT -> print_value (MenhirInterpreter.T T_UNSIGNED_SHORT) ()
  | UNSIGNED_LONG -> print_value (MenhirInterpreter.T T_UNSIGNED_LONG) ()
  | UNSIGNED_INT -> print_value (MenhirInterpreter.T T_UNSIGNED_INT) ()
  | UNSIGNED -> print_value (MenhirInterpreter.T T_UNSIGNED) ()
  | UNSEQUAL -> print_value (MenhirInterpreter.T T_UNSEQUAL) ()
  | UNLOCK -> print_value (MenhirInterpreter.T T_UNLOCK) ()
  | UNIVERSAL -> print_value (MenhirInterpreter.T T_UNIVERSAL) ()
  | UNIT -> print_value (MenhirInterpreter.T T_UNIT) ()
  | UNFRAMED -> print_value (MenhirInterpreter.T T_UNFRAMED) ()
  | UNEQUAL -> print_value (MenhirInterpreter.T T_UNEQUAL) ()
  | UNDERLINE -> print_value (MenhirInterpreter.T T_UNDERLINE) ()
  | UNBOUNDED -> print_value (MenhirInterpreter.T T_UNBOUNDED) ()
  | UFF -> print_value (MenhirInterpreter.T T_UFF) ()
  | UCS_4 -> print_value (MenhirInterpreter.T T_UCS_4) ()
  | U -> print_value (MenhirInterpreter.T T_U) ()
  | TYPEDEF -> print_value (MenhirInterpreter.T T_TYPEDEF) ()
  | TYPE -> print_value (MenhirInterpreter.T T_TYPE) ()
  | TRUNCATION -> print_value (MenhirInterpreter.T T_TRUNCATION) ()
  | TRUE -> print_value (MenhirInterpreter.T T_TRUE) ()
  | TRIMMED -> print_value (MenhirInterpreter.T T_TRIMMED) ()
  | TREE_VIEW -> print_value (MenhirInterpreter.T T_TREE_VIEW) ()
  | TRANSPARENT_COLOR -> print_value (MenhirInterpreter.T T_TRANSPARENT_COLOR) ()
  | TRANSPARENT -> print_value (MenhirInterpreter.T T_TRANSPARENT) ()
  | TRANSFORM -> print_value (MenhirInterpreter.T T_TRANSFORM) ()
  | TRANSACTION_STATUS -> print_value (MenhirInterpreter.T T_TRANSACTION_STATUS) ()
  | TRANSACTION -> print_value (MenhirInterpreter.T T_TRANSACTION) ()
  | TRAILING_SIGN -> print_value (MenhirInterpreter.T T_TRAILING_SIGN) ()
  | TRAILING_SHIFT -> print_value (MenhirInterpreter.T T_TRAILING_SHIFT) ()
  | TRAILING -> print_value (MenhirInterpreter.T T_TRAILING) ()
  | TRADITIONAL_FONT -> print_value (MenhirInterpreter.T T_TRADITIONAL_FONT) ()
  | TRACK_THUMB -> print_value (MenhirInterpreter.T T_TRACK_THUMB) ()
  | TRACK_LIMIT -> print_value (MenhirInterpreter.T T_TRACK_LIMIT) ()
  | TRACK_AREA -> print_value (MenhirInterpreter.T T_TRACK_AREA) ()
  | TRACKS -> print_value (MenhirInterpreter.T T_TRACKS) ()
  | TRACK -> print_value (MenhirInterpreter.T T_TRACK) ()
  | TRACE -> print_value (MenhirInterpreter.T T_TRACE) ()
  | TOWARD_LESSER -> print_value (MenhirInterpreter.T T_TOWARD_LESSER) ()
  | TOWARD_GREATER -> print_value (MenhirInterpreter.T T_TOWARD_GREATER) ()
  | TOTALING -> print_value (MenhirInterpreter.T T_TOTALING) ()
  | TOTALED -> print_value (MenhirInterpreter.T T_TOTALED) ()
  | TOP_LEVEL -> print_value (MenhirInterpreter.T T_TOP_LEVEL) ()
  | TOP -> print_value (MenhirInterpreter.T T_TOP) ()
  | TOOL_BAR -> print_value (MenhirInterpreter.T T_TOOL_BAR) ()
  | TO -> print_value (MenhirInterpreter.T T_TO) ()
  | TITLE_POSITION -> print_value (MenhirInterpreter.T T_TITLE_POSITION) ()
  | TITLE_BAR -> print_value (MenhirInterpreter.T T_TITLE_BAR) ()
  | TITLE -> print_value (MenhirInterpreter.T T_TITLE) ()
  | TIME_RECORD -> print_value (MenhirInterpreter.T T_TIME_RECORD) ()
  | TIME_OUT -> print_value (MenhirInterpreter.T T_TIME_OUT) ()
  | TIME_OF_DAY -> print_value (MenhirInterpreter.T T_TIME_OF_DAY) ()
  | TIMESTAMP_RECORD -> print_value (MenhirInterpreter.T T_TIMESTAMP_RECORD) ()
  | TIMESTAMP_OFFSET_RECORD -> print_value (MenhirInterpreter.T T_TIMESTAMP_OFFSET_RECORD) ()
  | TIMESTAMP_OFFSET -> print_value (MenhirInterpreter.T T_TIMESTAMP_OFFSET) ()
  | TIMESTAMP -> print_value (MenhirInterpreter.T T_TIMESTAMP) ()
  | TIMES -> print_value (MenhirInterpreter.T T_TIMES) ()
  | TIME -> print_value (MenhirInterpreter.T T_TIME) ()
  | TILED_HEADINGS -> print_value (MenhirInterpreter.T T_TILED_HEADINGS) ()
  | THUMB_POSITION -> print_value (MenhirInterpreter.T T_THUMB_POSITION) ()
  | THROUGH -> print_value (MenhirInterpreter.T T_THROUGH) ()
  | THREEDIMENSIONAL -> print_value (MenhirInterpreter.T T_THREEDIMENSIONAL) ()
  | THREAD_POINTER -> print_value (MenhirInterpreter.T T_THREAD_POINTER) ()
  | THREAD_LOCAL_STORAGE -> print_value (MenhirInterpreter.T T_THREAD_LOCAL_STORAGE) ()
  | THREAD_LOCAL -> print_value (MenhirInterpreter.T T_THREAD_LOCAL) ()
  | THREADS -> print_value (MenhirInterpreter.T T_THREADS) ()
  | THREAD -> print_value (MenhirInterpreter.T T_THREAD) ()
  | THEN -> print_value (MenhirInterpreter.T T_THEN) ()
  | THAN -> print_value (MenhirInterpreter.T T_THAN) ()
  | TEXT -> print_value (MenhirInterpreter.T T_TEXT) ()
  | TEST -> print_value (MenhirInterpreter.T T_TEST) ()
  | TERMINATION_VALUE -> print_value (MenhirInterpreter.T T_TERMINATION_VALUE) ()
  | TERMINATE -> print_value (MenhirInterpreter.T T_TERMINATE) ()
  | TERMINAL_X -> print_value (MenhirInterpreter.T T_TERMINAL_X) ()
  | TERMINAL_INFO -> print_value (MenhirInterpreter.T T_TERMINAL_INFO) ()
  | TERMINAL_3 -> print_value (MenhirInterpreter.T T_TERMINAL_3) ()
  | TERMINAL_2 -> print_value (MenhirInterpreter.T T_TERMINAL_2) ()
  | TERMINAL_1 -> print_value (MenhirInterpreter.T T_TERMINAL_1) ()
  | TERMINAL_0 -> print_value (MenhirInterpreter.T T_TERMINAL_0) ()
  | TERMINAL -> print_value (MenhirInterpreter.T T_TERMINAL) ()
  | TEMPORARY -> print_value (MenhirInterpreter.T T_TEMPORARY) ()
  | TEMP -> print_value (MenhirInterpreter.T T_TEMP) ()
  | TAPE -> print_value (MenhirInterpreter.T T_TAPE) ()
  | TALLYING -> print_value (MenhirInterpreter.T T_TALLYING) ()
  | TALLY -> print_value (MenhirInterpreter.T T_TALLY) ()
  | TAB_TO_DELETE -> print_value (MenhirInterpreter.T T_TAB_TO_DELETE) ()
  | TAB_TO_ADD -> print_value (MenhirInterpreter.T T_TAB_TO_ADD) ()
  | TAB_CONTROL -> print_value (MenhirInterpreter.T T_TAB_CONTROL) ()
  | TABLE -> print_value (MenhirInterpreter.T T_TABLE) ()
  | TAB -> print_value (MenhirInterpreter.T T_TAB) ()
  | SYSTEM_OFFSET -> print_value (MenhirInterpreter.T T_SYSTEM_OFFSET) ()
  | SYSTEM_INFO -> print_value (MenhirInterpreter.T T_SYSTEM_INFO) ()
  | SYSTEM_DEFAULT -> print_value (MenhirInterpreter.T T_SYSTEM_DEFAULT) ()
  | SYSTEM -> print_value (MenhirInterpreter.T T_SYSTEM) ()
  | SYSOUT_X -> print_value (MenhirInterpreter.T T_SYSOUT_X) ()
  | SYSOUT_3 -> print_value (MenhirInterpreter.T T_SYSOUT_3) ()
  | SYSOUT_2 -> print_value (MenhirInterpreter.T T_SYSOUT_2) ()
  | SYSOUT_1 -> print_value (MenhirInterpreter.T T_SYSOUT_1) ()
  | SYSOUT_0 -> print_value (MenhirInterpreter.T T_SYSOUT_0) ()
  | SYSIN_X -> print_value (MenhirInterpreter.T T_SYSIN_X) ()
  | SYSIN_3 -> print_value (MenhirInterpreter.T T_SYSIN_3) ()
  | SYSIN_2 -> print_value (MenhirInterpreter.T T_SYSIN_2) ()
  | SYSIN_1 -> print_value (MenhirInterpreter.T T_SYSIN_1) ()
  | SYSIN_0 -> print_value (MenhirInterpreter.T T_SYSIN_0) ()
  | SYNCHRONIZED -> print_value (MenhirInterpreter.T T_SYNCHRONIZED) ()
  | SYMBOLIC -> print_value (MenhirInterpreter.T T_SYMBOLIC) ()
  | SYMBOL -> print_value (MenhirInterpreter.T T_SYMBOL) ()
  | SWITCH -> print_value (MenhirInterpreter.T T_SWITCH) ()
  | SUPPRESS -> print_value (MenhirInterpreter.T T_SUPPRESS) ()
  | SUPER -> print_value (MenhirInterpreter.T T_SUPER) ()
  | SUM -> print_value (MenhirInterpreter.T T_SUM) ()
  | SUFFIXING -> print_value (MenhirInterpreter.T T_SUFFIXING) ()
  | SUB_SCHEMA -> print_value (MenhirInterpreter.T T_SUB_SCHEMA) ()
  | SUB_QUEUE_3 -> print_value (MenhirInterpreter.T T_SUB_QUEUE_3) ()
  | SUB_QUEUE_2 -> print_value (MenhirInterpreter.T T_SUB_QUEUE_2) ()
  | SUB_QUEUE_1 -> print_value (MenhirInterpreter.T T_SUB_QUEUE_1) ()
  | SUBWINDOW -> print_value (MenhirInterpreter.T T_SUBWINDOW) ()
  | SUBTRACT -> print_value (MenhirInterpreter.T T_SUBTRACT) ()
  | SUBFILE -> print_value (MenhirInterpreter.T T_SUBFILE) ()
  | STYLE -> print_value (MenhirInterpreter.T T_STYLE) ()
  | STRUCTURE -> print_value (MenhirInterpreter.T T_STRUCTURE) ()
  | STRONG_NAME -> print_value (MenhirInterpreter.T T_STRONG_NAME) ()
  | STRONG -> print_value (MenhirInterpreter.T T_STRONG) ()
  | STRING -> print_value (MenhirInterpreter.T T_STRING) ()
  | STOP_BROWSER -> print_value (MenhirInterpreter.T T_STOP_BROWSER) ()
  | STOP -> print_value (MenhirInterpreter.T T_STOP) ()
  | STEP -> print_value (MenhirInterpreter.T T_STEP) ()
  | STDCALL -> print_value (MenhirInterpreter.T T_STDCALL) ()
  | STATUS_TEXT -> print_value (MenhirInterpreter.T T_STATUS_TEXT) ()
  | STATUS_BAR -> print_value (MenhirInterpreter.T T_STATUS_BAR) ()
  | STATUS -> print_value (MenhirInterpreter.T T_STATUS) ()
  | STATION -> print_value (MenhirInterpreter.T T_STATION) ()
  | STATIC_LIST -> print_value (MenhirInterpreter.T T_STATIC_LIST) ()
  | STATIC -> print_value (MenhirInterpreter.T T_STATIC) ()
  | STATEMENT -> print_value (MenhirInterpreter.T T_STATEMENT) ()
  | START_Y -> print_value (MenhirInterpreter.T T_START_Y) ()
  | START_X -> print_value (MenhirInterpreter.T T_START_X) ()
  | STARTING -> print_value (MenhirInterpreter.T T_STARTING) ()
  | START -> print_value (MenhirInterpreter.T T_START) ()
  | STANDARD_DECIMAL -> print_value (MenhirInterpreter.T T_STANDARD_DECIMAL) ()
  | STANDARD_BINARY -> print_value (MenhirInterpreter.T T_STANDARD_BINARY) ()
  | STANDARD_2 -> print_value (MenhirInterpreter.T T_STANDARD_2) ()
  | STANDARD_1 -> print_value (MenhirInterpreter.T T_STANDARD_1) ()
  | STANDARD -> print_value (MenhirInterpreter.T T_STANDARD) ()
  | STACK -> print_value (MenhirInterpreter.T T_STACK) ()
  | SSF -> print_value (MenhirInterpreter.T T_SSF) ()
  | SQUARE -> print_value (MenhirInterpreter.T T_SQUARE) ()
  | SQL_ROWID -> print_value (MenhirInterpreter.T T_SQL_ROWID) ()
  | SQL_NCLOB -> print_value (MenhirInterpreter.T T_SQL_NCLOB) ()
  | SQL_CURSOR -> print_value (MenhirInterpreter.T T_SQL_CURSOR) ()
  | SQL_CLOB -> print_value (MenhirInterpreter.T T_SQL_CLOB) ()
  | SQL_BLOB -> print_value (MenhirInterpreter.T T_SQL_BLOB) ()
  | SQL_BFILE -> print_value (MenhirInterpreter.T T_SQL_BFILE) ()
  | SQLIMS -> print_value (MenhirInterpreter.T T_SQLIMS) ()
  | SQL -> print_value (MenhirInterpreter.T T_SQL) ()
  | SPINNER -> print_value (MenhirInterpreter.T T_SPINNER) ()
  | SPECIAL_NAMES -> print_value (MenhirInterpreter.T T_SPECIAL_NAMES) ()
  | SPACE_FILL -> print_value (MenhirInterpreter.T T_SPACE_FILL) ()
  | SPACE -> print_value (MenhirInterpreter.T T_SPACE) ()
  | SOURCE_COMPUTER -> print_value (MenhirInterpreter.T T_SOURCE_COMPUTER) ()
  | SOURCES -> print_value (MenhirInterpreter.T T_SOURCES) ()
  | SOURCE -> print_value (MenhirInterpreter.T T_SOURCE) ()
  | SORT_WORK -> print_value (MenhirInterpreter.T T_SORT_WORK) ()
  | SORT_RETURN -> print_value (MenhirInterpreter.T T_SORT_RETURN) ()
  | SORT_ORDER -> print_value (MenhirInterpreter.T T_SORT_ORDER) ()
  | SORT_MODE_SIZE -> print_value (MenhirInterpreter.T T_SORT_MODE_SIZE) ()
  | SORT_MESSAGE -> print_value (MenhirInterpreter.T T_SORT_MESSAGE) ()
  | SORT_MERGE -> print_value (MenhirInterpreter.T T_SORT_MERGE) ()
  | SORT_FILE_SIZE -> print_value (MenhirInterpreter.T T_SORT_FILE_SIZE) ()
  | SORT_CORE_SIZE -> print_value (MenhirInterpreter.T T_SORT_CORE_SIZE) ()
  | SORT_CONTROL -> print_value (MenhirInterpreter.T T_SORT_CONTROL) ()
  | SORT -> print_value (MenhirInterpreter.T T_SORT) ()
  | SMALL_FONT -> print_value (MenhirInterpreter.T T_SMALL_FONT) ()
  | SLASH -> print_value (MenhirInterpreter.T T_SLASH) ()
  | SKIP3 -> print_value (MenhirInterpreter.T T_SKIP3) ()
  | SKIP2 -> print_value (MenhirInterpreter.T T_SKIP2) ()
  | SKIP1 -> print_value (MenhirInterpreter.T T_SKIP1) ()
  | SIZE -> print_value (MenhirInterpreter.T T_SIZE) ()
  | SINTLIT v -> print_value (MenhirInterpreter.T T_SINTLIT) v
  | SIGNED_SHORT -> print_value (MenhirInterpreter.T T_SIGNED_SHORT) ()
  | SIGNED_LONG -> print_value (MenhirInterpreter.T T_SIGNED_LONG) ()
  | SIGNED_INT -> print_value (MenhirInterpreter.T T_SIGNED_INT) ()
  | SIGNED -> print_value (MenhirInterpreter.T T_SIGNED) ()
  | SIGN -> print_value (MenhirInterpreter.T T_SIGN) ()
  | SHOW_SEL_ALWAYS -> print_value (MenhirInterpreter.T T_SHOW_SEL_ALWAYS) ()
  | SHOW_NONE -> print_value (MenhirInterpreter.T T_SHOW_NONE) ()
  | SHOW_LINES -> print_value (MenhirInterpreter.T T_SHOW_LINES) ()
  | SHORT_DATE -> print_value (MenhirInterpreter.T T_SHORT_DATE) ()
  | SHORT -> print_value (MenhirInterpreter.T T_SHORT) ()
  | SHIFT_OUT -> print_value (MenhirInterpreter.T T_SHIFT_OUT) ()
  | SHIFT_IN -> print_value (MenhirInterpreter.T T_SHIFT_IN) ()
  | SHARING -> print_value (MenhirInterpreter.T T_SHARING) ()
  | SHADOW -> print_value (MenhirInterpreter.T T_SHADOW) ()
  | SHADING -> print_value (MenhirInterpreter.T T_SHADING) ()
  | SET -> print_value (MenhirInterpreter.T T_SET) ()
  | SERVICE -> print_value (MenhirInterpreter.T T_SERVICE) ()
  | SEQUENTIAL -> print_value (MenhirInterpreter.T T_SEQUENTIAL) ()
  | SEQUENCE -> print_value (MenhirInterpreter.T T_SEQUENCE) ()
  | SEPARATION -> print_value (MenhirInterpreter.T T_SEPARATION) ()
  | SEPARATE -> print_value (MenhirInterpreter.T T_SEPARATE) ()
  | SENTENCE -> print_value (MenhirInterpreter.T T_SENTENCE) ()
  | SEND -> print_value (MenhirInterpreter.T T_SEND) ()
  | SEMAPHORE_POINTER -> print_value (MenhirInterpreter.T T_SEMAPHORE_POINTER) ()
  | SELF_ACT -> print_value (MenhirInterpreter.T T_SELF_ACT) ()
  | SELFCLASS -> print_value (MenhirInterpreter.T T_SELFCLASS) ()
  | SELF -> print_value (MenhirInterpreter.T T_SELF) ()
  | SELECT_ALL -> print_value (MenhirInterpreter.T T_SELECT_ALL) ()
  | SELECTIVE -> print_value (MenhirInterpreter.T T_SELECTIVE) ()
  | SELECTION_TEXT -> print_value (MenhirInterpreter.T T_SELECTION_TEXT) ()
  | SELECTION_INDEX -> print_value (MenhirInterpreter.T T_SELECTION_INDEX) ()
  | SELECTION -> print_value (MenhirInterpreter.T T_SELECTION) ()
  | SELECT -> print_value (MenhirInterpreter.T T_SELECT) ()
  | SEGMENT_LIMIT -> print_value (MenhirInterpreter.T T_SEGMENT_LIMIT) ()
  | SEGMENT -> print_value (MenhirInterpreter.T T_SEGMENT) ()
  | SEEK -> print_value (MenhirInterpreter.T T_SEEK) ()
  | SECURITY -> print_value (MenhirInterpreter.T T_SECURITY) ()
  | SECURE -> print_value (MenhirInterpreter.T T_SECURE) ()
  | SECTION -> print_value (MenhirInterpreter.T T_SECTION) ()
  | SECONDS -> print_value (MenhirInterpreter.T T_SECONDS) ()
  | SECONDARY -> print_value (MenhirInterpreter.T T_SECONDARY) ()
  | SEARCH_TEXT -> print_value (MenhirInterpreter.T T_SEARCH_TEXT) ()
  | SEARCH_OPTIONS -> print_value (MenhirInterpreter.T T_SEARCH_OPTIONS) ()
  | SEARCH -> print_value (MenhirInterpreter.T T_SEARCH) ()
  | SD -> print_value (MenhirInterpreter.T T_SD) ()
  | SCROLL_BAR -> print_value (MenhirInterpreter.T T_SCROLL_BAR) ()
  | SCROLL -> print_value (MenhirInterpreter.T T_SCROLL) ()
  | SCREEN -> print_value (MenhirInterpreter.T T_SCREEN) ()
  | SAVE_AS_NO_PROMPT -> print_value (MenhirInterpreter.T T_SAVE_AS_NO_PROMPT) ()
  | SAVE_AS -> print_value (MenhirInterpreter.T T_SAVE_AS) ()
  | SARF -> print_value (MenhirInterpreter.T T_SARF) ()
  | SAME -> print_value (MenhirInterpreter.T T_SAME) ()
  | S -> print_value (MenhirInterpreter.T T_S) ()
  | RUN -> print_value (MenhirInterpreter.T T_RUN) ()
  | RPAR -> print_value (MenhirInterpreter.T T_RPAR) ()
  | ROW_PROTECTION -> print_value (MenhirInterpreter.T T_ROW_PROTECTION) ()
  | ROW_HEADINGS -> print_value (MenhirInterpreter.T T_ROW_HEADINGS) ()
  | ROW_FONT -> print_value (MenhirInterpreter.T T_ROW_FONT) ()
  | ROW_DIVIDERS -> print_value (MenhirInterpreter.T T_ROW_DIVIDERS) ()
  | ROW_COLOR_PATTERN -> print_value (MenhirInterpreter.T T_ROW_COLOR_PATTERN) ()
  | ROW_COLOR -> print_value (MenhirInterpreter.T T_ROW_COLOR) ()
  | ROWID -> print_value (MenhirInterpreter.T T_ROWID) ()
  | ROUNDING -> print_value (MenhirInterpreter.T T_ROUNDING) ()
  | ROUNDED -> print_value (MenhirInterpreter.T T_ROUNDED) ()
  | ROLLING -> print_value (MenhirInterpreter.T T_ROLLING) ()
  | ROLLBACK -> print_value (MenhirInterpreter.T T_ROLLBACK) ()
  | RIMMED -> print_value (MenhirInterpreter.T T_RIMMED) ()
  | RIGHT_JUSTIFY -> print_value (MenhirInterpreter.T T_RIGHT_JUSTIFY) ()
  | RIGHT_ALIGN -> print_value (MenhirInterpreter.T T_RIGHT_ALIGN) ()
  | RIGHT -> print_value (MenhirInterpreter.T T_RIGHT) ()
  | RH -> print_value (MenhirInterpreter.T T_RH) ()
  | RF -> print_value (MenhirInterpreter.T T_RF) ()
  | REWRITE -> print_value (MenhirInterpreter.T T_REWRITE) ()
  | REWIND -> print_value (MenhirInterpreter.T T_REWIND) ()
  | REVERSE_VIDEO -> print_value (MenhirInterpreter.T T_REVERSE_VIDEO) ()
  | REVERSED -> print_value (MenhirInterpreter.T T_REVERSED) ()
  | REVERSE -> print_value (MenhirInterpreter.T T_REVERSE) ()
  | RETURN_UNSIGNED -> print_value (MenhirInterpreter.T T_RETURN_UNSIGNED) ()
  | RETURN_CODE -> print_value (MenhirInterpreter.T T_RETURN_CODE) ()
  | RETURNING -> print_value (MenhirInterpreter.T T_RETURNING) ()
  | RETURN -> print_value (MenhirInterpreter.T T_RETURN) ()
  | RETRY -> print_value (MenhirInterpreter.T T_RETRY) ()
  | RETENTION -> print_value (MenhirInterpreter.T T_RETENTION) ()
  | RESUME -> print_value (MenhirInterpreter.T T_RESUME) ()
  | RESTRICTED -> print_value (MenhirInterpreter.T T_RESTRICTED) ()
  | RESIZABLE -> print_value (MenhirInterpreter.T T_RESIZABLE) ()
  | RESIDENT -> print_value (MenhirInterpreter.T T_RESIDENT) ()
  | RESET_TABS -> print_value (MenhirInterpreter.T T_RESET_TABS) ()
  | RESET_SET_LOCATOR -> print_value (MenhirInterpreter.T T_RESET_SET_LOCATOR) ()
  | RESET_LIST -> print_value (MenhirInterpreter.T T_RESET_LIST) ()
  | RESET_GRID -> print_value (MenhirInterpreter.T T_RESET_GRID) ()
  | RESET -> print_value (MenhirInterpreter.T T_RESET) ()
  | RESERVE -> print_value (MenhirInterpreter.T T_RESERVE) ()
  | RERUN -> print_value (MenhirInterpreter.T T_RERUN) ()
  | REREAD -> print_value (MenhirInterpreter.T T_REREAD) ()
  | REQUIRED -> print_value (MenhirInterpreter.T T_REQUIRED) ()
  | REPOSITORY -> print_value (MenhirInterpreter.T T_REPOSITORY) ()
  | REPORTS -> print_value (MenhirInterpreter.T T_REPORTS) ()
  | REPORTING -> print_value (MenhirInterpreter.T T_REPORTING) ()
  | REPORT -> print_value (MenhirInterpreter.T T_REPORT) ()
  | REPLACING -> print_value (MenhirInterpreter.T T_REPLACING) ()
  | REPLACED -> print_value (MenhirInterpreter.T T_REPLACED) ()
  | REPLACE -> print_value (MenhirInterpreter.T T_REPLACE) ()
  | REPEATED -> print_value (MenhirInterpreter.T T_REPEATED) ()
  | REORG_CRITERIA -> print_value (MenhirInterpreter.T T_REORG_CRITERIA) ()
  | RENAMES -> print_value (MenhirInterpreter.T T_RENAMES) ()
  | REMOVAL -> print_value (MenhirInterpreter.T T_REMOVAL) ()
  | REMARKS -> print_value (MenhirInterpreter.T T_REMARKS) ()
  | REMAINDER -> print_value (MenhirInterpreter.T T_REMAINDER) ()
  | RELOAD -> print_value (MenhirInterpreter.T T_RELOAD) ()
  | RELEASE -> print_value (MenhirInterpreter.T T_RELEASE) ()
  | RELATIVE -> print_value (MenhirInterpreter.T T_RELATIVE) ()
  | RELATION -> print_value (MenhirInterpreter.T T_RELATION) ()
  | REGION_COLOR -> print_value (MenhirInterpreter.T T_REGION_COLOR) ()
  | REFRESH -> print_value (MenhirInterpreter.T T_REFRESH) ()
  | REFERENCES -> print_value (MenhirInterpreter.T T_REFERENCES) ()
  | REFERENCE -> print_value (MenhirInterpreter.T T_REFERENCE) ()
  | REEL -> print_value (MenhirInterpreter.T T_REEL) ()
  | REDEFINITION -> print_value (MenhirInterpreter.T T_REDEFINITION) ()
  | REDEFINES -> print_value (MenhirInterpreter.T T_REDEFINES) ()
  | RECURSIVE -> print_value (MenhirInterpreter.T T_RECURSIVE) ()
  | RECORD_TO_DELETE -> print_value (MenhirInterpreter.T T_RECORD_TO_DELETE) ()
  | RECORD_TO_ADD -> print_value (MenhirInterpreter.T T_RECORD_TO_ADD) ()
  | RECORD_POSITION -> print_value (MenhirInterpreter.T T_RECORD_POSITION) ()
  | RECORD_OVERFLOW -> print_value (MenhirInterpreter.T T_RECORD_OVERFLOW) ()
  | RECORD_DATA -> print_value (MenhirInterpreter.T T_RECORD_DATA) ()
  | RECORDS -> print_value (MenhirInterpreter.T T_RECORDS) ()
  | RECORDING -> print_value (MenhirInterpreter.T T_RECORDING) ()
  | RECORD -> print_value (MenhirInterpreter.T T_RECORD) ()
  | RECEIVED -> print_value (MenhirInterpreter.T T_RECEIVED) ()
  | RECEIVE -> print_value (MenhirInterpreter.T T_RECEIVE) ()
  | READ_ONLY -> print_value (MenhirInterpreter.T T_READ_ONLY) ()
  | READY -> print_value (MenhirInterpreter.T T_READY) ()
  | READING -> print_value (MenhirInterpreter.T T_READING) ()
  | READERS -> print_value (MenhirInterpreter.T T_READERS) ()
  | READ -> print_value (MenhirInterpreter.T T_READ) ()
  | RD -> print_value (MenhirInterpreter.T T_RD) ()
  | RANGE -> print_value (MenhirInterpreter.T T_RANGE) ()
  | RANDOM -> print_value (MenhirInterpreter.T T_RANDOM) ()
  | RAISING -> print_value (MenhirInterpreter.T T_RAISING) ()
  | RAISED -> print_value (MenhirInterpreter.T T_RAISED) ()
  | RAISE -> print_value (MenhirInterpreter.T T_RAISE) ()
  | RADIO_BUTTON -> print_value (MenhirInterpreter.T T_RADIO_BUTTON) ()
  | QUOTE -> print_value (MenhirInterpreter.T T_QUOTE) ()
  | QUEUED -> print_value (MenhirInterpreter.T T_QUEUED) ()
  | QUEUE -> print_value (MenhirInterpreter.T T_QUEUE) ()
  | QUERY_INDEX -> print_value (MenhirInterpreter.T T_QUERY_INDEX) ()
  | PUSH_BUTTON -> print_value (MenhirInterpreter.T T_PUSH_BUTTON) ()
  | PURGE -> print_value (MenhirInterpreter.T T_PURGE) ()
  | PUBLIC -> print_value (MenhirInterpreter.T T_PUBLIC) ()
  | PROTOTYPE -> print_value (MenhirInterpreter.T T_PROTOTYPE) ()
  | PROTECTED -> print_value (MenhirInterpreter.T T_PROTECTED) ()
  | PROPERTY -> print_value (MenhirInterpreter.T T_PROPERTY) ()
  | PROPERTIES -> print_value (MenhirInterpreter.T T_PROPERTIES) ()
  | PROMPT -> print_value (MenhirInterpreter.T T_PROMPT) ()
  | PROHIBITED -> print_value (MenhirInterpreter.T T_PROHIBITED) ()
  | PROGRESS -> print_value (MenhirInterpreter.T T_PROGRESS) ()
  | PROGRAM_POINTER -> print_value (MenhirInterpreter.T T_PROGRAM_POINTER) ()
  | PROGRAM_ID -> print_value (MenhirInterpreter.T T_PROGRAM_ID) ()
  | PROGRAM -> print_value (MenhirInterpreter.T T_PROGRAM) ()
  | PROCESS_AREA -> print_value (MenhirInterpreter.T T_PROCESS_AREA) ()
  | PROCESSING -> print_value (MenhirInterpreter.T T_PROCESSING) ()
  | PROCESS -> print_value (MenhirInterpreter.T T_PROCESS) ()
  | PROCEED -> print_value (MenhirInterpreter.T T_PROCEED) ()
  | PROCEDURE_POINTER -> print_value (MenhirInterpreter.T T_PROCEDURE_POINTER) ()
  | PROCEDURE_NAME -> print_value (MenhirInterpreter.T T_PROCEDURE_NAME) ()
  | PROCEDURES -> print_value (MenhirInterpreter.T T_PROCEDURES) ()
  | PROCEDURE -> print_value (MenhirInterpreter.T T_PROCEDURE) ()
  | PRIVATE -> print_value (MenhirInterpreter.T T_PRIVATE) ()
  | PRIORITY -> print_value (MenhirInterpreter.T T_PRIORITY) ()
  | PRIOR -> print_value (MenhirInterpreter.T T_PRIOR) ()
  | PRINT_PREVIEW -> print_value (MenhirInterpreter.T T_PRINT_PREVIEW) ()
  | PRINT_NO_PROMPT -> print_value (MenhirInterpreter.T T_PRINT_NO_PROMPT) ()
  | PRINT_CONTROL -> print_value (MenhirInterpreter.T T_PRINT_CONTROL) ()
  | PRINTING -> print_value (MenhirInterpreter.T T_PRINTING) ()
  | PRINTER_1 -> print_value (MenhirInterpreter.T T_PRINTER_1) ()
  | PRINTER -> print_value (MenhirInterpreter.T T_PRINTER) ()
  | PRINT -> print_value (MenhirInterpreter.T T_PRINT) ()
  | PRIMARY -> print_value (MenhirInterpreter.T T_PRIMARY) ()
  | PREVIOUS -> print_value (MenhirInterpreter.T T_PREVIOUS) ()
  | PRESENT -> print_value (MenhirInterpreter.T T_PRESENT) ()
  | PREFIXING -> print_value (MenhirInterpreter.T T_PREFIXING) ()
  | PREFIXED -> print_value (MenhirInterpreter.T T_PREFIXED) ()
  | POSITIVE -> print_value (MenhirInterpreter.T T_POSITIVE) ()
  | POSITION_SHIFT -> print_value (MenhirInterpreter.T T_POSITION_SHIFT) ()
  | POSITIONING -> print_value (MenhirInterpreter.T T_POSITIONING) ()
  | POSITION -> print_value (MenhirInterpreter.T T_POSITION) ()
  | POS -> print_value (MenhirInterpreter.T T_POS) ()
  | POP_UP -> print_value (MenhirInterpreter.T T_POP_UP) ()
  | POINTER_32 -> print_value (MenhirInterpreter.T T_POINTER_32) ()
  | POINTER -> print_value (MenhirInterpreter.T T_POINTER) ()
  | PLUS_SIGN -> print_value (MenhirInterpreter.T T_PLUS_SIGN) ()
  | PLUS -> print_value (MenhirInterpreter.T T_PLUS) ()
  | PLACEMENT -> print_value (MenhirInterpreter.T T_PLACEMENT) ()
  | PIXEL -> print_value (MenhirInterpreter.T T_PIXEL) ()
  | PICTURE_STRING v -> print_value (MenhirInterpreter.T T_PICTURE_STRING) v
  | PICTURE -> print_value (MenhirInterpreter.T T_PICTURE) ()
  | PHYSICAL -> print_value (MenhirInterpreter.T T_PHYSICAL) ()
  | PH -> print_value (MenhirInterpreter.T T_PH) ()
  | PF -> print_value (MenhirInterpreter.T T_PF) ()
  | PERMANENT -> print_value (MenhirInterpreter.T T_PERMANENT) ()
  | PERIOD -> print_value (MenhirInterpreter.T T_PERIOD) ()
  | PERFORM -> print_value (MenhirInterpreter.T T_PERFORM) ()
  | PASSWORD -> print_value (MenhirInterpreter.T T_PASSWORD) ()
  | PASCAL -> print_value (MenhirInterpreter.T T_PASCAL) ()
  | PARSE -> print_value (MenhirInterpreter.T T_PARSE) ()
  | PARENT -> print_value (MenhirInterpreter.T T_PARENT) ()
  | PARAGRAPH -> print_value (MenhirInterpreter.T T_PARAGRAPH) ()
  | PANEL_WIDTHS -> print_value (MenhirInterpreter.T T_PANEL_WIDTHS) ()
  | PANEL_TEXT -> print_value (MenhirInterpreter.T T_PANEL_TEXT) ()
  | PANEL_STYLE -> print_value (MenhirInterpreter.T T_PANEL_STYLE) ()
  | PANEL_INDEX -> print_value (MenhirInterpreter.T T_PANEL_INDEX) ()
  | PAGE_SIZE -> print_value (MenhirInterpreter.T T_PAGE_SIZE) ()
  | PAGE_SETUP -> print_value (MenhirInterpreter.T T_PAGE_SETUP) ()
  | PAGE_COUNTER -> print_value (MenhirInterpreter.T T_PAGE_COUNTER) ()
  | PAGED -> print_value (MenhirInterpreter.T T_PAGED) ()
  | PAGE -> print_value (MenhirInterpreter.T T_PAGE) ()
  | PADDING -> print_value (MenhirInterpreter.T T_PADDING) ()
  | PACKED_DECIMAL -> print_value (MenhirInterpreter.T T_PACKED_DECIMAL) ()
  | O_FILL -> print_value (MenhirInterpreter.T T_O_FILL) ()
  | OVERRIDING -> print_value (MenhirInterpreter.T T_OVERRIDING) ()
  | OVERRIDE -> print_value (MenhirInterpreter.T T_OVERRIDE) ()
  | OVERLINE -> print_value (MenhirInterpreter.T T_OVERLINE) ()
  | OVERLAP_TOP -> print_value (MenhirInterpreter.T T_OVERLAP_TOP) ()
  | OVERLAP_LEFT -> print_value (MenhirInterpreter.T T_OVERLAP_LEFT) ()
  | OVERLAPPED -> print_value (MenhirInterpreter.T T_OVERLAPPED) ()
  | OVERFLOW -> print_value (MenhirInterpreter.T T_OVERFLOW) ()
  | OUTPUT -> print_value (MenhirInterpreter.T T_OUTPUT) ()
  | OTHERWISE -> print_value (MenhirInterpreter.T T_OTHERWISE) ()
  | OTHERS -> print_value (MenhirInterpreter.T T_OTHERS) ()
  | OTHER -> print_value (MenhirInterpreter.T T_OTHER) ()
  | ORGANIZATION -> print_value (MenhirInterpreter.T T_ORGANIZATION) ()
  | ORDER -> print_value (MenhirInterpreter.T T_ORDER) ()
  | OR -> print_value (MenhirInterpreter.T T_OR) ()
  | OPTIONS -> print_value (MenhirInterpreter.T T_OPTIONS) ()
  | OPTIONAL -> print_value (MenhirInterpreter.T T_OPTIONAL) ()
  | OPERATIONAL -> print_value (MenhirInterpreter.T T_OPERATIONAL) ()
  | OPEN -> print_value (MenhirInterpreter.T T_OPEN) ()
  | OOSTACKPTR -> print_value (MenhirInterpreter.T T_OOSTACKPTR) ()
  | ON_SIZE_ERROR -> print_value (MenhirInterpreter.T T_ON_SIZE_ERROR) ()
  | ON_OVERFLOW -> print_value (MenhirInterpreter.T T_ON_OVERFLOW) ()
  | ON_EXCEPTION -> print_value (MenhirInterpreter.T T_ON_EXCEPTION) ()
  | ONLY -> print_value (MenhirInterpreter.T T_ONLY) ()
  | ON -> print_value (MenhirInterpreter.T T_ON) ()
  | OMITTED -> print_value (MenhirInterpreter.T T_OMITTED) ()
  | OK_BUTTON -> print_value (MenhirInterpreter.T T_OK_BUTTON) ()
  | OFF -> print_value (MenhirInterpreter.T T_OFF) ()
  | OF -> print_value (MenhirInterpreter.T T_OF) ()
  | OCCURS -> print_value (MenhirInterpreter.T T_OCCURS) ()
  | OBJECT_STORAGE -> print_value (MenhirInterpreter.T T_OBJECT_STORAGE) ()
  | OBJECT_REFERENCE -> print_value (MenhirInterpreter.T T_OBJECT_REFERENCE) ()
  | OBJECT_PROGRAM -> print_value (MenhirInterpreter.T T_OBJECT_PROGRAM) ()
  | OBJECT_ID -> print_value (MenhirInterpreter.T T_OBJECT_ID) ()
  | OBJECT_COMPUTER -> print_value (MenhirInterpreter.T T_OBJECT_COMPUTER) ()
  | OBJECT -> print_value (MenhirInterpreter.T T_OBJECT) ()
  | NUM_ROW_HEADINGS -> print_value (MenhirInterpreter.T T_NUM_ROW_HEADINGS) ()
  | NUM_ROWS -> print_value (MenhirInterpreter.T T_NUM_ROWS) ()
  | NUM_COL_HEADINGS -> print_value (MenhirInterpreter.T T_NUM_COL_HEADINGS) ()
  | NUMERIC_FILL -> print_value (MenhirInterpreter.T T_NUMERIC_FILL) ()
  | NUMERIC_EDITED -> print_value (MenhirInterpreter.T T_NUMERIC_EDITED) ()
  | NUMERIC -> print_value (MenhirInterpreter.T T_NUMERIC) ()
  | NUMBERS -> print_value (MenhirInterpreter.T T_NUMBERS) ()
  | NUMBER -> print_value (MenhirInterpreter.T T_NUMBER) ()
  | NULLS -> print_value (MenhirInterpreter.T T_NULLS) ()
  | NULLIT v -> print_value (MenhirInterpreter.T T_NULLIT) v
  | NULL -> print_value (MenhirInterpreter.T T_NULL) ()
  | NO_UPDOWN -> print_value (MenhirInterpreter.T T_NO_UPDOWN) ()
  | NO_TAB -> print_value (MenhirInterpreter.T T_NO_TAB) ()
  | NO_SEARCH -> print_value (MenhirInterpreter.T T_NO_SEARCH) ()
  | NO_KEY_LETTER -> print_value (MenhirInterpreter.T T_NO_KEY_LETTER) ()
  | NO_GROUP_TAB -> print_value (MenhirInterpreter.T T_NO_GROUP_TAB) ()
  | NO_FOCUS -> print_value (MenhirInterpreter.T T_NO_FOCUS) ()
  | NO_F4 -> print_value (MenhirInterpreter.T T_NO_F4) ()
  | NO_ECHO -> print_value (MenhirInterpreter.T T_NO_ECHO) ()
  | NO_DIVIDERS -> print_value (MenhirInterpreter.T T_NO_DIVIDERS) ()
  | NO_DATA -> print_value (MenhirInterpreter.T T_NO_DATA) ()
  | NO_CLOSE -> print_value (MenhirInterpreter.T T_NO_CLOSE) ()
  | NO_CELL_DRAG -> print_value (MenhirInterpreter.T T_NO_CELL_DRAG) ()
  | NO_BOX -> print_value (MenhirInterpreter.T T_NO_BOX) ()
  | NO_AUTO_DEFAULT -> print_value (MenhirInterpreter.T T_NO_AUTO_DEFAULT) ()
  | NO_AUTOSEL -> print_value (MenhirInterpreter.T T_NO_AUTOSEL) ()
  | NOT_ON_SIZE_ERROR -> print_value (MenhirInterpreter.T T_NOT_ON_SIZE_ERROR) ()
  | NOT_ON_OVERFLOW -> print_value (MenhirInterpreter.T T_NOT_ON_OVERFLOW) ()
  | NOT_ON_EXCEPTION -> print_value (MenhirInterpreter.T T_NOT_ON_EXCEPTION) ()
  | NOT_INVALID_KEY -> print_value (MenhirInterpreter.T T_NOT_INVALID_KEY) ()
  | NOT_AT_EOP -> print_value (MenhirInterpreter.T T_NOT_AT_EOP) ()
  | NOT_AT_END -> print_value (MenhirInterpreter.T T_NOT_AT_END) ()
  | NOTIFY_SELCHANGE -> print_value (MenhirInterpreter.T T_NOTIFY_SELCHANGE) ()
  | NOTIFY_DBLCLICK -> print_value (MenhirInterpreter.T T_NOTIFY_DBLCLICK) ()
  | NOTIFY_CHANGE -> print_value (MenhirInterpreter.T T_NOTIFY_CHANGE) ()
  | NOTIFY -> print_value (MenhirInterpreter.T T_NOTIFY) ()
  | NOTHING -> print_value (MenhirInterpreter.T T_NOTHING) ()
  | NOTE -> print_value (MenhirInterpreter.T T_NOTE) ()
  | NOTAB -> print_value (MenhirInterpreter.T T_NOTAB) ()
  | NOT -> print_value (MenhirInterpreter.T T_NOT) ()
  | NORMAL -> print_value (MenhirInterpreter.T T_NORMAL) ()
  | NONNUMERIC -> print_value (MenhirInterpreter.T T_NONNUMERIC) ()
  | NONE -> print_value (MenhirInterpreter.T T_NONE) ()
  | NOMINAL -> print_value (MenhirInterpreter.T T_NOMINAL) ()
  | NO -> print_value (MenhirInterpreter.T T_NO) ()
  | NEXT_PAGE -> print_value (MenhirInterpreter.T T_NEXT_PAGE) ()
  | NEXT_ITEM -> print_value (MenhirInterpreter.T T_NEXT_ITEM) ()
  | NEXT -> print_value (MenhirInterpreter.T T_NEXT) ()
  | NEW -> print_value (MenhirInterpreter.T T_NEW) ()
  | NET_EVENT_LIST -> print_value (MenhirInterpreter.T T_NET_EVENT_LIST) ()
  | NESTED -> print_value (MenhirInterpreter.T T_NESTED) ()
  | NEGATIVE -> print_value (MenhirInterpreter.T T_NEGATIVE) ()
  | NEAREST_TO_ZERO -> print_value (MenhirInterpreter.T T_NEAREST_TO_ZERO) ()
  | NEAREST_TOWARD_ZERO -> print_value (MenhirInterpreter.T T_NEAREST_TOWARD_ZERO) ()
  | NEAREST_EVEN -> print_value (MenhirInterpreter.T T_NEAREST_EVEN) ()
  | NEAREST_AWAY_FROM_ZERO -> print_value (MenhirInterpreter.T T_NEAREST_AWAY_FROM_ZERO) ()
  | NE -> print_value (MenhirInterpreter.T T_NE) ()
  | NCLOB -> print_value (MenhirInterpreter.T T_NCLOB) ()
  | NCHAR -> print_value (MenhirInterpreter.T T_NCHAR) ()
  | NAVIGATE_URL -> print_value (MenhirInterpreter.T T_NAVIGATE_URL) ()
  | NATLIT v -> print_value (MenhirInterpreter.T T_NATLIT) v
  | NATIVE -> print_value (MenhirInterpreter.T T_NATIVE) ()
  | NATIONAL_EDITED -> print_value (MenhirInterpreter.T T_NATIONAL_EDITED) ()
  | NATIONAL -> print_value (MenhirInterpreter.T T_NATIONAL) ()
  | NAT -> print_value (MenhirInterpreter.T T_NAT) ()
  | NAMESPACE_PREFIX -> print_value (MenhirInterpreter.T T_NAMESPACE_PREFIX) ()
  | NAMESPACE -> print_value (MenhirInterpreter.T T_NAMESPACE) ()
  | NAMED -> print_value (MenhirInterpreter.T T_NAMED) ()
  | NAME -> print_value (MenhirInterpreter.T T_NAME) ()
  | MUTEX_POINTER -> print_value (MenhirInterpreter.T T_MUTEX_POINTER) ()
  | MULTIPLY -> print_value (MenhirInterpreter.T T_MULTIPLY) ()
  | MULTIPLE -> print_value (MenhirInterpreter.T T_MULTIPLE) ()
  | MULTILINE -> print_value (MenhirInterpreter.T T_MULTILINE) ()
  | MOVE -> print_value (MenhirInterpreter.T T_MOVE) ()
  | MORE_LABELS -> print_value (MenhirInterpreter.T T_MORE_LABELS) ()
  | MONITOR_POINTER -> print_value (MenhirInterpreter.T T_MONITOR_POINTER) ()
  | MODULES -> print_value (MenhirInterpreter.T T_MODULES) ()
  | MODULE -> print_value (MenhirInterpreter.T T_MODULE) ()
  | MODIFY -> print_value (MenhirInterpreter.T T_MODIFY) ()
  | MODIFIED -> print_value (MenhirInterpreter.T T_MODIFIED) ()
  | MODELESS -> print_value (MenhirInterpreter.T T_MODELESS) ()
  | MODE -> print_value (MenhirInterpreter.T T_MODE) ()
  | MODAL -> print_value (MenhirInterpreter.T T_MODAL) ()
  | MIN_WIDTH -> print_value (MenhirInterpreter.T T_MIN_WIDTH) ()
  | MIN_VALUE -> print_value (MenhirInterpreter.T T_MIN_VALUE) ()
  | MIN_VAL -> print_value (MenhirInterpreter.T T_MIN_VAL) ()
  | MIN_SIZE -> print_value (MenhirInterpreter.T T_MIN_SIZE) ()
  | MIN_LINES -> print_value (MenhirInterpreter.T T_MIN_LINES) ()
  | MIN_HEIGHT -> print_value (MenhirInterpreter.T T_MIN_HEIGHT) ()
  | MINUS -> print_value (MenhirInterpreter.T T_MINUS) ()
  | MICROSECOND_TIME -> print_value (MenhirInterpreter.T T_MICROSECOND_TIME) ()
  | METHOD_ID -> print_value (MenhirInterpreter.T T_METHOD_ID) ()
  | METHOD -> print_value (MenhirInterpreter.T T_METHOD) ()
  | META_CLASS -> print_value (MenhirInterpreter.T T_META_CLASS) ()
  | MESSAGE_TAG -> print_value (MenhirInterpreter.T T_MESSAGE_TAG) ()
  | MESSAGES -> print_value (MenhirInterpreter.T T_MESSAGES) ()
  | MESSAGE -> print_value (MenhirInterpreter.T T_MESSAGE) ()
  | MERGE -> print_value (MenhirInterpreter.T T_MERGE) ()
  | MENU -> print_value (MenhirInterpreter.T T_MENU) ()
  | MEMORY -> print_value (MenhirInterpreter.T T_MEMORY) ()
  | MEDIUM_FONT -> print_value (MenhirInterpreter.T T_MEDIUM_FONT) ()
  | MDI_FRAME -> print_value (MenhirInterpreter.T T_MDI_FRAME) ()
  | MDI_CHILD -> print_value (MenhirInterpreter.T T_MDI_CHILD) ()
  | MAX_WIDTH -> print_value (MenhirInterpreter.T T_MAX_WIDTH) ()
  | MAX_VALUE -> print_value (MenhirInterpreter.T T_MAX_VALUE) ()
  | MAX_VAL -> print_value (MenhirInterpreter.T T_MAX_VAL) ()
  | MAX_TEXT -> print_value (MenhirInterpreter.T T_MAX_TEXT) ()
  | MAX_SIZE -> print_value (MenhirInterpreter.T T_MAX_SIZE) ()
  | MAX_PROGRESS -> print_value (MenhirInterpreter.T T_MAX_PROGRESS) ()
  | MAX_LINES -> print_value (MenhirInterpreter.T T_MAX_LINES) ()
  | MAX_HEIGHT -> print_value (MenhirInterpreter.T T_MAX_HEIGHT) ()
  | MASTER_INDEX -> print_value (MenhirInterpreter.T T_MASTER_INDEX) ()
  | MASS_UPDATE -> print_value (MenhirInterpreter.T T_MASS_UPDATE) ()
  | MANUAL -> print_value (MenhirInterpreter.T T_MANUAL) ()
  | MAGNETIC_TAPE -> print_value (MenhirInterpreter.T T_MAGNETIC_TAPE) ()
  | LT -> print_value (MenhirInterpreter.T T_LT) ()
  | LPAR -> print_value (MenhirInterpreter.T T_LPAR) ()
  | LOW_VALUE -> print_value (MenhirInterpreter.T T_LOW_VALUE) ()
  | LOW_COLOR -> print_value (MenhirInterpreter.T T_LOW_COLOR) ()
  | LOWLIGHT -> print_value (MenhirInterpreter.T T_LOWLIGHT) ()
  | LOWEST_VALUE -> print_value (MenhirInterpreter.T T_LOWEST_VALUE) ()
  | LOWERED -> print_value (MenhirInterpreter.T T_LOWERED) ()
  | LOWER -> print_value (MenhirInterpreter.T T_LOWER) ()
  | LOW -> print_value (MenhirInterpreter.T T_LOW) ()
  | LONG_VARCHAR -> print_value (MenhirInterpreter.T T_LONG_VARCHAR) ()
  | LONG_VARBINARY -> print_value (MenhirInterpreter.T T_LONG_VARBINARY) ()
  | LONG_DATE -> print_value (MenhirInterpreter.T T_LONG_DATE) ()
  | LOCK_HOLDING -> print_value (MenhirInterpreter.T T_LOCK_HOLDING) ()
  | LOCKS -> print_value (MenhirInterpreter.T T_LOCKS) ()
  | LOCK -> print_value (MenhirInterpreter.T T_LOCK) ()
  | LOCATION -> print_value (MenhirInterpreter.T T_LOCATION) ()
  | LOCAL_STORAGE -> print_value (MenhirInterpreter.T T_LOCAL_STORAGE) ()
  | LOCALE -> print_value (MenhirInterpreter.T T_LOCALE) ()
  | LOC -> print_value (MenhirInterpreter.T T_LOC) ()
  | LM_RESIZE -> print_value (MenhirInterpreter.T T_LM_RESIZE) ()
  | LIST_BOX -> print_value (MenhirInterpreter.T T_LIST_BOX) ()
  | LINKAGE -> print_value (MenhirInterpreter.T T_LINKAGE) ()
  | LINK -> print_value (MenhirInterpreter.T T_LINK) ()
  | LINE_SEQUENTIAL -> print_value (MenhirInterpreter.T T_LINE_SEQUENTIAL) ()
  | LINE_COUNTER -> print_value (MenhirInterpreter.T T_LINE_COUNTER) ()
  | LINES_PER_PAGE -> print_value (MenhirInterpreter.T T_LINES_PER_PAGE) ()
  | LINES_AT_ROOT -> print_value (MenhirInterpreter.T T_LINES_AT_ROOT) ()
  | LINES -> print_value (MenhirInterpreter.T T_LINES) ()
  | LINE -> print_value (MenhirInterpreter.T T_LINE) ()
  | LINAGE_COUNTER -> print_value (MenhirInterpreter.T T_LINAGE_COUNTER) ()
  | LINAGE -> print_value (MenhirInterpreter.T T_LINAGE) ()
  | LIN -> print_value (MenhirInterpreter.T T_LIN) ()
  | LIMITS -> print_value (MenhirInterpreter.T T_LIMITS) ()
  | LIMIT -> print_value (MenhirInterpreter.T T_LIMIT) ()
  | LIKE -> print_value (MenhirInterpreter.T T_LIKE) ()
  | LIBRARY -> print_value (MenhirInterpreter.T T_LIBRARY) ()
  | LESS -> print_value (MenhirInterpreter.T T_LESS) ()
  | LENGTH -> print_value (MenhirInterpreter.T T_LENGTH) ()
  | LEFT_TEXT -> print_value (MenhirInterpreter.T T_LEFT_TEXT) ()
  | LEFT_JUSTIFY -> print_value (MenhirInterpreter.T T_LEFT_JUSTIFY) ()
  | LEFTLINE -> print_value (MenhirInterpreter.T T_LEFTLINE) ()
  | LEFT -> print_value (MenhirInterpreter.T T_LEFT) ()
  | LEAVE -> print_value (MenhirInterpreter.T T_LEAVE) ()
  | LEADING_SHIFT -> print_value (MenhirInterpreter.T T_LEADING_SHIFT) ()
  | LEADING -> print_value (MenhirInterpreter.T T_LEADING) ()
  | LE -> print_value (MenhirInterpreter.T T_LE) ()
  | LC_TIME -> print_value (MenhirInterpreter.T T_LC_TIME) ()
  | LC_NUMERIC -> print_value (MenhirInterpreter.T T_LC_NUMERIC) ()
  | LC_MONETARY -> print_value (MenhirInterpreter.T T_LC_MONETARY) ()
  | LC_MESSAGES -> print_value (MenhirInterpreter.T T_LC_MESSAGES) ()
  | LC_CTYPE -> print_value (MenhirInterpreter.T T_LC_CTYPE) ()
  | LC_COLLATE -> print_value (MenhirInterpreter.T T_LC_COLLATE) ()
  | LC_ALL -> print_value (MenhirInterpreter.T T_LC_ALL) ()
  | LAYOUT_MANAGER -> print_value (MenhirInterpreter.T T_LAYOUT_MANAGER) ()
  | LAYOUT_DATA -> print_value (MenhirInterpreter.T T_LAYOUT_DATA) ()
  | LAST_ROW -> print_value (MenhirInterpreter.T T_LAST_ROW) ()
  | LAST -> print_value (MenhirInterpreter.T T_LAST) ()
  | LARGE_OFFSET -> print_value (MenhirInterpreter.T T_LARGE_OFFSET) ()
  | LARGE_FONT -> print_value (MenhirInterpreter.T T_LARGE_FONT) ()
  | LABEL_OFFSET -> print_value (MenhirInterpreter.T T_LABEL_OFFSET) ()
  | LABEL -> print_value (MenhirInterpreter.T T_LABEL) ()
  | KEY_LOCATION -> print_value (MenhirInterpreter.T T_KEY_LOCATION) ()
  | KEYED -> print_value (MenhirInterpreter.T T_KEYED) ()
  | KEYBOARD -> print_value (MenhirInterpreter.T T_KEYBOARD) ()
  | KEY -> print_value (MenhirInterpreter.T T_KEY) ()
  | KEPT -> print_value (MenhirInterpreter.T T_KEPT) ()
  | KANJI -> print_value (MenhirInterpreter.T T_KANJI) ()
  | JUSTIFIED -> print_value (MenhirInterpreter.T T_JUSTIFIED) ()
  | JSON_STATUS -> print_value (MenhirInterpreter.T T_JSON_STATUS) ()
  | JSON_CODE -> print_value (MenhirInterpreter.T T_JSON_CODE) ()
  | JSON -> print_value (MenhirInterpreter.T T_JSON) ()
  | JOINING -> print_value (MenhirInterpreter.T T_JOINING) ()
  | JNIENVPTR -> print_value (MenhirInterpreter.T T_JNIENVPTR) ()
  | JAVA -> print_value (MenhirInterpreter.T T_JAVA) ()
  | JAPANESE -> print_value (MenhirInterpreter.T T_JAPANESE) ()
  | I_O_CONTROL -> print_value (MenhirInterpreter.T T_I_O_CONTROL) ()
  | I_O -> print_value (MenhirInterpreter.T T_I_O) ()
  | ITEM_VALUE -> print_value (MenhirInterpreter.T T_ITEM_VALUE) ()
  | ITEM_TO_EMPTY -> print_value (MenhirInterpreter.T T_ITEM_TO_EMPTY) ()
  | ITEM_TO_DELETE -> print_value (MenhirInterpreter.T T_ITEM_TO_DELETE) ()
  | ITEM_TO_ADD -> print_value (MenhirInterpreter.T T_ITEM_TO_ADD) ()
  | ITEM_TEXT -> print_value (MenhirInterpreter.T T_ITEM_TEXT) ()
  | ITEM_ID -> print_value (MenhirInterpreter.T T_ITEM_ID) ()
  | ITEM_BOLD -> print_value (MenhirInterpreter.T T_ITEM_BOLD) ()
  | ITEM -> print_value (MenhirInterpreter.T T_ITEM) ()
  | IS_TYPEDEF -> print_value (MenhirInterpreter.T T_IS_TYPEDEF) ()
  | IS_GLOBAL -> print_value (MenhirInterpreter.T T_IS_GLOBAL) ()
  | IS_EXTERNAL -> print_value (MenhirInterpreter.T T_IS_EXTERNAL) ()
  | IS -> print_value (MenhirInterpreter.T T_IS) ()
  | IN_ARITHMETIC_RANGE -> print_value (MenhirInterpreter.T T_IN_ARITHMETIC_RANGE) ()
  | INVOKING -> print_value (MenhirInterpreter.T T_INVOKING) ()
  | INVOKED -> print_value (MenhirInterpreter.T T_INVOKED) ()
  | INVOKE -> print_value (MenhirInterpreter.T T_INVOKE) ()
  | INVALID_KEY -> print_value (MenhirInterpreter.T T_INVALID_KEY) ()
  | INVALID -> print_value (MenhirInterpreter.T T_INVALID) ()
  | INTRINSIC -> print_value (MenhirInterpreter.T T_INTRINSIC) ()
  | INTO -> print_value (MenhirInterpreter.T T_INTO) ()
  | INTERVENING_ v -> print_value (MenhirInterpreter.T T_INTERVENING_) v
  | INTERVAL_TIMER -> print_value (MenhirInterpreter.T T_INTERVAL_TIMER) ()
  | INTERMEDIATE -> print_value (MenhirInterpreter.T T_INTERMEDIATE) ()
  | INTERFACE_ID -> print_value (MenhirInterpreter.T T_INTERFACE_ID) ()
  | INTERFACE -> print_value (MenhirInterpreter.T T_INTERFACE) ()
  | INSTANCE -> print_value (MenhirInterpreter.T T_INSTANCE) ()
  | INSTALLATION -> print_value (MenhirInterpreter.T T_INSTALLATION) ()
  | INSPECT -> print_value (MenhirInterpreter.T T_INSPECT) ()
  | INSERT_ROWS -> print_value (MenhirInterpreter.T T_INSERT_ROWS) ()
  | INSERTION_INDEX -> print_value (MenhirInterpreter.T T_INSERTION_INDEX) ()
  | INSERT -> print_value (MenhirInterpreter.T T_INSERT) ()
  | INQUIRE -> print_value (MenhirInterpreter.T T_INQUIRE) ()
  | INPUT_OUTPUT -> print_value (MenhirInterpreter.T T_INPUT_OUTPUT) ()
  | INPUT -> print_value (MenhirInterpreter.T T_INPUT) ()
  | INITIATE -> print_value (MenhirInterpreter.T T_INITIATE) ()
  | INITIAL_VALUE -> print_value (MenhirInterpreter.T T_INITIAL_VALUE) ()
  | INITIALIZED -> print_value (MenhirInterpreter.T T_INITIALIZED) ()
  | INITIALIZE -> print_value (MenhirInterpreter.T T_INITIALIZE) ()
  | INITIAL -> print_value (MenhirInterpreter.T T_INITIAL) ()
  | INHERITS -> print_value (MenhirInterpreter.T T_INHERITS) ()
  | INHERITING -> print_value (MenhirInterpreter.T T_INHERITING) ()
  | INFO_WORD v -> print_value (MenhirInterpreter.T T_INFO_WORD) v
  | INDICATORS -> print_value (MenhirInterpreter.T T_INDICATORS) ()
  | INDICATOR -> print_value (MenhirInterpreter.T T_INDICATOR) ()
  | INDICATE -> print_value (MenhirInterpreter.T T_INDICATE) ()
  | INDIC -> print_value (MenhirInterpreter.T T_INDIC) ()
  | INDEX_2 -> print_value (MenhirInterpreter.T T_INDEX_2) ()
  | INDEX_1 -> print_value (MenhirInterpreter.T T_INDEX_1) ()
  | INDEXED -> print_value (MenhirInterpreter.T T_INDEXED) ()
  | INDEX -> print_value (MenhirInterpreter.T T_INDEX) ()
  | INDEPENDENT -> print_value (MenhirInterpreter.T T_INDEPENDENT) ()
  | IN -> print_value (MenhirInterpreter.T T_IN) ()
  | IMPLEMENTS -> print_value (MenhirInterpreter.T T_IMPLEMENTS) ()
  | IMP -> print_value (MenhirInterpreter.T T_IMP) ()
  | IGNORING -> print_value (MenhirInterpreter.T T_IGNORING) ()
  | IGNORE -> print_value (MenhirInterpreter.T T_IGNORE) ()
  | IF -> print_value (MenhirInterpreter.T T_IF) ()
  | IDS_II -> print_value (MenhirInterpreter.T T_IDS_II) ()
  | IDENTIFIED -> print_value (MenhirInterpreter.T T_IDENTIFIED) ()
  | IDENTIFICATION -> print_value (MenhirInterpreter.T T_IDENTIFICATION) ()
  | ID -> print_value (MenhirInterpreter.T T_ID) ()
  | ICON -> print_value (MenhirInterpreter.T T_ICON) ()
  | HSCROLL_POS -> print_value (MenhirInterpreter.T T_HSCROLL_POS) ()
  | HSCROLL -> print_value (MenhirInterpreter.T T_HSCROLL) ()
  | HOT_TRACK -> print_value (MenhirInterpreter.T T_HOT_TRACK) ()
  | HORIZONTAL -> print_value (MenhirInterpreter.T T_HORIZONTAL) ()
  | HIGH_VALUE -> print_value (MenhirInterpreter.T T_HIGH_VALUE) ()
  | HIGH_ORDER_RIGHT -> print_value (MenhirInterpreter.T T_HIGH_ORDER_RIGHT) ()
  | HIGH_ORDER_LEFT -> print_value (MenhirInterpreter.T T_HIGH_ORDER_LEFT) ()
  | HIGH_COLOR -> print_value (MenhirInterpreter.T T_HIGH_COLOR) ()
  | HIGHLIGHT -> print_value (MenhirInterpreter.T T_HIGHLIGHT) ()
  | HIGHEST_VALUE -> print_value (MenhirInterpreter.T T_HIGHEST_VALUE) ()
  | HIGH -> print_value (MenhirInterpreter.T T_HIGH) ()
  | HIDDEN_DATA -> print_value (MenhirInterpreter.T T_HIDDEN_DATA) ()
  | HEXLIT v -> print_value (MenhirInterpreter.T T_HEXLIT) v
  | HEX -> print_value (MenhirInterpreter.T T_HEX) ()
  | HELP_ID -> print_value (MenhirInterpreter.T T_HELP_ID) ()
  | HEIGHT_IN_CELLS -> print_value (MenhirInterpreter.T T_HEIGHT_IN_CELLS) ()
  | HEIGHT -> print_value (MenhirInterpreter.T T_HEIGHT) ()
  | HEAVY -> print_value (MenhirInterpreter.T T_HEAVY) ()
  | HEADING_FONT -> print_value (MenhirInterpreter.T T_HEADING_FONT) ()
  | HEADING_DIVIDER_COLOR -> print_value (MenhirInterpreter.T T_HEADING_DIVIDER_COLOR) ()
  | HEADING_COLOR -> print_value (MenhirInterpreter.T T_HEADING_COLOR) ()
  | HEADING -> print_value (MenhirInterpreter.T T_HEADING) ()
  | HAS_CHILDREN -> print_value (MenhirInterpreter.T T_HAS_CHILDREN) ()
  | HANDLE -> print_value (MenhirInterpreter.T T_HANDLE) ()
  | GT -> print_value (MenhirInterpreter.T T_GT) ()
  | GROUP_VALUE -> print_value (MenhirInterpreter.T T_GROUP_VALUE) ()
  | GROUP_USAGE -> print_value (MenhirInterpreter.T T_GROUP_USAGE) ()
  | GROUP -> print_value (MenhirInterpreter.T T_GROUP) ()
  | GRIP -> print_value (MenhirInterpreter.T T_GRIP) ()
  | GRID -> print_value (MenhirInterpreter.T T_GRID) ()
  | GREATER -> print_value (MenhirInterpreter.T T_GREATER) ()
  | GRAPHICAL -> print_value (MenhirInterpreter.T T_GRAPHICAL) ()
  | GO_SEARCH -> print_value (MenhirInterpreter.T T_GO_SEARCH) ()
  | GO_HOME -> print_value (MenhirInterpreter.T T_GO_HOME) ()
  | GO_FORWARD -> print_value (MenhirInterpreter.T T_GO_FORWARD) ()
  | GO_BACK -> print_value (MenhirInterpreter.T T_GO_BACK) ()
  | GOBACK -> print_value (MenhirInterpreter.T T_GOBACK) ()
  | GO -> print_value (MenhirInterpreter.T T_GO) ()
  | GLOBAL -> print_value (MenhirInterpreter.T T_GLOBAL) ()
  | GIVING -> print_value (MenhirInterpreter.T T_GIVING) ()
  | GET -> print_value (MenhirInterpreter.T T_GET) ()
  | GENERATE -> print_value (MenhirInterpreter.T T_GENERATE) ()
  | GE -> print_value (MenhirInterpreter.T T_GE) ()
  | GCOS -> print_value (MenhirInterpreter.T T_GCOS) ()
  | FUNCTION_POINTER -> print_value (MenhirInterpreter.T T_FUNCTION_POINTER) ()
  | FUNCTION_ID -> print_value (MenhirInterpreter.T T_FUNCTION_ID) ()
  | FUNCTION -> print_value (MenhirInterpreter.T T_FUNCTION) ()
  | FULL_HEIGHT -> print_value (MenhirInterpreter.T T_FULL_HEIGHT) ()
  | FULL -> print_value (MenhirInterpreter.T T_FULL) ()
  | FROM -> print_value (MenhirInterpreter.T T_FROM) ()
  | FREE -> print_value (MenhirInterpreter.T T_FREE) ()
  | FRAMED -> print_value (MenhirInterpreter.T T_FRAMED) ()
  | FRAME -> print_value (MenhirInterpreter.T T_FRAME) ()
  | FORMAT -> print_value (MenhirInterpreter.T T_FORMAT) ()
  | FOREVER -> print_value (MenhirInterpreter.T T_FOREVER) ()
  | FOREGROUND_COLOR -> print_value (MenhirInterpreter.T T_FOREGROUND_COLOR) ()
  | FOR -> print_value (MenhirInterpreter.T T_FOR) ()
  | FOOTING -> print_value (MenhirInterpreter.T T_FOOTING) ()
  | FONT -> print_value (MenhirInterpreter.T T_FONT) ()
  | FLR -> print_value (MenhirInterpreter.T T_FLR) ()
  | FLOAT_SHORT -> print_value (MenhirInterpreter.T T_FLOAT_SHORT) ()
  | FLOAT_NOT_A_NUMBER_SIGNALING -> print_value (MenhirInterpreter.T T_FLOAT_NOT_A_NUMBER_SIGNALING) ()
  | FLOAT_NOT_A_NUMBER_QUIET -> print_value (MenhirInterpreter.T T_FLOAT_NOT_A_NUMBER_QUIET) ()
  | FLOAT_NOT_A_NUMBER -> print_value (MenhirInterpreter.T T_FLOAT_NOT_A_NUMBER) ()
  | FLOAT_LONG -> print_value (MenhirInterpreter.T T_FLOAT_LONG) ()
  | FLOAT_INFINITY -> print_value (MenhirInterpreter.T T_FLOAT_INFINITY) ()
  | FLOAT_EXTENDED -> print_value (MenhirInterpreter.T T_FLOAT_EXTENDED) ()
  | FLOAT_DECIMAL_34 -> print_value (MenhirInterpreter.T T_FLOAT_DECIMAL_34) ()
  | FLOAT_DECIMAL_16 -> print_value (MenhirInterpreter.T T_FLOAT_DECIMAL_16) ()
  | FLOAT_DECIMAL -> print_value (MenhirInterpreter.T T_FLOAT_DECIMAL) ()
  | FLOAT_BINARY_64 -> print_value (MenhirInterpreter.T T_FLOAT_BINARY_64) ()
  | FLOAT_BINARY_32 -> print_value (MenhirInterpreter.T T_FLOAT_BINARY_32) ()
  | FLOAT_BINARY_128 -> print_value (MenhirInterpreter.T T_FLOAT_BINARY_128) ()
  | FLOAT_BINARY -> print_value (MenhirInterpreter.T T_FLOAT_BINARY) ()
  | FLOATLIT v -> print_value (MenhirInterpreter.T T_FLOATLIT) v
  | FLOATING -> print_value (MenhirInterpreter.T T_FLOATING) ()
  | FLOAT -> print_value (MenhirInterpreter.T T_FLOAT) ()
  | FLAT_BUTTONS -> print_value (MenhirInterpreter.T T_FLAT_BUTTONS) ()
  | FLAT -> print_value (MenhirInterpreter.T T_FLAT) ()
  | FIXED_WIDTH -> print_value (MenhirInterpreter.T T_FIXED_WIDTH) ()
  | FIXED_FONT -> print_value (MenhirInterpreter.T T_FIXED_FONT) ()
  | FIXEDLIT v -> print_value (MenhirInterpreter.T T_FIXEDLIT) v
  | FIXED -> print_value (MenhirInterpreter.T T_FIXED) ()
  | FIRST -> print_value (MenhirInterpreter.T T_FIRST) ()
  | FINISH_REASON -> print_value (MenhirInterpreter.T T_FINISH_REASON) ()
  | FINALLY -> print_value (MenhirInterpreter.T T_FINALLY) ()
  | FINAL -> print_value (MenhirInterpreter.T T_FINAL) ()
  | FILL_PERCENT -> print_value (MenhirInterpreter.T T_FILL_PERCENT) ()
  | FILL_COLOR2 -> print_value (MenhirInterpreter.T T_FILL_COLOR2) ()
  | FILL_COLOR -> print_value (MenhirInterpreter.T T_FILL_COLOR) ()
  | FILLER -> print_value (MenhirInterpreter.T T_FILLER) ()
  | FILE_PREFIX -> print_value (MenhirInterpreter.T T_FILE_PREFIX) ()
  | FILE_POS -> print_value (MenhirInterpreter.T T_FILE_POS) ()
  | FILE_PATH -> print_value (MenhirInterpreter.T T_FILE_PATH) ()
  | FILE_NAME -> print_value (MenhirInterpreter.T T_FILE_NAME) ()
  | FILE_LIMITS -> print_value (MenhirInterpreter.T T_FILE_LIMITS) ()
  | FILE_LIMIT -> print_value (MenhirInterpreter.T T_FILE_LIMIT) ()
  | FILE_ID -> print_value (MenhirInterpreter.T T_FILE_ID) ()
  | FILE_CONTROL -> print_value (MenhirInterpreter.T T_FILE_CONTROL) ()
  | FILES -> print_value (MenhirInterpreter.T T_FILES) ()
  | FILE -> print_value (MenhirInterpreter.T T_FILE) ()
  | FIELD_TERMINATOR -> print_value (MenhirInterpreter.T T_FIELD_TERMINATOR) ()
  | FH__KEYDEF -> print_value (MenhirInterpreter.T T_FH__KEYDEF) ()
  | FH__FCD -> print_value (MenhirInterpreter.T T_FH__FCD) ()
  | FD -> print_value (MenhirInterpreter.T T_FD) ()
  | FARTHEST_FROM_ZERO -> print_value (MenhirInterpreter.T T_FARTHEST_FROM_ZERO) ()
  | FALSE -> print_value (MenhirInterpreter.T T_FALSE) ()
  | FACTORY -> print_value (MenhirInterpreter.T T_FACTORY) ()
  | F -> print_value (MenhirInterpreter.T T_F) ()
  | EXTERNAL_FORM -> print_value (MenhirInterpreter.T T_EXTERNAL_FORM) ()
  | EXTERNALLY_DESCRIBED_KEY -> print_value (MenhirInterpreter.T T_EXTERNALLY_DESCRIBED_KEY) ()
  | EXTERNAL -> print_value (MenhirInterpreter.T T_EXTERNAL) ()
  | EXTERN -> print_value (MenhirInterpreter.T T_EXTERN) ()
  | EXTENDED_SEARCH -> print_value (MenhirInterpreter.T T_EXTENDED_SEARCH) ()
  | EXTEND -> print_value (MenhirInterpreter.T T_EXTEND) ()
  | EXPANDS -> print_value (MenhirInterpreter.T T_EXPANDS) ()
  | EXPAND -> print_value (MenhirInterpreter.T T_EXPAND) ()
  | EXIT -> print_value (MenhirInterpreter.T T_EXIT) ()
  | EXHIBIT -> print_value (MenhirInterpreter.T T_EXHIBIT) ()
  | EXECUTE -> print_value (MenhirInterpreter.T T_EXECUTE) ()
  | EXEC -> print_value (MenhirInterpreter.T T_EXEC) ()
  | EXCLUSIVE_OR -> print_value (MenhirInterpreter.T T_EXCLUSIVE_OR) ()
  | EXCLUSIVE -> print_value (MenhirInterpreter.T T_EXCLUSIVE) ()
  | EXCLUDE_EVENT_LIST -> print_value (MenhirInterpreter.T T_EXCLUDE_EVENT_LIST) ()
  | EXCESS_3 -> print_value (MenhirInterpreter.T T_EXCESS_3) ()
  | EXCEPTION_VALUE -> print_value (MenhirInterpreter.T T_EXCEPTION_VALUE) ()
  | EXCEPTION_OBJECT -> print_value (MenhirInterpreter.T T_EXCEPTION_OBJECT) ()
  | EXCEPTION -> print_value (MenhirInterpreter.T T_EXCEPTION) ()
  | EXCEEDS -> print_value (MenhirInterpreter.T T_EXCEEDS) ()
  | EXAMINE -> print_value (MenhirInterpreter.T T_EXAMINE) ()
  | EVERY -> print_value (MenhirInterpreter.T T_EVERY) ()
  | EVENT_POINTER -> print_value (MenhirInterpreter.T T_EVENT_POINTER) ()
  | EVENT_LIST -> print_value (MenhirInterpreter.T T_EVENT_LIST) ()
  | EVENT -> print_value (MenhirInterpreter.T T_EVENT) ()
  | EVALUATE -> print_value (MenhirInterpreter.T T_EVALUATE) ()
  | ESI -> print_value (MenhirInterpreter.T T_ESI) ()
  | ESCAPE_BUTTON -> print_value (MenhirInterpreter.T T_ESCAPE_BUTTON) ()
  | ESCAPE -> print_value (MenhirInterpreter.T T_ESCAPE) ()
  | ERROR -> print_value (MenhirInterpreter.T T_ERROR) ()
  | ERASE -> print_value (MenhirInterpreter.T T_ERASE) ()
  | EQUAL -> print_value (MenhirInterpreter.T T_EQUAL) ()
  | EQ -> print_value (MenhirInterpreter.T T_EQ) ()
  | EOS -> print_value (MenhirInterpreter.T T_EOS) ()
  | EOP -> print_value (MenhirInterpreter.T T_EOP) ()
  | EOL -> print_value (MenhirInterpreter.T T_EOL) ()
  | EOF -> print_value (MenhirInterpreter.T T_EOF) ()
  | EO -> print_value (MenhirInterpreter.T T_EO) ()
  | ENVIRONMENT_VALUE -> print_value (MenhirInterpreter.T T_ENVIRONMENT_VALUE) ()
  | ENVIRONMENT_NAME -> print_value (MenhirInterpreter.T T_ENVIRONMENT_NAME) ()
  | ENVIRONMENT -> print_value (MenhirInterpreter.T T_ENVIRONMENT) ()
  | ENTRY_REASON -> print_value (MenhirInterpreter.T T_ENTRY_REASON) ()
  | ENTRY_FIELD -> print_value (MenhirInterpreter.T T_ENTRY_FIELD) ()
  | ENTRY_CONVENTION -> print_value (MenhirInterpreter.T T_ENTRY_CONVENTION) ()
  | ENTRY -> print_value (MenhirInterpreter.T T_ENTRY) ()
  | ENTER -> print_value (MenhirInterpreter.T T_ENTER) ()
  | ENSURE_VISIBLE -> print_value (MenhirInterpreter.T T_ENSURE_VISIBLE) ()
  | ENGRAVED -> print_value (MenhirInterpreter.T T_ENGRAVED) ()
  | END_XML -> print_value (MenhirInterpreter.T T_END_XML) ()
  | END_WRITE -> print_value (MenhirInterpreter.T T_END_WRITE) ()
  | END_WAIT -> print_value (MenhirInterpreter.T T_END_WAIT) ()
  | END_USE -> print_value (MenhirInterpreter.T T_END_USE) ()
  | END_UNSTRING -> print_value (MenhirInterpreter.T T_END_UNSTRING) ()
  | END_SUBTRACT -> print_value (MenhirInterpreter.T T_END_SUBTRACT) ()
  | END_STRING -> print_value (MenhirInterpreter.T T_END_STRING) ()
  | END_START -> print_value (MenhirInterpreter.T T_END_START) ()
  | END_SET -> print_value (MenhirInterpreter.T T_END_SET) ()
  | END_SEND -> print_value (MenhirInterpreter.T T_END_SEND) ()
  | END_SEARCH -> print_value (MenhirInterpreter.T T_END_SEARCH) ()
  | END_REWRITE -> print_value (MenhirInterpreter.T T_END_REWRITE) ()
  | END_RETURN -> print_value (MenhirInterpreter.T T_END_RETURN) ()
  | END_REPLACE -> print_value (MenhirInterpreter.T T_END_REPLACE) ()
  | END_RECEIVE -> print_value (MenhirInterpreter.T T_END_RECEIVE) ()
  | END_READ -> print_value (MenhirInterpreter.T T_END_READ) ()
  | END_PERFORM -> print_value (MenhirInterpreter.T T_END_PERFORM) ()
  | END_ON -> print_value (MenhirInterpreter.T T_END_ON) ()
  | END_OF_PAGE -> print_value (MenhirInterpreter.T T_END_OF_PAGE) ()
  | END_MULTIPLY -> print_value (MenhirInterpreter.T T_END_MULTIPLY) ()
  | END_MOVE -> print_value (MenhirInterpreter.T T_END_MOVE) ()
  | END_MODIFY -> print_value (MenhirInterpreter.T T_END_MODIFY) ()
  | END_JSON -> print_value (MenhirInterpreter.T T_END_JSON) ()
  | END_INVOKE -> print_value (MenhirInterpreter.T T_END_INVOKE) ()
  | END_IF -> print_value (MenhirInterpreter.T T_END_IF) ()
  | END_EXEC -> print_value (MenhirInterpreter.T T_END_EXEC) ()
  | END_EVALUATE -> print_value (MenhirInterpreter.T T_END_EVALUATE) ()
  | END_ENABLE -> print_value (MenhirInterpreter.T T_END_ENABLE) ()
  | END_DIVIDE -> print_value (MenhirInterpreter.T T_END_DIVIDE) ()
  | END_DISPLAY -> print_value (MenhirInterpreter.T T_END_DISPLAY) ()
  | END_DISABLE -> print_value (MenhirInterpreter.T T_END_DISABLE) ()
  | END_DELETE -> print_value (MenhirInterpreter.T T_END_DELETE) ()
  | END_COPY -> print_value (MenhirInterpreter.T T_END_COPY) ()
  | END_COMPUTE -> print_value (MenhirInterpreter.T T_END_COMPUTE) ()
  | END_COLOR -> print_value (MenhirInterpreter.T T_END_COLOR) ()
  | END_CHAIN -> print_value (MenhirInterpreter.T T_END_CHAIN) ()
  | END_CALL -> print_value (MenhirInterpreter.T T_END_CALL) ()
  | END_ADD -> print_value (MenhirInterpreter.T T_END_ADD) ()
  | END_ACCEPT -> print_value (MenhirInterpreter.T T_END_ACCEPT) ()
  | ENDING -> print_value (MenhirInterpreter.T T_ENDING) ()
  | END -> print_value (MenhirInterpreter.T T_END) ()
  | ENCRYPTION -> print_value (MenhirInterpreter.T T_ENCRYPTION) ()
  | ENCODING -> print_value (MenhirInterpreter.T T_ENCODING) ()
  | ENABLED -> print_value (MenhirInterpreter.T T_ENABLED) ()
  | ENABLE -> print_value (MenhirInterpreter.T T_ENABLE) ()
  | EMI -> print_value (MenhirInterpreter.T T_EMI) ()
  | ELSE -> print_value (MenhirInterpreter.T T_ELSE) ()
  | ELEMENT -> print_value (MenhirInterpreter.T T_ELEMENT) ()
  | EJECT -> print_value (MenhirInterpreter.T T_EJECT) ()
  | EIGHTY_EIGHT -> print_value (MenhirInterpreter.T T_EIGHTY_EIGHT) ()
  | EGI -> print_value (MenhirInterpreter.T T_EGI) ()
  | EGCS -> print_value (MenhirInterpreter.T T_EGCS) ()
  | EGC -> print_value (MenhirInterpreter.T T_EGC) ()
  | EDITING -> print_value (MenhirInterpreter.T T_EDITING) ()
  | ECHO -> print_value (MenhirInterpreter.T T_ECHO) ()
  | EC -> print_value (MenhirInterpreter.T T_EC) ()
  | EBCDIC -> print_value (MenhirInterpreter.T T_EBCDIC) ()
  | DYNAMIC -> print_value (MenhirInterpreter.T T_DYNAMIC) ()
  | DUPLICATES -> print_value (MenhirInterpreter.T T_DUPLICATES) ()
  | DROP_LIST -> print_value (MenhirInterpreter.T T_DROP_LIST) ()
  | DROP_DOWN -> print_value (MenhirInterpreter.T T_DROP_DOWN) ()
  | DROP -> print_value (MenhirInterpreter.T T_DROP) ()
  | DRAW -> print_value (MenhirInterpreter.T T_DRAW) ()
  | DRAG_COLOR -> print_value (MenhirInterpreter.T T_DRAG_COLOR) ()
  | DOWN -> print_value (MenhirInterpreter.T T_DOWN) ()
  | DOUBLE_COLON -> print_value (MenhirInterpreter.T T_DOUBLE_COLON) ()
  | DOUBLE_ASTERISK -> print_value (MenhirInterpreter.T T_DOUBLE_ASTERISK) ()
  | DOUBLE -> print_value (MenhirInterpreter.T T_DOUBLE) ()
  | DOT_DASH -> print_value (MenhirInterpreter.T T_DOT_DASH) ()
  | DOTTED -> print_value (MenhirInterpreter.T T_DOTTED) ()
  | DOTDASH -> print_value (MenhirInterpreter.T T_DOTDASH) ()
  | DIVISION -> print_value (MenhirInterpreter.T T_DIVISION) ()
  | DIVIDER_COLOR -> print_value (MenhirInterpreter.T T_DIVIDER_COLOR) ()
  | DIVIDERS -> print_value (MenhirInterpreter.T T_DIVIDERS) ()
  | DIVIDE -> print_value (MenhirInterpreter.T T_DIVIDE) ()
  | DISPLAY_ST -> print_value (MenhirInterpreter.T T_DISPLAY_ST) ()
  | DISPLAY_FORMAT -> print_value (MenhirInterpreter.T T_DISPLAY_FORMAT) ()
  | DISPLAY_COLUMNS -> print_value (MenhirInterpreter.T T_DISPLAY_COLUMNS) ()
  | DISPLAY_4 -> print_value (MenhirInterpreter.T T_DISPLAY_4) ()
  | DISPLAY_3 -> print_value (MenhirInterpreter.T T_DISPLAY_3) ()
  | DISPLAY_2 -> print_value (MenhirInterpreter.T T_DISPLAY_2) ()
  | DISPLAY_1 -> print_value (MenhirInterpreter.T T_DISPLAY_1) ()
  | DISPLAY -> print_value (MenhirInterpreter.T T_DISPLAY) ()
  | DISP -> print_value (MenhirInterpreter.T T_DISP) ()
  | DISK -> print_value (MenhirInterpreter.T T_DISK) ()
  | DISJOINING -> print_value (MenhirInterpreter.T T_DISJOINING) ()
  | DISCONNECT -> print_value (MenhirInterpreter.T T_DISCONNECT) ()
  | DISC -> print_value (MenhirInterpreter.T T_DISC) ()
  | DISABLE -> print_value (MenhirInterpreter.T T_DISABLE) ()
  | DIGITS v -> print_value (MenhirInterpreter.T T_DIGITS) v
  | DETAIL -> print_value (MenhirInterpreter.T T_DETAIL) ()
  | DESTROY -> print_value (MenhirInterpreter.T T_DESTROY) ()
  | DESTINATION -> print_value (MenhirInterpreter.T T_DESTINATION) ()
  | DESCRIPTOR -> print_value (MenhirInterpreter.T T_DESCRIPTOR) ()
  | DESCENDING -> print_value (MenhirInterpreter.T T_DESCENDING) ()
  | DEPENDING -> print_value (MenhirInterpreter.T T_DEPENDING) ()
  | DELIMITER -> print_value (MenhirInterpreter.T T_DELIMITER) ()
  | DELIMITED -> print_value (MenhirInterpreter.T T_DELIMITED) ()
  | DELETE -> print_value (MenhirInterpreter.T T_DELETE) ()
  | DEFINITION -> print_value (MenhirInterpreter.T T_DEFINITION) ()
  | DEFAULT_FONT -> print_value (MenhirInterpreter.T T_DEFAULT_FONT) ()
  | DEFAULT_BUTTON -> print_value (MenhirInterpreter.T T_DEFAULT_BUTTON) ()
  | DEFAULT -> print_value (MenhirInterpreter.T T_DEFAULT) ()
  | DECLARE -> print_value (MenhirInterpreter.T T_DECLARE) ()
  | DECLARATIVES -> print_value (MenhirInterpreter.T T_DECLARATIVES) ()
  | DECIMAL_POINT -> print_value (MenhirInterpreter.T T_DECIMAL_POINT) ()
  | DECIMAL_ENCODING -> print_value (MenhirInterpreter.T T_DECIMAL_ENCODING) ()
  | DEBUG_SUB_3 -> print_value (MenhirInterpreter.T T_DEBUG_SUB_3) ()
  | DEBUG_SUB_2 -> print_value (MenhirInterpreter.T T_DEBUG_SUB_2) ()
  | DEBUG_SUB_1 -> print_value (MenhirInterpreter.T T_DEBUG_SUB_1) ()
  | DEBUG_NAME -> print_value (MenhirInterpreter.T T_DEBUG_NAME) ()
  | DEBUG_LINE -> print_value (MenhirInterpreter.T T_DEBUG_LINE) ()
  | DEBUG_ITEM -> print_value (MenhirInterpreter.T T_DEBUG_ITEM) ()
  | DEBUG_CONTENTS -> print_value (MenhirInterpreter.T T_DEBUG_CONTENTS) ()
  | DEBUGGING -> print_value (MenhirInterpreter.T T_DEBUGGING) ()
  | DEBUG -> print_value (MenhirInterpreter.T T_DEBUG) ()
  | DBCS -> print_value (MenhirInterpreter.T T_DBCS) ()
  | DBCLOB_LOCATOR -> print_value (MenhirInterpreter.T T_DBCLOB_LOCATOR) ()
  | DBCLOB_FILE -> print_value (MenhirInterpreter.T T_DBCLOB_FILE) ()
  | DBCLOB -> print_value (MenhirInterpreter.T T_DBCLOB) ()
  | DAY_OF_WEEK -> print_value (MenhirInterpreter.T T_DAY_OF_WEEK) ()
  | DAY_AND_TIME -> print_value (MenhirInterpreter.T T_DAY_AND_TIME) ()
  | DAY -> print_value (MenhirInterpreter.T T_DAY) ()
  | DATE_WRITTEN -> print_value (MenhirInterpreter.T T_DATE_WRITTEN) ()
  | DATE_RECORD -> print_value (MenhirInterpreter.T T_DATE_RECORD) ()
  | DATE_MODIFIED -> print_value (MenhirInterpreter.T T_DATE_MODIFIED) ()
  | DATE_ENTRY -> print_value (MenhirInterpreter.T T_DATE_ENTRY) ()
  | DATE_COMPILED -> print_value (MenhirInterpreter.T T_DATE_COMPILED) ()
  | DATE_AND_TIME -> print_value (MenhirInterpreter.T T_DATE_AND_TIME) ()
  | DATE -> print_value (MenhirInterpreter.T T_DATE) ()
  | DATA_TYPES -> print_value (MenhirInterpreter.T T_DATA_TYPES) ()
  | DATA_RECORDS -> print_value (MenhirInterpreter.T T_DATA_RECORDS) ()
  | DATA_RECORD -> print_value (MenhirInterpreter.T T_DATA_RECORD) ()
  | DATA_POINTER -> print_value (MenhirInterpreter.T T_DATA_POINTER) ()
  | DATA_COLUMNS -> print_value (MenhirInterpreter.T T_DATA_COLUMNS) ()
  | DATA -> print_value (MenhirInterpreter.T T_DATA) ()
  | DASH_SIGN -> print_value (MenhirInterpreter.T T_DASH_SIGN) ()
  | DASHED -> print_value (MenhirInterpreter.T T_DASHED) ()
  | CYL_OVERFLOW -> print_value (MenhirInterpreter.T T_CYL_OVERFLOW) ()
  | CYL_INDEX -> print_value (MenhirInterpreter.T T_CYL_INDEX) ()
  | CYCLE -> print_value (MenhirInterpreter.T T_CYCLE) ()
  | CUSTOM_PRINT_TEMPLATE -> print_value (MenhirInterpreter.T T_CUSTOM_PRINT_TEMPLATE) ()
  | CURSOR_Y -> print_value (MenhirInterpreter.T T_CURSOR_Y) ()
  | CURSOR_X -> print_value (MenhirInterpreter.T T_CURSOR_X) ()
  | CURSOR_ROW -> print_value (MenhirInterpreter.T T_CURSOR_ROW) ()
  | CURSOR_FRAME_WIDTH -> print_value (MenhirInterpreter.T T_CURSOR_FRAME_WIDTH) ()
  | CURSOR_COLOR -> print_value (MenhirInterpreter.T T_CURSOR_COLOR) ()
  | CURSOR_COL -> print_value (MenhirInterpreter.T T_CURSOR_COL) ()
  | CURSOR -> print_value (MenhirInterpreter.T T_CURSOR) ()
  | CURRENT_DATE -> print_value (MenhirInterpreter.T T_CURRENT_DATE) ()
  | CURRENT -> print_value (MenhirInterpreter.T T_CURRENT) ()
  | CURRENCY -> print_value (MenhirInterpreter.T T_CURRENCY) ()
  | CULTURE -> print_value (MenhirInterpreter.T T_CULTURE) ()
  | CS_GENERAL -> print_value (MenhirInterpreter.T T_CS_GENERAL) ()
  | CS_BASIC -> print_value (MenhirInterpreter.T T_CS_BASIC) ()
  | CSP -> print_value (MenhirInterpreter.T T_CSP) ()
  | CSIZE -> print_value (MenhirInterpreter.T T_CSIZE) ()
  | CRT_UNDER -> print_value (MenhirInterpreter.T T_CRT_UNDER) ()
  | CRT -> print_value (MenhirInterpreter.T T_CRT) ()
  | CREATE -> print_value (MenhirInterpreter.T T_CREATE) ()
  | COUNT_TRAILING -> print_value (MenhirInterpreter.T T_COUNT_TRAILING) ()
  | COUNT_MIN -> print_value (MenhirInterpreter.T T_COUNT_MIN) ()
  | COUNT_MAX -> print_value (MenhirInterpreter.T T_COUNT_MAX) ()
  | COUNT_LEADLING -> print_value (MenhirInterpreter.T T_COUNT_LEADLING) ()
  | COUNT -> print_value (MenhirInterpreter.T T_COUNT) ()
  | CORRESPONDING -> print_value (MenhirInterpreter.T T_CORRESPONDING) ()
  | CORE_INDEX -> print_value (MenhirInterpreter.T T_CORE_INDEX) ()
  | COPY_SELECTION -> print_value (MenhirInterpreter.T T_COPY_SELECTION) ()
  | COPY -> print_value (MenhirInterpreter.T T_COPY) ()
  | CONVERTING -> print_value (MenhirInterpreter.T T_CONVERTING) ()
  | CONVERT -> print_value (MenhirInterpreter.T T_CONVERT) ()
  | CONVERSION -> print_value (MenhirInterpreter.T T_CONVERSION) ()
  | CONTROL_AREA -> print_value (MenhirInterpreter.T T_CONTROL_AREA) ()
  | CONTROLS_UNCROPPED -> print_value (MenhirInterpreter.T T_CONTROLS_UNCROPPED) ()
  | CONTROLS -> print_value (MenhirInterpreter.T T_CONTROLS) ()
  | CONTROL -> print_value (MenhirInterpreter.T T_CONTROL) ()
  | CONTINUE -> print_value (MenhirInterpreter.T T_CONTINUE) ()
  | CONTENT_OF -> print_value (MenhirInterpreter.T T_CONTENT_OF) ()
  | CONTENT -> print_value (MenhirInterpreter.T T_CONTENT) ()
  | CONTAINS -> print_value (MenhirInterpreter.T T_CONTAINS) ()
  | CONSTRUCTOR -> print_value (MenhirInterpreter.T T_CONSTRUCTOR) ()
  | CONSTANT_RECORD -> print_value (MenhirInterpreter.T T_CONSTANT_RECORD) ()
  | CONSTANT -> print_value (MenhirInterpreter.T T_CONSTANT) ()
  | CONSOLE_3 -> print_value (MenhirInterpreter.T T_CONSOLE_3) ()
  | CONSOLE_2 -> print_value (MenhirInterpreter.T T_CONSOLE_2) ()
  | CONSOLE_1 -> print_value (MenhirInterpreter.T T_CONSOLE_1) ()
  | CONSOLE_0 -> print_value (MenhirInterpreter.T T_CONSOLE_0) ()
  | CONNECT -> print_value (MenhirInterpreter.T T_CONNECT) ()
  | CONFIGURATION -> print_value (MenhirInterpreter.T T_CONFIGURATION) ()
  | CONDITION -> print_value (MenhirInterpreter.T T_CONDITION) ()
  | COM_REG -> print_value (MenhirInterpreter.T T_COM_REG) ()
  | COMP_X -> print_value (MenhirInterpreter.T T_COMP_X) ()
  | COMP_N -> print_value (MenhirInterpreter.T T_COMP_N) ()
  | COMP_9 -> print_value (MenhirInterpreter.T T_COMP_9) ()
  | COMP_7 -> print_value (MenhirInterpreter.T T_COMP_7) ()
  | COMP_6 -> print_value (MenhirInterpreter.T T_COMP_6) ()
  | COMP_5 -> print_value (MenhirInterpreter.T T_COMP_5) ()
  | COMP_4 -> print_value (MenhirInterpreter.T T_COMP_4) ()
  | COMP_3 -> print_value (MenhirInterpreter.T T_COMP_3) ()
  | COMP_2 -> print_value (MenhirInterpreter.T T_COMP_2) ()
  | COMP_15 -> print_value (MenhirInterpreter.T T_COMP_15) ()
  | COMP_14 -> print_value (MenhirInterpreter.T T_COMP_14) ()
  | COMP_13 -> print_value (MenhirInterpreter.T T_COMP_13) ()
  | COMP_12 -> print_value (MenhirInterpreter.T T_COMP_12) ()
  | COMP_11 -> print_value (MenhirInterpreter.T T_COMP_11) ()
  | COMP_10 -> print_value (MenhirInterpreter.T T_COMP_10) ()
  | COMP_1 -> print_value (MenhirInterpreter.T T_COMP_1) ()
  | COMP_0 -> print_value (MenhirInterpreter.T T_COMP_0) ()
  | COMPUTE -> print_value (MenhirInterpreter.T T_COMPUTE) ()
  | COMPUTATIONAL_7 -> print_value (MenhirInterpreter.T T_COMPUTATIONAL_7) ()
  | COMPUTATIONAL_14 -> print_value (MenhirInterpreter.T T_COMPUTATIONAL_14) ()
  | COMPUTATIONAL_13 -> print_value (MenhirInterpreter.T T_COMPUTATIONAL_13) ()
  | COMPUTATIONAL_12 -> print_value (MenhirInterpreter.T T_COMPUTATIONAL_12) ()
  | COMPUTATIONAL_11 -> print_value (MenhirInterpreter.T T_COMPUTATIONAL_11) ()
  | COMPRESSION -> print_value (MenhirInterpreter.T T_COMPRESSION) ()
  | COMPLEMENTARY -> print_value (MenhirInterpreter.T T_COMPLEMENTARY) ()
  | COMPLE -> print_value (MenhirInterpreter.T T_COMPLE) ()
  | COMP -> print_value (MenhirInterpreter.T T_COMP) ()
  | COMMUNICATION -> print_value (MenhirInterpreter.T T_COMMUNICATION) ()
  | COMMON -> print_value (MenhirInterpreter.T T_COMMON) ()
  | COMMITMENT -> print_value (MenhirInterpreter.T T_COMMITMENT) ()
  | COMMIT -> print_value (MenhirInterpreter.T T_COMMIT) ()
  | COMMENT_ENTRY v -> print_value (MenhirInterpreter.T T_COMMENT_ENTRY) v
  | COMMAND_LINE -> print_value (MenhirInterpreter.T T_COMMAND_LINE) ()
  | COMMA -> print_value (MenhirInterpreter.T T_COMMA) ()
  | COMBO_BOX -> print_value (MenhirInterpreter.T T_COMBO_BOX) ()
  | COLUMN_PROTECTION -> print_value (MenhirInterpreter.T T_COLUMN_PROTECTION) ()
  | COLUMN_HEADINGS -> print_value (MenhirInterpreter.T T_COLUMN_HEADINGS) ()
  | COLUMN_FONT -> print_value (MenhirInterpreter.T T_COLUMN_FONT) ()
  | COLUMN_DIVIDERS -> print_value (MenhirInterpreter.T T_COLUMN_DIVIDERS) ()
  | COLUMN_COLOR -> print_value (MenhirInterpreter.T T_COLUMN_COLOR) ()
  | COLUMNS -> print_value (MenhirInterpreter.T T_COLUMNS) ()
  | COLUMN -> print_value (MenhirInterpreter.T T_COLUMN) ()
  | COLORS -> print_value (MenhirInterpreter.T T_COLORS) ()
  | COLOR -> print_value (MenhirInterpreter.T T_COLOR) ()
  | COLON -> print_value (MenhirInterpreter.T T_COLON) ()
  | COLLATING -> print_value (MenhirInterpreter.T T_COLLATING) ()
  | COL -> print_value (MenhirInterpreter.T T_COL) ()
  | COERCION -> print_value (MenhirInterpreter.T T_COERCION) ()
  | CODE_SET -> print_value (MenhirInterpreter.T T_CODE_SET) ()
  | CODE -> print_value (MenhirInterpreter.T T_CODE) ()
  | COBOL -> print_value (MenhirInterpreter.T T_COBOL) ()
  | CLOSE -> print_value (MenhirInterpreter.T T_CLOSE) ()
  | CLOCK_UNITS -> print_value (MenhirInterpreter.T T_CLOCK_UNITS) ()
  | CLOB_LOCATOR -> print_value (MenhirInterpreter.T T_CLOB_LOCATOR) ()
  | CLOB_FILE -> print_value (MenhirInterpreter.T T_CLOB_FILE) ()
  | CLOB -> print_value (MenhirInterpreter.T T_CLOB) ()
  | CLINES -> print_value (MenhirInterpreter.T T_CLINES) ()
  | CLINE -> print_value (MenhirInterpreter.T T_CLINE) ()
  | CLEAR_SELECTION -> print_value (MenhirInterpreter.T T_CLEAR_SELECTION) ()
  | CLASS_OBJECT -> print_value (MenhirInterpreter.T T_CLASS_OBJECT) ()
  | CLASS_NAME -> print_value (MenhirInterpreter.T T_CLASS_NAME) ()
  | CLASS_ID -> print_value (MenhirInterpreter.T T_CLASS_ID) ()
  | CLASS_CONTROL -> print_value (MenhirInterpreter.T T_CLASS_CONTROL) ()
  | CLASSIFICATION -> print_value (MenhirInterpreter.T T_CLASSIFICATION) ()
  | CLASS -> print_value (MenhirInterpreter.T T_CLASS) ()
  | CICS -> print_value (MenhirInterpreter.T T_CICS) ()
  | CHECK_BOX -> print_value (MenhirInterpreter.T T_CHECK_BOX) ()
  | CHECKPOINT_FILE -> print_value (MenhirInterpreter.T T_CHECKPOINT_FILE) ()
  | CHECK -> print_value (MenhirInterpreter.T T_CHECK) ()
  | CHAR_VARYING -> print_value (MenhirInterpreter.T T_CHAR_VARYING) ()
  | CHART -> print_value (MenhirInterpreter.T T_CHART) ()
  | CHARACTERS -> print_value (MenhirInterpreter.T T_CHARACTERS) ()
  | CHARACTER -> print_value (MenhirInterpreter.T T_CHARACTER) ()
  | CHAR -> print_value (MenhirInterpreter.T T_CHAR) ()
  | CHANGED -> print_value (MenhirInterpreter.T T_CHANGED) ()
  | CHAINING -> print_value (MenhirInterpreter.T T_CHAINING) ()
  | CHAIN -> print_value (MenhirInterpreter.T T_CHAIN) ()
  | CH -> print_value (MenhirInterpreter.T T_CH) ()
  | CF -> print_value (MenhirInterpreter.T T_CF) ()
  | CENTURY_DAY -> print_value (MenhirInterpreter.T T_CENTURY_DAY) ()
  | CENTURY_DATE -> print_value (MenhirInterpreter.T T_CENTURY_DATE) ()
  | CENTERED_HEADINGS -> print_value (MenhirInterpreter.T T_CENTERED_HEADINGS) ()
  | CENTERED -> print_value (MenhirInterpreter.T T_CENTERED) ()
  | CENTER -> print_value (MenhirInterpreter.T T_CENTER) ()
  | CELL_PROTECTION -> print_value (MenhirInterpreter.T T_CELL_PROTECTION) ()
  | CELL_FONT -> print_value (MenhirInterpreter.T T_CELL_FONT) ()
  | CELL_DATA -> print_value (MenhirInterpreter.T T_CELL_DATA) ()
  | CELL_COLOR -> print_value (MenhirInterpreter.T T_CELL_COLOR) ()
  | CELL -> print_value (MenhirInterpreter.T T_CELL) ()
  | CD -> print_value (MenhirInterpreter.T T_CD) ()
  | CCOL -> print_value (MenhirInterpreter.T T_CCOL) ()
  | CBL -> print_value (MenhirInterpreter.T T_CBL) ()
  | CATALOGUE_NAME -> print_value (MenhirInterpreter.T T_CATALOGUE_NAME) ()
  | CATALOGUED -> print_value (MenhirInterpreter.T T_CATALOGUED) ()
  | CASSETTE -> print_value (MenhirInterpreter.T T_CASSETTE) ()
  | CASE_SENSITIVE -> print_value (MenhirInterpreter.T T_CASE_SENSITIVE) ()
  | CASE_INSENSITIVE -> print_value (MenhirInterpreter.T T_CASE_INSENSITIVE) ()
  | CARD_READER -> print_value (MenhirInterpreter.T T_CARD_READER) ()
  | CARD_PUNCH -> print_value (MenhirInterpreter.T T_CARD_PUNCH) ()
  | CAPACITY -> print_value (MenhirInterpreter.T T_CAPACITY) ()
  | CANCEL_BUTTON -> print_value (MenhirInterpreter.T T_CANCEL_BUTTON) ()
  | CANCEL -> print_value (MenhirInterpreter.T T_CANCEL) ()
  | CALLED -> print_value (MenhirInterpreter.T T_CALLED) ()
  | CALL -> print_value (MenhirInterpreter.T T_CALL) ()
  | CALENDAR_FONT -> print_value (MenhirInterpreter.T T_CALENDAR_FONT) ()
  | C -> print_value (MenhirInterpreter.T T_C) ()
  | B_XOR -> print_value (MenhirInterpreter.T T_B_XOR) ()
  | B_SHIFT_RC -> print_value (MenhirInterpreter.T T_B_SHIFT_RC) ()
  | B_SHIFT_R -> print_value (MenhirInterpreter.T T_B_SHIFT_R) ()
  | B_SHIFT_LC -> print_value (MenhirInterpreter.T T_B_SHIFT_LC) ()
  | B_SHIFT_L -> print_value (MenhirInterpreter.T T_B_SHIFT_L) ()
  | B_OR -> print_value (MenhirInterpreter.T T_B_OR) ()
  | B_NOT -> print_value (MenhirInterpreter.T T_B_NOT) ()
  | B_EXOR -> print_value (MenhirInterpreter.T T_B_EXOR) ()
  | B_AND -> print_value (MenhirInterpreter.T T_B_AND) ()
  | BYTE_LENGTH -> print_value (MenhirInterpreter.T T_BYTE_LENGTH) ()
  | BYTES -> print_value (MenhirInterpreter.T T_BYTES) ()
  | BYTE -> print_value (MenhirInterpreter.T T_BYTE) ()
  | BY -> print_value (MenhirInterpreter.T T_BY) ()
  | BUTTONS -> print_value (MenhirInterpreter.T T_BUTTONS) ()
  | BUSY -> print_value (MenhirInterpreter.T T_BUSY) ()
  | BULK_ADDITION -> print_value (MenhirInterpreter.T T_BULK_ADDITION) ()
  | BSN -> print_value (MenhirInterpreter.T T_BSN) ()
  | BROWSING -> print_value (MenhirInterpreter.T T_BROWSING) ()
  | BOXED -> print_value (MenhirInterpreter.T T_BOXED) ()
  | BOX -> print_value (MenhirInterpreter.T T_BOX) ()
  | BOTTOM -> print_value (MenhirInterpreter.T T_BOTTOM) ()
  | BOOLIT v -> print_value (MenhirInterpreter.T T_BOOLIT) v
  | BOOLEAN -> print_value (MenhirInterpreter.T T_BOOLEAN) ()
  | BLOCK -> print_value (MenhirInterpreter.T T_BLOCK) ()
  | BLOB_LOCATOR -> print_value (MenhirInterpreter.T T_BLOB_LOCATOR) ()
  | BLOB_FILE -> print_value (MenhirInterpreter.T T_BLOB_FILE) ()
  | BLOB -> print_value (MenhirInterpreter.T T_BLOB) ()
  | BLINK -> print_value (MenhirInterpreter.T T_BLINK) ()
  | BLANK -> print_value (MenhirInterpreter.T T_BLANK) ()
  | BITS -> print_value (MenhirInterpreter.T T_BITS) ()
  | BITMAP_WIDTH -> print_value (MenhirInterpreter.T T_BITMAP_WIDTH) ()
  | BITMAP_TRANSPARENT_COLOR -> print_value (MenhirInterpreter.T T_BITMAP_TRANSPARENT_COLOR) ()
  | BITMAP_TRAILING -> print_value (MenhirInterpreter.T T_BITMAP_TRAILING) ()
  | BITMAP_TIMER -> print_value (MenhirInterpreter.T T_BITMAP_TIMER) ()
  | BITMAP_START -> print_value (MenhirInterpreter.T T_BITMAP_START) ()
  | BITMAP_SCALE -> print_value (MenhirInterpreter.T T_BITMAP_SCALE) ()
  | BITMAP_RAW_WIDTH -> print_value (MenhirInterpreter.T T_BITMAP_RAW_WIDTH) ()
  | BITMAP_RAW_HEIGHT -> print_value (MenhirInterpreter.T T_BITMAP_RAW_HEIGHT) ()
  | BITMAP_NUMBER -> print_value (MenhirInterpreter.T T_BITMAP_NUMBER) ()
  | BITMAP_HANDLE -> print_value (MenhirInterpreter.T T_BITMAP_HANDLE) ()
  | BITMAP_END -> print_value (MenhirInterpreter.T T_BITMAP_END) ()
  | BITMAP -> print_value (MenhirInterpreter.T T_BITMAP) ()
  | BIT -> print_value (MenhirInterpreter.T T_BIT) ()
  | BIND -> print_value (MenhirInterpreter.T T_BIND) ()
  | BINARY_SHORT -> print_value (MenhirInterpreter.T T_BINARY_SHORT) ()
  | BINARY_SEQUENTIAL -> print_value (MenhirInterpreter.T T_BINARY_SEQUENTIAL) ()
  | BINARY_LONG -> print_value (MenhirInterpreter.T T_BINARY_LONG) ()
  | BINARY_ENCODING -> print_value (MenhirInterpreter.T T_BINARY_ENCODING) ()
  | BINARY_DOUBLE -> print_value (MenhirInterpreter.T T_BINARY_DOUBLE) ()
  | BINARY_C_LONG -> print_value (MenhirInterpreter.T T_BINARY_C_LONG) ()
  | BINARY_CHAR -> print_value (MenhirInterpreter.T T_BINARY_CHAR) ()
  | BINARY -> print_value (MenhirInterpreter.T T_BINARY) ()
  | BELL -> print_value (MenhirInterpreter.T T_BELL) ()
  | BEGINNING -> print_value (MenhirInterpreter.T T_BEGINNING) ()
  | BEFORE -> print_value (MenhirInterpreter.T T_BEFORE) ()
  | BECOMES -> print_value (MenhirInterpreter.T T_BECOMES) ()
  | BASIS -> print_value (MenhirInterpreter.T T_BASIS) ()
  | BASED -> print_value (MenhirInterpreter.T T_BASED) ()
  | BAR -> print_value (MenhirInterpreter.T T_BAR) ()
  | BACKWARD -> print_value (MenhirInterpreter.T T_BACKWARD) ()
  | BACKGROUND_STANDARD -> print_value (MenhirInterpreter.T T_BACKGROUND_STANDARD) ()
  | BACKGROUND_LOW -> print_value (MenhirInterpreter.T T_BACKGROUND_LOW) ()
  | BACKGROUND_HIGH -> print_value (MenhirInterpreter.T T_BACKGROUND_HIGH) ()
  | BACKGROUND_COLOR -> print_value (MenhirInterpreter.T T_BACKGROUND_COLOR) ()
  | AX_EVENT_LIST -> print_value (MenhirInterpreter.T T_AX_EVENT_LIST) ()
  | AWAY_FROM_ZERO -> print_value (MenhirInterpreter.T T_AWAY_FROM_ZERO) ()
  | AUTO_SPIN -> print_value (MenhirInterpreter.T T_AUTO_SPIN) ()
  | AUTO_RESIZE -> print_value (MenhirInterpreter.T T_AUTO_RESIZE) ()
  | AUTO_MINIMIZE -> print_value (MenhirInterpreter.T T_AUTO_MINIMIZE) ()
  | AUTO_HYPHEN_SKIP -> print_value (MenhirInterpreter.T T_AUTO_HYPHEN_SKIP) ()
  | AUTO_DECIMAL -> print_value (MenhirInterpreter.T T_AUTO_DECIMAL) ()
  | AUTOMATIC -> print_value (MenhirInterpreter.T T_AUTOMATIC) ()
  | AUTO -> print_value (MenhirInterpreter.T T_AUTO) ()
  | AUTHOR -> print_value (MenhirInterpreter.T T_AUTHOR) ()
  | AT_EOP -> print_value (MenhirInterpreter.T T_AT_EOP) ()
  | AT_END -> print_value (MenhirInterpreter.T T_AT_END) ()
  | ATTRIBUTES -> print_value (MenhirInterpreter.T T_ATTRIBUTES) ()
  | ATTRIBUTE -> print_value (MenhirInterpreter.T T_ATTRIBUTE) ()
  | AT -> print_value (MenhirInterpreter.T T_AT) ()
  | ASTERISK -> print_value (MenhirInterpreter.T T_ASTERISK) ()
  | ASSIGN -> print_value (MenhirInterpreter.T T_ASSIGN) ()
  | ASSEMBLY_NAME -> print_value (MenhirInterpreter.T T_ASSEMBLY_NAME) ()
  | ASCII -> print_value (MenhirInterpreter.T T_ASCII) ()
  | ASCENDING -> print_value (MenhirInterpreter.T T_ASCENDING) ()
  | ASA -> print_value (MenhirInterpreter.T T_ASA) ()
  | AS -> print_value (MenhirInterpreter.T T_AS) ()
  | ARITHMETIC -> print_value (MenhirInterpreter.T T_ARITHMETIC) ()
  | ARGUMENT_VALUE -> print_value (MenhirInterpreter.T T_ARGUMENT_VALUE) ()
  | ARGUMENT_NUMBER -> print_value (MenhirInterpreter.T T_ARGUMENT_NUMBER) ()
  | AREA_VALUES -> print_value (MenhirInterpreter.T T_AREA_VALUES) ()
  | AREAS -> print_value (MenhirInterpreter.T T_AREAS) ()
  | AREA -> print_value (MenhirInterpreter.T T_AREA) ()
  | ARE -> print_value (MenhirInterpreter.T T_ARE) ()
  | APPLY -> print_value (MenhirInterpreter.T T_APPLY) ()
  | ANYCASE -> print_value (MenhirInterpreter.T T_ANYCASE) ()
  | ANY -> print_value (MenhirInterpreter.T T_ANY) ()
  | ANUM -> print_value (MenhirInterpreter.T T_ANUM) ()
  | ANSI -> print_value (MenhirInterpreter.T T_ANSI) ()
  | AND -> print_value (MenhirInterpreter.T T_AND) ()
  | AMPERSAND -> print_value (MenhirInterpreter.T T_AMPERSAND) ()
  | ALTERNATE -> print_value (MenhirInterpreter.T T_ALTERNATE) ()
  | ALTERING -> print_value (MenhirInterpreter.T T_ALTERING) ()
  | ALTER -> print_value (MenhirInterpreter.T T_ALTER) ()
  | ALSO -> print_value (MenhirInterpreter.T T_ALSO) ()
  | ALPHANUM_PREFIX v -> print_value (MenhirInterpreter.T T_ALPHANUM_PREFIX) v
  | ALPHANUMERIC_EDITED -> print_value (MenhirInterpreter.T T_ALPHANUMERIC_EDITED) ()
  | ALPHANUMERIC -> print_value (MenhirInterpreter.T T_ALPHANUMERIC) ()
  | ALPHANUM v -> print_value (MenhirInterpreter.T T_ALPHANUM) v
  | ALPHABETIC_UPPER -> print_value (MenhirInterpreter.T T_ALPHABETIC_UPPER) ()
  | ALPHABETIC_LOWER -> print_value (MenhirInterpreter.T T_ALPHABETIC_LOWER) ()
  | ALPHABETIC -> print_value (MenhirInterpreter.T T_ALPHABETIC) ()
  | ALPHABET -> print_value (MenhirInterpreter.T T_ALPHABET) ()
  | ALLOWING -> print_value (MenhirInterpreter.T T_ALLOWING) ()
  | ALLOCATE -> print_value (MenhirInterpreter.T T_ALLOCATE) ()
  | ALL -> print_value (MenhirInterpreter.T T_ALL) ()
  | ALIGNMENT -> print_value (MenhirInterpreter.T T_ALIGNMENT) ()
  | ALIGNED -> print_value (MenhirInterpreter.T T_ALIGNED) ()
  | ALIAS -> print_value (MenhirInterpreter.T T_ALIAS) ()
  | AFTER -> print_value (MenhirInterpreter.T T_AFTER) ()
  | ADVANCING -> print_value (MenhirInterpreter.T T_ADVANCING) ()
  | ADJUSTABLE_COLUMNS -> print_value (MenhirInterpreter.T T_ADJUSTABLE_COLUMNS) ()
  | ADDRESS -> print_value (MenhirInterpreter.T T_ADDRESS) ()
  | ADD -> print_value (MenhirInterpreter.T T_ADD) ()
  | ACTUAL -> print_value (MenhirInterpreter.T T_ACTUAL) ()
  | ACTIVE_X -> print_value (MenhirInterpreter.T T_ACTIVE_X) ()
  | ACTIVE_CLASS -> print_value (MenhirInterpreter.T T_ACTIVE_CLASS) ()
  | ACTIVATING -> print_value (MenhirInterpreter.T T_ACTIVATING) ()
  | ACTION -> print_value (MenhirInterpreter.T T_ACTION) ()
  | ACQUIRE -> print_value (MenhirInterpreter.T T_ACQUIRE) ()
  | ACCESS -> print_value (MenhirInterpreter.T T_ACCESS) ()
  | ACCEPT -> print_value (MenhirInterpreter.T T_ACCEPT) ()
  | ABSTRACT -> print_value (MenhirInterpreter.T T_ABSTRACT) ()
  | ABSENT -> print_value (MenhirInterpreter.T T_ABSENT) ()

let token_of_terminal (type a) (t : a MenhirInterpreter.terminal) (v : a) : token =
  match t with
  | T_error -> assert false
  | T_ZERO_FILL -> ZERO_FILL
  | T_ZERO -> ZERO
  | T_YYYYMMDD -> YYYYMMDD
  | T_YYYYDDD -> YYYYDDD
  | T_Y -> Y
  | T_XOR -> XOR
  | T_XML_TEXT -> XML_TEXT
  | T_XML_SCHEMA -> XML_SCHEMA
  | T_XML_NTEXT -> XML_NTEXT
  | T_XML_EVENT -> XML_EVENT
  | T_XML_DECLARATION -> XML_DECLARATION
  | T_XML -> XML
  | T_X -> X
  | T_WRITING -> WRITING
  | T_WRITE_VERIFY -> WRITE_VERIFY
  | T_WRITE_ONLY -> WRITE_ONLY
  | T_WRITERS -> WRITERS
  | T_WRITE -> WRITE
  | T_WRAP -> WRAP
  | T_WORKING_STORAGE -> WORKING_STORAGE
  | T_WORD_IN_AREA_A -> WORD_IN_AREA_A v
  | T_WORDS -> WORDS
  | T_WORD -> WORD v
  | T_WITH_DATA -> WITH_DATA
  | T_WITH -> WITH
  | T_WINDOW -> WINDOW
  | T_WIDTH_IN_CELLS -> WIDTH_IN_CELLS
  | T_WIDTH -> WIDTH
  | T_WIDE -> WIDE
  | T_WHILE -> WHILE
  | T_WHEN_COMPILED -> WHEN_COMPILED
  | T_WHEN -> WHEN
  | T_WEB_BROWSER -> WEB_BROWSER
  | T_WAIT -> WAIT
  | T_VTOP -> VTOP
  | T_VSCROLL_POS -> VSCROLL_POS
  | T_VSCROLL_BAR -> VSCROLL_BAR
  | T_VSCROLL -> VSCROLL
  | T_VPADDING -> VPADDING
  | T_VOLATILE -> VOLATILE
  | T_VLR -> VLR
  | T_VISIBLE -> VISIBLE
  | T_VIRTUAL_WIDTH -> VIRTUAL_WIDTH
  | T_VIRTUAL -> VIRTUAL
  | T_VIA -> VIA
  | T_VERY_HEAVY -> VERY_HEAVY
  | T_VERTICAL -> VERTICAL
  | T_VERSION -> VERSION
  | T_VARYING -> VARYING
  | T_VARIANT -> VARIANT
  | T_VARIABLE -> VARIABLE
  | T_VARBINARY -> VARBINARY
  | T_VALUE_FORMAT -> VALUE_FORMAT
  | T_VALUES -> VALUES
  | T_VALUE -> VALUE
  | T_VALIDATING -> VALIDATING
  | T_VALIDATE_STATUS -> VALIDATE_STATUS
  | T_VALIDATE -> VALIDATE
  | T_VALID -> VALID
  | T_V -> V
  | T_UTF_8 -> UTF_8
  | T_UTF_16 -> UTF_16
  | T_USING -> USING
  | T_USE_TAB -> USE_TAB
  | T_USE_RETURN -> USE_RETURN
  | T_USE_ALT -> USE_ALT
  | T_USER_WHITE -> USER_WHITE
  | T_USER_GRAY -> USER_GRAY
  | T_USER_DEFAULT -> USER_DEFAULT
  | T_USER_COLORS -> USER_COLORS
  | T_USER -> USER
  | T_USE -> USE
  | T_USAGE -> USAGE
  | T_UPPER -> UPPER
  | T_UPON -> UPON
  | T_UPDATERS -> UPDATERS
  | T_UPDATE -> UPDATE
  | T_UP -> UP
  | T_UNUSED__ -> UNUSED__
  | T_UNTIL -> UNTIL
  | T_UNSTRING -> UNSTRING
  | T_UNSORTED -> UNSORTED
  | T_UNSIGNED_SHORT -> UNSIGNED_SHORT
  | T_UNSIGNED_LONG -> UNSIGNED_LONG
  | T_UNSIGNED_INT -> UNSIGNED_INT
  | T_UNSIGNED -> UNSIGNED
  | T_UNSEQUAL -> UNSEQUAL
  | T_UNLOCK -> UNLOCK
  | T_UNIVERSAL -> UNIVERSAL
  | T_UNIT -> UNIT
  | T_UNFRAMED -> UNFRAMED
  | T_UNEQUAL -> UNEQUAL
  | T_UNDERLINE -> UNDERLINE
  | T_UNBOUNDED -> UNBOUNDED
  | T_UFF -> UFF
  | T_UCS_4 -> UCS_4
  | T_U -> U
  | T_TYPEDEF -> TYPEDEF
  | T_TYPE -> TYPE
  | T_TRUNCATION -> TRUNCATION
  | T_TRUE -> TRUE
  | T_TRIMMED -> TRIMMED
  | T_TREE_VIEW -> TREE_VIEW
  | T_TRANSPARENT_COLOR -> TRANSPARENT_COLOR
  | T_TRANSPARENT -> TRANSPARENT
  | T_TRANSFORM -> TRANSFORM
  | T_TRANSACTION_STATUS -> TRANSACTION_STATUS
  | T_TRANSACTION -> TRANSACTION
  | T_TRAILING_SIGN -> TRAILING_SIGN
  | T_TRAILING_SHIFT -> TRAILING_SHIFT
  | T_TRAILING -> TRAILING
  | T_TRADITIONAL_FONT -> TRADITIONAL_FONT
  | T_TRACK_THUMB -> TRACK_THUMB
  | T_TRACK_LIMIT -> TRACK_LIMIT
  | T_TRACK_AREA -> TRACK_AREA
  | T_TRACKS -> TRACKS
  | T_TRACK -> TRACK
  | T_TRACE -> TRACE
  | T_TOWARD_LESSER -> TOWARD_LESSER
  | T_TOWARD_GREATER -> TOWARD_GREATER
  | T_TOTALING -> TOTALING
  | T_TOTALED -> TOTALED
  | T_TOP_LEVEL -> TOP_LEVEL
  | T_TOP -> TOP
  | T_TOOL_BAR -> TOOL_BAR
  | T_TO -> TO
  | T_TITLE_POSITION -> TITLE_POSITION
  | T_TITLE_BAR -> TITLE_BAR
  | T_TITLE -> TITLE
  | T_TIME_RECORD -> TIME_RECORD
  | T_TIME_OUT -> TIME_OUT
  | T_TIME_OF_DAY -> TIME_OF_DAY
  | T_TIMESTAMP_RECORD -> TIMESTAMP_RECORD
  | T_TIMESTAMP_OFFSET_RECORD -> TIMESTAMP_OFFSET_RECORD
  | T_TIMESTAMP_OFFSET -> TIMESTAMP_OFFSET
  | T_TIMESTAMP -> TIMESTAMP
  | T_TIMES -> TIMES
  | T_TIME -> TIME
  | T_TILED_HEADINGS -> TILED_HEADINGS
  | T_THUMB_POSITION -> THUMB_POSITION
  | T_THROUGH -> THROUGH
  | T_THREEDIMENSIONAL -> THREEDIMENSIONAL
  | T_THREAD_POINTER -> THREAD_POINTER
  | T_THREAD_LOCAL_STORAGE -> THREAD_LOCAL_STORAGE
  | T_THREAD_LOCAL -> THREAD_LOCAL
  | T_THREADS -> THREADS
  | T_THREAD -> THREAD
  | T_THEN -> THEN
  | T_THAN -> THAN
  | T_TEXT -> TEXT
  | T_TEST -> TEST
  | T_TERMINATION_VALUE -> TERMINATION_VALUE
  | T_TERMINATE -> TERMINATE
  | T_TERMINAL_X -> TERMINAL_X
  | T_TERMINAL_INFO -> TERMINAL_INFO
  | T_TERMINAL_3 -> TERMINAL_3
  | T_TERMINAL_2 -> TERMINAL_2
  | T_TERMINAL_1 -> TERMINAL_1
  | T_TERMINAL_0 -> TERMINAL_0
  | T_TERMINAL -> TERMINAL
  | T_TEMPORARY -> TEMPORARY
  | T_TEMP -> TEMP
  | T_TAPE -> TAPE
  | T_TALLYING -> TALLYING
  | T_TALLY -> TALLY
  | T_TAB_TO_DELETE -> TAB_TO_DELETE
  | T_TAB_TO_ADD -> TAB_TO_ADD
  | T_TAB_CONTROL -> TAB_CONTROL
  | T_TABLE -> TABLE
  | T_TAB -> TAB
  | T_SYSTEM_OFFSET -> SYSTEM_OFFSET
  | T_SYSTEM_INFO -> SYSTEM_INFO
  | T_SYSTEM_DEFAULT -> SYSTEM_DEFAULT
  | T_SYSTEM -> SYSTEM
  | T_SYSOUT_X -> SYSOUT_X
  | T_SYSOUT_3 -> SYSOUT_3
  | T_SYSOUT_2 -> SYSOUT_2
  | T_SYSOUT_1 -> SYSOUT_1
  | T_SYSOUT_0 -> SYSOUT_0
  | T_SYSIN_X -> SYSIN_X
  | T_SYSIN_3 -> SYSIN_3
  | T_SYSIN_2 -> SYSIN_2
  | T_SYSIN_1 -> SYSIN_1
  | T_SYSIN_0 -> SYSIN_0
  | T_SYNCHRONIZED -> SYNCHRONIZED
  | T_SYMBOLIC -> SYMBOLIC
  | T_SYMBOL -> SYMBOL
  | T_SWITCH -> SWITCH
  | T_SUPPRESS -> SUPPRESS
  | T_SUPER -> SUPER
  | T_SUM -> SUM
  | T_SUFFIXING -> SUFFIXING
  | T_SUB_SCHEMA -> SUB_SCHEMA
  | T_SUB_QUEUE_3 -> SUB_QUEUE_3
  | T_SUB_QUEUE_2 -> SUB_QUEUE_2
  | T_SUB_QUEUE_1 -> SUB_QUEUE_1
  | T_SUBWINDOW -> SUBWINDOW
  | T_SUBTRACT -> SUBTRACT
  | T_SUBFILE -> SUBFILE
  | T_STYLE -> STYLE
  | T_STRUCTURE -> STRUCTURE
  | T_STRONG_NAME -> STRONG_NAME
  | T_STRONG -> STRONG
  | T_STRING -> STRING
  | T_STOP_BROWSER -> STOP_BROWSER
  | T_STOP -> STOP
  | T_STEP -> STEP
  | T_STDCALL -> STDCALL
  | T_STATUS_TEXT -> STATUS_TEXT
  | T_STATUS_BAR -> STATUS_BAR
  | T_STATUS -> STATUS
  | T_STATION -> STATION
  | T_STATIC_LIST -> STATIC_LIST
  | T_STATIC -> STATIC
  | T_STATEMENT -> STATEMENT
  | T_START_Y -> START_Y
  | T_START_X -> START_X
  | T_STARTING -> STARTING
  | T_START -> START
  | T_STANDARD_DECIMAL -> STANDARD_DECIMAL
  | T_STANDARD_BINARY -> STANDARD_BINARY
  | T_STANDARD_2 -> STANDARD_2
  | T_STANDARD_1 -> STANDARD_1
  | T_STANDARD -> STANDARD
  | T_STACK -> STACK
  | T_SSF -> SSF
  | T_SQUARE -> SQUARE
  | T_SQL_ROWID -> SQL_ROWID
  | T_SQL_NCLOB -> SQL_NCLOB
  | T_SQL_CURSOR -> SQL_CURSOR
  | T_SQL_CLOB -> SQL_CLOB
  | T_SQL_BLOB -> SQL_BLOB
  | T_SQL_BFILE -> SQL_BFILE
  | T_SQLIMS -> SQLIMS
  | T_SQL -> SQL
  | T_SPINNER -> SPINNER
  | T_SPECIAL_NAMES -> SPECIAL_NAMES
  | T_SPACE_FILL -> SPACE_FILL
  | T_SPACE -> SPACE
  | T_SOURCE_COMPUTER -> SOURCE_COMPUTER
  | T_SOURCES -> SOURCES
  | T_SOURCE -> SOURCE
  | T_SORT_WORK -> SORT_WORK
  | T_SORT_RETURN -> SORT_RETURN
  | T_SORT_ORDER -> SORT_ORDER
  | T_SORT_MODE_SIZE -> SORT_MODE_SIZE
  | T_SORT_MESSAGE -> SORT_MESSAGE
  | T_SORT_MERGE -> SORT_MERGE
  | T_SORT_FILE_SIZE -> SORT_FILE_SIZE
  | T_SORT_CORE_SIZE -> SORT_CORE_SIZE
  | T_SORT_CONTROL -> SORT_CONTROL
  | T_SORT -> SORT
  | T_SMALL_FONT -> SMALL_FONT
  | T_SLASH -> SLASH
  | T_SKIP3 -> SKIP3
  | T_SKIP2 -> SKIP2
  | T_SKIP1 -> SKIP1
  | T_SIZE -> SIZE
  | T_SINTLIT -> SINTLIT v
  | T_SIGNED_SHORT -> SIGNED_SHORT
  | T_SIGNED_LONG -> SIGNED_LONG
  | T_SIGNED_INT -> SIGNED_INT
  | T_SIGNED -> SIGNED
  | T_SIGN -> SIGN
  | T_SHOW_SEL_ALWAYS -> SHOW_SEL_ALWAYS
  | T_SHOW_NONE -> SHOW_NONE
  | T_SHOW_LINES -> SHOW_LINES
  | T_SHORT_DATE -> SHORT_DATE
  | T_SHORT -> SHORT
  | T_SHIFT_OUT -> SHIFT_OUT
  | T_SHIFT_IN -> SHIFT_IN
  | T_SHARING -> SHARING
  | T_SHADOW -> SHADOW
  | T_SHADING -> SHADING
  | T_SET -> SET
  | T_SERVICE -> SERVICE
  | T_SEQUENTIAL -> SEQUENTIAL
  | T_SEQUENCE -> SEQUENCE
  | T_SEPARATION -> SEPARATION
  | T_SEPARATE -> SEPARATE
  | T_SENTENCE -> SENTENCE
  | T_SEND -> SEND
  | T_SEMAPHORE_POINTER -> SEMAPHORE_POINTER
  | T_SELF_ACT -> SELF_ACT
  | T_SELFCLASS -> SELFCLASS
  | T_SELF -> SELF
  | T_SELECT_ALL -> SELECT_ALL
  | T_SELECTIVE -> SELECTIVE
  | T_SELECTION_TEXT -> SELECTION_TEXT
  | T_SELECTION_INDEX -> SELECTION_INDEX
  | T_SELECTION -> SELECTION
  | T_SELECT -> SELECT
  | T_SEGMENT_LIMIT -> SEGMENT_LIMIT
  | T_SEGMENT -> SEGMENT
  | T_SEEK -> SEEK
  | T_SECURITY -> SECURITY
  | T_SECURE -> SECURE
  | T_SECTION -> SECTION
  | T_SECONDS -> SECONDS
  | T_SECONDARY -> SECONDARY
  | T_SEARCH_TEXT -> SEARCH_TEXT
  | T_SEARCH_OPTIONS -> SEARCH_OPTIONS
  | T_SEARCH -> SEARCH
  | T_SD -> SD
  | T_SCROLL_BAR -> SCROLL_BAR
  | T_SCROLL -> SCROLL
  | T_SCREEN -> SCREEN
  | T_SAVE_AS_NO_PROMPT -> SAVE_AS_NO_PROMPT
  | T_SAVE_AS -> SAVE_AS
  | T_SARF -> SARF
  | T_SAME -> SAME
  | T_S -> S
  | T_RUN -> RUN
  | T_RPAR -> RPAR
  | T_ROW_PROTECTION -> ROW_PROTECTION
  | T_ROW_HEADINGS -> ROW_HEADINGS
  | T_ROW_FONT -> ROW_FONT
  | T_ROW_DIVIDERS -> ROW_DIVIDERS
  | T_ROW_COLOR_PATTERN -> ROW_COLOR_PATTERN
  | T_ROW_COLOR -> ROW_COLOR
  | T_ROWID -> ROWID
  | T_ROUNDING -> ROUNDING
  | T_ROUNDED -> ROUNDED
  | T_ROLLING -> ROLLING
  | T_ROLLBACK -> ROLLBACK
  | T_RIMMED -> RIMMED
  | T_RIGHT_JUSTIFY -> RIGHT_JUSTIFY
  | T_RIGHT_ALIGN -> RIGHT_ALIGN
  | T_RIGHT -> RIGHT
  | T_RH -> RH
  | T_RF -> RF
  | T_REWRITE -> REWRITE
  | T_REWIND -> REWIND
  | T_REVERSE_VIDEO -> REVERSE_VIDEO
  | T_REVERSED -> REVERSED
  | T_REVERSE -> REVERSE
  | T_RETURN_UNSIGNED -> RETURN_UNSIGNED
  | T_RETURN_CODE -> RETURN_CODE
  | T_RETURNING -> RETURNING
  | T_RETURN -> RETURN
  | T_RETRY -> RETRY
  | T_RETENTION -> RETENTION
  | T_RESUME -> RESUME
  | T_RESTRICTED -> RESTRICTED
  | T_RESIZABLE -> RESIZABLE
  | T_RESIDENT -> RESIDENT
  | T_RESET_TABS -> RESET_TABS
  | T_RESET_SET_LOCATOR -> RESET_SET_LOCATOR
  | T_RESET_LIST -> RESET_LIST
  | T_RESET_GRID -> RESET_GRID
  | T_RESET -> RESET
  | T_RESERVE -> RESERVE
  | T_RERUN -> RERUN
  | T_REREAD -> REREAD
  | T_REQUIRED -> REQUIRED
  | T_REPOSITORY -> REPOSITORY
  | T_REPORTS -> REPORTS
  | T_REPORTING -> REPORTING
  | T_REPORT -> REPORT
  | T_REPLACING -> REPLACING
  | T_REPLACED -> REPLACED
  | T_REPLACE -> REPLACE
  | T_REPEATED -> REPEATED
  | T_REORG_CRITERIA -> REORG_CRITERIA
  | T_RENAMES -> RENAMES
  | T_REMOVAL -> REMOVAL
  | T_REMARKS -> REMARKS
  | T_REMAINDER -> REMAINDER
  | T_RELOAD -> RELOAD
  | T_RELEASE -> RELEASE
  | T_RELATIVE -> RELATIVE
  | T_RELATION -> RELATION
  | T_REGION_COLOR -> REGION_COLOR
  | T_REFRESH -> REFRESH
  | T_REFERENCES -> REFERENCES
  | T_REFERENCE -> REFERENCE
  | T_REEL -> REEL
  | T_REDEFINITION -> REDEFINITION
  | T_REDEFINES -> REDEFINES
  | T_RECURSIVE -> RECURSIVE
  | T_RECORD_TO_DELETE -> RECORD_TO_DELETE
  | T_RECORD_TO_ADD -> RECORD_TO_ADD
  | T_RECORD_POSITION -> RECORD_POSITION
  | T_RECORD_OVERFLOW -> RECORD_OVERFLOW
  | T_RECORD_DATA -> RECORD_DATA
  | T_RECORDS -> RECORDS
  | T_RECORDING -> RECORDING
  | T_RECORD -> RECORD
  | T_RECEIVED -> RECEIVED
  | T_RECEIVE -> RECEIVE
  | T_READ_ONLY -> READ_ONLY
  | T_READY -> READY
  | T_READING -> READING
  | T_READERS -> READERS
  | T_READ -> READ
  | T_RD -> RD
  | T_RANGE -> RANGE
  | T_RANDOM -> RANDOM
  | T_RAISING -> RAISING
  | T_RAISED -> RAISED
  | T_RAISE -> RAISE
  | T_RADIO_BUTTON -> RADIO_BUTTON
  | T_QUOTE -> QUOTE
  | T_QUEUED -> QUEUED
  | T_QUEUE -> QUEUE
  | T_QUERY_INDEX -> QUERY_INDEX
  | T_PUSH_BUTTON -> PUSH_BUTTON
  | T_PURGE -> PURGE
  | T_PUBLIC -> PUBLIC
  | T_PROTOTYPE -> PROTOTYPE
  | T_PROTECTED -> PROTECTED
  | T_PROPERTY -> PROPERTY
  | T_PROPERTIES -> PROPERTIES
  | T_PROMPT -> PROMPT
  | T_PROHIBITED -> PROHIBITED
  | T_PROGRESS -> PROGRESS
  | T_PROGRAM_POINTER -> PROGRAM_POINTER
  | T_PROGRAM_ID -> PROGRAM_ID
  | T_PROGRAM -> PROGRAM
  | T_PROCESS_AREA -> PROCESS_AREA
  | T_PROCESSING -> PROCESSING
  | T_PROCESS -> PROCESS
  | T_PROCEED -> PROCEED
  | T_PROCEDURE_POINTER -> PROCEDURE_POINTER
  | T_PROCEDURE_NAME -> PROCEDURE_NAME
  | T_PROCEDURES -> PROCEDURES
  | T_PROCEDURE -> PROCEDURE
  | T_PRIVATE -> PRIVATE
  | T_PRIORITY -> PRIORITY
  | T_PRIOR -> PRIOR
  | T_PRINT_PREVIEW -> PRINT_PREVIEW
  | T_PRINT_NO_PROMPT -> PRINT_NO_PROMPT
  | T_PRINT_CONTROL -> PRINT_CONTROL
  | T_PRINTING -> PRINTING
  | T_PRINTER_1 -> PRINTER_1
  | T_PRINTER -> PRINTER
  | T_PRINT -> PRINT
  | T_PRIMARY -> PRIMARY
  | T_PREVIOUS -> PREVIOUS
  | T_PRESENT -> PRESENT
  | T_PREFIXING -> PREFIXING
  | T_PREFIXED -> PREFIXED
  | T_POSITIVE -> POSITIVE
  | T_POSITION_SHIFT -> POSITION_SHIFT
  | T_POSITIONING -> POSITIONING
  | T_POSITION -> POSITION
  | T_POS -> POS
  | T_POP_UP -> POP_UP
  | T_POINTER_32 -> POINTER_32
  | T_POINTER -> POINTER
  | T_PLUS_SIGN -> PLUS_SIGN
  | T_PLUS -> PLUS
  | T_PLACEMENT -> PLACEMENT
  | T_PIXEL -> PIXEL
  | T_PICTURE_STRING -> PICTURE_STRING v
  | T_PICTURE -> PICTURE
  | T_PHYSICAL -> PHYSICAL
  | T_PH -> PH
  | T_PF -> PF
  | T_PERMANENT -> PERMANENT
  | T_PERIOD -> PERIOD
  | T_PERFORM -> PERFORM
  | T_PASSWORD -> PASSWORD
  | T_PASCAL -> PASCAL
  | T_PARSE -> PARSE
  | T_PARENT -> PARENT
  | T_PARAGRAPH -> PARAGRAPH
  | T_PANEL_WIDTHS -> PANEL_WIDTHS
  | T_PANEL_TEXT -> PANEL_TEXT
  | T_PANEL_STYLE -> PANEL_STYLE
  | T_PANEL_INDEX -> PANEL_INDEX
  | T_PAGE_SIZE -> PAGE_SIZE
  | T_PAGE_SETUP -> PAGE_SETUP
  | T_PAGE_COUNTER -> PAGE_COUNTER
  | T_PAGED -> PAGED
  | T_PAGE -> PAGE
  | T_PADDING -> PADDING
  | T_PACKED_DECIMAL -> PACKED_DECIMAL
  | T_O_FILL -> O_FILL
  | T_OVERRIDING -> OVERRIDING
  | T_OVERRIDE -> OVERRIDE
  | T_OVERLINE -> OVERLINE
  | T_OVERLAP_TOP -> OVERLAP_TOP
  | T_OVERLAP_LEFT -> OVERLAP_LEFT
  | T_OVERLAPPED -> OVERLAPPED
  | T_OVERFLOW -> OVERFLOW
  | T_OUTPUT -> OUTPUT
  | T_OTHERWISE -> OTHERWISE
  | T_OTHERS -> OTHERS
  | T_OTHER -> OTHER
  | T_ORGANIZATION -> ORGANIZATION
  | T_ORDER -> ORDER
  | T_OR -> OR
  | T_OPTIONS -> OPTIONS
  | T_OPTIONAL -> OPTIONAL
  | T_OPERATIONAL -> OPERATIONAL
  | T_OPEN -> OPEN
  | T_OOSTACKPTR -> OOSTACKPTR
  | T_ON_SIZE_ERROR -> ON_SIZE_ERROR
  | T_ON_OVERFLOW -> ON_OVERFLOW
  | T_ON_EXCEPTION -> ON_EXCEPTION
  | T_ONLY -> ONLY
  | T_ON -> ON
  | T_OMITTED -> OMITTED
  | T_OK_BUTTON -> OK_BUTTON
  | T_OFF -> OFF
  | T_OF -> OF
  | T_OCCURS -> OCCURS
  | T_OBJECT_STORAGE -> OBJECT_STORAGE
  | T_OBJECT_REFERENCE -> OBJECT_REFERENCE
  | T_OBJECT_PROGRAM -> OBJECT_PROGRAM
  | T_OBJECT_ID -> OBJECT_ID
  | T_OBJECT_COMPUTER -> OBJECT_COMPUTER
  | T_OBJECT -> OBJECT
  | T_NUM_ROW_HEADINGS -> NUM_ROW_HEADINGS
  | T_NUM_ROWS -> NUM_ROWS
  | T_NUM_COL_HEADINGS -> NUM_COL_HEADINGS
  | T_NUMERIC_FILL -> NUMERIC_FILL
  | T_NUMERIC_EDITED -> NUMERIC_EDITED
  | T_NUMERIC -> NUMERIC
  | T_NUMBERS -> NUMBERS
  | T_NUMBER -> NUMBER
  | T_NULLS -> NULLS
  | T_NULLIT -> NULLIT v
  | T_NULL -> NULL
  | T_NO_UPDOWN -> NO_UPDOWN
  | T_NO_TAB -> NO_TAB
  | T_NO_SEARCH -> NO_SEARCH
  | T_NO_KEY_LETTER -> NO_KEY_LETTER
  | T_NO_GROUP_TAB -> NO_GROUP_TAB
  | T_NO_FOCUS -> NO_FOCUS
  | T_NO_F4 -> NO_F4
  | T_NO_ECHO -> NO_ECHO
  | T_NO_DIVIDERS -> NO_DIVIDERS
  | T_NO_DATA -> NO_DATA
  | T_NO_CLOSE -> NO_CLOSE
  | T_NO_CELL_DRAG -> NO_CELL_DRAG
  | T_NO_BOX -> NO_BOX
  | T_NO_AUTO_DEFAULT -> NO_AUTO_DEFAULT
  | T_NO_AUTOSEL -> NO_AUTOSEL
  | T_NOT_ON_SIZE_ERROR -> NOT_ON_SIZE_ERROR
  | T_NOT_ON_OVERFLOW -> NOT_ON_OVERFLOW
  | T_NOT_ON_EXCEPTION -> NOT_ON_EXCEPTION
  | T_NOT_INVALID_KEY -> NOT_INVALID_KEY
  | T_NOT_AT_EOP -> NOT_AT_EOP
  | T_NOT_AT_END -> NOT_AT_END
  | T_NOTIFY_SELCHANGE -> NOTIFY_SELCHANGE
  | T_NOTIFY_DBLCLICK -> NOTIFY_DBLCLICK
  | T_NOTIFY_CHANGE -> NOTIFY_CHANGE
  | T_NOTIFY -> NOTIFY
  | T_NOTHING -> NOTHING
  | T_NOTE -> NOTE
  | T_NOTAB -> NOTAB
  | T_NOT -> NOT
  | T_NORMAL -> NORMAL
  | T_NONNUMERIC -> NONNUMERIC
  | T_NONE -> NONE
  | T_NOMINAL -> NOMINAL
  | T_NO -> NO
  | T_NEXT_PAGE -> NEXT_PAGE
  | T_NEXT_ITEM -> NEXT_ITEM
  | T_NEXT -> NEXT
  | T_NEW -> NEW
  | T_NET_EVENT_LIST -> NET_EVENT_LIST
  | T_NESTED -> NESTED
  | T_NEGATIVE -> NEGATIVE
  | T_NEAREST_TO_ZERO -> NEAREST_TO_ZERO
  | T_NEAREST_TOWARD_ZERO -> NEAREST_TOWARD_ZERO
  | T_NEAREST_EVEN -> NEAREST_EVEN
  | T_NEAREST_AWAY_FROM_ZERO -> NEAREST_AWAY_FROM_ZERO
  | T_NE -> NE
  | T_NCLOB -> NCLOB
  | T_NCHAR -> NCHAR
  | T_NAVIGATE_URL -> NAVIGATE_URL
  | T_NATLIT -> NATLIT v
  | T_NATIVE -> NATIVE
  | T_NATIONAL_EDITED -> NATIONAL_EDITED
  | T_NATIONAL -> NATIONAL
  | T_NAT -> NAT
  | T_NAMESPACE_PREFIX -> NAMESPACE_PREFIX
  | T_NAMESPACE -> NAMESPACE
  | T_NAMED -> NAMED
  | T_NAME -> NAME
  | T_MUTEX_POINTER -> MUTEX_POINTER
  | T_MULTIPLY -> MULTIPLY
  | T_MULTIPLE -> MULTIPLE
  | T_MULTILINE -> MULTILINE
  | T_MOVE -> MOVE
  | T_MORE_LABELS -> MORE_LABELS
  | T_MONITOR_POINTER -> MONITOR_POINTER
  | T_MODULES -> MODULES
  | T_MODULE -> MODULE
  | T_MODIFY -> MODIFY
  | T_MODIFIED -> MODIFIED
  | T_MODELESS -> MODELESS
  | T_MODE -> MODE
  | T_MODAL -> MODAL
  | T_MIN_WIDTH -> MIN_WIDTH
  | T_MIN_VALUE -> MIN_VALUE
  | T_MIN_VAL -> MIN_VAL
  | T_MIN_SIZE -> MIN_SIZE
  | T_MIN_LINES -> MIN_LINES
  | T_MIN_HEIGHT -> MIN_HEIGHT
  | T_MINUS -> MINUS
  | T_MICROSECOND_TIME -> MICROSECOND_TIME
  | T_METHOD_ID -> METHOD_ID
  | T_METHOD -> METHOD
  | T_META_CLASS -> META_CLASS
  | T_MESSAGE_TAG -> MESSAGE_TAG
  | T_MESSAGES -> MESSAGES
  | T_MESSAGE -> MESSAGE
  | T_MERGE -> MERGE
  | T_MENU -> MENU
  | T_MEMORY -> MEMORY
  | T_MEDIUM_FONT -> MEDIUM_FONT
  | T_MDI_FRAME -> MDI_FRAME
  | T_MDI_CHILD -> MDI_CHILD
  | T_MAX_WIDTH -> MAX_WIDTH
  | T_MAX_VALUE -> MAX_VALUE
  | T_MAX_VAL -> MAX_VAL
  | T_MAX_TEXT -> MAX_TEXT
  | T_MAX_SIZE -> MAX_SIZE
  | T_MAX_PROGRESS -> MAX_PROGRESS
  | T_MAX_LINES -> MAX_LINES
  | T_MAX_HEIGHT -> MAX_HEIGHT
  | T_MASTER_INDEX -> MASTER_INDEX
  | T_MASS_UPDATE -> MASS_UPDATE
  | T_MANUAL -> MANUAL
  | T_MAGNETIC_TAPE -> MAGNETIC_TAPE
  | T_LT -> LT
  | T_LPAR -> LPAR
  | T_LOW_VALUE -> LOW_VALUE
  | T_LOW_COLOR -> LOW_COLOR
  | T_LOWLIGHT -> LOWLIGHT
  | T_LOWEST_VALUE -> LOWEST_VALUE
  | T_LOWERED -> LOWERED
  | T_LOWER -> LOWER
  | T_LOW -> LOW
  | T_LONG_VARCHAR -> LONG_VARCHAR
  | T_LONG_VARBINARY -> LONG_VARBINARY
  | T_LONG_DATE -> LONG_DATE
  | T_LOCK_HOLDING -> LOCK_HOLDING
  | T_LOCKS -> LOCKS
  | T_LOCK -> LOCK
  | T_LOCATION -> LOCATION
  | T_LOCAL_STORAGE -> LOCAL_STORAGE
  | T_LOCALE -> LOCALE
  | T_LOC -> LOC
  | T_LM_RESIZE -> LM_RESIZE
  | T_LIST_BOX -> LIST_BOX
  | T_LINKAGE -> LINKAGE
  | T_LINK -> LINK
  | T_LINE_SEQUENTIAL -> LINE_SEQUENTIAL
  | T_LINE_COUNTER -> LINE_COUNTER
  | T_LINES_PER_PAGE -> LINES_PER_PAGE
  | T_LINES_AT_ROOT -> LINES_AT_ROOT
  | T_LINES -> LINES
  | T_LINE -> LINE
  | T_LINAGE_COUNTER -> LINAGE_COUNTER
  | T_LINAGE -> LINAGE
  | T_LIN -> LIN
  | T_LIMITS -> LIMITS
  | T_LIMIT -> LIMIT
  | T_LIKE -> LIKE
  | T_LIBRARY -> LIBRARY
  | T_LESS -> LESS
  | T_LENGTH -> LENGTH
  | T_LEFT_TEXT -> LEFT_TEXT
  | T_LEFT_JUSTIFY -> LEFT_JUSTIFY
  | T_LEFTLINE -> LEFTLINE
  | T_LEFT -> LEFT
  | T_LEAVE -> LEAVE
  | T_LEADING_SHIFT -> LEADING_SHIFT
  | T_LEADING -> LEADING
  | T_LE -> LE
  | T_LC_TIME -> LC_TIME
  | T_LC_NUMERIC -> LC_NUMERIC
  | T_LC_MONETARY -> LC_MONETARY
  | T_LC_MESSAGES -> LC_MESSAGES
  | T_LC_CTYPE -> LC_CTYPE
  | T_LC_COLLATE -> LC_COLLATE
  | T_LC_ALL -> LC_ALL
  | T_LAYOUT_MANAGER -> LAYOUT_MANAGER
  | T_LAYOUT_DATA -> LAYOUT_DATA
  | T_LAST_ROW -> LAST_ROW
  | T_LAST -> LAST
  | T_LARGE_OFFSET -> LARGE_OFFSET
  | T_LARGE_FONT -> LARGE_FONT
  | T_LABEL_OFFSET -> LABEL_OFFSET
  | T_LABEL -> LABEL
  | T_KEY_LOCATION -> KEY_LOCATION
  | T_KEYED -> KEYED
  | T_KEYBOARD -> KEYBOARD
  | T_KEY -> KEY
  | T_KEPT -> KEPT
  | T_KANJI -> KANJI
  | T_JUSTIFIED -> JUSTIFIED
  | T_JSON_STATUS -> JSON_STATUS
  | T_JSON_CODE -> JSON_CODE
  | T_JSON -> JSON
  | T_JOINING -> JOINING
  | T_JNIENVPTR -> JNIENVPTR
  | T_JAVA -> JAVA
  | T_JAPANESE -> JAPANESE
  | T_I_O_CONTROL -> I_O_CONTROL
  | T_I_O -> I_O
  | T_ITEM_VALUE -> ITEM_VALUE
  | T_ITEM_TO_EMPTY -> ITEM_TO_EMPTY
  | T_ITEM_TO_DELETE -> ITEM_TO_DELETE
  | T_ITEM_TO_ADD -> ITEM_TO_ADD
  | T_ITEM_TEXT -> ITEM_TEXT
  | T_ITEM_ID -> ITEM_ID
  | T_ITEM_BOLD -> ITEM_BOLD
  | T_ITEM -> ITEM
  | T_IS_TYPEDEF -> IS_TYPEDEF
  | T_IS_GLOBAL -> IS_GLOBAL
  | T_IS_EXTERNAL -> IS_EXTERNAL
  | T_IS -> IS
  | T_IN_ARITHMETIC_RANGE -> IN_ARITHMETIC_RANGE
  | T_INVOKING -> INVOKING
  | T_INVOKED -> INVOKED
  | T_INVOKE -> INVOKE
  | T_INVALID_KEY -> INVALID_KEY
  | T_INVALID -> INVALID
  | T_INTRINSIC -> INTRINSIC
  | T_INTO -> INTO
  | T_INTERVENING_ -> INTERVENING_ v
  | T_INTERVAL_TIMER -> INTERVAL_TIMER
  | T_INTERMEDIATE -> INTERMEDIATE
  | T_INTERFACE_ID -> INTERFACE_ID
  | T_INTERFACE -> INTERFACE
  | T_INSTANCE -> INSTANCE
  | T_INSTALLATION -> INSTALLATION
  | T_INSPECT -> INSPECT
  | T_INSERT_ROWS -> INSERT_ROWS
  | T_INSERTION_INDEX -> INSERTION_INDEX
  | T_INSERT -> INSERT
  | T_INQUIRE -> INQUIRE
  | T_INPUT_OUTPUT -> INPUT_OUTPUT
  | T_INPUT -> INPUT
  | T_INITIATE -> INITIATE
  | T_INITIAL_VALUE -> INITIAL_VALUE
  | T_INITIALIZED -> INITIALIZED
  | T_INITIALIZE -> INITIALIZE
  | T_INITIAL -> INITIAL
  | T_INHERITS -> INHERITS
  | T_INHERITING -> INHERITING
  | T_INFO_WORD -> INFO_WORD v
  | T_INDICATORS -> INDICATORS
  | T_INDICATOR -> INDICATOR
  | T_INDICATE -> INDICATE
  | T_INDIC -> INDIC
  | T_INDEX_2 -> INDEX_2
  | T_INDEX_1 -> INDEX_1
  | T_INDEXED -> INDEXED
  | T_INDEX -> INDEX
  | T_INDEPENDENT -> INDEPENDENT
  | T_IN -> IN
  | T_IMPLEMENTS -> IMPLEMENTS
  | T_IMP -> IMP
  | T_IGNORING -> IGNORING
  | T_IGNORE -> IGNORE
  | T_IF -> IF
  | T_IDS_II -> IDS_II
  | T_IDENTIFIED -> IDENTIFIED
  | T_IDENTIFICATION -> IDENTIFICATION
  | T_ID -> ID
  | T_ICON -> ICON
  | T_HSCROLL_POS -> HSCROLL_POS
  | T_HSCROLL -> HSCROLL
  | T_HOT_TRACK -> HOT_TRACK
  | T_HORIZONTAL -> HORIZONTAL
  | T_HIGH_VALUE -> HIGH_VALUE
  | T_HIGH_ORDER_RIGHT -> HIGH_ORDER_RIGHT
  | T_HIGH_ORDER_LEFT -> HIGH_ORDER_LEFT
  | T_HIGH_COLOR -> HIGH_COLOR
  | T_HIGHLIGHT -> HIGHLIGHT
  | T_HIGHEST_VALUE -> HIGHEST_VALUE
  | T_HIGH -> HIGH
  | T_HIDDEN_DATA -> HIDDEN_DATA
  | T_HEXLIT -> HEXLIT v
  | T_HEX -> HEX
  | T_HELP_ID -> HELP_ID
  | T_HEIGHT_IN_CELLS -> HEIGHT_IN_CELLS
  | T_HEIGHT -> HEIGHT
  | T_HEAVY -> HEAVY
  | T_HEADING_FONT -> HEADING_FONT
  | T_HEADING_DIVIDER_COLOR -> HEADING_DIVIDER_COLOR
  | T_HEADING_COLOR -> HEADING_COLOR
  | T_HEADING -> HEADING
  | T_HAS_CHILDREN -> HAS_CHILDREN
  | T_HANDLE -> HANDLE
  | T_GT -> GT
  | T_GROUP_VALUE -> GROUP_VALUE
  | T_GROUP_USAGE -> GROUP_USAGE
  | T_GROUP -> GROUP
  | T_GRIP -> GRIP
  | T_GRID -> GRID
  | T_GREATER -> GREATER
  | T_GRAPHICAL -> GRAPHICAL
  | T_GO_SEARCH -> GO_SEARCH
  | T_GO_HOME -> GO_HOME
  | T_GO_FORWARD -> GO_FORWARD
  | T_GO_BACK -> GO_BACK
  | T_GOBACK -> GOBACK
  | T_GO -> GO
  | T_GLOBAL -> GLOBAL
  | T_GIVING -> GIVING
  | T_GET -> GET
  | T_GENERATE -> GENERATE
  | T_GE -> GE
  | T_GCOS -> GCOS
  | T_FUNCTION_POINTER -> FUNCTION_POINTER
  | T_FUNCTION_ID -> FUNCTION_ID
  | T_FUNCTION -> FUNCTION
  | T_FULL_HEIGHT -> FULL_HEIGHT
  | T_FULL -> FULL
  | T_FROM -> FROM
  | T_FREE -> FREE
  | T_FRAMED -> FRAMED
  | T_FRAME -> FRAME
  | T_FORMAT -> FORMAT
  | T_FOREVER -> FOREVER
  | T_FOREGROUND_COLOR -> FOREGROUND_COLOR
  | T_FOR -> FOR
  | T_FOOTING -> FOOTING
  | T_FONT -> FONT
  | T_FLR -> FLR
  | T_FLOAT_SHORT -> FLOAT_SHORT
  | T_FLOAT_NOT_A_NUMBER_SIGNALING -> FLOAT_NOT_A_NUMBER_SIGNALING
  | T_FLOAT_NOT_A_NUMBER_QUIET -> FLOAT_NOT_A_NUMBER_QUIET
  | T_FLOAT_NOT_A_NUMBER -> FLOAT_NOT_A_NUMBER
  | T_FLOAT_LONG -> FLOAT_LONG
  | T_FLOAT_INFINITY -> FLOAT_INFINITY
  | T_FLOAT_EXTENDED -> FLOAT_EXTENDED
  | T_FLOAT_DECIMAL_34 -> FLOAT_DECIMAL_34
  | T_FLOAT_DECIMAL_16 -> FLOAT_DECIMAL_16
  | T_FLOAT_DECIMAL -> FLOAT_DECIMAL
  | T_FLOAT_BINARY_64 -> FLOAT_BINARY_64
  | T_FLOAT_BINARY_32 -> FLOAT_BINARY_32
  | T_FLOAT_BINARY_128 -> FLOAT_BINARY_128
  | T_FLOAT_BINARY -> FLOAT_BINARY
  | T_FLOATLIT -> FLOATLIT v
  | T_FLOATING -> FLOATING
  | T_FLOAT -> FLOAT
  | T_FLAT_BUTTONS -> FLAT_BUTTONS
  | T_FLAT -> FLAT
  | T_FIXED_WIDTH -> FIXED_WIDTH
  | T_FIXED_FONT -> FIXED_FONT
  | T_FIXEDLIT -> FIXEDLIT v
  | T_FIXED -> FIXED
  | T_FIRST -> FIRST
  | T_FINISH_REASON -> FINISH_REASON
  | T_FINALLY -> FINALLY
  | T_FINAL -> FINAL
  | T_FILL_PERCENT -> FILL_PERCENT
  | T_FILL_COLOR2 -> FILL_COLOR2
  | T_FILL_COLOR -> FILL_COLOR
  | T_FILLER -> FILLER
  | T_FILE_PREFIX -> FILE_PREFIX
  | T_FILE_POS -> FILE_POS
  | T_FILE_PATH -> FILE_PATH
  | T_FILE_NAME -> FILE_NAME
  | T_FILE_LIMITS -> FILE_LIMITS
  | T_FILE_LIMIT -> FILE_LIMIT
  | T_FILE_ID -> FILE_ID
  | T_FILE_CONTROL -> FILE_CONTROL
  | T_FILES -> FILES
  | T_FILE -> FILE
  | T_FIELD_TERMINATOR -> FIELD_TERMINATOR
  | T_FH__KEYDEF -> FH__KEYDEF
  | T_FH__FCD -> FH__FCD
  | T_FD -> FD
  | T_FARTHEST_FROM_ZERO -> FARTHEST_FROM_ZERO
  | T_FALSE -> FALSE
  | T_FACTORY -> FACTORY
  | T_F -> F
  | T_EXTERNAL_FORM -> EXTERNAL_FORM
  | T_EXTERNALLY_DESCRIBED_KEY -> EXTERNALLY_DESCRIBED_KEY
  | T_EXTERNAL -> EXTERNAL
  | T_EXTERN -> EXTERN
  | T_EXTENDED_SEARCH -> EXTENDED_SEARCH
  | T_EXTEND -> EXTEND
  | T_EXPANDS -> EXPANDS
  | T_EXPAND -> EXPAND
  | T_EXIT -> EXIT
  | T_EXHIBIT -> EXHIBIT
  | T_EXECUTE -> EXECUTE
  | T_EXEC -> EXEC
  | T_EXCLUSIVE_OR -> EXCLUSIVE_OR
  | T_EXCLUSIVE -> EXCLUSIVE
  | T_EXCLUDE_EVENT_LIST -> EXCLUDE_EVENT_LIST
  | T_EXCESS_3 -> EXCESS_3
  | T_EXCEPTION_VALUE -> EXCEPTION_VALUE
  | T_EXCEPTION_OBJECT -> EXCEPTION_OBJECT
  | T_EXCEPTION -> EXCEPTION
  | T_EXCEEDS -> EXCEEDS
  | T_EXAMINE -> EXAMINE
  | T_EVERY -> EVERY
  | T_EVENT_POINTER -> EVENT_POINTER
  | T_EVENT_LIST -> EVENT_LIST
  | T_EVENT -> EVENT
  | T_EVALUATE -> EVALUATE
  | T_ESI -> ESI
  | T_ESCAPE_BUTTON -> ESCAPE_BUTTON
  | T_ESCAPE -> ESCAPE
  | T_ERROR -> ERROR
  | T_ERASE -> ERASE
  | T_EQUAL -> EQUAL
  | T_EQ -> EQ
  | T_EOS -> EOS
  | T_EOP -> EOP
  | T_EOL -> EOL
  | T_EOF -> EOF
  | T_EO -> EO
  | T_ENVIRONMENT_VALUE -> ENVIRONMENT_VALUE
  | T_ENVIRONMENT_NAME -> ENVIRONMENT_NAME
  | T_ENVIRONMENT -> ENVIRONMENT
  | T_ENTRY_REASON -> ENTRY_REASON
  | T_ENTRY_FIELD -> ENTRY_FIELD
  | T_ENTRY_CONVENTION -> ENTRY_CONVENTION
  | T_ENTRY -> ENTRY
  | T_ENTER -> ENTER
  | T_ENSURE_VISIBLE -> ENSURE_VISIBLE
  | T_ENGRAVED -> ENGRAVED
  | T_END_XML -> END_XML
  | T_END_WRITE -> END_WRITE
  | T_END_WAIT -> END_WAIT
  | T_END_USE -> END_USE
  | T_END_UNSTRING -> END_UNSTRING
  | T_END_SUBTRACT -> END_SUBTRACT
  | T_END_STRING -> END_STRING
  | T_END_START -> END_START
  | T_END_SET -> END_SET
  | T_END_SEND -> END_SEND
  | T_END_SEARCH -> END_SEARCH
  | T_END_REWRITE -> END_REWRITE
  | T_END_RETURN -> END_RETURN
  | T_END_REPLACE -> END_REPLACE
  | T_END_RECEIVE -> END_RECEIVE
  | T_END_READ -> END_READ
  | T_END_PERFORM -> END_PERFORM
  | T_END_ON -> END_ON
  | T_END_OF_PAGE -> END_OF_PAGE
  | T_END_MULTIPLY -> END_MULTIPLY
  | T_END_MOVE -> END_MOVE
  | T_END_MODIFY -> END_MODIFY
  | T_END_JSON -> END_JSON
  | T_END_INVOKE -> END_INVOKE
  | T_END_IF -> END_IF
  | T_END_EXEC -> END_EXEC
  | T_END_EVALUATE -> END_EVALUATE
  | T_END_ENABLE -> END_ENABLE
  | T_END_DIVIDE -> END_DIVIDE
  | T_END_DISPLAY -> END_DISPLAY
  | T_END_DISABLE -> END_DISABLE
  | T_END_DELETE -> END_DELETE
  | T_END_COPY -> END_COPY
  | T_END_COMPUTE -> END_COMPUTE
  | T_END_COLOR -> END_COLOR
  | T_END_CHAIN -> END_CHAIN
  | T_END_CALL -> END_CALL
  | T_END_ADD -> END_ADD
  | T_END_ACCEPT -> END_ACCEPT
  | T_ENDING -> ENDING
  | T_END -> END
  | T_ENCRYPTION -> ENCRYPTION
  | T_ENCODING -> ENCODING
  | T_ENABLED -> ENABLED
  | T_ENABLE -> ENABLE
  | T_EMI -> EMI
  | T_ELSE -> ELSE
  | T_ELEMENT -> ELEMENT
  | T_EJECT -> EJECT
  | T_EIGHTY_EIGHT -> EIGHTY_EIGHT
  | T_EGI -> EGI
  | T_EGCS -> EGCS
  | T_EGC -> EGC
  | T_EDITING -> EDITING
  | T_ECHO -> ECHO
  | T_EC -> EC
  | T_EBCDIC -> EBCDIC
  | T_DYNAMIC -> DYNAMIC
  | T_DUPLICATES -> DUPLICATES
  | T_DROP_LIST -> DROP_LIST
  | T_DROP_DOWN -> DROP_DOWN
  | T_DROP -> DROP
  | T_DRAW -> DRAW
  | T_DRAG_COLOR -> DRAG_COLOR
  | T_DOWN -> DOWN
  | T_DOUBLE_COLON -> DOUBLE_COLON
  | T_DOUBLE_ASTERISK -> DOUBLE_ASTERISK
  | T_DOUBLE -> DOUBLE
  | T_DOT_DASH -> DOT_DASH
  | T_DOTTED -> DOTTED
  | T_DOTDASH -> DOTDASH
  | T_DIVISION -> DIVISION
  | T_DIVIDER_COLOR -> DIVIDER_COLOR
  | T_DIVIDERS -> DIVIDERS
  | T_DIVIDE -> DIVIDE
  | T_DISPLAY_ST -> DISPLAY_ST
  | T_DISPLAY_FORMAT -> DISPLAY_FORMAT
  | T_DISPLAY_COLUMNS -> DISPLAY_COLUMNS
  | T_DISPLAY_4 -> DISPLAY_4
  | T_DISPLAY_3 -> DISPLAY_3
  | T_DISPLAY_2 -> DISPLAY_2
  | T_DISPLAY_1 -> DISPLAY_1
  | T_DISPLAY -> DISPLAY
  | T_DISP -> DISP
  | T_DISK -> DISK
  | T_DISJOINING -> DISJOINING
  | T_DISCONNECT -> DISCONNECT
  | T_DISC -> DISC
  | T_DISABLE -> DISABLE
  | T_DIGITS -> DIGITS v
  | T_DETAIL -> DETAIL
  | T_DESTROY -> DESTROY
  | T_DESTINATION -> DESTINATION
  | T_DESCRIPTOR -> DESCRIPTOR
  | T_DESCENDING -> DESCENDING
  | T_DEPENDING -> DEPENDING
  | T_DELIMITER -> DELIMITER
  | T_DELIMITED -> DELIMITED
  | T_DELETE -> DELETE
  | T_DEFINITION -> DEFINITION
  | T_DEFAULT_FONT -> DEFAULT_FONT
  | T_DEFAULT_BUTTON -> DEFAULT_BUTTON
  | T_DEFAULT -> DEFAULT
  | T_DECLARE -> DECLARE
  | T_DECLARATIVES -> DECLARATIVES
  | T_DECIMAL_POINT -> DECIMAL_POINT
  | T_DECIMAL_ENCODING -> DECIMAL_ENCODING
  | T_DEBUG_SUB_3 -> DEBUG_SUB_3
  | T_DEBUG_SUB_2 -> DEBUG_SUB_2
  | T_DEBUG_SUB_1 -> DEBUG_SUB_1
  | T_DEBUG_NAME -> DEBUG_NAME
  | T_DEBUG_LINE -> DEBUG_LINE
  | T_DEBUG_ITEM -> DEBUG_ITEM
  | T_DEBUG_CONTENTS -> DEBUG_CONTENTS
  | T_DEBUGGING -> DEBUGGING
  | T_DEBUG -> DEBUG
  | T_DBCS -> DBCS
  | T_DBCLOB_LOCATOR -> DBCLOB_LOCATOR
  | T_DBCLOB_FILE -> DBCLOB_FILE
  | T_DBCLOB -> DBCLOB
  | T_DAY_OF_WEEK -> DAY_OF_WEEK
  | T_DAY_AND_TIME -> DAY_AND_TIME
  | T_DAY -> DAY
  | T_DATE_WRITTEN -> DATE_WRITTEN
  | T_DATE_RECORD -> DATE_RECORD
  | T_DATE_MODIFIED -> DATE_MODIFIED
  | T_DATE_ENTRY -> DATE_ENTRY
  | T_DATE_COMPILED -> DATE_COMPILED
  | T_DATE_AND_TIME -> DATE_AND_TIME
  | T_DATE -> DATE
  | T_DATA_TYPES -> DATA_TYPES
  | T_DATA_RECORDS -> DATA_RECORDS
  | T_DATA_RECORD -> DATA_RECORD
  | T_DATA_POINTER -> DATA_POINTER
  | T_DATA_COLUMNS -> DATA_COLUMNS
  | T_DATA -> DATA
  | T_DASH_SIGN -> DASH_SIGN
  | T_DASHED -> DASHED
  | T_CYL_OVERFLOW -> CYL_OVERFLOW
  | T_CYL_INDEX -> CYL_INDEX
  | T_CYCLE -> CYCLE
  | T_CUSTOM_PRINT_TEMPLATE -> CUSTOM_PRINT_TEMPLATE
  | T_CURSOR_Y -> CURSOR_Y
  | T_CURSOR_X -> CURSOR_X
  | T_CURSOR_ROW -> CURSOR_ROW
  | T_CURSOR_FRAME_WIDTH -> CURSOR_FRAME_WIDTH
  | T_CURSOR_COLOR -> CURSOR_COLOR
  | T_CURSOR_COL -> CURSOR_COL
  | T_CURSOR -> CURSOR
  | T_CURRENT_DATE -> CURRENT_DATE
  | T_CURRENT -> CURRENT
  | T_CURRENCY -> CURRENCY
  | T_CULTURE -> CULTURE
  | T_CS_GENERAL -> CS_GENERAL
  | T_CS_BASIC -> CS_BASIC
  | T_CSP -> CSP
  | T_CSIZE -> CSIZE
  | T_CRT_UNDER -> CRT_UNDER
  | T_CRT -> CRT
  | T_CREATE -> CREATE
  | T_COUNT_TRAILING -> COUNT_TRAILING
  | T_COUNT_MIN -> COUNT_MIN
  | T_COUNT_MAX -> COUNT_MAX
  | T_COUNT_LEADLING -> COUNT_LEADLING
  | T_COUNT -> COUNT
  | T_CORRESPONDING -> CORRESPONDING
  | T_CORE_INDEX -> CORE_INDEX
  | T_COPY_SELECTION -> COPY_SELECTION
  | T_COPY -> COPY
  | T_CONVERTING -> CONVERTING
  | T_CONVERT -> CONVERT
  | T_CONVERSION -> CONVERSION
  | T_CONTROL_AREA -> CONTROL_AREA
  | T_CONTROLS_UNCROPPED -> CONTROLS_UNCROPPED
  | T_CONTROLS -> CONTROLS
  | T_CONTROL -> CONTROL
  | T_CONTINUE -> CONTINUE
  | T_CONTENT_OF -> CONTENT_OF
  | T_CONTENT -> CONTENT
  | T_CONTAINS -> CONTAINS
  | T_CONSTRUCTOR -> CONSTRUCTOR
  | T_CONSTANT_RECORD -> CONSTANT_RECORD
  | T_CONSTANT -> CONSTANT
  | T_CONSOLE_3 -> CONSOLE_3
  | T_CONSOLE_2 -> CONSOLE_2
  | T_CONSOLE_1 -> CONSOLE_1
  | T_CONSOLE_0 -> CONSOLE_0
  | T_CONNECT -> CONNECT
  | T_CONFIGURATION -> CONFIGURATION
  | T_CONDITION -> CONDITION
  | T_COM_REG -> COM_REG
  | T_COMP_X -> COMP_X
  | T_COMP_N -> COMP_N
  | T_COMP_9 -> COMP_9
  | T_COMP_7 -> COMP_7
  | T_COMP_6 -> COMP_6
  | T_COMP_5 -> COMP_5
  | T_COMP_4 -> COMP_4
  | T_COMP_3 -> COMP_3
  | T_COMP_2 -> COMP_2
  | T_COMP_15 -> COMP_15
  | T_COMP_14 -> COMP_14
  | T_COMP_13 -> COMP_13
  | T_COMP_12 -> COMP_12
  | T_COMP_11 -> COMP_11
  | T_COMP_10 -> COMP_10
  | T_COMP_1 -> COMP_1
  | T_COMP_0 -> COMP_0
  | T_COMPUTE -> COMPUTE
  | T_COMPUTATIONAL_7 -> COMPUTATIONAL_7
  | T_COMPUTATIONAL_14 -> COMPUTATIONAL_14
  | T_COMPUTATIONAL_13 -> COMPUTATIONAL_13
  | T_COMPUTATIONAL_12 -> COMPUTATIONAL_12
  | T_COMPUTATIONAL_11 -> COMPUTATIONAL_11
  | T_COMPRESSION -> COMPRESSION
  | T_COMPLEMENTARY -> COMPLEMENTARY
  | T_COMPLE -> COMPLE
  | T_COMP -> COMP
  | T_COMMUNICATION -> COMMUNICATION
  | T_COMMON -> COMMON
  | T_COMMITMENT -> COMMITMENT
  | T_COMMIT -> COMMIT
  | T_COMMENT_ENTRY -> COMMENT_ENTRY v
  | T_COMMAND_LINE -> COMMAND_LINE
  | T_COMMA -> COMMA
  | T_COMBO_BOX -> COMBO_BOX
  | T_COLUMN_PROTECTION -> COLUMN_PROTECTION
  | T_COLUMN_HEADINGS -> COLUMN_HEADINGS
  | T_COLUMN_FONT -> COLUMN_FONT
  | T_COLUMN_DIVIDERS -> COLUMN_DIVIDERS
  | T_COLUMN_COLOR -> COLUMN_COLOR
  | T_COLUMNS -> COLUMNS
  | T_COLUMN -> COLUMN
  | T_COLORS -> COLORS
  | T_COLOR -> COLOR
  | T_COLON -> COLON
  | T_COLLATING -> COLLATING
  | T_COL -> COL
  | T_COERCION -> COERCION
  | T_CODE_SET -> CODE_SET
  | T_CODE -> CODE
  | T_COBOL -> COBOL
  | T_CLOSE -> CLOSE
  | T_CLOCK_UNITS -> CLOCK_UNITS
  | T_CLOB_LOCATOR -> CLOB_LOCATOR
  | T_CLOB_FILE -> CLOB_FILE
  | T_CLOB -> CLOB
  | T_CLINES -> CLINES
  | T_CLINE -> CLINE
  | T_CLEAR_SELECTION -> CLEAR_SELECTION
  | T_CLASS_OBJECT -> CLASS_OBJECT
  | T_CLASS_NAME -> CLASS_NAME
  | T_CLASS_ID -> CLASS_ID
  | T_CLASS_CONTROL -> CLASS_CONTROL
  | T_CLASSIFICATION -> CLASSIFICATION
  | T_CLASS -> CLASS
  | T_CICS -> CICS
  | T_CHECK_BOX -> CHECK_BOX
  | T_CHECKPOINT_FILE -> CHECKPOINT_FILE
  | T_CHECK -> CHECK
  | T_CHAR_VARYING -> CHAR_VARYING
  | T_CHART -> CHART
  | T_CHARACTERS -> CHARACTERS
  | T_CHARACTER -> CHARACTER
  | T_CHAR -> CHAR
  | T_CHANGED -> CHANGED
  | T_CHAINING -> CHAINING
  | T_CHAIN -> CHAIN
  | T_CH -> CH
  | T_CF -> CF
  | T_CENTURY_DAY -> CENTURY_DAY
  | T_CENTURY_DATE -> CENTURY_DATE
  | T_CENTERED_HEADINGS -> CENTERED_HEADINGS
  | T_CENTERED -> CENTERED
  | T_CENTER -> CENTER
  | T_CELL_PROTECTION -> CELL_PROTECTION
  | T_CELL_FONT -> CELL_FONT
  | T_CELL_DATA -> CELL_DATA
  | T_CELL_COLOR -> CELL_COLOR
  | T_CELL -> CELL
  | T_CD -> CD
  | T_CCOL -> CCOL
  | T_CBL -> CBL
  | T_CATALOGUE_NAME -> CATALOGUE_NAME
  | T_CATALOGUED -> CATALOGUED
  | T_CASSETTE -> CASSETTE
  | T_CASE_SENSITIVE -> CASE_SENSITIVE
  | T_CASE_INSENSITIVE -> CASE_INSENSITIVE
  | T_CARD_READER -> CARD_READER
  | T_CARD_PUNCH -> CARD_PUNCH
  | T_CAPACITY -> CAPACITY
  | T_CANCEL_BUTTON -> CANCEL_BUTTON
  | T_CANCEL -> CANCEL
  | T_CALLED -> CALLED
  | T_CALL -> CALL
  | T_CALENDAR_FONT -> CALENDAR_FONT
  | T_C -> C
  | T_B_XOR -> B_XOR
  | T_B_SHIFT_RC -> B_SHIFT_RC
  | T_B_SHIFT_R -> B_SHIFT_R
  | T_B_SHIFT_LC -> B_SHIFT_LC
  | T_B_SHIFT_L -> B_SHIFT_L
  | T_B_OR -> B_OR
  | T_B_NOT -> B_NOT
  | T_B_EXOR -> B_EXOR
  | T_B_AND -> B_AND
  | T_BYTE_LENGTH -> BYTE_LENGTH
  | T_BYTES -> BYTES
  | T_BYTE -> BYTE
  | T_BY -> BY
  | T_BUTTONS -> BUTTONS
  | T_BUSY -> BUSY
  | T_BULK_ADDITION -> BULK_ADDITION
  | T_BSN -> BSN
  | T_BROWSING -> BROWSING
  | T_BOXED -> BOXED
  | T_BOX -> BOX
  | T_BOTTOM -> BOTTOM
  | T_BOOLIT -> BOOLIT v
  | T_BOOLEAN -> BOOLEAN
  | T_BLOCK -> BLOCK
  | T_BLOB_LOCATOR -> BLOB_LOCATOR
  | T_BLOB_FILE -> BLOB_FILE
  | T_BLOB -> BLOB
  | T_BLINK -> BLINK
  | T_BLANK -> BLANK
  | T_BITS -> BITS
  | T_BITMAP_WIDTH -> BITMAP_WIDTH
  | T_BITMAP_TRANSPARENT_COLOR -> BITMAP_TRANSPARENT_COLOR
  | T_BITMAP_TRAILING -> BITMAP_TRAILING
  | T_BITMAP_TIMER -> BITMAP_TIMER
  | T_BITMAP_START -> BITMAP_START
  | T_BITMAP_SCALE -> BITMAP_SCALE
  | T_BITMAP_RAW_WIDTH -> BITMAP_RAW_WIDTH
  | T_BITMAP_RAW_HEIGHT -> BITMAP_RAW_HEIGHT
  | T_BITMAP_NUMBER -> BITMAP_NUMBER
  | T_BITMAP_HANDLE -> BITMAP_HANDLE
  | T_BITMAP_END -> BITMAP_END
  | T_BITMAP -> BITMAP
  | T_BIT -> BIT
  | T_BIND -> BIND
  | T_BINARY_SHORT -> BINARY_SHORT
  | T_BINARY_SEQUENTIAL -> BINARY_SEQUENTIAL
  | T_BINARY_LONG -> BINARY_LONG
  | T_BINARY_ENCODING -> BINARY_ENCODING
  | T_BINARY_DOUBLE -> BINARY_DOUBLE
  | T_BINARY_C_LONG -> BINARY_C_LONG
  | T_BINARY_CHAR -> BINARY_CHAR
  | T_BINARY -> BINARY
  | T_BELL -> BELL
  | T_BEGINNING -> BEGINNING
  | T_BEFORE -> BEFORE
  | T_BECOMES -> BECOMES
  | T_BASIS -> BASIS
  | T_BASED -> BASED
  | T_BAR -> BAR
  | T_BACKWARD -> BACKWARD
  | T_BACKGROUND_STANDARD -> BACKGROUND_STANDARD
  | T_BACKGROUND_LOW -> BACKGROUND_LOW
  | T_BACKGROUND_HIGH -> BACKGROUND_HIGH
  | T_BACKGROUND_COLOR -> BACKGROUND_COLOR
  | T_AX_EVENT_LIST -> AX_EVENT_LIST
  | T_AWAY_FROM_ZERO -> AWAY_FROM_ZERO
  | T_AUTO_SPIN -> AUTO_SPIN
  | T_AUTO_RESIZE -> AUTO_RESIZE
  | T_AUTO_MINIMIZE -> AUTO_MINIMIZE
  | T_AUTO_HYPHEN_SKIP -> AUTO_HYPHEN_SKIP
  | T_AUTO_DECIMAL -> AUTO_DECIMAL
  | T_AUTOMATIC -> AUTOMATIC
  | T_AUTO -> AUTO
  | T_AUTHOR -> AUTHOR
  | T_AT_EOP -> AT_EOP
  | T_AT_END -> AT_END
  | T_ATTRIBUTES -> ATTRIBUTES
  | T_ATTRIBUTE -> ATTRIBUTE
  | T_AT -> AT
  | T_ASTERISK -> ASTERISK
  | T_ASSIGN -> ASSIGN
  | T_ASSEMBLY_NAME -> ASSEMBLY_NAME
  | T_ASCII -> ASCII
  | T_ASCENDING -> ASCENDING
  | T_ASA -> ASA
  | T_AS -> AS
  | T_ARITHMETIC -> ARITHMETIC
  | T_ARGUMENT_VALUE -> ARGUMENT_VALUE
  | T_ARGUMENT_NUMBER -> ARGUMENT_NUMBER
  | T_AREA_VALUES -> AREA_VALUES
  | T_AREAS -> AREAS
  | T_AREA -> AREA
  | T_ARE -> ARE
  | T_APPLY -> APPLY
  | T_ANYCASE -> ANYCASE
  | T_ANY -> ANY
  | T_ANUM -> ANUM
  | T_ANSI -> ANSI
  | T_AND -> AND
  | T_AMPERSAND -> AMPERSAND
  | T_ALTERNATE -> ALTERNATE
  | T_ALTERING -> ALTERING
  | T_ALTER -> ALTER
  | T_ALSO -> ALSO
  | T_ALPHANUM_PREFIX -> ALPHANUM_PREFIX v
  | T_ALPHANUMERIC_EDITED -> ALPHANUMERIC_EDITED
  | T_ALPHANUMERIC -> ALPHANUMERIC
  | T_ALPHANUM -> ALPHANUM v
  | T_ALPHABETIC_UPPER -> ALPHABETIC_UPPER
  | T_ALPHABETIC_LOWER -> ALPHABETIC_LOWER
  | T_ALPHABETIC -> ALPHABETIC
  | T_ALPHABET -> ALPHABET
  | T_ALLOWING -> ALLOWING
  | T_ALLOCATE -> ALLOCATE
  | T_ALL -> ALL
  | T_ALIGNMENT -> ALIGNMENT
  | T_ALIGNED -> ALIGNED
  | T_ALIAS -> ALIAS
  | T_AFTER -> AFTER
  | T_ADVANCING -> ADVANCING
  | T_ADJUSTABLE_COLUMNS -> ADJUSTABLE_COLUMNS
  | T_ADDRESS -> ADDRESS
  | T_ADD -> ADD
  | T_ACTUAL -> ACTUAL
  | T_ACTIVE_X -> ACTIVE_X
  | T_ACTIVE_CLASS -> ACTIVE_CLASS
  | T_ACTIVATING -> ACTIVATING
  | T_ACTION -> ACTION
  | T_ACQUIRE -> ACQUIRE
  | T_ACCESS -> ACCESS
  | T_ACCEPT -> ACCEPT
  | T_ABSTRACT -> ABSTRACT
  | T_ABSENT -> ABSENT
