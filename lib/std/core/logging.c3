module std::core::log;
import std::io, std::thread, std::time, std::math::random;

<* Whether log calls capture the file/function/line of the call site. *>
const FULL_LOG = env::COMPILER_SAFE_MODE || $feat(FULL_LOG);

<* An identifier for a log category, used to filter and label log output. Backed by a `char`, so valid values range 0-255. *>
typedef LogCategory = inline char;
<* A short tag attached to log output for correlation, e.g. via `@tag_scope`. *>
typedef LogTag = char[12];

<*
 Built-in log categories, numbered 0-14. Categories 15-99 are reserved for
 `register_dynamic_category`; see `CATEGORY_CUSTOM_START` for the range applications may use
 directly.
*>
const LogCategory CATEGORY_APPLICATION   = (LogCategory)0;
const LogCategory CATEGORY_SYSTEM        = (LogCategory)1;
const LogCategory CATEGORY_KERNEL        = (LogCategory)2;
const LogCategory CATEGORY_AUDIO         = (LogCategory)3;
const LogCategory CATEGORY_VIDEO         = (LogCategory)4;
const LogCategory CATEGORY_RENDER        = (LogCategory)5;
const LogCategory CATEGORY_INPUT         = (LogCategory)6;
const LogCategory CATEGORY_NETWORK       = (LogCategory)7;
const LogCategory CATEGORY_SOCKET        = (LogCategory)8;
const LogCategory CATEGORY_SECURITY      = (LogCategory)9;
const LogCategory CATEGORY_TEST          = (LogCategory)10;
const LogCategory CATEGORY_ERROR         = (LogCategory)11;
const LogCategory CATEGORY_ASSERT        = (LogCategory)12;
const LogCategory CATEGORY_CRASH         = (LogCategory)13;
const LogCategory CATEGORY_STATS         = (LogCategory)14;
<* The first category id handed out by `register_dynamic_category`. *>
const LogCategory CATEGORY_DYNAMIC_START = (LogCategory)15;
<* All categories from 100 and upwards are available for applications *>
const LogCategory CATEGORY_CUSTOM_START  = (LogCategory)100;

<* The category new log calls use when none is explicitly given; see `@category_scope`. *>
tlocal LogCategory default_category = CATEGORY_APPLICATION;
<* The tag attached to new log calls; see `@tag_scope`/`push_tag`. *>
tlocal LogTag current_tag;

<* Returned by `register_dynamic_category` once all dynamic category slots are in use. *>
excuse NO_MORE_DYNAMIC_CATEGORIES;

<* The severity of a log message, from least to most severe. *>
enum LogPriority : int
{
	VERBOSE,
	DEBUG,
	INFO,
	WARN,
	ERROR,
	CRITICAL,
}

<*
 Implement this to receive log output via `set_logger`.
*>
interface Logger
{
	<*
	 Handle a single log message. Only called for messages at or above the configured priority
	 for `category`.

	 @param priority : "The severity of the message"
	 @param category : "The category the message was logged under"
	 @param tag : "The tag active at the log call site, see `@tag_scope`"
	 @param file : "The source file of the log call site, empty unless `FULL_LOG` is enabled"
	 @param function : "The function of the log call site, empty unless `FULL_LOG` is enabled"
	 @param line : "The line of the log call site, 0 unless `FULL_LOG` is enabled"
	 @param fmt : "The (unformatted) format string passed to the log call"
	 @param args : "The arguments referenced by `fmt`"
	*>
	fn void log(LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args);
}

<*
 Log a message at VERBOSE/DEBUG/INFO/WARN/ERROR/CRITICAL priority (one macro per level), using
 `default_category` unless `category` is given explicitly.

 @param fmt : "The format string"
 @param category : "The category to log under, defaults to the current `default_category`"
*>
macro void verbose(String fmt, ..., LogCategory category = default_category) @format(0) => call_log(VERBOSE, category, fmt, ...$vaarg);
macro void debug(String fmt, ..., LogCategory category = default_category) @format(0) => call_log(DEBUG, category, fmt, ...$vaarg);
macro void info(String fmt, ..., LogCategory category = default_category) @format(0) => call_log(INFO, category, fmt, ...$vaarg);
macro void warn(String fmt, ..., LogCategory category = default_category) @format(0) => call_log(WARN, category, fmt, ...$vaarg);
macro void error(String fmt, ..., LogCategory category = default_category) @format(0) => call_log(ERROR, category, fmt, ...$vaarg);
macro void critical(String fmt, ..., LogCategory category = default_category) @format(0) => call_log(CRITICAL, category, fmt, ...$vaarg);

<*
 Run `@body` with `default_category` temporarily set to `new_category`, restored on exit.

 @param new_category : "The category to use as default for the duration of the body"
*>
macro void @category_scope(LogCategory new_category; @body)
{
	LogCategory old = default_category;
	default_category = new_category;
	defer default_category = old;
	@body();
}

<*
 Run `@body` with `current_tag` temporarily set to a tag derived from `tag_prefix`, restored
 on exit.

 @param tag_prefix : "The (at most 3 byte) prefix for the temporary tag, see `create_tag`"
 @require [tag_prefix] tag_prefix.len <= 3 : "The prefix may not exceed 3 bytes"
*>
macro void @tag_scope(String tag_prefix = ""; @body)
{
	LogTag old = current_tag;
	push_tag(tag_prefix);
	defer current_tag = old;
	@body();
}

<*
 Set `current_tag` to a tag derived from `tag_prefix`. Unlike `@tag_scope`, this change is
 not automatically reverted.

 @param tag_prefix : "The (at most 3 byte) prefix for the new tag, see `create_tag`"
 @require [tag_prefix] tag_prefix.len <= 3 : "The prefix may not exceed 3 bytes"
*>
macro void push_tag(String tag_prefix = "")
{
	current_tag = create_tag(tag_prefix);
}

<*
 Create a tag consisting of `tag_prefix` followed by an underscore, padded with random
 lowercase letters to fill the tag; if `tag_prefix` is empty, the whole tag is random.

 @param tag_prefix : "The (at most 3 byte) prefix to start the tag with"
 @return "The newly created tag"
 @require [tag_prefix] tag_prefix.len <= 3 : "The prefix may not exceed 3 bytes"
*>
fn LogTag create_tag(String tag_prefix)
{
	LogTag tag @noinit;
	tag[:tag_prefix.len] = tag_prefix[..];
	int start = tag_prefix.len;
	if (start > 0) tag[start++] = '_';
	for (int i = start; i < tag.len; i++)
	{
		tag[i] = (char)rand_in_range('a', 'z');
	}
	return tag;
}

<*
 Set the minimum priority a message must have to be logged for `category`.

 @param category : "The category to configure"
 @param new_priority : "The new minimum priority"
*>
fn void set_priority_for_category(LogCategory category, LogPriority new_priority)
{
	@atomic_store(config_priorities[category], new_priority, UNORDERED);
}

<*
 Get the minimum priority a message must have to be logged for `category`.

 @param category : "The category to query"
 @return "The category's current minimum priority"
*>
fn LogPriority get_priority_for_category(LogCategory category)
{
	return @atomic_load(config_priorities[category], UNORDERED);
}

<*
 Registers a dynamic category, not thread safe.

 @param category_name : "The name of the new category"
 @param priority : "The initial minimum priority for the new category"
 @return "The newly allocated category id"
 @return? NO_MORE_DYNAMIC_CATEGORIES : "if no more category slots are available"

 @require [category_name] category_name != "" : "The name cannot be empty"
*>
fn LogCategory? register_dynamic_category(String category_name, LogPriority priority)
{
	static LogCategory current = CATEGORY_DYNAMIC_START;
	@in_logger_lock()
	{
		if (current == CATEGORY_CUSTOM_START) return NO_MORE_DYNAMIC_CATEGORIES~;
		config_priorities[current] = priority;
		category_names[current] = category_name;
        return current++;
	};
}

<*
 Set the minimum priority a message must have to be logged, for every category at once.

 @param new_priority : "The new minimum priority for all categories"
*>
fn void set_priority_all(LogPriority new_priority)
{
	for (int i = 0; i < config_priorities.len; i++)
	{
		@atomic_store(config_priorities[i], new_priority, UNORDERED);
	}
}

<*
 Set the active logger. All subsequent log calls (that pass the category's priority filter)
 are dispatched to `logger`.

 @param logger : "The new active logger"
*>
fn void set_logger(Logger logger)
{
	@in_logger_lock()
	{
		current_logger = logger;
		current_logfn = &logger.log;
	};
}

<*
 Get the active logger.

 @return "The currently active logger"
*>
fn Logger get_logger(Logger logger)
{
	return current_logger;
}

<*
 Lazily initialize the logging subsystem's mutex. Safe to call repeatedly and concurrently.
*>
macro void init()
{
	log_init.call(fn () => (void)logger_mutex.init());
}

<*
 Entry point used by `verbose`/`debug`/`info`/.../`critical` to forward a log call, capturing
 the call site location when `FULL_LOG` is enabled.

 @param prio : "The severity of the message"
 @param category : "The category to log under"
 @param fmt : "The format string"
 @param args : "The arguments referenced by `fmt`"
*>
macro void call_log(LogPriority prio, LogCategory category, String fmt, args...)
{
	$if FULL_LOG:
	call_log_internal(prio, category, $$FILE, $$FUNC, $$LINE, fmt, args);
	$else
	call_log_internal(prio, category, "", "", 0, fmt, args);
	$endif
}

<*
 Filter a log call against `category`'s configured priority and, if it passes, dispatch it to
 the active logger.

 @param prio : "The severity of the message"
 @param category : "The category to log under"
 @param file : "The source file of the log call site"
 @param func : "The function of the log call site"
 @param line : "The line of the log call site"
 @param fmt : "The format string"
 @param args : "The arguments referenced by `fmt`"
*>
fn void call_log_internal(LogPriority prio, LogCategory category, String file, String func, int line, String fmt, any[] args)
{
	LogPriority priority = mem::@atomic_load(config_priorities[category], UNORDERED);
	if (priority > prio) return;
	@in_logger_lock()
	{
		Logger logger = current_logger;
		LogFn logfn = current_logfn;
		logfn(logger.ptr, prio, category, current_tag, file, func, line, fmt, args);
	};
}

<*
 Get the name of a category, as set by `register_dynamic_category` or `set_category_name`.

 @param category : "The category to query"
 @return "The category's name"
 @return? NOT_FOUND : "if the category has no name set"
*>
fn String? get_category_name(LogCategory category)
{
	String val = category_names[category];
	return val ?: NOT_FOUND~;
}

<*
 Set the name of a category.

 @param category : "The category to name"
 @param name : "The new name"
 @require [name] name != "" : "The name must be non-empty"
*>
fn void set_category_name(LogCategory category, String name)
{
	category_names[category] = name;
}

<* A `Logger` that discards every message. Used when no other logger is available. *>
struct NullLogger (Logger)
{
	void* dummy;
}

fn void NullLogger.log(&self, LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args) @dynamic
{}

<* A `Logger` that forwards every message to each of `loggers` in turn. *>
struct MultiLogger (Logger)
{
	Logger[] loggers;
}

fn void MultiLogger.log(&self, LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args) @dynamic
{
	foreach (logger : self.loggers)
	{
		logger.log(priority, category, tag, file, function, line, fmt, args);
	}
}

module std::core::log @private;
import std::io, std::thread, std::time;

<*
 Run `@body` while holding the logging subsystem's mutex, initializing it first if necessary.
*>
macro @in_logger_lock(;@body)
{
	init();
	bool locked = logger_mutex.is_initialized();
	if (locked) logger_mutex.lock();
    defer if (locked) logger_mutex.unlock();
	@body();
}

<* A `Logger` that prints to stderr; the default logger when libc is available. *>
struct StderrLogger (Logger) @feat(LIBC)
{
	void* dummy;
}

fn void StderrLogger.log(&self, LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args) @dynamic @feat(LIBC)
{
	@stack_mem(256 + 64; Allocator mem)
	{
		DString str;
		str.init(mem, 256);
		str.appendf(fmt, ...args);
		TzDateTime time = datetime::now().to_local();
		$if FULL_LOG:
		io::eprintfn("[%02d:%02d:%02d:%04d] %s:%d [%s] %s", time.hour, time.min, time.sec, (time.usec / 1000), file, line, priority, str);
		$else
		io::eprintfn("[%02d:%02d:%02d:%04d] [%s] %s", time.hour, time.min, time.sec, (time.usec / 1000), priority, str);
		$endif
	};
}

<* The raw function-pointer form of `Logger.log`, bound to a specific logger's `self` pointer. *>
alias LogFn = fn void(void*, LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args);

<* The active logger's `log` method, bound to `current_logger`; kept in sync by `set_logger`. *>
LogFn current_logfn = env::LIBC ??? (LogFn)&StderrLogger.log : (LogFn)&NullLogger.log;

<* Guards one-time initialization of `logger_mutex`. *>
OnceFlag log_init;

<* Guards `current_logger`/`current_logfn` and dynamic category registration. *>
Mutex logger_mutex;

<* The currently active logger; change it via `set_logger`. *>
Logger current_logger = env::LIBC ??? &stderr_logger : &null_logger;

StderrLogger stderr_logger @feat(LIBC);

NullLogger null_logger;

<* Per-category minimum priority; indexed by `LogCategory`. *>
LogPriority[256] config_priorities = { [0..255] = ERROR, [CATEGORY_APPLICATION] = INFO, [CATEGORY_TEST] = VERBOSE, [CATEGORY_ASSERT] = WARN};

<* Per-category name; indexed by `LogCategory`. *>
String[256] category_names = {
	[CATEGORY_APPLICATION] = "APP",
    [CATEGORY_SYSTEM] = "SYSTEM",
    [CATEGORY_KERNEL] = "KERNEL",
    [CATEGORY_AUDIO] = "AUDIO",
    [CATEGORY_VIDEO] = "VIDEO",
    [CATEGORY_RENDER] = "RENDER",
    [CATEGORY_INPUT] = "INPUT",
    [CATEGORY_NETWORK] = "NETWORK",
    [CATEGORY_SOCKET] = "SOCKET",
    [CATEGORY_SECURITY] = "SECURITY",
    [CATEGORY_TEST] = "TEST",
    [CATEGORY_ERROR] = "ERROR",
    [CATEGORY_ASSERT] = "ASSERT",
    [CATEGORY_CRASH] = "CRASH",
    [CATEGORY_STATS] = "STATS"
};
