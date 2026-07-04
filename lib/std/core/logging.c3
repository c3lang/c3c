module std::core::log;
import std::io, std::thread, std::time, std::math::random;

const FULL_LOG = env::COMPILER_SAFE_MODE || $feature(FULL_LOG);

typedef LogCategory = inline char;
typedef LogTag = char[12];

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
const LogCategory CATEGORY_DYNAMIC_START = (LogCategory)15;
<* All categories from 100 and upwards are available for applications *>
const LogCategory CATEGORY_CUSTOM_START  = (LogCategory)100;

tlocal LogCategory default_category = CATEGORY_APPLICATION;
tlocal LogTag current_tag;

faultdef NO_MORE_DYNAMIC_CATEGORIES;

enum LogPriority : int
{
	VERBOSE,
	DEBUG,
	INFO,
	WARN,
	ERROR,
	CRITICAL,
}

interface Logger
{
	fn void log(LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args);
}

macro void verbose(String fmt, ..., LogCategory category = default_category) => call_log(VERBOSE, category, fmt, ...$vaarg);
macro void debug(String fmt, ..., LogCategory category = default_category) => call_log(DEBUG, category, fmt, ...$vaarg);
macro void info(String fmt, ..., LogCategory category = default_category) => call_log(INFO, category, fmt, ...$vaarg);
macro void warn(String fmt, ..., LogCategory category = default_category) => call_log(WARN, category, fmt, ...$vaarg);
macro void error(String fmt, ..., LogCategory category = default_category) => call_log(ERROR, category, fmt, ...$vaarg);
macro void critical(String fmt, ..., LogCategory category = default_category) => call_log(CRITICAL, category, fmt, ...$vaarg);

macro void @category_scope(LogCategory new_category; @body)
{
	LogCategory old = default_category;
	default_category = new_category;
	defer default_category = old;
	@body();
}

<*
 @require tag_prefix.len <= 3 : "The prefix may not exceed 3 bytes"
*>
macro void @tag_scope(String tag_prefix = ""; @body)
{
	LogTag old = current_tag;
	push_tag(tag_prefix);
	defer current_tag = old;
	@body();
}

<*
 @require tag_prefix.len <= 3 : "The prefix may not exceed 3 bytes"
*>
macro void push_tag(String tag_prefix = "")
{
	current_tag = create_tag(tag_prefix);
}

<*
 @require tag_prefix.len <= 3 : "The prefix may not exceed 3 bytes"
*>
fn LogTag create_tag(String tag_prefix)
{
	LogTag tag @noinit;
	int start = 0;
	foreach (int i, c : tag_prefix)
	{
		if (c == 0) break;
		tag[start++] = c;
	}
	if (start > 0) tag[start++] = '_';
	for (int i = start; i < tag.len; i++)
	{
		tag[i] = (char)rand_in_range('a', 'z');
	}
	return tag;
}

fn void set_priority_for_category(LogCategory category, LogPriority new_priority)
{
	@atomic_store(config_priorities[category], new_priority, UNORDERED);
}

fn LogPriority get_priority_for_category(LogCategory category)
{
	return @atomic_load(config_priorities[category], UNORDERED);
}

<*
 Registers a dynamic category, not thread safe.

 @require category_name != "" : "The name cannot be empty"
 @return? NO_MORE_DYNAMIC_CATEGORIES : "if no more category slots are available"
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

fn void set_priority_all(LogPriority new_priority)
{
	for (int i = 0; i < config_priorities.len; i++)
	{
		@atomic_store(config_priorities[i], new_priority, UNORDERED);
	}
}
fn void set_logger(Logger logger)
{
	@in_logger_lock()
	{
		current_logger = logger;
		current_logfn = &logger.log;
	};
}

macro void init()
{
	log_init.call(fn () => (void)logger_mutex.init());
}

macro void call_log(LogPriority prio, LogCategory category, String fmt, args...)
{
	$if FULL_LOG:
	call_log_internal(prio, category, $$FILE, $$FUNC, $$LINE, fmt, args);
	$else
	call_log_internal(prio, category, "", "", 0, fmt, args);
	$endif
}

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

fn String? get_category_name(LogCategory category)
{
	String val = category_names[category];
	return val ?: NOT_FOUND~;
}

<*
 @require name != "" : "The name must be non-empty"
*>
fn void set_category_name(LogCategory category, String name)
{
	category_names[category] = name;
}

struct NullLogger (Logger)
{
	void* dummy;
}

fn void NullLogger.log(&self, LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args) @dynamic
{}

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

macro @in_logger_lock(;@body)
{
	init();
	bool locked = logger_mutex.is_initialized();
	if (locked) logger_mutex.lock();
    defer if (locked) logger_mutex.unlock();
	@body();
}

struct StderrLogger (Logger) @if(env::LIBC)
{
	void* dummy;
}

fn void StderrLogger.log(&self, LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args) @dynamic @if(env::LIBC)
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

alias LogFn = fn void(void*, LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args);
LogFn current_logfn = env::LIBC ??? (LogFn)&StderrLogger.log : (LogFn)&NullLogger.log;
OnceFlag log_init;
Mutex logger_mutex;
Logger current_logger = env::LIBC ??? &stderr_logger : &null_logger;
StderrLogger stderr_logger @if (env::LIBC);
NullLogger null_logger;
LogPriority[256] config_priorities = { [0..255] = ERROR, [CATEGORY_APPLICATION] = INFO, [CATEGORY_TEST] = VERBOSE, [CATEGORY_ASSERT] = WARN};

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
