module std::os::android @if(env::ANDROID);


enum LogPriority : (CInt val)
{
	ANDROID_LOG_UNKNOWN = 0,
	ANDROID_LOG_DEFAULT = 1,
	ANDROID_LOG_VERBOSE = 2,
	ANDROID_LOG_DEBUG   = 3,
	ANDROID_LOG_INFO    = 4,
	ANDROID_LOG_WARN    = 5,
	ANDROID_LOG_ERROR   = 6,
	ANDROID_LOG_FATAL   = 7,
	ANDROID_LOG_SILENT  = 8
}

enum LogId : (CInt val)
{
	LOG_ID_MIN      = 0,
	LOG_ID_MAIN     = 0,
	LOG_ID_RADIO    = 1,
	LOG_ID_EVENTS   = 2,
	LOG_ID_SYSTEM   = 3,
	LOG_ID_CRASH    = 4,
	LOG_ID_STATS    = 5,
	LOG_ID_SECURITY = 6,
	LOG_ID_KERNEL   = 7,
	LOG_ID_MAX      = 7,
	LOG_ID_DEFAULT  = 0x7FFFFFFF
}

struct LogMessage @packed
{
	usz struct_size;
	CInt buffer_id;
	CInt priority;
	ZString tag;
	ZString file;
	CUInt line;
	ZString message;
}

extern fn CInt log_write(LogPriority prio, ZString tag, ZString text) @extern("__android_log_write");
extern fn CInt log_print(LogPriority prio, ZString tag, ZString fmt, ...) @extern("__android_log_print");
extern fn void log_assert(ZString cond, ZString tag, ZString fmt, ...) @extern("__android_log_assert");

fn bool log_id_is_valid(LogId id)
{
	return id >= LOG_ID_MIN && id < LOG_ID_MAX;
}

extern fn CInt log_buf_write(CInt bufID, CInt prio, ZString tag, ZString text) @extern("__android_log_buf_write");
extern fn CInt log_buf_print(CInt bufID, CInt prio, ZString tag, ZString fmt, ...) @extern("__android_log_buf_print");

alias LoggerFunction = fn void(LogMessage* log_message);
alias AborterFunction = fn void(ZString abort_message);

extern fn void log_write_log_message(LogMessage log_message) @extern("__android_log_write_log_message");
extern fn void log_set_logger(LoggerFunction logger) @extern("__android_log_set_logger");
extern fn void log_logd_logger(LogMessage log_message) @extern("__android_log_logd_logger");
extern fn void log_stderr_logger(LogMessage log_message) @extern("__android_log_stderr_logger");
extern fn void log_set_aborter(AborterFunction aborter) @extern("__android_log_set_aborter");
extern fn void log_call_aborter(ZString abort_message) @extern("__android_log_call_aborter");
extern fn void log_default_aborter(ZString abort_message) @extern("__android_log_default_aborter");
extern fn CInt log_is_loggable(CInt prio, ZString tag, CInt default_prio) @extern("__android_log_is_loggable");
extern fn CInt log_is_loggable_len(CInt prio, ZString tag, isz len, CInt default_prio) @extern("__android_log_is_loggable_len");
extern fn CInt log_set_minimum_priority(CInt priority) @extern("__android_log_set_minimum_priority");
extern fn CInt log_get_minimum_priority() @extern("__android_log_get_minimum_priority");
extern fn void log_set_default_tag(ZString tag) @extern("__android_log_set_default_tag");
