module logging_test @test;
import std::io, std::core::log;

struct RecordingLogger (Logger)
{
	LogPriority last_priority;
	LogCategory last_category;
	LogTag last_tag;
	String last_message;
	int call_count;
}

fn void RecordingLogger.log(&self, LogPriority priority, LogCategory category, LogTag tag, String file, String function, int line, String fmt, any[] args) @dynamic
{
	self.last_priority = priority;
	self.last_category = category;
	self.last_tag = tag;
	self.last_message = fmt;
	self.call_count++;
}

RecordingLogger global_recording_logger;

fn void test_register_dynamic_category_unique_ids()
{
	LogCategory a = log::register_dynamic_category("cat_a", LogPriority.VERBOSE)!!;
	LogCategory b = log::register_dynamic_category("cat_b", LogPriority.VERBOSE)!!;
	test::@check(a != b, "Expected distinct category ids");
	test::@check((int)b == (int)a + 1, "Expected monotonically increasing ids");
}

fn void test_priority_round_trip()
{
	LogCategory cat = log::register_dynamic_category("prio_test", LogPriority.VERBOSE)!!;
	test::eq(log::get_priority_for_category(cat), LogPriority.VERBOSE);

	log::set_priority_for_category(cat, LogPriority.WARN);
	test::eq(log::get_priority_for_category(cat), LogPriority.WARN);

	log::set_priority_for_category(cat, LogPriority.CRITICAL);
	test::eq(log::get_priority_for_category(cat), LogPriority.CRITICAL);
}

fn void test_category_name_round_trip()
{
	LogCategory cat = log::register_dynamic_category("named_category", LogPriority.INFO)!!;
	test::eq(log::get_category_name(cat)!!, "named_category");

	log::set_category_name(cat, "renamed");
	test::eq(log::get_category_name(cat)!!, "renamed");
}

fn void test_get_category_name_not_found()
{
	// A slot in the "custom application" range (>= CATEGORY_CUSTOM_START) that nothing in
	// this test file ever names.
	test::@error(log::get_category_name((LogCategory)250), NOT_FOUND);
}

fn void test_create_tag_with_prefix()
{
	LogTag tag = log::create_tag("ab");
	test::eq(tag[0], 'a');
	test::eq(tag[1], 'b');
	test::eq(tag[2], '_');
	// Remaining bytes are random lowercase fill.
	for (int i = 3; i < tag.len; i++)
	{
		test::@check(tag[i] >= 'a' && tag[i] <= 'z', "Expected a random lowercase fill byte");
	}
}

fn void test_create_tag_without_prefix()
{
	LogTag tag = log::create_tag("");
	foreach (c : tag)
	{
		test::@check(c >= 'a' && c <= 'z', "Expected a fully random lowercase tag");
	}
}

fn void test_tag_scope_restores()
{
	LogTag before = log::current_tag;
	log::@tag_scope("xy")
	{
		test::eq(log::current_tag[0], 'x');
		test::eq(log::current_tag[1], 'y');
		test::eq(log::current_tag[2], '_');
	};
	test::eq(log::current_tag, before);
}

fn void test_category_scope_restores()
{
	LogCategory cat = log::register_dynamic_category("scope_test", LogPriority.VERBOSE)!!;
	LogCategory before = log::default_category;
	log::@category_scope(cat)
	{
		test::eq(log::default_category, cat);
	};
	test::eq(log::default_category, before);
}

fn void test_multi_logger_forwards_to_all()
{
	RecordingLogger a;
	RecordingLogger b;
	Logger[2] loggers = { &a, &b };
	MultiLogger multi = { .loggers = loggers[..] };

	LogTag tag;
	multi.log(LogPriority.WARN, log::CATEGORY_APPLICATION, tag, "file", "func", 1, "hello", {});

	test::eq(a.call_count, 1);
	test::eq(b.call_count, 1);
	test::eq(a.last_priority, LogPriority.WARN);
	test::eq(b.last_priority, LogPriority.WARN);
	test::eq(a.last_message, "hello");
	test::eq(b.last_message, "hello");
}

fn void test_null_logger_does_not_crash()
{
	NullLogger n;
	LogTag tag;
	n.log(LogPriority.CRITICAL, log::CATEGORY_APPLICATION, tag, "file", "func", 1, "hello", {});
}

fn void test_logger_dispatch_and_filtering()
{
	log::set_logger(&global_recording_logger);

	LogCategory cat = log::register_dynamic_category("dispatch_test", LogPriority.VERBOSE)!!;
	log::set_priority_for_category(cat, LogPriority.WARN);

	global_recording_logger.call_count = 0;
	log::info("filtered out", category: cat);
	test::eq(global_recording_logger.call_count, 0);

	log::error("gets through", category: cat);
	test::eq(global_recording_logger.call_count, 1);
	test::eq(global_recording_logger.last_priority, LogPriority.ERROR);
	test::eq(global_recording_logger.last_category, cat);
	test::eq(global_recording_logger.last_message, "gets through");

	// The threshold itself is inclusive.
	log::warn("exactly at threshold", category: cat);
	test::eq(global_recording_logger.call_count, 2);
	test::eq(global_recording_logger.last_priority, LogPriority.WARN);
}
