module test_suite_runner;
import std::io, std::math, std::os, std::collections;
import libc;
import std::thread, std::atomic;

alias Pool = FixedThreadPool;

struct TestRunnerContext
{
	Path compiler_path;
	Atomic{int} test_count;
	Atomic{int} skip_count;
	Atomic{int} success_count;
	Atomic{int} failure_count;
	Path start_cwd;
	bool no_terminal;
	bool only_skipped;
	String stdlib;
	Pool pool;
	Mutex print_mutex;
	Atomic{int} thread_id_counter;
	usz total_test_files;
}

struct TestOutput
{
	DString buffer;
	String filename;
	usz index;
	bool has_output;
}


TestRunnerContext context;

fn int main(String[] args)
{
	libc::signal(libc::SIGINT, fn void (CInt _signal) {
		foreach (f : path::ls(mem, context.start_cwd)!!)
		{
			if (f.str_view().starts_with("_c3test_")) (void)path::rmtree(context.start_cwd.tappend(f.str_view())!!);
		}
		os::exit(1);
	});

	String appname = args[0];
	if (args.len < 3) usage(appname);

	context.no_terminal = !io::stdout().isatty();
	context.start_cwd = path::tcwd()!!;
	context.print_mutex.init()!!;

	Path? path = context.start_cwd.tappend(args[1]);
	if (catch path) arg_error_exit(appname, "Invalid compiler path: %s", args[1]);
	if (!path::is_file(path))
	{
		error_exit("Error: Invalid path to compiler: %s (%s relative to %s)", path.path_string, args[1], context.start_cwd);
	}
	// Ok we're done.
	context.compiler_path = path.absolute(tmem)!!;

	context.only_skipped = false;

	usz num_threads = os::num_cpu();

	for (int i = 3; i < args.len; i++)
	{
		String arg = args[i];
		switch (arg)
		{
			case "--no-terminal":
				context.no_terminal = true;
			case "-s":
			case "--skipped":
				context.only_skipped = true;
			case "--stdlib":
				if (i == args.len - 1 || args[i + 1].starts_with("-"))
				{
					arg_error_exit(appname, "Expected --stdlib to be followed by the path to the alternative standard library.");
				}
				context.stdlib = args[i + 1];
				i++;
				if (!os::native_is_dir(context.stdlib))
				{
					error_exit(appname, "Stdlib directory '%s' cannot be found.", context.stdlib);
				}
				context.stdlib = context.start_cwd.tappend(context.stdlib).absolute(mem)!!.str_view();
			case "--threads":
				if (i == args.len - 1 || args[i + 1].starts_with("-")) {
					arg_error_exit(appname, "Expected --threads to be followed by the number of threads.");
				}
				num_threads = args[i + 1].to_int()!!;
				i++;
			default:
				arg_error_exit(appname, "Unknown option '%s'.", args[i]);
		}
	}
	if (context.only_skipped && args[3] != "-s" && args[3] != "--skipped") usage(appname);

	Path? file = args[2].to_tpath().absolute(tmem);
	if (catch file) arg_error_exit(appname, "Invalid path: '%s'.", args[2]);
	
	PathList files_to_test;
	files_to_test.init(tmem);
	//defer files_to_test.free();

	switch
	{
		case path::is_file(file):
			files_to_test.push(file);
		case path::is_dir(file):
			collect_files(file, &files_to_test)!!;
		default:
			error_exit("Error: Path wasn't to directory or file: %s", file);
	}

	context.total_test_files = files_to_test.len();

	TestOutput[] test_outputs = mem::new_array(TestOutput, context.total_test_files);
	//Initialize all the DStrings
	for (usz i = 0; i < context.total_test_files; i++)
	{
		test_outputs[i].index = i;
		test_outputs[i].buffer.init(mem);
	}
	defer
	{
		foreach (o : test_outputs)
		{
			o.buffer.free();
			o.filename.free(mem);
		}
		mem::free(test_outputs);
	}

	context.pool.init(num_threads, context.total_test_files)!!;
	defer context.pool.stop_and_destroy();
	for (usz i = 0; i < context.total_test_files; i++)
	{
		context.pool.push(&test_file_threaded, files_to_test[i], &test_outputs[i], i)!!;
	}
	context.pool.join();

	int total = context.test_count.load();
	int success = context.success_count.load();
	int skipped = context.skip_count.load();
	int failed = context.failure_count.load();
	int to_run = total - skipped;
	bool all_ok = (success == to_run) && (failed == 0);

	if (!context.no_terminal) io::print(all_ok ? (String)Ansi.GREEN : (String)Ansi.RED);
	io::printfn("\nFound %d tests: %.1f%% (%d / %d) passed (%d skipped, %d failed).",
		total, (100.0 * success) / math::max(1, to_run), success, to_run, skipped, failed);
	if (!context.no_terminal) io::print(Ansi.RESET);

	return all_ok ? 0 : 1;
}


struct Error
{
	int line;
	String text;
	bool found;
}

<*
 Information about a given file that we're running
*>
struct RunFile
{
	String name;
	bool is_output;
	bool closed;
	union
	{
		struct
		{
			File file;
			List {Error} warnings;
			List {Error} errors;
			int line_offset;
		}
		List {String} expected_lines;
	}
}

fn void RunFile.add_line(&self, String line)
{
	if (self.is_output)
	{
		line = line.trim();
		if (line == "") return;
		self.expected_lines.push(line);
	}
	else
	{
		io::fprintn(&self.file, line)!!;
	}
}

fn void RunFile.dealloc(&self)
{
	self.close();
	if (self.is_output)
	{
		self.expected_lines.free();
	}
	else
	{
		self.warnings.free();
		self.errors.free();
	}
}

fn Ref{RunFile}? create_run_file(Path test_dir, String filename, bool is_output)
{
	if (is_output)
	{
		return ref::new{RunFile}({ .name = filename, .is_output = true });
	}
	File file = file::open_path(test_dir.tappend(filename), "wb")!;
	return ref::new{RunFile}({ .name = filename, .file = file, .is_output = false });
}

fn void RunFile.close(&self)
{
	if (!self.is_output && !self.closed) (void)self.file.close();
	self.closed = true;
}

struct RunSettings
{
	bool safe;
	bool debuginfo;
	bool no_deprecation;
	List{String} opts;
	String arch;
	List{Ref{RunFile}} input_files;
	List{Ref{RunFile}} output_files;
	RunFile* current_file;
}
<*
 Check a line from stderr returned by the compiler
*>
fn bool find_and_mark_error(RunSettings* settings, String type, String file, String line_str, String _col, String message) => @pool()
{
	int line_no = line_str.to_int()!!;

	String basename = file.file_tbasename()!!;

	foreach (f : settings.input_files)
	{
		if (f.ptr.name != basename) continue;
		List {Error}* list;
		switch (type)
		{
			case "Error":
				list = &f.ptr.errors;
			case "Warning":
				list = &f.ptr.warnings;
			default:
				io::printfn("FAILED - Unknown error type '%s'", type);
				return false;
		}
		foreach (&err : *list)
		{
			if (err.found || line_no != err.line || !message.contains(err.text)) continue;
			err.found = true;
			return true;
		}
	}
	return false;
}

<*
 Parse all stderr output
*>
fn bool parse_result(DString out, RunSettings settings, List{String}* cmdline, OutStream buffer_stream)
{
	List{String} unexpected_errors;

	foreach (line : out.str_view().tsplit("\n"))
	{
		if (!line) continue;
		String[] parts = line.tsplit("|", 5, skip_empty: true);

		if (parts.len != 5)
		{
			io::fprintn(buffer_stream, "\nFAILED - Unexpected response from compiler:")!!;
			io::fprint(buffer_stream, "Compiler command line: ")!!;
			foreach(arg : *cmdline) io::fprintf(buffer_stream, "%s ", arg)!!;
			io::fprintn(buffer_stream)!!;
			io::fprintn(buffer_stream, "Output: ----------------------------------------------------------")!!;
			io::fprintn(buffer_stream, out)!!;
			io::fprintn(buffer_stream, "------------------------------------------------------------------")!!;
			return false;
		}

		if (!find_and_mark_error(&settings, ...parts[:5]))
		{
			unexpected_errors.push(string::tformat("  - %s at %s:%s: \"%s\"", parts[0], parts[1].file_tbasename()!!, parts[2], parts[4]));
		}
	}

	List{String} missed_errors;
	List{String} missed_warnings;

	foreach (file : settings.input_files)
	{
		foreach (&err : file.ptr.errors) if (!err.found) missed_errors.push(string::tformat("  - %s:%d expected: \"%s\"", file.ptr.name, err.line, err.text));
		foreach (&warn : file.ptr.warnings) if (!warn.found) missed_warnings.push(string::tformat("  - %s:%d expected: \"%s\"", file.ptr.name, warn.line, warn.text));
	}

	bool is_ok = unexpected_errors.is_empty() && missed_errors.is_empty() && missed_warnings.is_empty();
	if (is_ok) return true;

	io::fprintn(buffer_stream, "FAILED")!!;

	if (!unexpected_errors.is_empty())
	{
		io::fprintn(buffer_stream, "\n Unexpected compilation errors:")!!;
		io::fprintn(buffer_stream, " ------------------------------")!!;
		foreach(e : unexpected_errors) io::fprintn(buffer_stream, e)!!;
	}

	if (!missed_errors.is_empty())
	{
		io::fprintn(buffer_stream, "\n Errors that never occurred:")!!;
		io::fprintn(buffer_stream, " ---------------------------")!!;
		foreach(e : missed_errors) io::fprintn(buffer_stream, e)!!;
	}

	if (!missed_warnings.is_empty())
	{
		io::fprintn(buffer_stream, "\n Warnings that never occurred:")!!;
		io::fprintn(buffer_stream, " -----------------------------")!!;
		foreach(w : missed_warnings) io::fprintn(buffer_stream, w)!!;
	}

	io::fprintn(buffer_stream)!!;
	return false;
}

<*
 Parse trailing directives #warning and #error
*>
fn void parse_trailing_directive(int line_number, String line, RunSettings* settings, bool is_single)
{
	if (settings.current_file.is_output) return;
	usz index = line.rindex_of("// #")!! + 4;
	line = line[index..].trim();
	switch
	{
		case line.starts_with("warning:"):
			line = line[8..].trim();
			settings.current_file.warnings.push({ line_number, line, false });
		case line.starts_with("error:"):
			line = line[6..].trim();
			settings.current_file.errors.push({ line_number, line, false });
		default:
			error_exit("FAILED - Unknown trailing directive '%s'", line);
	}
}

<*
 Parse header directives, #error, #safe, #debuginfo, #opt, #target, #deprecation, #file and #expect
*>
fn void parse_header_directive(int* line_no, String line, RunSettings* settings, bool is_single, Path test_dir)
{
	line = line[4..].trim();
	switch
	{
		case line.starts_with("error:"):
			line = line[6..].trim();
			if (settings.current_file.is_output) return;
			settings.current_file.errors.push({ *line_no, line, false });
		case line.starts_with("safe:"):
			if (settings.current_file.is_output) return;
			settings.safe = line[5..].trim() == "yes";
		case line.starts_with("debuginfo:"):
			if (settings.current_file.is_output) return;
			settings.debuginfo = line[10..].trim() == "yes";
		case line.starts_with("opt:"):
			if (settings.current_file.is_output) return;
			settings.opts.push(line[4..].trim());
		case line.starts_with("target:"):
			if (settings.current_file.is_output) return;
			settings.arch = line[7..].trim();
		case line.starts_with("deprecation:"):
			if (settings.current_file.is_output) return;
			settings.no_deprecation = line[12..].trim() == "no";
		case line.starts_with("file:"):
			if (is_single) error_exit("FAILED - 'file' directive only allowed with .c3t");
			settings.current_file.close();
			line = line[5..].trim();
			Ref{RunFile} file = create_run_file(test_dir, line, false)!!;
			settings.current_file = file.ptr;
			*line_no = 1;
			settings.input_files.push(file);
		case line.starts_with("expect:"):
			if (is_single) error_exit("FAILED - 'expect' directive only allowed with .c3t");
			line = line[7..].trim();
			if (settings.current_file) settings.current_file.close();
			Ref{RunFile} file = create_run_file(test_dir, line, true)!!;
			settings.current_file = file.ptr;
			settings.output_files.push(file);
		default:
			io::printfn("FAILED - Unknown header directive '%s'", line);
			context.failure_count.add(1);
			return;
	}
}

fn void test_file_threaded(any[] data)
{
	@pool_init(mem, 1024*1024) {
		Path file_path = *anycast(data[0], Path)!!;
		TestOutput* output = *anycast(data[1], TestOutput*)!!;
		usz index = *anycast(data[2], usz)!!;
		if (!test_file(file_path, output, index))
		{
			context.failure_count.add(1);
			if (!context.no_terminal)
			{
				Mutex.@in_lock(&context.print_mutex)
				{
					io::printn();
					io::printn("------------------------------------------------------------------");
					io::printf("- %d/%d %s: %s", index, context.failure_count.load(), file_path, output.buffer)!!;
					io::printn("------------------------------------------------------------------");
				};
			}
		}
		if (context.no_terminal)
		{
			Mutex.@in_lock(&context.print_mutex)
			{
				io::printf("- %d/%d %s: ", context.test_count.load(), context.failure_count.load(), file_path)!!;
				io::printn(output.buffer.str_view());
			};
		}
	};
}

<*
 Test a file
*>
fn bool test_file(Path file_path, TestOutput* output, usz index)
{
	context.test_count.add(1);
	update_status(index, file_path, output);
	bool single;
	int thread_id = context.thread_id_counter.add(1);
	Path test_dir = context.start_cwd.tappend(string::tformat("_c3test_%s", thread_id))!!;
	(void)path::rmtree(test_dir);
	defer (void)path::rmtree(test_dir);
	if (@catch(path::mkdir(test_dir)))
	{
		io::fprintfn((OutStream)&output.buffer, "FAILED - Failed to create temp test directory '%s'.", test_dir)!!;
		os::exit(1);
	}
	switch (file_path.extension() ?? "")
	{
		case "c3":
			single = true;
		case "c3t":
			single = false;
		default:
			io::fprintfn((OutStream)&output.buffer, "FAILED - Unexpected file name '%s', expected a file with a '.c3' or '.c3t' suffix.", file_path.str_view())!!;
			return false;
	}
	File? f = file::open_path(file_path, "rb");
	if (catch f)
	{
		io::fprintfn((OutStream)&output.buffer, "FAILED - Failed to open '%s'.", file_path)!!;
		return false;
	}
	defer (void)f.close();

	if (context.only_skipped)
	{
		bool has_skip = false;
		while (try line = io::treadline(&f))
		{
			if (line.contains("#skip")) { has_skip = true; break; }
		}
		if (!has_skip)
		{
			context.success_count.add(1);
			return true;
		}
		(void)f.seek(0);
	}

	RunSettings settings;
	Ref{RunFile} rf = create_run_file(test_dir, file_path.basename()[..^(single ? 4 : 5)].tconcat(".c3"), false)!!;
	settings.current_file = rf.ptr;
	settings.input_files.push(rf);
	int line_no = 1;
	while (try line = io::treadline(&f))
	{
		if (line.starts_with("// #") || line.starts_with("/* #"))
		{
			if (line.contains("#skip"))
			{
				context.skip_count.add(1);
				return true;
			}
			parse_header_directive(&line_no, line, &settings, single, test_dir);
			continue;
		}
		else if (line.contains("// #"))
		{
			parse_trailing_directive(line_no, line, &settings, single);
		}
		settings.current_file.add_line(line);
		line_no++;
	}
	settings.current_file.close();
	defer
	{
		foreach (&r : settings.input_files) r.release();
		foreach (&r : settings.output_files) r.release();
	}

	// Construct the compile line
	List{String} cmdline;
	cmdline.push(context.compiler_path.str_view());
	$if env::OPENBSD:
		cmdline.push("--max-mem");
		cmdline.push("128");
	$endif
	cmdline.push("compile-only");
	cmdline.push("--test");
	if (context.stdlib)
	{
		cmdline.push("--stdlib");
		cmdline.push(context.stdlib);
	}
	cmdline.push("--llvm-out");
	cmdline.push(test_dir.str_view());
	cmdline.push("--build-dir");
	cmdline.push(test_dir.tappend("build").str_view()!!);
	foreach (file : settings.input_files)
	{
		cmdline.push(test_dir.tappend(file.ptr.name).str_view()!!);
	}
	if (!single) cmdline.push("--emit-llvm");
	cmdline.push(settings.debuginfo ? "-g" : "-g0");
	if (settings.arch)
	{
		cmdline.push("--target");
		cmdline.push(settings.arch);
	}
	cmdline.push("-O0");
	if (settings.no_deprecation) cmdline.push("--silence-deprecation");
	cmdline.push(settings.safe ? "--safe=yes" : "--safe=no");
	foreach (opt : settings.opts)
	{
		cmdline.push(opt);
	}
	SubProcess compilation = process::create(cmdline.array_view(), { .search_user_path, .no_window, .inherit_environment })!!;
	defer compilation.destroy();
	CInt result = compilation.join() ?? 1;
	DString out;
	io::copy_to(&&compilation.stderr(), &out)!!;
	if (result != 0 && result != 1)
	{
		(void)io::copy_to(&&compilation.stdout(), (OutStream)&out);
		io::fprintfn((OutStream)&output.buffer, "FAILED - Error(%s): %s", result, out)!!;
		return false;
	}
	if (!parse_result(out, settings, &cmdline, (OutStream)&output.buffer)) return false;
	foreach (file : settings.output_files)
	{
		Path expected_file = test_dir.tappend(file.ptr.name)!!;
		if (!file::exists(expected_file.str_view()))
		{
			io::fprintfn((OutStream)&output.buffer, "FAILED - Did not compile file %s.", file.ptr.name)!!;
			return false;
		}
		File file_ll = file::open(expected_file.str_view(), "rb")!!;
		defer (void)file_ll.close();
		String? next = file.ptr.expected_lines.pop_first();
		while (try line = io::treadline(&file_ll) && try next_value = next)
		{
			line = line.trim();
			if (line == "") continue;
			if (line.contains(next_value))
			{
				next = file.ptr.expected_lines.pop_first();
			}
		}
		if (try next)
		{
			io::fprintfn((OutStream)&output.buffer, `FAILED - %s did not contain: "%s"`, file.ptr.name, next)!!;
			io::fprintfn((OutStream)&output.buffer, "\n\n\n---------------------------------------------------> %s\n\n", file.ptr.name)!!;
			(void)file_ll.seek(0);
			io::fprintn((OutStream)&output.buffer, (String)io::read_fully(tmem, &file_ll))!!;
			io::fprintfn((OutStream)&output.buffer, "<---------------------------------------------------- %s\n", file_path)!!;
			return false;
		}
	}
	context.success_count.add(1);
	update_status(index, {}, output);
	return true;
}

fn void update_status(usz index, Path filename, TestOutput* output)
{
	if (context.no_terminal)
	{
		output.has_output = true;
		if (filename.str_view().len == 0) io::fprint((OutStream)&output.buffer, "Passed.")!!;
		return;
	}

	Mutex.@in_lock(&context.print_mutex) {
		bool has_entry = filename.str_view().len != 0;
		const String[8] PARTS = { " ", "▏", "▎", "▍", "▌", "▋", "▊", "▉" };
		int progress_bar_width = 25;
		usz total = context.total_test_files;
		usz finished = context.test_count.load();
		if (!has_entry && total != finished) return;
		double fraction = total > 0.0 ? finished / (double)total : 0;
		int full_width = (int)math::floor(fraction * progress_bar_width);
		int remainder = (int)math::floor(8 * (fraction * progress_bar_width % 1.0));
		io::print("\e[0m\e[2K\e[1GTest progress: [");
		usz failed = context.failure_count.load();
		io::print(failed ? "\e[31m" : "\e[32m");
		for (int i = 0; i < full_width; i++) io::print("█");
		if (full_width != progress_bar_width) io::print(PARTS[remainder]);
		for (int i = full_width + 1; i < progress_bar_width; i++) io::print(" ");
		io::printf("\e[0m] %.1f%% complete (%d failed)", fraction * 100.0, failed);
		if (has_entry) io::printf(" - Testing: %s", filename.basename());
		(void)io::stdout().flush();
	};
}

fn void? collect_files(Path file_path, List{Path}* files)
{
	PathList entries = path::ls(tmem, file_path)!!;

	foreach (f : entries)
	{
		Path current_file = file_path.append(tmem, f.str_view())!;
		switch
		{
			case path::is_dir(current_file):
				collect_files(current_file, files)!;
			case path::is_file(current_file):
				switch (current_file.extension() ?? "")
				{
					case "c3":
					case "c3t":
						files.push(current_file);
				}
			default:
				io::printfn("Skip %s", current_file);
				// Ignore
		}
	}
}

fn void usage(String appname) @noreturn
{
	io::printfn("Usage: %s <compiler path> <file/dir> [options]", appname);
	io::printn();
	io::printn("Options:");
	io::printn("  -s, --skipped       only run skipped tests");
	io::printn("  --no-terminal       disable color and dynamic status line");
	io::printn("  --stdlib <path>     override the path to stdlib");
	io::printfn("  --threads [N]       run tests in N threads (default: %s)", os::num_cpu());
	os::exit(0);
}

fn void arg_error_exit(String appname, String fmt, args...) @noreturn
{
	io::printfn(fmt, ...args);
	usage(appname);
}

fn void error_exit(String fmt, args...) @noreturn
{
	io::printfn(fmt, ...args);
	libc::exit(1);
}
