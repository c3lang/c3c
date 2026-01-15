module test_suite_runner;
import std::io, std::math, std::os, std::collections;
import libc;
import std::thread, std::atomic;

alias Pool = FixedThreadPool;

Atomic{bool} global_should_quit;

struct TestRunnerContext
{
	Path compiler_path;
	Atomic{int} test_count;
	Atomic{int} skip_count;
	Atomic{int} success_count;
	Atomic{int} failure_count;
	Path start_cwd;
	Path test_root;
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
	usz index;
	bool has_output;
}

TestRunnerContext context;

/* ----------------------------- Utilities ------------------------------ */
fn void usage(String appname) @noreturn
{
	io::printfn("Usage: %s <compiler path> <file/dir> [options]", appname);
	io::printn();
	io::printn("Options:");
	io::printn(" -s, --skipped       only run skipped tests");
	io::printn(" --no-terminal       disable color and dynamic status line");
	io::printn(" --stdlib <path>     override the path to stdlib");
	io::printfn(" --threads [N]       run tests in N threads (default: %s)", os::num_cpu());
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

/* ----------------------- Run / File descriptor types ------------------- */
struct Error
{
	int line;
	String text;
	bool found;
}

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

fn RunFile* create_run_file(Path test_dir, String filename, bool is_output)
{
	RunFile* rf = mem::tnew(RunFile);
	*rf = { .name = filename, .is_output = is_output };
	if (is_output)
	{
		rf.expected_lines.init(tmem);
	}
	else
	{
		rf.warnings.init(tmem);
		rf.errors.init(tmem);
		rf.file = file::open_path(test_dir.tappend(filename), "wb")!!;
	}
	return rf;
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
	List{RunFile*} input_files;
	List{RunFile*} output_files;
	RunFile* current_file;
}

/* --------------------- Error parsing helpers -------------------------- */
fn bool find_and_mark_error(RunSettings* settings, String type, String file,
	String line_str, String _col, String message) => @pool()
{
	int line_no = line_str.to_int()!!;
	String basename = file.file_tbasename()!!;
	foreach (f : settings.input_files)
	{
		if (f.name != basename) continue;
		List {Error}* list;
		switch (type)
		{
			case "Error":
				list = &f.errors;
			case "Warning":
				list = &f.warnings;
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

fn void print_errors(OutStream stream, String title, List{String} errors)
{
	if (errors.is_empty()) return;
	io::fprintfn(stream, "\n %s:\n ------------------------------", title)!!;
	foreach(e : errors) io::fprintn(stream, e)!!;
}

fn bool parse_result(DString out, RunSettings settings, List{String}* cmdline, OutStream buffer_stream, CInt result)
{
	List{String} unexpected_errors;
	Splitter lines = out.str_view().tokenize("\n");
	
	while (try line = lines.next())
	{
		if (!line) continue;

		Splitter s = line.tokenize("|");

		String? type_opt = s.next();
		String? file_opt = s.next();
		String? line_opt = s.next();
		String? col_opt = s.next();

		if (try type = type_opt && try file = file_opt && 
			try line_str = line_opt && try col = col_opt && 
			s.current < line.len)
		{
			String msg = line[s.current..];

			if (!find_and_mark_error(&settings, type, file, line_str, col, msg))
			{
				unexpected_errors.push(string::tformat(" - %s at %s:%s: %s",
					type, file.file_tbasename()!!, line_str, msg));
			}
		}
		else
		{
			io::fprintn(buffer_stream, "\nFAILED - Unexpected response from compiler:")!!;
			io::fprint(buffer_stream, "Compiler command line: ")!!;
			foreach(arg : *cmdline) io::fprintf(buffer_stream, "%s ", arg)!!;
			io::fprintn(buffer_stream)!!;
			io::fprintn(buffer_stream, "Output: ------------------------------------------------------->>>")!!;
			io::fprintn(buffer_stream, out)!!;
			io::fprintn(buffer_stream, "<<<---------------------------------------------------------------")!!;
			return false;
		}
	}

	List{String} missed_errors;
	List{String} missed_warnings;
	foreach (file : settings.input_files)
	{
		foreach (&err : file.errors) if (!err.found)
		{
			missed_errors.push(string::tformat(" - %s:%d expected: \"%s\"", file.name, err.line, err.text));
		}
		foreach (&warn : file.warnings) if (!warn.found)
		{
			missed_warnings.push(string::tformat(" - %s:%d expected: \"%s\"", file.name, warn.line, warn.text));
		}
	}

	bool is_ok = unexpected_errors.is_empty() && missed_errors.is_empty() && missed_warnings.is_empty();
	if (is_ok && result != 0)
	{
		bool has_expected_errors = false;
		foreach (file : settings.input_files)
		{
			if (!file.errors.is_empty())
			{
				has_expected_errors = true;
				break;
			}
		}
		if (!has_expected_errors)
		{
			unexpected_errors.push(string::tformat(" - Compiler exited with error code %d but no errors were expected.", result));
			is_ok = false;
		}
	}

	if (is_ok) return true;
	io::fprintn(buffer_stream, "FAILED")!!;
	print_errors(buffer_stream, "Unexpected compilation errors", unexpected_errors);
	print_errors(buffer_stream, "Errors that never occurred", missed_errors);
	print_errors(buffer_stream, "Warnings that never occurred", missed_warnings);
	io::fprintn(buffer_stream)!!;
	return false;
}

/* --------------------- Directive parsing ----------------------------- */
fn bool parse_trailing_directive(int line_number, String line, RunSettings* settings, bool is_single)
{
	if (settings.current_file.is_output) return true;
	usz index = line.rindex_of("// #")!! + 4;
	String clean_line = line[index..].trim();
	
	Splitter s = clean_line.tokenize(":");
	String? key = s.next();
	
	if (@catch(key) || s.current > clean_line.len) 
	{
		io::printfn("FAILED - Unknown trailing directive '%s'", clean_line);
		return false;
	}

	String value = clean_line[s.current..].trim();

	switch (key!!)
	{
		case "warning":
			settings.current_file.warnings.push({ line_number, value, false });
		case "error":
			settings.current_file.errors.push({ line_number, value, false });
		default:
			io::printfn("FAILED - Unknown trailing directive '%s'", clean_line);
			return false;
	}
	return true;
}

fn void parse_header_directive(int* line_no, String line, RunSettings* settings, bool is_single, Path test_dir)
{
	String clean_line = line[4..].trim();
	Splitter s = clean_line.tokenize(":");
	String? key_opt = s.next();
	
	if (catch err = key_opt) return;
	String key = key_opt;
	
	String value = (s.current <= clean_line.len) ? clean_line[s.current..].trim() : "";

	bool is_output = settings.current_file.is_output;
	
	switch (key)
	{
		case "error":
			if (!is_output) settings.current_file.errors.push({ *line_no, value, false });
		case "safe":
			if (!is_output) settings.safe = value == "yes";
		case "debuginfo":
			if (!is_output) settings.debuginfo = value == "yes";
		case "opt":
			if (!is_output) settings.opts.push(value);
		case "target":
			if (!is_output) settings.arch = value;
		case "deprecation":
			if (!is_output) settings.no_deprecation = value == "no";
		case "file":
			if (is_single) error_exit("FAILED - 'file' directive only allowed with .c3t");
			settings.current_file.close();
			RunFile* file = create_run_file(test_dir, value, false);
			settings.current_file = file;
			*line_no = 1;
			settings.input_files.push(file);
		case "expect":
			if (is_single) error_exit("FAILED - 'expect' directive only allowed with .c3t");
			if (settings.current_file) settings.current_file.close();
			RunFile* ofile = create_run_file(test_dir, value, true);
			settings.current_file = ofile;
			settings.output_files.push(ofile);
		default:
			io::printfn("FAILED - Unknown header directive '%s'", clean_line);
			context.failure_count.add(1);
	}
}

/* ----------------------- Status / Progress --------------------------- */
struct Winsize { ushort ws_row, ws_col, ws_xpixel, ws_ypixel; }
fn int get_terminal_width()
{
	$if env::LINUX || env::DARWIN || env::BSD_FAMILY:
		const ulong TIOCGWINSZ @if(env::LINUX) = 0x5413;
		const ulong TIOCGWINSZ @if(env::DARWIN || env::BSD_FAMILY) = 0x40087468;

		Winsize ws;
		if (libc::ioctl(1, TIOCGWINSZ, &ws) == 0) return (int)ws.ws_col;
	$endif

	return 80;
}

const String CLEAR_LINE = "\e[2K";
const String MOVETO_COLUMN_ONE = "\e[1G"; 

fn void update_status(usz index, Path filename, TestOutput* output)
{
	if (context.no_terminal)
	{
		output.has_output = true;
		if (filename.str_view().len == 0) io::fprint((OutStream)&output.buffer, "Passed.")!!;
		return;
	}

	Mutex.@in_lock(&context.print_mutex)
	{
		bool has_entry = filename.str_view().len != 0;
		const String[8] PARTS = { " ", "▏", "▎", "▍", "▌", "▋", "▊", "▉" };
		int progress_bar_width = 25;
		usz total = context.total_test_files;
		usz finished = context.test_count.load();
		if (!has_entry && total != finished) return;
		double fraction = total > 0.0 ? finished / (double)total : 0;
		int full_width = (int)math::floor(fraction * progress_bar_width);
		int remainder = (int)math::floor(8 * (fraction * progress_bar_width % 1.0));
		io::printf("%s%s%sTest progress: [", Ansi.RESET, CLEAR_LINE, MOVETO_COLUMN_ONE);
		usz failed = context.failure_count.load();
		io::print(failed ? Ansi.RED : Ansi.GREEN);
		for (int i = 0; i < full_width; i++) io::print("█");
		if (full_width != progress_bar_width) io::print(PARTS[remainder]);
		for (int i = full_width + 1; i < progress_bar_width; i++) io::print(" ");
		io::printf("%s] %.1f%% complete (%d failed)", Ansi.RESET, fraction * 100.0, failed);
		if (has_entry) io::printf(" - Testing: %s", filename.basename());
		(void)io::stdout().flush();
	};
}

fn void print_failure_box(usz index, Path file_path, String error_output)
{
	Mutex.@in_lock(&context.print_mutex)
	{
		int width = get_terminal_width();

		io::printfn("%s%s%s", Ansi.RESET, CLEAR_LINE, MOVETO_COLUMN_ONE);
		io::print(Ansi.RED +++ "\u256d");
		for (int i = 2; i < width; i++) io::print("\u2500");
		io::printn("\u256e" +++ Ansi.RESET);

		io::printf("- %d/%d %s: %s", index, context.failure_count.load(), file_path, error_output)!!;

		if (!error_output.ends_with("\n")) io::printn();
		
		io::print(Ansi.RED +++ "\u2570");
		for (int i = 2; i < width; i++) io::print("\u2500");
		io::printn("\u256f" +++ Ansi.RESET);
	};
}

/* ----------------------- File collection ----------------------------- */
fn void? collect_files(Path file_path, List{Path}* files)
{
	PathList entries = path::ls(tmem, file_path)!!;
	foreach (f : entries)
	{
		Path current_file = file_path.append(tmem, f.str_view())!;
		switch
		{
			case path::is_dir(current_file): collect_files(current_file, files)!;
			case path::is_file(current_file):
				switch (current_file.extension() ?? "")
				{
					case "c3":
					case "c3t": files.push(current_file);
				}
			default:
				io::printfn("Skip %s", current_file);
		}
	}
}

/* ----------------------- Core test logic ------------------------------ */
fn bool test_file(Path file_path, TestOutput* output, usz index)
{
	if (global_should_quit.load()) return false;
	context.test_count.add(1);
	update_status(index, file_path, output);

	bool single;
	int thread_id = context.thread_id_counter.add(1);

 	Path test_dir = context.test_root.tappend(string::tformat("_c3test_%d", thread_id))!!;

	(void)path::rmtree(test_dir);
	defer @catch(path::rmtree(test_dir));

	if (@catch(path::mkdir(test_dir)))
	{
		io::fprintfn((OutStream)&output.buffer, "FAILED - Failed to create temp test directory '%s'.", test_dir)!!;
		os::exit(1);
	}

	switch (file_path.extension() ?? "")
	{
		case "c3": single = true;
		case "c3t": single = false;
		default:
			io::fprintfn((OutStream)&output.buffer,
				"FAILED - Unexpected file name '%s', expected a file with a '.c3' or '.c3t' suffix.",
				file_path.str_view())!!;
			return false;
	}

	char[]? content_raw = file::load_path_temp(file_path);
	if (catch content_raw)
	{
		io::fprintfn((OutStream)&output.buffer, "FAILED - Failed to open/read '%s'.", file_path)!!;
		return false;
	}
	String content = (String)content_raw;

	RunSettings settings;
	settings.opts.init(tmem);
	settings.input_files.init(tmem);
	settings.output_files.init(tmem);

	RunFile* rf = create_run_file(
		test_dir,
		file_path.basename()[..^(single ? 4 : 5)].tconcat(".c3"),
		false);

	settings.current_file = rf;
	settings.input_files.push(rf);

	bool found_skip_directive = false;

	int line_no = 1;
	
	Splitter lines = content.tokenize_all("\n", skip_last: true);
	while (try line = lines.next())
	{
		line = line.trim_right("\r");

		if (line.starts_with("// #") || line.starts_with("/* #"))
		{
			if (line.contains("#skip"))
			{
				found_skip_directive = true;
				if (!context.only_skipped)
				{
					context.skip_count.add(1);
					settings.current_file.close();
					foreach (r : settings.input_files) r.close();
					return true;
				}
				continue;
			}
			parse_header_directive(&line_no, line, &settings, single, test_dir);
			continue;
		}
		else if (line.contains("// #"))
		{
			if (!parse_trailing_directive(line_no, line, &settings, single))
			{
				foreach (r : settings.input_files) r.close();
				return false;
			}
		}
		
		settings.current_file.add_line(line);
		line_no++;
	}

	settings.current_file.close();
	defer foreach (r : settings.input_files) r.close();

	if (context.only_skipped && !found_skip_directive)
	{
		context.skip_count.add(1);
		return true;
	}

	List{String} cmdline;
	cmdline.init(tmem, 32);
	cmdline.push(context.compiler_path.str_view());

	cmdline.push_all({"compile-only", "--test","--max-mem", "512"});

	if (context.stdlib)
	{
		cmdline.push_all({"--stdlib", context.stdlib});
	}

	cmdline.push_all({"--llvm-out", test_dir.str_view() });
	cmdline.push_all({"--build-dir", test_dir.tappend("build").str_view()!! });

	foreach (file : settings.input_files)
	{
		cmdline.push(test_dir.tappend(file.name).str_view()!!);
	}

	if (!single)
	{
		cmdline.push("--emit-llvm");
	}

	cmdline.push(settings.debuginfo ? "-g" : "-g0");

	if (settings.arch) 
	{
		cmdline.push("--target");
		cmdline.push(settings.arch);
	}

	cmdline.push("-O0");

	if (settings.no_deprecation)
	{
		cmdline.push("--silence-deprecation");
	}

	cmdline.push(settings.safe ? "--safe=yes" : "--safe=no");

	foreach (opt : settings.opts)
	{
		cmdline.push(opt);
	}


	if (global_should_quit.load()) return false;
	SubProcess compilation = process::create(
		cmdline.array_view(),
		{ .search_user_path, .no_window, .inherit_environment })!!;

	defer compilation.destroy();
	
	DString out;
	out.init(tmem, 4096);
	char[4096] tmp_buf;
	InStream stderr_stream = &&compilation.stderr();
	while (try read = stderr_stream.read(&tmp_buf)) 
	{
		if (read == 0) break;
		if (out.len() < 65536) {
			out.append(tmp_buf[:read]);
		}
	}

	CInt result = compilation.join() ?? 1;

	if (result != 0 && result != 1)
	{
		out.append("\n[STDOUT]:\n");
		InStream stdout_stream = &&compilation.stdout();
		while (try read = stdout_stream.read(&tmp_buf))
		{
			if (read == 0) break;
			if (out.len() < 70000) out.append(tmp_buf[:read]);
		}
		
		io::fprintfn((OutStream)&output.buffer, "FAILED - Error(%s): %s", result, out)!!;
		return false;
	}

	if (!parse_result(out, settings, &cmdline, (OutStream)&output.buffer, result)) return false;

	foreach (file : settings.output_files)
	{
		Path expected_file = test_dir.tappend(file.name)!!;
		if (!file::exists(expected_file.str_view()))
		{
			io::fprintfn((OutStream)&output.buffer, "FAILED - Did not compile file %s.", file.name)!!;
			return false;
		}

		File file_ll = file::open(expected_file.str_view(), "rb")!!;
		defer (void)file_ll.close();

		String? next = file.expected_lines.pop_first();
		while (try line = io::treadline(&file_ll) && try next_value = next)
		{
			line = line.trim();
			if (line == "") continue;
			if (line.contains(next_value))
			{
				next = file.expected_lines.pop_first();
			}
		}

		if (try next)
		{
			io::fprintfn((OutStream)&output.buffer,
				"FAILED - %s did not contain: %s", file.name, next)!!;
			io::fprintfn((OutStream)&output.buffer,
				"\n\n------------------------------------------------>>>> %s\n", file.name)!!;
			(void)file_ll.seek(0);
			io::fprintn((OutStream)&output.buffer,
				(String)io::read_fully(tmem, &file_ll))!!;
			io::fprintfn((OutStream)&output.buffer,
				"<<<<------------------------------------------------- %s\n", file_path)!!;
			return false;
		}
	}

	context.success_count.add(1);
	update_status(index, {}, output);
	return true;
}

fn void test_file_threaded(any[] data)
{
	@pool_init(mem, 1024 * 1024)
	{
		Path file_path = *anycast(data[0], Path)!!;
		TestOutput* output = *anycast(data[1], TestOutput*)!!;
		usz index = *anycast(data[2], usz)!!;

		if (global_should_quit.load()) return;

		if (!test_file(file_path, output, index))
		{
			if (global_should_quit.load()) return;
			context.failure_count.add(1);
			if (!context.no_terminal)
			{
				print_failure_box(index, file_path, output.buffer.str_view());
			}
		}

		if (context.no_terminal)
		{
			if (global_should_quit.load() || output.buffer.len() == 0) return;
			Mutex.@in_lock(&context.print_mutex)
			{
				io::printf("- %d/%d %s: ",
					context.test_count.load(),
					context.failure_count.load(),
					file_path)!!;
				io::printn(output.buffer.str_view());
			};
		}
	};
}

/* --------------------------------------------------------------------- */
/* ----------------------------- Main ---------------------------------- */
/* --------------------------------------------------------------------- */
fn int main(String[] args)
{
	libc::signal(libc::SIGINT, fn void (CInt _signal)
	{
		global_should_quit.store(true);
	});

	String appname = args[0];
	if (args.len < 3) usage(appname);

	context.no_terminal = !io::stdout().isatty();
	context.start_cwd = path::tcwd()!!;
	context.print_mutex.init()!!;

	context.test_root = path::temp_directory(mem)!!;

	Path? path = args[1].to_tpath();
	if (catch path) arg_error_exit(appname, "Invalid compiler path: %s", args[1]);
	context.compiler_path = path.absolute(tmem)!!;
	if (!path::is_file(context.compiler_path))
	{
		error_exit("Error: Invalid path to compiler: %s (%s relative to %s)", context.compiler_path, args[1], context.start_cwd);
	}

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
					arg_error_exit(appname,
						"Expected --stdlib to be followed by the path to the alternative standard library.");
				}
				context.stdlib = args[i + 1];
				i++;
				if (!os::native_is_dir(context.stdlib))
				{
					error_exit(appname,
						"Stdlib directory '%s' cannot be found.",
						context.stdlib);
				}
				context.stdlib = context.start_cwd
					.tappend(context.stdlib)
					.absolute(mem)!!
					.str_view();
			case "--threads":
				if (i == args.len - 1 || args[i + 1].starts_with("-"))
				{
					arg_error_exit(appname,
						"Expected --threads to be followed by the number of threads.");
				}
				num_threads = args[i + 1].to_int()!!;
				i++;
			default:
				arg_error_exit(appname, "Unknown option '%s'.", args[i]);
		}
	}

	Path? file = args[2].to_tpath().absolute(tmem);
	if (catch file) arg_error_exit(appname, "Invalid path: '%s'.", args[2]);

	PathList files_to_test;
	files_to_test.init(tmem);

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
		}
		mem::free(test_outputs);
	}

	context.pool.init(num_threads, context.total_test_files)!!;
	defer context.pool.stop_and_destroy();

	for (usz i = 0; i < context.total_test_files; i++)
	{
		context.pool.push(&test_file_threaded,
			files_to_test[i],
			&test_outputs[i],
			i)!!;
	}

	while (context.success_count.load() + context.failure_count.load() + context.skip_count.load() < context.total_test_files)
	{
		if (global_should_quit.load())
		{
			io::printn("\n[Ctrl+C] Stopping tests and cleaning up...");
			break; 
		}
		thread::sleep_ms(50);
	}

	context.pool.stop_and_destroy(); 

	io::printn(" Removing temporary directories...");
	PathList entries = path::ls(mem, context.test_root)!!;
	foreach (f : entries)
	{
		if (f.str_view().starts_with("_c3test_"))
		{
			path::rmtree(context.test_root.tappend(f.str_view())!!)!!;
		}
	}

	if (global_should_quit.load()) os::exit(1);	

	int total_files = context.test_count.load();
	int success = context.success_count.load();
	int skipped_files = context.skip_count.load();
	int failed = context.failure_count.load();

	int display_total = total_files;
	int display_skipped = skipped_files;
	int display_run = total_files - skipped_files;

	if (context.only_skipped)
	{
		display_skipped = 0;
		display_total = success + failed;
		display_run = display_total;
	}

	bool all_ok = (success == display_run) && (failed == 0);

	if (!context.no_terminal)
	{
		io::print(all_ok ? (String)Ansi.GREEN : (String)Ansi.RED);
	}

	io::printfn(
		"\nFound %d tests: %.1f%% (%d / %d) passed (%d skipped, %d failed).",
		display_total,
		(100.0 * success) / math::max(1, display_run),
		success,
		display_run,
		display_skipped,
		failed);

	if (!context.no_terminal)
	{
		io::print(Ansi.RESET);
	}

	return all_ok ? 0 : 1;
}