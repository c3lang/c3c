module test_suite_runner;
import std::io, std::math, std::os, std::collections;
import libc;

Path compiler_path;
int test_count;
int skip_count;
int success_count;
Path start_cwd;
Path test_dir;
bool print_to_file;
String stdlib;


fn int main(String[] args)
{
	// Grab the name for `usage`
	String appname = args[0];
	if (args.len < 3) usage(appname);

	// Do we want the print to file variant
	print_to_file = !io::stdout().isatty();

	// Retain our current path.
	start_cwd = path::tcwd()!!;

	// Create our test path, note that this prevents us from doing tests in parallel
	test_dir = start_cwd.tappend("_c3test_")!!;
	defer (void)path::rmtree(test_dir);

	// Find the compiler
	Path? path = start_cwd.tappend(args[1]);
	if (catch path) arg_error_exit(appname, "Invalid compiler path: %s", args[1]);
	// Is it a valid file?
	if (!path::is_file(path))
	{
		error_exit("Error: Invalid path to compiler: %s (%s relative to %s)", path.path_string, args[1], start_cwd);
	}
	// Ok we're done.
	compiler_path = path;

	bool only_skipped = false;
	for (int i = 3; i < args.len; i++)
	{
		String arg = args[i];
		switch (arg)
		{
			case "--no-terminal":
				print_to_file = true;
			case "-s":
			case "--skipped":
				only_skipped = true;
			case "--stdlib":
				if (i == args.len - 1 || args[i + 1].starts_with("-"))
				{
					arg_error_exit(appname, "Expected --stdlib to be followed by the path to the alternative standard library.");
				}
				stdlib = args[i + 1];
				i++;
				if (!os::native_is_dir(stdlib))
				{
					error_exit(appname, "Stdlib directory '%s' cannot be found.", stdlib);
				}
				stdlib = start_cwd.tappend(stdlib).str_view()!!;
			default:
				arg_error_exit(appname, "Unknown option '%s'.", args[i]);
		}
	}
	if (only_skipped && args[3] != "-s" && args[3] != "--skipped") usage(appname);

	// Get the directory or file to test
	Path? file = args[2].to_tpath();
	if (catch file) arg_error_exit(appname, "Invalid path: '%s'.", args[2]);

	// Now just run all tests recursively.
	switch
	{
		case path::is_file(file):
			test_file(file);
		case path::is_dir(file):
			test_path(file)!!;
		default:
			error_exit("Error: Path wasn't to directory or file: %s", file);
	}

	// Print the test result
	io::printfn("Found %d tests: %.1f%% (%d / %d) passed (%d skipped).",
        test_count, (100.0 * success_count) / math::max(1, test_count - skip_count),
        success_count, test_count - skip_count, skip_count);
    return success_count == test_count - skip_count ? 0 :  1;
}

struct Error
{
	int line;
	String text;
}

<*
 Information about a given file that we're running
*>
struct RunFile
{
	String name;
	bool is_output;
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

fn RunFile*? create_input_file(String filename)
{
	File file = file::open_path(test_dir.tappend(filename), "wb")!;
	RunFile *run_file = mem::tnew(RunFile, { .name = filename, .file = file, .is_output = false });
	return run_file;
}

fn RunFile*? create_output_file(String filename)
{
	RunFile *run_file = mem::tnew(RunFile, { .name = filename, .is_output = true });
	return run_file;
}

fn void RunFile.close(&self)
{
	if (!self.is_output) (void)self.file.close();
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
<*
 Check a line from stderr returned by the compiler
*>
fn bool check_line(RunSettings* settings, String type, String file, String line_str, String col, String message) => @pool()
{
	// Convert the line number
	int line = line_str.to_int()!!;

	// Get the base name
	String basename = file.file_tbasename()!!;

	// Loop through our input files (usually only 1!)
	foreach (f : settings.input_files)
	{
		if (f.name != basename) continue;
		List {Error}* list;
		switch (type)
		{
			case "Error":
				list = &f.errors;
				break;
			case "Warning":
				list = &f.warnings;
				break;
			default:
				error_exit("FAILED - Unknown error type '%s'", type);
		}
		// See if we match the message
		foreach (i, err : *list)
		{
			if (line != err.line) continue;
			if (!message.contains(err.text)) return false;
			// Match, remove it!
			list.remove_at(i);
			return true;
		}
	}
	// No match
	return false;
}

<*
 Parse all stderr output
*>
fn bool parse_result(DString out, RunSettings settings)
{
	// Note that this is a bit inefficient
	bool success = true;
	int errors = 0;
	foreach (line : out.str_view().tsplit("\n"))
	{
		// Skip empty lines
		if (!line) continue;
		// Split the output from the compiler using `|`
		String[] parts = line.tsplit("|", 5);
		// We should have 5 parts, otherwise just print an error for this whole thing.
		if (parts.len != 5)
		{
			io::printn("FAILED - Unexpected response from compiler:");
			io::printn("Output: ----------------------------------------------------------");
			io::print(out);
			io::printn("------------------------------------------------------------------");
			return false;
		}
		// Check the line
		if (!check_line(&settings, ...parts[:5]))
		{
			// Print error if there is no match.
			if (success)
			{
				io::printn("FAILED\n\n Unexpected compilation errors:");
				io::printn(" ------------------------------");
			}
			io::printf("  %d. %s at %s:%s: ", ++errors, parts[0], parts[1].file_tbasename()!!, parts[2]);
			io::printfn(`"%s"`, parts[4]);
			success = false;
		}
	}
	// Now let's check for missing errors:
	int not_found_errors, not_found_warnings;
	foreach (file : settings.input_files)
	{
		if (file.errors.len())
		{
			if (success) io::printn("FAILED - Missing errors");
			if (!not_found_errors)
			{
				io::printn();
				io::printn(" Errors that never occurred:");
				io::printn(" ---------------------------");
			}
			success = false;
			foreach (i, &item : file.errors)
			{
				io::printfn(`  %d. %s:%d expected: "%s"`, ++not_found_errors, file.name, item.line, item.text);
			}
		}
	}
	// Missing warnings
	foreach (file : settings.input_files)
    {
    	if (file.warnings.len())
		{
			if (success) io::printn("FAILED - Missing warnings");
    		success = false;
			if (!not_found_warnings)
			{
				if (!success) io::printn();
				io::printn(" Warnings that never occurred:");
				io::printn(" -----------------------------");
			}
    		foreach (i, &item : file.warnings)
    		{
    		    io::printn(file.name);
    		    io::printn("Ok");
    		    io::printn(item.text);
				io::printfn(`  %d. %s:%d expected: "%s"`, ++not_found_errors, file.name, item.line, item.text);
    		}
    	}
    }
    if (!success) io::printn();
    return success;
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
			settings.current_file.warnings.push({ line_number, line });
		case line.starts_with("error:"):
			line = line[6..].trim();
			settings.current_file.errors.push({ line_number, line });
		default:
			error_exit("FAILED - Unknown trailing directive '%s'", line);
	}
}

<*
 Parse header directives, #error, #safe, #debuginfo, #opt, #target, #deprecation, #file and #expect
*>
fn void parse_header_directive(int* line_no, String line, RunSettings* settings, bool is_single)
{
	line = line[4..].trim();
	switch
	{
		case line.starts_with("error:"):
			line = line[6..].trim();
			if (settings.current_file.is_output) return;
			settings.current_file.errors.push({ *line_no, line });
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
			RunFile* file = settings.current_file = create_input_file(line)!!;
			*line_no = 1;
			settings.input_files.push(file);
			settings.current_file = file;
		case line.starts_with("expect:"):
			if (is_single) error_exit("FAILED - 'expect' directive only allowed with .c3t");
			line = line[7..].trim();
			settings.current_file.close();
			RunFile* file = settings.current_file = create_output_file(line)!!;
			settings.output_files.push(file);
		default:
			io::printfn("FAILED - Unknown header directive '%s'", line);
			os::exit(1);
	}
}

<*
 Test a file
*>
fn void test_file(Path file_path)
{
	test_count++;
	if (print_to_file)
	{
		io::printf("- %d/%d %s: ", test_count, test_count - success_count - 1, file_path);
	}
	else
	{
		io::printf("- %d/%d Compiling: %s ", test_count, test_count - success_count - 1, file_path);
	}
	(void)io::stdout().flush();
	static int count = 1;
	bool single;
	(void)path::rmtree(test_dir);
	if (@catch(path::mkdir(test_dir)))
	{
		error_exit("FAILED - Failed to create temp test directory '%s'.", test_dir);
	}
	switch (file_path.extension() ?? "")
	{
		case "c3":
			single = true;
		case "c3t":
			single = false;
		default:
			error_exit("FAILED - Unexpected file name '%s', expected a file with a '.c3' or '.c3t' suffix.", file_path.str_view());
	}
	File? f = file::open_path(file_path, "rb");
	if (catch f)
	{
		error_exit("FAILED - Failed to open '%s'.", file_path);
	}
	defer (void)f.close();
	RunSettings settings;
	settings.current_file = create_input_file(file_path.basename()[..^(single ? 4 : 5)].tconcat(".c3"))!!;
	settings.input_files.push(settings.current_file);
	int line_no = 1;
	while (try line = io::treadline(&f))
	{
		if (line.starts_with("// #") || line.starts_with("/* #"))
		{
			parse_header_directive(&line_no, line, &settings, single);
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

	// Construct the compile line
	List{String} cmdline;
	cmdline.push(compiler_path.str_view());
	$if env::OPENBSD:
		cmdline.push("--max-mem");
		cmdline.push("128");
	$endif
	cmdline.push("compile-only");
	cmdline.push("--test");
	if (stdlib)
	{
		cmdline.push("--stdlib");
		cmdline.push(stdlib);
		cmdline.push("--enable-new-generics");
	}
	cmdline.push("--llvm-out");
	cmdline.push(".");
	foreach (file : settings.input_files)
	{
		cmdline.push(file.name);
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
	path::chdir(test_dir)!!;
	// Start process
	SubProcess compilation = process::create(cmdline.array_view(), { .search_user_path, .no_window, .inherit_environment })!!;
	defer compilation.destroy();
    CInt result = compilation.join() ?? 1;
	DString out;
	io::copy_to(&&compilation.stderr(), &out)!!;
	if (result != 0 && result != 1)
	{
		(void)io::copy_to(&&compilation.stdout(), &out);
		io::printfn("FAILED - Error(%s): %s", result, out);
		return;
	}
	if (!parse_result(out, settings)) return;
	foreach (file : settings.output_files)
	{
		if (!file::exists(file.name))
		{
    		io::printfn("FAILED - Did not compile file %s.", file.name);
    		return;
		}
		File file_ll = file::open(file.name, "rb")!!;
		defer (void)file_ll.close();
		String? next = file.expected_lines.pop_first();
		while (try line = io::treadline(&file_ll) && try value = next)
		{
			line = line.trim();
			if (line == "") continue;
			if (line.contains(value))
			{
				next = file.expected_lines.pop_first();
			}
		}
		if (try next)
		{
    		io::printfn(`FAILED - %s did not contain: "%s"`, file.name, next);
    		io::printfn("\n\n\n---------------------------------------------------> %s\n\n", file.name);
    		(void)file_ll.seek(0);
    		(void)io::printn((String)io::read_fully(tmem, &file_ll));
    		io::printfn("<---------------------------------------------------- %s\n", file_path);
    		return;
    	}
    }
    if (print_to_file)
    {
	    io::print("Passed.");
        io::printn();
    }
    else
    {
        for (int i = 0; i < 200; i++) io::print("\b \b");
    }
    success_count++;
}

fn void? test_path(Path file_path)
{
	(void)path::chdir(start_cwd);
	foreach (file : path::ls(tmem, file_path)!!)
	{
		@pool()
		{
			(void)path::chdir(start_cwd);
			file = file_path.tappend(file.str_view())!;
			switch
			{
				case path::is_dir(file):
					test_path(file)!;
				case path::is_file(file):
					switch (file.extension() ?? "")
                	{
                		case "c3":
                		case "c3t":
                		    test_file(file);
                	}
                default:
                    io::printfn("Skip %s", file);
					// Ignore
			}
		};
	}
}

fn void usage(String appname) @noreturn
{
    io::printfn("Usage: %s <compiler path> <file/dir> [options]", appname);
    io::printn();
    io::printn("Options:");
    io::printn("  -s, --skipped       only run skipped tests");
    io::printn("  --stdlib <path>     override the path to stdlib");
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


