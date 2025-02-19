import std::io;
import std::io::file;
import libc;
import std::math;
import std::collections::map;
import std::collections::list;
import std::core::dstring;
import std::os::process;

def StringMap = HashMap{String, String};
def StringList = List{String};

String appname;
Path compiler_path;
int test_count;
int skip_count;
int success_count;
Path start_cwd;
Path test_dir;
bool print_to_file;

fn void arg_error_exit(String fmt, args...) @noreturn
{
	io::printfn(fmt, ...args);
	usage();
}

fn void error_exit(String fmt, args...) @noreturn
{
	io::printfn(fmt, ...args);
	libc::exit(1);
}


fn void main(String[] args)
{
	appname = args[0];
	if (args.len < 3 || args.len > 4) usage();
	print_to_file = !io::stdout().isatty();
	// Check compiler path
	start_cwd = path::temp_cwd()!!;
	test_dir = start_cwd.temp_append("_c3test")!!;
	Path! path = start_cwd.temp_append(args[1]);
	if (catch path) arg_error_exit("Invalid compiler path: %s", args[1]);

	if (!path::is_file(path))
	{
		error_exit("Error: Invalid path to compiler: %s (%s relative to %s)", path.path_string, args[1], start_cwd);
	}
	compiler_path = path;

	bool only_skipped = args.len == 4;

	if (only_skipped && args[3] != "-s" && args[3] != "--skipped") usage();

	Path! file = path::temp_new(args[2]);
	if (catch file) arg_error_exit("Invalid path: '%s'.", args[2]);

	switch
	{
		case path::is_file(file):
			test_file(file);
		case path::is_dir(file):
			test_path(file)!!;
		default:
			error_exit("Error: Path wasn't to directory or file: %s", file);
	}

	io::printfn("Found %d tests: %.1f%% (%d / %d) passed (%d skipped).",
        test_count, 100 * success_count / math::max(1, test_count - skip_count),
        success_count, test_count - skip_count, skip_count);
    libc::exit(success_count == test_count - skip_count ? 0 :  1);
}

struct Error
{
	int line;
	String text;
}
struct RunFile
{
	String name;
	File file;
	int line_offset;
	List {String} expected_lines;
	List {Error} warnings;
	List {Error} errors;
	bool is_llvm;
}

fn void RunFile.add_line(&self, String line)
{
	if (self.is_llvm)
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

fn RunFile*! create_file(String filename, bool llvm = false)
{
	File file = file::open_path(test_dir.temp_append(filename), "wb")!;
	RunFile *run_file = mem::temp_new(RunFile, { .name = filename, .file = file, .is_llvm = llvm });
	run_file.warnings.temp_init();
	run_file.errors.temp_init();
	if (llvm) run_file.expected_lines.temp_init();
	return run_file;
}

fn void RunFile.close(&self)
{
	(void)self.file.close();
}

struct RunSettings
{
	bool safe;
	bool debuginfo;
	bool no_deprecation;
	List{String} opts;
	String arch;
	RunFile* current_file;
	List{RunFile*} files;
}

fn bool check_line(RunSettings* settings, String type, String file, String line_str, String col, String message) => @pool()
{
	int line = line_str.to_int()!!;
	String basename = path::temp_new(file).basename()!!;
	foreach (f : settings.files)
	{
		if (f.name != basename) continue;
		List{Error}* list;
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
		foreach (i, err : *list)
		{
			if (line != err.line) continue;
			if (!message.contains(err.text)) return false;
			list.remove_at(i);
			return true;
		}
	}
	return false;
}

fn bool parse_result(DString out, RunSettings settings)
{
	// Inefficient, fix.
	bool success = true;
	int errors = 0;
	foreach (line : out.str_view().tsplit("\n"))
	{
		if (!line) continue;
		String[] parts = line.tsplit("|", 5);
		if (parts.len != 5)
		{
			io::printn("FAILED - Unexpected response from compiler:");
			io::printn("Output: ----------------------------------------------------------");
			io::print(out);
			io::printn("------------------------------------------------------------------");
			return false;
		}
		if (!check_line(&settings, ...parts[:5]))
		{
			if (success)
			{
				io::printn("FAILED\n\n Unexpected compilation errors:");
				io::printn(" ------------------------------");
			}
			io::printf("  %d. %s at %s:%s: ", ++errors, parts[0], path::temp_new(parts[1]).basename()!!, parts[2]);
			io::printfn(`"%s"`, parts[4]);
			success = false;
		}
	}
	int not_found_errors, not_found_warnings;
	foreach (file : settings.files)
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
	foreach (file : settings.files)
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
				io::printfn(`  %d. %s:%d expected: "%s"`, ++not_found_errors, file.name, item.line, item.text);
    		}
    	}
    }
    if (!success) io::printn();
    return success;
}

fn void parse_trailing_directive(int line_number, String line, RunSettings* settings, bool is_single)
{
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
fn void parse_header_directive(int* line_no, String line, RunSettings* settings, bool is_single)
{
	line = line[4..].trim();
	switch
	{
		case line.starts_with("error:"):
			line = line[6..].trim();
			settings.current_file.errors.push({ *line_no, line });
		case line.starts_with("safe:"):
			settings.safe = line[5..].trim() == "yes";
		case line.starts_with("debuginfo:"):
			settings.debuginfo = line[10..].trim() == "yes";
		case line.starts_with("opt:"):
			settings.opts.push(line[4..].trim());
		case line.starts_with("target:"):
			settings.arch = line[7..].trim();
		case line.starts_with("deprecation:"):
			settings.no_deprecation = line[12..].trim() == "no";
		case line.starts_with("file:"):
			if (is_single) error_exit("FAILED - 'file' directive only allowed with .c3t");
			settings.current_file.close();
			line = line[5..].trim();
			RunFile* file = settings.current_file = create_file(line)!!;
			*line_no = 1;
			settings.files.push(file);
			settings.current_file = file;
		case line.starts_with("expect:"):
			if (is_single) error_exit("FAILED - 'expect' directive only allowed with .c3t");
			line = line[7..].trim();
			settings.current_file.close();
			RunFile* file = settings.current_file = create_file(line, llvm: true)!!;
			settings.files.push(file);
		default:
			io::printfn("FAILED - Unknown header directive '%s'", line);
			libc::exit(1);
	}
}

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
	File! f = file::open_path(file_path, "rb");
	if (catch f)
	{
		error_exit("FAILED - Failed to open '%s'.", file_path);
	}
	defer (void)f.close();
	RunSettings settings;
	settings.opts.temp_init();
	settings.files.temp_init();
	settings.current_file = create_file(file_path.basename()[..^(single ? 4 : 5)].tconcat(".c3"))!!;
	settings.files.push(settings.current_file);
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
	DString cmdline = dstring::temp_new(compiler_path.str_view());
	cmdline.append(" compile-only --test ");
	foreach (file : settings.files)
	{
		if (file.is_llvm) continue;
		cmdline.append(file.name);
		cmdline.append(" ");
	}
	if (!single) cmdline.append("--emit-llvm ");
	cmdline.append(settings.debuginfo ? "-g " : "-g0 ");
	if (settings.arch) cmdline.appendf("--target %s ", settings.arch);
	cmdline.append("-O0 ");
	if (settings.no_deprecation) cmdline.append("--silence-deprecation ");
	cmdline.append(settings.safe ? "--safe=yes " : "--safe=no ");
	foreach (opt : settings.opts)
	{
		cmdline.appendf("%s ", opt);
	}
	path::chdir(test_dir)!!;
	SubProcess compilation = process::create(cmdline.str_view().trim().tsplit(" "), { .search_user_path, .no_window, .inherit_environment })!!;
	defer compilation.destroy();
    CInt result = compilation.join()!!;
	DString out = dstring::temp_new();
	io::copy_to(&&compilation.stderr(), &out)!!;
	if (result != 0 && result != 1)
	{
		(void)io::copy_to(&&compilation.stdout(), &out);
		io::printfn("FAILED - Error(%s): ", result, out);
		return;
	}
	if (!parse_result(out, settings)) return;
	foreach (file : settings.files)
	{
		if (!file.is_llvm) continue;
		if (!file::exists(file.name))
		{
    		io::printfn("FAILED - Did not compile file %s.", file.name);
    		return;
		}
		File file_ll = file::open(file.name, "rb")!!;
		defer (void)file_ll.close();
		String! next = file.expected_lines.pop_first();
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
    		(void)io::printn((String)io::read_new_fully(&file_ll, allocator: allocator::temp()));
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

fn void! test_path(Path file_path)
{
	(void)path::chdir(start_cwd);
	foreach (file : path::temp_ls(file_path)!!)
	{
		@pool()
		{
			(void)path::chdir(start_cwd);
			file = file_path.temp_append(file.str_view())!;
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

fn void usage() @noreturn
{
    io::printfn("Usage: %s <compiler path> <file/dir> [-s]", appname);
    io::printn();
    io::printn("Options:");
    io::printn("  -s, --skipped       only run skipped tests");
	libc::exit(0);
}
