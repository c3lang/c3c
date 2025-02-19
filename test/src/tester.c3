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
	args = { "foo", "../cmake-build-debug-latest/c3c", "test_suite/" };
	if (args.len < 3 || args.len > 4) usage();

	// Check compiler path
	start_cwd = path::temp_cwd()!!;
	test_dir = start_cwd.temp_append("_c3test")!!;
	Path! path = start_cwd.temp_append(args[1]);
	if (catch path) arg_error_exit("Invalid compiler path: %s", args[1]);

	if (!path::is_file(path))
	{
		error_exit("Error: Invalid path to compiler: %s", path.path_string);
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
	List{String} expected_lines;
	List{Error} warnings;
	List{Error} errors;
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

fn bool check_line(RunSettings*, String type, String file, String line, String col, String message)
{
	return false;
/*
	def check_line(self, typ, file, line, col, message):
		map_ = {}
		if typ == 'Error':
			map_ = self.errors
		elif typ == 'Warning':
			map_ = self.warnings
		else:
			self.exit_error("Unknown type: " + typ)
		file = os.path.basename(file)
		key = file + ":" + line
		value = map_.get(key)
		if value is None:
			return False
		if value in message:
			del map_[key]
			return True
		else:
			return False
*/
}
fn bool parse_result(DString out, RunSettings settings)
{
	// Inefficient, fix.
	bool success = true;
	foreach (line : out.str_view().tsplit("\n"))
	{
		if (!line) continue;
		String[] parts = line.tsplit("|", 5);
		if (parts.len != 5)
		{
			error_exit("Unexpected error: %s", out);
		}
		if (!check_line(&settings, ...parts[:5]))
		{
			parts[0].convert_ascii_to_lower();
			io::printf("Unexpected %s in %s line %s:", parts[0], path::temp_new(parts[1]).basename()!!, parts[2]);
			io::printfn(`"%s"`, parts[4]);
			success = false;
		}
	}
	int not_found_errors, not_found_warnings;
	foreach (file : settings.files)
	{
		if (file.errors.len())
		{
			success = false;
			if (!not_found_errors) io::printn("Errors that never occurred:");
			foreach (i, &item : file.errors)
			{
				io::printfn(`%d. %s %d expected: "%s"`, ++not_found_errors, file.name, item.line, item.text);
			}
		}
	}
	foreach (file : settings.files)
    {
    	if (file.warnings.len())
		{
    		success = false;
    		if (!not_found_warnings) io::printn("Warnings that never occurred:");
    		foreach (i, &item : file.warnings)
    		{
    			io::printfn(`%d. %s %d expected: "%s"`, ++not_found_warnings, file.name, item.line, item.text);
    		}
    	}
    }
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
			error_exit("Unknown trailing directive '%s'", line);
	}
}
fn void parse_header_directive(int line_no, String line, RunSettings* settings, bool is_single)
{
	line = line[4..].trim();
	switch
	{
		case line.starts_with("error:"):
			line = line[6..].trim();
			settings.current_file.errors.push({ line_no, line });
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
			if (is_single) error_exit("'file' directive only allowed with .c3t");
			settings.current_file.close();
			line = line[5..].trim();
			RunFile* file = settings.current_file = create_file(line)!!;
			settings.files.push(file);
			settings.current_file = file;
		case line.starts_with("expect:"):
			if (is_single) error_exit("'expect' directive only allowed with .c3t");
			line = line[7..].trim();
			settings.current_file.close();
			RunFile* file = settings.current_file = create_file(line, llvm: true)!!;
			settings.files.push(file);
		default:
			io::printfn("Unknown header directive '%s'", line);
			libc::exit(1);
	}
}

fn void test_file(Path file_path)
{
	io::printfn("Checking %s", file_path);
	static int count = 1;
	bool single;
	(void)path::rmtree(test_dir);
	if (@catch(path::mkdir(test_dir)))
	{
		io::printfn("Failed to create temp test directory '%s'.", test_dir);
		libc::exit(1);
	}
	switch (file_path.extension() ?? "")
	{
		case "c3":
			single = true;
		case "c3t":
			single = false;
		default:
			io::printfn("Unexpected file name '%s', expected a file with a '.c3' or '.c3t' suffix.", file_path.str_view());
			libc::exit(1);
	}
	File! f = file::open_path(file_path, "rb");
	if (catch f)
	{
		io::printfn("Error: Failed to open '%s'.", file_path);
		libc::exit(1);
	}
	defer (void)f.close();
	RunSettings settings;
	settings.opts.temp_init();
	settings.files.temp_init();
	settings.current_file = create_file(file_path.basename()[..^(single ? 4 : 5)].tconcat(".c3"))!!;
	settings.files.push(settings.current_file);
	int line_no = 0;
	while (try line = io::treadline(&f))
	{
		if (line.starts_with("// #") || line.starts_with("/* #"))
		{
			parse_header_directive(line_no, line, &settings, single);
			continue;
		}
		else if (line.contains("// #"))
		{
			parse_trailing_directive(line_no, line, &settings, single);
			continue;
		}
		settings.current_file.add_line(line);
		line_no++;
	}
	settings.current_file.close();
	test_count += 1;
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
	io::printfn("Dir: %s", test_dir);
	path::chdir(test_dir)!!;
	io::printn(cmdline);
	SubProcess compilation = process::create(cmdline.str_view().trim().tsplit(" "), { .search_user_path = true })!!;
    CInt result = compilation.join()!!;
	DString out = dstring::temp_new();
	io::copy_to(&&compilation.stderr(), &out)!!;
	if (result != 0 && result != 1)
	{
		io::printfn("Error(%s): ", result, out);
		return;
	}
	if (!parse_result(out, settings)) return;
	foreach (file : settings.files)
	{
		if (!file.is_llvm) continue;
		if (!file::exists(file.name))
		{
    		io::printfn("Did not compile file %s.", file.name);
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
    		io::printfn(`%s did not contain: "%s"`, file.name, next);
    		io::printfn("\n\n\n---------------------------------------------------> %s\n\n", file.name);
    		(void)file_ll.seek(0);
    		(void)io::printn((String)io::read_new_fully(&file_ll, allocator: allocator::temp()));
    		io::printfn("<---------------------------------------------------- %s\n", file_path);
    		return;
    	}
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
