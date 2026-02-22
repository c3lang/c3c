<*
 Compare `C3 zip` vs `7z` extraction
 External dependencies: 7z, diff
*>
module verify_zip;
import std;
import libc;

fn int main(String[] args)
{
	if (args.len < 2)
	{
		io::printfn("Usage: %s [-r|--recursive] [-o|--output <dir>] <zip_dir>", args[0]);
		return 1;
	}

	bool recursive = false;
	String zip_dir;
	String output_dir;

	for (int i = 1; i < args.len; i++)
	{
		String arg = args[i];
		switch (arg)
		{
			case "-r":
			case "--recursive":
				recursive = true;
			case "-o":
			case "--output":
				if (++i >= args.len)
				{
					io::printfn("Error: %s requires a directory path", arg);
					return 1;
				}
				output_dir = args[i];
			default:
				if (arg.starts_with("-"))
				{
					io::printfn("Error: unknown option %s", arg);
					return 1;
				}
				if (zip_dir)
				{
					io::printfn("Error: multiple zip directories specified ('%s' and '%s')", zip_dir, arg);
					return 1;
				}
				zip_dir = arg;
		}
	}

	if (!zip_dir)
	{
		io::printfn("Error: no zip directory specified.");
		return 1;
	}

	return process_dir(zip_dir, recursive, output_dir);
}

fn int process_dir(String dir, bool recursive, String output_dir)
{
	PathList? files = path::ls(tmem, path::temp(dir)!!);
	if (catch excuse = files)
	{
		io::printfn("Could not open directory: %s (Excuse: %s)", dir, excuse);
		return 1;
	}

	foreach (p : files)
	{
		String name = p.basename();
		if (name == "." || name == "..") continue;

		String zip_path = path::temp(dir)!!.tappend(name)!!.str_view();

		if (file::is_dir(zip_path))
		{
			if (recursive)
			{
				if (process_dir(zip_path, recursive, output_dir) != 0) return 1;
			}
			continue;
		}

		if (!name.ends_with(".zip")) continue;

		ulong size = 0;
		File? f = file::open(zip_path, "rb");
		if (try fh = f)
		{
			(void)fh.seek(0, Seek.END);
			size = fh.seek(0, Seek.CURSOR) ?? 0;
			fh.close()!!;
		}
		io::printf("Verifying %-40s [%7d KB] ", name[:(min(name.len, 40))], size / 1024);

		switch (verify_one(zip_path, output_dir))
		{
			case 0:
				io::printfn("%sFAILED%s ❌", Ansi.RED, Ansi.RESET);
				return 1;
			case 1:
				io::printfn("%sPASSED%s ✅", Ansi.GREEN, Ansi.RESET);
			default:
				io::printn();
		}
	}

	return 0;
}

fn int verify_one(String zip_path, String output_dir)
{
	Path extract_root;
	if (output_dir)
	{
		extract_root = path::temp(output_dir)!!;
	}
	else
	{
		extract_root = path::temp_directory(tmem)!!;
	}

	String name = (String)path::temp(zip_path)!!.basename();

	Path temp_c3 = extract_root.tappend(name.tconcat("_c3"))!!;
	Path temp_7z = extract_root.tappend(name.tconcat("_7z"))!!;

	(void)path::mkdir(temp_c3, true);
	(void)path::mkdir(temp_7z, true);

	ZipArchive? archive = zip::open(zip_path, "r");
	if (catch excuse = archive)
	{
		io::printfn("%sFAIL%s (open: %s)", Ansi.RED, Ansi.RESET, excuse);
		return 0;
	}
	defer (void)archive.close();

	Time start = time::now();
	if (catch excuse = archive.extract(temp_c3.str_view()))
	{
		if (excuse == zip::ENCRYPTED_FILE)
		{
			io::printf("%sSKIPPED%s (Encrypted)", Ansi.YELLOW, Ansi.RESET);
			return 2;
		}
		io::printfn("%sFAIL%s (extract: %s)", Ansi.RED, Ansi.RESET, excuse);
		return 0;
	}
	Duration c3_time = time::now() - start;

	start = time::now();
	if (!extract_7z(zip_path, temp_7z.str_view()))
	{
		io::printfn("%sFAIL%s (7z extract)", Ansi.RED, Ansi.RESET);
		return 0;
	}
	Duration p7_time = time::now() - start;

	io::printf(" [C3: %5d ms, 7z: %5d ms]", (long)c3_time / 1000, (long)p7_time / 1000);

	io::print(" Comparing... ");
	if (!compare_dirs(temp_c3.str_view(), temp_7z.str_view()))
	{
		io::printfn("%sFAIL%s (Differences found)", Ansi.RED, Ansi.RESET);
		return 0;
	}

	// keep files on error for manual verification
	(void)path::rmtree(temp_c3);
	(void)path::rmtree(temp_7z);

	return 1;
}

fn bool extract_7z(String zip_path, String output_dir)
{
	String out_opt = "-o".tconcat(output_dir);
	String[] cmd = { "7z", "x", zip_path, out_opt, "-y", "-bb0" };
	SubProcess? proc = process::create(cmd, { .search_user_path = true });
	if (catch excuse = proc) return false;
	return (int)proc.join()!! == 0;
}

fn bool compare_dirs(String dir1, String dir2)
{
	String[] cmd = { "diff", "-r", dir1, dir2 };
	SubProcess? proc = process::create(cmd, { .search_user_path = true, .inherit_stdio = true });
	if (catch excuse = proc) return false;
	int res = (int)proc.join()!!;
	return res == 0;
}
