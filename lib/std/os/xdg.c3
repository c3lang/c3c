module std::os::posix @if(env::POSIX);
import std::io, std::os::env;

fn String? xdg_user_dir_lookup(Allocator allocator, String type) => @pool()
{
	String home = env::tget_var("HOME")!;
	String config_file @noinit;
	if (try String config_home = env::tget_var("XDG_CONFIG_HOME") && config_home.len)
	{
		config_file = config_home.tconcat("/user-dirs.dirs");
	}
	else
	{
		config_file = home.tconcat("/.config/user-dirs.dirs");
	}
	File f = file::open(config_file, "r")!;
	defer (void)f.close();
	while (try line = io::treadline(&f))
	{
		line = line.trim();
		if (!line.starts_with("XDG_")) continue;
		line = line[4..];
		if (!line.starts_with(type)) continue;
		line = line[type.len ..];
		if (!line.starts_with("_DIR")) continue;
		line = line[4..];
		line = line.trim();
		if (!line.starts_with("=")) continue;
		line = line[1..].trim();
		if (!line.starts_with("\"")) continue;
		line = line[1..];
		bool relative = false;
		if (line.starts_with("$HOME/"))
		{
			relative = true;
			line = line[6..];
		}
		else
		{
			if (!line.starts_with("/")) continue;
			if (line.len < 2) continue;
			line = line[1..];
		}
		if (line.len < 1 || line[^1] != '"') continue;
		line = line[:^1];
		if (relative)
		{
			return string::format(allocator, "%s/%s", home, line);
		}
		return line.copy(allocator);
	}
	return io::PATH_COULD_NOT_BE_FOUND?;
}