module std::io::file::os;

// native_temp_directory, for non Win32
$if (!env::os_is_win32()):

fn Path! native_temp_directory(Allocator* using = mem::heap())
{
	foreach (String env : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" })
	{
		String tmpdir = env::get_var(env) ?? "";
		if (tmpdir) return path::new(tmpdir, using);
	}
	return path::new("/tmp", using);
}

$if (env::COMPILER_LIBC_AVAILABLE):

extern fn void* opendir(ZString);
extern fn void closedir(void*);

const DT_UNKNOWN =  0;
const DT_FIFO    =  1;
const DT_CHR     =  2;
const DT_DIR     =  4;
const DT_BLK     =  6;
const DT_REG     =  8;
const DT_LNK     = 10;
const DT_SOCK    = 12;
const DT_WHT     = 14;

fn PathList! native_readdir(Path dir, bool no_dirs, bool no_symlinks, String mask, Allocator* using)
{
	PathList list;
	list.init(.using = using);
	void* directory = opendir(dir.as_str() ? dir.as_zstr() : (ZString)".");
	defer if (directory) closedir(directory);
	if (!directory) return (path::is_dir(dir) ? IoError.CANNOT_READ_DIR : IoError.FILE_NOT_DIR)!;
	NativeDirentry* entry;
	while ((entry = readdir(directory)))
	{
		String name = ((ZString)&entry.name).as_str();
		if (!name || name == "." || name == "..") continue;
		if (entry.type == DT_LNK && no_symlinks) continue;
		if (entry.type == DT_DIR && no_dirs) continue;
		Path path = path::new(name.copy(using), using)!!;
		list.append(path);
	}
	return list;
}

$endif;

$endif;

$if (!env::os_is_darwin() && !env::os_is_win32()):

fn usz! native_file_size(String path)
{
	File f = file::open(path, "r")?;
	defer (void)f.close();
	return f.seek(0, Seek.END)?;
}

$if (env::os_is_posix() && env::COMPILER_LIBC_AVAILABLE):

fn bool native_file_or_dir_exists(String path)
{
	@pool()
	{
		return os::access(path.zstr_tcopy(), 0 /* F_OK */) != -1;
	};
}

fn bool native_is_file(String path)
{
	File! f = file::open(path, "r");
	defer (void)f.close();
	return try? f;
}

fn bool native_is_dir(String path)
{
	return native_file_or_dir_exists(path) && !native_is_file(path);
}

$else:

fn bool native_file_or_dir_exists(String path)
{
	unreachable("Tried to call file_or_dir_exists without support.");
}

fn bool native_is_dir(String path)
{
	unreachable("Tried to call is_dir without support.");
}

fn bool native_is_file(String path)
{
	unreachable("Tried to call is_file without support.");
}

$endif;

$endif;

$switch (env::OS_TYPE):
$case IOS:
$case MACOSX:
$case TVOS:
$case WATCHOS:
extern fn NativeDirentry* readdir(void*) @extern("readdir$INODE64");
struct NativeDirentry
{
	usz ino;
	usz seekoff;
	ushort reclen;
	ushort namelen;
	char type;
	char[1024] name;
}
$case LINUX:
extern fn NativeDirentry* readdir(void*);
struct NativeDirentry
{
	usz ino;
	isz seekoff;
	ushort reclen;
	char type;
	char[*] name;
}
$default:
// Fix this as we go along.
extern fn NativeDirentry* readdir(void*);
struct NativeDirentry
{
	usz ino;
	isz seekoff;
	ushort reclen;
	char type;
	char[*] name;
}
$endswitch;
