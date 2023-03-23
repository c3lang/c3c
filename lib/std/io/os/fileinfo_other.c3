module std::io::file::os;

// native_temp_directory, for non Win32
$if (!env::os_is_win32())

fn Path! native_temp_directory(Allocator* using = mem::heap())
{
	foreach (String env : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" })
	{
		String tmpdir = env::get_var(env) ?? "";
		if (tmpdir) return path::new(tmpdir, using);
	}
	return path::new("/tmp", using);
}

$if (env::COMPILER_LIBC_AVAILABLE)

extern fn void* opendir(ZString);
extern fn void closedir(void*);
extern fn int remove(ZString);

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
	if (!directory) return (path::is_dir(dir) ? IoError.CANNOT_READ_DIR : IoError.FILE_NOT_DIR)?;
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

/**
 * @require dir.as_str()
 **/
fn void! native_rmtree(Path dir)
{
	void* directory = opendir(dir.as_zstr());
	defer if (directory) closedir(directory);
	if (!directory) return path::is_dir(dir) ? IoError.CANNOT_READ_DIR? : IoError.FILE_NOT_DIR?;
	NativeDirentry* entry;
	while ((entry = readdir(directory)))
	{
		@pool()
		{
			String name = ((ZString)&entry.name).as_str();
            if (!name || name == "." || name == "..") continue;
            Path new_path = dir.tappend(name)!;
            if (entry.type == DT_DIR)
            {
            	native_rmtree(new_path)!;
            	continue;
            }
            if (remove(new_path.as_zstr()))
            {
            	// TODO improve
            	return IoError.GENERAL_ERROR?;
            }
		};
	}
	os::native_rmdir(dir)!;
}

$endif

$endif

$if (!env::os_is_darwin() && !env::os_is_win32())

fn usz! native_file_size(String path)
{
	File f = file::open(path, "r")!;
	defer (void)f.close();
	return f.seek(0, Seek.END)!;
}

$if (env::os_is_posix() && env::COMPILER_LIBC_AVAILABLE)

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
	return @ok(f);
}

fn bool native_is_dir(String path)
{
	return native_file_or_dir_exists(path) && !native_is_file(path);
}

$else

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

$endif

$endif

$switch (env::OS_TYPE)
$case IOS:
$case MACOS:
$case TVOS:
$case WATCHOS:

$if (env::ARCH_TYPE == X86_64)
extern fn NativeDirentry* readdir(void*) @extern("readdir$INODE64");
$else
extern fn NativeDirentry* readdir(void*) @extern("readdir");
$endif

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
$endswitch
