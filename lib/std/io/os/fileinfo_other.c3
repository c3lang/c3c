module std::io::file::os;

const USE_DARWIN_INODE64 = env::os_is_darwin() && env::ARCH_TYPE == X86_64;

extern fn NativeDirentry* readdir(void*) @extern("readdir") @if(!USE_DARWIN_INODE64) ;
extern fn NativeDirentry* readdir(void*) @extern("readdir$INODE64") @if(USE_DARWIN_INODE64);

struct NativeDirentry
{
	usz ino;
	usz seekoff @if(env::os_is_darwin());
	isz seekoff @if(!env::os_is_darwin());
	ushort reclen;
	ushort namelen @if(env::os_is_darwin());
	char type;
	char[1024] name @if(env::os_is_darwin());
	char[*] name @if(!env::os_is_darwin());
}

module std::io::file::os @if(!env::os_is_win32());

// native_temp_directory, for non Win32

fn Path! native_temp_directory(Allocator* using = mem::heap())
{
	foreach (String env : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" })
	{
		String tmpdir = env::get_var(env) ?? "";
		if (tmpdir) return path::new(tmpdir, using);
	}
	return path::new("/tmp", using);
}

module std::io::file::os @if(env::COMPILER_LIBC_AVAILABLE && !env::os_is_win32());

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

module std::io::file::os @if(!env::os_is_darwin() && !env::os_is_win32());

fn usz! native_file_size(String path)
{
	File f = file::open(path, "r")!;
	defer (void)f.close();
	return f.seek(0, Seek.END)!;
}

fn bool native_file_or_dir_exists(String path)
{
	$if POSIX_LIBC:
		@pool()
		{
			return os::access(path.zstr_tcopy(), 0 /* F_OK */) != -1;
		};
	$else
		unreachable("Tried to call file_or_dir_exists without support.");
		return false;
	$endif
}

fn bool native_is_file(String path)
{
	$if POSIX_LIBC:
		File! f = file::open(path, "r");
		defer (void)f.close();
		return @ok(f);
	$else
		unreachable("Tried to call is_file without support.");
		return false;
	$endif
}

fn bool native_is_dir(String path)
{
	$if POSIX_LIBC:
		return native_file_or_dir_exists(path) && !native_is_file(path);
	$else
		unreachable("Tried to call is_dir without support.");
		return false;
	$endif
}

