module std::io::os;

enum NativeSystemDir
{
	DESKTOP,
	DOCUMENTS,
	VIDEOS,
	MUSIC,
	DOWNLOADS,
	PICTURES,
	TEMPLATES,
	PUBLIC_SHARE,
	SAVED_GAMES,
	SCREENSHOTS
}

module std::io::os @if(env::LIBC);
import std::io, std::os;

fn String? win32_get_known_folder_temp(Win32_REFKNOWNFOLDERID rfid) @private @if(env::WIN32)
{
	Win32_PWSTR path;
	Win32_HRESULT res = win32::shGetKnownFolderPath(rfid, 0x00008000 /* KF_FLAG_CREATE */, null, &path);
	if (res) return io::PATH_COULD_NOT_BE_FOUND?;
	return string::from_wstring(tmem, (WString)path);
}

fn Path? native_home_directory(Allocator allocator) => @pool()
{
	$switch env::OS_TYPE:
		$case IOS:
		$case MACOS:
		$case TVOS:
		$case WATCHOS:
        $case FREEBSD:
        $case KFREEBSD:
		$case LINUX:
        $case NETBSD:
		$case OPENBSD:
		$case HAIKU:
			return path::new(allocator, env::tget_var("HOME")) ?? io::PATH_COULD_NOT_BE_FOUND?;
		$case WIN32:
			return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_PROFILE));
		$default:
			return io::PATH_COULD_NOT_BE_FOUND?;
	$endswitch
}

fn Path? native_user_directory(Allocator allocator, NativeSystemDir dir) => @pool()
{
	$switch env::OS_TYPE:
        $case FREEBSD:
        $case KFREEBSD:
		$case LINUX:
        $case NETBSD:
		$case OPENBSD:
		$case HAIKU:
			switch (dir)
			{
				case DESKTOP: return path::new(allocator, posix::xdg_user_dir_lookup(tmem, "DESKTOP"));
				case DOWNLOADS: return path::new(allocator, posix::xdg_user_dir_lookup(tmem, "DOWNLOAD"));
				case DOCUMENTS: return path::new(allocator, posix::xdg_user_dir_lookup(tmem, "DOCUMENTS"));
				case MUSIC: return path::new(allocator, posix::xdg_user_dir_lookup(tmem, "MUSIC"));
				case VIDEOS: return path::new(allocator, posix::xdg_user_dir_lookup(tmem, "VIDEOS"));
				case PICTURES: return path::new(allocator, posix::xdg_user_dir_lookup(tmem, "PICTURES"));
				case PUBLIC_SHARE: return path::new(allocator, posix::xdg_user_dir_lookup(tmem, "PUBLICSHARE"));
				case TEMPLATES: return path::new(allocator, posix::xdg_user_dir_lookup(tmem, "TEMPLATES"));
				case SAVED_GAMES:
				case SCREENSHOTS: nextcase;
				default: return io::PATH_COULD_NOT_BE_FOUND?;
			}
		$case IOS:
		$case MACOS:
		$case WATCHOS:
		$case TVOS:
			switch (dir)
			{
				case DESKTOP: return path::new(allocator, darwin::find_first_directory_temp(DESKTOP, USER));
				case DOWNLOADS: return path::new(allocator, darwin::find_first_directory_temp(DOWNLOADS, USER));
				case DOCUMENTS: return path::new(allocator, darwin::find_first_directory_temp(DOCUMENT, USER));
				case MUSIC: return path::new(allocator, darwin::find_first_directory_temp(MUSIC, USER));
				case VIDEOS: return path::new(allocator, darwin::find_first_directory_temp(MOVIES, USER));
				case PICTURES: return path::new(allocator, darwin::find_first_directory_temp(PICTURES, USER));
				case PUBLIC_SHARE: return path::new(allocator, darwin::find_first_directory_temp(SHARED_PUBLIC, USER));
				case SAVED_GAMES:
				case SCREENSHOTS:
				case TEMPLATES: nextcase;
				default: return io::PATH_COULD_NOT_BE_FOUND?;
			}
		$case WIN32:
			switch (dir)
			{
				case DOWNLOADS: return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_DOWNLOADS));
				case DOCUMENTS: return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_DOCUMENTS));
				case DESKTOP: return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_DESKTOP));
				case MUSIC: return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_MUSIC));
				case VIDEOS: return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_VIDEOS));
				case PICTURES: return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_PICTURES));
				case SAVED_GAMES: return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_SAVED_GAMES));
				case SCREENSHOTS: return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_SCREENSHOTS));
				case TEMPLATES: return path::new(allocator, win32_get_known_folder_temp(&win32::FOLDERID_TEMPLATES));
				case PUBLIC_SHARE: nextcase;
				default: return io::PATH_COULD_NOT_BE_FOUND?;
			}
		$default:
			return io::PATH_COULD_NOT_BE_FOUND?;
	$endswitch
}


fn Path? native_temp_directory(Allocator allocator) @if(!env::WIN32)
{
	foreach (String env : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" })
	{
		String tmpdir = env::tget_var(env) ?? "";
		if (tmpdir) return path::new(allocator, tmpdir);
	}
	return path::new(allocator, "/tmp");
}

fn Path? native_temp_directory(Allocator allocator) @if(env::WIN32) => @pool()
{
	Win32_DWORD len = win32::getTempPathW(0, null);
	if (!len) return io::GENERAL_ERROR?;
	Char16[] buff = mem::talloc_array(Char16, len + (usz)1);
	if (!win32::getTempPathW(len, buff)) return io::GENERAL_ERROR?;
	return path::new(allocator, string::tfrom_utf16(buff[:len]));
}

module std::io::os @if(env::NO_LIBC);
import std::io::path;

macro Path? native_home_directory(Allocator allocator) => io::PATH_COULD_NOT_BE_FOUND?;
macro Path? native_temp_directory(Allocator allocator) => io::PATH_COULD_NOT_BE_FOUND?;
fn Path? native_user_directory(Allocator allocator, NativeSystemDir dir) => io::PATH_COULD_NOT_BE_FOUND?;
