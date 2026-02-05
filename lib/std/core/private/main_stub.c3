module std::core::main_stub;

macro usz _strlen(ptr) @private
{
	usz len = 0;
	while (ptr[len]) len++;
	return len;
}

macro int @main_no_args(#m, int, char**)
{
	$if $typeof(#m()) == void:
		#m();
		return 0;
	$else
		return #m();
	$endif
}

macro int @main_args(#m, int argc, char** argv)
{
	String[] list = args_to_strings(argc, argv);
	defer free(list.ptr);
	$if $typeof(#m(list)) == void:
		#m(list);
		return 0;
	$else
		return #m(list);
	$endif
}

macro String[] args_to_strings(int argc, char** argv) @private
{
	String[] list = mem::alloc_array(String, argc);
	for (int i = 0; i < argc; i++)
	{
		char* arg = argv[i];
		usz len = 0;
		list[i] = (String)arg[:_strlen(arg)];
	}
	return list;
}


macro int @_main_runner(#m, int argc, char** argv)
{
	String[] list = args_to_strings(argc, argv);
	defer free(list.ptr);
	return #m(list) ? 0 : 1;
}

module std::core::main_stub @if(env::WIN32);
import std::os::win32;

macro win32_set_utf8_codepage() @local
{
	// By default windows uses an OEM codepage that differs based on locale
	// and does not support printing utf-8 characters. This allows both
	// printing utf-8 characters from strings and reading them from stdin.
	win32::setConsoleCP(UTF8);
	win32::setConsoleOutputCP(UTF8);
}


extern fn Char16** _win_command_line_to_argv_w(ushort* cmd_line, int* argc_ptr) @cname("CommandLineToArgvW");

macro String[] win_command_line_to_strings(ushort* cmd_line) @private
{
	int argc;
	Char16** argv = _win_command_line_to_argv_w(cmd_line, &argc);
	return wargs_strings(argc, argv);
}

macro String[] wargs_strings(int argc, Char16** argv) @private
{
	String[] list = mem::alloc_array(String, argc);
	for (int i = 0; i < argc; i++)
	{
		Char16* arg = argv[i];
		Char16[] argstring = arg[:_strlen(arg)];
		list[i] = string::from_utf16(mem, argstring) ?? "?".copy(mem);
	}
	return list[:argc];
}

macro void release_wargs(String[] list) @private
{
	foreach (s : list) free(s.ptr);
	free(list.ptr);
}

macro int @win_main_no_args(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	win32_set_utf8_codepage();
	$if $typeof(#m()) == void:
		#m();
		return 0;
	$else
		return #m();
	$endif
}

macro int @win_main_args(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	win32_set_utf8_codepage();
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	$if $typeof(#m(args)) == void:
		#m(args);
		return 0;
	$else
		return #m(args);
	$endif
}

macro int @win_main(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	win32_set_utf8_codepage();
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	$if $typeof(#m(handle, prev_handle, args, show_cmd)) == void:
		#m(handle, prev_handle, args, show_cmd);
		return 0;
	$else
		return #m(handle, prev_handle, args, show_cmd);
	$endif
}


macro int @wmain_main(#m, int argc, Char16** argv)
{
	win32_set_utf8_codepage();
	String[] args = wargs_strings(argc, argv);
	defer release_wargs(args);
	$if $typeof(#m(args)) == void:
		#m(args);
		return 0;
	$else
		return #m(args);
	$endif
}

macro int @_wmain_runner(#m, int argc, Char16** argv)
{
	win32_set_utf8_codepage();
	String[] args = wargs_strings(argc, argv);
	defer release_wargs(args);
	return #m(args) ? 0 : 1;
}
