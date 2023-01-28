module std::core::main_stub;

private macro usz _strlen(ptr)
{
	usz len = 0;
	while (ptr[len]) len++;
	return len;
}

macro int @main_to_err_main(#m, int, char**) => catch(#m()) ? 1 : 0;
macro int @main_to_int_main(#m, int, char**) => #m();
macro int @main_to_void_main(#m, int, char**)
{
	#m();
	return 0;
}

private macro String[] args_to_strings(int argc, char** argv)
{
	String *list = malloc(String.sizeof * argc);
	for (int i = 0; i < argc; i++)
	{
		char* arg = argv[i];
		usz len = 0;
		list[i] = arg[:_strlen(arg)];
	}
	return list[:argc];
}


macro int @main_to_err_main_args(#m, int argc, char** argv)
{
	String[] list = args_to_strings(argc, argv);
	defer free(list.ptr);
	return catch(#m(list)) ? 1 : 0;
}

macro int @main_to_int_main_args(#m, int argc, char** argv)
{
	String[] list = args_to_strings(argc, argv);
	defer free(list.ptr);
	return #m(list);
}

macro int @main_to_void_main_args(#m, int argc, char** argv)
{
	String[] list = args_to_strings(argc, argv);
	defer free(list.ptr);
	#m(list);
	return 0;
}

$if (env::OS_TYPE == OsType.WIN32):

extern fn Char16** _win_command_line_to_argv_w(ushort* cmd_line, int* argc_ptr) @extname("CommandLineToArgvW");

private macro String[] win_command_line_to_strings(ushort* cmd_line)
{
	int argc;
	Char16** argv = _win_command_line_to_argv_w(cmd_line, &argc);
	return wargs_strings(argc, argv);
}

private macro String[] wargs_strings(int argc, Char16** argv)
{
	String *list = malloc(String.sizeof * argc);
	for (int i = 0; i < argc; i++)
	{
		Char16* arg = argv[i];
		Char16[] argstring = arg[:_strlen(arg)];
		list[i] = str::utf16to8(argstring) ?? str::copy("?");
	}
	return list[:argc];
}

private macro void release_wargs(String[] list)
{
	foreach (s : list) free(s.ptr);
	free(list.ptr);
}

macro int @win_to_err_main_noargs(#m, void* handle, Char16* cmd_line, int show_cmd) => catch(#m()) ? 1 : 0;
macro int @win_to_int_main_noargs(#m, void* handle, Char16* cmd_line, int show_cmd) => #m();
macro int @win_to_void_main_noargs(#m, void* handle, Char16* cmd_line, int show_cmd)
{
	#m();
	return 0;
}

macro int @win_to_err_main_args(#m, void* handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
    return catch(#m(args)) ? 1 : 0;
}

macro int @win_to_int_main_args(#m, void* handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
    return #m(args);
}

macro int @win_to_void_main_args(#m, void* handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	#m(args);
	return 0;
}

macro int @win_to_err_main(#m, void* handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
    return catch(#m(handle, args, show_cmd)) ? 1 : 0;
}

macro int @win_to_int_main(#m, void* handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
    return #m(handle, args, show_cmd);
}

macro int @win_to_void_main(#m, void* handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	#m(handle, args, show_cmd);
	return 0;
}

macro int @wmain_to_err_main_args(#m, int argc, Char16** argv)
{
	String[] args = wargs_strings(argc, argv);
	defer release_wargs(args);
    return catch(#m(args)) ? 1 : 0;
}

macro int @wmain_to_int_main_args(#m, int argc, Char16** argv)
{
	String[] args = wargs_strings(argc, argv);
	defer release_wargs(args);
    return #m(args);
}

macro int @wmain_to_void_main_args(#m, int argc, Char16** argv)
{
	String[] args = wargs_strings(argc, argv);
	defer release_wargs(args);
	#m(args);
	return 0;
}

$endif;