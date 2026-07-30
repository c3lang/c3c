module std::io::os @feat(NO_LIBC);
import libc;

alias FopenFn = fn void*?(String, String);
alias FreopenFn = fn void*?(void*, String, String);
alias FcloseFn = fn void?(void*);
alias FseekFn = fn void?(void*, long, SeekOrigin);
alias FtellFn = fn long?(void*);
alias FwriteFn = fn sz?(void*, char[] buffer);
alias FreadFn = fn sz?(void*, char[] buffer);
alias FflushFn = fn void?(void*);
alias RemoveFn = fn void?(String);
alias FputcFn = fn void?(int, void*);
alias SetModifiedTimeFn = fn void?(String, Time_t);

FopenFn native_fopen_fn @weak;
FcloseFn native_fclose_fn @weak;
FreopenFn native_freopen_fn @weak;
FseekFn native_fseek_fn @weak;
FtellFn native_ftell_fn @weak;
FwriteFn native_fwrite_fn @weak;
FreadFn native_fread_fn @weak;
FflushFn native_fflush_fn @weak;
RemoveFn native_remove_fn @weak;
FputcFn native_fputc_fn @weak;
SetModifiedTimeFn native_set_modified_time_fn @weak;

<*
 @require mode.len > 0
 @require filename.len > 0
*>
fn void*? native_fopen(String filename, String mode) @inline
{
	if (native_fopen_fn) return native_fopen_fn(filename, mode);
	return io::UNSUPPORTED_OPERATION~;
}

<*
 Delete a file.

 @require filename.len > 0
*>
fn void? native_remove(String filename) @inline
{
	if (native_remove_fn) return native_remove_fn(filename);
	return io::UNSUPPORTED_OPERATION~;
}

<*
 @require mode.len > 0
 @require filename.len > 0
*>
fn void*? native_freopen(void* file, String filename, String mode) @inline
{
	if (native_freopen_fn) return native_freopen_fn(file, filename, mode);
	return io::UNSUPPORTED_OPERATION~;
}

fn void? native_fseek(void* file, long offset, SeekOrigin whence) @inline
{
	if (native_fseek_fn) return native_fseek_fn(file, offset, whence);
	return io::UNSUPPORTED_OPERATION~;
}

fn long? native_ftell(CFile file) @inline
{
	if (native_ftell_fn) return native_ftell_fn(file);
	return io::UNSUPPORTED_OPERATION~;
}

fn sz? native_fwrite(CFile file, char[] buffer) @inline
{
	if (native_fwrite_fn) return native_fwrite_fn(file, buffer);
	return io::UNSUPPORTED_OPERATION~;
}

fn sz? native_fread(CFile file, char[] buffer) @inline
{
	if (native_fread_fn) return native_fread_fn(file, buffer);
	return io::UNSUPPORTED_OPERATION~;
}

fn void? native_fflush(CFile file) @inline @maydiscard
{
	if (native_fflush_fn) return native_fflush_fn(file);
	return io::UNSUPPORTED_OPERATION~;
}

fn void? native_fputc(CInt c, CFile stream) @inline
{
	if (native_fputc_fn) return native_fputc_fn(c, stream);
	return io::UNSUPPORTED_OPERATION~;
}

fn void? native_set_modified_time(String filename, Time_t time) @inline
{
	if (native_set_modified_time_fn) return native_set_modified_time_fn(filename, time);
	return io::UNSUPPORTED_OPERATION~;
}
