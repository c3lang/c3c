module std::os;
import libc;

<*
 Exit the process with a given exit code. This will typically call 'exit' in LibC
*>
fn void exit(int result, bool cleanup = true) @weak
{
	$if env::LIBC:
		libc::exit(result);
	$else
		$$trap();
	$endif
}

<*
 Exit the process with a given exit code. This will typically call '_Exit' in LibC
 usually bypassing '@finalizer' functions.
*>
fn void fastexit(int result, bool cleanup = true) @weak
{
	$if env::LIBC:
		libc::_exit(result);
	$else
		$$trap();
	$endif
}
