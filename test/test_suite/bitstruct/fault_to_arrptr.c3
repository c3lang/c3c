// #target: linux-x64
import libc;
fn void a()
{
	fault x = f; // #error:  'f' could not be found
}

fn void b()
{
	ulong z = (ulong); // #error: Implicitly casting 'typeid' to 'ulong' is not permitted
}



fn void c()
{
	fault x;
	ulong z = (Sigset_t*)(x); // #error:  Implicitly casting 'Sigset_t*' (ulong[16]*) to 'ulong'
}

