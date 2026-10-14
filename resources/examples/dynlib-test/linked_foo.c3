module linked_foo;
import std::core::string, std::os::env;

interface Hello { fn String hello(); }

struct Foo (Hello) { int x; }

fn String Foo.hello(&self) @dynamic => "hello from Foo";

// The environment is shared by all images in the process, so it can log the order of the @init calls.
fn void record(String step) => @pool()
{
	String current = env::tget_var("LINKED_ORDER") ?? "";
	(void)env::set_var("LINKED_ORDER", string::tformat("%s%s ", current, step));
}
