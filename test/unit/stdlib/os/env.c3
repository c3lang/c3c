module std::os::env @test;

 fn void set_get_unset()
 {
 	const NAME = "C3_TEST_ENVVAR";
 	const VALUE = "foobar";

 	env::set_var(NAME, VALUE);
 	String v = env::get_var(NAME)!!;
 	assert(v == VALUE, "got %s; want %s", v, VALUE);

 	env::clear_var(NAME);
 	if (try env::get_var(NAME))
 	{
 	    unreachable("environment variable should no longer exist");
 	}
 }