module std::time::os @if(env::EMSCRIPTEN);

extern fn double emscripten_get_now() @callconv("cdecl");

fn Time native_timestamp()
{
	return (Time)(emscripten_get_now() * 1000.0);
}

fn Clock native_clock()
{
	return (Clock)(emscripten_get_now() * 1000000.0);
}
