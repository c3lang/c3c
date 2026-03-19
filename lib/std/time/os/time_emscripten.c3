module std::time::os @if(env::WASM && !env::WASI);

extern fn double emscripten_get_now() @callconv("cdecl");

fn Time native_timestamp()
{
	return (Time)(emscripten_get_now() * 1_000.0);
}

fn Clock native_clock()
{
	return (Clock)(emscripten_get_now() * 1_000_000.0);
}
