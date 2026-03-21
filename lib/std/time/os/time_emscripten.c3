module std::time::os @if(env::EMSCRIPTEN);

// High-resolution monotonic timer (ms since page load, like performance.now()).
extern fn double emscripten_get_now() @callconv("cdecl");
// Wall-clock time as ms since Unix epoch (like Date.now()).
extern fn double emscripten_date_now() @callconv("cdecl");

fn Time native_timestamp()
{
	// emscripten_date_now() returns milliseconds since Unix epoch.
	// Convert to microseconds (the unit used by Time).
	return (Time)(emscripten_date_now() * 1000.0);
}

fn Clock native_clock()
{
	return (Clock)(emscripten_get_now() * 1000000.0);
}
