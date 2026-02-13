module ratio_benchmark;
import std;

fn int main(String[] args)
{
	if (args.len < 2)
	{
		io::printfn("Usage: %s <file>", args[0]);
		return 1;
	}

	io::printfn("%s%-35s | %10s | %19s | %19s%s", Ansi.BOLD, "Test Case", "Original", "C3", "7z (Gzip/mx9)", Ansi.RESET);
	io::printfn("--------------------------------------------------------------------------------------------");

	char[] redundant = allocator::alloc_array(tmem, char, 100000);
	mem::set(redundant.ptr, 'A', 100000);
	run_test("All A's (100k)", redundant);

	DString json_data;
	for (int i = 0; i < 1000; i++) {
		json_data.appendf("{\"id\": %d, \"type\": \"user\", \"active\": true}, ", i);
	}
	run_test("JSON Array", json_data.str_view());

	String text = "The quick brown fox jumps over the lazy dog. ";
	DString long_text;
	for (int i = 0; i < 1000; i++)
	{
		long_text.append(text);
	}
	run_test("English Text (Repetitive)", long_text.str_view());

	Path p = path::new(tmem, "lib/std/compression/deflate.c3")!!;
	if (try data = file::load_path(tmem, p)) {
		run_test("deflate.c3", data);
	}

	Path bin_p1 = path::new(tmem, args[0])!!;
	if (try bin_data = file::load_path(tmem, bin_p1)) {
		run_test("this binary", bin_data);
	}

	DString noise;
	for (int i = 0; i < 50000; i++) {
		noise.append(rand('z' - 'a' + 1));
	}
	run_test("Random Noise (50k)", noise.str_view());

	Path bin_p2 = path::new(tmem, args[1])!!;
	if (try bin_data = file::load_path(tmem, bin_p2)) {
		run_test(string::tformat("(%s)", bin_p2.str_view()), bin_data);
	}
	return 0;
}

struct TestResult
{
	String name;
	usz original_size;
	usz c3_size;
	usz zlib_size;
}

fn void? run_external_zlib(char[] input, usz *size)
{
	Path temp_in = path::temp_directory(tmem).tappend("ratio_test_in")!;
	Path temp_out = path::temp_directory(tmem).tappend("ratio_test_out.gz")!;
	file::save(temp_in.str_view(), input)!;

	String[] cmd = { "7z", "a", "-tgzip", "-mx=9", temp_out.str_view(), temp_in.str_view() };
	SubProcess sub = process::create(cmd, { .search_user_path = true })!;
	if (sub.join()! != 0) return io::GENERAL_ERROR~;

	char[] compressed = file::load_path(tmem, temp_out)!;
	*size = compressed.len;
	return;
}

fn void run_test(String name, char[] data)
{
	usz c3_size = deflate::compress(data, tmem)!!.len;
	usz zlib_size;
	run_external_zlib(data, &zlib_size)!!;

	double c3_ratio = (double)c3_size / data.len * 100.0;
	double zlib_ratio = (double)zlib_size / data.len * 100.0;

	io::printfn("%-35s | %10d | %10d (%5.2f%%) | %10d (%5.2f%%)",
		name[:(min(name.len, 35))], data.len, c3_size, c3_ratio, zlib_size, zlib_ratio);
}