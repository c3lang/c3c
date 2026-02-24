// 1. `gcc -O3 -c dependencies/miniz/miniz.c -o build/miniz.o`
// 2. `build/c3c -O3 compile-run test/compression/deflate_benchmark.c3 build/miniz.o`

module deflate_benchmark;
import std, std::time::clock;

const int AMOUNT_OF_WORK = 10; // Increase this to scale test data sizes

fn int main(String[] args)
{
	io::printf("\n%s%s DEFLATE BENCHMARK %s", Ansi.BOLD, Ansi.BG_CYAN, Ansi.RESET);
	io::printfn(" Comparing C3 std::compression::deflate with miniz (in-process)\n");
	io::printfn(" Work Scale: %dx\n", AMOUNT_OF_WORK);

	io::printfn("%-26s | %7s | %7s | %7s | %7s | %-10s", "Test Case", "C3 Rat.", "Miz Rat.", "C3 MB/s", "Miz MB/s", "Winner");
	io::printfn("---------------------------+---------+---------+---------+---------+-----------");

	// Test 1: Redundant data
	usz redundant_size = 10_000_000 * (usz)AMOUNT_OF_WORK;
	char[] redundant = allocator::alloc_array(tmem, char, redundant_size);
	mem::set(redundant.ptr, 'A', redundant_size);
	run_bench(string::tformat("Redundant (%dMB 'A')", (int)(redundant_size / 1_000_000)), redundant);

	// Test 2: Large Source Project (All .c files in src/compiler)
	DString project_src;
	Path src_dir = path::new(tmem, "src/compiler")!!;
	PathList? compiler_files = path::ls(tmem, src_dir);
	if (try files = compiler_files) {
		for (int i = 0; i < AMOUNT_OF_WORK; i++) {
			foreach (p : files) {
				if (p.basename().ends_with(".c")) {
					Path full_p = src_dir.tappend(p.str_view())!!;
					if (try data = file::load_path(tmem, full_p)) {
						project_src.append(data);
					}
				}
			}
		}
	}
	run_bench("Compiler Source (Bulk)", project_src.str_view());

	// Test 3: Standard Library (All .c3 files in lib/std)
	DString std_src;
	for (int i = 0; i < AMOUNT_OF_WORK; i++) {
		collect_files(path::new(tmem, "lib/std")!!, ".c3", &std_src);
	}
	run_bench("Stdlib Source (Bulk)", std_src.str_view());

	// Test 4: Log Files (Simulated)
	DString log_data;
	for (int i = 0; i < 50_000 * AMOUNT_OF_WORK; i++) {
		log_data.appendf("2024-02-13 21:30:%02d.%03d [INFO] Connection established from 192.168.1.%d\n", i % 60, i % 1000, i % 255);
		log_data.appendf("2024-02-13 21:30:%02d.%03d [DEBUG] Buffer size: %d bytes\n", i % 60, i % 1000, (i * 123) % 4096);
	}
	run_bench("Log Files (Simulated)", log_data.str_view());

	// Test 5: Web Content (Simulated HTML/CSS)
	DString web_data;
	web_data.append("<!DOCTYPE html><html><head><style>.item { color: red; margin: 10px; }</style></head><body>");
	for (int i = 0; i < 1000 * AMOUNT_OF_WORK; i++) {
		web_data.appendf("<div class='item' id='obj%d'>", i);
		web_data.append("<h1>Title of the item</h1><p>This is some repetitive descriptive text that might appear on a web page.</p>");
		web_data.append("<ul><li>Feature 1</li><li>Feature 2</li><li>Feature 3</li></ul></div>");
	}
	web_data.append("</body></html>");
	run_bench("Web Content (Simulated)", web_data.str_view());

	// Test 6: CSV Data (Simulated)
	DString csv_data;
	csv_data.append("id,name,value1,value2,status,category\n");
	for (int i = 0; i < 20_000 * AMOUNT_OF_WORK; i++) {
		csv_data.appendf("%d,Product_%d,%d.5,%d,\"%s\",\"%s\"\n", 
			i, i % 100, i * 10, i % 500, 
			i % 3 == 0 ? "Active" : "Inactive", 
			i % 5 == 0 ? "Electronics" : "Home");
	}
	run_bench("CSV Data (Simulated)", csv_data.str_view());

	// Test 7: Binary Data (Structured)
	usz binary_size = 2_000_000 * (usz)AMOUNT_OF_WORK;
	char[] binary = allocator::alloc_array(tmem, char, binary_size);
	for (usz i = 0; i < binary.len; i += 4) {
		uint val = (uint)i ^ 0xDEADBEEF;
		mem::copy(binary.ptr + i, &val, 4);
	}
	run_bench("Binary Data (Structured)", binary);

	// Test 8: Random Noise (1MB)
	usz noise_size = 1_000_000 * (usz)AMOUNT_OF_WORK;
	DString noise;
	for (usz i = 0; i < noise_size; i++) {
		noise.append((char)rand('z' - 'a' + 1) + 'a');
	}
	run_bench("Random Noise (Scaled)", noise.str_view());

	// Test 9: Tiny File (Check overhead)
	run_bench("Tiny File (asd.c3)", "module asd; fn void main() {}\n");

	// Test 10: Natural Language (Repetitive)
	String text = "The quick brown fox jumps over the lazy dog. ";
	DString long_text;
	for (int i = 0; i < 50_000 * AMOUNT_OF_WORK; i++) long_text.append(text);
	run_bench("Natural Text (Scaled)", long_text.str_view());

	if (args.len > 1) {
		Path custom_p = path::new(tmem, args[1])!!;
		if (try custom_data = file::load_path(tmem, custom_p)) {
			run_bench(string::tformat("Custom: %s", custom_p.basename()), custom_data);
		}
	}
	// Final Summary
	double avg_c3 = totals.c3_speed_sum / totals.count;
	double avg_miniz = totals.miniz_speed_sum / totals.count;
	double total_factor = avg_c3 / avg_miniz;

	io::printfn("\n%sOVERALL SUMMARY%s", Ansi.BOLD, Ansi.RESET);
	io::printfn("  Average Throughput C3:    %8.1f MB/s", avg_c3);
	io::printfn("  Average Throughput Miniz: %8.1f MB/s", avg_miniz);
	io::printfn("  %sC3 is %.1fx faster on average!%s\n", Ansi.BOLD, total_factor, Ansi.RESET);

	return 0;
}

struct BenchResult {
	long time_ns;
	usz size;
	double ratio;
	double throughput_mbs;
}

struct BenchTotal {
	double c3_speed_sum;
	double miniz_speed_sum;
	int count;
}
BenchTotal totals;

fn void run_bench(String title, char[] data)
{
	// C3 Bench
	Clock start = clock::now();
	char[] c3_compressed = deflate::compress(data, tmem)!!;
	Clock end = clock::now();
	BenchResult c3 = calculate_metrics(data.len, (long)(end - start), c3_compressed.len);

	// Miniz Bench
	usz miniz_size;
	start = clock::now();
	void* miniz_ptr = tdefl_compress_mem_to_heap(data.ptr, data.len, &miniz_size, MINIZ_FLAGS);
	end = clock::now();
	BenchResult miniz = calculate_metrics(data.len, (long)(end - start), miniz_size);
	if (miniz_ptr) mz_free(miniz_ptr);

	// Performance Delta
	double speed_factor = c3.throughput_mbs / miniz.throughput_mbs;
	
	io::printf("%-26s | %6.2f%% | %6.2f%% | %7.1f | %7.1f | %s%s (%.1fx)%s\n",
		title[:(min(title.len, 26))],
		c3.ratio, miniz.ratio,
		c3.throughput_mbs, miniz.throughput_mbs,
		speed_factor > 1.0 ? Ansi.CYAN : Ansi.WHITE,
		speed_factor > 1.0 ? "C3" : "Miniz",
		speed_factor > 1.0 ? speed_factor : 1.0 / speed_factor,
		Ansi.RESET);

	totals.c3_speed_sum += c3.throughput_mbs;
	totals.miniz_speed_sum += miniz.throughput_mbs;
	totals.count++;
}

fn void collect_files(Path dir, String suffix, DString* out)
{
	PathList? items = path::ls(tmem, dir);
	if (catch items) return;
	foreach (p : items) {
		Path full = dir.tappend(p.str_view())!!;
		if (path::is_dir(full)) {
			if (p.basename() != "." && p.basename() != "..") {
				collect_files(full, suffix, out);
			}
		} else if (p.basename().ends_with(suffix)) {
			if (try data = file::load_path(tmem, full)) {
				out.append(data);
			}
		}
	}
}

fn BenchResult calculate_metrics(usz original_len, long time_ns, usz compressed_len)
{
	BenchResult res;
	res.time_ns = time_ns;
	res.size = compressed_len;
	res.ratio = (double)compressed_len / (double)original_len * 100.0;
	res.throughput_mbs = (double)original_len / (1024.0 * 1024.0) / ((double)time_ns / 1_000_000_000.0);
	return res;
}

// External Miniz bindings
extern fn void* tdefl_compress_mem_to_heap(void* pSrc_buf, usz src_buf_len, usz* pOut_len, int flags);
extern fn void mz_free(void* p);

const int TDEFL_GREEDY_PARSING_FLAG = 0x04000;
const int TDEFL_NONDETERMINISTIC_PARSING_FLAG = 0x08000; // Fastest init for miniz for a fair comparisson
const int C3_EQUIVALENT_PROBES = 16; // C3 uses MAX_CHAIN = 16 as default (this should be exposed)

const int MINIZ_FLAGS = C3_EQUIVALENT_PROBES | TDEFL_GREEDY_PARSING_FLAG | TDEFL_NONDETERMINISTIC_PARSING_FLAG;
