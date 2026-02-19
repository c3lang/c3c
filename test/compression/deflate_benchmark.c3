// 1. `gcc -O3 -c dependencies/miniz/miniz.c -o build/miniz.o`
// 2. `build/c3c -O3 compile-run test/compression/deflate_benchmark.c3 build/miniz.o`

module deflate_benchmark;
import std, std::time::clock, libc;

const int AMOUNT_OF_WORK = 10; // Increase this to scale test data sizes

fn int main(String[] args)
{
	io::printf("\n%s%s DEFLATE BENCHMARK %s", Ansi.BOLD, Ansi.BG_CYAN, Ansi.RESET);
	io::printfn(" Comparing C3 std::compression::deflate with miniz (in-process)\n");
	io::printfn(" Work Scale: %dx\n", AMOUNT_OF_WORK);

	List{DataSetResult} all_results;
	all_results.tinit();

	// Test 1: Redundant data
	usz redundant_size = 10_000_000 * (usz)AMOUNT_OF_WORK;
	char[] redundant = allocator::alloc_array(tmem, char, redundant_size);
	mem::set(redundant.ptr, 'A', redundant_size);
	all_results.push(run_bench(string::tformat("Redundant (%dMB 'A')", (int)(redundant_size / 1_000_000)), redundant));

	// Test 2
	DString project_src;
	Path src_dir = path::new(tmem, "src/compiler")!!;
	if (try files = path::ls(tmem, src_dir)) {
		for (int i = 0; i < AMOUNT_OF_WORK; i++) {
			foreach (p : files) {
				if (p.basename().ends_with(".c")) {
					if (try data = file::load_path(tmem, src_dir.tappend(p.str_view())!!)) project_src.append(data);
				}
			}
		}
	}
	all_results.push(run_bench("Compiler Source (Bulk)", project_src.str_view()));

	// Test 3
	DString std_src;
	for (int i = 0; i < AMOUNT_OF_WORK; i++) collect_files(path::new(tmem, "lib/std")!!, ".c3", &std_src);
	all_results.push(run_bench("Stdlib Source (Bulk)", std_src.str_view()));

	// Test 4
	DString log_data;
	for (int i = 0; i < 50_000 * AMOUNT_OF_WORK; i++) {
		log_data.appendf("2024-02-13 21:30:%02d.%03d [INFO] Connection established from 192.168.1.%d\n", i % 60, i % 1000, i % 255);
		log_data.appendf("2024-02-13 21:30:%02d.%03d [DEBUG] Buffer size: %d bytes\n", i % 60, i % 1000, (i * 123) % 4096);
	}
	all_results.push(run_bench("Log Files (Simulated)", log_data.str_view()));

	// Test 5
	DString web_data;
	web_data.append("<!DOCTYPE html><html><head><style>.item { color: red; margin: 10px; }</style></head><body>");
	for (int i = 0; i < 1000 * AMOUNT_OF_WORK; i++) {
		web_data.appendf("<div class='item' id='obj%d'>", i);
		web_data.append("<h1>Title of the item</h1><p>This is some repetitive descriptive text that might appear on a web page.</p>");
		web_data.append("<ul><li>Feature 1</li><li>Feature 2</li></ul></div>");
	}
	web_data.append("</body></html>");
	all_results.push(run_bench("Web Content (Simulated)", web_data.str_view()));

	// Test 6
	DString csv_data;
	csv_data.append("id,name,value1,value2,status,category\n");
	for (int i = 0; i < 20_000 * AMOUNT_OF_WORK; i++) {
		csv_data.appendf("%d,Product_%d,%d.5,%d,\"%s\",\"%s\"\n", i, i % 100, i * 10, i % 500, i % 3 == 0 ? "Active" : "Inactive", i % 5 == 0 ? "Electronics" : "Home");
	}
	all_results.push(run_bench("CSV Data (Simulated)", csv_data.str_view()));

	// Test 7
	usz binary_size = 2_000_000 * (usz)AMOUNT_OF_WORK;
	char[] binary = allocator::alloc_array(tmem, char, binary_size);
	for (usz i = 0; i < binary.len; i += 4) { uint val = (uint)i ^ 0xDEADBEEF; mem::copy(binary.ptr + i, &val, 4); }
	all_results.push(run_bench("Binary Data (Structured)", binary));

	// Test 8
	usz noise_size = 1_000_000 * (usz)AMOUNT_OF_WORK;
	DString noise;
	for (usz i = 0; i < noise_size; i++) noise.append((char)rand('z' - 'a' + 1) + 'a');
	all_results.push(run_bench("Random Noise (Scaled)", noise.str_view()));

	// Test 9
	all_results.push(run_bench("Tiny File (asd.c3)", "module asd; fn void main() {}\n"));

	// Test 10
	DString long_text;
	for (int i = 0; i < 50_000 * AMOUNT_OF_WORK; i++) long_text.append("The quick brown fox jumps over the lazy dog. ");
	all_results.push(run_bench("Natural Text (Scaled)", long_text.str_view()));

	if (args.len > 1) {
		if (try custom_data = file::load_path(tmem, path::new(tmem, args[1])!!)) {
			all_results.push(run_bench(string::tformat("Custom: %s", path::new(tmem, args[1])!!.basename()), custom_data));
		}
	}

	// --- PRINT COMPRESSION TABLE ---
	io::printfn("\n%s COMPRESSION PERFORMANCE %s", Ansi.BOLD, Ansi.RESET);
	io::printfn("%-26s | %7s | %7s | %10s | %10s | %10s | %10s", "Test Case", "C3 Rat.", "Mz Rat.", "C3 Mem", "C3 Stream", "Mz Mem", "Mz Stream");
	io::printfn("---------------------------+---------+---------+------------+------------+------------+------------");
	foreach (res : all_results) {
		io::printf("%-26s | %6.2f%% | %6.2f%% | %10.1f | %10.1f | %10.1f | %10.1f\n",
			res.title[:(min(res.title.len, 26))],
			res.c3c.ratio, res.mzc.ratio,
			res.c3c.throughput_mbs, res.c3cs.throughput_mbs, res.mzc.throughput_mbs, res.mzcs.throughput_mbs);
	}

	// --- PRINT DECOMPRESSION TABLE ---
	io::printfn("\n%s DECOMPRESSION PERFORMANCE %s", Ansi.BOLD, Ansi.RESET);
	io::printfn("%-26s | %10s | %10s | %10s | %10s", "Test Case", "C3 Mem", "C3 Stream", "Mz Mem", "Mz Stream");
	io::printfn("---------------------------+------------+------------+------------+------------");
	foreach (res : all_results) {
		io::printf("%-26s | %10.1f | %10.1f | %10.1f | %10.1f\n",
			res.title[:(min(res.title.len, 26))],
			res.c3d.throughput_mbs, res.c3ds.throughput_mbs, res.mzd.throughput_mbs, res.mzds.throughput_mbs);
	}
	double avg_c3 = totals.c3_speed_sum / totals.count;
	double avg_c3s = totals.c3s_speed_sum / totals.count;
	double avg_mz = totals.mz_speed_sum / totals.count;
	double avg_mzs = totals.mzs_speed_sum / totals.count;
	double avg_c3d = totals.c3d_speed_sum / totals.count;
	double avg_c3ds = totals.c3ds_speed_sum / totals.count;
	double avg_mzd = totals.mzd_speed_sum / totals.count;
	double avg_mzds = totals.mzds_speed_sum / totals.count;

	io::printfn("\n%sOVERALL SUMMARY%s", Ansi.BOLD, Ansi.RESET);
	io::printfn("  %-18s | %10s | %10s | %10s | %10s", "Mode", "C3 Mem", "C3 Stream", "Mz Mem", "Mz Stream");
	io::printfn("  -------------------+------------+------------+------------+------------");
	io::printfn("  %-18s | %8.1f   | %8.1f   | %8.1f   | %8.1f", "Compression (MB/s)", avg_c3, avg_c3s, avg_mz, avg_mzs);
	io::printfn("  %-18s | %8.1f   | %8.1f   | %8.1f   | %8.1f", "Decompress (MB/s)", avg_c3d, avg_c3ds, avg_mzd, avg_mzds);
	io::printfn("\n  %sComp Mem:    C3 is %.1fx vs Miniz%s", Ansi.BOLD, avg_c3 / avg_mz, Ansi.RESET);
	io::printfn("  %sComp Stream: C3 is %.1fx vs Miniz%s", Ansi.BOLD, avg_c3s / avg_mzs, Ansi.RESET);
	io::printfn("  %sDecomp Mem:  C3 is %.1fx vs Miniz%s", Ansi.BOLD, avg_c3d / avg_mzd, Ansi.RESET);
	io::printfn("  %sDecomp Stream: C3 is %.1fx vs Miniz%s\n", Ansi.BOLD, avg_c3ds / avg_mzds, Ansi.RESET);

	return 0;
}

struct BenchResult {
	long time_ns;
	usz size;
	double ratio;
	double throughput_mbs;
}

struct DataSetResult {
	String title;
	BenchResult c3c;
	BenchResult c3cs;
	BenchResult mzc;
	BenchResult mzcs;
	BenchResult c3d;
	BenchResult c3ds;
	BenchResult mzd;
	BenchResult mzds;
}

struct BenchTotal {
	double c3_speed_sum;
	double c3s_speed_sum;
	double mz_speed_sum;
	double mzs_speed_sum;
	double c3d_speed_sum;
	double c3ds_speed_sum;
	double mzd_speed_sum;
	double mzds_speed_sum;
	int count;
}
BenchTotal totals;

struct NullWriter (io::OutStream)
{
	usz* written;
}

fn usz? NullWriter.write(&self, char[] bytes) @dynamic
{
	*self.written += bytes.len;
	return bytes.len;
}

fn void? NullWriter.write_byte(&self, char byte) @dynamic
{
	*self.written += 1;
}

fn DataSetResult run_bench(String title, char[] data)
{
	DataSetResult out_res;
	out_res.title = title;

	// --- COMPRESSION ---

	// C3 In-Memory Bench
	Clock start = clock::now();
	char[] c3_compressed = deflate::compress(tmem, data)!!;
	Clock end = clock::now();
	out_res.c3c = calculate_metrics(data.len, (long)(end - start), c3_compressed.len);

	// C3 Streaming Bench
	usz stream_written = 0;
	NullWriter null_out = { .written = &stream_written };
	start = clock::now();
	{
		deflate::Deflater def;
		def.init(output: &null_out);
		usz chunk = 32768;
		usz off = 0;
		while (off < data.len)
		{
			usz n = min(chunk, data.len - off);
			def.write(data[off:n])!!;
			off += n;
		}
		def.close()!!;
	}
	end = clock::now();
	out_res.c3cs = calculate_metrics(data.len, (long)(end - start), stream_written);

	// Miniz In-Memory Bench
	usz miniz_size;
	start = clock::now();
	void* miniz_ptr = tdefl_compress_mem_to_heap(data.ptr, data.len, &miniz_size, MINIZ_FLAGS);
	end = clock::now();
	out_res.mzc = calculate_metrics(data.len, (long)(end - start), miniz_size);

	// Miniz Streaming Bench
	void* mz_comp = libc::malloc(TDEFL_COMPRESSOR_SIZE);
	char[] mz_outbuf = mem::temp_array(char, 65536);
	usz mzs_total_out = 0;
	start = clock::now();
	tdefl_init(mz_comp, null, null, MINIZ_FLAGS);
	{
		usz chunk = 32768;
		usz off = 0;
		while (off < data.len)
		{
			usz in_n = min(chunk, data.len - off);
			usz out_n = mz_outbuf.len;
			tdefl_compress(mz_comp, data.ptr + off, &in_n, mz_outbuf.ptr, &out_n, 0);
			mzs_total_out += out_n;
			off += in_n;
		}
		usz zero_in = 0;
		usz out_n = mz_outbuf.len;
		tdefl_compress(mz_comp, null, &zero_in, mz_outbuf.ptr, &out_n, 4);
		mzs_total_out += out_n;
	}
	end = clock::now();
	out_res.mzcs = calculate_metrics(data.len, (long)(end - start), mzs_total_out);
	libc::free(mz_comp);

	// --- DECOMPRESSION ---

	// C3 Decompression
	start = clock::now();
	char[] c3_decompressed = deflate::decompress(tmem, c3_compressed)!!;
	end = clock::now();
	out_res.c3d = calculate_metrics(data.len, (long)(end - start), c3_decompressed.len);

	// C3 Streaming Decompression
	usz c3ds_written = 0;
	NullWriter c3ds_out = { .written = &c3ds_written };
	io::ByteReader c3ds_in = io::wrap_bytes(c3_compressed);
	start = clock::now();
	deflate::decompress_stream(&c3ds_in, &c3ds_out)!!;
	end = clock::now();
	out_res.c3ds = calculate_metrics(data.len, (long)(end - start), c3ds_written);

	// Miniz Decompression
	usz mzd_out_len;
	start = clock::now();
	void* mzd_ptr = tinfl_decompress_mem_to_heap(miniz_ptr, miniz_size, &mzd_out_len, 0);
	end = clock::now();
	out_res.mzd = calculate_metrics(data.len, (long)(end - start), mzd_out_len);
	if (mzd_ptr) mz_free(mzd_ptr);

	// Miniz Streaming Decompression
	void* mzs_decomp = libc::malloc(TINFL_DECOMPRESSOR_SIZE);
	char[] mzs_out_buf = mem::temp_array(char, 65536);
	usz mzs_total_decomp = 0;
	start = clock::now();
	mem::set(mzs_decomp, 0, TINFL_DECOMPRESSOR_SIZE);
	{
		usz in_off = 0;
		while (true)
		{
			usz in_n = miniz_size - in_off;
			usz out_n = mzs_out_buf.len;
			int status = tinfl_decompress(mzs_decomp, (char*)miniz_ptr + in_off, &in_n, mzs_out_buf.ptr, mzs_out_buf.ptr, &out_n, in_off + in_n < miniz_size ? 0x01 : 0);
			mzs_total_decomp += out_n;
			in_off += in_n;
			if (status == 0) break;
			if (status < 0) break;
		}
	}
	end = clock::now();
	out_res.mzds = calculate_metrics(data.len, (long)(end - start), mzs_total_decomp);
	libc::free(mzs_decomp);
	if (miniz_ptr) mz_free(miniz_ptr);

	totals.c3_speed_sum += out_res.c3c.throughput_mbs;
	totals.c3s_speed_sum += out_res.c3cs.throughput_mbs;
	totals.mz_speed_sum += out_res.mzc.throughput_mbs;
	totals.mzs_speed_sum += out_res.mzcs.throughput_mbs;
	totals.c3d_speed_sum += out_res.c3d.throughput_mbs;
	totals.c3ds_speed_sum += out_res.c3ds.throughput_mbs;
	totals.mzd_speed_sum += out_res.mzd.throughput_mbs;
	totals.mzds_speed_sum += out_res.mzds.throughput_mbs;
	totals.count++;

	return out_res;
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
extern fn int tdefl_init(void* d, void* pPut_buf_func, void* pPut_buf_user, int flags);
extern fn int tdefl_compress(void* d, void* pIn_buf, usz* pIn_buf_size, void* pOut_buf, usz* pOut_buf_size, int flush);

extern fn void* tinfl_decompress_mem_to_heap(void* pSrc_buf, usz src_buf_len, usz* pOut_len, int flags);
extern fn int tinfl_decompress(void* r, void* pIn_buf, usz* pIn_buf_size, void* pOut_buf_start, void* pOut_buf_next, usz* pOut_buf_size, int decomp_flags);

const usz TDEFL_COMPRESSOR_SIZE = 320000; // sizeof(tdefl_compressor) ~ 319352
const usz TINFL_DECOMPRESSOR_SIZE = 16384; // sizeof(tinfl_decompressor) ~ 12440
const int TDEFL_GREEDY_PARSING_FLAG = 0x04000;
const int TDEFL_NONDETERMINISTIC_PARSING_FLAG = 0x08000;
const int C3_EQUIVALENT_PROBES = 16;

const int MINIZ_FLAGS = C3_EQUIVALENT_PROBES | TDEFL_GREEDY_PARSING_FLAG | TDEFL_NONDETERMINISTIC_PARSING_FLAG;
