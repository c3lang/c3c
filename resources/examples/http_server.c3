import std, libc;

const int THREAD_COUNT = 64;

fn void ignore_sigpipe() @init(1001) @if(!env::WIN32)
{
	libc::signal(libc::SIGPIPE, fn void(int) {});
}

fn int main(String[] args)
{
	int port = 8080;
	String root_dir = ".";

	for (sz i = 1; i < args.len; i++)
	{
		String arg = args[i];
		if ((arg == "-p" || arg == "--port") && i + 1 < args.len)
		{
			i++;
			int? parsed_port = args[i].to_int();
			if (catch parsed_port)
			{
				io::eprintfn("Error: invalid port '%s'", args[i]);
				return 1;
			}
			port = parsed_port;
		}
		else if ((arg == "-r" || arg == "--root") && i + 1 < args.len)
		{
			i++;
			root_dir = args[i];
		}
		else if (arg == "-h" || arg == "--help")
		{
			io::printfn("Usage: %s [options]", args[0]);
			io::printfn("Options:");
			io::printfn("  -p, --port <port>    Set the port to listen on (default: 8080)");
			io::printfn("  -r, --root <dir>     Set the root directory to serve (default: .)");
			io::printfn("  -h, --help           Show this help message");
			return 0;
		}
	}

	if (catch path::chdir(root_dir))
	{
		io::eprintfn("Error: could not change directory to '%s'", root_dir);
		return 1;
	}

	TcpServerSocket? server = tcp::listen("0.0.0.0", port, 128, REUSEADDR);
	if (catch err = server)
	{
		io::eprintfn("Failed to listen on port %d: %s", port, err);
		return 1;
	}

	Path cwd = path::tcwd()!!;
	io::printfn("Serving %s on http://0.0.0.0:%d", cwd.str_view(), port);

	pool::ThreadPool{THREAD_COUNT} worker_pool;
	if (catch err = worker_pool.init())
	{
		io::eprintfn("Failed to init thread pool: %s", err);
		return 1;
	}
	defer worker_pool.stop_and_destroy();

	while (true)
	{
		TcpSocket? conn = tcp::accept(&server);
		if (catch conn) continue;

		TcpSocket* conn_ptr = mem::new(TcpSocket);
		*conn_ptr = conn;

		worker_pool.push(&handle_thread, conn_ptr);
	}
}

fn int handle_thread(void* arg) => @pool_init(mem, 65536)
{
	TcpSocket* conn_ptr = (TcpSocket*)arg;
	TcpSocket conn = *conn_ptr;
	mem::free(conn_ptr);
	(void)handle(conn);
	return 0;
}

fn void? handle(TcpSocket conn)
{
	defer (void)conn.close();
	Clock start_time = clock::now();
	int status_code = 0;
	long response_size = 0;
	String method_str = "-";
	String path_str = "-";

	defer
	{
		if (status_code != 0)
		{
			String date_str = datetime::from_time_tz(time::now(), 0).format(tmem, RFC3339);
			double elapsed_ms = (double)start_time.to_now() / 1_000_000.0;
			String time_str = string::tformat("%.1fms", elapsed_ms);
			String human_s = human_size(response_size);
			String ip_str = "127.0.0.1";
			if (conn.addr_storage[1] == 2)
			{
				ip_str = string::tformat("%d.%d.%d.%d",
					(conn.addr_storage[4] & 0xFF), (conn.addr_storage[5] & 0xFF),
					(conn.addr_storage[6] & 0xFF), (conn.addr_storage[7] & 0xFF));
			}
			else if (conn.addr_storage[1] == 10)
			{
				ip_str = "::1";
			}
			io::printfn("[%s] %3d %-6s %-32s %8s %8s  %s",
				date_str, status_code, method_str, path_str, time_str, human_s, ip_str);
		}
	}

	char[8192] buf;
	sz total = 0;
	while (total < buf.len)
	{
		sz? bytes_read = conn.read(buf[total : buf.len - total]);
		if (catch bytes_read) return;
		if (bytes_read == 0) return;
		total += bytes_read;
		String so_far = (String)buf[:total];
		if (so_far.contains("\r\n\r\n")) break;
	}

	String request_data = (String)buf[:total];
	ParsedRequest pr = parse_request(request_data);
	method_str = pr.method;
	path_str = pr.path;
	if (!pr.path)
	{
		send_error(&conn, 400, "Bad Request", &status_code, &response_size)!;
		return;
	}

	String raw_path = pr.path;

	if (raw_path.contains("/../") || raw_path.ends_with("/.."))
	{
		send_error(&conn, 403, "Forbidden", &status_code, &response_size)!;
		return;
	}

	if (raw_path == "/favicon.ico" && !file::exists("favicon.ico"))
	{
		conn.write("HTTP/1.1 204 No Content\r\nConnection: close\r\n\r\n")!;
		status_code = 204;
		return;
	}

	String range_hdr = extract_header(request_data, "Range");
	String if_none_match = extract_header(request_data, "If-None-Match");

	String rel = raw_path == "/" ? "." : raw_path[1..];
	if (!rel.len) rel = ".";

	bool is_dir = false;
	@pool()
	{
		Path dir_check_path = path::temp(rel)!!;
		is_dir = path::is_dir(dir_check_path);
	};

	if (is_dir)
	{
		String index_path = rel.len > 0 && rel != "."
			? string::tformat("%s/index.html", rel)
			: "index.html";

		if (file::exists(index_path))
		{
			send_file(&conn, index_path, "", if_none_match, pr.method == "HEAD", pr.accept_gzip, &status_code, &response_size)!;
		}
		else
		{
			send_dir_listing(&conn, rel, raw_path, pr.accept_gzip, &status_code, &response_size)!;
		}
	}
	else
	{
		send_file(&conn, rel, range_hdr, if_none_match, pr.method == "HEAD", pr.accept_gzip, &status_code, &response_size)!;
	}
}

fn String ext_of(String path_str)
{
	sz? dot = path_str.rindex_of_char('.');
	if (catch dot) return "";
	String raw = path_str[dot + 1..];
	DString lower = dstring::temp_with_capacity(raw.len);
	foreach (c : raw) lower.append((char)ascii::to_lower(c));
	return lower.str_view();
}


struct ParsedRequest
{
	String method;
	String path;
	String query;
	bool accept_gzip;
}

fn String url_decode(Allocator allocator, String url_str)
{
	if (!url_str.contains("%")) return url_str.copy(allocator);

	DString builder;
	builder.init(tmem, url_str.len);
	for (sz i = 0; i < url_str.len; i++)
	{
		char c = url_str[i];
		if (c == '%' && i + 2 < url_str.len)
		{
			if (try val = url_str[i + 1 : 2].to_int(16))
			{
				builder.append((char)val);
				i += 2;
				continue;
			}
		}

		builder.append(c);
	}
	return builder.copy_str(allocator);
}

fn String url_encode(Allocator allocator, String raw_str)
{
	DString builder;
	builder.init(tmem, raw_str.len * 3);
	foreach (c : raw_str)
	{
		if (ascii::is_alnum(c) || c == '-' || c == '_' || c == '.' || c == '~')
		{
			builder.append(c);
		}
		else
		{
			builder.appendf("%%%02X", (char)c);
		}
	}
	return builder.copy_str(allocator);
}

fn String extract_header(String req, String header_name)
{
	sz? idx = req.index_of(header_name);
	if (catch idx) return "";

	String rest = req[idx + header_name.len..];
	sz? eol = rest.index_of("\r\n");
	if (catch eol) return "";

	String val = rest[:eol];
	sz? col = val.index_of_char(':');
	if (try col) val = val[col + 1..];

	return val.trim();
}

fn ParsedRequest parse_request(String req)
{
	ParsedRequest pr = { .method = "", .path = "", .query = "", .accept_gzip = false };
	if (req.starts_with("GET ")) { pr.method = "GET"; }
	else if (req.starts_with("HEAD ")) { pr.method = "HEAD"; }
	else { return pr; }

	String rest = req[pr.method.len + 1..];
	sz? space_idx = rest.index_of_char(' ');
	if (catch space_idx) return pr;

	String raw = rest[:space_idx];
	sz? query_idx = raw.index_of_char('?');
	if (try query_idx)
	{
		pr.path = url_decode(tmem, raw[:query_idx]);
		pr.query = raw[query_idx + 1..];
	}
	else
	{
		pr.path = url_decode(tmem, raw);
	}

	String enc = extract_header(req, "Accept-Encoding");
	if (enc.contains("gzip")) pr.accept_gzip = true;

	return pr;
}


fn void? send_error(TcpSocket* sock, int code, String reason, int* out_status, long* out_size)
{
	String body = string::tformat("<h1>%d %s</h1>", code, reason);
	sock.write(string::tformat("HTTP/1.1 %d %s\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s", code, reason, body.len, body))!;
	if (out_status) *out_status = code;
	if (out_size) *out_size = body.len;
}

fn void? stream_gzip(TcpSocket* sock, InStream input, Allocator allocator)
{
	gzip::GzipWriter gz;
	gz.init(allocator, sock)!;
	defer (void)gz.close();

	io::copy_to(input, &gz)!;
}

fn void? send_file(TcpSocket* sock, String path_str, String range_header, String if_none_match, bool head_only, bool accept_gzip, int* out_status, long* out_size)
{
	@pool()
	{
		if (!file::exists(path_str))
		{
			send_error(sock, 404, "Not Found", out_status, out_size)!;
			return;
		}
		File target_file = file::open(path_str, "rb")!;
		defer (void)target_file.close();

		long size = target_file.size()!;

		Time mod = file::last_modified(path_str) ?? (Time)0;
		String etag = string::tformat("\"%d-%d\"", mod, size);

		if (if_none_match == etag)
		{
			sock.write(string::tformat("HTTP/1.1 304 Not Modified\r\nETag: %s\r\nConnection: close\r\n\r\n", etag))!;
			if (out_status) *out_status = 304;
			if (out_size) *out_size = 0;
			return;
		}

		long start = 0;
		long end = size - 1;
		bool is_partial = false;

		if (range_header.len > 0 && range_header.starts_with("bytes="))
		{
			String range_val = range_header[6..];
			sz? dash_idx = range_val.index_of_char('-');
			if (try dash_idx)
			{
				String start_str = range_val[:dash_idx].trim();
				String end_str = range_val[dash_idx + 1..].trim();
				if (start_str.len > 0)
				{
					start = start_str.to_long() ?? 0;
					if (end_str.len > 0) end = end_str.to_long() ?? (size - 1);
				}
				else if (end_str.len > 0)
				{
					long suffix = end_str.to_long() ?? 0;
					start = size - suffix;
					end = size - 1;
				}
				if (start < 0) start = 0;
				if (end >= size) end = size - 1;
				if (start <= end) is_partial = true;
			}
		}

		String mt = mime_for_ext(ext_of(path_str));
		bool compress = accept_gzip && !is_partial && is_compressible(mt);

		String date_now = datetime::from_time_tz(time::now(), 0).format(tmem, RFC1123);

		if (is_partial)
		{
			long content_len = end - start + 1;
			if (out_status) *out_status = 206;
			if (out_size) *out_size = content_len;
			sock.write(string::tformat("HTTP/1.1 206 Partial Content\r\n"
				"Date: %s\r\n"
				"Content-Type: %s\r\n"
				"Content-Length: %d\r\n"
				"Accept-Ranges: bytes\r\n"
				"Content-Range: bytes %d-%d/%d\r\n"
				"ETag: %s\r\n"
				"Connection: close\r\n\r\n",
				date_now, mt, content_len, start, end, size, etag))!;

			if (head_only) return;
			target_file.seek(start)!;
			char[65536] buf;
			usz remaining = content_len;
			while (remaining > 0)
			{
				sz to_read = (sz)min(remaining, (usz)buf.len);
				sz bytes_read = target_file.read(buf[0 : to_read])!;
				if (bytes_read == 0) break;
				if (catch sock.write(buf[:bytes_read])) return;
				remaining -= (usz)bytes_read;
			}
		}
		else
		{
			if (out_status) *out_status = 200;
			if (out_size) *out_size = size;
			sock.write(string::tformat("HTTP/1.1 200 OK\r\n"
				"Date: %s\r\n"
				"Content-Type: %s\r\n"
				"Accept-Ranges: bytes\r\n"
				"ETag: %s\r\n"
				"Connection: close\r\n", date_now, mt, etag))!;

			if (compress)
		{
			sock.write("Content-Encoding: gzip\r\n\r\n")!;
			if (head_only) return;
			stream_gzip(sock, &target_file, tmem)!;
		}
		else
		{
			sock.write(string::tformat("Content-Length: %d\r\n\r\n", size))!;
			if (head_only) return;
			io::copy_to(&target_file, sock)!;
		}
		}
	};
}

fn int path_cmp_name(Path* a, Path* b)
{
	bool a_dir = path::is_dir(*a);
	bool b_dir = path::is_dir(*b);
	if (a_dir != b_dir) return a_dir ? -1 : 1;
	return a.basename().compare_to_ignore_case(b.basename());
}

fn String human_size(long size)
{
	if (size < 1024) return string::tformat("%dB", size);
	double d = (double)size / 1024.0;
	if (d < 1024) return string::tformat("%.1fKB", d);
	d /= 1024.0;
	if (d < 1024) return string::tformat("%.1fMB", d);
	d /= 1024.0;
	return string::tformat("%.1fGB", d);
}

fn void? send_dir_listing(TcpSocket* sock, String rel_path, String raw_path, bool accept_gzip, int* out_status, long* out_size)
{
	@pool()
	{
		Path dir_path = path::temp(rel_path)!!;
		PathList? files = path::ls(tmem, dir_path);
		if (catch files)
		{
			send_error(sock, 500, "Internal Server Error", out_status, out_size)!;
			return;
		}

		foreach (&entry_path : files)
		{
			*entry_path = dir_path.append(tmem, entry_path.basename())!!;
		}

		Path[] view = files.array_view();
		sort::quicksort(view, &path_cmp_name);

		DString html = dstring::new_with_capacity(tmem, 32768);

		String favicon = `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8%2F9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAjFJREFUeNqsU8uOElEQPffR3XQ3ONASdBJCSBxHos5%2B3Bg3rvkCv8PElS78gPkO%2FATjQoUdO2ftrJiRh6aneTb9sOpC4weMN6lcuFV16pxDIfI8x12OYIDhcPiu2Wx%2B%2FHF5CW1Z6Jyegt%2FTNEWSJIjjGFEUIQxDrFYrWFSzXC4%2FdLvd95pRKpXKy%2BpRFZ7nwaWo1%2BsGnQG2260BKJfLKJVKGI1GEEJw7ateryd0v993W63WEwjgxfn5obGYzgCbzcaEbdsIggDj8Riu6z6iUk9SYZMSx8W0LMsM%2FSKK75xnJlIq80anQXdbEp0OhcPJ0eiaJnGRMEyyPDsAKKUM9clkYoDo3SZJzzSdp0VSKYmfV1co%2Bz580kw5KDIM8RbRfEnUf1HzxtQyMAGcaGruTKczMzEIaqhKifV6jd%2BzGQQB5llunF%2FM52BizC2K5sYPYvZcu653tjOM9O93wnYc08gmkgg4VAxixfqFUJT36AYBZGd6PJkFCZnnlBxMp38gqIgLpZB0y4Nph18lyWh5FFbrOSxbl3V4G%2BVB7T4ajYYxTyuLtO%2BCvWGgJE1Mc7JNsJEhvgw%2FQV4fo%2F24nbEsX2u1d5sVyn8sJO0ZAQiIYnFh%2BxrfLz%2Fj29cBS%2FO14zg3i8XigW3ZkErDtmKoeM%2BAJGRMnXeEPGKf0nCD1ydvkDzU9Jbc6OpR7WIw6L8lQ%2B4pQ1%2FlPF0RGM9Ns91Wmptk0GfB4EJkt77vXYj%2F8m%2B8y%2FkrwABHbz2H9V68DQAAAABJRU5ErkJggg%3D%3D`;

		html.appendf(`<!DOCTYPE html><html><head><meta charset="UTF-8">
<link rel="icon" type="image/png" href="%s">
<title>Index of %s</title>
<style type="text/css">
:root {
  --bg-outer: #dcdcdc; --bg-body: #ffffff; --border: #b8b8b8;
  --text: #1a1a1a; --link: #0055bb; --head-border: #cccccc; --hover: rgba(0,0,0,0.04);
  font-family: sans-serif; font-size: 13px; background-color: var(--bg-outer); color: var(--text); padding-inline: 2em; color-scheme: light dark;
}
@media (prefers-color-scheme: dark) { :root {
  --bg-outer: #181818; --bg-body: #252525; --border: #404040;
  --text: #e0e0e0; --link: #6db3f2; --head-border: #444444; --hover: rgba(255,255,255,0.05);
} }
body { border: 1px solid var(--border); border-radius: 8px; padding: 3em; min-width: 30em; max-width: 65em; margin: 4em auto; background-color: var(--bg-body); color: var(--text); }
h1 { font-size: 160%%; margin: 0 0 0.6em; border-bottom: 1px solid var(--head-border); font-weight: normal; }
a { text-decoration: none; color: var(--link); display: block; overflow: hidden; white-space: nowrap; width: 100%%; }
a:hover { text-decoration: underline; }
table { clear: both; width: 100%%; margin: 0 auto; border-collapse: collapse; table-layout: fixed; }
thead { font-size: 130%%; border-bottom: 1px solid var(--head-border); }
th { text-align: start; white-space: nowrap; cursor: pointer; padding: 0.5em; overflow: hidden; text-overflow: ellipsis; }
th::after { display: none; width: .8em; margin-inline-end: -.8em; text-align: end; }
table[order='asc'] > thead > tr > th.active::after { content: "\2193"; display: inline-block; }
table[order='desc'] > thead > tr > th.active::after { content: "\2191"; display: inline-block; }
td { padding: 0.5em; white-space: nowrap; overflow: hidden; }
tr:hover { background: var(--hover); }
.name-col { overflow: hidden; }
.name-col a { mask-image: linear-gradient(to right, black 70%%, transparent 100%%); -webkit-mask-image: linear-gradient(to right, black 70%%, transparent 100%%); }
.size { text-align: end; padding-inline-end: 1em; }
.date { text-align: end; padding-inline-end: 0.5em; }
.up { display: flex; align-items: center; gap: 6px; overflow: visible; }
.up::before { content: "⬆️"; flex-shrink: 0; transform: scale(1.3); }
.dir { display: flex; align-items: center; gap: 6px; overflow: visible; }
.dir::before { content: "📁"; flex-shrink: 0; transform: scale(1.3); }
.file { display: flex; align-items: center; gap: 6px; overflow: visible; }
.file::before { content: "📄"; flex-shrink: 0; transform: scale(1.3); }
a.file[href$=".html" i]::before, a.file[href$=".htm" i]::before, a.file[href$=".xhtml" i]::before { content: "🌐"; }
a.file[href$=".css" i]::before { content: "🎨"; }
a.file[href$=".js" i]::before, a.file[href$=".mjs" i]::before, a.file[href$=".ts" i]::before { content: "⚡"; }
a.file[href$=".json" i]::before, a.file[href$=".xml" i]::before, a.file[href$=".yaml" i]::before, a.file[href$=".yml" i]::before, a.file[href$=".toml" i]::before { content: "🔧"; }
a.file[href$=".c" i]::before, a.file[href$=".h" i]::before, a.file[href$=".cpp" i]::before, a.file[href$=".hpp" i]::before, a.file[href$=".c3" i]::before { content: "📜"; }
a.file[href$=".py" i]::before, a.file[href$=".sh" i]::before, a.file[href$=".rs" i]::before, a.file[href$=".go" i]::before { content: "📜"; }
a.file[href$=".md" i]::before, a.file[href$=".txt" i]::before, a.file[href$=".csv" i]::before, a.file[href$=".rtf" i]::before { content: "📝"; }
a.file[href$=".pdf" i]::before { content: "📑"; }
a.file[href$=".png" i]::before, a.file[href$=".jpg" i]::before, a.file[href$=".jpeg" i]::before, a.file[href$=".gif" i]::before, a.file[href$=".svg" i]::before, a.file[href$=".webp" i]::before, a.file[href$=".avif" i]::before, a.file[href$=".ico" i]::before { content: "📷"; }
a.file[href$=".mp4" i]::before, a.file[href$=".mkv" i]::before, a.file[href$=".webm" i]::before, a.file[href$=".avi" i]::before, a.file[href$=".mov" i]::before { content: "🎬"; }
a.file[href$=".mp3" i]::before, a.file[href$=".wav" i]::before, a.file[href$=".ogg" i]::before, a.file[href$=".flac" i]::before, a.file[href$=".opus" i]::before, a.file[href$=".m4a" i]::before, a.file[href$=".mid" i]::before { content: "🎵"; }
a.file[href$=".zip" i]::before, a.file[href$=".tar" i]::before, a.file[href$=".gz" i]::before, a.file[href$=".xz" i]::before, a.file[href$=".7z" i]::before { content: "📦"; }
a.file[href$=".ttf" i]::before, a.file[href$=".otf" i]::before, a.file[href$=".woff" i]::before, a.file[href$=".woff2" i]::before { content: "🔤"; }
a.file[href$=".wasm" i]::before { content: "⚙️"; }
tr.hidden-object { display: none; } table.show-hidden tr.hidden-object { display: table-row; }
table.show-hidden tr.hidden-object a::before { opacity: 0.4; }
</style>
<script type="application/javascript">
'use strict';
var gTable, gOrderBy, gTBody, gRows;
document.addEventListener("DOMContentLoaded", function() {
gTable = document.getElementsByTagName("table")[0]; gTBody = gTable.tBodies[0];
if (gTBody.rows.length < 1) return;
var headCells = gTable.tHead.rows[0].cells;
for (var i = 0; i < headCells.length; i++) {
	headCells[i].addEventListener("click", (function(index) { return function() { orderBy(index); }; })(i));
}
gTable.setAttribute("order", "");
});
function compareRows(rowA, rowB) {
var a = rowA.cells[gOrderBy].getAttribute("sortable-data") || rowA.cells[gOrderBy].textContent;
var b = rowB.cells[gOrderBy].getAttribute("sortable-data") || rowB.cells[gOrderBy].textContent;
var intA = parseFloat(a); var intB = parseFloat(b);
if (!isNaN(intA) && !isNaN(intB)) { a = intA; b = intB; } else { a = a.toLowerCase(); b = b.toLowerCase(); }
return a < b ? -1 : (a > b ? 1 : 0);
}
function orderBy(column) {
if (!gRows) gRows = Array.from(gTBody.rows);
var order = (gOrderBy == column && gTable.getAttribute("order") == "asc") ? "desc" : "asc";
gOrderBy = column; gRows.sort(compareRows); if (order == "desc") gRows.reverse();
gTable.setAttribute("order", order);
var heads = gTable.tHead.rows[0].cells; for(var i=0; i<heads.length; i++) heads[i].classList.remove('active');
heads[column].classList.add('active');
while (gTBody.firstChild) gTBody.removeChild(gTBody.firstChild);
gRows.forEach(row => gTBody.appendChild(row));
}
function updateHidden(cb) { gTable.classList.toggle("show-hidden", cb.checked); }
</script></head><body>
<h1>Index of %s</h1>`, favicon, raw_path, raw_path);

		if (raw_path != "/")
		{
			html.append(`<p id="UI_goUp"><a class="up" href="..">Up to higher level directory</a></p>`);
		}

		html.append(`<p id="UI_showHidden"><label><input type="checkbox" onchange="updateHidden(this)"> Show hidden objects</label></p>
			<table>
			<colgroup>
			<col style="width: auto;">
			<col style="width: 100px;">
			<col style="width: 220px;">
			</colgroup>
			<thead><tr><th class="name-col">Name</th><th class="size">Size</th><th class="date">Last Modified</th></tr></thead><tbody>`);

		foreach (entry : files)
		{
			String name = entry.basename();
			if (name == "." || name == "..") continue;

			bool is_d = path::is_dir(entry);
			long size_raw = is_d ? -1 : (path::file_size(entry) ?? 0);
			String size_str = is_d ? "" : human_size(size_raw);

			long mod_raw = 0;
			String date_str = "-";
			Time? mod = file::last_modified(entry.str_view());
			if (try mod)
			{
				mod_raw = (long)mod;
				date_str = datetime::from_time(mod).to_local().format(tmem, DATETIME);
			}

			String encoded_link = url_encode(tmem, name);
			if (is_d) encoded_link = string::tformat("%s/", encoded_link);

			html.appendf(`<tr class="%s">
				<td class="name-col" sortable-data="%d%s"><a class="%s" href="%s" title="%s">%s</a></td>
				<td class="size" sortable-data="%d">%s</td>
				<td class="date" sortable-data="%d">%s</td></tr>`,
				name.starts_with(".") ? "hidden-object" : "",
				is_d ? 0 : 1, name, is_d ? "dir" : "file", encoded_link, name, name,
				size_raw, size_str, mod_raw, date_str);
		}

		html.append(`</tbody></table></body></html>`);

		String out_html = html.str_view();
		sock.write(string::tformat("HTTP/1.1 200 OK\r\n"
			"Date: %s\r\n"
			"Content-Type: text/html; charset=utf-8\r\n"
			"Connection: close\r\n", datetime::from_time_tz(time::now(), 0).format(tmem, RFC1123)))!;

		if (accept_gzip) {
			sock.write("Content-Encoding: gzip\r\n\r\n")!;
			stream_gzip(sock, &&io::wrap_bytes(out_html), tmem)!;
		} else {
			sock.write(string::tformat("Content-Length: %d\r\n\r\n", out_html.len))!;
			sock.write(out_html)!;
		}
		if (out_status) *out_status = 200;
		if (out_size) *out_size = out_html.len;
	};
}

fn String mime_for_ext(String ext)
{
	switch (ext)
	{
		case "html": case "htm":  return "text/html; charset=utf-8";
		case "xhtml":             return "application/xhtml+xml";
		case "css":               return "text/css";
		case "js":  case "mjs":   return "application/javascript";
		case "ts":                return "text/x-typescript";
		case "json":              return "application/json";
		case "map":               return "application/json";
		case "webmanifest":       return "application/manifest+json";
		case "xml":               return "application/xml";
		case "atom":              return "application/atom+xml";
		case "rss":               return "application/rss+xml";
		case "txt":               return "text/plain; charset=utf-8";
		case "md":                return "text/plain; charset=utf-8";
		case "csv":               return "text/csv";
		case "rtf":               return "application/rtf";
		case "sh":                return "text/x-sh";
		case "py":                return "text/x-python";
		case "c":   case "h":     return "text/x-csrc";
		case "cpp": case "hpp":   return "text/x-c++src";
		case "c3":                return "text/x-c3";
		case "yaml": case "yml":  return "text/yaml";
		case "toml":              return "text/x-toml";
		case "conf": case "ini":  return "text/plain";
		case "wasm":              return "application/wasm";
		case "ttf":               return "font/ttf";
		case "otf":               return "font/otf";
		case "woff":              return "font/woff";
		case "woff2":             return "font/woff2";
		case "eot":               return "application/vnd.ms-fontobject";
		case "png":               return "image/png";
		case "jpg": case "jpeg":  return "image/jpeg";
		case "gif":               return "image/gif";
		case "svg":               return "image/svg+xml";
		case "ico":               return "image/x-icon";
		case "webp":              return "image/webp";
		case "avif":              return "image/avif";
		case "mp4":               return "video/mp4";
		case "mkv":               return "video/x-matroska";
		case "webm":              return "video/webm";
		case "avi":               return "video/x-msvideo";
		case "mov":               return "video/quicktime";
		case "mp3":               return "audio/mpeg";
		case "wav":               return "audio/wav";
		case "ogg":               return "audio/ogg";
		case "opus":              return "audio/ogg; codecs=opus";
		case "flac":              return "audio/flac";
		case "weba":              return "audio/webm";
		case "m4a":               return "audio/mp4";
		case "mid":               return "audio/midi";
		case "pdf":               return "application/pdf";
		case "gz":                return "application/gzip";
		case "zip":               return "application/zip";
		case "tar":               return "application/x-tar";
		case "xz":                return "application/x-xz";
		case "7z":                return "application/x-7z-compressed";
		default:                  return "application/octet-stream";
	}
}

fn bool is_compressible(String mime)
{
	if (mime.starts_with("text/")) return true;
	if (mime.contains("javascript") || mime.contains("json") || mime.contains("xml")) return true;
	if (mime.contains("svg") || mime.contains("wasm")) return true;
	if (mime == "application/rtf") return true;
	if (mime == "application/vnd.ms-fontobject") return true;
	if (mime.starts_with("font/") && !mime.contains("woff2")) return true;
	return false;
}