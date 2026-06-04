<*
 This is a simple static HTTP server example. It listens on a TCP port, parses basic
 HTTP/1.1 requests, handles Keep-Alive connections, serves static files with Range
 and ETag support, applies dynamic Gzip compression, and displays directory listings.
*>
module http_server;
import std, libc;

const int THREAD_COUNT = 64;
const int TCP_BACKLOG = 128;

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
		switch (args[i])
		{
			case "-p":
			case "--port":
				if (i + 1 >= args.len)
				{
					io::eprintfn("Error: missing value for port option");
					return 1;
				}
				i++;
				int? parsed_port = args[i].to_int();
				if (catch parsed_port)
				{
					io::eprintfn("Error: invalid port '%s'", args[i]);
					return 1;
				}
				port = parsed_port;
			case "-r":
			case "--root":
				if (i + 1 >= args.len)
				{
					io::eprintfn("Error: missing value for root option");
					return 1;
				}
				i++;
				root_dir = args[i];
			case "-h":
			case "--help":
				io::printfn("Usage: %s [options]", args[0]);
				io::printfn("Options:");
				io::printfn("  -p, --port <port>    Set the port to listen on (default: 8080)");
				io::printfn("  -r, --root <dir>     Set the root directory to serve (default: .)");
				io::printfn("  -h, --help           Show this help message");
				return 0;
			default:
				io::eprintfn("Error: unknown option '%s'", args[i]);
				return 1;
		}
	}

	if (catch path::chdir(root_dir))
	{
		io::eprintfn("Error: could not change directory to '%s'", root_dir);
		return 1;
	}

	TcpServerSocket? server = tcp::listen("0.0.0.0", port, TCP_BACKLOG, REUSEADDR);
	if (catch err = server)
	{
		io::eprintfn("Failed to listen on port %d: %s", port, err);
		return 1;
	}

	Path cwd = path::tcwd()!!;
	io::printfn("Serving %s on http://0.0.0.0:%d", cwd, port);

	ThreadPool{THREAD_COUNT} worker_pool;
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

	while REQ_LOOP: (true)
	{
		Poll[1] p = { { conn.sock, READ, (PollEvent)0 } };
		ulong ready = net::poll_ms(p[..], 5000) ?? 0;
		if (ready == 0) return;

		@pool()
		{
			Clock start_time = clock::now();
			int status_code = 0;
			long response_size = 0;
			ParsedRequest parsed_req;

			defer
			{
				if (status_code != 0)
				{
					String date_str = datetime::from_time_tz(time::now(), 0).format(tmem, RFC3339);
					double elapsed_ms = (double)start_time.to_now() / 1_000_000.0;
					String time_str = string::tformat("%.1fms", elapsed_ms);
					String human_s = human_size(response_size);
					String ip_str = "127.0.0.1";
					AIFamily family = (AIFamily)(conn.addr_storage[1] == 0 ? conn.addr_storage[0] : conn.addr_storage[1]);
					switch (family)
					{
						case os::AF_INET:
							ip_str = string::tformat("%d.%d.%d.%d",
								(conn.addr_storage[4] & 0xFF), (conn.addr_storage[5] & 0xFF),
								(conn.addr_storage[6] & 0xFF), (conn.addr_storage[7] & 0xFF));
						case os::AF_INET6:
							ip_str = "::1";
					}
					io::printfn("[%s] %3d %-6s %-32s %8s %8s  %s",
						date_str, status_code,
						parsed_req.method ?: "-",
						parsed_req.path ?: "-",
						time_str, human_s, ip_str);
				}
			}

			char[8192] buf;
			sz total = 0;
			while (total < buf.len)
			{
				sz? bytes_read = conn.read(buf[total..]);
				if (catch bytes_read) return;
				if (bytes_read == 0) return;
				total += bytes_read;
				String so_far = (String)buf[:total];
				if (so_far.contains("\r\n\r\n")) break;
			}

			String request_data = (String)buf[:total];

			if (try req_val = parse_request(request_data))
			{
				parsed_req = req_val;
			}
			else
			{
				send_error(&conn, 400, "Bad Request", &status_code, &response_size, false)!;
				return;
			}

			String raw_path = parsed_req.path;
			if (env::WIN32)	raw_path = raw_path.treplace("\\", "/");

			Splitter segments = raw_path.tokenize("/");
			while (try seg = segments.next())
			{
				if (seg == "..")
				{
					send_error(&conn, 403, "Forbidden", &status_code, &response_size, parsed_req.keep_alive)!;
					if (!parsed_req.keep_alive) return;
					continue REQ_LOOP;
				}
			}

			if (raw_path == "/favicon.ico" && !file::exists("favicon.ico"))
			{
				String conn_str = parsed_req.keep_alive ? "keep-alive" : "close";
				conn.write(string::tformat("HTTP/1.1 204 No Content\r\nConnection: %s\r\n\r\n", conn_str))!;
				status_code = 204;
				if (!parsed_req.keep_alive) return;
				continue REQ_LOOP;
			}

			String rel = raw_path.len > 1 ? raw_path[1..] : ".";

			bool is_dir = path::is_dir(path::tnew(rel)!!);

			if (is_dir)
			{
				String index_path = string::tformat("%s/index.html", rel);
				if (file::exists(index_path))
				{
					rel = index_path;
					parsed_req.range_header = "";
				}
				else
				{
					send_dir_listing(&conn, rel, &parsed_req, &status_code, &response_size)!;
					if (!parsed_req.keep_alive) return;
					continue REQ_LOOP;
				}
			}

			send_file(&conn, rel, &parsed_req, &status_code, &response_size)!;
			if (!parsed_req.keep_alive) return;
		};
	}
}

struct ParsedRequest
{
	String method;
	String path;
	String query;
	bool accept_gzip;
	String range_header;
	String if_none_match;
	bool keep_alive;
}

faultdef MALFORMED_REQUEST;

fn ParsedRequest? parse_request(String req)
{
	ParsedRequest parsed_req;
	switch
	{
		case req.starts_with("GET "):  parsed_req.method = "GET";
		case req.starts_with("HEAD "): parsed_req.method = "HEAD";
		default: return MALFORMED_REQUEST~;
	}

	sz? first_crlf = req.index_of("\r\n");
	if (catch first_crlf) return MALFORMED_REQUEST~;

	String rest = req[parsed_req.method.len + 1 .. first_crlf];
	sz? space_idx = rest.index_of_char(' ');
	if (catch space_idx) return MALFORMED_REQUEST~;

	String raw = rest[:space_idx];
	String version = rest[space_idx + 1..].trim();
	parsed_req.keep_alive = false;

	String path_part = raw;
	if (try query_idx = raw.index_of_char('?'))
	{
		path_part = raw[:query_idx];
		parsed_req.query = raw[query_idx + 1..];
	}
	parsed_req.path = url::tdecode(path_part, PATH) ?? path_part;

	sz? end_headers = req.index_of("\r\n\r\n");
	String headers_part = req[first_crlf + 2 .. (end_headers ?? req.len)];

	Splitter lines = headers_part.tokenize("\r\n");
	while (try line = lines.next())
	{
		if (try col = line.index_of_char(':'))
		{
			String name = line[:col].trim();
			String val = line[col + 1..].trim();
			switch (name.to_lower_tcopy())
			{
				case "connection":
					if (val.to_lower_tcopy() == "close") parsed_req.keep_alive = false;
					if (val.to_lower_tcopy() == "keep-alive") parsed_req.keep_alive = true;
				case "range":
					parsed_req.range_header = val;
				case "if-none-match":
					parsed_req.if_none_match = val;
				case "accept-encoding":
					if (val.contains("gzip")) parsed_req.accept_gzip = true;
			}
		}
	}

	return parsed_req;
}

<*
 @param [out] out_status
 @param [out] out_size
*>
fn void? send_error(TcpSocket* sock, int code, String reason, int* out_status, long* out_size, bool keep_alive)
{
	String body = string::tformat("<h1>%d %s</h1>", code, reason);
	String conn_str = keep_alive ? "keep-alive" : "close";
	sock.write(string::tformat("HTTP/1.1 %d %s\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: %d\r\nConnection: %s\r\n\r\n%s", code, reason, body.len, conn_str, body))!;
	if (out_status) *out_status = code;
	if (out_size) *out_size = body.len;
}

fn void? stream_gzip(TcpSocket* sock, InStream input, Allocator allocator)
{
	GzipWriter gz;
	gz.init(allocator, sock)!;
	defer (void)gz.close();

	io::copy_to(input, &gz)!;
}

fn void? write_response_headers(TcpSocket* sock, String status, String mime_type, bool keep_alive, String extra_fmt, args...)
{
	String date_now = datetime::from_time_tz(time::now(), 0).format(tmem, RFC1123);
	String extra_headers = string::tformat(extra_fmt, ...args);
	String conn_str = keep_alive ? "keep-alive" : "close";
	sock.write(string::tformat("HTTP/1.1 %s\r\n"
		"Date: %s\r\n"
		"Content-Type: %s\r\n"
		"%s"
		"Connection: %s\r\n\r\n", status, date_now, mime_type, extra_headers, conn_str))!;
}

<*
 @param [out] out_status
 @param [out] out_size
*>
fn void? send_file(TcpSocket* sock, String path_str, ParsedRequest* req, int* out_status, long* out_size)
{
	@pool()
	{
		File? target_file_opt = file::open(path_str, "rb");
		if (catch err = target_file_opt)
		{
			if (err == io::FILE_NOT_FOUND)
			{
				send_error(sock, 404, "Not Found", out_status, out_size, req.keep_alive)!;
				return;
			}
			return err~;
		}
		File target_file = target_file_opt;
		defer (void)target_file.close();

		long size = target_file.size()!;

		Time mod = file::last_modified(path_str) ?? (Time)0;
		String etag = string::tformat("\"%d-%d\"", mod, size);

		if (req.if_none_match == etag)
		{
			String conn_str = req.keep_alive ? "keep-alive" : "close";
			sock.write(string::tformat("HTTP/1.1 304 Not Modified\r\nETag: %s\r\nConnection: %s\r\n\r\n", etag, conn_str))!;
			if (out_status) *out_status = 304;
			if (out_size) *out_size = 0;
			return;
		}

		long start = 0;
		long end = size - 1;
		bool is_partial = false;

		if (req.range_header.starts_with("bytes="))
		{
			String range_val = req.range_header[6..];
			if (try dash_idx = range_val.index_of_char('-'))
			{
				String start_str = range_val[:dash_idx].trim();
				String end_str = range_val[dash_idx + 1..].trim();
				switch
				{
					case start_str.len > 0:
						start = start_str.to_long() ?? 0;
						if (end_str.len > 0) end = end_str.to_long() ?? (size - 1);
					case end_str.len > 0:
						long suffix = end_str.to_long() ?? 0;
						start = size - suffix;
						end = size - 1;
				}
				if (start < 0) start = 0;
				if (end >= size) end = size - 1;
				if (start <= end) is_partial = true;
			}
		}

		Path file_path = path::tnew(path_str)!!;
		String mime_type = mime_for_ext((file_path.extension() ?? "").to_lower_tcopy());
		bool compress = req.accept_gzip && !is_partial && is_compressible(mime_type);


		long content_len = is_partial ? (end - start + 1) : size;

		if (is_partial)
		{
			if (out_status) *out_status = 206;
			if (out_size) *out_size = content_len;

			write_response_headers(sock, "206 Partial Content", mime_type, req.keep_alive,
				"Accept-Ranges: bytes\r\nContent-Length: %d\r\nContent-Range: bytes %d-%d/%d\r\nETag: %s\r\n",
				content_len, start, end, size, etag)!;
		}
		else
		{
			if (out_status) *out_status = 200;
			if (out_size) *out_size = size;

			if (compress)
			{
				req.keep_alive = false;
				write_response_headers(sock, "200 OK", mime_type, false,
					"Accept-Ranges: bytes\r\nETag: %s\r\nContent-Encoding: gzip\r\n", etag)!;
				if (req.method == "HEAD") return;
				stream_gzip(sock, &target_file, tmem)!;
				return;
			}
			else
			{
				write_response_headers(sock, "200 OK", mime_type, req.keep_alive,
					"Accept-Ranges: bytes\r\nETag: %s\r\nContent-Length: %d\r\n", etag, size)!;
			}
		}

		if (req.method == "HEAD") return;

		if (content_len > 64*1024)
		{
			if (try map = file::mmap_file(target_file))
			{
				defer (void)map.destroy();
				char* ptr = (char*)map.vm.ptr;
				sock.write(ptr[(sz)start : (sz)content_len])!;
				return;
			}
		}

		target_file.seek(start)!;
		LimitReader lr;
		lr.init(&target_file, content_len);
		io::copy_to(&lr, sock)!;
	};
}

struct FileEntry
{
	Path path;
	String name;
	bool is_dir;
}

fn int entry_cmp(FileEntry* a, FileEntry* b)
{
	switch
	{
		case a.is_dir && !b.is_dir: return -1;
		case !a.is_dir && b.is_dir: return 1;
		default: return a.name.compare_to_ignore_case(b.name);
	}
}

fn String human_size(long size)
{
	if (size < 1024) return string::tformat("%dB", size);
	double scaled_size = size / 1024d;
	if (scaled_size < 1024) return string::tformat("%.1fKB", scaled_size);
	scaled_size /= 1024d;
	if (scaled_size < 1024) return string::tformat("%.1fMB", scaled_size);
	scaled_size /= 1024d;
	return string::tformat("%.1fGB", scaled_size);
}

fn String html_escape(String input_string) // TODO: std::encoding::html
{
	// Worst-case HTML entity size
	String buf = (String)mem::talloc_array(char, input_string.len * 6);
	sz pos = 0;
	foreach (character : input_string)
	{
		String entity;
		switch (character)
		{
			case '&':  entity = "&amp;";
			case '<':  entity = "&lt;";
			case '>':  entity = "&gt;";
			case '"':  entity = "&quot;";
			case '\'': entity = "&#39;";
			default:
				buf[pos++] = character;
				continue;
		}
		buf[pos : entity.len] = entity[..];
		pos += entity.len;
	}
	return buf[:pos];
}

<*
 @param [out] out_status
 @param [out] out_size
*>
fn void? send_dir_listing(TcpSocket* sock, String rel_path, ParsedRequest* req, int* out_status, long* out_size)
{
	@pool()
	{
		Path dir_path = path::tnew(rel_path)!!;
		PathList? files = path::ls(tmem, dir_path, relative: false);
		if (catch files)
		{
			send_error(sock, 500, "Internal Server Error", out_status, out_size, req.keep_alive)!;
			return;
		}

		FileEntry[] entries = mem::talloc_array(FileEntry, files.size);
		sz count = 0;
		foreach (entry_path : files)
		{
			String name = entry_path.basename();
			if (name == "." || name == "..") continue;

			entries[count] = {
				.path = entry_path,
				.name = name,
				.is_dir = path::is_dir(entry_path)
			};
			count++;
		}

		FileEntry[] entries_to_sort = entries[:count];
		sort::quicksort(entries_to_sort, &entry_cmp);

		DString html = dstring::new_with_capacity(tmem, 32768);

		html.appendf(DIR_LISTING_TEMPLATE_HEADER, FAVICON, req.path, req.path);

		if (req.path != "/") html.append(DIR_LISTING_GO_UP);

		html.append(DIR_LISTING_TABLE_HEADER);

		foreach (entry : entries_to_sort)
		{
			@pool()
			{
				long size_raw = entry.is_dir ? -1 : (path::file_size(entry.path) ?? 0);
				String size_str = entry.is_dir ? "" : human_size(size_raw);

				Time mod = file::last_modified(entry.path) ?? (Time)0;
				String date_str = mod == (Time)0 ? "-" : datetime::from_time(mod).to_local().format(tmem, DATETIME);

				String encoded_link = url::tencode(entry.name, UNRESERVED);
				if (entry.is_dir) encoded_link = string::tformat("%s/", encoded_link);

				String safe_name = html_escape(entry.name);
				html.appendf(DIR_LISTING_ROW_TEMPLATE,
					entry.name.starts_with(".") ? "hidden-object" : "",
					entry.is_dir ? "D" : "F", safe_name, entry.is_dir ? "dir" : "file", encoded_link, safe_name, safe_name,
					size_raw, size_str, (long)mod, date_str);
			};
		}

		html.append(DIR_LISTING_TABLE_FOOTER);

		String out_html = html.str_view();

		if (req.accept_gzip)
		{
			req.keep_alive = false;
			write_response_headers(sock, "200 OK", "text/html; charset=utf-8", false,
				"Content-Encoding: gzip\r\n")!;
			if (req.method == "HEAD") return;
			stream_gzip(sock, &&io::wrap_bytes(out_html), tmem)!;
		}
		else
		{
			write_response_headers(sock, "200 OK", "text/html; charset=utf-8", req.keep_alive,
				"Content-Length: %d\r\n", out_html.len)!;
			if (req.method == "HEAD") return;
			sock.write(out_html)!;
		}
		if (out_status) *out_status = 200;
		if (out_size) *out_size = out_html.len;
	};
}

enum MimeType : int (String ext, String mime, bool compress)
{
	HTML        { "html",        "text/html; charset=utf-8",      true  },
	HTM         { "htm",         "text/html; charset=utf-8",      true  },
	XHTML       { "xhtml",       "application/xhtml+xml",         true  },
	CSS         { "css",         "text/css",                      true  },
	JS          { "js",          "application/javascript",        true  },
	MJS         { "mjs",         "application/javascript",        true  },
	TS          { "ts",          "text/x-typescript",             true  },
	JSON        { "json",        "application/json",              true  },
	MAP         { "map",         "application/json",              true  },
	WEBMANIFEST { "webmanifest", "application/manifest+json",     true  },
	XML         { "xml",         "application/xml",               true  },
	ATOM        { "atom",        "application/atom+xml",          true  },
	RSS         { "rss",         "application/rss+xml",           true  },
	TXT         { "txt",         "text/plain; charset=utf-8",     true  },
	MD          { "md",          "text/plain; charset=utf-8",     true  },
	CSV         { "csv",         "text/csv",                      true  },
	RTF         { "rtf",         "application/rtf",               true  },
	SH          { "sh",          "text/x-sh",                     true  },
	PY          { "py",          "text/x-python",                 true  },
	C           { "c",           "text/x-csrc",                   true  },
	H           { "h",           "text/x-csrc",                   true  },
	CPP         { "cpp",         "text/x-c++src",                 true  },
	HPP         { "hpp",         "text/x-c++src",                 true  },
	C3          { "c3",          "text/x-c3",                     true  },
	YAML        { "yaml",        "text/yaml",                     true  },
	YML         { "yml",         "text/yaml",                     true  },
	TOML        { "toml",        "text/x-toml",                   true  },
	CONF        { "conf",        "text/plain",                    true  },
	INI         { "ini",         "text/plain",                    true  },
	WASM        { "wasm",        "application/wasm",              true  },
	TTF         { "ttf",         "font/ttf",                      true  },
	OTF         { "otf",         "font/otf",                      true  },
	WOFF        { "woff",        "font/woff",                     true  },
	WOFF2       { "woff2",       "font/woff2",                    false },
	EOT         { "eot",         "application/vnd.ms-fontobject", true  },
	PNG         { "png",         "image/png",                     false },
	JPG         { "jpg",         "image/jpeg",                    false },
	JPEG        { "jpeg",        "image/jpeg",                    false },
	GIF         { "gif",         "image/gif",                     false },
	SVG         { "svg",         "image/svg+xml",                 true  },
	ICO         { "ico",         "image/x-icon",                  false },
	WEBP        { "webp",        "image/webp",                    false },
	AVIF        { "avif",        "image/avif",                    false },
	MP4         { "mp4",         "video/mp4",                     false },
	MKV         { "mkv",         "video/x-matroska",              false },
	WEBM        { "webm",        "video/webm",                    false },
	AVI         { "avi",         "video/x-msvideo",               false },
	MOV         { "mov",         "video/quicktime",               false },
	MP3         { "mp3",         "audio/mpeg",                    false },
	WAV         { "wav",         "audio/wav",                     false },
	OGG         { "ogg",         "audio/ogg",                     false },
	OPUS        { "opus",        "audio/ogg; codecs=opus",        false },
	FLAC        { "flac",        "audio/flac",                    false },
	WEBA        { "weba",        "audio/webm",                    false },
	M4A         { "m4a",         "audio/mp4",                     false },
	MID         { "mid",         "audio/midi",                    false },
	PDF         { "pdf",         "application/pdf",               false },
	GZ          { "gz",          "application/gzip",              false },
	ZIP         { "zip",         "application/zip",               false },
	TAR         { "tar",         "application/x-tar",             false },
	XZ          { "xz",          "application/x-xz",              false },
	SEVEN_Z     { "7z",          "application/x-7z-compressed",   false }
}

fn String mime_for_ext(String file_ext)
{
	if (try matched = MimeType::lookup_field(ext, file_ext)) return matched.mime;
	return "application/octet-stream";
}

fn bool is_compressible(String content_type)
{
	if (try matched = MimeType::lookup_field(mime, content_type)) return matched.compress;
	return content_type.starts_with("text/");
}

const String DIR_LISTING_TEMPLATE_HEADER =
`<!DOCTYPE html><html><head><meta charset="UTF-8">
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
var cb = document.querySelector("#UI_showHidden input");
if (cb && sessionStorage.getItem("showHidden") === "true") {
	cb.checked = true; gTable.classList.add("show-hidden");
}
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
if (gOrderBy !== 0) {
	var intA = parseFloat(a); var intB = parseFloat(b);
	if (!isNaN(intA) && !isNaN(intB)) { return intA < intB ? -1 : (intA > intB ? 1 : 0); }
}
a = a.toUpperCase(); b = b.toUpperCase();
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
function updateHidden(cb) {
	gTable.classList.toggle("show-hidden", cb.checked);
	sessionStorage.setItem("showHidden", cb.checked);
}
</script>
<noscript><style>
tr.hidden-object { display: table-row; }
tr.hidden-object a::before { opacity: 0.4; }
#UI_showHidden { display: none; }
</style></noscript>
</head><body>
<h1>Index of %s</h1>`;

const String DIR_LISTING_GO_UP = `<p id="UI_goUp"><a class="up" href="..">Up to higher level directory</a></p>`;

const String DIR_LISTING_TABLE_HEADER =
`<p id="UI_showHidden"><label><input type="checkbox" onchange="updateHidden(this)"> Show hidden objects</label></p>
<table>
<colgroup>
<col style="width: auto;">
<col style="width: 100px;">
<col style="width: 220px;">
</colgroup>
<thead><tr><th class="name-col">Name</th><th class="size">Size</th><th class="date">Last Modified</th></tr></thead><tbody>`;

const String DIR_LISTING_ROW_TEMPLATE =
`<tr class="%s">
<td class="name-col" sortable-data="%s%s"><a class="%s" href="%s" title="%s">%s</a></td>
<td class="size" sortable-data="%d">%s</td>
<td class="date" sortable-data="%d">%s</td></tr>`;

const String DIR_LISTING_TABLE_FOOTER = `</tbody></table></body></html>`;

const String FAVICON = `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8%2F9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAjFJREFUeNqsU8uOElEQPffR3XQ3ONASdBJCSBxHos5%2B3Bg3rvkCv8PElS78gPkO%2FATjQoUdO2ftrJiRh6aneTb9sOpC4weMN6lcuFV16pxDIfI8x12OYIDhcPiu2Wx%2B%2FHF5CW1Z6Jyegt%2FTNEWSJIjjGFEUIQxDrFYrWFSzXC4%2FdLvd95pRKpXKy%2BpRFZ7nwaWo1%2BsGnQG2260BKJfLKJVKGI1GEEJw7ateryd0v993W63WEwjgxfn5obGYzgCbzcaEbdsIggDj8Riu6z6iUk9SYZMSx8W0LMsM%2FSKK75xnJlIq80anQXdbEp0OhcPJ0eiaJnGRMEyyPDsAKKUM9clkYoDo3SZJzzSdp0VSKYmfV1co%2Bz580kw5KDIM8RbRfEnUf1HzxtQyMAGcaGruTKczMzEIaqhKifV6jd%2BzGQQB5llunF%2FM52BizC2K5sYPYvZcu653tjOM9O93wnYc08gmkgg4VAxixfqFUJT36AYBZGd6PJkFCZnnlBxMp38gqIgLpZB0y4Nph18lyWh5FFbrOSxbl3V4G%2BVB7T4ajYYxTyuLtO%2BCvWGgJE1Mc7JNsJEhvgw%2FQV4fo%2F24nbEsX2u1d5sVyn8sJO0ZAQiIYnFh%2BxrfLz%2Fj29cBS%2FO14zg3i8XigW3ZkErDtmKoeM%2BAJGRMnXeEPGKf0nCD1ydvkDzU9Jbc6OpR7WIw6L8lQ%2B4pQ1%2FlPF0RGM9Ns91Wmptk0GfB4EJkt77vXYj%2F8m%2B8y%2FkrwABHbz2H9V68DQAAAABJRU5ErkJggg%3D%3D`;