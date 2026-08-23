<*
 High-performance, event-driven HTTP/1.1 static file server.

 Architecture & Engine:
 - Non-blocking socket multiplexing (`net::poll_ms`) with instant self-pipe wakeup
 - Worker thread pool fed by a thread-safe `UnboundedChannel` (zero Keep-Alive thread starvation)
 - Zero-copy memory-mapped streaming (`file::mmap_file`) for large assets (>64 KB)
 - Pre-allocated connection slot table (`Connection[MAX_CONNS]`) with zero accept heap churn

 HTTP/1.1 Features:
 - Persistent Keep-Alive connections, ETag caching (304), and byte-range requests (206)
 - On-the-fly dynamic Gzip compression on compressible MIME types
 - Interactive HTML directory listings with client-side sorting, icons, and hidden file toggles
 - Directory traversal protection (`..`), 400/403/404/503 error handlers, and structured access logging
*>
module http_server;
import std, libc;

// ============================================================================
// 1. Configuration & Constants
// ============================================================================

const int THREAD_COUNT = 32;
const int TCP_BACKLOG = 256;
const sz MAX_CONNS = 2048;
const Duration KEEPALIVE_TIMEOUT = 5 * time::SEC;
const Duration READ_TIMEOUT = 10 * time::SEC;

fn void ignore_sigpipe() @init(1001) @feat(!WIN32)
{
	libc::signal(libc::SIGPIPE, fn void(CInt) {});
}

// ============================================================================
// 2. Connection State & Global Context
// ============================================================================

enum ConnState : char
{
	UNUSED,
	READING,
	PROCESSING
}

struct Connection
{
	TcpSocket sock;
	ConnState state;
	Time last_activity;
	bool keep_alive;

	char[8192] read_buf;
	sz read_len;
}

Connection[MAX_CONNS] g_conns;
Poll[MAX_CONNS + 2] g_poll_fds;
sz[MAX_CONNS + 2] g_poll_map;

UnboundedChannel{sz}* g_work_queue;
Thread[THREAD_COUNT] g_workers;

TcpSocketPair g_wake_pair;

fn void init_wake_channel()
{
	(void)g_wake_pair.init();
	(void)g_wake_pair.send.sock.set_non_blocking(true);
	(void)g_wake_pair.recv.sock.set_non_blocking(true);
}

fn void notify_poller()
{
	(void)g_wake_pair.send.write("x");
}

fn void drain_poller_wake()
{
	char[512] buf;
	(void)g_wake_pair.recv.read(&buf);
}

// ============================================================================
// 3. Worker Thread Task Runner
// ============================================================================

fn int worker_thread_main(void* arg) => @pool_init(mem, 131072)
{
	while (try slot = g_work_queue.pop()) process_request_task(slot);
	return 0;
}

// ============================================================================
// 4. Main Event Loop & Dispatcher
// ============================================================================

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
				if (try p = args[i].to_int())
				{
					port = p;
				}
				else
				{
					io::eprintfn("Error: invalid port '%s'", args[i]);
					return 1;
				}
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
	(void)server.sock.set_non_blocking(true);

	init_wake_channel();

	Path cwd = path::tcwd() ?? (Path)".";
	io::printfn("Event-driven HTTP server serving %s on http://0.0.0.0:%d (Workers: %d, Max Conns: %d)",
		cwd, port, THREAD_COUNT, MAX_CONNS);

	g_work_queue = channel::create_unbounded(mem)!!;
	for (int i = 0; i < THREAD_COUNT; i++)
	{
		g_workers[i].create(&worker_thread_main, null)!!;
	}

	foreach (&conn : g_conns)
	{
		conn.state = UNUSED;
	}

	while (true)
	{
		Time now_time = time::now();

		// Timeout check & reap dead connections
		foreach (i, &conn : g_conns)
		{
			if (conn.state != READING) continue;

			Duration timeout = conn.read_len == 0 ? KEEPALIVE_TIMEOUT : READ_TIMEOUT;
			if (now_time.diff_us(conn.last_activity) > timeout)
			{
				close_conn(i);
			}
		}

		// Build polling set
		sz poll_count = 0;

		// Index 0: Server listen socket
		g_poll_fds[poll_count] = { .socket = server.sock, .events = READ };
		poll_count++;

		// Index 1: Poller wake notification
		g_poll_fds[poll_count] = { .socket = g_wake_pair.recv.sock, .events = READ };
		poll_count++;

		// Active client sockets
		foreach (i, &conn : g_conns)
		{
			if (conn.state == READING)
			{
				g_poll_map[poll_count] = i;
				g_poll_fds[poll_count] = { .socket = conn.sock.sock, .events = READ };
				poll_count++;
			}
		}

		ulong ready = net::poll_ms(g_poll_fds[:poll_count], 50) ?? 0;
		if (ready == 0) continue;

		// 1. Accept new incoming connections
		if (g_poll_fds[0].revents != (PollEvent)0)
		{
			while (try client = tcp::accept(&server))
			{
				(void)client.sock.set_non_blocking(true);

				if (try slot = find_free_conn_slot())
				{
					g_conns[slot].sock = client;
					g_conns[slot].state = READING;
					g_conns[slot].last_activity = time::now();
					g_conns[slot].keep_alive = true;
					g_conns[slot].read_len = 0;
				}
				else
				{
					String busy_resp = "HTTP/1.1 503 Service Unavailable\r\nConnection: close\r\nContent-Length: 0\r\n\r\n";
					(void)client.write(busy_resp);
					(void)client.close();
					break;
				}
			}
		}

		// 2. Handle wakeup notification
		if (g_poll_fds[1].revents != (PollEvent)0)
		{
			drain_poller_wake();
		}

		// 3. Handle active connection read events
		for (sz p = 2; p < poll_count; p++)
		{
			if (g_poll_fds[p].revents == (PollEvent)0) continue;

			sz slot = g_poll_map[p];
			Connection* conn = &g_conns[slot];

			if (conn.state != READING) continue;

			sz space_left = conn.read_buf.len - conn.read_len;
			if (space_left == 0)
			{
				close_conn(slot);
				continue;
			}

			sz bytes_read = conn.sock.read(conn.read_buf[conn.read_len..]) ?? 0;
			if (bytes_read == 0)
			{
				close_conn(slot);
				continue;
			}

			conn.read_len += bytes_read;
			conn.last_activity = time::now();

			String req_str = (String)conn.read_buf[:conn.read_len];
			if (req_str.contains("\r\n\r\n"))
			{
				conn.state = PROCESSING;
				(void)g_work_queue.push(slot);
			}
		}
	}
}

fn sz? find_free_conn_slot()
{
	foreach (i, &conn : g_conns)
	{
		if (conn.state == UNUSED) return i;
	}
	return NOT_FOUND~;
}

fn void close_conn(sz slot)
{
	Connection* conn = &g_conns[slot];
	(void)conn.sock.close();
	conn.state = UNUSED;
	conn.read_len = 0;
}

// ============================================================================
// 5. Request Processing & Response Dispatching
// ============================================================================

fn void process_request_task(sz slot) => @pool()
{
	Connection* conn = &g_conns[slot];

	Clock start_time = clock::now();
	int status_code = 0;
	long response_size = 0;

	String request_data = (String)conn.read_buf[:conn.read_len];

	ParsedRequest? parsed_req = parse_request(request_data);
	if (catch parsed_req)
	{
		make_and_dispatch_error(conn, slot, 400, "Bad Request", &status_code, &response_size, false);
		log_request(conn, start_time, status_code, {}, response_size);
		return;
	}

	conn.keep_alive = parsed_req.keep_alive;

	String raw_path = parsed_req.path;
	$if env::WIN32:
		raw_path = raw_path.treplace("\\", "/");
	$endif

	Splitter segments = raw_path.tokenize("/");
	while (try seg = segments.next())
	{
		if (seg == "..")
		{
			make_and_dispatch_error(conn, slot, 403, "Forbidden", &status_code, &response_size, parsed_req.keep_alive);
			log_request(conn, start_time, status_code, parsed_req, response_size);
			return;
		}
	}

	if (raw_path == "/favicon.ico" && !file::exists("favicon.ico"))
	{
		String conn_str = parsed_req.keep_alive ? "keep-alive" : "close";
		String resp = string::tformat("HTTP/1.1 204 No Content\r\nConnection: %s\r\n\r\n", conn_str);
		dispatch_response_bytes(conn, slot, resp);
		status_code = 204;
		log_request(conn, start_time, status_code, parsed_req, response_size);
		return;
	}

	String rel = raw_path.len > 1 ? raw_path[1..] : ".";
	bool is_dir = file::is_dir(rel);

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
			serve_dir_listing(conn, slot, rel, &parsed_req, &status_code, &response_size);
			log_request(conn, start_time, status_code, parsed_req, response_size);
			return;
		}
	}

	serve_static_file(conn, slot, rel, &parsed_req, &status_code, &response_size);
	log_request(conn, start_time, status_code, parsed_req, response_size);
}

fn bool send_all_bytes(TcpSocket* sock, char[] data)
{
	sz sent = 0;
	while (sent < data.len)
	{
		sz chunk_len = math::min(data.len - sent, (sz)(128 * 1024));

		sz? n = sock.write(data[sent : chunk_len]);
		if (catch err = n)
		{
			if (err == io::WOULD_BLOCK || err == io::INTERRUPTED)
			{
				Poll[1] p = { { .socket = sock.sock, .events = WRITE } };
				if ((net::poll_ms(&p, 5000) ?? 0) == 0) return false;
				continue;
			}
			return false;
		}
		if (n == 0)
		{
			Poll[1] p = { { .socket = sock.sock, .events = WRITE } };
			if ((net::poll_ms(&p, 5000) ?? 0) == 0) return false;
			continue;
		}
		sent += n;
	}
	return true;
}

fn void dispatch_response_bytes(Connection* conn, sz slot, char[] data)
{
	bool ok = send_all_bytes(&conn.sock, data);
	if (!ok)
	{
		close_conn(slot);
		return;
	}

	if (conn.keep_alive)
	{
		conn.read_len = 0;
		conn.last_activity = time::now();
		conn.state = READING;
		notify_poller();
	}
	else
	{
		close_conn(slot);
	}
}

fn void make_and_dispatch_error(Connection* conn, sz slot, int code, String reason, int* out_status, long* out_size, bool keep_alive)
{
	String body = string::tformat("<h1>%d %s</h1>", code, reason);
	String conn_str = keep_alive ? "keep-alive" : "close";
	String resp = string::tformat("HTTP/1.1 %d %s\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: %d\r\nConnection: %s\r\n\r\n%s",
		code, reason, body.len, conn_str, body);

	if (out_status) *out_status = code;
	if (out_size) *out_size = body.len;

	dispatch_response_bytes(conn, slot, resp);
}

fn void log_request(Connection* conn, Clock start_time, int status_code, ParsedRequest parsed_req, long response_size)
{
	if (status_code == 0) return;

	String date_str = datetime::from_time_tz(time::now(), 0).format(tmem, RFC3339);
	NanoDuration elapsed = start_time.to_now();
	String human_s = human_size(response_size);
	String ip_str = conn.sock.peer_address().to_tstring() ?? "127.0.0.1";

	io::printfn("[%s] %3d %-6s %-32s %8s %8s  %s",
		date_str, status_code,
		parsed_req.method ?: "-",
		parsed_req.path ?: "-",
		elapsed.tunit_str(), human_s, ip_str);
}

// ============================================================================
// 6. Static File & Directory Serving
// ============================================================================

fn void serve_dir_listing(Connection* conn, sz slot, String rel_path, ParsedRequest* req, int* out_status, long* out_size)
{
	Path dir_path = path::tnew(rel_path) ?? (Path)rel_path;
	PathList? files = path::ls(tmem, dir_path, relative: false);
	if (catch files)
	{
		make_and_dispatch_error(conn, slot, 500, "Internal Server Error", out_status, out_size, req.keep_alive);
		return;
	}

	FileEntry[] entries = mem::talloc_array(FileEntry, files.len());
	sz count = 0;
	foreach (entry_path : files)
	{
		String name = entry_path.basename();
		if (name == "." || name == "..") continue;

		entries[count] = {
			.path = entry_path,
			.name = name,
			.is_dir = file::is_dir(entry_path)
		};
		count++;
	}

	FileEntry[] entries_to_sort = entries[:count];
	sort::quicksort(entries_to_sort, &entry_cmp);

	DString html = dstring::temp_with_capacity(32768);
	html.appendf(DIR_LISTING_TEMPLATE_HEADER, FAVICON, req.path, req.path);

	if (req.path != "/") html.append(DIR_LISTING_GO_UP);

	html.append(DIR_LISTING_TABLE_HEADER);

	foreach (entry : entries_to_sort)
	{
		long size_raw = entry.is_dir ? -1 : (file::get_size(entry.path) ?? 0);
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
	}

	html.append(DIR_LISTING_TABLE_FOOTER);
	String out_html = html.str_view();

	String date_now = datetime::from_time_tz(time::now(), 0).format(tmem, RFC1123);
	String conn_str = req.keep_alive ? "keep-alive" : "close";

	if (req.accept_gzip)
	{
		if (try compressed = gzip::compress(tmem, out_html))
		{
			String headers = string::tformat("HTTP/1.1 200 OK\r\nDate: %s\r\nContent-Type: text/html; charset=utf-8\r\nContent-Encoding: gzip\r\nContent-Length: %d\r\nConnection: %s\r\n\r\n",
				date_now, compressed.len, conn_str);

			if (req.method == "HEAD")
			{
				dispatch_response_bytes(conn, slot, headers);
			}
			else
			{
				char[] full_resp = mem::talloc_array(char, headers.len + compressed.len);
				full_resp[:headers.len] = headers[..];
				full_resp[headers.len..] = compressed[..];
				dispatch_response_bytes(conn, slot, full_resp);
			}
			if (out_status) *out_status = 200;
			if (out_size) *out_size = compressed.len;
			return;
		}
	}

	String headers = string::tformat("HTTP/1.1 200 OK\r\nDate: %s\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: %d\r\nConnection: %s\r\n\r\n",
		date_now, out_html.len, conn_str);

	if (req.method == "HEAD")
	{
		dispatch_response_bytes(conn, slot, headers);
	}
	else
	{
		char[] full_resp = mem::talloc_array(char, headers.len + out_html.len);
		full_resp[:headers.len] = headers[..];
		full_resp[headers.len..] = out_html[..];
		dispatch_response_bytes(conn, slot, full_resp);
	}

	if (out_status) *out_status = 200;
	if (out_size) *out_size = out_html.len;
}

fn void serve_static_file(Connection* conn, sz slot, String path_str, ParsedRequest* req, int* out_status, long* out_size)
{
	File? target_file = file::open(path_str, "rb");
	if (catch err = target_file)
	{
		if (err == io::FILE_NOT_FOUND)
		{
			make_and_dispatch_error(conn, slot, 404, "Not Found", out_status, out_size, req.keep_alive);
			return;
		}
		make_and_dispatch_error(conn, slot, 500, "Internal Server Error", out_status, out_size, req.keep_alive);
		return;
	}
	defer (void)target_file.close();

	long size = target_file.size() ?? 0;
	Time mod = file::last_modified(path_str) ?? (Time)0;
	String etag = string::tformat("\"%d-%d\"", mod, size);

	if (req.if_none_match.len > 0 && req.if_none_match == etag)
	{
		String date_now = datetime::from_time_tz(time::now(), 0).format(tmem, RFC1123);
		String conn_str = req.keep_alive ? "keep-alive" : "close";
		String resp = string::tformat("HTTP/1.1 304 Not Modified\r\nDate: %s\r\nETag: %s\r\nConnection: %s\r\n\r\n",
			date_now, etag, conn_str);

		dispatch_response_bytes(conn, slot, resp);
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

	Path file_path = path::tnew(path_str) ?? (Path)path_str;
	String mime_type = mime_for_ext((file_path.extension() ?? "").to_lower_tcopy());
	bool compress = req.accept_gzip && !is_partial && is_compressible(mime_type);
	long content_len = is_partial ? (end - start + 1) : size;

	String date_now = datetime::from_time_tz(time::now(), 0).format(tmem, RFC1123);
	String conn_str = req.keep_alive ? "keep-alive" : "close";

	if (is_partial)
	{
		String headers = string::tformat(
			"HTTP/1.1 206 Partial Content\r\n"
			"Date: %s\r\n"
			"Content-Type: %s\r\n"
			"Accept-Ranges: bytes\r\n"
			"Content-Length: %d\r\n"
			"Content-Range: bytes %d-%d/%d\r\n"
			"ETag: %s\r\n"
			"Connection: %s\r\n\r\n",
			date_now, mime_type, content_len, start, end, size, etag, conn_str);

		if (out_status) *out_status = 206;
		if (out_size) *out_size = content_len;

		if (req.method == "HEAD")
		{
			dispatch_response_bytes(conn, slot, headers);
			return;
		}

		// Memory-mapped zero-copy slice for partial content
		if (content_len > 64 * 1024)
		{
			if (try map = file::mmap_file(target_file, (sz)start, (sz)content_len))
			{
				defer (void)map.destroy();
				bool ok = send_all_bytes(&conn.sock, headers) &&
				          send_all_bytes(&conn.sock, map.bytes());

				if (!ok)
				{
					close_conn(slot);
					return;
				}

				if (conn.keep_alive)
				{
					conn.read_len = 0;
					conn.last_activity = time::now();
					conn.state = READING;
					notify_poller();
				}
				else
				{
					close_conn(slot);
				}
				return;
			}
		}

		char[] file_bytes = mem::talloc_array(char, (sz)content_len);
		(void)target_file.seek(start);
		sz valid_read = target_file.read(file_bytes) ?? 0;

		char[] full_resp = mem::talloc_array(char, headers.len + valid_read);
		full_resp[:headers.len] = headers[..];
		full_resp[headers.len..] = file_bytes[:valid_read];

		dispatch_response_bytes(conn, slot, full_resp);
		return;
	}

	if (compress)
	{
		if (try file_data = file::load(tmem, path_str))
		{
			if (try compressed = gzip::compress(tmem, file_data))
			{
				String headers = string::tformat(
					"HTTP/1.1 200 OK\r\n"
					"Date: %s\r\n"
					"Content-Type: %s\r\n"
					"Accept-Ranges: bytes\r\n"
					"ETag: %s\r\n"
					"Content-Encoding: gzip\r\n"
					"Content-Length: %d\r\n"
					"Connection: %s\r\n\r\n",
					date_now, mime_type, etag, compressed.len, conn_str);

				if (out_status) *out_status = 200;
				if (out_size) *out_size = compressed.len;

				if (req.method == "HEAD")
				{
					dispatch_response_bytes(conn, slot, headers);
					return;
				}

				char[] full_resp = mem::talloc_array(char, headers.len + compressed.len);
				full_resp[:headers.len] = headers[..];
				full_resp[headers.len..] = compressed[..];
				dispatch_response_bytes(conn, slot, full_resp);
				return;
			}
		}
	}

	// Zero-copy Memory-Mapped streaming for files > 64 KB
	if (size > 64 * 1024)
	{
		if (try map = file::mmap_file(target_file, 0, (sz)size))
		{
			defer (void)map.destroy();

			String headers = string::tformat(
				"HTTP/1.1 200 OK\r\n"
				"Date: %s\r\n"
				"Content-Type: %s\r\n"
				"Accept-Ranges: bytes\r\n"
				"ETag: %s\r\n"
				"Content-Length: %d\r\n"
				"Connection: %s\r\n\r\n",
				date_now, mime_type, etag, size, conn_str);

			if (out_status) *out_status = 200;
			if (out_size) *out_size = size;

			if (req.method == "HEAD")
			{
				dispatch_response_bytes(conn, slot, headers);
				return;
			}

			bool ok = send_all_bytes(&conn.sock, headers) &&
			          send_all_bytes(&conn.sock, map.bytes());

			if (!ok)
			{
				close_conn(slot);
				return;
			}

			if (conn.keep_alive)
			{
				conn.read_len = 0;
				conn.last_activity = time::now();
				conn.state = READING;
				notify_poller();
			}
			else
			{
				close_conn(slot);
			}
			return;
		}
	}

	String headers = string::tformat(
		"HTTP/1.1 200 OK\r\n"
		"Date: %s\r\n"
		"Content-Type: %s\r\n"
		"Accept-Ranges: bytes\r\n"
		"ETag: %s\r\n"
		"Content-Length: %d\r\n"
		"Connection: %s\r\n\r\n",
		date_now, mime_type, etag, size, conn_str);

	if (out_status) *out_status = 200;
	if (out_size) *out_size = size;

	if (req.method == "HEAD")
	{
		dispatch_response_bytes(conn, slot, headers);
		return;
	}

	if (try file_data = file::load(tmem, path_str))
	{
		char[] full_resp = mem::talloc_array(char, headers.len + file_data.len);
		full_resp[:headers.len] = headers[..];
		full_resp[headers.len..] = file_data[..];
		dispatch_response_bytes(conn, slot, full_resp);
	}
	else
	{
		make_and_dispatch_error(conn, slot, 500, "Internal Server Error", out_status, out_size, req.keep_alive);
	}
}

// ============================================================================
// 7. Request Parsing & Mime Utilities
// ============================================================================

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

excuse MALFORMED_REQUEST;

fn ParsedRequest? parse_request(String req)
{
	ParsedRequest parsed_req;
	Splitter lines = req.tokenize_all("\r\n");
	String request_line = lines.next() ?? MALFORMED_REQUEST~!;

	switch
	{
		case request_line.starts_with("GET "):  parsed_req.method = "GET";
		case request_line.starts_with("HEAD "): parsed_req.method = "HEAD";
		default: return MALFORMED_REQUEST~;
	}

	String rest = request_line[parsed_req.method.len + 1..];
	sz space_idx = rest.index_of_char(' ') ?? MALFORMED_REQUEST~!;

	String raw_path = rest[:space_idx];
	String version = rest[space_idx + 1..].trim();
	parsed_req.keep_alive = !version.ends_with("1.0");

	String path_part = raw_path;
	if (try query_idx = raw_path.index_of_char('?'))
	{
		path_part = raw_path[:query_idx];
		parsed_req.query = raw_path[query_idx + 1..];
	}
	parsed_req.path = url::tdecode(path_part, PATH) ?? path_part;

	while (try line = lines.next() && line.len)
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
	double scaled_size = size / 1024.0;
	if (scaled_size < 1024.0) return string::tformat("%.1fKB", scaled_size);
	scaled_size /= 1024.0;
	if (scaled_size < 1024.0) return string::tformat("%.1fMB", scaled_size);
	scaled_size /= 1024.0;
	return string::tformat("%.1fGB", scaled_size);
}

fn String html_escape(String input_string)
{
	char[] buf = mem::talloc_array(char, input_string.len * 6);
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
	return (String)buf[:pos];
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

// ============================================================================
// 8. HTML Templates
// ============================================================================

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