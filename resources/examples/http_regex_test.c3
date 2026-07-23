<* Regex Tester Server *>
module test_regex;

import std, libc;

const int THREAD_COUNT = 8;

// Clean up SIGPIPE errors under Unix-like targets
fn void ignore_sigpipe() @init(1001) @if(!env::WIN32)
{
	// c3fmt off
	libc::signal(libc::SIGPIPE, fn void (int) {});
	// c3fmt on
}

struct RegexQuery
{
	String regex;
	String data;
	String op;
	String replace_with;
}

struct ParsedRequest
{
	String method;
	String path;
	String query;
	bool keep_alive;
}

faultdef MALFORMED_REQUEST;

fn int main(String[] args)
{
	int port                = 8080;
	TcpServerSocket? server = tcp::listen("0.0.0.0", port, 128, REUSEADDR);
	if (catch err = server)
	{
		io::eprintfn("Failed to listen on port %d: %s", port, err);
		return 1;
	}

	io::printfn("Regex Tester running at http://127.0.0.1:%d", port);

	ThreadGroup{TcpSocket*, int} worker_pool;
	// ThreadGroup{TcpSocket*, void} worker_pool; // not working: https://github.com/c3lang/c3c/blob/master/lib/std/threads/threadgroup.c3#L461
	if (catch err = worker_pool.init(mem, &handle_thread, THREAD_COUNT))
	{
		io::eprintfn("Failed to initialize thread group: %s", err);
		return 1;
	}
	worker_pool.start()!!;
	defer worker_pool.destroy();

	while (true)
	{
		TcpSocket? conn = tcp::accept(&server);
		if (catch conn) continue;

		TcpSocket* conn_ptr = mem::new(TcpSocket);
		*conn_ptr = conn;

		worker_pool.add_task(conn_ptr);
	}
}

// fn void handle_thread(TcpSocket* conn_ptr, void* context) => @pool_init(mem, 65536) // oops, need to return something
fn int handle_thread(TcpSocket* conn_ptr, void* context) => @pool_init(mem, 65536)
{
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
		Poll[1] p   = { { conn.sock, READ, (PollEvent)0 } };
		ulong ready = net::poll_ms(p[..], 5000) ?? 0;
		if (ready == 0) return;

		@pool()
		{
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

			ParsedRequest parsed_req;
			if (try req_val = parse_request(request_data))
			{
				parsed_req = req_val;
			}
			else
			{
				send_error(&conn, 400, "Bad Request")!;
				return;
			}

			RegexQuery rq = parse_query_params(parsed_req.query);

			// defaults for testing
			if (rq.regex.len == 0 && rq.data.len == 0)
			{
				rq.regex = "(\\w+)@(\\w+)\\.(\\w+)";
				rq.data = "John Doe <john.doe@example.com> (555-123-4567) - 2026-07-16\nJane Smith <jane_smith@domain.co.uk> (555-987-6543) - 1999-12-31\nInvalid email: admin@localhost, test@com";
				rq.op = "find";
			}

			String result_html = "";
			if (rq.regex.len > 0)
			{
				result_html = execute_regex_operation(rq);
			}

			String html_content = generate_html(rq, result_html);

			conn.write(
				string::tformat(
					"HTTP/1.1 200 OK\r\n" "Date: %s\r\n" "Content-Type: text/html; charset=utf-8\r\n" "Content-Length: %d\r\n" "Connection: %s\r\n\r\n" "%s",
					datetime::from_time_tz(time::now(), 0)
				.format(tmem, RFC1123), html_content.len, parsed_req.keep_alive ? "keep-alive" : "close", html_content))!;

			if (!parsed_req.keep_alive) return;
		};
	}
}

fn ParsedRequest? parse_request(String req)
{
	ParsedRequest parsed_req;
	sz? first_crlf = req.index_of("\r\n");
	if (catch first_crlf) return MALFORMED_REQUEST~;

	String first_line = req[:first_crlf];
	Splitter parts    = first_line.tokenize(" ");
	String method     = parts.next() ?? "";
	String raw_path   = parts.next() ?? "";

	if (method.len == 0 || raw_path.len == 0) return MALFORMED_REQUEST~;
	if (method != "GET" && method != "HEAD") return MALFORMED_REQUEST~;

	parsed_req.method = method;
	parsed_req.keep_alive = false;

	if (try query_idx = raw_path.index_of_char('?'))
	{
		parsed_req.path = raw_path[:query_idx];
		parsed_req.query = raw_path[query_idx + 1..];
	}
	else
	{
		parsed_req.path = raw_path;
	}

	String headers_part = req[first_crlf + 2..];
	Splitter lines      = headers_part.tokenize("\r\n");
	while (try line = lines.next())
	{
		if (try col = line.index_of_char(':'))
		{
			String name = line[:col].trim();
			String val  = line[col + 1..].trim();
			switch (name.to_lower_tcopy())
			{
				case "connection":
					if (val.to_lower_tcopy() == "close") parsed_req.keep_alive = false;
					if (val.to_lower_tcopy() == "keep-alive") parsed_req.keep_alive = true;
			}
		}
	}

	return parsed_req;
}

fn void? send_error(TcpSocket* sock, int code, String reason)
{
	String body = string::tformat("<h1>%d %s</h1>", code, reason);
	sock.write(string::tformat("HTTP/1.1 %d %s\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s", code, reason, body.len, body))!;
}

fn String get_first_param(UrlQueryValues* vals, String key, String default_val = "")
{
	if (try list = vals.map.get(key)) return list[0];
	return default_val;
}

fn RegexQuery parse_query_params(String query)
{
	RegexQuery rq = { "", "", "find", "" };
	if (query.len == 0) return rq;

	UrlQueryValues vals = url::parse_query_to_temp(query);
	rq.regex = get_first_param(&vals, "regex");
	rq.data = get_first_param(&vals, "data");
	rq.op = get_first_param(&vals, "op", "find");
	rq.replace_with = get_first_param(&vals, "replace_with");
	return rq;
}

fn String html_escape(String input_string)
{
	if (input_string.len == 0) return "";

	DString ds;
	ds.tinit();

	foreach (character : input_string)
	{
		switch (character)
		{
			case '&': ds.append("&amp;");
			case '<': ds.append("&lt;");
			case '>': ds.append("&gt;");
			case '"': ds.append("&quot;");
			case '\'': ds.append("&#39;");
			default: ds.append_char(character);
		}
	}
	return ds.copy_str(tmem);
}

fn String build_highlighted_text(String data, regex::Match[] matches)
{
	DString ds;
	ds.tinit();
	sz last_offset = 0;
	foreach (m : matches)
	{
		int start = m.start();
		int end   = m.end();
		if (start >= (int)last_offset)
		{
			ds.append(html_escape(data[last_offset:(sz)start - last_offset]));
			ds.append("<span class=\"match-highlight\">");
			ds.append(html_escape(data[(sz)start:(sz)(end - start)]));
			ds.append("</span>");
			last_offset = (sz)end;
		}
	}
	if (last_offset < data.len)
	{
		ds.append(html_escape(data[last_offset..]));
	}
	return ds.copy_str(tmem);
}

fn String execute_regex_operation(RegexQuery rq)
{
	if (rq.regex.len == 0) return "";

	DString result_ds;
	result_ds.tinit();

	regex::CompileFlags flags = regex::CompileFlags.EXTENDED;

	regex::Regex? re_opt = regex::compile(tmem, rq.regex, flags);
	if (catch err = re_opt)
	{
		result_ds.appendf("<div style=\"color: #cc0000; font-weight: bold;\">Regex Compile Error: %s</div>", regex::error_string(err));
		return result_ds.copy_str(tmem);
	}
	regex::Regex re = re_opt;
	defer re.free();

	switch (rq.op)
	{
		case "matches":

			{
				bool is_match = re.matches(rq.data);
				result_ds.appendf("<p>Matches: <b style=\"color: %s;\">%s</b></p>", is_match ? "#008000" : "#cc0000", is_match ? "TRUE" : "FALSE");
			}
		case "find":

			{
				regex::Match[]? matches_opt = re.find_all(tmem, rq.data);
				if (catch err = matches_opt)
				{
					result_ds.appendf("<div style=\"color: #cc0000;\">Execution Error: %s</div>", regex::error_string(err));
				}
				else
				{
					regex::Match[] matches = matches_opt;
					if (matches.len == 0)
					{
						result_ds.append("<p style=\"color: #cc0000; font-weight: bold;\">No matches found.</p>");
					}
					else
					{
						result_ds.appendf("<p style=\"color: #008000; font-weight: bold;\">Found %d non-overlapping matches:</p>", matches.len);
						result_ds.append("<ol style=\"padding-left: 1.5rem;\">");
						foreach (m : matches)
						{
							result_ds.appendf("<li>Match: <code>\"%s\"</code> at byte offsets [%d .. %d]</li>", html_escape(m.str), m.start(), m.end());
						}
						result_ds.append("</ol>");

						result_ds.append("<p><b>Visualized Matches:</b></p><pre>");
						result_ds.append(build_highlighted_text(rq.data, matches));
						result_ds.append("</pre>");
					}
				}
			}
		case "replace":

			{
				String? replaced_opt = re.replace(tmem, rq.data, rq.replace_with, true);
				if (catch err = replaced_opt)
				{
					result_ds.appendf("<div style=\"color: #cc0000;\">Execution Error: %s</div>", regex::error_string(err));
				}
				else
				{
					String replaced = replaced_opt;
					result_ds.append("<p><b>Resulting Text:</b></p>");
					result_ds.appendf("<pre>%s</pre>", html_escape(replaced));
				}
			}
		case "split":

			{
				String[]? parts_opt = re.split(tmem, rq.data);
				if (catch err = parts_opt)
				{
					result_ds.appendf("<div style=\"color: #cc0000;\">Execution Error: %s</div>", regex::error_string(err));
				}
				else
				{
					String[] parts = parts_opt;
					result_ds.appendf("<p style=\"color: #008000; font-weight: bold;\">Split into %d parts:</p>", parts.len);
					result_ds.append("<ol style=\"padding-left: 1.5rem;\">");
					foreach (p : parts)
					{
						result_ds.appendf("<li><code>\"%s\"</code></li>", html_escape(p));
					}
					result_ds.append("</ol>");
				}
			}
		default:
			result_ds.appendf("<div style=\"color: #cc0000;\">Unknown operation: %s</div>", html_escape(rq.op));
	}

	return result_ds.copy_str(tmem);
}

fn String generate_html(RegexQuery rq, String result_html)
{
	DString ds;
	ds.tinit();

	String escaped_regex        = html_escape(rq.regex);
	String escaped_data         = html_escape(rq.data);
	String escaped_replace_with = html_escape(rq.replace_with);

	ds.appendf(HTML_PART_1, escaped_regex, escaped_data);

	String[4] ops       = { "find", "replace", "split", "matches" };
	String[4] op_labels = { "Find All Matches", "Replace Matches", "Split String", "Test Match (bool)" };
	for (int i = 0; i < 4; i++)
	{
		ds.appendf("						<option value=\"%s\"%s>%s</option>\n", ops[i], (rq.op == ops[i]) ? " selected" : "", op_labels[i]);
	}

	ds.appendf(HTML_PART_2, escaped_replace_with);

	if (result_html.len > 0)
	{
		ds.appendf(HTML_RESULT_WRAPPER, result_html);
	}

	ds.append(HTML_PART_3);

	return ds.copy_str(tmem);
}

// HTML Page Templates

const String HTML_PART_1 = `<!DOCTYPE html>
<html>
<head>
	<meta charset="UTF-8">
	<title>C3 Regex Tester</title>
	<style>
		body {
			font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
			background-color: #f5f5f7;
			color: #1d1d1f;
			margin: 0;
			padding: 2rem;
			display: flex;
			justify-content: center;
		}
		.container {
			background: #ffffff;
			padding: 2.5rem;
			border-radius: 12px;
			box-shadow: 0 4px 12px rgba(0,0,0,0.08);
			width: 100%%;
			max-width: 700px;
		}
		h1 {
			font-size: 1.8rem;
			margin-bottom: 1.5rem;
			text-align: center;
			color: #333;
		}
		.form-group {
			margin-bottom: 1.2rem;
		}
		label {
			display: block;
			font-weight: 600;
			margin-bottom: 0.5rem;
			font-size: 0.9rem;
		}
		input[type="text"], textarea, select {
			width: 100%%;
			padding: 0.75rem;
			border: 1px solid #ccc;
			border-radius: 6px;
			box-sizing: border-box;
			font-size: 1rem;
			font-family: inherit;
		}
		textarea {
			resize: vertical;
			min-height: 120px;
		}
		.row {
			display: flex;
			gap: 1rem;
		}
		.row .form-group {
			flex: 1;
		}
		button {
			background-color: #0071e3;
			color: white;
			border: none;
			padding: 0.75rem 1.5rem;
			font-size: 1rem;
			font-weight: 600;
			border-radius: 6px;
			cursor: pointer;
			width: 100%%;
			transition: background-color 0.2s;
		}
		button:hover {
			background-color: #0077ed;
		}
		.results-box {
			margin-top: 2rem;
			padding: 1.5rem;
			background-color: #f9f9fb;
			border-left: 4px solid #0071e3;
			border-radius: 4px;
		}
		.results-title {
			font-weight: bold;
			margin-bottom: 0.8rem;
			font-size: 1.1rem;
		}
		pre {
			background: #ebebeb;
			padding: 0.8rem;
			border-radius: 4px;
			overflow-x: auto;
			font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
			font-size: 0.9rem;
			margin: 0;
			white-space: pre-wrap;
		}
		.match-highlight {
			background-color: #ffe066;
			font-weight: bold;
			border-radius: 2px;
			padding: 1px 2px;
			color: #1d1d1f;
		}
	</style>
	<script>
		function toggleReplace() {
			var op = document.getElementById('op').value;
			var replGroup = document.getElementById('replace-group');
			if (op === 'replace') {
				replGroup.style.display = 'block';
			} else {
				replGroup.style.display = 'none';
			}
		}

		function applyPreset() {
			var preset = document.getElementById('presets').value;
			if (preset) {
				document.getElementById('regex').value = preset;
			}
		}

		window.onload = function() {
			toggleReplace();
		};
	</script>
</head>
<body>
	<div class="container">
		<h1>C3 Regex Tester</h1>
		<form method="GET" action="/">
			<div class="form-group">
				<label for="presets">Quick Regex Presets</label>
				<select id="presets" onchange="applyPreset()">
					<option value="">-- Choose a preset pattern --</option>
					<option value="(\w+)@(\w+)\.(\w+)">Email Address (Simple)</option>
					<option value="\d{4}-\d{2}-\d{2}">Date (YYYY-MM-DD)</option>
					<option value="\d{3}-\d{3}-\d{4}">Phone Number (555-XXX-XXXX)</option>
					<option value="\b[jJ]\w+">Words starting with 'J'</option>
					<option value="&lt;[^&gt;]+&gt;">HTML Tag (&lt;...&gt;)</option>
					<option value="\w+">Word Characters</option>
				</select>
			</div>
			<div class="form-group">
				<label for="regex">Regex Pattern</label>
				<input type="text" id="regex" name="regex" value="%s" placeholder="e.g. (\\w+)@(\\w+)" required>
			</div>
			<div class="form-group">
				<label for="data">Test Data / Text</label>
				<textarea id="data" name="data" placeholder="Enter text to match against..." required>%s</textarea>
			</div>
			<div class="row">
				<div class="form-group">
					<label for="op">Operation</label>
					<select id="op" name="op" onchange="toggleReplace()">
`;

const String HTML_PART_2 = `					</select>
				</div>
				<div class="form-group" id="replace-group" style="display: none;">
					<label for="replace_with">Replace With</label>
					<input type="text" id="replace_with" name="replace_with" value="%s" placeholder="Replacement text">
				</div>
			</div>
			<button type="submit">Run Regex</button>
		</form>
`;

const String HTML_RESULT_WRAPPER = `		<div class="results-box">
			<div class="results-title">Execution Results</div>
			%s
		</div>
`;

const String HTML_PART_3 = `	</div>
</body>
</html>
`;
