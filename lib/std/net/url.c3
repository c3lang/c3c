module std::net::url;

import std::io, std::collections::map, std::collections::list;

fault UrlParsingResult
{
	EMPTY,
	INVALID_SCHEME,
	INVALID_USER,
	INVALID_PASSWORD,
	INVALID_HOST,
	INVALID_PATH,
	INVALID_FRAGMENT,
}

<*
 Represents the actual (decoded) Url.

 An Url can be parsed from a String with `new_parse()` or `temp_parse()`. The
 parsed fields are decoded. The only field that is not decoded is `query`.
 To access the decoded query values, use `new_parse_query(query)`.

 `Url.to_string()` will re-assemble the fields into a valid Url string with
 proper percent-encoded values.

 If the Url struct fields are filled in manually, use the actual (un-encoded)
 values. To create a raw query string, initialize an `UrlQueryValues` map, use
 `UrlQueryValues.add()` to add the query parameters and, finally, call
 `UrlQueryValues.to_string()`.
*>
struct Url(Printable)
{
	String scheme;
	String host;
	uint   port;
	String username;
	String password;
	String path;
	String query;
	String fragment;

	Allocator allocator;
}

<*
 Parse a URL string into a Url struct.

 @param [in] url_string
 @require url_string.len > 0 "the url_string must be len 1 or more"
 @return "the parsed Url"
*>
fn Url! temp_parse(String url_string) => new_parse(url_string, allocator::temp());

<*
 Parse a URL string into a Url struct.

 @param [in] url_string
 @require url_string.len > 0 "the url_string must be len 1 or more"
 @return "the parsed Url"
*>
fn Url! new_parse(String url_string, Allocator allocator = allocator::heap())
{
	url_string = url_string.trim();
	if (!url_string) return UrlParsingResult.EMPTY?;
	Url url = { .allocator = allocator };

	// Parse scheme
	if (try pos = url_string.index_of("://"))
	{
		if (!pos) return UrlParsingResult.INVALID_SCHEME?;
		url.scheme = url_string[:pos].copy(allocator);
		url_string = url_string[url.scheme.len + 3 ..];
	}
	else if (try pos = url_string.index_of(":"))
	{
		// Handle schemes without authority like 'mailto:'
		if (!pos) return UrlParsingResult.INVALID_SCHEME?;
		url.scheme = url_string[:pos].copy(allocator);
		url.path = decode(url_string[pos + 1 ..], PATH, allocator) ?? UrlParsingResult.INVALID_PATH?!;
		return url;
	}

	// Parse host, port
	if (url.scheme != "urn")
	{
		usz authority_end = url_string.index_of_chars("/?#") ?? url_string.len;
		String authority = url_string[:authority_end];

		if (try user_info_end = authority.index_of_char('@'))
		{
			String userinfo = authority[:user_info_end];
			String username @noinit;
			String password;
			@pool(allocator)
			{
				String[] userpass = userinfo.tsplit(":", 2);
				username = userpass[0];
				if (!username.len) return UrlParsingResult.INVALID_USER?;
							url.host =

				url.username = decode(username, HOST, allocator) ?? UrlParsingResult.INVALID_USER?!;
				if (userpass.len) url.password = decode(userpass[1], USERPASS, allocator) ?? UrlParsingResult.INVALID_PASSWORD?!;
			};
			authority = authority[userinfo.len + 1 ..];
		}

		// Check for IPv6 address in square brackets
		String host;
		if (authority.starts_with("[") && authority.contains("]"))
		{
			usz ipv6_end = authority.index_of("]")!;
			host = authority[0 .. ipv6_end];  // Includes closing bracket
			if ((ipv6_end + 1) < authority.len && authority[.. ipv6_end] == ":")
			{
				url.port = authority[.. ipv6_end + 1].to_uint()!;
			}
		}
		else
		{
			@pool(allocator)
			{
				String[] host_port = authority.tsplit(":", 2);
				if (host_port.len > 1)
				{
					host = host_port[0];
					url.port = host_port[1].to_uint()!;
				}
				else
				{
					host = authority;
				}
			};
		}
		url.host = decode(host, HOST, allocator) ?? UrlParsingResult.INVALID_HOST?!;
		url_string = url_string[authority_end ..];
	}

	// Parse path
	usz! query_index = url_string.index_of_char('?');
	usz! fragment_index = url_string.index_of_char('#');

	if (@ok(query_index) || @ok(fragment_index))
	{
		usz path_end = min(query_index ?? url_string.len, fragment_index ?? url_string.len);
		url.path = decode(url_string[:path_end], PATH, allocator) ?? UrlParsingResult.INVALID_PATH?!;
		url_string = url_string[path_end ..];
	}
	else
	{
		url.path = decode(url_string, PATH, allocator) ?? UrlParsingResult.INVALID_PATH?!;
		url_string = "";
	}

	// Remove the path part from url for further parsing


	// Parse query
	if (url_string.starts_with("?"))
	{
		usz index = url_string.index_of_char('#') ?? url_string.len;
		url.query = url_string[1 .. index - 1].copy(allocator);
		url_string = url_string[index ..];
	}

	// Parse fragment
	if (url_string.starts_with("#"))
	{
		url.fragment = decode(url_string[1..], FRAGMENT, allocator) ?? UrlParsingResult.INVALID_FRAGMENT?!;
	}
	return url;
}

<*
 Stringify a Url struct.

 @param [in] self
 @param [inout] allocator
 @return "Url as a string"
*>
fn String Url.to_string(&self, Allocator allocator = allocator::heap()) @dynamic
{
	@pool(allocator)
	{
		DString builder = dstring::temp_new();

		// Add scheme if it exists
		if (self.scheme != "")
		{
			builder.append_chars(self.scheme);
			builder.append_char(':');
			if (self.host.len > 0) builder.append_chars("//");
		}

		// Add username and password if they exist
		if (self.username != "")
		{
			String username = temp_encode(self.username, USERPASS);
			builder.append_chars(username);

			if (self.password != "")
			{
				builder.append_char(':');

				String password = temp_encode(self.password, USERPASS);
				builder.append_chars(password);
			}
			builder.append_char('@');
		}

		// Add host
		String host = temp_encode(self.host, HOST);
		builder.append_chars(host);

		// Add port
		if (self.port != 0)
		{
			builder.append_char(':');
			builder.appendf("%d", self.port);
		}

		// Add path
		String path = temp_encode(self.path, PATH);
		builder.append_chars(path);

		// Add query if it exists (note that `query` is expected to
		// be already properly encoded).
		if (self.query != "")
		{
			builder.append_char('?');
			builder.append_chars(self.query);
		}

		// Add fragment if it exists
		if (self.fragment != "")
		{
			builder.append_char('#');

			String fragment = temp_encode(self.fragment, FRAGMENT);
			builder.append_chars(fragment);
		}

		return builder.copy_str(allocator);
	};
}

def UrlQueryValueList = List(<String>);

struct UrlQueryValues
{
	inline HashMap(<String, UrlQueryValueList>) map;
	UrlQueryValueList key_order;
}

<*
 Parse the query parameters of the Url into a UrlQueryValues map.

 @param [in] query
 @return "a UrlQueryValues HashMap"
*>
fn UrlQueryValues temp_parse_query(String query) => parse_query(query, allocator::temp());

<*
 Parse the query parameters of the Url into a UrlQueryValues map.

 @param [in] query
 @return "a UrlQueryValues HashMap"
*>
fn UrlQueryValues new_parse_query(String query) => parse_query(query, allocator::heap());

<*
 Parse the query parameters of the Url into a UrlQueryValues map.

 @param [in] query
 @param [inout] allocator
 @return "a UrlQueryValues HashMap"
*>
fn UrlQueryValues parse_query(String query, Allocator allocator)
{
	UrlQueryValues vals;
	vals.map.init(allocator);
	vals.key_order.new_init(allocator: allocator);

	Splitter raw_vals = query.tokenize("&");
	while (try String rv = raw_vals.next())
	{
		@pool(allocator)
		{
			String[] parts = rv.tsplit("=", 2);
			String key = temp_decode(parts[0], QUERY) ?? parts[0];
			vals.add(key, parts.len == 1 ? key : (temp_decode(parts[1], QUERY) ?? parts[1]));
		};
	}
	return vals;
}

<*
 Add copies of the key and value strings to the UrlQueryValues map. These
 copies are freed when the UrlQueryValues map is freed.

 @param [in] self
 @param key
 @param value
 @return "a UrlQueryValues map"
*>
fn UrlQueryValues* UrlQueryValues.add(&self, String key, String value)
{
	String value_copy = value.copy(self.allocator);
	if (try existing = self.get_ref(key))
	{
		existing.push(value_copy);
	}
	else
	{
		UrlQueryValueList new_list;
		new_list.new_init_with_array({ value_copy }, self.allocator);
		(*self)[key] = new_list;
		self.key_order.push(key.copy(self.allocator));
	}
	return self;
}


<*
 Stringify UrlQueryValues into an encoded query string.

 @param [in] self
 @param [inout] allocator
 @return "a percent-encoded query string"
*>
fn String UrlQueryValues.to_string(&self, Allocator allocator = allocator::heap()) @dynamic
{
	@pool(allocator)
	{
		DString builder = dstring::temp_new();

		usz i;
		foreach (key: self.key_order)
		{
			String encoded_key = temp_encode(key, QUERY);

			UrlQueryValueList! values = self.map.get(key);
			if (catch values) continue;

			foreach (value: values)
			{
				if (i > 0) builder.append_char('&');

				builder.append_chars(encoded_key);
				builder.append_char('=');

				String encoded_value = temp_encode(value, QUERY);
				builder.append_chars(encoded_value);
				i++;
			}
		};

		return builder.copy_str(allocator);
	};
}

fn void UrlQueryValues.free(&self)
{
	self.map.@each(;String key, UrlQueryValueList values)
	{
		foreach (value: values) value.free(self.allocator);
		values.free();
	};
	self.map.free();

	foreach (&key: self.key_order) key.free(self.allocator);
	self.key_order.free();
}

<*
 Free an Url struct.

 @param [in] self
*>
fn void Url.free(&self)
{
	if (!self.allocator) return;
	self.scheme.free(self.allocator);
	self.host.free(self.allocator);
	self.username.free(self.allocator);
	self.password.free(self.allocator);
	self.path.free(self.allocator);
	self.query.free(self.allocator);
	self.fragment.free(self.allocator);
}
