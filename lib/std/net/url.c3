module std::net::url;

import std::io, std::collections::map, std::collections::list;

def UrlQueryValueList = List(<String>);

struct UrlQueryValues
{
	inline HashMap(<String, UrlQueryValueList>) map;
	UrlQueryValueList key_order;
}

<*
 Represents the actual (decoded) Url.

 An Url can be parsed from a String with `new_parse()` or `temp_parse()`. The
 parsed fields are decoded. The only field that is not decoded is `raw_query`.
 To access the decoded query values, use `Url.query_values()`.

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
	String raw_query;
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
	Url url = { .allocator = allocator };
	url_string = url_string.trim();
	if (!url_string.len)
	{
		return url;
	}

	// Parse scheme
	if (try pos = url_string.index_of("://"))
	{
		url.scheme = url_string[:pos];
		url_string = url_string[url.scheme.len + 3 ..];
	}
	else if (url_string.contains(":"))
	{
		// Handle schemes without authority like 'mailto:'
		url.scheme = url_string[:url_string.index_of(":")!];
		url_string = url_string[url.scheme.len + 1 ..];
		url.path = url_string;

		return url.decode_copy(allocator);
	}

	// Parse host, port
	if (url.scheme != "urn")
	{
		usz! authority_end = url_string.index_of_chars("/?#");
		if (catch authority_end)
		{
			authority_end = url_string.len;
		}

		String authority = url_string[:authority_end]!;

		if (try usz userInfo_end = url_string.index_of_char('@'))
		{
			String userinfo = authority[:userInfo_end];
			String[] userpass = userinfo.split(":");
			defer free(userpass);
			url.username = userpass[0];
			if (userpass.len > 1)
			{
				url.password = userpass[1];
			}
			authority = authority[userInfo_end + 1 ..];
		}

		// Check for IPv6 address in square brackets
		if (authority.starts_with("[") && authority.contains("]"))
		{
			usz ipv6_end = authority.index_of("]")!;
			url.host = authority[0 .. ipv6_end];  // Includes closing bracket
			if ((ipv6_end + 1) < authority.len && authority[.. ipv6_end] == ":")
			{
				url.port = authority[.. ipv6_end + 1].to_uint()!;
			}
		}
		else
		{
			String[] host_port = authority.split(":");
			defer mem::free(host_port);
			if (host_port.len > 1)
			{
				url.host = host_port[0];
				url.port = host_port[1].to_uint()!;
			}
			else
			{
				url.host = authority;
			}
		}
		url_string = url_string[authority_end ..]!;
	}

	// Parse path
	long query_index = (long)url_string.index_of_char('?') ?? -1;
	long fragment_index = (long)url_string.index_of_char('#') ?? -1;

	if (query_index != -1 || fragment_index != -1)
	{
		long pathEnd = min(query_index == -1 ? url_string.len : query_index,
						   fragment_index == -1 ? url_string.len : fragment_index,
						   url_string.len);
		url.path = url_string[:pathEnd];
	}
	else
	{
		url.path = url_string;
	}

	// Remove the path part from url for further parsing
	url_string = url_string[url.path.len ..];

	// Parse query
	if (url_string.starts_with("?"))
	{
		fragment_index = (long)url_string.index_of_char('#') ?? -1;
		if (fragment_index == -1)
		{
			fragment_index = url_string.len;
		}
		url.raw_query = url_string[1 .. fragment_index - 1];
		url_string = url_string[fragment_index ..];
	}

	// Parse fragment
	if (url_string.starts_with("#"))
	{
		url.fragment = url_string[1 ..];
	}

	return url.decode_copy(allocator);
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
			String username = encode_temp(self.username, USERPASS);
			builder.append_chars(username);

			if (self.password != "")
			{
				builder.append_char(':');

				String password = encode_temp(self.password, USERPASS);
				builder.append_chars(password);
			}
			builder.append_char('@');
		}

		// Add host
		String host = encode_temp(self.host, HOST);
		builder.append_chars(host);

		// Add port
		if (self.port != 0)
		{
			builder.append_char(':');
			builder.appendf("%d", self.port);
		}

		// Add path
		String path = encode_temp(self.path, PATH);
		builder.append_chars(path);

		// Add query if it exists (note that `raw_query` is expected to
		// be already properly encoded).
		if (self.raw_query != "")
		{
			builder.append_char('?');
			builder.append_chars(self.raw_query);
		}

		// Add fragment if it exists
		if (self.fragment != "")
		{
			builder.append_char('#');

			String fragment = encode_temp(self.fragment, FRAGMENT);
			builder.append_chars(fragment);
		}

		return builder.copy_str(allocator);
	};
}

<*
 Parse the query parameters of the Url into a UrlQueryValues map.

 @param [in] self
 @param [inout] allocator
 @return "a UrlQueryValues HashMap"
*>
fn UrlQueryValues Url.query_values(&self, Allocator allocator)
{
	UrlQueryValues vals;
	vals.init(allocator);

	Splitter raw_vals = self.raw_query.tokenize("&");

	while (try String rv = raw_vals.next())
	{
		@pool(allocator)
		{
			String[] parts = rv.tsplit("=", 2);
			String key = decode_temp(parts[0], QUERY) ?? parts[0];
			String value = decode_temp(parts[1], QUERY) ?? parts[1];
			vals.add(key, value);
		};
	}
	return vals;
}

<*
 Parse the query parameters of the Url into a UrlQueryValues map,
 to be freed using values.free()

 @param [in] self
 @return "a UrlQueryValues map"
*>
fn UrlQueryValues Url.new_query_values(&self)
{
	return self.query_values(allocator::heap()) @inline;
}

<*
 Parse the query parameters of the Url into a UrlQueryValues map.
 stored on the temp allocator.

 @param [in] self
 @return "a UrlQueryValues map"
*>
fn UrlQueryValues Url.temp_query_values(&self)
{
	return self.query_values(allocator::temp()) @inline;
}

fn UrlQueryValues* UrlQueryValues.new_init(&self) => self.init(allocator::heap());
fn UrlQueryValues* UrlQueryValues.temp_init(&self) => self.init(allocator::temp());
<*
 @param [&inout] allocator "The allocator to use"
*>
fn UrlQueryValues* UrlQueryValues.init(&self, Allocator allocator)
{
	self.map.init(allocator);
	self.key_order.new_init(allocator: allocator);
	return self;
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
			String encoded_key = encode_temp(key, QUERY);

			UrlQueryValueList! values = self.map.get(key);
			if (catch values) continue;

			foreach (value: values)
			{
				if (i > 0) builder.append_char('&');

				builder.append_chars(encoded_key);
				builder.append_char('=');

				String encoded_value = encode_temp(value, QUERY);
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
		foreach (value: values) value.free();
		values.free();
	};
	self.map.free();

	foreach (&key: self.key_order) key.free();
	self.key_order.free();
}

<*
 Returns a decoded Url copy on the allocator. `decode_copy` should only be
 called from `new_parse()` or `temp_parse()`. The parse function does not
 allocate any memory, but simply stores string slices referencing the input
 string.

 @param [in] self
 @param [&inout] allocator
 @return "a decoded Url struct"
*>
fn Url! Url.decode_copy(&self, Allocator allocator) @private
{
	return {
		.scheme    = self.scheme.copy(allocator),
		.host 	   = decode_new(self.host, HOST, allocator) ?? self.host.copy(allocator),
		.port 	   = self.port,
		.username  = decode_new(self.username, HOST, allocator) ?? self.username.copy(allocator),
		.password  = decode_new(self.password, USERPASS, allocator) ?? self.password.copy(allocator),
		.path 	   = decode_new(self.path, PATH, allocator) ?? self.path.copy(allocator),
		.raw_query = self.raw_query.copy(allocator), // decoded query fields should be accessed vie `Url.query_values()`
		.fragment  = decode_new(self.fragment, FRAGMENT, allocator) ?? self.fragment.copy(allocator),
		.allocator = allocator,
	};
}

<*
 Free an Url struct. Required when `new_parse()` is used.

 @param [in] self
*>
fn void Url.free(&self)
{
	self.scheme.free();
	self.host.free();
	self.username.free();
	self.password.free();
	self.path.free();
	self.raw_query.free();
	self.fragment.free();
}
