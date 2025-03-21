module std::net::url;

import std::io, std::collections::map, std::collections::list;

faultdef
	EMPTY,
	INVALID_SCHEME,
	INVALID_USER,
	INVALID_PASSWORD,
	INVALID_HOST,
	INVALID_PATH,
	INVALID_FRAGMENT;

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
 @require url_string.len > 0 : "the url_string must be len 1 or more"
 @return "the parsed Url"
*>
fn Url? tparse(String url_string) => parse(tmem, url_string);

<*
 Parse a URL string into a Url struct.

 @param [in] url_string
 @require url_string.len > 0 : "the url_string must be len 1 or more"
 @return "the parsed Url"
*>
fn Url? parse(Allocator allocator, String url_string)
{
	url_string = url_string.trim();
	if (!url_string) return EMPTY?;
	Url url = { .allocator = allocator };

	// Parse scheme
	if (try pos = url_string.index_of("://"))
	{
		if (!pos) return INVALID_SCHEME?;
		url.scheme = url_string[:pos].copy(allocator);
		url_string = url_string[url.scheme.len + 3 ..];
	}
	else if (try pos = url_string.index_of(":"))
	{
		// Handle schemes without authority like 'mailto:'
		if (!pos) return INVALID_SCHEME?;
		url.scheme = url_string[:pos].copy(allocator);
		url.path = decode(allocator, url_string[pos + 1 ..], PATH) ?? INVALID_PATH?!;
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
			@pool()
			{
				String[] userpass = userinfo.tsplit(":", 2);
				username = userpass[0];
				if (!username.len) return INVALID_USER?;
							url.host =

				url.username = decode(allocator, username, HOST) ?? INVALID_USER?!;
				if (userpass.len) url.password = decode(allocator, userpass[1], USERPASS) ?? INVALID_PASSWORD?!;
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
			@pool()
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
		url.host = decode(allocator, host, HOST) ?? INVALID_HOST?!;
		url_string = url_string[authority_end ..];
	}

	// Parse path
	usz? query_index = url_string.index_of_char('?');
	usz? fragment_index = url_string.index_of_char('#');

	if (@ok(query_index) || @ok(fragment_index))
	{
		usz path_end = min(query_index ?? url_string.len, fragment_index ?? url_string.len);
		url.path = decode(allocator, url_string[:path_end], PATH) ?? INVALID_PATH?!;
		url_string = url_string[path_end ..];
	}
	else
	{
		url.path = decode(allocator, url_string, PATH) ?? INVALID_PATH?!;
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
		url.fragment = decode(allocator, url_string[1..], FRAGMENT) ?? INVALID_FRAGMENT?!;
	}
	return url;
}

fn usz? Url.to_format(&self, Formatter* f) @dynamic
{
	usz len;
	// Add scheme if it exists
	if (self.scheme != "")
	{
		len += f.print(self.scheme)!;
		len += f.print(":")!;
		if (self.host.len > 0) len += f.print("//")!;
	}

	// Add username and password if they exist
	if (self.username)
	{
		@stack_mem(64; Allocator smem)
		{
			len += f.print(encode(smem, self.username, USERPASS))!;
		};
		if (self.password)
		{
			len += f.print(":")!;
			@stack_mem(64; Allocator smem)
			{
				len += f.print(encode(smem, self.password, USERPASS))!;
			};
		}
		len += f.print("@")!;
	}

	// Add host
	@stack_mem(128; Allocator smem)
	{
		len += f.print(encode(smem, self.host, HOST))!;
	};

	// Add port
	if (self.port) len += f.printf(":%d", self.port)!;

	// Add path
	@stack_mem(256; Allocator smem)
	{
		len += f.print(encode(smem, self.path, PATH))!;
	};

	// Add query if it exists (note that `query` is expected to
	// be already properly encoded).
	if (self.query)
	{
		len += f.print("?")!;
		len += f.print(self.query)!;
	}

	// Add fragment if it exists
	if (self.fragment)
	{
		@stack_mem(256; Allocator smem)
		{
			len += f.print("#")!;
			len += f.print(encode(smem, self.fragment, FRAGMENT))!;
		};
	}
	return len;
}

fn String Url.to_string(&self, Allocator allocator)
{
	return string::format(allocator, "%s", *self);
}


alias UrlQueryValueList = List{String};

struct UrlQueryValues
{
	inline HashMap{String, UrlQueryValueList} map;
	UrlQueryValueList key_order;
}

<*
 Parse the query parameters of the Url into a UrlQueryValues map.

 @param [in] query
 @return "a UrlQueryValues HashMap"
*>
fn UrlQueryValues parse_query_to_temp(String query) => parse_query(tmem, query);

<*
 Parse the query parameters of the Url into a UrlQueryValues map.

 @param [in] query
 @param [inout] allocator
 @return "a UrlQueryValues HashMap"
*>
fn UrlQueryValues parse_query(Allocator allocator, String query)
{
	UrlQueryValues vals;
	vals.map.init(allocator);
	vals.key_order.init(allocator);

	Splitter raw_vals = query.tokenize("&");
	while (try String rv = raw_vals.next())
	{
		@pool()
		{
			String[] parts = rv.tsplit("=", 2);
			String key = tdecode(parts[0], QUERY) ?? parts[0];
			vals.add(key, parts.len == 1 ? key : (tdecode(parts[1], QUERY) ?? parts[1]));
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
		new_list.init_with_array(self.allocator, { value_copy });
		(*self)[key] = new_list;
		self.key_order.push(key.copy(self.allocator));
	}
	return self;
}



fn usz? UrlQueryValues.to_format(&self, Formatter* f) @dynamic
{
	usz len;
	usz i;
	foreach (key: self.key_order)
	{
		@stack_mem(128; Allocator mem)
		{
			String encoded_key = encode(mem, key, QUERY);
			UrlQueryValueList? values = self.map.get(key);
			if (catch values) continue;
			foreach (value : values)
			{
				if (i > 0) len += f.print("&")!;
				len += f.print(encoded_key)!;
				len += f.print("=")!;
				@stack_mem(256; Allocator smem)
				{
					len += f.print(encode(smem, value, QUERY))!;
				};
				i++;
			}
		};
	}
	return len;
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
