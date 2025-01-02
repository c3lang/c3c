module std::net::url;

import std::io, std::collections::map, std::collections::list;

def UrlQueryValueList = List(<String>);

struct UrlQueryValues
{
	inline HashMap(<String, UrlQueryValueList>) map;
}

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
}

<*
 Parse a URL string into a Url struct.

 @param [in] url_string
 @require url_string.len > 0 "the url_string must be len 1 or more"
 @return "the parsed Url"
*>
fn Url! parse(String url_string)
{
	Url url;
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

		return url;
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
		url.query = url_string[1 .. fragment_index - 1];
		url_string = url_string[fragment_index ..];
	}

	// Parse fragment
	if (url_string.starts_with("#"))
	{
		url.fragment = url_string[1 ..];
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
			builder.append_chars(self.username);
			if (self.password != "")
			{
				builder.append_char(':');
				builder.append_chars(self.password);
			}
			builder.append_char('@');
		}

		// Add host
		builder.append_chars(self.host);

		// Add port
		if (self.port != 0)
		{
			builder.append_char(':');
			builder.appendf("%d", self.port);
		}

		// Add path
		builder.append_chars(self.path);

		// Add query if it exists
		if (self.query != "")
		{
			builder.append_char('?');
			builder.append_chars(self.query);
		}

		// Add fragment if it exists
		if (self.fragment != "")
		{
			builder.append_char('#');
			builder.append_chars(self.fragment);
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

	Splitter raw_vals = self.query.tokenize("&");

	while (try String rv = raw_vals.next())
	{
		@pool(allocator)
		{
			String[] parts = rv.tsplit("=", 2);
			if (try existing = vals.get_ref(parts[0]))
			{
				existing.push(parts[1]);
			}
			else
			{
				UrlQueryValueList new_list;
				new_list.new_init_with_array({ parts[1] }, allocator);
				vals[parts[0]] = new_list;
			}
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

fn void UrlQueryValues.free(&self)
{
	self.map.@each(;String key, UrlQueryValueList value)
	{
		value.free();
	};
	self.map.free();
}

