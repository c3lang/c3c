module std::net::url;

import std::io, std::collections::map, std::collections::list;

def StringList = List(<String>);
def Values = HashMap(<String, StringList>);

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
        usz! authorityEnd = url_string.index_of_chars("/?#");
        if (catch authorityEnd)
        {
            authorityEnd = url_string.len;
        }

        String authority = url_string[:authorityEnd]!;

        if (try usz userInfoEnd = url_string.index_of_char('@'))
        {
            String userinfo = authority[:userInfoEnd];
            String[] userpass = userinfo.split(":");
            defer free(userpass);
            url.username = userpass[0];
            if (userpass.len > 1)
            {
                url.password = userpass[1];
            }
            authority = authority[userInfoEnd + 1 ..];
        }

        // Check for IPv6 address in square brackets
        if (authority.starts_with("[") && authority.contains("]"))
        {
            usz ipv6End = authority.index_of("]")!;
            url.host = authority[0 .. ipv6End];  // Includes closing bracket
            if ((ipv6End + 1) < authority.len && authority[.. ipv6End] == ":")
            {
                url.port = authority[.. ipv6End + 1].to_uint()!;
            }
        }
        else
        {
            String[] hostPort = authority.split(":");
            defer mem::free(hostPort);
            if (hostPort.len > 1)
            {
                url.host = hostPort[0];
                url.port = hostPort[1].to_uint()!;
            }
            else
            {
                url.host = authority;
            }
        }
        url_string = url_string[authorityEnd ..]!;
    }

    // Parse path
    long queryIndex = (long)url_string.index_of_char('?') ?? -1;
    long fragmentIndex = (long)url_string.index_of_char('#') ?? -1;

    if (queryIndex != -1 || fragmentIndex != -1)
    {
        long pathEnd = min(queryIndex == -1 ? url_string.len : queryIndex,
                           fragmentIndex == -1 ? url_string.len : fragmentIndex,
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
        fragmentIndex = (long)url_string.index_of_char('#') ?? -1;
        if (fragmentIndex == -1)
        {
            fragmentIndex = url_string.len;
        }
        url.query = url_string[1 .. fragmentIndex - 1];
        url_string = url_string[fragmentIndex ..];
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
fn String Url.to_string(Url* self, Allocator allocator = allocator::temp()) @dynamic
{
    @pool(allocator)
	{
        DString builder = dstring::new("", allocator);
        defer builder.free();

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

        return builder.str_view();
    };
}

<*
 Parse the query parameters of the Url into a Values HashMap.

 @param [in] self
 @param [inout] allocator
 @return "a Values HashMap"
*>
fn Values Url.query_values(Url* self, Allocator allocator = allocator::heap())
{
    Values vals;
    String[] raw_vals = self.query.split("&");
    defer free(raw_vals);

    foreach (rv : raw_vals)
    {
        String[] parts = rv.split("=", 2);
        defer free(parts);

        if (try existing = vals[parts[0]])
        {
            existing.push(parts[1]);
            vals[parts[0]] = existing;
        }
        else
        {
            StringList new_list;
            new_list.new_init_with_array({parts[1]}, allocator);
            vals[parts[0]] = new_list;
        }
    }

    return vals;
}