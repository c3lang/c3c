<*
 This module section provides encoding and decoding functions for URL
 components according to RFC 3986.
*>
module std::net::url;
import std::encoding::hex;

enum UrlEncodingMode : char (String allowed)
{
	UNRESERVED = "-_.~",            // section 2.3
	PATH 	   = "$&+,/:;=@",       // section 3.3
	HOST 	   = "!$&'()*+,;=:[]",  // section 3.2.2 (also include ':', '[', ']' for ipv6 hosts)
	USERPASS   = ";:&=+$,",         // section 3.2.1
	QUERY 	   = "",                // section 3.4
	FRAGMENT   = "$&+,/:;=?@!()*",	// section 4.1
}

faultdef INVALID_HEX;

<*
 Returns true if char c should be encoded according to RFC 3986.

 @param c : "Character to check if it should be encoded."
 @param mode : "Url encoding mode."
*>
fn bool should_encode(char c, UrlEncodingMode mode) @private
{
	// alphanumeric characters are allowed
	if (c.is_alnum()) return false;

	// unreserved characters are allowed
	if (try UrlEncodingMode.UNRESERVED.allowed.index_of_char(c)) return false;

	// some mode-specific characters are allowed
	if (try mode.allowed.index_of_char(c)) return false;

	// everything else must be encoded
	return true;
}

<*
 Calculate the length of the percent-encoded string.
*>
fn usz encode_len(String s, UrlEncodingMode mode) @inline
{
	usz n;
	foreach (c: s)
	{
		if (!should_encode(c, mode)) continue;
		if (c != ' ' || mode != QUERY)
		{
			n++;
		}
	}
	return s.len + 2 * n;
}

<*
 Encode the string s for a given encoding mode.
 Returned string must be freed.

 @param s : "String to encode"
 @param mode : "Url encoding mode"
 @param [inout] allocator
 @return "Percent-encoded String"
*>
fn String encode(Allocator allocator, String s, UrlEncodingMode mode) => @pool()
{
	usz n = encode_len(s, mode);
	DString builder = dstring::temp_with_capacity(n);

	foreach(i, c: s)
	{
		switch
		{
			// encode spaces in queries
			case c == ' ' && mode == QUERY:
				builder.append_char('+');

			// add encoded char
			case should_encode(c, mode):
				builder.append_char('%');
				String hex = hex::tencode(s[i:1]);
				builder.append(hex.to_upper_tcopy());

			// use char, no encoding needed
			default:
				builder.append_char(c);
		}
	}

	return builder.copy_str(allocator);
}


<*
 Encode string s for a given encoding mode, stored on the temp allocator.

 @param s : "String to encode"
 @param mode : "Url encoding mode"
 @return "Percent-encoded String"
*>
fn String tencode(String s, UrlEncodingMode mode) => encode(tmem, s, mode);

<*
 Calculate the length of the percent-decoded string.

 @return? INVALID_HEX
*>
fn usz? decode_len(String s, UrlEncodingMode mode) @inline
{
	usz n;
	foreach (i, c: s)
	{
		if (c != '%') continue;
		if (i + 2 >= s.len || !s[i+1].is_xdigit() || !s[i+2].is_xdigit())
		{
			return INVALID_HEX?;
		}
		n++;
	}
	return s.len - 2 * n;
}

<*
 Decode string s for a given encoding mode.
 Returned string must be freed.

 @param s : "String to decode"
 @param mode : "Url encoding mode"
 @param [inout] allocator
 @return "Percent-decoded String"
*>
fn String? decode(Allocator allocator, String s, UrlEncodingMode  mode) => @pool()
{
	usz n = decode_len(s, mode)!;
	DString builder = dstring::temp_with_capacity(n);

	for (usz i = 0; i < s.len; i++)
	{
		switch (s[i])
		{
			// decode encoded char
			case '%':
				char[] hex = hex::tdecode(s[i+1:2])!;
				builder.append(hex);
				i += 2;

			// decode space when in queries
			case '+':
				builder.append_char((mode == QUERY) ? ' ' : '+');

			// use char, no decoding needed
			default:
				builder.append_char(s[i]);
		}
	}

	return builder.copy_str(allocator);
}


<*
 Decode string s for a given encoding mode, stored on the temp allocator.

 @param s : "String to decode"
 @param mode : "Url encoding mode"
 @return "Percent-decoded String"
*>
fn String? tdecode(String s, UrlEncodingMode  mode) => decode(tmem, s, mode);
