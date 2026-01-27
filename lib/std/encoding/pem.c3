// Copyright (c) 2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// A module for encoding or decoding PEM blobs [mostly] in accordance with RFCs 1421-1424.
//   This implementation retains a lot of flexibility in parsing input PEM blobs.
//
module std::encoding::pem;

import std::collections, std::encoding::base64;

<* A safe, default tag to use per RFC 1421's rules. *>
const String DEFAULT_TAG = "PRIVACY-ENHANCED MESSAGE";

<* The set of characters which are considered valid for PEM tags (which appear inside of Encapsulation Boundaries). *>
const AsciiCharset TAG_SET @local = ascii::@combine_sets(ascii::ALPHA_UPPER_SET, ascii::NUMBER_SET, ascii::@create_set(" _-/+()"));
<* The set of characters which are considered valid for optional PEM headers used. *>
const AsciiCharset HEADER_KEY_SET @local = ascii::@combine_sets(ascii::ALPHANUMERIC_SET, ascii::@create_set("!#$%&'*+-.^_`|~"));

<* All PEM Encapsulation Boundaries must use this delimiter to demarcate the PEM from its surrounding content, if any. *>
const String EB_DELIMITER @local = "-----";
<* All PEM blobs will start with this Encapsulation Boundary prefix. *>
const String PRE_EB_PREFIX @local = EB_DELIMITER +++ "BEGIN ";
<* All PEM blobs will terminate with this Encapsulation Boundary prefix. *>
const String POST_EB_PREFIX @local = EB_DELIMITER +++ "END ";

<* When encoding new PEM blobs to string values, specifies which line-ending sequence to use. This is set at compile-time. *>
const String ENDL = $feature(PEM_CRLF) ??? "\r\n" : "\n";

<* Specify a set of possible PEM en/decoding faults. *>
faultdef
	BODY_REQUIRED,   // encoding: no body given (or too few of them)
	HEADERS_REQUIRED,   // encoding: no headers given (or too few of them)
	HEADER_KEY_REQUIRED,   // encoding: blank header keys are not allowed
	HEADER_VALUE_REQUIRED,   // encoding: blank header values are not allowed
	INVALID_BODY,   // decoding: invalid body, likely bad base64
	INVALID_FORMAT,   // decoding: invalid input formatting - no pre-EB or just plain wrong
	INVALID_HEADER,   // decoding: invalid headers
	INVALID_HEADER_KEY,   // decoding: invalid or empty header key
	INVALID_PRE_EB,   // decoding: invalid pre-EncapsBoundary BEFORE the PEM body
	INVALID_POST_EB,   // decoding: invalid post-EncapsBoundary AFTER the PEM body
	INVALID_TAG,   // decoding: invalid tag within an EB
	MISMATCHED_TAG,   // decoding: the tag from the pre-EB doesn't match that of the post-EB
	MISSING_BODY,   // decoding: missing PEM body base64
	MISSING_HEADER_KEY,   // decoding: the header is missing its key
	MISSING_HEADER_VALUE,   // decoding: the header is missing its value
	MISSING_POST_EB,   // decoding: no post-EB was found to close off the PEM
	MISSING_TAG,   // decoding: no tag was defined or parsed from the EB
	TAG_REQUIRED,   // encoding: no/empty tag given (or too few of them)
;


<* Represents a PEM object in memory, with a reference to the body data, tag value, and optional headers. *>
struct Pem
{
	<* The allocator associated with the PEM's creation and destruction. *>
	Allocator allocator;
	<* A flexible 'tag' value used within the Encapsulation Boundary to denote the type of the PEM. *>
	String tag;
	<* A set of optional headers used to provide more context or information about the body of the PEM object. *>
	LinkedHashMap{String, String} headers;
	<* The core boy data of the PEM itself - the main values to be transmitted in this format. *>
	char[] data;
}


<*
 Create a new PEM object from a few inputs. Each input (i.e., tag, data, and headers) is copied to a new memory location.
 The PEM object itself is not allocated in-memory, but is a simple container that points to each value that _is_.

 Key-Value pairs for headers are provided in sequence as variadic arguments: `"key", "value", "key2", "value2", ...`

 Created PEMs that are not temporary should be destroyed with `Pem.free`.

 @param [&inout] allocator : "The allocator to use when copying the provided PEM object's fields."
 @param [in] data : "The body data of the PEM."
 @param [in] tag : "The tag value to use within the PEM's Encapsulation Boundary."

 @return "A new PEM object."
*>
macro Pem create(Allocator allocator, char[] data, String tag, ...)
{
	$assert $vacount % 2 == 0 : "You must use an even amount of headers (key-value pairings) to initialize a PEM object.";
	Pem result = {
		.allocator = allocator,
		.tag = tag.copy(allocator),
		.data = allocator::clone_slice(allocator, data),
	};
	$if $vacount > 0: result.headers.init_with_key_values(allocator, $vasplat); $endif
	return result;
}

<*
 A temporary-allocator alias for `create`, used to create a new PEM container object.

 @param [in] data : "The body data of the PEM."
 @param [in] tag : "The tag value to use within the PEM's Encapsulation Boundary."

 @return "A new PEM object."
*>
macro Pem tcreate(char[] data, String tag, ...) => create(tmem, data, tag, $vasplat);


<*
 Duplicate a `Pem` container and allocate copies of its members using the given allocator.

 @param [&inout] allocator : "The allocator to use when copying the `Pem` members."
*>
fn Pem Pem.copy(&self, Allocator allocator)
{
	Pem result = create(allocator, self.data, self.tag);
	if (self.headers.len() > 0) result.headers.init_from_map(allocator, &self.headers);
	return result;
}

<*
 A temporary-allocator alias for `Pem.copy`, which duplicates a `Pem` container and allocates copies of its members.
*>
macro Pem Pem.tcopy(&self) => self.copy(tmem);

<*
 Safely destroys a `Pem` and deallocate all of its members. This should always be explicitly called when not using `tmem`.
*>
fn void Pem.free(&self)
{
	mem::zero_volatile(self.data);
	if (self.allocator != tmem)
	{
		self.headers.free();
		self.tag.free(self.allocator);
		allocator::free(self.allocator, self.data);
	}
	mem::zero_volatile(@as_char_view(*self));
}

<*
 Attempt to decode an input string into one or more `Pem` objects. If the input contains any non-PEM or otherwise
 invalid data, then this will throw an error. Ideally, this function is used to decode PEM files explicitly, lest
 the caller need to be sure they're only providing PEM data +/- some intermediate whitespace.

 @param [&inout] allocator : "The allocator to use when creating the `Pem` outputs and members."
 @param [in] input : "The string to parse one or more PEM blobs from."

 @return "An array of decoded `Pem` objects, depending on how many were present in the input (separated optionally by whitespace)."
*>
macro Pem[]? decode(Allocator allocator, String input)
{
	// If using the temp allocator, we don't want to lose any allocated HashMap structures on scope-exit
	//   by wrapping w/ @pool. We'd rather just keep the mess around than try to lift it out.
	if (allocator == tmem) return _decode_all(allocator, input);
	@pool()
	{
		return _decode_all(allocator, input);
	};
}

<*
 A temporary-allocator alias for the `decode` function, which reads an input string and returns one or more decoded PEMs.

 @param [in] input : "The string to parse one or more PEM blobs from."

 @return "An array of decoded `Pem` objects, depending on how many were present in the input (separated optionally by whitespace)."
*>
macro Pem[]? tdecode(String input) => decode(tmem, input);

<*
 INTERNAL ONLY: Decodes one or more PEM objects from the input string.

 @param [&inout] allocator : "The allocator to use during decoding to return the result."
 @param [in] input : "The string to decode."
*>
fn Pem[]? _decode_all(Allocator allocator, String input) @local
{
	List{Pem} pem_list;
	pem_list.tinit();
	defer pem_list.free();

	String[] lines = input.treplace("\r\n", "\n").tsplit("\n");
	foreach (&line : lines) *line = (*line).trim_right();   // remove any trailing whitespace as this can disrupt parsing (but shouldn't)
	while (lines.len > 0)
	{
		pem_list.push(_decode_single(allocator, &lines)!);
		while (lines.len > 0 && lines[0].trim().len == 0) lines = lines[1..];   // skip all empty lines in between or after PEM boundaries
	}

	return pem_list.to_array(allocator);
}

<*
 INTERNAL ONLY: Decode one PEM at a time, from pre-EB to its discovered post-EB.

 @param [&inout] allocator : "The allocator to use during decoding to return the result."
 @param [&inout] lines_io : "A pointer to an input slice to modify as the single PEM is parsed from it."

*>
fn Pem? _decode_single(Allocator allocator, String[]* lines_io) @local
{
	String[] lines = *lines_io;   // copy to local var
	Pem result = { .allocator = allocator };
	result.headers.init(allocator);
	defer catch result.free();

	// Remove any preceding whitespace-only lines.
	while (lines[0].trim().len == 0) lines = lines[1..];

	if (lines.len < 3) return INVALID_FORMAT~;   // at least 3 lines (pre-EB, body, post-EB) are always required

	// The Pre-Encapsulation-Boundary must be of the format: -----BEGIN TAG-----, where "TAG" can be any upper-case identifier [A-Z_ -/]
	String pre_eb = lines[0];
	if (pre_eb[0:11] != PRE_EB_PREFIX || pre_eb[^5..] != EB_DELIMITER) return INVALID_PRE_EB~;
	String tag = pre_eb[PRE_EB_PREFIX.len..^6];
	if (!tag.len || !tag.trim().len) return MISSING_TAG~;
	foreach (c : tag) if (!TAG_SET.contains(c)) return INVALID_TAG~;
	result.tag = tag.copy(allocator);

	// The Post-Encapsulation-Boundary is the same, but uses "END", and the extracted tag must match.
	//   Since the input might contain more than one PEM unit, we need to search for the ending encapsulation boundary dynamically.
	String post_eb;
	usz endl;
	for SEARCH_EB: (endl = 1; endl < lines.len; endl++)
	{
		if (lines[endl].len > POST_EB_PREFIX.len && lines[endl][0:EB_DELIMITER.len] == EB_DELIMITER)
		{
			post_eb = lines[endl];
			break SEARCH_EB;
		}
	}
	if (!post_eb.len) return MISSING_POST_EB~;
	if (post_eb[0:9] != POST_EB_PREFIX || post_eb[^5..] != EB_DELIMITER) return INVALID_POST_EB~;
	String post_tag = post_eb[POST_EB_PREFIX.len..^6];
	if (post_tag.len != tag.len || post_tag != tag) return MISMATCHED_TAG~;

	// Now that the inner portion is decapsulated, tag is, strip off the boundaries.
	*lines_io = lines[endl+1..];   // update the iterated slice of lines from the calling context - see: `decode`
	lines = lines[1:endl-1];

	// while there's a colon+space in the current line, we should assume that this is a key-value header pair
	while (lines[0].contains(": "))
	{
		if (!HEADER_KEY_SET.contains(lines[0][0])) return INVALID_HEADER~;   // not a multiline header? error out if the first char is not appropriate
		String[] marker = lines;   // temporary marker
		usz span = 1;   // how many lines this header spans

		// Search for multi-line key-value pairs, indicated by a whitespace character beginning the current line.
		for (lines = lines[1..]; lines[0].len > 0 && ascii::WHITESPACE_SET.contains(lines[0][0]); lines = lines[1..], span++);
		foreach (&line : marker[:span]) *line = (*line).trim();   // always trim on both sides

		String full_header = string::tjoin(marker[:span], " ");   // join the lines with a single space
		if (!full_header.contains(": ")) return INVALID_HEADER~;   // reassert the presence of this

		// Extract the key and value from the message, then validate.
		//   The header name should match a valid set of characters, but the value doesn't need to conform to anything other than existing
		String[] kv = full_header.tsplit(": ", max: 2);
		if (!kv[0].len) return MISSING_HEADER_KEY~;
		if (!kv[1].len) return MISSING_HEADER_VALUE~;
		foreach (c : kv[0]) if (!HEADER_KEY_SET.contains(c)) return INVALID_HEADER_KEY~;

		result.headers.set(kv[0], kv[1]);   // finally, push the values
	}

	// if any headers were present, the line after the headers MUST BE EMPTY
	if (result.headers.len() > 0)
	{
		if (lines[0].trim().len > 0) return INVALID_FORMAT~;   // but we are forgiving about whitespace here
		lines = lines[1..];
	}

	// Here, we assume lines[0] is the start of base64 data. This means there must be at least 1 line, of course.
	if (lines.len < 1) return MISSING_BODY~;

	// ... While the PEM format specifies a 64-character width on all but the last line of the base64 body,
	//   this parser doesn't need to be particular about that as long as the base64 is ok
	// In this case, the rest of the lines in the set should be base64 and should decode accordingly
	String to_decode = string::tjoin(lines, "");
	if (!to_decode.len) return MISSING_BODY~;   // paranoia
	result.data = (base64::decode(allocator, to_decode) ?? INVALID_BODY~)!;

	return result;
}


<*
 Encodes a single `Pem` object into a new PEM-formatted string.

 @param [&inout] allocator : "The allocator to use for allocating the final encoded string."
*>
fn String? Pem.encode(&self, Allocator allocator)
{
	if (!self.data.len) return BODY_REQUIRED~;
	if (!self.tag.len) return TAG_REQUIRED~;

	DString out;
	out.tinit();

	@pool()
	{
		out.appendf(PRE_EB_PREFIX +++ "%s" +++ EB_DELIMITER +++ ENDL, self.tag);
		foreach KEY_ITER: (key : self.headers.tkeys())
		{
			if (!key.len) return HEADER_KEY_REQUIRED~;
			String value = self.headers[key]!!;
			if (!value.len) return HEADER_VALUE_REQUIRED~;
			usz first_line_length = 64 - 2 - key.len;
			if (value.len <= first_line_length)
			{
				out.appendf("%s: %s" +++ ENDL, key, value);
				continue KEY_ITER;
			}
			out.appendf("%s: %s" +++ ENDL, key, value[:first_line_length].trim());
			value = value[first_line_length..];
			while (value.len > 0)
			{
				out.appendf(" %s" +++ ENDL, (value.len >= 63 ? value[:63] : value[..]).trim());
				value = value.len >= 63 ? value[63..] : {};
			}
		}
		if (self.headers.len() > 0) out.append(ENDL);
		String body = base64::tencode(self.data);
		while (body.len > 0)
		{
			out.appendf("%s" +++ ENDL, body.len >= 64 ? body[:64] : body[..]);
			body = body.len >= 64 ? body[64..] : {};
		}
		out.appendf(POST_EB_PREFIX +++ "%s" +++ EB_DELIMITER +++ ENDL, self.tag);
	};

	return allocator == tmem ? out.str_view() : out.copy_str(allocator);
}

<* A temporary-allocator alias for `Pem.encode`, encoding the `Pem` object into a new string. *>
macro String? Pem.tencode(&self) => self.encode(tmem, text_width: text_width);

<*
 Encodes a set of input data into a `String` containing the PEM-encoded data.

 @param [&inout] allocator : "The allocator to use when creating the final output string."
 @param [in] data : "The body data for the output PEM."
 @param [in] tag : "The tag "
*>
macro String? encode(Allocator allocator, char[] data, String tag, ...)
{
	if (!data.len) return BODY_REQUIRED~;
	Pem new = create(allocator, data, tag, $vasplat);
	defer new.free();
	return new.encode(allocator);
}

<**>
macro String? tencode(char[] data, String tag, ...) => encode(tmem, data, tag, $vasplat);

<*
 Encode many inputs to a single output string that represents chained/sequential PEM objects in the order they were provided.
 The length of the `bodies` and `tags` array must be equal.
 If headers are provided, they must be arrays of String objects, matching both the number of tags and the number of bodies.

 NOTE that this is ok to be left as a macro, as most implementors should only ever call this once.

 @param [&inout] allocator : "The allocator to use when creating the final output string."
 @param [in] bodies : "An ordered array of binary arrays, each representing the body of a single PEM."
 @param [in] tags : "An ordered array of tag strings, each representing the tag of a single PEM."

 @return "A new `String`, allocated with `allocator`, that contains all PEM objects in the order they were given."
*>
macro String? encode_many(Allocator allocator, char[][] bodies, String[] tags, ...)
{
	$foreach $val : { $vasplat }:
		$assert $typeof($val) == String[] &&& $defined($val.len) &&& ($val.len % 2) == 0
			: `Each vararg must be an array of strings with an even length; i.e., { "key1", "val1", "key2", "val2", ... }`;
	$endforeach

	usz entries = max(bodies.len, tags.len, $vacount);
	switch
	{
		case bodies.len < entries: return BODY_REQUIRED~;
		case tags.len < entries: return TAG_REQUIRED~;
		case $vacount > 0 && $vacount < entries: return HEADERS_REQUIRED~;
	}

	DString out;
	out.tinit();

	$if $vacount == 0:
		foreach (x, body : bodies) @pool()
		{
			out.append(tencode(body, tags[x])!);
		};
	$else
		$foreach $x, $headers : { $vasplat }:
			out.append(tencode(bodies[$x], tags[$x], ...$headers)!);
		$endforeach
	$endif

	return allocator == tmem ? out.str_view() : out.copy_str(allocator);
}

<*
 A temporary-allocator alias for the `encode_many` function, encoding many PEM inputs to a single output string.

 @param [in] bodies : "An ordered array of binary arrays, each representing the body of a single PEM."
 @param [in] tags : "An ordered array of tag strings, each representing the tag of a single PEM."

 @return "A new `String`, allocated with `allocator`, that contains all PEM objects in the order they were given."
*>
macro String? tencode_many(char[][] bodies, String[] tags, ...) => encode_many(tmem, bodies, tags, $vasplat);
