// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// Based on Golang's "textproto" package: https://pkg.go.dev/net/textproto
module std::net::textproto;

import std::collections;
import std::io;


faultdef
	STATUS_CODE_LINE_TOO_SHORT,
	STATUS_CODE_LINE_BAD_FORMAT,
	STATUS_CODE_LINE_BAD_CODE,
	STATUS_CODE_LINE_CODE_OUT_OF_RANGE;


<* Represents a numeric status response from a server. *>
struct TextProtoStatus (Printable)
{
	<* The given message's status code. *>
	int     code;
	<* The accompanying status message with the code. *>
	String  message;
	<**>
	bool	continued;
}

fn usz? TextProtoStatus.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("%03d %s", self.code, self.message);
}


alias ProtocolError = String;


fn TextProtoStatus? parse_code_line(String line)
{
	if (line.len < 4) return STATUS_CODE_LINE_TOO_SHORT?;
	if (line[3] != ' ' && line[3] != '-') return STATUS_CODE_LINE_BAD_FORMAT?;

	int parsed_code = 0;
	if (catch @try(parsed_code, line[:3].to_int())) return STATUS_CODE_LINE_BAD_CODE?;

	if (parsed_code < 100 || parsed_code > 999) return STATUS_CODE_LINE_CODE_OUT_OF_RANGE?;

	return {
		.code = parsed_code,
		.message = line[4..],
		.continued = line[3] == '-'
	};
}


<* MIME Headers are represented by a string key -> array of strings value. *>
struct MimeHeader
{
	inline HashMap { String, LinkedList { String } } map;
}

const AsciiCharset PERMITTED_HEADER_FIELD_CHARS
	= ascii::@combine_sets(ascii::ALPHANUMERIC_SET, ascii::@create_set("!#$%&'*+-.^_`|~"));
fn bool is_valid_header_field_byte(char c) => PERMITTED_HEADER_FIELD_CHARS.contains(c);

const AsciiCharset PERMITTED_HEADER_VALUE_CHARS
	= ascii::@combine_sets((AsciiCharset)(((1ull << (0x7f-0x21))-1) << 0x21) /* all chars 0x21 - 0x7f, inclusive */, ascii::@create_set(" \t"));
fn bool is_valid_header_value_byte(char c) => PERMITTED_HEADER_VALUE_CHARS.contains(c);

// macro String _canonical_mime_header_key()

fn String canonical_mime_header_key(String header)
{
	bool upper = true;
	foreach (c : header)
	{
		if (!is_valid_header_field_byte(c)) return header; // returns the original string per RFC 9112
		// if (upper && ascii::LOWER_SET.contains(c)) return ;
		// if (!upper && ascii::UPPER_SET.contains(c)) return get_canonical_header;
		upper = c == '-';
	}

	return "";
}

fn void MimeHeader.add(&self, String key, String value)
{
	String _key = canonical_mime_header_key(key);
	if (self.map.has_key(_key) && !self.map[_key]!!.is_initialized()) self.map[_key]!!.init(mem);
	self.map[_key]!!.push(value);
}

fn void MimeHeader.set(&self, String key, String value)
{
	String _key = canonical_mime_header_key(key);
	if (self.map.has_key(_key) && !self.map[_key]!!.is_initialized()) self.map[_key]!!.init(mem);
	self.map[_key]!!.clear();
	self.map[_key]!!.push(value);
}

fn void MimeHeader.del(&self, String key)
{
	String _key = canonical_mime_header_key(key);
	if (self.map.has_key(_key) && !self.map[_key]!!.is_initialized()) return;
	self.map[_key]!!.clear();
}

fn String MimeHeader.get(&self, String key)
{
	if (self.map.is_empty() || !self.map.has_key(canonical_mime_header_key(key))) return "";

	return self.map[canonical_mime_header_key(key)]!!.get(0);
}

// fn String[] MimeHeader.values(&self, String key)
// {
//     if (self.map.is_empty() || !self.map.has_key(canonical_mime_header_key(key))) return {};

//     return list_common::list_to_array(String, self.map[canonical_mime_header_key(key)]!!, mem);
// }
