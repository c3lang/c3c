<*
XML 1.0 parser and writer based on yxml <https://dev.yorhel.nl/yxml>

A native port of the yxml C library. It provides a low-level streaming
parser (XmlReader) and a high-level DOM API (XmlDocument).

Parser Characteristics:
- Non-validating: Checks syntax/nesting but ignores DTD/Schema rules.
- Byte-oriented: State-machine based, buffer-less design.
- No core malloc: The internal engine requires no heap allocation.
- ASCII-compatible: Works with UTF-8, ISO-8859-1, etc.

What it DOES NOT do (Limitations):
- No DTD/Schema validation.
- No custom entity references (only standard &amp;, &lt;, etc.).
- No namespace assistance (prefixes are kept in the name).
- No XPath or XSLT.
- No verification of Unicode ranges (assumes valid input).
- No attribute uniqueness checks.
- No household chores.

API & Implementation:
- XmlReader: Low-level streaming parser that works with any InStream.
- XmlSerializer: XML serializer that integrates with any OutStream.
- XmlDocument: High-level DOM API for loading, searching, and modifying XML.
- Internal: The `internal::Parser` contains the core ported state-machine.

Examples: https://github.com/c3lang/c3c/tree/master/resources/examples/xml/
*>
module std::encoding::xml;
import std::collections, std::io;
import std::encoding::xml::internal @public;

faultdef XML_SYNTAX, XML_STACK_OVERFLOW, XML_TAG_MISMATCH, XML_INVALID_REF;

const sz DEFAULT_STACK_SIZE = 4096;
alias XmlNodeList = List{XmlNode*};

enum XmlEventKind
{
	START_ELEMENT,
	END_ELEMENT,
	ATTRIBUTE,
	TEXT,
	PI,
	COMMENT
}

struct XmlEvent
{
	XmlEventKind kind;
	String name;
	String value;
}

<*
 Loads and parses an XML document from a stream into a DOM tree.
 @require stack_size >= 128 && stack_size < 2 << 29 : "Stack size should be between 128 and 2^30 bytes"
*>
fn XmlDocument*? load(Allocator allocator, InStream stream, sz stack_size = DEFAULT_STACK_SIZE)
{
	XmlReader reader;

	char[] stack = alloc::alloc_array(allocator, char, stack_size);
	defer alloc::free(allocator, stack.ptr);

	reader.init(stream, stack, allocator)!;
	defer reader.destroy();

	XmlDocument* document = alloc::new(allocator, XmlDocument);
	defer catch alloc::free(allocator, document);
	document.allocator = allocator;

	XmlNode* root = null;
	XmlNode* current = null;
	XmlNodeList node_stack;
	node_stack.init(allocator);
	defer node_stack.free();

	defer catch
	{
		if (root)
		{
			root.free(allocator);
			alloc::free(allocator, root);
		}
	}


	while (true)
	{
		XmlEvent? event_opt = reader.next_event();
		if (catch err = event_opt)
		{
			if (err == io::EOF) break;
			return err~;
		}
		XmlEvent event = event_opt;
		switch (event.kind)
		{
			case START_ELEMENT:
				XmlNode* node = new_start_node(document, event.name);
				switch
				{
					case current != null: current.children.push(node);
					default: root = node;
				}
				node_stack.push(current);
				current = node;

			case END_ELEMENT:
				if (!current) return XML_TAG_MISMATCH~;
				current = node_stack.pop()!!;

			case ATTRIBUTE:
				if (!current) return XML_SYNTAX~;
				current.attributes.set(event.name, event.value.copy(allocator));

			case TEXT:
				XmlNode* node = new_text_node(document, event.value);
				switch
				{
					case current != null: current.children.push(node);
					// Allow top-level text as a fallback root (Standard XML requires a root element).
					case !root: root = node;
				}

			case PI:
				XmlNode* node = new_pi_node(document, event.name, event.value);
				switch
				{
					case current != null: current.children.push(node);
					// Allow PI to be the initial root; will be replaced if a START_ELEMENT follows.
					case !root: root = node;
				}

			case COMMENT:
				XmlNode* node = new_comment_node(document, event.value);
				switch
				{
					case current != null: current.children.push(node);
					// Allow top-level comments as fallback root nodes.
					case !root: root = node;
				}
		}
	}

	if (node_stack.len() || current) return XML_TAG_MISMATCH~;
	document.root = root;
	return document;
}

<* Loads and parses an XML document from a stream into a DOM tree using the temp allocator. *>
fn XmlDocument*? tload(InStream stream, usz stack_size = DEFAULT_STACK_SIZE)
{
	return load(tmem, stream, stack_size);
}

<* Parses an XML document from a string. *>
fn XmlDocument*? parse(Allocator allocator, String s, usz stack_size = DEFAULT_STACK_SIZE)
{
	ByteReader reader;
	reader.init(s);
	return load(allocator, &reader, stack_size);
}

<* Parses an XML document from a string using the temp allocator. *>
fn XmlDocument*? tparse(String s, usz stack_size = DEFAULT_STACK_SIZE)
{
	return parse(tmem, s, stack_size);
}


// ----------------------------------------------------------------------------

fn XmlNode* new_text_node(XmlDocument *doc, String text)
{
	XmlNode* node = alloc::new(doc.allocator, XmlNode, { .kind = TEXT, .value = text.copy(doc.allocator), .doc = doc });
	node.attributes.init(doc.allocator);
	node.children.init(doc.allocator);
	return node;
}

fn XmlNode* new_start_node(XmlDocument *doc, String name)
{
	XmlNode* node = alloc::new(doc.allocator, XmlNode, { .kind = START_ELEMENT, .name = name.copy(doc.allocator), .doc = doc });
	node.attributes.init(doc.allocator);
	node.children.init(doc.allocator);
	return node;
}

fn XmlNode* new_pi_node(XmlDocument *doc, String name, String value)
{
	XmlNode* node = alloc::new(doc.allocator, XmlNode, { .kind = PI, .name = name.copy(doc.allocator), .value = value.copy(doc.allocator), .doc = doc });
	node.attributes.init(doc.allocator);
	node.children.init(doc.allocator);
	return node;
}

fn XmlNode* new_comment_node(XmlDocument *doc, String value)
{
	XmlNode* node = alloc::new(doc.allocator, XmlNode, { .kind = COMMENT, .value = value.copy(doc.allocator), .doc = doc });
	node.attributes.init(doc.allocator);
	node.children.init(doc.allocator);
	return node;
}

/**
 * An XML node in the DOM tree.
 */
struct XmlNode
{
	XmlEventKind kind;
	String name;
	String value;
	HashMap{String, String} attributes;
	XmlNodeList children;
	XmlDocument* doc; // Back-pointer to owner document
}

/**
 * A parsed XML document.
 */
struct XmlDocument
{
	XmlNode* root;
	Allocator allocator;
}

alias NodeFilter = fn bool(XmlNode* node);

/**
 * Recursively searches for nodes matching the filter.
 */
fn void XmlNode.find_by_filter(&self, NodeFilter filter, XmlNodeList* results)
{
	if (filter(self)) results.push(self);
	foreach (child : self.children)
	{
		child.find_by_filter(filter, results);
	}
}

/**
 * Finds all nodes with the given name.
 */
fn void XmlNode.find_all(&self, String name, XmlNodeList* results)
{
	if (self.name == name) results.push(self);
	foreach (child : self.children)
	{
		child.find_all(name, results);
	}
}

/**
 * Finds the first node with the given name (depth-first).
 */
fn XmlNode*? XmlNode.find(&self, String name)
{
	if (self.name == name) return self;
	foreach (child : self.children)
	{
		if (try found = child.find(name)) return found;
	}
	return NOT_FOUND~;
}

/**
 * Returns the text content of the node (from its first TEXT child).
 */
fn String? XmlNode.text(&self)
{
	foreach (child : self.children)
	{
		if (child.kind == TEXT) return child.value;
	}
	return NOT_FOUND~;
}

/**
 * Sets the text content of the node, creating or updating a TEXT child.
 */
fn void XmlNode.set_text(&self, String text)
{
	foreach (child : self.children)
	{
		if (child.kind == TEXT)
		{
			child.set_value(text);
			return;
		}
	}
	// Create new text node
	self.children.push(new_text_node(self.doc, text));
}

/**
 * Sets the node's name, handling memory management.
 */
fn void XmlNode.set_name(&self, String name)
{
	self.name.free(self.doc.allocator);
	self.name = name.copy(self.doc.allocator);
}

/**
 * Sets the node's value, handling memory management.
 */
fn void XmlNode.set_value(&self, String value)
{
	self.value.free(self.doc.allocator);
	self.value = value.copy(self.doc.allocator);
}

/**
 * Sets an attribute value.
 */
fn void XmlNode.set_attr(&self, String key, String value) @operator([]=)
{
	(void)self.attributes.get(key).free(self.doc.allocator);
	self.attributes.set(key, value.copy(self.doc.allocator));
}

/**
 * Gets an attribute value.
 */
fn String? XmlNode.attr(&self, String key) @operator([])
{
	return self.attributes.get(key);
}

/**
 * Frees the node and all its children.
 */
fn void XmlNode.free(&self, Allocator allocator)
{
	self.name.free(allocator);
	self.value.free(allocator);
	self.attributes.@each(; key, val)
	{
		val.free(allocator);
	};
	self.attributes.free();
	foreach (child : self.children)
	{
		child.free(allocator);
		alloc::free(allocator, child);
	}
	self.children.free();
}

/**
 * Frees the document tree and the document container itself.
 */
fn void XmlDocument.free(XmlDocument* self)
{
	if (!self) return;
	Allocator allocator = self.allocator;
	if (self.root)
	{
		self.root.free(allocator);
		alloc::free(allocator, self.root);
	}
	alloc::free(allocator, self);
}

// ----------------------------------------------------------------------------

alias ReadFn = $typeof(InStream.read);
alias ReadByteFn = $typeof(InStream.read_byte);

struct XmlReader
{
	InStream stream;
	ReadFn read_fn;
	Allocator allocator;
	Parser parser;
	DString buffer;
	DString tag_stack;
	sz io_buf_pos;
	sz io_buf_len;
	Ret peeked_res;
	char[512] io_buf;
}

fn void? XmlReader.init(&self, InStream stream, char[] stack, Allocator allocator = tmem)
{
	mem::clear(self, XmlReader::size);
	self.stream = stream;
	self.read_fn = &stream.read;
	self.allocator = allocator;
	internal::init(&self.parser, stack);
	self.buffer.init(allocator, 256);
	self.tag_stack.init(allocator, 256);
}

fn void XmlReader.destroy(&self)
{
	self.buffer.free();
	self.tag_stack.free();
}

fn Ret? XmlReader.low_level_next(&self) @inline
{
	if (self.peeked_res != OK)
	{
		Ret res = self.peeked_res;
		self.peeked_res = OK;
		return res;
	}
	while (true)
	{
		if (self.io_buf_pos >= self.io_buf_len)
		{
			sz n = self.read_fn(self.stream.ptr, &self.io_buf)!;
			if (n == 0) return io::EOF~;
			self.io_buf_len = n;
			self.io_buf_pos = 0;
		}

		while (self.io_buf_pos < self.io_buf_len)
		{
			char c = self.io_buf[self.io_buf_pos++];
			Ret res = internal::xml_parse_internal(&self.parser, (int)c);
			if (res < OK)
			{
				switch (res)
				{
					case EEOF:   return io::UNEXPECTED_EOF~;
					case EREF:   return xml::XML_INVALID_REF~;
					case ECLOSE: return xml::XML_TAG_MISMATCH~;
					case ESTACK: return xml::XML_STACK_OVERFLOW~;
					case ESYN:   return xml::XML_SYNTAX~;
					default: unreachable();
				}
			}
			if (res > OK) return res;
		}
	}
}

fn void? XmlReader.close(&self)
{
	if (internal::eof(&self.parser) != OK) return xml::XML_SYNTAX~;
	return;
}

fn XmlEvent? XmlReader.next_event(&self)
{
	while (true)
	{
		Ret? res_opt = self.low_level_next();
		if (catch err = res_opt)
		{
			if (err == io::EOF)
			{
				if (internal::eof(&self.parser) != Ret.OK) return xml::XML_SYNTAX~;
				return io::EOF~;
			}
			return err~;
		}
		Ret res = res_opt;
		switch (res)
		{
			case ELEMSTART:
			{
				String name = self.parser.elem_view();
				self.tag_stack.append(name);
				self.tag_stack.append('\0');
				return { .kind = START_ELEMENT, .name = name };
			}
			case ELEMEND:
			{
				String tags = self.tag_stack.str_view();
				if (tags.len == 0) return xml::XML_TAG_MISMATCH~;
				// The last tag is before the last null byte.
				// Find the second-to-last null byte (or start of string).
				sz last_null = tags.len - 1;
				sz prev_null = last_null;
				while (prev_null > 0 && tags[prev_null - 1] != 0) prev_null--;

				String name = tags[prev_null : last_null - prev_null];
				self.tag_stack.chop(prev_null);
				return { .kind = END_ELEMENT, .name = name };
			}
			case ATTRSTART:
				String name = self.parser.attr_view();
				self.buffer.clear();
				while (true)
				{
					Ret next_res = self.low_level_next()!;
					if (next_res == ATTRVAL)
					{
						self.buffer.append(self.parser.data_view());
					}
					else if (next_res == ATTREND) break;
				}
				return { .kind = ATTRIBUTE, .name = name, .value = self.buffer.str_view() };
			case CONTENT:
				self.buffer.clear();
				self.buffer.append(self.parser.data_view());
				while (true)
				{
					Ret? next_res_opt = self.low_level_next();
					if (catch err = next_res_opt)
					{
						if (err == io::EOF) break;
						return err~;
					}
					Ret next_res = next_res_opt;
					if (next_res == Ret.CONTENT)
					{
						self.buffer.append(self.parser.data_view());
					}
					else
					{
						self.peeked_res = next_res;
						break;
					}
				}
				return { .kind = TEXT, .value = self.buffer.str_view() };
			case PISTART:
				String name = self.parser.pi_view();
				self.buffer.clear();
				while (true)
				{
					Ret next_res = self.low_level_next()!;
					if (next_res == PICONTENT)
					{
						self.buffer.append(self.parser.data_view());
					}
					else if (next_res == PIEND) break;
				}
				return { .kind = PI, .name = name, .value = self.buffer.str_view() };
			default:
				continue;
		}
	}
}

alias WriteFn = $typeof(OutStream.write);
alias WriteByteFn = $typeof(OutStream.write_byte);

struct XmlSerializer
{
	OutStream stream;
	WriteFn write_fn;
	WriteByteFn write_byte_fn;
	bool in_tag;
}

fn void? serialize(OutStream stream, XmlDocument* doc)
{
	XmlSerializer w;
	w.init(stream);
	w.start_doc()!;
	w.serialize(doc)!;
	w.end_doc()!;
}

fn void XmlSerializer.init(&self, OutStream stream)
{
	*self = {
		.stream        = stream,
		.write_fn      = &stream.write,
		.write_byte_fn = &stream.write_byte
	};
}

fn void? XmlSerializer.start_doc(&self, String version = "1.0", String encoding = "UTF-8")
{
	self.write_fn(self.stream.ptr, `<?xml version="`)!;
	self.write_fn(self.stream.ptr, version)!;
	self.write_fn(self.stream.ptr, `" encoding="`)!;
	self.write_fn(self.stream.ptr, encoding)!;
	self.write_fn(self.stream.ptr, `?>` "\n")!;
}

fn void? XmlSerializer.end_doc(&self)
{
	if (self.in_tag)
	{
		self.write_fn(self.stream.ptr, ">")!;
		self.in_tag = false;
	}
}

fn void? XmlSerializer.start_element(&self, String name)
{
	if (self.in_tag) self.write_fn(self.stream.ptr, ">")!;
	self.write_fn(self.stream.ptr, "<")!;
	self.write_fn(self.stream.ptr, name)!;
	self.in_tag = true;
}

fn void? XmlSerializer.end_element(&self, String name, bool self_closing = false)
{
	if (self.in_tag)
	{
		if (self_closing)
		{
			self.write_fn(self.stream.ptr, " />")!;
		}
		else
		{
			self.write_fn(self.stream.ptr, ">")!;
			self.write_fn(self.stream.ptr, "</")!;
			self.write_fn(self.stream.ptr, name)!;
			self.write_fn(self.stream.ptr, ">")!;
		}
		self.in_tag = false;
	}
	else
	{
		self.write_fn(self.stream.ptr, "</")!;
		self.write_fn(self.stream.ptr, name)!;
		self.write_fn(self.stream.ptr, ">")!;
	}
}


fn void? XmlSerializer.add_empty_element(&self, String name)
{
	self.start_element(name)!;
	self.end_element(name, true)!;
}

fn void? XmlSerializer.add_attribute(&self, String name, String value)
{
	assert(self.in_tag);
	self.write_fn(self.stream.ptr, " ")!;
	self.write_fn(self.stream.ptr, name)!;
	self.write_fn(self.stream.ptr, `="`)!;
	self.add_content_escaped(value)!;
	self.write_fn(self.stream.ptr, `"`)!;
}

fn void? XmlSerializer.add_content_escaped(&self, String s)
{
	foreach (c : s)
	{
		switch (c)
		{
			case '<': self.write_fn(self.stream.ptr, "&lt;")!;
			case '>': self.write_fn(self.stream.ptr, "&gt;")!;
			case '&': self.write_fn(self.stream.ptr, "&amp;")!;
			case '"': self.write_fn(self.stream.ptr, "&quot;")!;
			case '\'': self.write_fn(self.stream.ptr, "&apos;")!;
			default: self.write_byte_fn(self.stream.ptr, c)!;
		}
	}
}

fn void? XmlSerializer.add_content(&self, char[] bytes)
{
	if (self.in_tag)
	{
		self.write_fn(self.stream.ptr, ">")!;
		self.in_tag = false;
	}
	self.add_content_escaped((String)bytes)!;
}

/**
 * Recursively writes the node and its children to an XmlSerializer.
 */
fn void? XmlSerializer.serialize_node(&self, XmlNode* node)
{
	switch (node.kind)
	{
		case START_ELEMENT:
			self.start_element(node.name)!;
			node.attributes.@each(; key, val)
			{
				self.add_attribute(key, val)!;
			};
			foreach (child : node.children)
			{
				self.serialize_node(child)!;
			}
			self.end_element(node.name)!;
		case TEXT:
			self.add_content(node.value)!;
		case PI:
			self.add_content("<?")!;
			self.add_content(node.name)!;
			self.add_content(" ")!;
			self.add_content(node.value)!;
			self.add_content("?>")!;
		case COMMENT:
			self.add_content("<!--")!;
			self.add_content(node.value)!;
			self.add_content("-->")!;
		default:
			break;
	}
}

/**
 * Writes the entire document to an XmlSerializer.
 */
fn void? XmlSerializer.serialize(&self, XmlDocument* doc)
{
	if (doc.root) self.serialize_node(doc.root)!;
}



<*
 Yxml in c3 (https://dev.yorhel.nl/yxml), by Manu Linares

 (https://code.blicky.net/yorhel/yxml/src/branch/master/yxml.c)
 (https://code.blicky.net/yorhel/yxml/src/branch/master/yxml.h)

 I did my best to do a mostly one to one translation of the yxml C source to C3.
*>
module std::encoding::xml::internal @private;

enum Ret
{
	EEOF     , 	/* Unexpected EOF                             */
	EREF     ,	/* Invalid character or entity reference (&whatever;) */
	ECLOSE   ,	/* Close tag does not match open tag (<Tag> .. </OtherTag>) */
	ESTACK   ,	/* Stack overflow (too deeply nested tags or too long element/attribute name) */
	ESYN     ,	/* Syntax error (unexpected byte)             */
	OK       ,	/* Character consumed, no new token present   */
	ELEMSTART,	/* Start of an element:   '<Tag ..'           */
	CONTENT  ,	/* Element content                            */
	ELEMEND  ,	/* End of an element:     '.. />' or '</Tag>' */
	ATTRSTART,	/* Attribute:             'Name=..'           */
	ATTRVAL  ,	/* Attribute value                            */
	ATTREND  ,	/* End of attribute       '.."'               */
	PISTART  ,	/* Start of a processing instruction          */
	PICONTENT,	/* Content of a PI                            */
	PIEND    ,  /* End of a processing instruction            */
}

/* When, exactly, are tokens returned?
 *
 * <TagName
 *   '>' ELEMSTART
 *   '/' ELEMSTART, '>' ELEMEND
 *   ' ' ELEMSTART
 *     '>'
 *     '/', '>' ELEMEND
 *     Attr
 *       '=' ATTRSTART
 *         "X ATTRVAL
 *           'Y'  ATTRVAL
 *             'Z'  ATTRVAL
 *               '"' ATTREND
 *                 '>'
 *                 '/', '>' ELEMEND
 *
 * </TagName
 *   '>' ELEMEND
 */

struct Parser
{
/* PUBLIC (read-only) */

	/* Name of the current element, zero-length if not in any element. Changed
	 * after YXML_ELEMSTART. The pointer will remain valid up to and including
	 * the next non-YXML_ATTR* token, the pointed-to buffer will remain valid
	 * up to and including the YXML_ELEMEND for the corresponding element. */
	char* elem;

	/* The last read character(s) of an attribute value (YXML_ATTRVAL), element
	 * data (YXML_CONTENT), or processing instruction (YXML_PICONTENT). Changed
	 * after one of the respective YXML_ values is returned, and only valid
	 * until the next yxml_parse() call. Usually, this string only consists of
	 * a single byte, but multiple bytes are returned in the following cases:
	 * - "<?SomePI ?x ?>": The two characters "?x"
	 * - "<![CDATA[ ]x ]]>": The two characters "]x"
	 * - "<![CDATA[ ]]x ]]>": The three characters "]]x"
	 * - "&#N;" and "&#xN;", where dec(n) > 127. The referenced Unicode
	 *   character is then encoded in multiple UTF-8 bytes.
	 */
	char[8] data;

	/* Name of the current attribute. Changed after YXML_ATTRSTART, valid up to
	 * and including the next YXML_ATTREND. */
	char* attr;

	/* Name/target of the current processing instruction, zero-length if not in
	 * a PI. Changed after YXML_PISTART, valid up to (but excluding)
	 * the next YXML_PIEND. */
	char* pi;

	/* Line number, byte offset within that line, and total bytes read. These
	 * values refer to the position _after_ the last byte given to
	 * yxml_parse(). These are useful for debugging and error reporting. */
	ulong byte;
	ulong total;
	uint line;


	/* PRIVATE */
	State state;
	char* stack; /* Stack of element names + attribute/PI name, separated by \0. Also starts with a \0. */
	usz stacksize, stacklen;
	uint reflen;
	uint quote;
	State nextstate; /* Used for '@' state remembering and for the "string" consuming state */
	uint ignore;
	char* string;
}

<* Returns a view of the current element name. *>
fn String Parser.elem_view(&self) @inline => ((ZString)self.elem).str_view();
<* Returns a view of the current attribute name.*>
fn String Parser.attr_view(&self) @inline => ((ZString)self.attr).str_view();
<* Returns a view of the current processing instruction name. *>
fn String Parser.pi_view(&self) @inline => ((ZString)self.pi).str_view();
<* Returns a view of the contents of the data buffer. *>
fn String Parser.data_view(&self) @inline => ((ZString)&self.data[0]).str_view();

enum State : int
{
	STRING,
	ATTR0, ATTR1, ATTR2, ATTR3, ATTR4,
	CD0, CD1, CD2,
	COMMENT0, COMMENT1, COMMENT2, COMMENT3, COMMENT4,
	DT0, DT1, DT2, DT3, DT4,
	ELEM0, ELEM1, ELEM2, ELEM3,
	ENC0, ENC1, ENC2, ENC3,
	ETAG0, ETAG1, ETAG2,
	INIT,
	LE0, LE1, LE2, LE3,
	LEE1, LEE2,
	LEQ0,
	MISC0, MISC1, MISC2, MISC2A, MISC3,
	PI0, PI1, PI2, PI3, PI4,
	STD0, STD1, STD2, STD3,
	VER0, VER1, VER2, VER3,
	XMLDECL0, XMLDECL1, XMLDECL2, XMLDECL3, XMLDECL4, XMLDECL5, XMLDECL6, XMLDECL7, XMLDECL8, XMLDECL9,
}

macro bool isChar(uint c) => true;
/* 0xd should be part of SP, too, but yxml_parse() already normalizes that into 0xa */
macro bool isSP(uint c)        => (c == 0x20 || c == 0x09 || c == 0x0a);
macro bool isAlpha(uint c)     => ((c|32)-'a' < 26);
macro bool isNum(uint c)       => (c-'0' < 10);
macro bool isHex(uint c)       => (isNum(c) || (c|32)-'a' < 6);
macro bool isEncName(uint c)   => (isAlpha(c) || isNum(c) || c == '.' || c == '_' || c == '-');
macro bool isNameStart(uint c) => (isAlpha(c) || c == ':' || c == '_' || c >= 128);
macro bool isName(uint c)      => (isNameStart(c) || isNum(c) || c == '-' || c == '.');
/* XXX: The valid characters are dependent on the quote char, hence the access to x->quote */
macro bool isAttValue(Parser* x, uint c) => (isChar(c) && c != x.quote && c != '<' && c != '&');
/* Anything between '&' and ';', the yxml_ref* functions will do further
 * validation. Strictly speaking, this is "yxml_isName(c) || c == '#'", but
 * this parser doesn't understand entities with '.', ':', etc, anwyay.  */
macro bool isRef(uint c) => (isNum(c) || isAlpha(c) || c == '#');

macro ulong int_from_5_chars(uint a, uint b, uint c, uint d, uint e) =>
	((((ulong)a)<<32) | (((ulong)b)<<24) | (((ulong)c)<<16) | (((ulong)d)<<8) | (ulong)e);

/* Set the given char value to ch (0<=ch<=255). */
fn void setchar(char* dest, uint ch)
{
	*dest = (char)ch;
}

/* Similar to yxml_setchar(), but will convert ch (any valid unicode point) to
 * UTF-8 and appends a '\0'. dest must have room for at least 5 bytes. */
fn void setutf8(char* dest, uint ch)
{
	switch (ch)
	{
		case 0..0x007F:
			setchar(dest, ch); dest++;
		case 0x0080..0x07FF:
			setchar(dest, 0xC0 | (ch>>6)); dest++;
			setchar(dest, 0x80 | (ch & 0x3F)); dest++;
		case 0x0800..0xFFFF:
			setchar(dest, 0xE0 | (ch>>12)); dest++;
			setchar(dest, 0x80 | ((ch>>6) & 0x3F)); dest++;
			setchar(dest, 0x80 | (ch & 0x3F)); dest++;
		default:
			setchar(dest, 0xF0 | (ch>>18)); dest++;
			setchar(dest, 0x80 | ((ch>>12) & 0x3F)); dest++;
			setchar(dest, 0x80 | ((ch>>6) & 0x3F)); dest++;
			setchar(dest, 0x80 | (ch & 0x3F)); dest++;
	}
	*dest = 0;
}

fn Ret datacontent(Parser* x, uint ch)
{
	setchar(&x.data[0], ch);
	x.data[1] = 0;
	return CONTENT;
}

fn Ret datapi1(Parser* x, uint ch)
{
	setchar(&x.data[0], ch);
	x.data[1] = 0;
	return PICONTENT;
}

fn Ret datapi2(Parser* x, uint ch)
{
	x.data[0] = '?';
	setchar(&x.data[1], ch);
	x.data[2] = 0;
	return PICONTENT;
}

fn Ret datacd1(Parser* x, uint ch)
{
	x.data[0] = ']';
	setchar(&x.data[1], ch);
	x.data[2] = 0;
	return CONTENT;
}

fn Ret datacd2(Parser* x, uint ch)
{
	x.data[0] = ']';
	x.data[1] = ']';
	setchar(&x.data[2], ch);
	x.data[3] = 0;
	return CONTENT;
}

fn Ret dataattr(Parser* x, uint ch)
{
	/* Normalize attribute values according to the XML spec section 3.3.3. */
	setchar(&x.data[0], ch == 0x9 || ch == 0xa ? 0x20 : ch);
	x.data[1] = 0;
	return ATTRVAL;
}

fn Ret pushstack(Parser* x, char** res, uint ch)
{
	if (x.stacklen+2 >= x.stacksize) return ESTACK;
	x.stacklen++;
	*res = (char *)x.stack+x.stacklen;
	x.stack[x.stacklen] = (char)ch;
	x.stacklen++;
	x.stack[x.stacklen] = 0;
	return OK;
}

fn Ret pushstackc(Parser* x, uint ch)
{
	if (x.stacklen+1 >= x.stacksize) return ESTACK;
	x.stack[x.stacklen] = (char)ch;
	x.stacklen++;
	x.stack[x.stacklen] = 0;
	return OK;
}

fn void popstack(Parser* x)
{
	do { x.stacklen--; }
	while (x.stack[x.stacklen] != 0);
}

fn Ret elemstart(Parser* x, uint ch)   => pushstack(x, &x.elem, ch);
fn Ret elemname(Parser* x, uint ch)    => pushstackc(x, ch);
fn Ret elemnameend(Parser* x, uint ch) => ELEMSTART;

/* Also used in elemcloseend(), since this function just removes the last
 * element from the stack and returns ELEMEND. */
fn Ret selfclose(Parser* x, uint ch)
{
	popstack(x);
	if (x.stacklen)
	{
		x.elem = (char*)x.stack+x.stacklen-1;
		while (*(x.elem - 1))
		{
			x.elem--;
		}
		return ELEMEND;
	}
	x.elem = (char*)x.stack;
	x.state = MISC3;
	return ELEMEND;
}

fn Ret elemclose(Parser* x, uint ch)
{
	if (*((char*)x.elem) != ch) return ECLOSE;
	x.elem++;
	return OK;
}

fn Ret elemcloseend(Parser* x, uint ch)
{
	if (*x.elem) return ECLOSE;
	return selfclose(x, ch);
}

fn Ret attrstart(Parser* x, uint ch)   => pushstack(x, &x.attr, ch);
fn Ret attrname(Parser* x, uint ch)    => pushstackc(x, ch);
fn Ret attrnameend(Parser* x, uint ch) => ATTRSTART;

fn Ret attrvalend(Parser* x, uint ch)
{
	popstack(x);
	return ATTREND;
}

fn Ret pistart(Parser* x, uint ch) => pushstack(x, &x.pi, ch);
fn Ret piname(Parser* x, uint ch)  => pushstackc(x, ch);

fn Ret piabort(Parser* x, uint ch)
{
	popstack(x);
	return OK;
}

fn Ret pinameend(Parser* x, uint ch) => (x.pi[0]|32) == 'x' && (x.pi[1]|32) == 'm' && (x.pi[2]|32) == 'l' && !x.pi[3] ? ESYN : PISTART;

fn Ret pivalend(Parser* x, uint ch)
{
	popstack(x); x.pi = (char *)x.stack;
	return PIEND;
}

fn Ret refstart(Parser* x, uint ch)
{
	mem::set(&x.data[0], 0, 8);
	x.reflen = 0;
	return OK;
}

fn Ret ref(Parser* x, uint ch)
{
	if (x.reflen >= x.data.len-1) return EREF;
	setchar(&x.data[x.reflen], ch);
	x.reflen++;
	return OK;
}

fn Ret refend(Parser* x, Ret ret)
{
	char* r = &x.data[0];
	uint ch = 0;
	if (*r == '#')
	{
		if (r[1] == 'x')
		{
			for (r += 2; isHex((uint)*r); r++)
			{
				ch = (ch<<4) + (*r <= '9' ? *r-'0' : (*r|32)-'a' + 10);
			}
		}
		else
		{
			for (r++; isNum((uint)*r); r++)
			{
				ch = (ch*10) + (*r-'0');
			}
		}
		if (*r)
		{
			ch = 0;
		}
	}
	else
	{
		ulong i = int_from_5_chars(r[0], r[1], r[2], r[3], r[4]);
		ch =
			i == int_from_5_chars('l','t', 0,  0, 0) ? '<' :
			i == int_from_5_chars('g','t', 0,  0, 0) ? '>' :
			i == int_from_5_chars('a','m','p', 0, 0) ? '&' :
			i == int_from_5_chars('a','p','o','s',0) ? '\'':
			i == int_from_5_chars('q','u','o','t',0) ? '"' : 0;
	}

	/* Codepoints not allowed in the XML 1.1 definition of a Char */
	if (!ch || ch > 0x10FFFF || ch == 0xFFFE || ch == 0xFFFF || (ch-0xDFFF) < 0x7FF) return EREF;
	setutf8(&x.data[0], ch);
	return ret;

}

fn Ret refcontent(Parser* x, uint ch) => refend(x, CONTENT);
fn Ret refattrval(Parser* x, uint ch) => refend(x, ATTRVAL);

fn void init(Parser* x, char[] stack)
{
	*x = {};
	x.line = 1;
	x.stack = stack;
	x.stacksize = stack.len;
	*x.stack = 0;
	x.elem = x.pi = x.attr = x.stack;
	x.state = INIT;
}

fn Ret xml_parse_internal(Parser* x, int _ch) @inline
{
	/* Ensure that characters are in the range of 0..255 rather than -126..125.
	 * All character comparisons are done with positive integers. */
	uint ch = (uint)(_ch+256) & 0xff;
	if (!ch) return ESYN;
	x.total++;

	/* End-of-Line normalization, "\rX", "\r\n" and "\n" are recognized and
	 * normalized to a single '\n' as per XML 1.0 section 2.11. XML 1.1 adds
	 * some non-ASCII character sequences to this list, but we can only handle
	 * ASCII here without making assumptions about the input encoding. */
	if (x.ignore == ch)
	{
		x.ignore = 0;
		return OK;
	}
	x.ignore = (ch == 0xd) ? 0xa : 0;
	if (ch == 0xa || ch == 0xd)
	{
		ch = 0xa;
		x.line++;
		x.byte = 0;
	}
	x.byte++;

	switch (x.state)
	{
		case STRING:
			if (ch == *x.string)
			{
				x.string++;
				if (!*x.string) x.state = x.nextstate;
				return OK;
			}
			break;
		case ATTR0:
			if (isName(ch)) return attrname(x, ch);
			if (isSP(ch))
			{
				x.state = ATTR1;
				return attrnameend(x, ch);
			}
			if (ch == (char)'=')
			{
				x.state = ATTR2;
				return attrnameend(x, ch);
			}
			break;
		case ATTR1:
			if (isSP(ch)) return OK;
			if (ch == (char)'=')
			{
				x.state = ATTR2;
				return OK;
			}
			break;
		case ATTR2:
			if (isSP(ch)) return OK;
			if (ch == (char)'\'' || ch == (char)'"')
			{
				x.state = ATTR3;
				x.quote = ch;
				return OK;
			}
			break;
		case ATTR3:
			if (isAttValue(x, ch)) return dataattr(x, ch);
			if (ch == (char)'&')
			{
				x.state = ATTR4;
				return refstart(x, ch);
			}
			if (x.quote == ch)
			{
				x.state = ELEM2;
				return attrvalend(x, ch);
			}
			break;
		case ATTR4:
			if (isRef(ch)) return ref(x, ch);
			if (ch == (char)'\x3b')
			{
				x.state = ATTR3;
				return refattrval(x, ch);
			}
			break;
		case CD0:
			if (ch == (char)']')
			{
				x.state = CD1;
				return OK;
			}
			if (isChar(ch)) return datacontent(x, ch);
			break;
		case CD1:
			if (ch == (char)']')
			{
				x.state = CD2;
				return OK;
			}
			if (isChar(ch))
			{
				x.state = CD0;
				return datacd1(x, ch);
			}
			break;
		case CD2:
			if (ch == (char)']') return datacontent(x, ch);
			if (ch == (char)'>')
			{
				x.state = MISC2;
				return OK;
			}
			if (isChar(ch))
			{
				x.state = CD0;
				return datacd2(x, ch);
			}
			break;
		case COMMENT0:
			if (ch == (char)'-')
			{
				x.state = COMMENT1;
				return OK;
			}
			break;
		case COMMENT1:
			if (ch == (char)'-')
			{
				x.state = COMMENT2;
				return OK;
			}
			break;
		case COMMENT2:
			if (ch == (char)'-')
			{
				x.state = COMMENT3;
				return OK;
			}
			if (isChar(ch)) return OK;
			break;
		case COMMENT3:
			if (ch == (char)'-')
			{
				x.state = COMMENT4;
				return OK;
			}
			if (isChar(ch))
			{
				x.state = COMMENT2;
				return OK;
			}
			break;
		case COMMENT4:
			if (ch == (char)'>')
			{
				x.state = x.nextstate;
				return OK;
			}
			break;
		case DT0:
			if (ch == (char)'>')
			{
				x.state = MISC1;
				return OK;
			}
			if (ch == (char)'\'' || ch == (char)'"')
			{
				x.state = DT1;
				x.quote = ch;
				x.nextstate = DT0;
				return OK;
			}
			if (ch == (char)'<')
			{
				x.state = DT2;
				return OK;
			}
			if (isChar(ch)) return OK;
			break;
		case DT1:
			if (x.quote == ch)
			{
				x.state = x.nextstate;
				return OK;
			}
			if (isChar(ch)) return OK;
			break;
		case DT2:
			if (ch == (char)'?')
			{
				x.state = PI0;
				x.nextstate = DT0;
				return OK;
			}
			if (ch == (char)'!')
			{
				x.state = DT3;
				return OK;
			}
			break;
		case DT3:
			if (ch == (char)'-')
			{
				x.state = COMMENT1;
				x.nextstate = DT0;
				return OK;
			}
			if (isChar(ch))
			{
				x.state = DT4;
				return OK;
			}
			break;
		case DT4:
			if (ch == (char)'\'' || ch == (char)'"')
			{
				x.state = DT1;
				x.quote = ch;
				x.nextstate = DT4;
				return OK;
			}
			if (ch == (char)'>')
			{
				x.state = DT0;
				return OK;
			}
			if (isChar(ch)) return OK;
			break;
		case ELEM0:
			if (isName(ch)) return elemname(x, ch);
			if (isSP(ch))
			{
				x.state = ELEM1;
				return elemnameend(x, ch);
			}
			if (ch == (char)'/')
			{
				x.state = ELEM3;
				return elemnameend(x, ch);
			}
			if (ch == (char)'>')
			{
				x.state = MISC2;
				return elemnameend(x, ch);
			}
			break;
		case ELEM1:
			if (isSP(ch)) return OK;
			if (ch == (char)'/')
			{
				x.state = ELEM3;
				return OK;
			}
			if (ch == (char)'>')
			{
				x.state = MISC2;
				return OK;
			}
			if (isNameStart(ch))
			{
				x.state = ATTR0;
				return attrstart(x, ch);
			}
			break;
		case ELEM2:
			if (isSP(ch))
			{
				x.state = ELEM1;
				return OK;
			}
			if (ch == (char)'/')
			{
				x.state = ELEM3;
				return OK;
			}
			if (ch == (char)'>')
			{
				x.state = MISC2;
				return OK;
			}
			break;
		case ELEM3:
			if (ch == (char)'>')
			{
				x.state = MISC2;
				return selfclose(x, ch);
			}
			break;
		case ENC0:
			if (isSP(ch)) return OK;
			if (ch == (char)'=')
			{
				x.state = ENC1;
				return OK;
			}
			break;
		case ENC1:
			if (isSP(ch)) return OK;
			if (ch == (char)'\'' || ch == (char)'"')
			{
				x.state = ENC2;
				x.quote = ch;
				return OK;
			}
			break;
		case ENC2:
			if (isAlpha(ch))
			{
				x.state = ENC3;
				return OK;
			}
			break;
		case ENC3:
			if (isEncName(ch)) return OK;
			if (x.quote == ch)
			{
				x.state = XMLDECL6;
				return OK;
			}
			break;
		case ETAG0:
			if (isNameStart(ch))
			{
				x.state = ETAG1;
				return elemclose(x, ch);
			}
			break;
		case ETAG1:
			if (isName(ch)) return elemclose(x, ch);
			if (isSP(ch))
			{
				x.state = ETAG2;
				return elemcloseend(x, ch);
			}
			if (ch == (char)'>')
			{
				x.state = MISC2;
				return elemcloseend(x, ch);
			}
			break;
		case ETAG2:
			if (isSP(ch)) return OK;
			if (ch == (char)'>')
			{
				x.state = MISC2;
				return OK;
			}
			break;
		case INIT:
			if (ch == (char)'\xef')
			{
				x.state = STRING;
				x.nextstate = MISC0;
				x.string = (char*)"\xbb\xbf";
				return OK;
			}
			if (isSP(ch))
			{
				x.state = MISC0;
				return OK;
			}
			if (ch == (char)'<')
			{
				x.state = LE0;
				return OK;
			}
			break;
		case LE0:
			if (ch == (char)'!')
			{
				x.state = LEE1;
				return OK;
			}
			if (ch == (char)'?')
			{
				x.state = LEQ0;
				return OK;
			}
			if (isNameStart(ch))
			{
				x.state = ELEM0;
				return elemstart(x, ch);
			}
			break;
		case LE1:
			if (ch == (char)'!')
			{
				x.state = LEE1;
				return OK;
			}
			if (ch == (char)'?')
			{
				x.state = PI0;
				x.nextstate = MISC1;
				return OK;
			}
			if (isNameStart(ch))
			{
				x.state = ELEM0;
				return elemstart(x, ch);
			}
			break;
		case LE2:
			if (ch == (char)'!')
			{
				x.state = LEE2;
				return OK;
			}
			if (ch == (char)'?')
			{
				x.state = PI0;
				x.nextstate = MISC2;
				return OK;
			}
			if (ch == (char)'/')
			{
				x.state = ETAG0;
				return OK;
			}
			if (isNameStart(ch))
			{
				x.state = ELEM0;
				return elemstart(x, ch);
			}
			break;
		case LE3:
			if (ch == (char)'!')
			{
				x.state = COMMENT0;
				x.nextstate = MISC3;
				return OK;
			}
			if (ch == (char)'?')
			{
				x.state = PI0;
				x.nextstate = MISC3;
				return OK;
			}
			break;
		case LEE1:
			if (ch == (char)'-')
			{
				x.state = COMMENT1;
				x.nextstate = MISC1;
				return OK;
			}
			if (ch == (char)'D')
			{
				x.state = STRING;
				x.nextstate = DT0;
				x.string = (char*)"OCTYPE";
				return OK;
			}
			break;
		case LEE2:
			if (ch == (char)'-')
			{
				x.state = COMMENT1;
				x.nextstate = MISC2;
				return OK;
			}
			if (ch == (char)'[')
			{
				x.state = STRING;
				x.nextstate = CD0;
				x.string = (char*)"CDATA[";
				return OK;
			}
			break;
		case LEQ0:
			if (ch == (char)'x')
			{
				x.state = XMLDECL0;
				x.nextstate = MISC1;
				return pistart(x, ch);
			}
			if (isNameStart(ch))
			{
				x.state = PI1;
				x.nextstate = MISC1;
				return pistart(x, ch);
			}
			break;
		case MISC0:
			if (isSP(ch)) return OK;
			if (ch == (char)'<')
			{
				x.state = LE0;
				return OK;
			}
			break;
		case MISC1:
			if (isSP(ch)) return OK;
			if (ch == (char)'<')
			{
				x.state = LE1;
				return OK;
			}
			break;
		case MISC2:
			if (ch == (char)'<')
			{
				x.state = LE2;
				return OK;
			}
			if (ch == (char)'&')
			{
				x.state = MISC2A;
				return refstart(x, ch);
			}
			if (isChar(ch)) return datacontent(x, ch);
			break;
		case MISC2A:
			if (isRef(ch)) return ref(x, ch);
			if (ch == (char)'\x3b')
			{
				x.state = MISC2;
				return refcontent(x, ch);
			}
			break;
		case MISC3:
			if (isSP(ch)) return OK;
			if (ch == (char)'<')
			{
				x.state = LE3;
				return OK;
			}
			break;
		case PI0:
			if (isNameStart(ch))
			{
				x.state = PI1;
				return pistart(x, ch);
			}
			break;
		case PI1:
			if (isName(ch)) return piname(x, ch);
			if (ch == (char)'?')
			{
				x.state = PI4;
				return pinameend(x, ch);
			}
			if (isSP(ch))
			{
				x.state = PI2;
				return pinameend(x, ch);
			}
			break;
		case PI2:
			if (ch == (char)'?')
			{
				x.state = PI3;
				return OK;
			}
			if (isChar(ch)) return datapi1(x, ch);
			break;
		case PI3:
			if (ch == (char)'>')
			{
				x.state = x.nextstate;
				return pivalend(x, ch);
			}
			if (isChar(ch))
			{
				x.state = PI2;
				return datapi2(x, ch);
			}
			break;
		case PI4:
			if (ch == (char)'>')
			{
				x.state = x.nextstate;
				return pivalend(x, ch);
			}
			break;
		case STD0:
			if (isSP(ch)) return OK;
			if (ch == (char)'=')
			{
				x.state = STD1;
				return OK;
			}
			break;
		case STD1:
			if (isSP(ch)) return OK;
			if (ch == (char)'\'' || ch == (char)'"')
			{
				x.state = STD2;
				x.quote = ch;
				return OK;
			}
			break;
		case STD2:
			if (ch == (char)'y')
			{
				x.state = STRING;
				x.nextstate = STD3;
				x.string = (char*)"es";
				return OK;
			}
			if (ch == (char)'n')
			{
				x.state = STRING;
				x.nextstate = STD3;
				x.string = (char*)"o";
				return OK;
			}
			break;
		case STD3:
			if (x.quote == ch)
			{
				x.state = XMLDECL8;
				return OK;
			}
			break;
		case VER0:
			if (isSP(ch)) return OK;
			if (ch == (char)'=')
			{
				x.state = VER1;
				return OK;
			}
			break;
		case VER1:
			if (isSP(ch)) return OK;
			if (ch == (char)'\'' || ch == (char)'"')
			{
				x.state = STRING;
				x.quote = ch;
				x.nextstate = VER2;
				x.string = (char*)"1.";
				return OK;
			}
			break;
		case VER2:
			if (isNum(ch))
			{
				x.state = VER3;
				return OK;
			}
			break;
		case VER3:
			if (isNum(ch)) return OK;
			if (x.quote == ch)
			{
				x.state = XMLDECL4;
				return OK;
			}
			break;
		case XMLDECL0:
			if (ch == (char)'m')
			{
				x.state = XMLDECL1;
				return piname(x, ch);
			}
			if (isName(ch))
			{
				x.state = PI1;
				return piname(x, ch);
			}
			if (ch == (char)'?')
			{
				x.state = PI4;
				return pinameend(x, ch);
			}
			if (isSP(ch))
			{
				x.state = PI2;
				return pinameend(x, ch);
			}
			break;
		case XMLDECL1:
			if (ch == (char)'l')
			{
				x.state = XMLDECL2;
				return piname(x, ch);
			}
			if (isName(ch))
			{
				x.state = PI1;
				return piname(x, ch);
			}
			if (ch == (char)'?')
			{
				x.state = PI4;
				return pinameend(x, ch);
			}
			if (isSP(ch))
			{
				x.state = PI2;
				return pinameend(x, ch);
			}
			break;
		case XMLDECL2:
			if (isSP(ch))
			{
				x.state = XMLDECL3;
				return piabort(x, ch);
			}
			if (isName(ch))
			{
				x.state = PI1;
				return piname(x, ch);
			}
			break;
		case XMLDECL3:
			if (isSP(ch)) return OK;
			if (ch == (char)'v')
			{
				x.state = STRING;
				x.nextstate = VER0;
				x.string = (char*)"ersion";
				return OK;
			}
			break;
		case XMLDECL4:
			if (isSP(ch))
			{
				x.state = XMLDECL5;
				return OK;
			}
			if (ch == (char)'?')
			{
				x.state = XMLDECL9;
				return OK;
			}
			break;
		case XMLDECL5:
			if (isSP(ch)) return OK;
			if (ch == (char)'?')
			{
				x.state = XMLDECL9;
				return OK;
			}
			if (ch == (char)'e')
			{
				x.state = STRING;
				x.nextstate = ENC0;
				x.string = (char*)"ncoding";
				return OK;
			}
			if (ch == (char)'s')
			{
				x.state = STRING;
				x.nextstate = STD0;
				x.string = (char*)"tandalone";
				return OK;
			}
			break;
		case XMLDECL6:
			if (isSP(ch))
			{
				x.state = XMLDECL7;
				return OK;
			}
			if (ch == (char)'?')
			{
				x.state = XMLDECL9;
				return OK;
			}
			break;
		case XMLDECL7:
			if (isSP(ch)) return OK;
			if (ch == (char)'?')
			{
				x.state = XMLDECL9;
				return OK;
			}
			if (ch == (char)'s')
			{
				x.state = STRING;
				x.nextstate = STD0;
				x.string = (char*)"tandalone";
				return OK;
			}
			break;
		case XMLDECL8:
			if (isSP(ch)) return OK;
			if (ch == (char)'?')
			{
				x.state = XMLDECL9;
				return OK;
			}
			break;
		case XMLDECL9:
			if (ch == (char)'>')
			{
				x.state = MISC1;
				return OK;
			}
			break;
	}
	return ESYN;

}

/**
 * Returns the length of the element name (x->elem), attribute name (x->attr),
 * or PI name (x->pi). This function should ONLY be used directly after the
 * Ret.ELEMSTART, Ret.ATTRSTART or Ret.PISTART (respectively) tokens have
 * been returned by parse(), calling this at any other time may not give
 * the correct results. This function should also NOT be used on strings other
 * than x->elem, x->attr or x->pi.
 */
fn usz symlen(Parser* x, char* s) => (usz)((x.stack + x.stacklen) - s);

/**
 * May be called after the last character has been given to parse().
 * Returns Ret.OK if the XML document is valid, Ret.EEOF otherwise. Using
 * this function isn't really necessary, but can be used to detect documents
 * that don't end correctly. In particular, an error is returned when the XML
 * document did not contain a (complete) root element, or when the document
 * ended while in a comment or processing instruction.
 */
fn Ret eof(Parser* x)
{
	if (x.state != MISC3) return EEOF;
	return OK;
}
