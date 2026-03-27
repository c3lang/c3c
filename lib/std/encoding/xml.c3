module std::encoding::xml;
import std::io, std::io::file, std::collections::map, std::collections::list;

faultdef UNEXPECTED_CHARACTER, UNEXPECTED_EOF, INVALID_TAG, MISMATCHED_TAG, INVALID_ATTRIBUTE, MAX_DEPTH_REACHED;

enum XmlNodeType
{
    ELEMENT,
    TEXT,
    CDATA,
    COMMENT,
    PROCESSING_INSTRUCTION,
}

// Called when the parser needs external entity content (SYSTEM/PUBLIC reference).
// data:      opaque pointer from XmlParseOptions.entity_resolver_data
// system_id: the SYSTEM identifier string (e.g. "097.ent", "http://…")
// public_id: the PUBLIC identifier string (empty if none)
// Returns the entity content (may be tmem-allocated — valid for the parse duration),
// or a fault to indicate the entity is unavailable (parser silently skips it).
alias EntityResolver = fn String?(void* data, String system_id, String public_id);

// Convenience struct for file-based entity resolution.
// Pass a pointer to this as XmlParseOptions.entity_resolver_data, and use
// xml::resolve_file_entity as the resolver function.
struct XmlFileEntityResolver
{
    String base_dir; // directory containing the XML file
}

// File-based entity resolver: resolves SYSTEM identifiers relative to base_dir.
// Use this as XmlParseOptions.entity_resolver, passing &resolver as entity_resolver_data.
fn String? resolve_file_entity(void* data, String system_id, String public_id)
{
    XmlFileEntityResolver* res = (XmlFileEntityResolver*)data;
    DString path_buf = dstring::new_with_capacity(tmem, res.base_dir.len + system_id.len + 2);
    if (res.base_dir.len > 0)
    {
        path_buf.append(res.base_dir);
        if (res.base_dir[res.base_dir.len - 1] != '/') path_buf.append('/');
    }
    path_buf.append(system_id);
    char[]? raw = file::load(tmem, path_buf.str_view());
    if (catch err = raw) return err~;

    String content;
    char[] bytes = raw;
    if (bytes.len >= 2 &&
        ((bytes[0] == (char)0xFF && bytes[1] == (char)0xFE) ||
         (bytes[0] == (char)0xFE && bytes[1] == (char)0xFF)))
    {
        DString utf8 = dstring::new_with_capacity(tmem, bytes.len);
        transcode_utf16(&utf8, bytes)!;
        content = utf8.str_view();
    }
    else
    {
        content = (String)bytes;
    }

    // Strip leading text declaration <?xml ...?> (§4.3.1 — encoding info only, not content).
    if (content.starts_with("<?xml"))
    {
        usz i = 5;
        while (i + 1 < content.len)
        {
            if (content[i] == '?' && content[i + 1] == '>') { i += 2; break; }
            i++;
        }
        content = i <= content.len ? content[i..] : "";
    }

    return content;
}

struct XmlParseOptions
{
    bool           keep_comments;
    bool           keep_processing_instructions;
    int            max_depth;          // 0 = use default (128)
    EntityResolver entity_resolver;    // null = no external entity support
    void*          entity_resolver_data; // passed as first arg to entity_resolver
}

struct XmlEncodeOptions
{
    String version; // XML version string; defaults to "1.0" if empty
    String indent;  // per-level indent string, e.g. "  " or "\t"; empty = compact (default)
}

struct XmlDoctype
{
    String name;
    String public_id;
    String system_id;
}

struct XmlNode
{
    XmlNodeType type;
    Allocator allocator;
    String name;      // ELEMENT: tag name; PROCESSING_INSTRUCTION: target; others: (empty, not heap-allocated)
    String content;   // TEXT/CDATA/COMMENT: text; PROCESSING_INSTRUCTION: data; ELEMENT: (empty, not heap-allocated)
    XmlAttrMap attrs;
    XmlNodeList children;
}

struct XmlDoc
{
    Allocator allocator;
    XmlNode* root;
    XmlNodeList prologue;        // PIs and comments before root, in document order
    XmlNodeList epilogue;        // PIs and comments after root, in document order
    XmlNode*   xml_declaration;  // <?xml ...?> node if present, null otherwise
    XmlDoctype* doctype;         // DOCTYPE if present, null otherwise
}

fn void? transcode_utf16(DString* out, char[] bytes) @local
{
    if (bytes.len < 2) return xml::UNEXPECTED_EOF~;
    bool le;
    if      (bytes[0] == (char)0xFF && bytes[1] == (char)0xFE) { le = true; }
    else if (bytes[0] == (char)0xFE && bytes[1] == (char)0xFF) { le = false; }
    else    { return xml::UNEXPECTED_CHARACTER~; }

    usz i = 2;
    while (i + 1 < bytes.len)
    {
        uint unit = le
            ? ((uint)bytes[i] | ((uint)bytes[i + 1] << 8))
            : (((uint)bytes[i] << 8) | (uint)bytes[i + 1]);
        i += 2;

        uint cp;
        if (unit >= 0xD800 && unit <= 0xDBFF)
        {
            if (i + 1 < bytes.len)
            {
                uint low = le
                    ? ((uint)bytes[i] | ((uint)bytes[i + 1] << 8))
                    : (((uint)bytes[i] << 8) | (uint)bytes[i + 1]);
                if (low >= 0xDC00 && low <= 0xDFFF)
                {
                    cp = 0x10000 + ((unit - 0xD800) << 10) + (low - 0xDC00);
                    i += 2;
                }
                else
                {
                    cp = unit; // lone high surrogate
                }
            }
            else
            {
                cp = unit; // truncated surrogate pair
            }
        }
        else
        {
            cp = unit;
        }
        out.append_char32((Char32)cp);
    }
}

fn XmlDoc? parse_string(Allocator allocator, String s, XmlParseOptions opts = {})
{
    char[] bytes = (char[])s;
    if (bytes.len >= 2 &&
        ((bytes[0] == (char)0xFF && bytes[1] == (char)0xFE) ||
         (bytes[0] == (char)0xFE && bytes[1] == (char)0xFF)))
    {
        @pool()
        {
            DString utf8 = dstring::new_with_capacity(tmem, bytes.len);
            transcode_utf16(&utf8, bytes)!;
            ByteReader reader;
            return parse(allocator, reader.init(utf8.str_view()), opts)!;
        };
    }
    ByteReader reader;
    return parse(allocator, reader.init(s), opts);
}

fn XmlDoc? tparse_string(String s, XmlParseOptions opts = {}) => parse_string(tmem, s, opts) @inline;

fn XmlDoc? parse(Allocator allocator, InStream s, XmlParseOptions opts = {})
{
    @stack_mem(512; Allocator smem)
    {
        XmlContext ctx = {
            .scratch = dstring::new_with_capacity(smem, 64),
            .stream = s,
            .allocator = allocator,
            .keep_comments = opts.keep_comments,
            .keep_processing_instructions = opts.keep_processing_instructions,
            .max_depth = opts.max_depth > 0 ? opts.max_depth : 128,
            .entity_resolver = opts.entity_resolver,
            .entity_resolver_data = opts.entity_resolver_data,
        };
        @pool()
        {
            return parse_document(&ctx)!;
        };
    };
}

fn XmlDoc? tparse(InStream s, XmlParseOptions opts = {}) => parse(tmem, s, opts) @inline;

fn void XmlDoc.free(&self)
{
    if (self.root) self.root.free();
    foreach (node : self.prologue) node.free();
    self.prologue.free();
    foreach (node : self.epilogue) node.free();
    self.epilogue.free();
    if (self.xml_declaration) self.xml_declaration.free();
    if (self.doctype)
    {
        if (self.doctype.name.ptr)      self.doctype.name.free(self.allocator);
        if (self.doctype.public_id.ptr) self.doctype.public_id.free(self.allocator);
        if (self.doctype.system_id.ptr) self.doctype.system_id.free(self.allocator);
        alloc::free(self.allocator, self.doctype);
    }
}

fn String XmlDoc.encode(&self, Allocator allocator, XmlEncodeOptions opts = {})
{
    ByteWriter bw;
    bw.tinit();
    bool pretty = opts.indent.len > 0;
    if (opts.version.len > 0)
    {
        io::fprintf(&bw, `<?xml version="%s" encoding="UTF-8"?>`, opts.version)!!;
    }
    else if (self.xml_declaration)
    {
        write_node(&bw, self.xml_declaration, opts.indent, 0)!!;
    }
    if (self.doctype)
    {
        if (pretty && bw.str_view().len > 0) bw.write_byte('\n')!!;
        io::fprintf(&bw, "<!DOCTYPE %s", self.doctype.name)!!;
        if (self.doctype.public_id.len > 0)
        {
            io::fprintf(&bw, ` PUBLIC "%s" "%s"`, self.doctype.public_id, self.doctype.system_id)!!;
        }
        else if (self.doctype.system_id.len > 0)
        {
            io::fprintf(&bw, ` SYSTEM "%s"`, self.doctype.system_id)!!;
        }
        bw.write(">")!!;
    }
    foreach (node : self.prologue)
    {
        if (pretty && bw.str_view().len > 0) bw.write_byte('\n')!!;
        write_node(&bw, node, opts.indent, 0)!!;
    }
    if (self.root)
    {
        if (pretty && bw.str_view().len > 0) bw.write_byte('\n')!!;
        write_node(&bw, self.root, opts.indent, 0)!!;
    }
    foreach (node : self.epilogue)
    {
        if (pretty && bw.str_view().len > 0) bw.write_byte('\n')!!;
        write_node(&bw, node, opts.indent, 0)!!;
    }
    if (pretty && bw.str_view().len > 0) bw.write_byte('\n')!!;
    return bw.str_view().copy(allocator);
}

fn String XmlDoc.tencode(&self, XmlEncodeOptions opts = {}) => self.encode(tmem, opts) @inline;

fn void XmlNode.free(&self)
{
    if (self.name.ptr) self.name.free(self.allocator);
    if (self.content.ptr) self.content.free(self.allocator);
    if (self.type == ELEMENT)
    {
        // HashMap copies keys (COPY_KEYS=true), so only free values here.
        self.attrs.@each(; String k, String v)
        {
            v.free(self.allocator);
        };
        self.attrs.free();
        foreach (child : self.children) child.free();
        self.children.free();
    }
    alloc::free(self.allocator, self);
}

<*
 @return "the attribute value, or NOT_FOUND if absent or node is not an element"
*>
fn String? XmlNode.get_attr(&self, String name)
{
    if (self.type != ELEMENT) return NOT_FOUND~;
    return self.attrs.get(name);
}

fn bool XmlNode.has_attr(&self, String name) => self.type == ELEMENT && self.attrs.has_key(name);

<*
 @return "the first child element with the given tag name, or NOT_FOUND"
*>
fn XmlNode*? XmlNode.child(&self, String tag)
{
    if (self.type != ELEMENT) return NOT_FOUND~;
    foreach (child : self.children)
    {
        if (child.type == ELEMENT && child.name == tag) return child;
    }
    return NOT_FOUND~;
}

<*
 Returns a temp-allocated slice of all child elements with the given tag name.
*>
fn XmlNode*[] XmlNode.children_named(&self, String tag)
{
    if (self.type != ELEMENT) return {};
    XmlNodeList result;
    result.tinit();
    foreach (child : self.children)
    {
        if (child.type == ELEMENT && child.name == tag) result.push(child);
    }
    return result.array_view();
}

<*
 Returns a temp-allocated string with the concatenated text content of all
 immediate TEXT and CDATA children. For non-element nodes, returns the node's
 own content.
*>
fn String XmlNode.text_content(&self)
{
    if (self.type != ELEMENT) return self.content;
    // Fast path: single text/CDATA child avoids a DString allocation.
    if (self.children.len() == 1)
    {
        XmlNode* only = self.children.get(0);
        if (only.type == TEXT || only.type == CDATA) return only.content;
    }
    DString buf = dstring::new(tmem);
    foreach (child : self.children)
    {
        if (child.type == TEXT || child.type == CDATA) buf.append(child.content);
    }
    return buf.str_view();
}

<*
 Returns the number of children. Always 0 for non-element nodes.
*>
fn usz XmlNode.len(&self) @operator(len) @inline => self.children.len();

<*
 @require self.type == ELEMENT : "Node must be an element to index children"
 @require index < self.children.len() : "Index out of bounds"
*>
fn XmlNode* XmlNode.get(&self, usz index) @operator([]) @inline => self.children.get(index);

<*
 @require self.type == ELEMENT : "Node must be an element to index children"
 @require index < self.children.len() : "Index out of bounds"
*>
fn XmlNode** XmlNode.get_ref(&self, usz index) @operator(&[]) @inline => self.children.get_ref(index);

alias XmlAttrMap  = HashMap{String, String};
alias XmlNodeList = List{XmlNode*};

// Represents one attribute default from an <!ATTLIST> declaration.
struct DtdDefault @local
{
    String element;    // element name  (tmem-allocated during parse)
    String attr;       // attribute name (tmem-allocated during parse)
    String value;      // default value  (tmem-allocated during parse; "" if no default)
    bool nmtokens;     // type is NMTOKEN or NMTOKENS — value needs collapse+trim normalization
    bool has_default;  // false for #IMPLIED / #REQUIRED
}
alias DtdDefaultList = List{DtdDefault};

struct XmlContext @local
{
    uint line;
    InStream stream;
    Allocator allocator;
    DString scratch;
    char current;
    int depth;
    int max_depth;
    bitstruct : char
    {
        bool reached_end;
        bool pushed_back;
        bool keep_comments;
        bool keep_processing_instructions;
        bool in_attr_value; // true while parsing inside an attribute value
    }
    // tmem-allocated; valid for the duration of the @pool() parse scope.
    HashMap{String, String} entities;
    DtdDefaultList          dtd_defaults;
    HashMap{String, String} param_entities;
    EntityResolver entity_resolver;
    void*          entity_resolver_data;
}

fn char? read_next(XmlContext* ctx) @local
{
    if (ctx.reached_end) return '\0';
    if (ctx.pushed_back)
    {
        ctx.pushed_back = false;
        return ctx.current;
    }
    char? c = ctx.stream.read_byte();
    if (catch err = c)
    {
        if (err == io::EOF)
        {
            ctx.reached_end = true;
            return '\0';
        }
        return err~;
    }
    // NUL is not a stream terminator in XML input; reject it explicitly.
    if (c == '\0') return UNEXPECTED_CHARACTER~;
    // §2.11: normalize CR and CR+LF to LF.
    if (c == '\r')
    {
        if (try nc = ctx.stream.read_byte())
        {
            if (nc != '\n') { ctx.pushed_back = true; ctx.current = nc; }
        }
        c = '\n';
    }
    return c;
}

<*
 @require !ctx.pushed_back : "Cannot push back twice in a row"
*>
fn void pushback(XmlContext* ctx, char c) @local @inline
{
    if (!ctx.reached_end)
    {
        ctx.pushed_back = true;
        ctx.current = c;
    }
}

fn void? skip_whitespace(XmlContext* ctx) @local
{
    while (true)
    {
        char c = read_next(ctx)!;
        switch (c)
        {
            case '\n': ctx.line++; nextcase;
            case ' ': case '\t': case '\r': continue;
            default:
                pushback(ctx, c);
                return;
        }
    }
}

fn void? match_str(XmlContext* ctx, String expected) @local
{
    foreach (e : expected)
    {
        char c = read_next(ctx)!;
        if (c != e) return UNEXPECTED_CHARACTER~;
    }
}

<* @pure *>
fn bool is_name_start(char c) @local @inline => c.is_alpha() || c == '_' || c == ':' || c >= 0x80;
<* @pure *>
fn bool is_name_char(char c) @local @inline => c.is_alnum() || c == '_' || c == ':' || c == '-' || c == '.' || c >= 0x80;

fn void? read_name_scratch(XmlContext* ctx) @local
{
    ctx.scratch.clear();
    char c = read_next(ctx)!;
    if (!is_name_start(c)) return INVALID_TAG~;
    ctx.scratch.append(c);
    while (true)
    {
        c = read_next(ctx)!;
        if (!is_name_char(c)) { pushback(ctx, c); break; }
        ctx.scratch.append(c);
    }
}

fn String? read_name(XmlContext* ctx) @local
{
    read_name_scratch(ctx)!;
    return ctx.scratch.str_view().copy(ctx.allocator);
}

// §2.11: normalize CR and CRLF to LF in external entity content before processing.
fn String normalize_cr(Allocator alloc, String s) @local
{
    DString result = dstring::new_with_capacity(alloc, s.len);
    for (usz i = 0; i < s.len; i++)
    {
        char c = s[i];
        if (c == '\r')
        {
            if (i + 1 < s.len && s[i + 1] == '\n') i++;
            result.append('\n');
        }
        else
        {
            result.append(c);
        }
    }
    return result.str_view().copy(alloc);
}

// Re-expand a stored entity replacement value. Handles char refs and the five
// predefined named entities; unknown entities are passed through as-is.
fn void? expand_entity_value(XmlContext* ctx, String val) @local
{
    usz i = 0;
    while (i < val.len)
    {
        char c = val[i++];
        if (c != '&')
        {
            if (c == '\n') ctx.line++;
            // §3.3.3: literal whitespace in entity replacement text normalizes to
            // space in attribute value context (character references are exempt).
            if (ctx.in_attr_value && (c == '\t' || c == '\n' || c == '\r')) c = ' ';
            ctx.scratch.append(c);
            continue;
        }

        usz start = i;
        while (i < val.len && val[i] != ';') i++;
        if (i >= val.len)
        {
            // Malformed entity ref — pass through.
            ctx.scratch.append('&');
            ctx.scratch.append(val[start..]);
            break;
        }
        String ref = i > start ? val[start..(i - 1)] : "";
        i++;

        if (ref.len == 0) { ctx.scratch.append('&'); ctx.scratch.append(';'); continue; }

        if (ref[0] == '#')
        {
            uint codepoint = 0;
            if (ref.len > 1 && ref[1] == 'x')
            {
                for (usz j = 2; j < ref.len; j++)
                {
                    char hc = ref[j];
                    if (hc >= '0' && hc <= '9')           { codepoint = codepoint * 16 + (hc - '0'); }
                    else if (hc >= 'a' && hc <= 'f')      { codepoint = codepoint * 16 + (hc - 'a' + 10); }
                    else if (hc >= 'A' && hc <= 'F')      { codepoint = codepoint * 16 + (hc - 'A' + 10); }
                }
            }
            else
            {
                for (usz j = 1; j < ref.len; j++)
                {
                    char dc = ref[j];
                    if (dc >= '0' && dc <= '9') codepoint = codepoint * 10 + (dc - '0');
                }
            }
            // §2.11: CR (U+000D) in entity replacement text normalizes to LF.
            if (codepoint == 0x0D) codepoint = 0x0A;
            // §3.3.3: whitespace chars from entity replacement text normalize to
            // space in attribute-value context (char refs directly in the attr
            // value bypass this, but char refs inside entity values do not).
            if (ctx.in_attr_value && (codepoint == 0x09 || codepoint == 0x0A))
            {
                ctx.scratch.append(' ');
            }
            else if (codepoint <= 0x10ffff)
            {
                ctx.scratch.append_char32((Char32)codepoint);
            }
            continue;
        }

        switch (ref)
        {
            case "lt":   ctx.scratch.append('<');  continue;
            case "gt":   ctx.scratch.append('>');  continue;
            case "amp":  ctx.scratch.append('&');  continue;
            case "apos": ctx.scratch.append('\''); continue;
            case "quot": ctx.scratch.append('"');  continue;
        }
        String? nested = ctx.entities.get(ref);
        if (try nval = nested) { expand_entity_value(ctx, nval)!; continue; }

        // Unknown entity — pass through.
        ctx.scratch.append('&');
        ctx.scratch.append(ref);
        ctx.scratch.append(';');
    }
}

// Returns true if the entity replacement text contains a '<' (i.e., markup).
fn bool entity_has_markup(String val) @local
{
    usz i = 0;
    while (i < val.len)
    {
        char c = val[i++];
        if (c == '<') return true;
        if (c != '&') continue;
        usz start = i;
        while (i < val.len && val[i] != ';') i++;
        if (i >= val.len) break;
        String ref = i > start ? val[start..(i - 1)] : "";
        i++;
        if (ref.len > 0 && ref[0] == '#')
        {
            uint cp = 0;
            if (ref.len > 1 && ref[1] == 'x')
            {
                for (usz j = 2; j < ref.len; j++)
                {
                    char hc = ref[j];
                    if (hc >= '0' && hc <= '9')      { cp = cp * 16 + (hc - '0'); }
                    else if (hc >= 'a' && hc <= 'f') { cp = cp * 16 + (hc - 'a' + 10); }
                    else if (hc >= 'A' && hc <= 'F') { cp = cp * 16 + (hc - 'A' + 10); }
                }
            }
            else
            {
                for (usz j = 1; j < ref.len; j++)
                {
                    char dc = ref[j];
                    if (dc >= '0' && dc <= '9') { cp = cp * 10 + (dc - '0'); }
                }
            }
            if (cp == 0x3C) return true; // '<' = U+003C
        }
    }
    return false;
}

// Expand only char refs (&#dec;/&#xhex;) in val into ctx.scratch; named entity
// refs are kept verbatim (§4.4.7 bypass) so the sub-parser can expand them in context.
fn void? expand_char_refs_only(XmlContext* ctx, String val) @local
{
    usz i = 0;
    while (i < val.len)
    {
        char c = val[i++];
        if (c != '&') { if (c == '\n') ctx.line++; ctx.scratch.append(c); continue; }
        usz start = i;
        while (i < val.len && val[i] != ';') i++;
        if (i >= val.len)
        {
            ctx.scratch.append('&');
            ctx.scratch.append(val[start..]);
            break;
        }
        String ref = i > start ? val[start..(i - 1)] : "";
        i++;
        if (ref.len > 0 && ref[0] == '#')
        {
            uint codepoint = 0;
            if (ref.len > 1 && ref[1] == 'x')
            {
                for (usz j = 2; j < ref.len; j++)
                {
                    char hc = ref[j];
                    if      (hc >= '0' && hc <= '9') { codepoint = codepoint * 16 + (hc - '0'); }
                    else if (hc >= 'a' && hc <= 'f') { codepoint = codepoint * 16 + (hc - 'a' + 10); }
                    else if (hc >= 'A' && hc <= 'F') { codepoint = codepoint * 16 + (hc - 'A' + 10); }
                }
            }
            else
            {
                for (usz j = 1; j < ref.len; j++)
                {
                    char dc = ref[j];
                    if (dc >= '0' && dc <= '9') codepoint = codepoint * 10 + (dc - '0');
                }
            }
            ctx.scratch.append_char32((Char32)codepoint);
        }
        else
        {
            // Named entity ref: keep verbatim (§4.4.7 bypass).
            ctx.scratch.append('&');
            ctx.scratch.append(ref);
            ctx.scratch.append(';');
        }
    }
}

// Expand entity_val as XML markup, flushing any pending text, then add resulting nodes to parent.
fn void? parse_entity_markup(XmlContext* ctx, XmlNode* parent, String entity_val) @local
{
    String pending = ctx.scratch.str_view();
    if (pending.len > 0)
    {
        parent.children.push(new_leaf(ctx, TEXT, pending.copy(ctx.allocator)));
        ctx.scratch.clear();
    }

    // §4.4.4: pre-expand char refs only; named entity refs are bypassed (§4.4.7)
    // and left as &name; tokens for the sub-parser to expand in context.
    DString saved = ctx.scratch;
    ctx.scratch = dstring::new_with_capacity(tmem, entity_val.len + 4);
    expand_char_refs_only(ctx, entity_val)!;
    String markup = ctx.scratch.str_view().copy(tmem);
    ctx.scratch = saved;

    ByteReader subreader;
    XmlContext sub = {
        .scratch      = dstring::new_with_capacity(tmem, 64),
        .stream       = subreader.init(markup),
        .allocator    = ctx.allocator,
        .keep_comments                = ctx.keep_comments,
        .keep_processing_instructions = ctx.keep_processing_instructions,
        .max_depth    = ctx.max_depth,
        .depth        = ctx.depth,
        .entities     = ctx.entities,
        .dtd_defaults = ctx.dtd_defaults,
    };

    while (true)
    {
        bool hit_lt = read_text_to(&sub, parent)!;
        if (!hit_lt) break;

        char c = read_next(&sub)!;
        if (c == '\0') break;

        if (is_name_start(c))
        {
            pushback(&sub, c);
            XmlNode* child = parse_element(&sub)!;
            parent.children.push(child);
            continue;
        }
        if (c == '!')
        {
            char c2 = read_next(&sub)!;
            if (c2 == '[')
            {
                match_str(&sub, "CDATA[")!;
                XmlNode* cdata = parse_cdata(&sub)!;
                parent.children.push(cdata);
                continue;
            }
            if (c2 == '-')
            {
                if (read_next(&sub)! != '-') break;
                XmlNode* comment = parse_comment(&sub)!;
                if (sub.keep_comments) { parent.children.push(comment); } else { comment.free(); }
                continue;
            }
        }
        if (c == '?')
        {
            XmlNode* pi = parse_pi(&sub)!;
            if (sub.keep_processing_instructions) { parent.children.push(pi); } else { pi.free(); }
            continue;
        }
        break; // unexpected
    }

    String tail = sub.scratch.str_view();
    if (tail.len > 0) parent.children.push(new_leaf(ctx, TEXT, tail.copy(ctx.allocator)));
}

// '&' already consumed. When parent is non-null, entities with markup replacement
// text are parsed via parse_entity_markup and their nodes inserted into parent.
fn void? append_entity(XmlContext* ctx, XmlNode* parent = null) @local
{
    DString entity_buf = dstring::new(tmem);
    while (true)
    {
        char c = read_next(ctx)!;
        if (c == ';') break;
        if (c == '\0') return UNEXPECTED_EOF~;
        entity_buf.append(c);
    }
    String name = entity_buf.str_view();
    switch (name)
    {
        case "lt":   ctx.scratch.append('<');  return;
        case "gt":   ctx.scratch.append('>');  return;
        case "amp":  ctx.scratch.append('&');  return;
        case "apos": ctx.scratch.append('\''); return;
        case "quot": ctx.scratch.append('"');  return;
    }
    if (name.len > 0 && name[0] == '#')
    {
        uint codepoint = 0;
        if (name.len > 1 && name[1] == 'x')
        {
            for (usz i = 2; i < name.len; i++)
            {
                char hc = name[i];
                if (hc >= '0' && hc <= '9')
                {
                    codepoint = codepoint * 16 + (hc - '0');
                }
                else if (hc >= 'a' && hc <= 'f')
                {
                    codepoint = codepoint * 16 + (hc - 'a' + 10);
                }
                else if (hc >= 'A' && hc <= 'F')
                {
                    codepoint = codepoint * 16 + (hc - 'A' + 10);
                }
                else
                {
                    return UNEXPECTED_CHARACTER~;
                }
            }
        }
        else
        {
            for (usz i = 1; i < name.len; i++)
            {
                char dc = name[i];
                if (dc < '0' || dc > '9') { return UNEXPECTED_CHARACTER~; }
                codepoint = codepoint * 10 + (dc - '0');
            }
        }
        if (codepoint > 0x10ffff) return UNEXPECTED_CHARACTER~;
        ctx.scratch.append_char32((Char32)codepoint);
        return;
    }
    String? mapped = ctx.entities.get(name);
    if (try val = mapped)
    {
        if (parent != null && entity_has_markup(val))
        {
            parse_entity_markup(ctx, parent, val)!;
            return;
        }
        expand_entity_value(ctx, val)!;
        return;
    }
    ctx.scratch.append('&');
    ctx.scratch.append(name);
    ctx.scratch.append(';');
}

<*
 @require quote == '"' || quote == '\'' : "Quote must be ' or \""
*>
fn String? read_attr_value(XmlContext* ctx, char quote) @local
{
    ctx.scratch.clear();
    ctx.in_attr_value = true;
    defer ctx.in_attr_value = false;
    while (true)
    {
        char c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == quote) break;
        if (c == '&') { append_entity(ctx)!; continue; }
        // §3.3.3: literal whitespace normalizes to space (char refs via append_entity are exempt).
        if (c == '\n') { ctx.line++; ctx.scratch.append(' '); continue; }
        if (c == '\t') { ctx.scratch.append(' '); continue; }
        ctx.scratch.append(c);
    }
    return ctx.scratch.str_view().copy(ctx.allocator);
}

fn XmlNode* new_element(XmlContext* ctx, String name) @local
{
    XmlNode* node = alloc::new(ctx.allocator, XmlNode);
    node.allocator = ctx.allocator;
    node.type = ELEMENT;
    node.name = name;
    node.attrs.init(ctx.allocator);
    node.children.init(ctx.allocator);
    return node;
}

fn XmlNode* new_leaf(XmlContext* ctx, XmlNodeType type, String content) @local
{
    XmlNode* node = alloc::new(ctx.allocator, XmlNode);
    node.allocator = ctx.allocator;
    node.type = type;
    node.content = content;
    return node;
}

// Entered after '<!--'; reads until '-->' and returns a COMMENT node.
fn XmlNode*? parse_comment(XmlContext* ctx) @local
{
    ctx.scratch.clear();
    while (true)
    {
        char c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == '\n') ctx.line++;
        if (c != '-') { ctx.scratch.append(c); continue; }
        char c2 = read_next(ctx)!;
        if (c2 == '\0') return UNEXPECTED_EOF~;
        if (c2 != '-') { ctx.scratch.append(c); pushback(ctx, c2); continue; }
        char c3 = read_next(ctx)!;
        if (c3 == '\0') return UNEXPECTED_EOF~;
        if (c3 == '>') break;
        // "--" not followed by ">" — lenient, keep the dashes and continue.
        ctx.scratch.append(c);
        ctx.scratch.append(c2);
        pushback(ctx, c3);
    }
    return new_leaf(ctx, COMMENT, ctx.scratch.str_view().copy(ctx.allocator));
}

fn void? skip_comment(XmlContext* ctx) @local
{
    XmlNode* node = parse_comment(ctx)!;
    node.free();
}

// Entered after '<?'; reads target and data until '?>'.
fn XmlNode*? parse_pi(XmlContext* ctx) @local
{
    read_name_scratch(ctx)!;
    String target = ctx.scratch.str_view().copy(ctx.allocator);
    defer catch target.free(ctx.allocator);
    skip_whitespace(ctx)!;
    ctx.scratch.clear();
    while (true)
    {
        char c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == '\n') ctx.line++;
        if (c != '?') { ctx.scratch.append(c); continue; }
        char c2 = read_next(ctx)!;
        if (c2 == '\0') return UNEXPECTED_EOF~;
        if (c2 == '>') break;
        ctx.scratch.append(c);
        pushback(ctx, c2);
    }
    XmlNode* node = alloc::new(ctx.allocator, XmlNode);
    node.allocator = ctx.allocator;
    node.type = PROCESSING_INSTRUCTION;
    node.name = target;
    node.content = ctx.scratch.str_view().copy(ctx.allocator);
    return node;
}

fn void? skip_pi(XmlContext* ctx) @local
{
    XmlNode* node = parse_pi(ctx)!;
    node.free();
}

fn XmlDoctype*? parse_doctype(XmlContext* ctx) @local
{
    skip_whitespace(ctx)!;
    read_name_scratch(ctx)!;
    XmlDoctype* dt = alloc::new(ctx.allocator, XmlDoctype);
    dt.name = ctx.scratch.str_view().copy(ctx.allocator);
    defer catch
    {
        if (dt.name.ptr)      dt.name.free(ctx.allocator);
        if (dt.public_id.ptr) dt.public_id.free(ctx.allocator);
        if (dt.system_id.ptr) dt.system_id.free(ctx.allocator);
        alloc::free(ctx.allocator, dt);
    }

    skip_whitespace(ctx)!;
    char c = read_next(ctx)!;

    if (c == 'P' || c == 'S')
    {
        pushback(ctx, c);
        read_name_scratch(ctx)!;
        bool is_public = ctx.scratch.str_view() == "PUBLIC";
        if (!is_public && ctx.scratch.str_view() != "SYSTEM") return UNEXPECTED_CHARACTER~;

        if (is_public)
        {
            skip_whitespace(ctx)!;
            char q = read_next(ctx)!;
            if (q != '"' && q != '\'') return UNEXPECTED_CHARACTER~;
            dt.public_id = read_attr_value(ctx, q)!;
        }

        skip_whitespace(ctx)!;
        char q = read_next(ctx)!;
        if (q == '"' || q == '\'')
        {
            dt.system_id = read_attr_value(ctx, q)!;
        }
        else
        {
            pushback(ctx, q);
        }

        skip_whitespace(ctx)!;
        c = read_next(ctx)!;
    }

    // Internal subset parsed first: internal declarations take precedence (first-decl-wins).
    if (c == '[')
    {
        parse_dtd_subset(ctx)!;
        skip_whitespace(ctx)!;
        c = read_next(ctx)!;
    }

    if (ctx.entity_resolver != null && dt.system_id.len > 0)
    {
        String? ext = ctx.entity_resolver(ctx.entity_resolver_data, dt.system_id, dt.public_id);
        if (try content = ext) parse_external_dtd(ctx, content)!;
    }

    if (c != '>') return UNEXPECTED_CHARACTER~;
    return dt;
}

fn void? skip_to_gt(XmlContext* ctx) @local
{
    while (true)
    {
        char c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == '\n') ctx.line++;
        if (c == '"' || c == '\'')
        {
            char q = c;
            while (true)
            {
                char d = read_next(ctx)!;
                if (d == '\0') return UNEXPECTED_EOF~;
                if (d == '\n') ctx.line++;
                if (d == q) break;
            }
            continue;
        }
        if (c == '>') return;
    }
}

fn void? parse_dtd_entity(XmlContext* ctx) @local
{
    skip_whitespace(ctx)!;
    char c = read_next(ctx)!;
    if (c == '%')
    {
        skip_whitespace(ctx)!;
        read_name_scratch(ctx)!;
        String pe_name = ctx.scratch.str_view().copy(tmem);
        skip_whitespace(ctx)!;
        char pq = read_next(ctx)!;
        if (is_name_start(pq))
        {
            pushback(ctx, pq);
            read_name_scratch(ctx)!;
            bool is_pub = ctx.scratch.str_view() == "PUBLIC";
            String pub_id = "";
            String sys_id = "";
            if (is_pub)
            {
                skip_whitespace(ctx)!;
                char ppq = read_next(ctx)!;
                if (ppq == '"' || ppq == '\'')
                {
                    ctx.scratch.clear();
                    while (true)
                    {
                        char ch = read_next(ctx)!;
                        if (ch == ppq || ch == '\0') break;
                        ctx.scratch.append(ch);
                    }
                    pub_id = ctx.scratch.str_view().copy(tmem);
                }
            }
            skip_whitespace(ctx)!;
            char sq = read_next(ctx)!;
            if (sq == '"' || sq == '\'')
            {
                ctx.scratch.clear();
                while (true)
                {
                    char ch = read_next(ctx)!;
                    if (ch == sq || ch == '\0') break;
                    ctx.scratch.append(ch);
                }
                sys_id = ctx.scratch.str_view().copy(tmem);
            }
            if (ctx.entity_resolver != null && sys_id.len > 0 && !ctx.param_entities.has_key(pe_name))
            {
                String? resolved = ctx.entity_resolver(ctx.entity_resolver_data, sys_id, pub_id);
                if (try content = resolved) ctx.param_entities.set(pe_name, normalize_cr(tmem, content));
            }
        }
        else if (pq == '"' || pq == '\'')
        {
            // Internal PE: pre-expand char refs (§4.4.4) and nested PE refs (§4.4.8).
            ctx.scratch.clear();
            while (true)
            {
                char ch = read_next(ctx)!;
                if (ch == pq || ch == '\0') break;
                if (ch == '\n') ctx.line++;
                if (ch == '%')
                {
                    char[64] pbuf;
                    usz plen = 0;
                    while (true)
                    {
                        char d = read_next(ctx)!;
                        if (d == ';') break;
                        if (d == '\0') return UNEXPECTED_EOF~;
                        if (plen < 63) pbuf[plen++] = d;
                    }
                    String? nested = ctx.param_entities.get((String)pbuf[:plen]);
                    if (try nv = nested) ctx.scratch.append(nv);
                    continue;
                }
                if (ch == '&')
                {
                    // Expand char refs; bypass named entity refs (§4.4.7).
                    char[64] rbuf;
                    usz rlen = 0;
                    bool hit_semi = false;
                    while (true)
                    {
                        char d = read_next(ctx)!;
                        if (d == ';') { hit_semi = true; break; }
                        if (d == '\0' || d == pq) { pushback(ctx, d); break; }
                        if (rlen < 63) rbuf[rlen++] = d;
                    }
                    if (!hit_semi || rlen == 0 || rbuf[0] != '#')
                    {
                        ctx.scratch.append('&');
                        ctx.scratch.append((String)rbuf[:rlen]);
                        if (hit_semi) ctx.scratch.append(';');
                        continue;
                    }
                    uint cp = 0;
                    if (rlen > 1 && rbuf[1] == 'x')
                    {
                        for (usz j = 2; j < rlen; j++)
                        {
                            char hc = rbuf[j];
                            if      (hc >= '0' && hc <= '9') { cp = cp * 16 + (hc - '0'); }
                            else if (hc >= 'a' && hc <= 'f') { cp = cp * 16 + (hc - 'a' + 10); }
                            else if (hc >= 'A' && hc <= 'F') { cp = cp * 16 + (hc - 'A' + 10); }
                        }
                    }
                    else
                    {
                        for (usz j = 1; j < rlen; j++)
                        {
                            char dc = rbuf[j];
                            if (dc >= '0' && dc <= '9') cp = cp * 10 + (dc - '0');
                        }
                    }
                    ctx.scratch.append_char32((Char32)cp);
                    continue;
                }
                ctx.scratch.append(ch);
            }
            String pe_val = ctx.scratch.str_view().copy(tmem);
            if (!ctx.param_entities.has_key(pe_name)) ctx.param_entities.set(pe_name, pe_val);
        }
        skip_to_gt(ctx)!;
        return;
    }
    pushback(ctx, c);

    read_name_scratch(ctx)!;
    String name = ctx.scratch.str_view().copy(tmem);

    skip_whitespace(ctx)!;
    char q = read_next(ctx)!;
    if (q != '"' && q != '\'')
    {
        if (!is_name_start(q)) { skip_to_gt(ctx)!; return; }
        pushback(ctx, q);
        read_name_scratch(ctx)!;
        bool is_pub = ctx.scratch.str_view() == "PUBLIC";
        String pub_id = "";
        String sys_id = "";
        if (is_pub)
        {
            skip_whitespace(ctx)!;
            char ppq = read_next(ctx)!;
            if (ppq == '"' || ppq == '\'')
            {
                ctx.scratch.clear();
                while (true) { char ch = read_next(ctx)!; if (ch == ppq || ch == '\0') break; ctx.scratch.append(ch); }
                pub_id = ctx.scratch.str_view().copy(tmem);
            }
        }
        skip_whitespace(ctx)!;
        char sq = read_next(ctx)!;
        if (sq == '"' || sq == '\'')
        {
            ctx.scratch.clear();
            while (true) { char ch = read_next(ctx)!; if (ch == sq || ch == '\0') break; ctx.scratch.append(ch); }
            sys_id = ctx.scratch.str_view().copy(tmem);
        }
        if (ctx.entity_resolver != null && sys_id.len > 0 && !ctx.entities.has_key(name))
        {
            String? resolved = ctx.entity_resolver(ctx.entity_resolver_data, sys_id, pub_id);
            if (try content = resolved) ctx.entities.set(name, normalize_cr(tmem, content));
        }
        skip_to_gt(ctx)!;
        return;
    }
    // Internal entity: expand PE refs (§4.4.8) but not general entity refs.
    ctx.scratch.clear();
    while (true)
    {
        char ch = read_next(ctx)!;
        if (ch == '\0') return UNEXPECTED_EOF~;
        if (ch == q) break;
        if (ch == '\n') ctx.line++;
        if (ch == '%')
        {
            char[64] pbuf;
            usz plen = 0;
            while (true)
            {
                char d = read_next(ctx)!;
                if (d == ';') break;
                if (d == '\0') return UNEXPECTED_EOF~;
                if (plen < 63) pbuf[plen++] = d;
            }
            String? pe_val = ctx.param_entities.get((String)pbuf[:plen]);
            if (try pv = pe_val) ctx.scratch.append(pv);
            continue;
        }
        ctx.scratch.append(ch);
    }
    String value = ctx.scratch.str_view().copy(tmem);
    if (!ctx.entities.has_key(name)) ctx.entities.set(name, value); // first declaration wins (§4.2)
    skip_whitespace(ctx)!;
    if (read_next(ctx)! != '>') return UNEXPECTED_CHARACTER~;
}

fn void? parse_dtd_attlist(XmlContext* ctx) @local
{
    skip_whitespace(ctx)!;
    read_name_scratch(ctx)!;
    String elem = ctx.scratch.str_view().copy(tmem);

    while (true)
    {
        skip_whitespace(ctx)!;
        char c = read_next(ctx)!;
        if (c == '>') return;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (!is_name_start(c)) { pushback(ctx, c); skip_to_gt(ctx)!; return; }
        pushback(ctx, c);

        read_name_scratch(ctx)!;
        String attr = ctx.scratch.str_view().copy(tmem);

        skip_whitespace(ctx)!;
        char t = read_next(ctx)!;
        bool is_nmtokens = false;
        if (t == '(')
        {
            while (true)
            {
                char d = read_next(ctx)!;
                if (d == '\0') return UNEXPECTED_EOF~;
                if (d == ')') break;
            }
            is_nmtokens = true; // §3.3.3: enumerated types need whitespace normalization
        }
        else
        {
            pushback(ctx, t);
            read_name_scratch(ctx)!;
            String type_name = ctx.scratch.str_view();
            is_nmtokens = (type_name != "CDATA"); // §3.3.3: non-CDATA types need normalization
            // NOTATION is followed by an enumeration of notation names.
            if (type_name == "NOTATION")
            {
                skip_whitespace(ctx)!;
                char n = read_next(ctx)!;
                if (n == '(')
                {
                    while (true)
                    {
                        char d = read_next(ctx)!;
                        if (d == '\0') return UNEXPECTED_EOF~;
                        if (d == ')') break;
                    }
                }
                else
                {
                    pushback(ctx, n);
                }
            }
        }

        skip_whitespace(ctx)!;
        char d = read_next(ctx)!;
        if (d == '#')
        {
            read_name_scratch(ctx)!;
            if (ctx.scratch.str_view() != "FIXED")
            {
                // Push a sentinel so later redeclarations are ignored (first-decl-wins).
                // The nmtokens flag also enables value normalization for #REQUIRED/#IMPLIED.
                bool already = false;
                foreach (ex : ctx.dtd_defaults) { if (ex.element == elem && ex.attr == attr) { already = true; break; } }
                if (!already) ctx.dtd_defaults.push({ elem, attr, "", is_nmtokens, false });
                continue;
            }
            skip_whitespace(ctx)!;
            d = read_next(ctx)!;
        }
        if (d == '"' || d == '\'')
        {
            String? val = read_attr_value(ctx, d);
            if (catch err = val) { skip_to_gt(ctx)!; return; }
            bool already = false;
            foreach (existing : ctx.dtd_defaults) { if (existing.element == elem && existing.attr == attr) { already = true; break; } }
            if (!already) ctx.dtd_defaults.push({ elem, attr, val.copy(tmem), is_nmtokens, true });
            val.free(ctx.allocator); // read_attr_value uses ctx.allocator; swap to tmem
        }
    }
}

fn void? expand_param_entity_ref(XmlContext* ctx) @local
{
    char[64] buf;
    usz len = 0;
    while (true)
    {
        char c = read_next(ctx)!;
        if (c == ';') break;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (len < 63) buf[len++] = c;
    }
    String name = (String)buf[:len];
    String? content = ctx.param_entities.get(name);
    if (try val = content) parse_external_dtd(ctx, val)!;
}

fn void? parse_external_dtd(XmlContext* outer, String content) @local
{
    ByteReader subreader;
    XmlContext sub = {
        .scratch        = dstring::new_with_capacity(tmem, 64),
        .stream         = subreader.init(content),
        .allocator      = outer.allocator,
        .keep_comments  = outer.keep_comments,
        .keep_processing_instructions = outer.keep_processing_instructions,
        .max_depth      = outer.max_depth,
        .entities       = outer.entities,
        .dtd_defaults   = outer.dtd_defaults,
        .param_entities = outer.param_entities,
        .entity_resolver      = outer.entity_resolver,
        .entity_resolver_data = outer.entity_resolver_data,
    };
    parse_external_dtd_content(&sub)!;
    // Sync back: maps are value-copied but backing storage may have reallocated.
    outer.entities       = sub.entities;
    outer.dtd_defaults   = sub.dtd_defaults;
    outer.param_entities = sub.param_entities;
}

// Recursively expand PE refs in text into result; PE refs inside quoted strings are not expanded.
fn void expand_pe_in_text(XmlContext* ctx, String text, DString* result) @local
{
    usz i = 0;
    char in_q = '\0';
    while (i < text.len)
    {
        char c = text[i++];
        if (in_q != '\0')
        {
            result.append(c);
            if (c == in_q) in_q = '\0';
            continue;
        }
        if (c == '"' || c == '\'') { in_q = c; result.append(c); continue; }
        if (c == '%')
        {
            usz start = i;
            while (i < text.len && text[i] != ';') i++;
            if (i >= text.len) { result.append('%'); result.append(text[start..]); break; }
            String pe_name = start < i ? text[start..(i - 1)] : "";
            i++; // consume ';'
            String? pe_content = ctx.param_entities.get(pe_name);
            if (try val = pe_content)
            {
                result.append(' ');
                expand_pe_in_text(ctx, val, result); // recursive
                result.append(' ');
            }
            continue;
        }
        result.append(c);
    }
}

// Read declaration content up to '>' (inclusive), expanding PE refs outside quotes.
fn String? read_decl_content_expanded(XmlContext* ctx) @local
{
    DString result = dstring::new_with_capacity(tmem, 64);
    char in_q = '\0';
    while (true)
    {
        char c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == '\n') ctx.line++;
        if (in_q != '\0')
        {
            result.append(c);
            if (c == in_q) in_q = '\0';
            continue;
        }
        if (c == '"' || c == '\'') { in_q = c; result.append(c); continue; }
        if (c == '%')
        {
            char[64] buf;
            usz len = 0;
            while (true)
            {
                char d = read_next(ctx)!;
                if (d == ';') break;
                if (d == '\0') return UNEXPECTED_EOF~;
                if (len < 63) buf[len++] = d;
            }
            String? pe_val = ctx.param_entities.get((String)buf[:len]);
            if (try val = pe_val)
            {
                result.append(' ');
                expand_pe_in_text(ctx, val, &result);
                result.append(' ');
            }
            continue;
        }
        result.append(c);
        if (c == '>') break;
    }
    return result.str_view();
}

// Pre-expand PE refs in the ATTLIST declaration before passing to parse_dtd_attlist.
fn void? parse_dtd_attlist_expanded(XmlContext* outer) @local
{
    String? expanded = read_decl_content_expanded(outer);
    if (catch err = expanded) return err~;
    ByteReader sub_reader;
    XmlContext sub = {
        .scratch        = dstring::new_with_capacity(tmem, 64),
        .stream         = sub_reader.init(expanded),
        .allocator      = outer.allocator,
        .max_depth      = outer.max_depth,
        .entities       = outer.entities,
        .dtd_defaults   = outer.dtd_defaults,
        .param_entities = outer.param_entities,
        .entity_resolver      = outer.entity_resolver,
        .entity_resolver_data = outer.entity_resolver_data,
    };
    parse_dtd_attlist(&sub)!;
    // Sync back: list/map reallocs may have moved backing pointers.
    outer.dtd_defaults   = sub.dtd_defaults;
    outer.entities       = sub.entities;
    outer.param_entities = sub.param_entities;
}

// Skip an IGNORE conditional section until the matching ]]>, depth-aware.
fn void? skip_ignored_section(XmlContext* ctx) @local
{
    int depth = 1;
    while (depth > 0)
    {
        char c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == '\n') ctx.line++;
        if (c == '<')
        {
            char c2 = read_next(ctx)!;
            if (c2 != '!') { pushback(ctx, c2); continue; }
            char c3 = read_next(ctx)!;
            if (c3 == '[') { depth++; continue; }
            pushback(ctx, c3);
            continue;
        }
        if (c == ']')
        {
            char c2 = read_next(ctx)!;
            if (c2 != ']') { pushback(ctx, c2); continue; }
            char c3 = read_next(ctx)!;
            if (c3 == '>') { depth--; continue; }
            pushback(ctx, c3);
        }
    }
}

// Parse a conditional section after consuming <![; reads keyword (INCLUDE/IGNORE) then dispatches.
fn void? parse_conditional_section(XmlContext* ctx) @local
{
    DString kw_buf = dstring::new_with_capacity(tmem, 16);
    while (true)
    {
        skip_whitespace(ctx)!;
        char c = read_next(ctx)!;
        if (c == '[') break;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == '%')
        {
            char[64] buf;
            usz len = 0;
            while (true)
            {
                char d = read_next(ctx)!;
                if (d == ';') break;
                if (d == '\0') return UNEXPECTED_EOF~;
                if (len < 63) buf[len++] = d;
            }
            String? pe_val = ctx.param_entities.get((String)buf[:len]);
            if (try val = pe_val) kw_buf.append(val.trim());
            continue;
        }
        kw_buf.append(c);
    }
    String keyword = kw_buf.str_view().trim();
    if (keyword == "INCLUDE")
    {
        parse_conditional_include(ctx)!;
    }
    else
    {
        skip_ignored_section(ctx)!;
    }
}

fn void? parse_conditional_include(XmlContext* ctx) @local
{
    while (true)
    {
        skip_whitespace(ctx)!;
        char c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == ']')
        {
            char c2 = read_next(ctx)!;
            if (c2 == ']')
            {
                char c3 = read_next(ctx)!;
                if (c3 == '>') return;
                pushback(ctx, c3);
            }
            pushback(ctx, c2);
            continue;
        }
        if (c == '<')
        {
            char c2 = read_next(ctx)!;
            if (c2 == '?') { XmlNode* pi = parse_pi(ctx)!; pi.free(); continue; }
            if (c2 == '!')
            {
                char c3 = read_next(ctx)!;
                if (c3 == '-')
                {
                    if (read_next(ctx)! != '-') { skip_to_gt(ctx)!; continue; }
                    XmlNode* cm = parse_comment(ctx)!;
                    cm.free();
                    continue;
                }
                if (c3 == '[') { parse_conditional_section(ctx)!; continue; }
                pushback(ctx, c3);
                if (catch err = read_name_scratch(ctx)) { skip_to_gt(ctx)!; continue; }
                String kw = ctx.scratch.str_view();
                switch (kw)
                {
                    case "ENTITY":  parse_dtd_entity(ctx)!;
                    case "ATTLIST": parse_dtd_attlist_expanded(ctx)!;
                    default:        skip_to_gt(ctx)!;
                }
                continue;
            }
            pushback(ctx, c2);
            skip_to_gt(ctx)!;
            continue;
        }
        if (c == '%') { expand_param_entity_ref(ctx)!; continue; }
    }
}

fn void? parse_external_dtd_content(XmlContext* ctx) @local
{
    while (true)
    {
        skip_whitespace(ctx)!;
        char c = read_next(ctx)!;
        if (c == '\0') return;

        if (c == '<')
        {
            char c2 = read_next(ctx)!;
            if (c2 == '?')
            {
                XmlNode* pi = parse_pi(ctx)!;
                pi.free();
                continue;
            }
            if (c2 == '!')
            {
                char c3 = read_next(ctx)!;
                if (c3 == '-')
                {
                    if (read_next(ctx)! != '-') { skip_to_gt(ctx)!; continue; }
                    XmlNode* cm = parse_comment(ctx)!;
                    cm.free();
                    continue;
                }
                if (c3 == '[') { parse_conditional_section(ctx)!; continue; }
                pushback(ctx, c3);
                if (catch err = read_name_scratch(ctx)) { skip_to_gt(ctx)!; continue; }
                String kw = ctx.scratch.str_view();
                switch (kw)
                {
                    case "ENTITY":  parse_dtd_entity(ctx)!;
                    case "ATTLIST": parse_dtd_attlist_expanded(ctx)!;
                    default:        skip_to_gt(ctx)!;
                }
                continue;
            }
            pushback(ctx, c2);
            skip_to_gt(ctx)!;
            continue;
        }

        if (c == '%')
        {
            expand_param_entity_ref(ctx)!;
            continue;
        }
    }
}

fn void? parse_dtd_subset(XmlContext* ctx) @local
{
    while (true)
    {
        skip_whitespace(ctx)!;
        char c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == ']') return;

        if (c == '<')
        {
            char c2 = read_next(ctx)!;
            if (c2 == '?')
            {
                XmlNode* pi = parse_pi(ctx)!;
                pi.free();
                continue;
            }
            if (c2 == '!')
            {
                char c3 = read_next(ctx)!;
                if (c3 == '-')
                {
                    if (read_next(ctx)! != '-') { skip_to_gt(ctx)!; continue; }
                    XmlNode* cm = parse_comment(ctx)!;
                    cm.free();
                    continue;
                }
                pushback(ctx, c3);
                if (catch err = read_name_scratch(ctx)) { skip_to_gt(ctx)!; continue; }
                String kw = ctx.scratch.str_view();
                switch (kw)
                {
                    case "ENTITY":  parse_dtd_entity(ctx)!;
                    case "ATTLIST": parse_dtd_attlist(ctx)!;
                    default:        skip_to_gt(ctx)!;
                }
                continue;
            }
            pushback(ctx, c2);
            skip_to_gt(ctx)!;
            continue;
        }

        if (c == '%')
        {
            expand_param_entity_ref(ctx)!;
            continue;
        }
    }
}

// Entered after '<![CDATA['; reads until ']]>'. Uses a bracket counter to handle sequences like ]]]>.
fn XmlNode*? parse_cdata(XmlContext* ctx) @local
{
    ctx.scratch.clear();
    int brackets = 0; // consecutive ']' accumulated but not yet emitted
    while (true)
    {
        char c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c == '\n') ctx.line++;
        if (c == ']')
        {
            if (brackets == 2)
            {
                // Three or more consecutive ']': first is content, rest stay accumulated.
                ctx.scratch.append(']');
            }
            else
            {
                brackets++;
            }
            continue;
        }
        if (c == '>' && brackets == 2) break;
        for (int i = 0; i < brackets; i++) ctx.scratch.append(']');
        brackets = 0;
        ctx.scratch.append(c);
    }
    return new_leaf(ctx, CDATA, ctx.scratch.str_view().copy(ctx.allocator));
}

// Reads text until '<', expanding entities. Appends a TEXT child if text was accumulated.
// Returns true if stopped at '<', false at EOF.
fn bool? read_text_to(XmlContext* ctx, XmlNode* parent) @local
{
    ctx.scratch.clear();
    while (true)
    {
        char c = read_next(ctx)!;
        if (c == '\0') return false;
        if (c == '<')
        {
            String view = ctx.scratch.str_view();
            if (view.len > 0) parent.children.push(new_leaf(ctx, TEXT, view.copy(ctx.allocator)));
            return true;
        }
        if (c == '&') { append_entity(ctx, parent)!; continue; }
        if (c == '\n') ctx.line++;
        ctx.scratch.append(c);
    }
}

// Parses attributes and consumes the closing '>' or '/>'. Returns true if self-closing.
fn bool? parse_attrs_closing(XmlContext* ctx, XmlNode* node) @local
{
    while (true)
    {
        skip_whitespace(ctx)!;
        char c = read_next(ctx)!;
        if (c == '>') return false;
        if (c == '/')
        {
            if (read_next(ctx)! != '>') return UNEXPECTED_CHARACTER~;
            return true;
        }
        if (c == '\0') return UNEXPECTED_EOF~;
        if (!is_name_start(c)) return INVALID_ATTRIBUTE~;
        pushback(ctx, c);

        String attr_name = read_name(ctx)!;

        skip_whitespace(ctx)!;
        if (read_next(ctx)! != '=')
        {
            attr_name.free(ctx.allocator);
            return INVALID_ATTRIBUTE~;
        }
        skip_whitespace(ctx)!;
        char quote = read_next(ctx)!;
        if (quote != '"' && quote != '\'')
        {
            attr_name.free(ctx.allocator);
            return INVALID_ATTRIBUTE~;
        }
        String? attr_val = read_attr_value(ctx, quote);
        if (catch err = attr_val)
        {
            attr_name.free(ctx.allocator);
            return err~;
        }
        node.attrs.set(attr_name, attr_val); // HashMap copies the key (COPY_KEYS=true)
        attr_name.free(ctx.allocator);
    }
}

fn void? parse_content(XmlContext* ctx, XmlNode* parent, String close_tag) @local
{
    while (true)
    {
        bool hit_lt = read_text_to(ctx, parent)!;
        if (!hit_lt) return UNEXPECTED_EOF~;

        char c = read_next(ctx)!;

        if (c == '/')
        {
            read_name_scratch(ctx)!;
            if (ctx.scratch.str_view() != close_tag) return MISMATCHED_TAG~;
            skip_whitespace(ctx)!;
            if (read_next(ctx)! != '>') return UNEXPECTED_CHARACTER~;
            return;
        }

        if (c == '!')
        {
            char c2 = read_next(ctx)!;
            if (c2 == '-')
            {
                if (read_next(ctx)! != '-') return UNEXPECTED_CHARACTER~;
                XmlNode* comment = parse_comment(ctx)!;
                if (ctx.keep_comments)
                {
                    parent.children.push(comment);
                }
                else
                {
                    comment.free();
                }
                continue;
            }
            if (c2 == '[')
            {
                match_str(ctx, "CDATA[")!;
                XmlNode* cdata = parse_cdata(ctx)!;
                parent.children.push(cdata);
                continue;
            }
            return UNEXPECTED_CHARACTER~;
        }

        if (c == '?')
        {
            XmlNode* pi = parse_pi(ctx)!;
            if (ctx.keep_processing_instructions)
            {
                parent.children.push(pi);
            }
            else
            {
                pi.free();
            }
            continue;
        }

        if (is_name_start(c))
        {
            pushback(ctx, c);
            XmlNode* child = parse_element(ctx)!;
            parent.children.push(child);
            continue;
        }

        if (c == '\0') return UNEXPECTED_EOF~;
        return UNEXPECTED_CHARACTER~;
    }
}

// §3.3.3: after whitespace→space normalization, collapse runs and strip leading/trailing spaces.
fn String nmtokens_normalize(Allocator alloc, String s) @local
{
    DString result = dstring::new_with_capacity(alloc, s.len);
    bool need_space = false;
    bool has_content = false;
    foreach (c : s)
    {
        if (c == ' ')
        {
            if (has_content) need_space = true;
        }
        else
        {
            if (need_space) { result.append(' '); need_space = false; }
            result.append(c);
            has_content = true;
        }
    }
    return result.str_view().copy(alloc);
}

fn void apply_dtd_defaults(XmlContext* ctx, XmlNode* node, String elem_name) @local
{
    foreach (d : ctx.dtd_defaults)
    {
        if (d.element != elem_name) continue;
        bool present = node.has_attr(d.attr);
        if (!present && d.has_default)
        {
            String val = d.value.copy(ctx.allocator);
            if (d.nmtokens) val = nmtokens_normalize(ctx.allocator, val);
            node.attrs.set(d.attr, val);
        }
        else if (present && d.nmtokens)
        {
            String? existing = node.attrs.get(d.attr);
            if (try v = existing)
            {
                String normalized = nmtokens_normalize(ctx.allocator, v);
                if (normalized != v)
                {
                    v.free(ctx.allocator);
                    node.attrs.set(d.attr, normalized);
                }
                else
                {
                    normalized.free(ctx.allocator);
                }
            }
        }
    }
}

fn XmlNode*? parse_element(XmlContext* ctx) @local
{
    defer ctx.depth--;
    if (++ctx.depth >= ctx.max_depth) return xml::MAX_DEPTH_REACHED~;

    String name = read_name(ctx)!;
    XmlNode* node = new_element(ctx, name);
    defer catch node.free();

    bool self_closing = parse_attrs_closing(ctx, node)!;
    if (ctx.dtd_defaults.len() > 0) apply_dtd_defaults(ctx, node, name);
    if (!self_closing) parse_content(ctx, node, name)!;

    return node;
}

fn XmlDoc? parse_document(XmlContext* ctx) @local
{
    // Skip optional UTF-8 BOM (EF BB BF).
    char c = read_next(ctx)!;
    if (c == '\xef')
    {
        if (read_next(ctx)! != '\xbb' || read_next(ctx)! != '\xbf')
        {
            return UNEXPECTED_CHARACTER~;
        }
    }
    else
    {
        pushback(ctx, c);
    }

    ctx.entities.init(tmem);
    ctx.dtd_defaults.init(tmem);
    ctx.param_entities.init(tmem);

    XmlNodeList prologue;
    prologue.init(ctx.allocator);
    XmlNodeList epilogue;
    epilogue.init(ctx.allocator);
    XmlNode* xml_declaration = null;
    XmlDoctype* doctype = null;
    defer catch
    {
        foreach (node : prologue) node.free(); prologue.free();
        foreach (node : epilogue) node.free(); epilogue.free();
        if (xml_declaration) xml_declaration.free();
        if (doctype)
        {
            if (doctype.name.ptr)      doctype.name.free(ctx.allocator);
            if (doctype.public_id.ptr) doctype.public_id.free(ctx.allocator);
            if (doctype.system_id.ptr) doctype.system_id.free(ctx.allocator);
            alloc::free(ctx.allocator, doctype);
        }
    }

    XmlNode* root = null;
    while (!root)
    {
        skip_whitespace(ctx)!;
        c = read_next(ctx)!;
        if (c == '\0') return UNEXPECTED_EOF~;
        if (c != '<') return UNEXPECTED_CHARACTER~;

        c = read_next(ctx)!;
        if (c == '?')
        {
            XmlNode* pi = parse_pi(ctx)!;
            if (pi.name == "xml")
            {
                // Always stored regardless of keep_processing_instructions.
                if (xml_declaration) xml_declaration.free();
                xml_declaration = pi;
            }
            else if (ctx.keep_processing_instructions)
            {
                prologue.push(pi);
            }
            else
            {
                pi.free();
            }
            continue;
        }
        if (c == '!')
        {
            char c2 = read_next(ctx)!;
            if (c2 == '-')
            {
                if (read_next(ctx)! != '-') return UNEXPECTED_CHARACTER~;
                if (ctx.keep_comments)
                {
                    prologue.push(parse_comment(ctx)!);
                }
                else
                {
                    skip_comment(ctx)!;
                }
                continue;
            }
            if (c2 == 'D')
            {
                match_str(ctx, "OCTYPE")!;
                if (doctype)
                {
                    if (doctype.name.ptr)      doctype.name.free(ctx.allocator);
                    if (doctype.public_id.ptr) doctype.public_id.free(ctx.allocator);
                    if (doctype.system_id.ptr) doctype.system_id.free(ctx.allocator);
                    alloc::free(ctx.allocator, doctype);
                }
                doctype = parse_doctype(ctx)!;
                continue;
            }
            return UNEXPECTED_CHARACTER~;
        }
        if (is_name_start(c))
        {
            pushback(ctx, c);
            root = parse_element(ctx)!;
            break;
        }
        return UNEXPECTED_CHARACTER~;
    }

    while (true)
    {
        skip_whitespace(ctx)!;
        c = read_next(ctx)!;
        if (c == '\0') break;
        if (c != '<') return UNEXPECTED_CHARACTER~;
        c = read_next(ctx)!;
        if (c == '?')
        {
            XmlNode* pi = parse_pi(ctx)!;
            if (ctx.keep_processing_instructions)
            {
                epilogue.push(pi);
            }
            else
            {
                pi.free();
            }
            continue;
        }
        if (c == '!')
        {
            char c2 = read_next(ctx)!;
            if (c2 == '-')
            {
                if (read_next(ctx)! != '-') return UNEXPECTED_CHARACTER~;
                if (ctx.keep_comments)
                {
                    epilogue.push(parse_comment(ctx)!);
                }
                else
                {
                    skip_comment(ctx)!;
                }
                continue;
            }
        }
        return UNEXPECTED_CHARACTER~;
    }

    return {
        .allocator       = ctx.allocator,
        .root            = root,
        .prologue        = prologue,
        .epilogue        = epilogue,
        .xml_declaration = xml_declaration,
        .doctype         = doctype,
    };
}

fn void? write_indent(ByteWriter* bw, String indent, int depth) @local
{
    for (int i = 0; i < depth; i++) bw.write(indent)!;
}

// Returns true if the element should use block (indented) layout when pretty-printing.
fn bool has_block_children(XmlNode* node) @local
{
    foreach (child : node.children)
    {
        switch (child.type)
        {
            case ELEMENT:
            case COMMENT:
            case PROCESSING_INSTRUCTION:
                return true;
            default: break;
        }
    }
    return false;
}

fn void? write_escaped_text(ByteWriter* bw, String s) @local
{
    foreach (c : s)
    {
        switch (c)
        {
            case '<': bw.write("&lt;")!;
            case '>': bw.write("&gt;")!;
            case '&': bw.write("&amp;")!;
            // §2.11: literal \r would normalize to \n on re-parse, changing content.
            case '\r': bw.write("&#xD;")!;
            default:   bw.write_byte(c)!;
        }
    }
}

fn void? write_escaped_attr(ByteWriter* bw, String s) @local
{
    foreach (c : s)
    {
        switch (c)
        {
            case '<':  bw.write("&lt;")!;
            case '&':  bw.write("&amp;")!;
            case '"':  bw.write("&quot;")!;
            // §3.3.3: literal \t/\n/\r normalize to space on re-parse; encode as char refs to preserve.
            case '\t': bw.write("&#x9;")!;
            case '\n': bw.write("&#xA;")!;
            case '\r': bw.write("&#xD;")!;
            default:   bw.write_byte(c)!;
        }
    }
}

// Attribute order is HashMap-defined (not insertion order).
// TODO: switch to OrderedMap when https://github.com/c3lang/c3c/pull/3067 is merged.
fn void? write_node(ByteWriter* bw, XmlNode* node, String indent, int depth) @local
{
    switch (node.type)
    {
        case ELEMENT:
            io::fprintf(bw, "<%s", node.name)!;
            node.attrs.@each(; String k, String v)
            {
                io::fprintf(bw, ` %s="`, k)!!;
                write_escaped_attr(bw, v)!!;
                bw.write_byte('"')!!;
            };
            if (node.children.is_empty())
            {
                bw.write("/>")!;
            }
            else
            {
                bool block = indent.len > 0 && has_block_children(node);
                bw.write_byte('>')!;
                foreach (child : node.children)
                {
                    if (block)
                    {
                        bw.write_byte('\n')!;
                        write_indent(bw, indent, depth + 1)!;
                    }
                    write_node(bw, child, indent, depth + 1)!;
                }
                if (block)
                {
                    bw.write_byte('\n')!;
                    write_indent(bw, indent, depth)!;
                }
                io::fprintf(bw, "</%s>", node.name)!;
            }
        case TEXT:
            write_escaped_text(bw, node.content)!;
        case CDATA:
            io::fprintf(bw, "<![CDATA[%s]]>", node.content)!;
        case COMMENT:
            io::fprintf(bw, "<!--%s-->", node.content)!;
        case PROCESSING_INSTRUCTION:
            if (node.content.len > 0)
            {
                io::fprintf(bw, "<?%s %s?>", node.name, node.content)!;
            }
            else
            {
                io::fprintf(bw, "<?%s?>", node.name)!;
            }
    }
}
