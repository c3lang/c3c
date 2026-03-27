module xml_test @test;
import std::encoding::xml;
import std::io;

fn void test_simple_element()
{
    XmlDoc doc = xml::parse_string(mem, `<root/>`)!!;
    defer doc.free();
    assert(doc.root != null);
    assert(doc.root.type == XmlNodeType.ELEMENT);
    assert(doc.root.name == "root");
    assert(doc.root.children.is_empty());
}

fn void test_element_with_text()
{
    XmlDoc doc = xml::parse_string(mem, `<greeting>Hello, World!</greeting>`)!!;
    defer doc.free();
    assert(doc.root.name == "greeting");
    assert(doc.root.children.len() == 1);
    assert(doc.root.children.get(0).type == XmlNodeType.TEXT);
    assert(doc.root.children.get(0).content == "Hello, World!");
}

fn void test_nested_elements()
{
    XmlDoc doc = xml::parse_string(mem, `<a><b><c/></b></a>`)!!;
    defer doc.free();
    assert(doc.root.name == "a");
    XmlNode* b = doc.root.child("b")!!;
    assert(b.name == "b");
    XmlNode* c = b.child("c")!!;
    assert(c.name == "c");
    assert(c.children.is_empty());
}

fn void test_attributes_double_quoted()
{
    XmlDoc doc = xml::parse_string(mem, `<el id="42" name="foo"/>`)!!;
    defer doc.free();
    assert(doc.root.get_attr("id")!! == "42");
    assert(doc.root.get_attr("name")!! == "foo");
    assert(doc.root.has_attr("id"));
    assert(!doc.root.has_attr("missing"));
}

fn void test_attributes_single_quoted()
{
    XmlDoc doc = xml::parse_string(mem, `<el id='99'/>`)!!;
    defer doc.free();
    assert(doc.root.get_attr("id")!! == "99");
}

fn void test_attribute_with_entity()
{
    XmlDoc doc = xml::parse_string(mem, `<el title="a&amp;b"/>`)!!;
    defer doc.free();
    assert(doc.root.get_attr("title")!! == "a&b");
}

fn void test_attr_whitespace_normalization()
{
    // Per XML §3.3.3: literal \n, \r, \t in attribute values normalize to space.
    // Character references like &#xA; are preserved as their actual character.
    XmlDoc doc = xml::parse_string(mem, "<el a=\"x\ny\" b=\"&#xA;\" c=\"x&#xA;y\"/>")!!;
    defer doc.free();
    assert(doc.root.get_attr("a")!! == "x y");    // literal \n > space
    assert(doc.root.get_attr("b")!! == "\n");     // &#xA; > preserved as newline
	assert(doc.root.get_attr("c")!! == "x\ny");   // x&#xA;y; > preserved as newline
}

fn void test_text_entities()
{
    XmlDoc doc = xml::parse_string(mem, `<x>&lt;tag&gt;&amp;&quot;&apos;</x>`)!!;
    defer doc.free();
    assert(doc.root.text_content() == `<tag>&"'`);
}

fn void test_numeric_entity_decimal()
{
    XmlDoc doc = xml::parse_string(mem, `<x>&#65;</x>`)!!;
    defer doc.free();
    assert(doc.root.text_content() == "A");
}

fn void test_numeric_entity_hex()
{
    XmlDoc doc = xml::parse_string(mem, `<x>&#x41;</x>`)!!;
    defer doc.free();
    assert(doc.root.text_content() == "A");
}

fn void test_unknown_entity_passthrough()
{
    // Custom/unknown entities (e.g. DocBook's &hsize5;) -> pass through as-is.
    XmlDoc doc = xml::parse_string(mem, `<x>&hsize5; &mdash; &copy;</x>`)!!;
    defer doc.free();
    assert(doc.root.text_content() == "&hsize5; &mdash; &copy;");
}

fn void test_unknown_entity_in_attr()
{
    XmlDoc doc = xml::parse_string(mem, `<x title="a &mdash; b"/>`)!!;
    defer doc.free();
    assert(doc.root.get_attr("title")!! == "a &mdash; b");
}

fn void test_docbook_titlepage()
{
    String src = `<t:titlepage element="book" wrapper="fo:block">
    <t:titlepage-content side="recto">
      <title
             font-size="&hsize5;"
             space-before="&hsize5space;"
			 font-weight="bold"
             named-template="division.title"
             text-align="center"/>
    </t:titlepage-content>
  </t:titlepage>`;
    XmlDoc doc = xml::parse_string(mem, src)!!;
    defer doc.free();
    assert(doc.root.name == "t:titlepage");
    assert(doc.root.get_attr("element")!! == "book");
    assert(doc.root.get_attr("wrapper")!! == "fo:block");
    XmlNode* content = doc.root.child("t:titlepage-content")!!;
    assert(content.get_attr("side")!! == "recto");
    XmlNode* title = content.child("title")!!;
    assert(title.get_attr("font-size")!! == "&hsize5;");
    assert(title.get_attr("space-before")!! == "&hsize5space;");
    assert(title.get_attr("font-weight")!! == "bold");
    assert(title.get_attr("named-template")!! == "division.title");
	assert(title.get_attr("text-align")!! == "center");
}

fn void test_cdata()
{
    XmlDoc doc = xml::parse_string(mem, `<x><![CDATA[<not-a-tag> & raw]]></x>`)!!;
    defer doc.free();
    assert(doc.root.children.len() == 1);
    assert(doc.root.children.get(0).type == XmlNodeType.CDATA);
    assert(doc.root.children.get(0).content == "<not-a-tag> & raw");
}

fn void test_cdata_text_content()
{
    XmlDoc doc = xml::parse_string(mem, `<x>hello <![CDATA[world]]></x>`)!!;
    defer doc.free();
    assert(doc.root.text_content() == "hello world");
}

fn void test_cdata_bracket_sequences()
{
    // ]]]> — one bracket as content, ]]> terminates
    XmlDoc doc = xml::parse_string(mem, "<x><![CDATA[]]]></x>")!!;
    defer doc.free();
    assert(doc.root.children.len() == 1);
    assert(doc.root.children.get(0).content == "]");

    // ]]]]> — two brackets as content, ]]> terminates
    XmlDoc doc2 = xml::parse_string(mem, "<x><![CDATA[]]]]></x>")!!;
    defer doc2.free();
    assert(doc2.root.children.len() == 1);
    assert(doc2.root.children.get(0).content == "]]");
}

fn void test_comment_node()
{
    // With keep_comments: comments become COMMENT nodes.
    XmlDoc doc = xml::parse_string(mem, `<x><!-- a comment -->text</x>`, { .keep_comments = true })!!;
    defer doc.free();
    assert(doc.root.children.len() == 2);
    assert(doc.root.children.get(0).type == XmlNodeType.COMMENT);
    assert(doc.root.children.get(0).content == " a comment ");
    assert(doc.root.children.get(1).type == XmlNodeType.TEXT);
    assert(doc.root.text_content() == "text");
}

fn void test_comment_skipped()
{
    // Without keep_comments (default): comments are discarded.
    XmlDoc doc = xml::parse_string(mem, `<x><!-- a comment -->text</x>`)!!;
    defer doc.free();
    assert(doc.root.children.len() == 1);
    assert(doc.root.children.get(0).type == XmlNodeType.TEXT);
    assert(doc.root.text_content() == "text");
}

fn void test_prologue_comment()
{
    XmlDoc doc = xml::parse_string(mem, `<!-- comment --><root/>`)!!;
    defer doc.free();
    assert(doc.root.name == "root");
}

fn void test_processing_instruction_node()
{
    // With keep_processing_instructions: PIs become PROCESSING_INSTRUCTION nodes.
    XmlDoc doc = xml::parse_string(mem,
        `<x><?app-version 2.0?><?dbfo page-orientation="landscape"?></x>`,
        { .keep_processing_instructions = true })!!;
    defer doc.free();
    assert(doc.root.children.len() == 2);
    XmlNode* pi1 = doc.root.children.get(0);
    assert(pi1.type == XmlNodeType.PROCESSING_INSTRUCTION);
    assert(pi1.name == "app-version");
    assert(pi1.content == "2.0");
    XmlNode* pi2 = doc.root.children.get(1);
    assert(pi2.type == XmlNodeType.PROCESSING_INSTRUCTION);
    assert(pi2.name == "dbfo");
    assert(pi2.content == `page-orientation="landscape"`);
}

fn void test_xml_stylesheet_pi()
{
    XmlDoc doc = xml::parse_string(mem,
        `<!-- catalogue -->` `<?xml-stylesheet type="text/xsl" href="style.xsl"?><root/>`,
        { .keep_comments = true, .keep_processing_instructions = true })!!;
    defer doc.free();
    assert(doc.prologue.len() == 2);
    XmlNode* comment = doc.prologue.get(0);
    assert(comment.type == XmlNodeType.COMMENT);
    assert(comment.content == " catalogue ");
    XmlNode* pi = doc.prologue.get(1);
    assert(pi.type == XmlNodeType.PROCESSING_INSTRUCTION);
    assert(pi.name == "xml-stylesheet");
    assert(pi.content == `type="text/xsl" href="style.xsl"`);
    assert(doc.root.name == "root");
}

fn void test_processing_instruction_skipped()
{
    // Without keep_processing_instructions (default): PIs are discarded.
    XmlDoc doc = xml::parse_string(mem, `<x><?app-version 2.0?>text</x>`)!!;
    defer doc.free();
    assert(doc.root.children.len() == 1);
    assert(doc.root.children.get(0).type == XmlNodeType.TEXT);
}

fn void test_xml_declaration_skipped()
{
    XmlDoc doc = xml::parse_string(mem, `<?xml version="1.0" encoding="UTF-8"?><root/>`)!!;
    defer doc.free();
    assert(doc.root.name == "root");
}

fn void test_xml_declaration_node()
{
    XmlDoc doc = xml::parse_string(mem, `<?xml version="1.0" encoding="UTF-8"?><root/>`)!!;
    defer doc.free();
    assert(doc.xml_declaration != null);
    assert(doc.xml_declaration.type == XmlNodeType.PROCESSING_INSTRUCTION);
    assert(doc.xml_declaration.name == "xml");
    assert(doc.xml_declaration.content == `version="1.0" encoding="UTF-8"`);
}

fn void test_doctype_skipped()
{
    XmlDoc doc = xml::parse_string(mem, `<!DOCTYPE html><html/>`)!!;
    defer doc.free();
    assert(doc.root.name == "html");
}

fn void test_doctype_parsed()
{
    XmlDoc doc = xml::parse_string(mem,
        `<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd"><html/>`)!!;
    defer doc.free();
    assert(doc.doctype != null);
    assert(doc.doctype.name == "html");
    assert(doc.doctype.public_id == "-//W3C//DTD HTML 4.01//EN");
    assert(doc.doctype.system_id == "http://www.w3.org/TR/html4/strict.dtd");
}

fn void test_doctype_system_only()
{
    XmlDoc doc = xml::parse_string(mem,
        `<!DOCTYPE root SYSTEM "root.dtd"><root/>`)!!;
    defer doc.free();
    assert(doc.doctype != null);
    assert(doc.doctype.name == "root");
    assert(doc.doctype.public_id == "");
    assert(doc.doctype.system_id == "root.dtd");
}

fn void test_doctype_with_internal_subset()
{
    XmlDoc doc = xml::parse_string(mem, `<!DOCTYPE root [<!ELEMENT root ANY>]><root/>`)!!;
    defer doc.free();
    assert(doc.root.name == "root");
    assert(doc.doctype != null);
    assert(doc.doctype.name == "root");
}

fn void test_epilogue_nodes()
{
    XmlDoc doc = xml::parse_string(mem,
        `<root/><?app-version 2.0?><!-- end -->`,
        { .keep_processing_instructions = true, .keep_comments = true })!!;
    defer doc.free();
    assert(doc.epilogue.len() == 2);
    assert(doc.epilogue.get(0).type == XmlNodeType.PROCESSING_INSTRUCTION);
    assert(doc.epilogue.get(0).name == "app-version");
    assert(doc.epilogue.get(1).type == XmlNodeType.COMMENT);
    assert(doc.epilogue.get(1).content == " end ");
}

fn void test_mixed_content()
{
    XmlDoc doc = xml::parse_string(mem, `<p>Hello <b>World</b>!</p>`)!!;
    defer doc.free();
    // children: TEXT "Hello ", ELEMENT "b", TEXT "!"
    assert(doc.root.children.len() == 3);
    assert(doc.root.children.get(0).type == XmlNodeType.TEXT);
    assert(doc.root.children.get(0).content == "Hello ");
    assert(doc.root.children.get(1).type == XmlNodeType.ELEMENT);
    assert(doc.root.children.get(1).name == "b");
    assert(doc.root.children.get(2).type == XmlNodeType.TEXT);
    assert(doc.root.children.get(2).content == "!");
}

fn void test_children_named()
{
    XmlDoc doc = xml::parse_string(mem, `<root><item>a</item><item>b</item><other/></root>`)!!;
    defer doc.free();
    XmlNode*[] items = doc.root.children_named("item");
    assert(items.len == 2);
    assert(items[0].text_content() == "a");
    assert(items[1].text_content() == "b");
}

fn void test_whitespace_around_root()
{
    XmlDoc doc = xml::parse_string(mem, "  \n  <root/>  ")!!;
    defer doc.free();
    assert(doc.root.name == "root");
}

fn void test_encode_roundtrip()
{
    String src = `<?xml version="1.0" encoding="UTF-8"?><root id="1"><child>text</child></root>`;
    XmlDoc doc = xml::parse_string(mem, src)!!;
    defer doc.free();
    String encoded = doc.tencode();
    assert(encoded.starts_with(`<?xml version="1.0" encoding="UTF-8"?>`));
    // Re-parse the encoded output and verify structure is preserved
    XmlDoc doc2 = xml::tparse_string(encoded)!!;
    defer doc2.free();
    assert(doc2.root.name == "root");
    assert(doc2.root.get_attr("id")!! == "1");
    assert(doc2.root.child("child")!!.text_content() == "text");
}

fn void test_encode_escapes_text()
{
    XmlDoc doc = xml::parse_string(mem, `<x>&lt;hello&gt;</x>`)!!;
    defer doc.free();
    String encoded = doc.tencode();
    // The encoded output should escape < and > in text
    assert(encoded.contains("&lt;hello&gt;"));
}

fn void test_encode_self_closing()
{
    XmlDoc doc = xml::parse_string(mem, `<br/>`)!!;
    defer doc.free();
    String encoded = doc.tencode();
    assert(encoded.contains("/>"));
}

fn void test_tparse_string()
{
    @pool()
    {
        XmlDoc doc = xml::tparse_string(`<x/>`)!!;
        defer doc.free();
        assert(doc.root.name == "x");
    };
}

fn void test_comprehensive()
{
    // A single document that exercises: XML declaration, DOCTYPE, prologue
    // comment, multiple nesting levels, every attribute quote style, all five
    // named entities in text and in attributes, numeric entities (decimal and
    // hex), CDATA section, inline comment, processing instruction, self-closing
    // elements, elements with both attributes and children, and mixed content.
    String src = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE library [<!ELEMENT library ANY>]>
<!-- Library catalogue -->
<?app-version 2.0?>
<library name="City Library" established="1923" open='true'>

  <!-- Books section -->
  <books count="3">

    <book id="1" genre="fiction" available="yes">
      <title lang="en">The &amp; Adventures</title>
      <author first='Jane' last="Doe"/>
      <price currency="USD">12.99</price>
      <summary><![CDATA[A story with <special> chars & "quotes".]]></summary>
      <tags>
        <tag>adventure</tag>
        <tag>fiction</tag>
      </tags>
    </book>

    <book id="2" genre="non-fiction" available='no'>
      <title lang="de">Geschichte &amp; Kultur</title>
      <author first="Max" last='Müller'/>
      <price currency="EUR">24.50</price>
      <description>Contains &lt;markup&gt; examples &amp; code &quot;snippets&quot;.</description>
      <note>Apostrophe: &apos;test&apos;</note>
    </book>

    <book id="3" genre="sci-fi" available="yes">
      <title lang="en">&#84;ime &#x54;ravel</title>
      <author first="Alex" last="Smith"/>
      <price currency="GBP">9.99</price>
      <!-- Out of stock until &#x32;&#48;&#50;&#53; -->
      <tags><tag>sci-fi</tag><tag>time</tag><tag>space</tag></tags>
    </book>

  </books>

  <metadata>
    <created>2024-01-15</created>
    <updated>2024-06-01</updated>
    <contact email="info@library.example" phone='+1-555-0100'/>
    <self-closing-demo a="1" b='2' c="3"/>
  </metadata>

</library>`;

    XmlDoc doc = xml::parse_string(mem, src)!!;
    defer doc.free();

    // Root element
    assert(doc.root.name == "library");
    assert(doc.root.get_attr("name")!! == "City Library");
    assert(doc.root.get_attr("established")!! == "1923");
    assert(doc.root.get_attr("open")!! == "true");

    // <books>
    XmlNode* books = doc.root.child("books")!!;
    assert(books.get_attr("count")!! == "3");
    XmlNode*[] book_list = books.children_named("book");
    assert(book_list.len == 3);

    // Book 1 — entity in text, self-closing author, nested children
    XmlNode* b1 = book_list[0];
    assert(b1.get_attr("id")!! == "1");
    assert(b1.get_attr("genre")!! == "fiction");
    assert(b1.child("title")!!.text_content() == "The & Adventures");
    assert(b1.child("title")!!.get_attr("lang")!! == "en");
    XmlNode* a1 = b1.child("author")!!;
    assert(a1.get_attr("first")!! == "Jane");
    assert(a1.get_attr("last")!! == "Doe");
    assert(a1.children.is_empty());    // self-closing
    assert(b1.child("price")!!.text_content() == "12.99");
    assert(b1.child("price")!!.get_attr("currency")!! == "USD");
    // CDATA
    assert(b1.child("summary")!!.text_content() == `A story with <special> chars & "quotes".`);
    // Nested <tags>
    XmlNode*[] tags1 = b1.child("tags")!!.children_named("tag");
    assert(tags1.len == 2);
    assert(tags1[0].text_content() == "adventure");
    assert(tags1[1].text_content() == "fiction");

    // Book 2 — mixed entity types, single-quoted attrs
    XmlNode* b2 = book_list[1];
    assert(b2.get_attr("id")!! == "2");
    assert(b2.get_attr("available")!! == "no");
    assert(b2.child("title")!!.text_content() == "Geschichte & Kultur");
    assert(b2.child("author")!!.get_attr("last")!! == "Müller");
    // &lt; &gt; &quot; in text
    assert(b2.child("description")!!.text_content() == `Contains <markup> examples & code "snippets".`);
    // &apos; in text
    assert(b2.child("note")!!.text_content() == "Apostrophe: 'test'");

    // Book 3 — decimal and hex numeric entities in title, inline comment
    XmlNode* b3 = book_list[2];
    assert(b3.get_attr("genre")!! == "sci-fi");
    // &#84; = 'T', &#x54; = 'T' → "Time Travel"
    assert(b3.child("title")!!.text_content() == "Time Travel");
    XmlNode*[] tags3 = b3.child("tags")!!.children_named("tag");
    assert(tags3.len == 3);
    assert(tags3[2].text_content() == "space");

    // <metadata> — self-closing with multiple attrs
    XmlNode* meta = doc.root.child("metadata")!!;
    assert(meta.child("created")!!.text_content() == "2024-01-15");
    XmlNode* contact = meta.child("contact")!!;
    assert(contact.get_attr("email")!! == "info@library.example");
    assert(contact.get_attr("phone")!! == "+1-555-0100");
    assert(contact.children.is_empty());
    XmlNode* demo = meta.child("self-closing-demo")!!;
    assert(demo.get_attr("a")!! == "1");
    assert(demo.get_attr("b")!! == "2");
    assert(demo.get_attr("c")!! == "3");

    // Encode and re-parse: structure must survive a roundtrip
    String encoded = doc.tencode();
    XmlDoc doc2 = xml::tparse_string(encoded)!!;
    defer doc2.free();
    assert(doc2.root.name == "library");
    assert(doc2.root.child("books")!!.children_named("book").len == 3);
    assert(doc2.root.child("books")!!.children_named("book")[0]
        .child("title")!!.text_content() == "The & Adventures");
    assert(doc2.root.child("books")!!.children_named("book")[1]
        .child("description")!!.text_content() == `Contains <markup> examples & code "snippets".`);
}

fn void test_error_mismatched_tag()
{
    if (catch err = xml::parse_string(mem, `<a></b>`))
    {
        assert(err == xml::MISMATCHED_TAG);
    }
    else
    {
        assert(false, "Expected MISMATCHED_TAG");
    }
}

fn void test_error_unclosed_tag()
{
    if (catch err = xml::parse_string(mem, `<a>`))
    {
        assert(err == xml::UNEXPECTED_EOF);
    }
    else
    {
        assert(false, "Expected UNEXPECTED_EOF");
    }
}

fn void test_error_malformed_attribute()
{
    if (catch err = xml::parse_string(mem, `<a id=noquote/>`))
    {
        assert(err == xml::INVALID_ATTRIBUTE);
    }
    else
    {
        assert(false, "Expected INVALID_ATTRIBUTE");
    }
}

fn void test_error_no_root()
{
    if (catch err = xml::parse_string(mem, ``))
    {
        assert(err == xml::UNEXPECTED_EOF);
    }
    else
    {
        assert(false, "Expected UNEXPECTED_EOF");
    }
}

fn void test_attr_crlf_normalization()
{
    // \r\n in attribute value should become a single space, not two.
    XmlDoc doc = xml::parse_string(mem, "<el a=\"x\r\ny\"/>")!!;
    defer doc.free();
    assert(doc.root.get_attr("a")!! == "x y");
}

fn void test_invalid_numeric_entity()
{
    // Codepoints above U+10FFFF must error rather than crashing append_char32.
    if (catch err = xml::parse_string(mem, `<x>&#x200000;</x>`))
    {
        assert(err == xml::UNEXPECTED_CHARACTER);
    }
    else
    {
        assert(false, "Expected UNEXPECTED_CHARACTER for out-of-range codepoint");
    }
}

fn void test_error_depth_limit()
{
    // Build a deeply nested XML that exceeds the default max_depth of 128.
    DString s = dstring::new(tmem);
    for (int i = 0; i < 130; i++) s.appendf("<a%d>", i);
    s.append("x");
    for (int i = 129; i >= 0; i--) s.appendf("</a%d>", i);
    if (catch err = xml::parse_string(mem, s.str_view()))
    {
        assert(err == xml::MAX_DEPTH_REACHED);
    }
    else
    {
        assert(false, "Expected MAX_DEPTH_REACHED");
    }
}

fn void test_custom_max_depth()
{
    // Depth of 3 should succeed with max_depth = 4, fail with max_depth = 2.
    String src = `<a><b><c>x</c></b></a>`;
    XmlDoc doc = xml::parse_string(mem, src, { .max_depth = 4 })!!;
    defer doc.free();
    assert(doc.root.child("b")!!.child("c")!!.text_content() == "x");

    if (catch err = xml::parse_string(mem, src, { .max_depth = 2 }))
    {
        assert(err == xml::MAX_DEPTH_REACHED);
    }
    else
    {
        assert(false, "Expected MAX_DEPTH_REACHED");
    }
}

fn void test_unicode_element_names()
{
    XmlDoc doc = xml::parse_string(mem, "<图书><作者>张三</作者></图书>")!!;
    defer doc.free();
    assert(doc.root.name == "图书");
    assert(doc.root.child("作者")!!.text_content() == "张三");
}

fn void test_unicode_attr_names()
{
    XmlDoc doc = xml::parse_string(mem, `<el 名前="value"/>`)!!;
    defer doc.free();
    assert(doc.root.get_attr("名前")!! == "value");
}

fn void test_encode_pretty_basic()
{
    XmlDoc doc = xml::parse_string(mem, `<root><child>text</child></root>`)!!;
    defer doc.free();
    String encoded = doc.tencode({ .indent = "  " });
    // Must contain a newline and indentation
    assert(encoded.contains("\n"));
    assert(encoded.contains("  <child>"));
    // Re-parse must round-trip correctly
    XmlDoc doc2 = xml::tparse_string(encoded)!!;
    defer doc2.free();
    assert(doc2.root.child("child")!!.text_content() == "text");
}

fn void test_encode_pretty_nested()
{
    XmlDoc doc = xml::parse_string(mem, `<a><b><c>x</c></b></a>`)!!;
    defer doc.free();
    String encoded = doc.tencode({ .indent = "\t" });
    assert(encoded.contains("\t<b>"));
    assert(encoded.contains("\t\t<c>"));
    XmlDoc doc2 = xml::tparse_string(encoded)!!;
    defer doc2.free();
    assert(doc2.root.child("b")!!.child("c")!!.text_content() == "x");
}

fn void test_encode_pretty_inline_text()
{
    // Elements with only text/CDATA children stay inline even when indenting.
    XmlDoc doc = xml::parse_string(mem, `<root><title>Hello World</title></root>`)!!;
    defer doc.free();
    String encoded = doc.tencode({ .indent = "  " });
    // <title>Hello World</title> must be on one line (no newline inside it)
    assert(encoded.contains("<title>Hello World</title>"));
}

fn void test_encode_pretty_self_closing()
{
    XmlDoc doc = xml::parse_string(mem, `<root><br/><img src="x"/></root>`)!!;
    defer doc.free();
    String encoded = doc.tencode({ .indent = "  " });
    assert(encoded.contains("  <br/>"));
    assert(encoded.contains(`  <img`));
}

fn void test_encode_pretty_with_doctype_and_prologue()
{
    XmlDoc doc = xml::parse_string(mem,
        `<?xml version="1.0"?><!-- comment --><root/>`,
        { .keep_comments = true })!!;
    defer doc.free();
    String encoded = doc.tencode({ .indent = "  " });
    // Each top-level section on its own line
    assert(encoded.contains("\n<!--"));
    assert(encoded.contains("\n<root/>"));
}

fn void test_encode_compact_unchanged()
{
    // Default (no indent) must produce compact output without extra newlines.
    XmlDoc doc = xml::parse_string(mem, `<root><child>text</child></root>`)!!;
    defer doc.free();
    String encoded = doc.tencode();
    assert(!encoded.contains("\n"));
    assert(encoded == `<root><child>text</child></root>`);
}

fn void test_encode_version_override()
{
    XmlDoc doc = xml::parse_string(mem, `<root/>`)!!;
    defer doc.free();
    String encoded = doc.tencode({ .version = "1.1" });
    assert(encoded.starts_with(`<?xml version="1.1" encoding="UTF-8"?>`));
    assert(encoded.ends_with(`<root/>`));
}

fn void test_encode_pretty_roundtrip_comprehensive()
{
    String src = `<library>
  <book id="1"><title>The &amp; Adventures</title><tags><tag>fiction</tag></tags></book>
  <book id="2"><title>Histoire</title></book>
</library>`;
    XmlDoc doc = xml::parse_string(mem, src)!!;
    defer doc.free();
    String encoded = doc.tencode({ .indent = "  " });
    XmlDoc doc2 = xml::tparse_string(encoded)!!;
    defer doc2.free();
    XmlNode*[] books = doc2.root.children_named("book");
    assert(books.len == 2);
    assert(books[0].child("title")!!.text_content() == "The & Adventures");
    assert(books[0].child("tags")!!.children_named("tag")[0].text_content() == "fiction");
    assert(books[1].child("title")!!.text_content() == "Histoire");
}

fn void test_long_unknown_entity_passthrough()
{
    String name = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789extra";
    String src = string::format(mem, "<x>&%s;</x>", name);
    defer src.free(mem);
    String expected = string::format(mem, "&%s;", name);
    defer expected.free(mem);
    XmlDoc doc = xml::parse_string(mem, src)!!;
    defer doc.free();
    assert(doc.root.text_content() == expected);
}

fn void test_error_reject_nul_character()
{
    char[] src = { '<', 'x', '>', '\0', '<', '/', 'x', '>' };
    if (catch err = xml::parse_string(mem, (String)src))
    {
        assert(err == xml::UNEXPECTED_CHARACTER);
    }
    else
    {
        assert(false, "Expected UNEXPECTED_CHARACTER");
    }
}
