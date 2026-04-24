module xml_test @test;
import std::io;
import std::collections;
import std::encoding::xml;

fn void test_writer_simple()
{
	char[256] buf;
	ByteWriter bw;
	bw.init_with_buffer(buf[..]);

	XmlSerializer w;
	w.init(&bw);

	w.start_element("root")!!;
	w.add_attribute("attr", "val")!!;
	w.start_element("child")!!;
	w.add_content("Hello & World")!!;
	w.end_element("child")!!;
	w.end_element("root")!!;

	String expected = `<root attr="val"><child>Hello &amp; World</child></root>`;
	test::eq(bw.str_view(), expected);
}

fn void test_writer_self_closing()
{
	char[256] buf;
	ByteWriter bw;
	bw.init_with_buffer(buf[..]);

	XmlSerializer w;
	w.init(&bw);

	w.start_element("root")!!;
	w.add_empty_element("empty")!!;
	w.end_element("root")!!;

	String expected = `<root><empty /></root>`;
	test::eq(bw.str_view(), expected);
}

fn void test_writer_escaping()
{
	char[256] buf;
	ByteWriter bw;
	bw.init_with_buffer(buf[..]);

	XmlSerializer w;
	w.init(&bw);

	w.start_element("root")!!;
	w.add_attribute("a", "<\"&'>")!!;
	w.add_content("<\"&'>")!!;
	w.end_element("root")!!;

	String expected = `<root a="&lt;&quot;&amp;&apos;&gt;">&lt;&quot;&amp;&apos;&gt;</root>`;
	test::eq(bw.str_view(), expected);
}

fn void test_reader_simple()
{
	String xml_str = `<root a="v"><child>text</child></root>`;
	ByteReader br;
	br.init(xml_str);

	char[1024] stack;
	XmlReader r;
	r.init(&br, &stack)!!;

	XmlEvent ev @noinit;

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.START_ELEMENT);
	test::eq(ev.name, "root");

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.ATTRIBUTE);
	test::eq(ev.name, "a");
	test::eq(ev.value, "v");

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.START_ELEMENT);
	test::eq(ev.name, "child");

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.TEXT);
	test::eq(ev.value, "text");

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.END_ELEMENT);
	test::eq(ev.name, "child");

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.END_ELEMENT);
	test::eq(ev.name, "root");

	if (catch err = r.next_event())
	{
		test::eq(err, io::EOF);
	}
	else
	{
		assert(false, "Expected EOF");
	}
}

fn void test_reader_pi_and_ref()
{
	String xml_str = `<?pi data?><a>&lt;</a>`;
	ByteReader br;
	br.init(xml_str);

	char[1024] stack;
	XmlReader r;
	r.init(&br, &stack)!!;

	XmlEvent ev @noinit;

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.PI);
	test::eq(ev.name, "pi");
	test::eq(ev.value, "data");

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.START_ELEMENT);
	test::eq(ev.name, "a");

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.TEXT);
	test::eq(ev.value, "<");

	ev = r.next_event()!!;
	test::eq(ev.kind, XmlEventKind.END_ELEMENT);
	test::eq(ev.name, "a");
}

fn void test_reader_errors() {
	// Tag mismatch
	{
		String xml_str = `<a></b>`;
		ByteReader br;
		br.init(xml_str);
		char[1024] stack;
		XmlReader r;
		r.init(&br, &stack)!!;
		r.next_event()!!; // <a>
		if (catch err = r.next_event()) {
			test::eq(err, xml::XML_TAG_MISMATCH);
		} else {
			assert(false, "Expected TAG_MISMATCH");
		}
	}

	// Syntax error
	{
		String xml_str = `<root <`;
		ByteReader br;
		br.init(xml_str);
		char[1024] stack;
		XmlReader r;
		r.init(&br, &stack)!!;
		r.next_event()!!; // <root
		if (catch err = r.next_event())
		{
			test::eq(err, xml::XML_SYNTAX);
		}
		else
		{
			assert(false, "Expected SYNTAX error");
		}
	}

	// Truncated document
	{
		String xml_str = `<root>`;
		ByteReader br;
		br.init(xml_str);
		char[1024] stack;
		XmlReader r;
		r.init(&br, &stack)!!;
		r.next_event()!!; // <root>
		if (catch err = r.next_event()) {
			test::eq(err, xml::XML_SYNTAX);
		} else {
			assert(false, "Expected error on truncated document");
		}
	}
}

fn void test_dom_simple()
{
	String xml_str = `<root api="vulkan"><member>1</member><member>2</member></root>`;
	XmlDocument* doc = xml::parse(mem, xml_str)!!;
	defer doc.free();

	XmlNode* root = doc.root;
	test::eq(root.name, "root");
	test::eq(root.attr("api")!!, "vulkan"); // Changed to attr()
	test::eq(root.children.len(), 2);
}

fn void test_dom_search()
{
	String xml_str = `
	<root>
		<member category="a">1</member>
		<member category="b">2</member>
		<other>3</other>
	</root>`;
	
	XmlDocument* doc = xml::parse(mem, xml_str)!!;
	defer doc.free();
	
	List{XmlNode*} members;
	members.init(mem);
	defer members.free();
	
	doc.root.find_all("member", &members);
	test::eq(members.len(), 2);
	test::eq(members[0].attr("category")!!, "a");
	test::eq(members[1].children[0].value, "2");
}

fn void test_dom_advanced()
{
	String xml_str = `<root><book id="1"><title>C3</title></book></root>`;
	XmlDocument* doc = xml::parse(mem, xml_str)!!;
	defer doc.free();

	XmlNode* book = doc.root.find("book") ?? null;
	test::ne(book, null);
	test::eq(book.attr("id")!!, "1");

	XmlNode* title = book.find("title") ?? null;
	test::ne(title, null);
	test::eq(title.text()!!, "C3");
}

fn void test_dom_modification()
{
	String xml_str = `<root><item>old</item></root>`;
	XmlDocument* doc = xml::parse(mem, xml_str)!!;
	defer doc.free();

	XmlNode* item = doc.root.find("item") ?? null;
	test::ne(item, null);
	
	// Test set_text
	item.set_text("new");
	test::eq(item.text()!!, "new");

	// Test set_attr
	item.set_attr("status", "updated");
	test::eq(item.attr("status")!!, "updated");
}

fn void test_document_write2()
{
	String xml_str = `<root><child>text</child></root>`;
	XmlDocument* doc = xml::parse(mem, xml_str)!!;
	defer doc.free();

	char[256] buf;
	ByteWriter bw;
	bw.init_with_buffer(buf[..]);

	XmlSerializer w;
	w.init(&bw);

	w.start_doc()!!;
	w.serialize(doc)!!;
	w.end_doc()!!;

	String output = bw.str_view();
	test::@check(output.contains("<?xml version=\"1.0\""));
	test::@check(output.contains("<root><child>text</child></root>"));
}

fn void test_document_write()
{
	String xml_str = `<root><child>text</child></root>`;
	XmlDocument* doc = xml::parse(mem, xml_str)!!;
	defer doc.free();

	char[256] buf;
	ByteWriter bw;
	bw.init_with_buffer(buf[..]);

	xml::serialize(&bw, doc)!!;

	String output = bw.str_view();
	test::@check(output.contains("<?xml version=\"1.0\""));
	test::@check(output.contains("<root><child>text</child></root>"));
}

fn void test_dom_filter()
{
	String xml_str = `
	<root>
		<item match="yes">1</item>
		<item match="no">2</item>
		<other match="yes">3</other>
	</root>`;
	
	XmlDocument* doc = xml::parse(mem, xml_str)!!;
	defer doc.free();
	
	List{XmlNode*} results;
	results.init(mem);
	defer results.free();
	
	doc.root.find_by_filter(&my_filter, &results);
	test::eq(results.len(), 2);
	test::eq(results[0].name, "item");
	test::eq(results[1].name, "other");
}


module xml_test;
import std::encoding::xml;

fn bool my_filter(XmlNode* node)
{
	return node.kind == START_ELEMENT && (node.attr("match") ?? "") == "yes";
}