module xml_benchmarks;
import std, std::encoding::xml, std::time::clock;

struct BenchData
{
	String tiny;
	String common;
	String deep;
	String attrs;
	String large_text;
	String comprehensive;
	XmlDoc doc_comp;
}
BenchData data;

fn void initialize() @init
{
	data.tiny = `<root><item id="1">Hello</item><item id="2">World</item></root>`;
	data.common = generate_common_xml(mem);
	data.deep = generate_deep_xml(mem);
	data.attrs = generate_many_attrs_xml(mem);
	data.large_text = generate_large_text_xml(mem);
	data.comprehensive = generate_comprehensive_xml(mem);
	data.doc_comp = xml::parse_string(mem, data.comprehensive)!!;

	set_benchmark_warmup_iterations(2);
	set_benchmark_max_iterations(200);

	set_benchmark_func_iterations("xml_bench::parse_tiny", 50000);
	set_benchmark_func_iterations("xml_bench::parse_common", 430);
	set_benchmark_func_iterations("xml_bench::parse_deep", 2500);
	set_benchmark_func_iterations("xml_bench::parse_comprehensive", 140);
	set_benchmark_func_iterations("xml_bench::encode_comprehensive", 350);
}

fn String generate_common_xml(Allocator allocator)
{
	DString ds = dstring::new(allocator);
	ds.append(`<root>`);
	for (int i = 0; i < 100; i++) {
		ds.appendf(`<item id="%d" category="benchmark">`, i);
		ds.appendf(`<name>Item %d</name>`, i);
		ds.appendf(`<description>This is a description for item %d.</description>`, i);
		ds.append(`</item>`);
	}
	ds.append(`</root>`);
	return ds.str_view();
}

fn String generate_deep_xml(Allocator allocator)
{
	DString ds = dstring::new(allocator);
	for (int i = 0; i < 100; i++) ds.appendf(`<layer%d>`, i);
	ds.append(`content`);
	for (int i = 99; i >= 0; i--) ds.appendf(`</layer%d>`, i);
	return ds.str_view();
}

fn String generate_many_attrs_xml(Allocator allocator)
{
	DString ds = dstring::new(allocator);
	ds.append(`<root `);
	for (int i = 0; i < 100; i++) ds.appendf(`attr%d="value%d" `, i, i);
	ds.append(`/>`);
	return ds.str_view();
}

fn String generate_large_text_xml(Allocator allocator)
{
	DString ds = dstring::new(allocator);
	ds.append(`<root>`);
	for (int i = 0; i < 1000; i++) ds.append(`The quick brown fox jumps over the lazy dog. `);
	ds.append(`</root>`);
	return ds.str_view();
}

fn String generate_comprehensive_xml(Allocator allocator)
{
	String src = `
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
    </book>`;
	DString ds = dstring::new(allocator);
	ds.append(`<library>`);
	for (int i = 0; i < 50; i++) ds.append(src);
	ds.append(`</library>`);
	return ds.str_view();
}


module xml_benchmarks @benchmark;
import std::encoding::xml;

fn void parse_tiny()
{
	@start_benchmark();
	@pool() {
		XmlDoc doc = xml::parse_string(tmem, data.tiny)!!;
		doc.free();
	};
	@end_benchmark();
}

fn void parse_common()
{
	@start_benchmark();
	@pool() {
		XmlDoc doc = xml::parse_string(tmem, data.common)!!;
		doc.free();
	};
	@end_benchmark();
}

fn void parse_deep()
{
	@start_benchmark();
	@pool() {
		XmlDoc doc = xml::parse_string(tmem, data.deep)!!;
		doc.free();
	};
	@end_benchmark();
}

fn void parse_comprehensive()
{
	@start_benchmark();
	@pool() {
		XmlDoc doc = xml::parse_string(tmem, data.comprehensive)!!;
		doc.free();
	};
	@end_benchmark();
}

fn void encode_comprehensive()
{
	@start_benchmark();
	@pool() {
		String s = data.doc_comp.tencode();
	};
	@end_benchmark();
}
