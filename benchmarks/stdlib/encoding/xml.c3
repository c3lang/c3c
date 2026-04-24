module xml_benchmarks;
import std::encoding::xml;
import std::io;
import std::io::file;
import std::compression::gzip;

String hamlet_xml;
String labels_xml;
char[8192] xml_stack;

fn void bench_setup() @init
{
	set_benchmark_warmup_iterations(1);
	set_benchmark_max_iterations(20);

	// Load hamlet.c3.xml
	char[]? data_hamlet = file::load(mem, "resources/examples/xml/hamlet.xml");
	if (catch err = data_hamlet)
	{
		hamlet_xml = `<root><child attr="val">Text content</child></root>`;
	}
	else
	{
		hamlet_xml = (String)data_hamlet;
	}

	// Load labels.xml.gz and decompress using stdlib
	char[]? data_gz = file::load(mem, "resources/examples/xml/labels.xml.gz");
	if (catch err_gz = data_gz)
	{
		labels_xml = hamlet_xml;
	}
	else
	{
		char[]? data_uncompressed = gzip::uncompress_bytes(mem, data_gz);
		if (catch err_uncompressed = data_uncompressed)
		{
			labels_xml = hamlet_xml;
		}
		else
		{
			labels_xml = (String)data_uncompressed;
		}
	}

	set_benchmark_func_iterations($reflect(xml_parse_hamlet).qname, 100);
	set_benchmark_func_iterations($reflect(xml_parse_discogs).qname, 2);
	set_benchmark_func_iterations($reflect(xml_dom_parse_hamlet).qname, 50);
	set_benchmark_func_iterations($reflect(xml_dom_search_hamlet).qname, 200);
	set_benchmark_func_iterations($reflect(xml_dom_write_hamlet).qname, 100);
	set_benchmark_func_iterations($reflect(xml_dom_modify_hamlet).qname, 500);
	set_benchmark_func_iterations($reflect(xml_write_simple).qname, 10000);
}


module xml_benchmarks @benchmark;

import std::encoding::xml;
import std::io;
import std::collections::list;

fn void xml_parse_hamlet()
{
	@pool()
	{
		ByteReader br;
		br.init(hamlet_xml);

		XmlReader r;
		r.init(&br, &xml_stack)!!;

		while (true)
		{
			if (catch err = r.next_event())
			{
				if (err == io::EOF) break;
				unreachable("Unexpected error in benchmark: %s", err);
			}
		}
	};
}

fn void xml_parse_discogs()
{
	@pool()
	{
		ByteReader br;
		br.init(labels_xml);

		XmlReader r;
		r.init(&br, &xml_stack)!!;

		while (true)
		{
			if (catch err = r.next_event())
			{
				if (err == io::EOF) break;
				unreachable("Unexpected error in benchmark: %s", err);
			}
		}
	};
}

fn void xml_write_simple()
{
	@pool()
	{
		ByteWriter bw;
		bw.tinit(8192);

		XmlSerializer w;
		w.init(&bw);

		w.start_element("root")!!;
		w.add_attribute("a", "v")!!;
		for (int i = 0; i < 100; i++)
		{
			w.start_element("child")!!;
			w.add_content("Some test content with escaping: < & >")!!;
			w.end_element("child")!!;
		}
		w.end_element("root")!!;
	};
}

fn void xml_dom_parse_hamlet()
{
	@pool()
	{
		XmlDocument* doc = xml::parse(mem, hamlet_xml)!!;
		doc.free();
	};
}

fn void xml_dom_search_hamlet()
{
	static XmlDocument* doc = null;
	if (!doc) doc = xml::parse(mem, hamlet_xml)!!;

	@pool()
	{
		XmlNodeList speakers;
		speakers.init(tmem);
		doc.root.find_all("SPEAKER", &speakers);
	};
}

fn void xml_dom_write_hamlet()
{
	static XmlDocument* doc = null;
	if (!doc) doc = xml::parse(mem, hamlet_xml)!!;

	@pool()
	{
		ByteWriter bw;
		bw.tinit(512 * 1024);

		XmlSerializer w;
		w.init(&bw);
		(void)w.serialize(doc);
	};
}

fn void xml_dom_modify_hamlet()
{
	static XmlDocument* doc = null;
	if (!doc) doc = xml::parse(mem, hamlet_xml)!!;

	@pool()
	{
		XmlNodeList speakers;
		speakers.init(tmem);
		doc.root.find_all("SPEAKER", &speakers);
		foreach (node : speakers)
		{
			node.set_name("SPEAKER_MODIFIED");
		}
		foreach (node : speakers)
		{
			node.set_name("SPEAKER");
		}
	};
}
