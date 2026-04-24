/*
 * This example demonstrates the low-level streaming XmlReader API.
 * It reads the Hamlet XML file one event at a time (zero DOM allocation)
 * and re-serializes it to a new file, appending " (C3 Edition)" to any
 * TITLE element content.
 *
 * This is the most efficient approach for large files.
 */
module xml_read_stream;
import std::io;
import std::encoding::xml;

const String INPUT_FILE  = "resources/examples/xml/hamlet.xml";
const String OUTPUT_FILE = INPUT_FILE +++ ".modified.xml";

fn int main()
{
	File hamlet_file = io::file::open(INPUT_FILE, "rb")!!;
	defer hamlet_file.close();

	File out_file = io::file::open(OUTPUT_FILE, "wb")!!;
	defer out_file.close();

	char[4096] stack;
	XmlReader r;
	r.init(&hamlet_file, &stack)!!;

	XmlSerializer w;
	w.init(&out_file);

	bool in_title = false;

	while (true)
	{
		XmlEvent? ev_opt = r.next_event();
		if (catch err = ev_opt)
		{
			if (err == io::EOF) break;
			io::printfn("Error parsing XML: %s at line %d", err, r.parser.line);
			return 1;
		}
		XmlEvent ev = ev_opt;

		switch (ev.kind)
		{
			case START_ELEMENT:
				w.start_element(ev.name)!!;
				in_title = ev.name == "TITLE";
			case ATTRIBUTE:
				w.add_attribute(ev.name, ev.value)!!;
			case TEXT:
				if (in_title && ev.value.contains("Hamlet"))
				{
					w.add_content(ev.value)!!;
					w.add_content(" (C3 Edition)")!!;
				}
				else
				{
					w.add_content(ev.value)!!;
				}
			case END_ELEMENT:
				w.end_element(ev.name)!!;
				in_title = false;
			default:
				break;
		}
	}
	io::printfn("Successfully modified\t%s\n-> Written:\t\t%s", INPUT_FILE, OUTPUT_FILE);
	return 0;
}
