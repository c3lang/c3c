/*
 * This example demonstrates the XmlWriter API, showing how to generate
 * XML documents with attributes, nested elements, and escaped text.
 * It also shows how the writer integrates with standard I/O via OutStream.
 */
module xml_write_example;
import std::io;
import std::encoding::xml;

fn int main() 
{
	XmlSerializer w;
	w.init(io::stdout());

	w.start_doc()!!;
	w.start_element("shop")!!;
	w.add_attribute("name", "The C3 Emporium")!!;
	w.add_attribute("location", "Internet")!!;

	w.start_element("inventory")!!;
		// Item 1
		w.start_element("item")!!;
			w.add_attribute("id", "c3-001")!!;
			w.start_element("name")!!;
				w.add_content("Magic Pointer")!!;
			w.end_element("name")!!;
			w.start_element("price")!!;
				w.add_attribute("currency", "USD")!!;
				w.add_content("49.99")!!;
			w.end_element("price")!!;
		w.end_element("item")!!;

		// Item 2
		w.start_element("item")!!;
			w.attr("id", "c3-002")!!;
			w.start_element("name")!!;
				w.add_content("Heap-Safe Potion")!!;
			w.end_element("name")!!;
			w.start_element("price")!!;
				w.add_attribute("currency", "ESC")!!;
				w.add_content("999.00")!!;
			w.end_element("price")!!;
			
			// Self-closing elements for metadata.
			w.start_element("meta")!!;
			w.add_empty_element("in-stock")!!;
			w.end_element("meta")!!;
		w.end_element("item")!!;
	w.end_element("inventory")!!;
	w.end_element("shop")!!;
	w.end_doc()!!;
	
	io::printn(); // or `io::stdout().flush()!!;`

	return 0;
}
