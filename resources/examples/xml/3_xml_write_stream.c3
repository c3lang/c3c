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
	XmlWriter w;
	w.init(io::stdout());

	w.start_doc()!!;
	w.open("shop")!!;
	w.attr("name", "The C3 Emporium")!!;
	w.attr("location", "Internet")!!;

	w.open("inventory")!!;
		// Item 1
		w.open("item")!!;
			w.attr("id", "c3-001")!!;
			w.open("name")!!;
				io::fprint(&w, "Magic Pointer")!!;
			w.close_el("name")!!;
			w.open("price")!!;
				w.attr("currency", "USD")!!;
				io::fprint(&w, "49.99")!!;
			w.close_el("price")!!;
		w.close_el("item")!!;

		// Item 2
		w.open("item")!!;
			w.attr("id", "c3-002")!!;
			w.open("name")!!;
				io::fprint(&w, "Heap-Safe Potion")!!;
			w.close_el("name")!!;
			w.open("price")!!;
				w.attr("currency", "ESC")!!;
				io::fprint(&w, "999.00")!!;
			w.close_el("price")!!;
			
			// Self-closing elements for metadata.
			w.open("meta")!!;
			w.empty("in-stock")!!;
			w.close_el("meta")!!;
		w.close_el("item")!!;
	w.close_el("inventory")!!;
	w.close_el("shop")!!;
	w.end_doc()!!;
	
	io::printn(); // or `io::stdout().flush()!!;`

	return 0;
}
