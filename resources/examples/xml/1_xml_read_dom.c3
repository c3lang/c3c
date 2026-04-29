/*
 * This example demonstrates the high-level DOM API.
 * It shows how to parse a string, navigate the tree, and find specific nodes.
 */
module xml_dom_simple;
import std::io;
import std::encoding::xml;
import std::collections;

fn int main() 
{
	String xml_data = 
		"<library api=\"vulkan\">\n"
		"    <book id=\"1\">\n"
		"        <title>The C3 Programming Language</title>\n"
		"        <author>C3 Community</author>\n"
		"    </book>\n"
		"    <book id=\"2\">\n"
		"        <title>XML Mastery</title>\n"
		"        <author>Manu Linares</author>\n"
		"    </book>\n"
		"</library>";

	// 1. Loading from string using the parse() API.
	XmlDocument* doc = xml::parse(mem, xml_data)!!;
	defer doc.free();

	// 2. Accessing attributes with attr()
	String api = doc.root.attr("api") ?? "";
	if (api.len)
	{
		io::printfn("API: %s", api);
	}

	// 3. Find a node with find()
	if (try book = doc.root.find("book"))
	{
		io::printfn("Found first book: %s", book.name);
		if (try title = book.find("title"))
		{
			io::printfn("First book title: %s", title.text() ?? "N/A");
		}
	}

	// 4. Finding all matches with find_all()
	XmlNodeList books;
	books.init(tmem);
	doc.root.find_all("book", &books);
	
	io::printfn("\nAll books in library (%d found):", books.len());
	foreach (book : books)
	{
		if (try title = book.find("title"))
		{
			io::printfn("- %s", title.text() ?? "Unknown");
		}
	}

	return 0;
}
