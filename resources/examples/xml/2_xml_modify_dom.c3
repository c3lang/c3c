/*
 * This example demonstrates the "Smart" DOM API.
 * It shows how to easily find, modify, and serialize XML nodes
 * without manual memory management or complex traversals.
 */
module example_modify_dom;
import std::io;
import std::encoding::xml;
import std::collections;

fn int main() 
{
	String xml_input = 
		"<settings>\n"
		"    <theme>light</theme>\n"
		"    <font size=\"12\">Sans</font>\n"
		"</settings>";

	// 1. Load the XML into a DOM tree.
	// We'll get a pointer back to a heap-allocated document.
	XmlDocument* doc = xml::parse(mem, xml_input)!!;
	defer doc.free();

	// 2. Locate and modify specific nodes using the new Unified API
	if (try theme = doc.root.find("theme"))
	{
		theme.set_text("dark");
	}

	if (try font = doc.root.find("font"))
	{
		font.set_attr("size", "14");
	}

	// 3. Save the modified tree back to an out stream (stdout).
	XmlSerializer w;
	w.init(io::stdout());

	io::printn("Modified XML Output:");
	io::printn("--------------------");


	w.start_doc()!!;
	w.serialize(doc)!!;
	io::printn();

	return 0;
}
