/*
 * This example shows how to use the DOM API to perform data analysis on a
 * larger XML file (Hamlet). It counts occurrences of SPEAKER elements
 * to find out who speaks the most.
 */
module hamlet_stats;
import std::io;
import std::encoding::xml;
import std::collections;

fn int main() 
{
	String path = "resources/examples/xml/hamlet.xml";
	io::printfn("Loading %s...", path);

	File f = file::open(path, "rb")!!;
	defer f.close();

	XmlDocument* doc = xml::load(mem, &f)!!;
	defer doc.free();

	// We'll use a HashMap to count speeches per character.
	HashMap{String, int} speech_counts;
	speech_counts.init(mem);
	defer speech_counts.free();

	// Search for all <SPEAKER> tags in the document using find_all.
	XmlNodeList speakers;
	speakers.init(tmem);
	doc.root.find_all("SPEAKER", &speakers);

	foreach (node : speakers) 
	{
		if (node.children.len() > 0 && node.children[0].kind == TEXT)
		{
			String name = node.children[0].value;
			// Increment the count for this speaker.
			int count = speech_counts.get(name) ?? 0;
			speech_counts.set(name, count + 1);
		}
	}

	io::printfn("\nTop speakers in Hamlet (more than 50 speeches):");
	io::printfn("-----------------------------------------------");
	
	speech_counts.@each(; name, count)
	{
		if (count > 50) 
		{
			io::printfn("%-15s : %d", name, count);
		}
	};

	return 0;
}
