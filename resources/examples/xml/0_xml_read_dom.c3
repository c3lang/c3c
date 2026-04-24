/*
 * This example shows how to use the find_by_filter API to perform
 * advanced searches in a DOM tree using a custom filter function.
 */
module xml_filter_example;
import std::io;
import std::encoding::xml;
import std::collections;

// A custom filter function that matches "critical" tasks.
fn bool is_critical_task(XmlNode* node)
{
	return node.name == "task" && (node.attr("priority") ?? "") == "critical";
}

fn int main() 
{
	String xml_data = 
		"<project name=\"C3 Compiler\">\n"
		"    <task id=\"1\" priority=\"critical\">\n"
		"        <name>Fix memory leak in parser</name>\n"
		"    </task>\n"
		"    <task id=\"2\" priority=\"normal\">\n"
		"        <name>Update documentation</name>\n"
		"    </task>\n"
		"    <task id=\"3\" priority=\"critical\">\n"
		"        <name>Implement new optimization pass</name>\n"
		"    </task>\n"
		"</project>";

	XmlDocument* doc = xml::parse(mem, xml_data)!!;
	defer doc.free();

	// Finding nodes using our custom filter.
	XmlNodeList critical_tasks;
	critical_tasks.init(tmem);
	
	doc.root.find_by_filter(&is_critical_task, &critical_tasks);
	
	io::printfn("Found %d critical tasks:", critical_tasks.len());
	foreach (task : critical_tasks)
	{
		if (try name_node = task.find("name"))
		{
			io::printfn("- %s (ID: %s)", name_node.text() ?? "N/A", task.attr("id") ?? "?");
		}
	}

	return 0;
}
