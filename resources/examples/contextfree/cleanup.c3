import std::io;


struct Resource
{
	String name;
}

fault Error
{
	WELCOME_TO_YOUR_DOOM
}

fn Resource! resource_init(String name)
{
	io::printfn("open %s", name);
	return { name };
}

fn void Resource.deinit(Resource this) => io::printfn("close %s", this.name);

macro void! @open_with(String name; @body(Resource resource))
{
	Resource resource = resource_init(name)!;
	defer
	{
		io::printn("Using open_with to close");
		resource.deinit();
	}
	@body(resource);
}

fn Resource! prep_out(String out_name, String[] prep_names)
{
	Resource writer = resource_init(out_name)!; // Rethrow the optional result
	defer catch writer.deinit();
	foreach (name : prep_names)
	{
		@open_with(name; Resource reader)
		{
	        io::printfn("use %s", reader.name);
	        // if (true) return Error.WELCOME_TO_YOUR_DOOM?;
		}!;
	}
	return writer;
}

fn void main()
{
	Resource writer = prep_out("out", String[] { "a", "b"})!!;
    defer writer.deinit();
    io::printn("use out");
}