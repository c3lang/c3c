module json_pretty_print;
import std;

fn int main(String[] args)
{
    if (args.len < 2)
    {
        io::printn("Usage: json_pretty_print <file.json>");
        return 1;
    }

    String filename = args[1];

    File? file = file::open(filename, "r");
    if (catch err = file)
    {
        io::printfn("Error: Could not open file '%s' (%s)", filename, err);
        return 1;
    }
    defer (void)file.close();

    Object*? json_object = json::tload(&file);
    if (catch err = json_object)
    {
        io::printfn("Error: Failed to parse JSON (%s)", err);
        return 1;
    }

    if (catch err = json::marshal_to(io::stdout(), json_object, SPACE_4))
    {
        io::printfn("Error: Failed to write pretty printed JSON (%s)", err);
        return 1;
    }

    io::printn();

    return 0;
}