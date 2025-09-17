import std;

const VERSION = "2.0";
enum Subcommand : (String name, String desc, String[] args)
{
	FOO = {"oekfe", "foek", { "foke", "foekfe"}},
	BAR = {"foek", "foek", { "foekf", "foekfe"}},
	BAZ = {"Foekfef", "foek", { "foke", "foekfe" }}
}
macro void help (String program_name, bool to_stderr = false)
{
    var printfn = to_stderr ? &io::printfn : &io::eprintfn;
    var printf = to_stderr ? &io::printfn : &io::eprintfn;

    @pool()
    {
        printfn("mu2d version " +++ VERSION);
        printfn("");
 (void) printfn("Usage: %s [flags] <subcommand> [options] ...", program_name.file_tbasename());
        printfn("");
        printfn("Subcommands:");
        var $ident = "    ";
        $foreach $subcommand : Subcommand.values:
            var $line = $ident +++ $subcommand.name;
            $foreach $arg : $subcommand.args:
                $line = $line +++ " " +++ $arg;
            $endforeach
            printfn($line);

            var $row_counter = 0;
            $foreach $c : $subcommand.desc:
                $if $row_counter == 0:
                    $row_counter += $ident.len * 2;
                    printf($ident + $ident); // #error: Cannot do the addition 'String' + 'String'
                $endif

                $if $row_counter + 1 >= 80 &&& $c == ' ':
                    printf("\n");
                    $row_counter = 0;
                $else
                    printf((String) { $c }); // #error: The format string must be a constant
                    $row_counter += 1;
                $endif
            $endforeach
        $endforeach
    };
}
fn void main()
{
	help("aaa", true);
}