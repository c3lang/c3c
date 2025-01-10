module std::io @test;
import std::collections::list;

def Results = List(<String>);

struct ScanTest
{
    String in;
    String[] out;
    String left_over;
}

fn void scanner()
{
    ScanTest[] tcases = {
        {"aa,,bb", {"aa"}, "bb"},
        {"a,,b,,", {"a", "b"}, ""},
        {"ab,,c", {"ab"}, "c"},
        {"ab,,cd,,e", {"ab", "cd"}, "e"},
    };
    foreach (tc : tcases)
    {
        ByteReader br;
        br.init(tc.in);
        Scanner sc;
        char[4] buffer; // max match (2) + pattern length (2)
        sc.init(&br, buffer[..]);

        Results results;
        while LOOP: (true)
        {
        	char[]! res = sc.scan(",,");
        	if (catch err = res)
        	{
        		case SearchResult.MISSING:
        			break LOOP;
        		default:
        			return err?!!;
        	}
        	String str = (String)res;
            results.push(str.tconcat(""));
        }

        String[] got = results.array_view();
        assert(got == tc.out, "got %s; want %s", got, tc.out);
        char[] fl = sc.flush();
        String left_over = (String)fl;
        assert(left_over == tc.left_over, "%s -> %s", tc.in, left_over);
    }
}

fn void scanner_as_reader()
{
	ByteReader br;
	br.init("Lorem ipsum sit.");
	Scanner sc;
	char[8] buffer;
	sc.init(&br, buffer[..]);

	sc.scan(" ")!!;

	char[16] res;
	usz n = sc.read(&res)!!;
	String str = (String)res[:n];

	assert(str == "ipsum sit.", "got '%s'; want 'ipsum sit.'", str);
}