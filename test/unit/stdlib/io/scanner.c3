module scanner_test @test;
import std::collections::list;
import std::io;

def Results = List(<String>);

struct ScanTest
{
    String in;
    String[] out;
    String left_over;
}

fn void! test_scanner()
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
        			return err?;
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