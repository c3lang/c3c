module test;
import libc;
import std::io;
import std::collections::maybe;

def MaybeString = Maybe(<String>);
def MaybeHead = Maybe(<Head>);
def new_head_val = maybe::value(<Head>);
def new_string_val = maybe::value(<String>);

fault TitleResult
{
	TITLE_MISSING
}

fault ReadError
{
	BAD_READ,
}

struct Doc { MaybeHead head; }
struct Head { MaybeString title; }

struct Summary
{
	MaybeString title;
	bool ok;
}

fn void! Summary.print(Summary *s, OutStream* out)
{
	io::fprintf(out, "Summary({ .title = %s, .ok = %s})", s.title.get() ?? "missing", s.ok)!;
}

fn Doc! read_doc(String url)
{
	if (url.contains("fail")) return ReadError.BAD_READ?;
	if (url.contains("head-missing")) return { };
	if (url.contains("title-missing")) return { .head = new_head_val({}) };
	if (url.contains("title-empty")) return { .head = new_head_val({ .title = new_string_val("")}) };
	return { .head = new_head_val({ .title = new_string_val(string::new_format("Title of %s", url)) }) };
}

fn Summary build_summary(Doc doc)
{
    return Summary {
        .title = new_string_val(doc.head.get().title.get()) ?? MaybeString {},
        .ok = true,
    };
}

fn Summary read_and_build_summary(String url)
{
	return build_summary(read_doc(url)) ?? Summary {};
}

fn bool! is_title_non_empty(Doc doc)
{
	String! title = doc.head.get().title.get();
	if (catch title) return TitleResult.TITLE_MISSING?;
	return title.len > 0;
}

fn bool! read_whether_title_non_empty(String url)
{
    return is_title_non_empty(read_doc(url));
}

fn String bool_to_string(bool b)
{
	return b ? "true" : "false";
}

fn void main()
{
    const String[] URLS = { "good", "title-empty", "title-missing", "head-missing", "fail" };
    DynamicArenaAllocator dynamic_arena;
	dynamic_arena.init(1024, allocator::heap());
	OutStream* out = io::stdout();
    foreach (String url : URLS)
    {
        mem::@scoped(&dynamic_arena)
        {
            io::printf(`Checking "https://%s/":` "\n", url);
            Summary summary = read_and_build_summary(url);
            io::fprintf(out, "  Summary: ")!!;
            summary.print(out)!!;
            io::fprintn(out, "")!!;
            io::fprintf(out, "  Title: %s\n", summary.title.get() ?? "")!!;
            bool! has_title = read_whether_title_non_empty(url);
            // This looks a bit less than elegant, but as you see it's mostly due to having to
            // use printf here.
            io::fprintf(out, "  Has title: %s vs %s\n", bool_to_string(has_title) ?? (@catch(has_title)).nameof, has_title ?? false)!!;
        };
        dynamic_arena.reset();
    }
    dynamic_arena.free();
}
