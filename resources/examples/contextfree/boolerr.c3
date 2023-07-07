module test;
import libc;
import std::io;

struct Doc { Head *head; }
struct Head { DString* title; }

struct Summary
{
	DString* title;
	bool ok;
}

struct StringData @private
{
	Allocator allocator;
	usz len;
	usz capacity;
	char[*] chars;
}

fn void Summary.print(Summary *s, File out)
{
	String title = s.title ? s.title.as_str() : "missing";
	out.printf("Summary({ .title = %s, .ok = %s})", title, s.ok);
}

fn bool contains(String haystack, String needle)
{
	usz len = haystack.len;
	usz needle_len = needle.len;
	if (len < needle_len) return false;
	if (!needle_len) return true;
	len -= needle_len - 1;
	for (usz i = 0; i < len; i++)
	{
		if (mem::equals(haystack[i..], needle))
		{
			return true;
		}
	}
	return false;
}

macro @dupe(value)
{
	$typeof(&value) temp = malloc_checked($typeof(value))!;
	*temp = value;
	return temp;
}

fault ReadError
{
	BAD_READ,
}

fn Doc! readDoc(String url)
{
	if (contains(url, "fail")) return ReadError.BAD_READ?;
	if (contains(url, "head-missing")) return { .head = null };
	if (contains(url, "title-missing")) return { @dupe(Head { .title = null }) };
	if (contains(url, "title-empty")) return { @dupe(Head { .title = @dupe((DString)null) }) };
	DString str;
	str.printf("Title of %s", url);
	return { @dupe(Head { .title = @dupe(str) }) };
}

fn Summary buildSummary(Doc doc)
{
    return Summary {
        .title = doc.head ? doc.head.title : null,
        .ok = true,
    };
}

fn Summary readAndBuildSummary(String url)
{
	return buildSummary(readDoc(url)) ?? Summary { .title = null, .ok = false };
	/*
	// or
	Summary summary = buildSummary(readDoc(url));
	if (catch summary) return Summary { .title = null, .ok = false };
	return summary;
	// or
	Summary summary = buildSummary(readDoc(url));
	if (try summary) return summary;
	return Summary { .title = null, .ok = false };
	*/
}


fault TitleResult
{
	TITLE_MISSING
}

fn bool! isTitleNonEmpty(Doc doc)
{
	if (!doc.head) return TitleResult.TITLE_MISSING?;
	DString* head = doc.head.title;
	if (!head) return TitleResult.TITLE_MISSING?;
	return head.len() > 0;
}


fn bool! readWhetherTitleNonEmpty(String url)
{
    return isTitleNonEmpty(readDoc(url));
}

fn String bool_to_string(bool b)
{
	return b ? "true" : "false";
}


fn void main()
{
    const String[] URLS = { "good", "title-empty", "title-missing", "head-missing", "fail" };
    DynamicArenaAllocator dynamic_arena;
	dynamic_arena.init(1024);
    foreach (String url : URLS)
    {
        mem::@scoped(&dynamic_arena)
        {
            io::printf(`Checking "https://%s/":` "\n", url);
            Summary summary = readAndBuildSummary(url);
            io::printf("  Summary: ");
            summary.print(io::stdout());
            io::printn("");
            String title_sure = summary.title ? summary.title.as_str() : "";
            io::printf("  Title: %s\n", title_sure);
            bool! has_title = readWhetherTitleNonEmpty(url);
            // This looks a bit less than elegant, but as you see it's mostly due to having to
            // use printf here.
            io::printf("  Has title: %s vs %s\n", bool_to_string(has_title) ?? (@catchof(has_title)).nameof, has_title ?? false);
        };
        dynamic_arena.reset();
    }
    dynamic_arena.free();
}
