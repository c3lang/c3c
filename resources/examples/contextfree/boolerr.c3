module test;
import libc;
import std::io;

struct Doc { Head *head; }
struct Head { String* title; }

struct Summary
{
	String* title;
	bool ok;
}

private struct StringData
{
	Allocator allocator;
	usize len;
	usize capacity;
	char[*] chars;
}

fn void Summary.print(Summary *s, File out)
{
	char[] title = s.title ? s.title.str() : "missing";
	out.printf("Summary({ .title = %s, .ok = %s})", title, s.ok);
}

fn bool contains(char[] haystack, char[] needle)
{
	usize len = haystack.len;
	usize needle_len = needle.len;
	if (len < needle_len) return false;
	if (!needle_len) return true;
	len -= needle_len - 1;
	for (usize i = 0; i < len; i++)
	{
		if (libc::memcmp(&haystack[i], needle.ptr, needle_len) == 0)
		{
			return true;
		}
	}
	return false;
}

macro @dupe(value)
{
	$typeof(&value) temp = malloc_checked($sizeof(value))?;
	*temp = value;
	return temp;
}

fault ReadError
{
	BAD_READ,
}

fn Doc! readDoc(char[] url)
{
	if (contains(url, "fail")) return ReadError.BAD_READ!;
	if (contains(url, "head-missing")) return { .head = null };
	if (contains(url, "title-missing")) return { @dupe(Head { .title = null }) };
	if (contains(url, "title-empty")) return { @dupe(Head { .title = @dupe((String)null) }) };
	String str;
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

fn Summary readAndBuildSummary(char[] url)
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
	if (!doc.head) return TitleResult.TITLE_MISSING!;
	String* head = doc.head.title;
	if (!head) return TitleResult.TITLE_MISSING!;
	return head.len() > 0;
}


fn bool! readWhetherTitleNonEmpty(char[] url)
{
    return isTitleNonEmpty(readDoc(url));
}

fn char[] bool_to_string(bool b)
{
	return b ? "true" : "false";
}


fn void main()
{
    const char[][] URLS = { "good", "title-empty", "title-missing", "head-missing", "fail" };
    DynamicArenaAllocator dynamic_arena;
	dynamic_arena.init(1024);
    foreach (char[] url : URLS)
    {
        mem::@scoped(&dynamic_arena)
        {
            io::printf(`Checking "https://%s/":` "\n", url);
            Summary summary = readAndBuildSummary(url);
            io::printf("  Summary: ");
            summary.print(io::stdout());
            io::println("");
            char[] title_sure = summary.title ? summary.title.str() : "";
            io::printf("  Title: %s\n", title_sure);
            bool! has_title = readWhetherTitleNonEmpty(url);
            // This looks a bit less than elegant, but as you see it's mostly due to having to
            // use printf here.
            io::printf("  Has title: %s vs %s\n", bool_to_string(has_title) ?? catch(has_title).nameof, has_title ?? false);
        };
        dynamic_arena.reset();
    }
    dynamic_arena.destroy();
}
