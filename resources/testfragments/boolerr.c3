module test;
import libc;

struct Doc { Head *head; }
struct Head { char[]* title; }

struct Summary
{
	char[]* title;
	bool ok;
}

fn void Summary.print(Summary *s, CFile out)
{
	// We don't have a native printf in C3 yet, so use libc,
	// which is not all that nice for the strings but...
	char[] title = s.title ? *s.title : "missing";
	libc::fprintf(out, "Summary({ .title = %.*s, .ok = %s})", (int)title.len, title.ptr, s.ok ? "true" : "false");
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

macro dupe(value)
{
	$typeof(&value) temp = mem::alloc_checked($sizeof(value))?;
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
	if (contains(url, "title-missing")) return { dupe(Head { .title = null })? };
	if (contains(url, "title-empty")) return { dupe(Head { .title = dupe((char[])"")? })? };
	// Not particularly elegant due to missing string functions.
	int len = libc::snprintf(null, 0, "Title of %.*s", (int)url.len, url.ptr);
	char* str = mem::alloc_checked(len + 1)?;
	libc::snprintf(str, len + 1, "Title of %.*s", (int)url.len, url.ptr);
	return { dupe(Head { .title = dupe(str[..len - 1])? })? };
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
	char[]* head = doc.head.title;
	if (!head) return TitleResult.TITLE_MISSING!;
	return (*head).len > 0;
}


fn bool! readWhetherTitleNonEmpty(char[] url)
{
    return isTitleNonEmpty(readDoc(url));
}

fn char* bool_to_string(bool b)
{
	return b ? "true" : "false";
}


fn void main()
{
    const char[][] URLS = { "good", "title-empty", "title-missing", "head-missing", "fail" };
    DynamicArenaAllocator dynamic_arena;
	dynamic_arena.init(1024);
    Allocator allocator = mem::dynamic_arena_allocator(&dynamic_arena);
    foreach (char[] url : URLS)
    {
        mem::@with_allocator(allocator)
        {
	        // Yes, it's pretty onerous to print strings for the moment in C3
            libc::printf(`Checking "https://%.*s/":` "\n", (int)url.len, url.ptr);
            Summary summary = readAndBuildSummary(url);
            libc::printf("  Summary: ");
            summary.print(libc::stdout());
            libc::printf("\n");
            char[] title_sure = summary.title ? *summary.title : "";
            libc::printf("  Title: %.*s\n", (int)title_sure.len, title_sure.ptr);
            bool! has_title = readWhetherTitleNonEmpty(url);
            // This looks a bit less than elegant, but as you see it's mostly due to having to
            // use printf here.
            libc::printf("  Has title: %s vs %s\n", bool_to_string(has_title) ?? catch(has_title).nameof, (has_title ?? false) ? "true" : "false");
        };
        allocator.reset();
    }
    dynamic_arena.destroy();
}
