module std::str;
import std::mem;

define ZString = char*;
define String = char[];

fn ZString copy_zstring(String s)
{
    usize len = s.len;
    char* str = mem::alloc(len + 1);
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return str;
}

fn ZString tcopy_zstring(String s)
{
    usize len = s.len;
    char* str = mem::talloc(len + 1)!!;
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return str;
}

fn String copy(String s)
{
    usize len = s.len;
    ZString str_copy = copy_zstring(s) @inline;
    return str_copy[..len];
}

fn String tcopy(String s)
{
    usize len = s.len;
    ZString str_copy = tcopy_zstring(s) @inline;
    return str_copy[..len];
}

fn String tconcat(String s1, String s2)
{
    usize full_len = s1.len + s2.len;
    char* str = mem::talloc(full_len + 1)!!;
    usize s1_len = s1.len;
    mem::copy(str, s1.ptr, s1_len);
    mem::copy(str + s1_len, s2.ptr, s2.len);
    str[full_len] = 0;
    return str[..full_len];
}

fn String concat(String s1, String s2)
{
    usize full_len = s1.len + s2.len;
    char* str = mem::alloc(full_len + 1);
    usize s1_len = s1.len;
    mem::copy(str, s1.ptr, s1_len);
    mem::copy(str + s1_len, s2.ptr, s2.len);
    str[full_len] = 0;
    return str[..full_len];
}