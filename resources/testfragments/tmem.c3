module tmem;
import std::mem;
import std::io;

struct String
{
    Allocator allocator;
    usize len;
    usize capacity;
    char* ptr;
}

fn void String.init(String *s, char[] c)
{
    s.capacity = c.len + 16;
    s.ptr = mem::_malloc(s.capacity);
    s.len = c.len;
    mem::copy(s.ptr, (char*)(c), c.len);
}

fn char* String.zstr(String *s)
{
    char* c = mem::_malloc(s.len + 1);
    mem::copy(c, s.ptr, s.len);
    c[s.len] = 0;
    return c;
}

fn void String.appendc(String *s, char c)
{
    if (s.capacity == s.len)
    {
        s.capacity *= 2;
        char* new_ptr = mem::_malloc(s.capacity);
        mem::copy(new_ptr, s.ptr, s.len);
        s.ptr = new_ptr;
    }
    s.ptr[s.len++] = c;
}

fn void String.append(String *s, char[] other_string)
{
    if (s.capacity < s.len + other_string.len)
    {
        do
        {
            s.capacity *= 2;
        }
        while (s.capacity < s.len + other_string.len);
        char* new_ptr = mem::_malloc(s.capacity);
        mem::copy(new_ptr, s.ptr, s.len);
        s.ptr = new_ptr;
    }
    mem::copy(s.ptr + s.len, (char*)(other_string), other_string.len);
    s.len += other_string.len;
}

fn void String.concat(String *s, String* other_string)
{
    if (s.capacity < s.len + other_string.len)
    {
        do
        {
            s.capacity *= 2;
        }
        while (s.capacity < s.len + other_string.len);
        char* new_ptr = mem::_malloc(s.capacity);
        mem::copy(new_ptr, s.ptr, s.len);
        s.ptr = new_ptr;
    }
    mem::copy(s.ptr + s.len, other_string.ptr, other_string.len);
    s.len += other_string.len;
}

fn void main()
{
    String s;
    s.init("Hello");
    s.appendc(' ');
    s.appendc('W');
    s.append("orld!");
    String w;
    w.init("Yo man!");
    s.concat(&w);
    io::printf("Message was: %s\n", s.zstr());
}