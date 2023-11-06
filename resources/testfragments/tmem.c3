module tmem;
import std::io;
import libc;

struct VarString
{
    usz len;
    usz capacity;
    char* ptr;
}

fn void VarString.init(VarString *s, String c)
{
    s.capacity = c.len + 16;
    s.ptr = malloc(s.capacity);
    s.len = c.len;
    mem::copy(s.ptr, (char*)(c), c.len);
}

fn char* VarString.zstr(VarString *s)
{
    char* c = malloc(s.len + 1);
    mem::copy(c, s.ptr, s.len);
    c[s.len] = 0;
    return c;
}

fn void VarString.appendc(VarString *s, char c)
{
    if (s.capacity == s.len)
    {
        s.capacity *= 2;
        char* new_ptr = malloc(s.capacity);
        mem::copy(new_ptr, s.ptr, s.len);
        s.ptr = new_ptr;
    }
    s.ptr[s.len++] = c;
}

fn void VarString.append(VarString *s, String other_string)
{
    if (s.capacity < s.len + other_string.len)
    {
        do
        {
            s.capacity *= 2;
        }
        while (s.capacity < s.len + other_string.len);
        char* new_ptr = malloc(s.capacity);
        mem::copy(new_ptr, s.ptr, s.len);
        s.ptr = new_ptr;
    }
    mem::copy(s.ptr + s.len, (char*)(other_string), other_string.len);
    s.len += other_string.len;
}

fn void VarString.concat(VarString *s, VarString* other_string)
{
    if (s.capacity < s.len + other_string.len)
    {
        do
        {
            s.capacity *= 2;
        }
        while (s.capacity < s.len + other_string.len);
        char* new_ptr = malloc(s.capacity);
        mem::copy(new_ptr, s.ptr, s.len);
        s.ptr = new_ptr;
    }
    mem::copy(s.ptr + s.len, other_string.ptr, other_string.len);
    s.len += other_string.len;
}

fn void main()
{
    VarString s;
    s.init("Hello");
    s.appendc(' ');
    s.appendc('W');
    s.append("orld!");
    VarString w;
    w.init("Yo man!");
    s.concat(&w);
    libc::printf("Message was: %s\n", s.zstr());
}