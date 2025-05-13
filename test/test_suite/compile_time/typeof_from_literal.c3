module foo;

fn void a()
{
    $typeof(9146744073709551615L) ef;
    int fffx = ef; // #error: 'long'
}

fn void b()
{
    $typeof(9223372036854775809UL) ef;
    int fffx = ef; // #error: 'ulong'
}

