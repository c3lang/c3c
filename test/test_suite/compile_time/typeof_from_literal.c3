module foo;

func void a()
{
    $typeof(9146744073709551615) ef;
    int fffx = ef; // #error: Cannot implicitly cast 'long' to 'int'
}

func void b()
{
    $typeof(9223372036854775809) ef;
    int fffx = ef; // #error: Cannot implicitly cast 'ulong' to 'int'
}

