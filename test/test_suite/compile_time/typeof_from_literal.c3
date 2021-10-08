module foo;

func void a()
{
    $typeof(9146744073709551615i64) ef;
    int fffx = ef; // #error: 'long' to 'int'
}

func void b()
{
    $typeof(9223372036854775809u64) ef;
    int fffx = ef; // #error: 'ulong' to 'int'
}

