fn void syntaxErrors()
{
    int! i = 0;
    while (i + 1) {} // #error: The expression may not be a failable, but was 'int!'
    if (i + 1) {} // #error: The expression may not be a failable, but was 'int!'
    for (int x = i;;) {} // #error: 'int!' to 'int'
    for (int x = 0; x < i + 1;) {} // #error: The expression may not be a failable, but was 'bool!'
    for (int x = 0; x < 10; x += i + 1) {} // #error: Cannot assign a failable value to a non-failable
    switch (i + 1) // #error: The expression may not be a failable, but was 'int!'
    {
        default:
            i + 1;
    }
}
