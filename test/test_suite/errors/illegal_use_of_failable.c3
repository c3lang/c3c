func void syntaxErrors()
{
    int! i = 0;
    while (i + 1) {} // #error: 'int!' to 'bool'
    if (i + 1) {} // #error: 'int!' to 'bool'
    for (int x = i;;) {} // #error: 'int!' to 'int'
    for (int x = 0; x < i + 1;) {} // #error: A failable 'bool!' cannot be implicitly converted to a regular boolean
    for (int x = 0; x < 10; x += i + 1) {} // #error: Cannot assign a failable value to a non-failable
    switch (i + 1) // #error: 'int!' to 'int'
    {
        default:
            i + 1;
    }
}
