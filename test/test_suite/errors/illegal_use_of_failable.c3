func void syntaxErrors()
{
    int! i = 0;
    while (i + 1) {} // #error: 'int!' cannot be converted to 'bool'
    if (i + 1) {} // #error: 'int!' cannot be converted to 'bool'
    for (int x = i;;) {} // #error: 'int!' cannot be converted to 'int'.
    for (int x = 0; x < i + 1;) {} // #error: 'bool!' cannot be converted to 'bool'
    for (int x = 0; x < 10; x += i + 1) {} // #error: 'int!' cannot be converted to 'int'
    switch (i + 1) // #error: 'int!' cannot be converted to 'int'
    {
        default:
            i + 1;
    }
}
