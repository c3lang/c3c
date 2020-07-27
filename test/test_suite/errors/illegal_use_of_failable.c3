func void syntaxErrors()
{
    int! i = 0;
    while (i + 1) {} // #error: 'int!' cannot be converted into 'bool'
    if (i + 1) {} // #error: 'int!' cannot be converted into 'bool'
    for (int x = i;;) {} // #error: 'int!' cannot be implicitly cast to 'int'.
    for (int x = 0; x < i + 1;) {} // #error: 'bool!' cannot be implicitly cast to 'bool'
    for (int x = 0; x < 10; x += i + 1) {} // #error: 'int!' cannot be implicitly cast to 'int'
    switch (i + 1) // #error: 'int!' cannot be converted into 'int'
    {
        default:
            i + 1;
    }
}
