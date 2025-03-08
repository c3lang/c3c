ushort[?] a = $$wstr16("γειά ok \U0001F600"); // #error: Invalid codepoint in string: a 16-bit wide-string cannot accommodate codepoints over U+FFFF.
ushort[?] b = $$wstr16("\U00121F60"); // #error: Invalid codepoint in string: a 16-bit wide-string cannot accommodate codepoints over U+FFFF.
