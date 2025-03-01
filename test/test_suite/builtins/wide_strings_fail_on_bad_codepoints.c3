ushort[] b = $$wstr16("😀😀😀"); // #error: Invalid codepoint in string: a 16-bit wide-string cannot accommodate codepoints over U+FFFF.
ushort[] b = $$wstr16("ascii 𝄞 around"); // #error: Invalid codepoint in string: a 16-bit wide-string cannot accommodate codepoints over U+FFFF.
