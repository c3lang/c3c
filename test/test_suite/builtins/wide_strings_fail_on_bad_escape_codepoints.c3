ushort[?] a = $$wstr16("γειά ok \U0001F600"); // #error: Escape sequence produced a codepoint too large for a 16-bit string (9 bytes).
ushort[?] b = $$wstr16("\U00121F600"); // #error: Escape sequence produced a codepoint too large for a 16-bit string (9 bytes).
ushort[?] b = $$wstr16("\U0000000A"); // #error: Escape sequence produced a codepoint too large for a 16-bit string (9 bytes).
