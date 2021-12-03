0o8 // #error: An expression starting with '0o' should be followed by octal numbers (0-7).

0o% // #error: An expression starting with '0o' should be followed by octal numbers (0-7).

0b2 // #error: An expression starting with '0b' should be followed by binary digits

0b# // #error: An expression starting with '0b' should be followed by binary digits

0xg // #error: '0x' starts a hexadecimal number, so the next character should be 0-9, a-f or A-F.

0x! // #error: '0x' starts a hexadecimal number, so the next character should be 0-9, a-f or A-F.

0b // #error: An expression starting with '0b' should be followed by binary digits
