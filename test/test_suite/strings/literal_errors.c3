// #target: x64-darwin

char bar = '\xaf';
char bar = '\x0F';

char bar = '\xgh'; // #error: Expected a two character

char baz = '\ueeof'; // #error: Expected a four char
char eofk = '\u233'; // #error: Expected a four char

char zab = '\Uaokdokok'; // #error: Expected an eight
char zab = '\Uaokdooekfoekfekfkeofkekok'; // #error: Expected an eight
char eofkq = '\UaUfko'; // #error: Expected an eight

char x1 = '\u0023a'; // #error: Character literals with '\u' can only contain one character
char x2 = '\U00000023a'; // #error: Character literals with '\U' can only contain one character

char x = 'äö'; // #error: may only contain one character
char feokf = '\9'; // #error: Invalid escape sequence '\9'
char fje = '123456789012345678'; // #error: The character literal exceeds 16 characters.

char foekfe = ''; // #error: The character literal was empty