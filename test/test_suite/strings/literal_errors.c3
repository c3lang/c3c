char bar = '\xaf';
char bar = '\x0F';

char bar = '\xgh'; // #error: Expected a two character

char baz = '\ueeof'; // #error: Expected a four char
char eofk = '\u233'; // #error: Expected a four char

char zab = '\Uaokdokok'; // #error: Expected an eight
char zab = '\Uaokdooekfoekfekfkeofkekok'; // #error: Expected an eight
char eofk = '\UaUfko'; // #error: Expected an eight

char foo = ' // #error: The character literal did not terminate

