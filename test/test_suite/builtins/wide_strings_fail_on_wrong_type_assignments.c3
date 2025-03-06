char[*] a = $$wstr16(`short str`); // #error: It is not possible to cast 'ushort[10]' to 'char[*]'.
uint[*] b = $$wstr16("str"); // #error: It is not possible to cast 'ushort[4]' to 'uint[*]'.
