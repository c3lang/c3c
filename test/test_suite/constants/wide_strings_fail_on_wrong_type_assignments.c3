char[?] a = u`short str`; // #error: It is not possible to cast 'ushort[20]' to 'char[?]'.
uint[?] b = u"str"; // #error: It is not possible to cast 'ushort[8]' to 'uint[?]'.
// this is apparently permitted -- ushort[?] c = U"str"; // : It is not possible to cast 'uint[8]' to 'ushort[?]'.
