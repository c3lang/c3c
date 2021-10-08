// TODO string str = "hello";
char* str2 = "hello";
char[] str3 = "hello";


int[2] a1 = { 1, 2 };

int[2] a2 = 30;   // #error: 'int' into 'int[2]'
// TODO int[2] a3 = a1;

ichar[*] a; // #error: Inferred array types can only be used in declarations with initializers

ichar ca = 0;
ichar cb = 1;

ichar cc = 127;
ichar cd = -128;

ichar ce = 128;       // #error: The value '128' is out of range for 'ichar'
ichar cf = -129;      // #error: The value '-129' is out of range for 'ichar'

ichar cg = 70000;     // #error: The value '70000' is out of range for 'ichar'
ichar ch = -70000;    // #error: The value '-70000' is out of range for 'ichar'