// TODO string str = "hello";
//char* str2 = "hello";

//char[] str3 = "hello";


int[2] a1 = { 1, 2 };

int[2] a2 = 30;   // #error: Cannot implicitly cast 'compint' to 'int[2]'
// TODO int[2] a3 = a1;

// i8[] a;   // @error{definition of variable with array type needs an explicit size or an initializer}

ichar ca = 0;
ichar cb = 1;

ichar cc = 127;
ichar cd = -128;

ichar ce = 128;       // #error: '128' does not fit
ichar cf = -129;      // #error: '-129' does not fit

ichar cg = 70000;     // #error: '70000' does not fit
ichar ch = -70000;    // #error: '-70000' does not fit