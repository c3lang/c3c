// TODO string str = "hello";
//char* str2 = "hello";

//char[] str3 = "hello";

func void test2()
{
    int[2] a = { 1, 2 };

    int[2] b = 30;   // #error: Cannot implicitly cast 'compint' to 'int[2]'
    int[2] c = a;
}

int[2] a1 = { 1, 2 };

int[2] a2 = 30;   // #error: Cannot implicitly cast 'compint' to 'int[2]'
// TODO int[2] a3 = a1;

// i8[] a;   // @error{definition of variable with array type needs an explicit size or an initializer}

char ca = 0;
char cb = 1;

char cc = 127;
char cd = -128;

char ce = 128;       // #error: '128' does not fit
char cf = -129;      // #error: '-129' does not fit

char cg = 70000;     // #error: '70000' does not fit
char ch = -70000;    // #error: '-70000' does not fit