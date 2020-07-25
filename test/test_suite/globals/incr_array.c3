// #skip

int[+] a;

a += 10;

public func int main()
{
    a += 20;    // @error{cannot add values to incremental array in function scope}
    return 0;
}
/*
const int[+] A;
A += 10;
A += 20;
A += 30;
*/

int[+] b;

func void test1()
{
    int[+] d;     // @error{incremental arrays not allowed in function scope}
}

int[+] c;

func void test2()
{
    b += 10;        // @error{cannot add values to incremental array in function scope}
}

int[+] x = {};    // @error{incremental array cannot have initializer}


int g;
char[2] h = { 1, 2 };

g += 10;    // @error{'a' is not an incremental array}

xyz += 20;    // @error{module test has no symbol b}

h += 30;    // @error{'d' is not an incremental array}

test2 += 20; // @error{'main' is not an incremental array}

int[+] i;

i += xyz; // @error{use of undeclared identifier c}

a += test2;  // @error{invalid type conversion from 'i32 ()' to 'i32'}

int[+] k;

k += 1;
k += 2;
k += 3;

public func void test3()
{
    int c = a;        // @error{invalid type conversion from 'i32[3]' to 'i32'}
}

struct Point
{
    int x;
    int y;
}

Point[+] points;

points += { 10, 11 };
points += { 20, main };      // @error{invalid type conversion from 'i32 ()' to 'i32'}
points += { 30, 31 };

