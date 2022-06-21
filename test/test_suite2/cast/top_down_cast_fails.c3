fn void test()
{
    int x;
    int y;
    long z = x * y;
    z = x * y + z; // #error: 'int' to 'long'
    z = x * y + x; // #error: 'int' to 'long'
    z = x / y;
    z = x + y;
    z = x - y;
    z = x % y;
    z = x / y + z; // #error: 'int' to 'long'
    z = x + y + z; // #error: 'int' to 'long'
    z = x - y + z; // #error: 'int' to 'long'
    z = x % y + z; // #error: 'int' to 'long'
    z = x / y + x; // #error: 'int' to 'long'
    z = x + y + x; // #error: 'int' to 'long'
    z = x - y + x; // #error: 'int' to 'long'
    z = x % y + x; // #error: 'int' to 'long'

    z = x << y + x; // #error: 'int' to 'long'
    z = x >> y + x; // #error: 'int' to 'long'
    z = x << y + z; // #error: 'int' to 'long'
    z = x >> y + z; // #error: 'int' to 'long'
    z = ~x + x; // #error: 'int' to 'long'
    z = ~x + z; // #error: 'int' to 'long'
    z = -x + z; // #error: 'int' to 'long'
    z = x + z;
}