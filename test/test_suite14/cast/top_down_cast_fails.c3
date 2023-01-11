fn void test()
{
    int x;
    int y;
    long z = x * y;
    z = x * y + z; // #error: 'long'
    z = x * y + x; // #error: 'long'
    z = x / y;
    z = x + y;
    z = x - y;
    z = x % y;
    z = x / y + z; // #error: 'long'
    z = x + y + z; // #error: 'long'
    z = x - y + z; // #error: 'long'
    z = x % y + z; // #error: 'long'
    z = x / y + x; // #error: 'long'
    z = x + y + x; // #error: 'long'
    z = x - y + x; // #error: 'long'
    z = x % y + x; // #error: 'long'

    z = x << y + x; // #error: 'long'
    z = x >> y + x; // #error: 'long'
    z = x << y + z; // #error: 'long'
    z = x >> y + z; // #error: 'long'
    z = ~x + x; // #error: 'long'
    z = ~x + z; // #error: 'long'
    z = -x + z; // #error: 'long'
    z = x + z;
}