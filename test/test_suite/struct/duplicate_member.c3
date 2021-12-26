// @warnings{no-unused}
module test;

struct Aa
{
    int a;
    int a;    // #error: Duplicate member name 'a'
}

struct Bb
{
    int a;
    struct a // #error: Duplicate member name 'a'
    {
        int b;
    }
}

union Cc
{
    int a;
    int a;    // #error: Duplicate member name 'a'

    struct b
    {
        int c;
        int c;    // #error: Duplicate member name 'c'
    }
}

struct Dd
{
    int b;
    int a;
    struct
    {
        union
        {
            short a;  // #error: Duplicate member name 'a'
            int b;  // #error: Duplicate member name 'b'
        }
    }
}

union Ee
{
    int a;
    struct
    {
        short a;  // #error: Duplicate member name 'a'
    }
}

struct Ff
{
    struct
    {
        int a;
    }
    struct b
    {
        int a;
    }
    union
    {
        int a;  // #error: Duplicate member name 'a'
    }
}

