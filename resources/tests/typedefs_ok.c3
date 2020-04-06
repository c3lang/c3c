module typedefs;

// Standard case
typedef int as Foo;

// Nested resolution
typedef AType as BType;
typedef int as AType;

enum Bar : BType
{
    A,
    B
}
