enum Foo : char
{
    MY_VAL1 = 255,
    MY_VAL2, // #error: The enum value would implicitly be 256 which does not fit in 'char'
}