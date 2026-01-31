enum MyEnum : const
{
    VAL1 = VAL3, // #error: Unable to properly resolve enum constant value, this can sometimes happen in recursive definitions
    VAL2 ,
    VAL3
}

fn int main()
{
    return 0;
}