fn int[*] hello() // #error: Inferred array types can only be used in declarations with initializers
{
    return int[3] { 1, 2, 3};
}

int[*] c; // #error: Inferred array types can only be used in declarations with initializers

