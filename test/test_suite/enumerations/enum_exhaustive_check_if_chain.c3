enum Baz
{
    A, B, C,
}
fn void test_missing_all_cases(Baz x)
{
    switch (x)
    {
        case x:
    }
}
fn int main() => 0;