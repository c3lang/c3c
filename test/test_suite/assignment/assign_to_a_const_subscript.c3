fn void test(int , usz n)
{
    bool y = { true, false, }[n] = 33; // #error: You cannot assign to a constant expression.
}
fn int main() => 0;