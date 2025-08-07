macro int main2()
{
    (fn void (*arg) { // #error: Expected a parameter
    })(&&true);
}