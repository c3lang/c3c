fn int main()
{
    int x;
    double df2 = ($Typeof(double))(x); // #error:  You cannot cast 'int' to 'typeid'
    return 0;
}