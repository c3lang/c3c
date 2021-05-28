
func int main()
{
    do FOO:
    {
        while FOO: (1) // #error: would shadow a previous declaration
        {
            return 1;
        }
    }
    return 0;
}


func void test1()
{
    do FOO:
    {
        while (1)
        {
            break BAR; // #error: Cannot find a labelled statement with the name 'BAR'
        }
    }
}