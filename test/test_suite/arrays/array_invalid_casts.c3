
func void test()
{
    int[3] x;
    double *y = &x; // #error: 'int[3]*' to 'double*
}

func void test2()
{
    int[3] x;
    double[] z = &x; // #error: 'int[3]*' into 'double[]'
}