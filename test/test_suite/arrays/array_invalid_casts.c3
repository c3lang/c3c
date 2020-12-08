
func void test()
{
    int[3] x;
    double *y = &x; // #error: Cannot implicitly cast 'int[3]*' to 'double*
}

func void test2()
{
    int[3] x;
    double[] z = &x; // #error: Cannot cast 'int[3]*' to 'double[]'
}