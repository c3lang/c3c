module test @test;

fn void pointer_non_decay()
{
    int[6] x;
    int[3]* y = (int[3]*)&x;
    int* z = y;
    int[] sub = y;
    int[3] y1 = y[1];
    int z1 = z[1];
    int* xx = y + 1;
    int[3]* yy = (int[3]*)(xx);
    int* zz = yy - 1;
    assert(y == z);
    assert(z == zz);
    assert(&(*y)[1] == &xx[-2]);
    x[1] = 123;
    assert(x[1] == z[1]);
}