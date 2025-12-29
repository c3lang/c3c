import std;


macro transpose4x4_mat(a, b, c, d)
{
    Matrix4x4{float} mat;

    mat.m[0..3] = *a[..]; // #error: Omitting the end index is not allowed for pointers, did you perhaps forget to dereference the pointer before slicing wih []?
    mat.m[4..7] = *b[..];
    mat.m[8..11] = *c[..];
    mat.m[12..15] = *d[..];

    mat = mat.transpose();

    *a = mat.m[0..3];
    *b = mat.m[4..7];
    *c = mat.m[8..11];
    *d = mat.m[12..15];
}

fn void main()
{
    float[<4>] a = { 0,1,2,3 };
    float[<4>] b = { 4,5,6,7 };
    float[<4>] c = { 8,9,10,11 };
    float[<4>] d = { 12,13,14,15 };

    transpose4x4_mat(&a, &b, &c, &d);

    io::printn(a);
    io::printn(b);
    io::printn(c);
    io::printn(d);
}
