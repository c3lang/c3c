module comparisons;

fn void test_signed()
{
    int a = 0;
    int b = 1;

    bool ab_gt = a > b;
    bool ab_ge = a >= b;
    bool ab_le = a <= b;
    bool ab_lt = a < b;
    bool ab_ne = a != b;
    bool ab_eq = a == b;

}

fn void test_unsigned()
{
    uint a = 0;
    uint b = 1;

    bool ab_gt = a > b;
    bool ab_ge = a >= b;
    bool ab_le = a <= b;
    bool ab_lt = a < b;
    bool ab_ne = a != b;
    bool ab_eq = a == b;

}

extern fn void printf(char *s);

fn void test_signedunsigned()
{
    char a = 0 - 1;
    byte b = (byte)(a);

    printf("Signed-unsigned -1 0xFF \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    a = 1;
    b = 1;

    printf("Signed-unsigned 1 1 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    a = 1;
    b = 4;

    printf("Signed-unsigned 1 4 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    a = 4;
    b = 1;

    printf("Signed-unsigned 4 1 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    a = 4;
    b = 129;

    printf("Signed-unsigned 4 129 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    a = 0 - 4;
    b = 129;

    printf("Signed-unsigned -4 129 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

}

fn void test_unsignedsigned()
{
    int b = -1;
    uint a = (uint)(b);

    printf("Unsigned-signed 0xFFFFFFFF -1 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    a = 1;
    b = 1;

    printf("Unsigned-signed 1 1 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    a = 4;
    b = 1;

    printf("Unsigned-signed 4 1 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    a = 1;
    b = 4;

    printf("Unsigned-signed 1 4 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    a = 0x8000_0001;
    b = 4;

    printf("Unsigned-signed 0x8000_0001 4 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

    b = 0 - 4;
    a = 0x8000_0001;

    printf("Unsigned-signed 0x8000_0001 -4 \n");
    if (a > b) printf("a > b\n");
    if (a >= b) printf("a >= b\n");
    if (a < b) printf("a < b\n");
    if (a <= b) printf("a <= b\n");
    if (a != b) printf("a != b\n");
    if (a == b) printf("a == b\n");

}

fn void main()
{
    test_signedunsigned();
    test_unsignedsigned();
    printf("-- Done ---\n");
}