module demo1;

// Use C functions directly.
extern fn int printf(char *, ...);
extern fn void puts(char *);

struct Foo
{
    int a;
    int b;
}

fn Foo createFoo(int a, int b = 10)
{
    // Compound initializer
    return Foo {a, b};
}

struct Bar
{
    int x;
    int y;
}


fn void testStruct()
{
    // Default arguments
    Foo foo = createFoo(100);
    printf("Foo a:%d b:%d\n", foo.a, foo.b);

    // No need to write foo = (Foo){ 11, 22 }
    // unlike C
    foo = { 11, 12 };

    // Named arguments
    foo = createFoo(a: 14, b: 13);
    printf("Foo a:%d b:%d\n", foo.a, foo.b);

    Bar bar = bitcast(foo, Bar);
    printf("Bar x:%d y:%d\n", bar.x, bar.y);

    // Type functions
    printf("Mult: %d\n", foo.mult());
    printf("Add: %d\n", bar.add());

}

fn int Bar.add(Bar* b)
{
    return b.x + b.y;
}

fn int Foo.mult(Foo* f)
{
    return f.a * f.b;
}

fn void printArray(int[] array)
{
    printf("[");
    foreach (i, a : array)
    {
        if (i != 0) printf(", ");
        printf("%d", a);
    }
    printf("]\n");
}


fn void testArrays()
{
    int[5] x = { [0] = 100, [1..2] = 4 };
    puts("Testing arrays---");
    foreach (a : x)
    {
        printf("%d\n", a);
    }
    puts("Arrays with index---");
    foreach (i, a : x)
    {
        printf("index[%d]: %d\n", i, a);
    }
    puts("Mutating arrays---");
    foreach (i, int* &a : x) { *a += (int)(i); }
    foreach (i, a : x)
    {
        printf("index[%d]: %d\n", i, a);
    }
    puts("Without & there is no change---");
    foreach (i, a : x) { a += (int)(i); }
    foreach (i, a : x)
    {
        printf("index[%d]: %d\n", i, a);
    }
    printf("Length is a compile time value: %d\n", x.len);
    puts("Getting a slice---");
    int[] y = x[1..3];
    foreach (i, a : y)
    {
        printf("index[%d]: %d\n", i, a);
    }
    printf("Length is a runtime value: %d\n", y.len);
    puts("Getting a slice from the beginning to an offset of the end---");
    y = x[..^2]; // Same as x[0..^2]
    foreach (i, a : y)
    {
        printf("index[%d]: %d\n", i, a);
    }
    puts("Printing arrays---");
    printArray(&x);
    printArray(y);
    printArray(x[1..]);
    puts("Array to pointer to slice---");
    int* z = &x;
    printArray(z[0..2]);
    printf("Pointer to array: %p\nPointer to slice: %p\nPointer to first element of slice: %p\n", z, &y, &y[0]);
}

fn void testExpressionBlocks()
{
    int x = {|
        int j = 0;
        for (int i = 0; i < 10; i++)
        {
           j += i + j;
           if (j > 10) return j;
        }
        return 0;
    |};
    printf("x: %d\n", x);
    {|
       for (int i = 0; i < 10; i++)
       {
         printf("%d\n", i);
         if (i && i % 4 == 0) return;
       }
       puts("Reached end");
    |};
    if ({| for (int i = 0; i < 10; i++) printf("--%d\n", i); puts("Done!"); return true; |})
    {
        puts("In if");
    }
}

fn void main()
{
    testStruct();
    testArrays();
    testExpressionBlocks();
}
