module module6;

extern fn int rand();
extern fn void* malloc(usz);
extern fn void free(void*);
extern fn int printf(char*, ...);

macro @inc(#x)
{
    #x = #x + 1;
}

macro mix(a, b)
{
    return ((a * 31) ^ (b + 7));
}

macro clamp(v, lo, hi)
{
    if (v < lo)
    {
        v = lo;
    }
    else if (v > hi)
    {
        v = hi;
    }
    return v;
}

macro uint hash_block(ptr, len, out)
{
    int i = 0;
    uint h = 2166136261u;

    for (i = 0; i < len; i++)
    {
        h ^= ptr[i];

        if ((i & 3) == 0)
        {
            h *= 16777619u;
        }
        else
        {
            h += i;
        }
    }

    return (uint)h;
}

fn int t1(int a) { return a + 1; }
fn int t2(int a) { return a * 2; }
fn int t3(int a) { return mix(a, 3); }
fn int t4(int a) { @inc(a); return a; }
fn int t5(int a) { return a - 7; }
fn int t6(int a) { return a ^ 0x55; }
fn int t7(int a) { return a & 0xFF; }
fn int t8(int a) { return (a << 1) | 1; }

fn int s1(int x)
{
    int i;
    for (i = 0; i < 10; i++)
    {
        x += i;
    }
    return x;
}

fn int s2(int x)
{
    if (x & 1)
    {
        x *= 3;
    }
    else
    {
        x /= 2;
    }
    return x;
}

fn int s3(int x)
{
    switch (x % 4)
    {
        case 0: return x + 10;
        case 1: return x + 20;
        case 2: return x + 30;
        default: return x + 40;
    }
}

fn int s4(int x)
{
    x = clamp(x, 0, 100);
    return x;
}

fn int s5(int x)
{
    int r = rand();
    return mix(x, r);
}

fn int s6(int x)
{
    int i;
    for (i = 0; i < 5; i++)
    {
        @inc(x);
    }
    return x;
}

fn int m1(int n)
{
    int i;
    int total = 0;

    for (i = 0; i < n; i++)
    {
        int v = rand();

        if (v & 1)
        {
            total += t1(v);
        }
        else
        {
            total += s1(v);
        }

        total ^= s3(v);
    }

    return total;
}

fn int m2(int n)
{
    int* mem = (int*)malloc(n * int.sizeof);
    int i;
    int total = 0;

    for (i = 0; i < n; i++)
    {
        mem[i] = rand();
    }

    int hash = hash_block(mem, n, 0);

    total += hash;

    free(mem);
    return total;
}

fn int m3(int n)
{
    int i;
    int acc = 0;

    for (i = 0; i < n; i++)
    {
        acc += s2(i);
        acc ^= s3(i);
        acc += t5(i);
    }

    return acc;
}

fn int m4(int n)
{
    int i = 0;
    int total = 0;

    while (i < n)
    {
        total += mix(i, total);

        if (i % 3 == 0)
        {
            total += s6(i);
        }

        i++;
    }

    return total;
}

fn int large1(int n)
{
    int i, j;
    int total = 0;

    for (i = 0; i < n; i++)
    {
        for (j = 0; j < 10; j++)
        {
            int v = rand();

            switch ((v + j) % 5)
            {
                case 0:
                    total += m1(5);
                    break;

                case 1:
                    total += m3(7);
                    break;

                case 2:
                    total ^= s2(v);
                    break;

                case 3:
                    total += t7(v);
                    break;

                default:
                    total += v;
                    break;
            }

            if (total & 1)
            {
                total += mix(i, j);
            }
            else
            {
                total ^= mix(j, i);
            }
        }
    }

    return total;
}

fn int large2(int n)
{
    int i;
    int total = 0;

    for (i = 0; i < n; i++)
    {
        total += large1(3);

        if (i % 2)
        {
            total += m4(10);
        }
        else
        {
            total ^= m2(16);
        }

        total = clamp(total, -1000000, 1000000);
    }

    return total;
}

fn int start()
{
    return large2(5);
}