module fannkuch;
import std::mem;

macro int max(int a, int b)
{
    return a > b ? a : b;
}

func int fannkuchredux(int n)
{
    int* perm = mem::alloc($sizeof(int), n);
    int* perm1 = mem::alloc($sizeof(int), n);
    int* count = mem::alloc($sizeof(int), n);
    int maxFlipsCount;
    int permCount;
    int checksum;

    for (int i = 0; i < n; i++) perm1[i] = i;

    int r = n;

    while (1)
    {
        for (; r != 1; r--) count[r - 1] = r;

        for (int i = 0; i < n; i++) perm[i] = perm1[i];

        int flipsCount = 0;
        int k;

        while (!((k = perm[0]) == 0))
        {
            int k2 = (k + 1) >> 1;
            for (int i = 0; i < k2; i++)
            {
                int temp = perm[i];
                perm[i] = perm[k - i];
                perm[k - i] = temp;
            }
            flipsCount++;
        }

        maxFlipsCount = @max(maxFlipsCount, flipsCount);
        checksum += permCount % 2 == 0 ? flipsCount : -flipsCount;

        /* Use incremental change to generate another permutation */
        while (1)
        {
            if (r == n)
            {
                printf("%d\n", checksum);
                return maxFlipsCount;
            }

            int perm0 = perm1[0];
            int i = 0;
            while (i < r)
            {
                int j = i + 1;
                perm1[i] = perm1[j];
                i = j;
            }
            perm1[r] = perm0;
            count[r] = count[r] - 1;
            if (count[r] > 0) break;
            r++;
        }
        permCount++;
    }
    return 0;
}

extern func int atoi(char *s);
extern func int printf(char *s, ...);

func int main(int argc, char** argv)
{
    int n = argc > 1 ? atoi(argv[1]) : 7;
    printf("Pfannkuchen(%d) = %d\n", n, fannkuchredux(n));
    return 0;
}