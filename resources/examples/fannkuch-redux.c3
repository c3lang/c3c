module fannkuch;
import std::io;
import std::math;
import libc;

fn int fannkuchredux(int n)
{
	int* perm = mem::alloc_array(int, n);
	int* perm1 = mem::alloc_array(int, n);
	int* count = mem::alloc_array(int, n);
	int max_flips_count;
	int perm_count;
	int checksum;

	for (int i = 0; i < n; i++) perm1[i] = i;

	int r = n;

	while (1)
	{
		for (; r != 1; r--) count[r - 1] = r;

		for (int i = 0; i < n; i++) perm[i] = perm1[i];

		int flips_count = 0;
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
			flips_count++;
		}

		max_flips_count = max(max_flips_count, flips_count);
		checksum += perm_count % 2 == 0 ? flips_count : -flips_count;

		/* Use incremental change to generate another permutation */
		while (1)
		{
			if (r == n)
			{
				io::printf("%d\n", checksum);
				return max_flips_count;
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
		perm_count++;
	}
}

fn int main(int argc, char** argv)
{
	int n = argc > 1 ? libc::atoi(argv[1]) : 7;
	io::printf("Pfannkuchen(%d) = %d\n", n, fannkuchredux(n));
	return 0;
}