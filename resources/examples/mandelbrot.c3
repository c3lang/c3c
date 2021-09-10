module mandelbrot;

extern func int atoi(char *s);
extern func int printf(char *s, ...);
extern func void putchar(int c);

func void main(int argc, char **argv)
{
	int w = atoi(argv[1]);
	int h = w;

	const LIMIT = 2.0;
	const SQUARE_LIMIT = LIMIT * LIMIT;

	printf("P4\n%d %d\n", w, h);

	int iter = 50;
	int bit_num = 0;
	char byte_acc = 0;
	for (double y = 0; y < h; y++)
	{
		for (double x = 0; x < w; x++)
		{
		    double zr;
		    double zi;
		    double ti;
		    double tr;
			double cr = (2.0 * x / w - 1.5);
			double ci = (2.0 * y / h - 1.0);
			for (int i = 0; i < iter && (tr + ti <= SQUARE_LIMIT); i++)
			{
				zi = 2.0 * zr * zi + ci;
				zr = tr - ti + cr;
				tr = zr * zr;
				ti = zi * zi;
			}

			byte_acc <<= 1;
			if (tr + ti <= SQUARE_LIMIT) byte_acc |= 0x01;

			++bit_num;

			if (bit_num == 8)
			{
				putchar(byte_acc);
				byte_acc = 0;
				bit_num = 0;
			}
			else if (x == w - 1)
			{
				byte_acc <<= (8 - w % 8);
				putchar(byte_acc);
				byte_acc = 0;
				bit_num = 0;
			}
		}
	}
}
