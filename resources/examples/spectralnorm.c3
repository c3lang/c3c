module spectralnorm;
import std::mem;
import std::array;

extern fn int atoi(char *s);
extern fn int printf(char *s, ...);
extern fn double sqrt(double);

double[] temparr;

fn double eval_A(int i, int j)
{
	return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1);
}

fn void eval_A_times_u(double[] u, double[] au)
{
	foreach (i, &val : au)
	{
		*val = 0;
		foreach (j, uval : u)
		{
			*val += eval_A((int)(i), (int)(j)) * uval;
		}
	}
}

fn void eval_At_times_u(double[] u, double[] au)
{
	foreach (i, &val : au)
	{
		*val = 0;
		foreach (j, uval : u)
		{
			*val += eval_A((int)(j), (int)(i)) * uval;
		}
	}
}

fn void eval_AtA_times_u(double[] u, double[] atau) @noinline
{
	eval_A_times_u(u, temparr);
	eval_At_times_u(temparr, atau);
}

fn int main(int argc, char **argv)
{
	int n = (argc == 2) ? atoi(argv[1]) : 2000;
	temparr = array::alloc(double, n);
	double[] u = array::alloc(double, n);
	double[] v = array::alloc(double, n);
	foreach(&uval : u) *uval = 1;
	for (int i = 0; i < 10; i++)
	{
		eval_AtA_times_u(u, v);
		eval_AtA_times_u(v, u);
	}
	double vBv;
	double vv;
	foreach (i, vval : v)
	{
		vBv += u[i] * vval;
		vv += vval * vval;
	}
	printf("%0.9f\n", sqrt(vBv / vv));
	return 0;
}
