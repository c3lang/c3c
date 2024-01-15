module spectralnorm;
import std::io;
import std::math;

double[] temparr;

fn double eval_A(int i, int j)
{
	return 1.0 / (double)((i + j) * (i + j + 1) / 2 + i + 1);
}

fn void eval_A_times_u(double[] u, double[] au, double[] x)
{
	int len = au.len;
	foreach (i, &val : au)
	{
		*val = 0;
		foreach (j, uval : u)
		{
			*val += x[i * len + j] * uval;
		}
	}
}

fn void eval_At_times_u(double[] u, double[] au, double[] x)
{
	int len = au.len;
	foreach (i, &val : au)
	{
		*val = 0;
		foreach (j, uval : u)
		{
			*val += x[i * len + j] * uval;
		}
	}
}

fn void eval_AtA_times_u(double[] u, double[] atau, double[] x)
{
	eval_A_times_u(u, temparr, x);
	eval_At_times_u(temparr, atau, x);
}

fn void main(String[] args)
{
	int n = args.len == 2 ? args[1].to_int()!! : 2000;
	temparr = mem::alloc_array(double, n);
	double[] u = mem::alloc_array(double, n);
	double[] v = mem::alloc_array(double, n);
	double[] x = mem::alloc_array(double, (usz)(n * n));
	for (int i = 0; i < n; i++)
	{
		for (int j = 0; j < n; j++)
		{
			x[i * n + j] = eval_A(i, j);
		}
	}
	u[..] = 1;
	for (int i = 0; i < 10; i++)
	{
		eval_AtA_times_u(u, v, x);
		eval_AtA_times_u(v, u, x);
	}
	double vBv;
	double vv;
	foreach (i, vval : v)
	{
		vBv += u[i] * vval;
		vv += vval * vval;
	}
	io::printf("%0.9f\n", math::sqrt(vBv / vv));
}
