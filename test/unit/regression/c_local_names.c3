module c_local_names;
import libc, std::math;

// The C backend names locals after C3's; none may hide a C name the function uses.
int dgemm_ @cname("c_local_names_dgemm_") = 5;
int global_ @cname("global_") = 7;

fn int with_globals(int dgemm, int global)
{
	return dgemm + dgemm_ + global + global_;
}

fn void locals_keep_c_names_visible() @test
{
	assert(with_globals(1, 2) == 15);
	usz strlen = 3;
	assert(libc::strlen("abc") == strlen);
	double exp = 1.0;
	assert(math::exp(exp) > 2.7);
}
