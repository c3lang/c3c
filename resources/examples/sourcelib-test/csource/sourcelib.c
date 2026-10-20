#include "sourcelib.h"
#include "bias.h"

int sourcelib_c_add(int a, int b)
{
	return a + b + SOURCELIB_BIAS;
}
