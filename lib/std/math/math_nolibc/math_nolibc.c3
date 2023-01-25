module std::math::nolibc;

const double TOINT = 1 / math::DOUBLE_EPSILON;
const float TOINTF = (float)(1 / math::FLOAT_EPSILON);

macro @force_eval_add(&x, v)
{
	@volatile_store(x, @volatile_load(x) + v);
}