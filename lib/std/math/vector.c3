// Vector supplemental methods
module std::math::vector;
import std::math @public;

alias Vec2 @weak = float[<2>];
alias Vec3 @weak = float[<3>];
alias Vec4 @weak = float[<4>];

<*
 @require $Type::kind == VECTOR &&& @kindof(($Type){}[0]) == BOOL : "Expected a bool vector"
 @require @kindof(mask).is_int() : "Expected an integer mask"
*>
macro bool[<*>] mask_from_int($Type, mask)
{
	return $$int_to_mask(mask, $Type::len);
}

macro bool[<*>].mask_to_int(self)
{
	return $$mask_to_int(self);
}

fn float[<3>] cubic_hermitef(float[<3>] v1, float[<3>] tangent1, float[<3>] v2, float[<3>] tangent2, float amount) => cubic_hermite3(v1, tangent1, v2, tangent2, amount);
fn void orthonormalizef(float[<3>]* v1, float[<3>]* v2) => ortho_normalize3(v1, v2);

fn double[<3>] cubic_hermite(double[<3>] v1, double[<3>] tangent1, double[<3>] v2, double[<3>] tangent2, double amount) => cubic_hermite3(v1, tangent1, v2, tangent2, amount);
fn void orthonormalize(double[<3>]* v1, double[<3>]* v2) => ortho_normalize3(v1, v2);

fn float float[<2>].cross(self, float[<2>] v2) => cross2(self, v2);
macro float[<2>] float[<2>].transform(self, Matrix4f mat) => transform2(self, mat);
macro float[<2>] float[<2>].rotate(self, float angle) => rotate(self, angle);
macro float[<2>] float[<2>].angle(self, float[<2>] v2) => math::atan2(v2.y, v2.x) - math::atan2(self.y, self.x);

macro double[<2>] double[<2>].transform(self, Matrix4 mat) => transform2(self, mat);
macro double[<2>] double[<2>].rotate(self, double angle) => rotate(self, angle);
macro double[<2>] double[<2>].angle(self, double[<2>] v2) => math::atan2(v2.y, v2.x) - math::atan2(self.y, self.x);
fn double double[<2>].cross(self, double[<2>] v2) => cross2(self, v2);

fn float float[<3>].angle(self, float[<3>] v2) => angle3(self, v2);
fn float[<3>] float[<3>].cross(self, float[<3>] v2) => cross3(self, v2);
fn float[<3>] float[<3>].perpendicular(self) => perpendicular3(self);
fn float[<3>] float[<3>].barycentric(self, float[<3>] a, float[<3>] b, float[<3>] c) => barycentric3(self, a, b, c);
fn float[<3>] float[<3>].transform(self, Matrix4f mat) => transform3(self, mat);
fn float[<3>] float[<3>].refract(self, float[<3>] n, float r) => refract3(self, n, r);
fn float[<3>] float[<3>].rotate_quat(self, Quaternionf q) => q * self;
fn float[<3>] float[<3>].rotate_axis(self, float[<3>] axis, float angle) => rotate_axis_angle(self, axis, angle);
fn float[<3>] float[<3>].rejection(v1, float[<3>] v2) => rejection3(v1, v2);
fn float[<3>] float[<3>].project(self, Matrix4f model, Matrix4f projection, float[<4>] viewport) => project3(self, model, projection, viewport);
fn float[<3>] float[<3>].unproject(self, Matrix4f model, Matrix4f projection, float[<4>] viewport) => unproject3(self, model, projection, viewport);

fn double double[<3>].angle(self, double[<3>] v2) => angle3(self, v2);
fn double[<3>] double[<3>].cross(self, double[<3>] v2) => cross3(self, v2);
fn double[<3>] double[<3>].perpendicular(self) => perpendicular3(self);
fn double[<3>] double[<3>].barycentric(self, double[<3>] a, double[<3>] b, double[<3>] c) => barycentric3(self, a, b, c);
fn double[<3>] double[<3>].transform(self, Matrix4 mat) => transform3(self, mat);
fn double[<3>] double[<3>].refract(self, double[<3>] n, double r) => refract3(self, n, r);
fn double[<3>] double[<3>].rotate_quat(self, Quaternion q) => q * self;
fn double[<3>] double[<3>].rotate_axis(self, double[<3>] axis, double angle) => rotate_axis_angle(self, axis, angle);
fn double[<3>] double[<3>].rejection(v1, double[<3>] v2) => rejection3(v1, v2);
fn double[<3>] double[<3>].project(self, Matrix4 model, Matrix4 projection, double[<4>] viewport) => project3(self, model, projection, viewport);
fn double[<3>] double[<3>].unproject(self, Matrix4 model, Matrix4 projection, double[<4>] viewport) => unproject3(self, model, projection, viewport);

macro float float[<*>].sum(float[<*>] x, float start = 0.0) => $$reduce_fadd(x, start);
macro float float[<*>].product(float[<*>] x, float start = 1.0) => $$reduce_fmul(x, start);
macro float float[<*>].max(float[<*>] x) => $$reduce_max(x);
macro float float[<*>].min(float[<*>] x) => $$reduce_min(x);
macro float[<*>] float[<*>].ceil(float[<*>] x) => $$ceil(x);
macro float[<*>] float[<*>].clamp(float[<*>] x, float[<*>] lower, float[<*>] upper) => $$max(lower, $$min(x, upper));
macro float[<*>] float[<*>].copysign(float[<*>] mag, float[<*>] sgn) => $$copysign(mag, sgn);
macro float[<*>] float[<*>].fma(float[<*>] a, float[<*>] b, float[<*>] c) => $$fma(a, b, c);
macro float[<*>] float[<*>].floor(float[<*>] x) => $$floor(x);
macro float[<*>] float[<*>].nearbyint(float[<*>] x) => $$nearbyint(x);
macro float[<*>] float[<*>].pow(float[<*>] x, exp) => pow(x, exp);
macro float[<*>] float[<*>].rint(float[<*>] x) => $$rint(x);
macro float[<*>] float[<*>].round(float[<*>] x) => $$round(x);
macro float[<*>] float[<*>].roundeven(float[<*>] x) => $$roundeven(x);
macro float[<*>] float[<*>].trunc(float[<*>] x) => $$trunc(x);
macro float float[<*>].dot(float[<*>] x, float[<*>] y) => (x * y).sum();
macro float float[<*>].length(float[<*>] x) => $$sqrt(x.dot(x));
macro float float[<*>].length_sq(self) => self.dot(self);
macro float float[<*>].distance(float[<*>] x, float[<*>] y) => (x - y).length();
macro float float[<*>].distance_sq(self, float[<*>] v2) => (self - v2).length_sq();
macro float[<*>] float[<*>].normalize(float[<*>] x) => normalize(x);
macro float[<*>] float[<*>].lerp(float[<*>] x, float[<*>] y, float amount) => math::lerp(x, y, amount);
macro float[<*>] float[<*>].reflect(float[<*>] x, float[<*>] y) => reflect(x, y);
macro float[<*>] float[<*>].clamp_length(self, float min, float max)  => clamp_length{$typeof(self)}(self, min, max);
macro float[<*>] float[<*>].move_towards(self, float[<*>] target, float max_distance) => towards{$typeof(self)}(self, target, max_distance);
macro bool float[<*>].equals(float[<*>] x, float[<*>] y) => equals(x, y);
macro bool float[<*>].not_equals(float[<*>] x, float[<*>] y) => not_equals(x, y);
macro bool float[<*>].is_normalized(float[<*>] x) => math::is_approx(x.length_sq(), 1.0, math::FLOAT_VECTOR_EPSILON);
macro bool[<*>] float[<*>].comp_lt(float[<*>] x, float[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] float[<*>].comp_le(float[<*>] x, float[<*>] y) => $$veccomple(x, y);
macro bool[<*>] float[<*>].comp_eq(float[<*>] x, float[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] float[<*>].comp_gt(float[<*>] x, float[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] float[<*>].comp_ge(float[<*>] x, float[<*>] y) => $$veccompge(x, y);
macro bool[<*>] float[<*>].comp_ne(float[<*>] x, float[<*>] y) => $$veccompne(x, y);

macro double double[<*>].sum(double[<*>] x, double start = 0.0) => $$reduce_fadd(x, start);
macro double double[<*>].product(double[<*>] x, double start = 1.0) => $$reduce_fmul(x, start);
macro double double[<*>].max(double[<*>] x) => $$reduce_max(x);
macro double double[<*>].min(double[<*>] x) => $$reduce_min(x);
macro double[<*>] double[<*>].ceil(double[<*>] x) => $$ceil(x);
macro double[<*>] double[<*>].clamp(double[<*>] x, double[<*>] lower, double[<*>] upper) => $$max(lower, $$min(x, upper));
macro double[<*>] double[<*>].copysign(double[<*>] mag, double[<*>] sgn) => $$copysign(mag, sgn);
macro double[<*>] double[<*>].floor(double[<*>] x) => $$floor(x);
macro double[<*>] double[<*>].fma(double[<*>] a, double[<*>] b, double[<*>] c) => $$fma(a, b, c);
macro double[<*>] double[<*>].nearbyint(double[<*>] x) => $$nearbyint(x);
macro double[<*>] double[<*>].pow(double[<*>] x, exp) => pow(x, exp);
macro double[<*>] double[<*>].rint(double[<*>] x) => $$rint(x);
macro double[<*>] double[<*>].round(double[<*>] x) => $$round(x);
macro double[<*>] double[<*>].roundeven(double[<*>] x) => $$roundeven(x);
macro double[<*>] double[<*>].trunc(double[<*>] x) => $$trunc(x);
macro double double[<*>].dot(double[<*>] x, double[<*>] y) => (x * y).sum();
macro double double[<*>].length(double[<*>] x) => $$sqrt(x.dot(x));
macro double double[<*>].length_sq(double[<*>] x) => x.dot(x);
macro double double[<*>].distance(double[<*>] x, double[<*>] y) => (x - y).length();
macro double double[<*>].distance_sq(self, double[<*>] v2) => (self - v2).length_sq();
macro double[<*>] double[<*>].clamp_length(self, double min, double max) => clamp_length{$typeof(self)}(self, min, max);
macro double[<*>] double[<*>].move_towards(self, double[<*>] target, double max_distance) => towards{$typeof(self)}(self, target, max_distance);
macro double[<*>] double[<*>].normalize(double[<*>] x) => normalize(x);
macro double[<*>] double[<*>].reflect(double[<*>] x, double[<*>] y) => reflect(x, y);
macro double[<*>] double[<*>].lerp(double[<*>] x, double[<*>] y, double amount) => math::lerp(x, y, amount);
macro bool double[<*>].equals(double[<*>] x, double[<*>] y) => equals(x, y);
macro bool double[<*>].not_equals(double[<*>] x, double[<*>] y) => not_equals(x, y);
macro bool double[<*>].is_normalized(double[<*>] x) => math::is_approx(x.length_sq(), 1.0, math::DOUBLE_VECTOR_EPSILON);
macro bool[<*>] double[<*>].comp_lt(double[<*>] x, double[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] double[<*>].comp_le(double[<*>] x, double[<*>] y) => $$veccomple(x, y);
macro bool[<*>] double[<*>].comp_eq(double[<*>] x, double[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] double[<*>].comp_gt(double[<*>] x, double[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] double[<*>].comp_ge(double[<*>] x, double[<*>] y) => $$veccompge(x, y);
macro bool[<*>] double[<*>].comp_ne(double[<*>] x, double[<*>] y) => $$veccompne(x, y);

macro bool[<*>] ichar[<*>].comp_lt(ichar[<*>] x, ichar[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] ichar[<*>].comp_le(ichar[<*>] x, ichar[<*>] y) => $$veccomple(x, y);
macro bool[<*>] ichar[<*>].comp_eq(ichar[<*>] x, ichar[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] ichar[<*>].comp_gt(ichar[<*>] x, ichar[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] ichar[<*>].comp_ge(ichar[<*>] x, ichar[<*>] y) => $$veccompge(x, y);
macro bool[<*>] ichar[<*>].comp_ne(ichar[<*>] x, ichar[<*>] y) => $$veccompne(x, y);
macro ichar ichar[<*>].sum(ichar[<*>] x) => $$reduce_add(x);
macro ichar ichar[<*>].product(ichar[<*>] x) => $$reduce_mul(x);
macro ichar ichar[<*>].and(ichar[<*>] x) => $$reduce_and(x);
macro ichar ichar[<*>].or(ichar[<*>] x) => $$reduce_or(x);
macro ichar ichar[<*>].xor(ichar[<*>] x) => $$reduce_xor(x);
macro ichar ichar[<*>].max(ichar[<*>] x) => $$reduce_max(x);
macro ichar ichar[<*>].min(ichar[<*>] x) => $$reduce_min(x);
macro ichar ichar[<*>].dot(ichar[<*>] x, ichar[<*>] y) => (x * y).sum();
<*
 @require @is_same_vector_or_scalar(self, mul) : `mul must be a vector of the same type as self, or be an integer scalar`
 @require @is_same_vector_or_scalar(self, div) : `div must be a vector of the same type as self, or be an integer scalar`
*>
macro ichar[<*>] ichar[<*>].muldiv(self, mul, div)  => math::mul_div_helper(self, mul, div);

macro bool[<*>] short[<*>].comp_lt(short[<*>] x, short[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] short[<*>].comp_le(short[<*>] x, short[<*>] y) => $$veccomple(x, y);
macro bool[<*>] short[<*>].comp_eq(short[<*>] x, short[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] short[<*>].comp_gt(short[<*>] x, short[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] short[<*>].comp_ge(short[<*>] x, short[<*>] y) => $$veccompge(x, y);
macro bool[<*>] short[<*>].comp_ne(short[<*>] x, short[<*>] y) => $$veccompne(x, y);
macro short short[<*>].sum(short[<*>] x) => $$reduce_add(x);
macro short short[<*>].product(short[<*>] x) => $$reduce_mul(x);
macro short short[<*>].and(short[<*>] x) => $$reduce_and(x);
macro short short[<*>].or(short[<*>] x) => $$reduce_or(x);
macro short short[<*>].xor(short[<*>] x) => $$reduce_xor(x);
macro short short[<*>].max(short[<*>] x) => $$reduce_max(x);
macro short short[<*>].min(short[<*>] x) => $$reduce_min(x);
macro short short[<*>].dot(short[<*>] x, short[<*>] y) => (x * y).sum();
<*
 @require @is_same_vector_or_scalar(self, mul) : `mul must be a vector of the same type as self, or be an integer scalar`
 @require @is_same_vector_or_scalar(self, div) : `div must be a vector of the same type as self, or be an integer scalar`
*>
macro short[<*>] short[<*>].muldiv(self, mul, div) => math::mul_div_helper(self, mul, div);

macro bool[<*>] int[<*>].comp_lt(int[<*>] x, int[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] int[<*>].comp_le(int[<*>] x, int[<*>] y) => $$veccomple(x, y);
macro bool[<*>] int[<*>].comp_eq(int[<*>] x, int[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] int[<*>].comp_gt(int[<*>] x, int[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] int[<*>].comp_ge(int[<*>] x, int[<*>] y) => $$veccompge(x, y);
macro bool[<*>] int[<*>].comp_ne(int[<*>] x, int[<*>] y) => $$veccompne(x, y);
macro int int[<*>].sum(int[<*>] x) => $$reduce_add(x);
macro int int[<*>].product(int[<*>] x) => $$reduce_mul(x);
macro int int[<*>].and(int[<*>] x) => $$reduce_and(x);
macro int int[<*>].or(int[<*>] x) => $$reduce_or(x);
macro int int[<*>].xor(int[<*>] x) => $$reduce_xor(x);
macro int int[<*>].max(int[<*>] x) => $$reduce_max(x);
macro int int[<*>].min(int[<*>] x) => $$reduce_min(x);
macro int int[<*>].dot(int[<*>] x, int[<*>] y) => (x * y).sum();
<*
 @require @is_same_vector_or_scalar(self, mul) : `mul must be a vector of the same type as self, or be an integer scalar`
 @require @is_same_vector_or_scalar(self, div) : `div must be a vector of the same type as self, or be an integer scalar`
*>
macro int[<*>] int[<*>].muldiv(self, mul, div) => math::mul_div_helper(self, mul, div);

macro bool[<*>] long[<*>].comp_lt(long[<*>] x, long[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] long[<*>].comp_le(long[<*>] x, long[<*>] y) => $$veccomple(x, y);
macro bool[<*>] long[<*>].comp_eq(long[<*>] x, long[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] long[<*>].comp_gt(long[<*>] x, long[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] long[<*>].comp_ge(long[<*>] x, long[<*>] y) => $$veccompge(x, y);
macro bool[<*>] long[<*>].comp_ne(long[<*>] x, long[<*>] y) => $$veccompne(x, y);
macro long long[<*>].sum(long[<*>] x) => $$reduce_add(x);
macro long long[<*>].product(long[<*>] x) => $$reduce_mul(x);
macro long long[<*>].and(long[<*>] x) => $$reduce_and(x);
macro long long[<*>].or(long[<*>] x) => $$reduce_or(x);
macro long long[<*>].xor(long[<*>] x) => $$reduce_xor(x);
macro long long[<*>].max(long[<*>] x) => $$reduce_max(x);
macro long long[<*>].min(long[<*>] x) => $$reduce_min(x);
macro long long[<*>].dot(long[<*>] x, long[<*>] y) => (x * y).sum();
<*
 @require @is_same_vector_or_scalar(self, mul) : `mul must be a vector of the same type as self, or be an integer scalar`
 @require @is_same_vector_or_scalar(self, div) : `div must be a vector of the same type as self, or be an integer scalar`
*>
macro long[<*>] long[<*>].muldiv(self, mul, div) => math::mul_div_helper(self, mul, div);

macro bool[<*>] int128[<*>].comp_lt(int128[<*>] x, int128[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] int128[<*>].comp_le(int128[<*>] x, int128[<*>] y) => $$veccomple(x, y);
macro bool[<*>] int128[<*>].comp_eq(int128[<*>] x, int128[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] int128[<*>].comp_gt(int128[<*>] x, int128[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] int128[<*>].comp_ge(int128[<*>] x, int128[<*>] y) => $$veccompge(x, y);
macro bool[<*>] int128[<*>].comp_ne(int128[<*>] x, int128[<*>] y) => $$veccompne(x, y);
macro int128 int128[<*>].sum(int128[<*>] x) => $$reduce_add(x);
macro int128 int128[<*>].product(int128[<*>] x) => $$reduce_mul(x);
macro int128 int128[<*>].and(int128[<*>] x) => $$reduce_and(x);
macro int128 int128[<*>].or(int128[<*>] x) => $$reduce_or(x);
macro int128 int128[<*>].xor(int128[<*>] x) => $$reduce_xor(x);
macro int128 int128[<*>].max(int128[<*>] x) => $$reduce_max(x);
macro int128 int128[<*>].min(int128[<*>] x) => $$reduce_min(x);
macro int128 int128[<*>].dot(int128[<*>] x, int128[<*>] y) => (x * y).sum();

macro bool[<*>] bool[<*>].comp_lt(bool[<*>] x, bool[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] bool[<*>].comp_le(bool[<*>] x, bool[<*>] y) => $$veccomple(x, y);
macro bool[<*>] bool[<*>].comp_eq(bool[<*>] x, bool[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] bool[<*>].comp_gt(bool[<*>] x, bool[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] bool[<*>].comp_ge(bool[<*>] x, bool[<*>] y) => $$veccompge(x, y);
macro bool[<*>] bool[<*>].comp_ne(bool[<*>] x, bool[<*>] y) => $$veccompne(x, y);
macro bool bool[<*>].sum(bool[<*>] x) => $$reduce_add(x);
macro bool bool[<*>].product(bool[<*>] x) => $$reduce_mul(x);
macro bool bool[<*>].and(bool[<*>] x) => $$reduce_and(x);
macro bool bool[<*>].or(bool[<*>] x) => $$reduce_or(x);
macro bool bool[<*>].xor(bool[<*>] x) => $$reduce_xor(x);
macro bool bool[<*>].max(bool[<*>] x) => $$reduce_max(x);
macro bool bool[<*>].min(bool[<*>] x) => $$reduce_min(x);

macro bool[<*>] char[<*>].comp_lt(char[<*>] x, char[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] char[<*>].comp_le(char[<*>] x, char[<*>] y) => $$veccomple(x, y);
macro bool[<*>] char[<*>].comp_eq(char[<*>] x, char[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] char[<*>].comp_gt(char[<*>] x, char[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] char[<*>].comp_ge(char[<*>] x, char[<*>] y) => $$veccompge(x, y);
macro bool[<*>] char[<*>].comp_ne(char[<*>] x, char[<*>] y) => $$veccompne(x, y);
macro char char[<*>].sum(char[<*>] x) => $$reduce_add(x);
macro char char[<*>].product(char[<*>] x) => $$reduce_mul(x);
macro char char[<*>].and(char[<*>] x) => $$reduce_and(x);
macro char char[<*>].or(char[<*>] x) => $$reduce_or(x);
macro char char[<*>].xor(char[<*>] x) => $$reduce_xor(x);
macro char char[<*>].max(char[<*>] x) => $$reduce_max(x);
macro char char[<*>].min(char[<*>] x) => $$reduce_min(x);
macro char char[<*>].dot(char[<*>] x, char[<*>] y) => (x * y).sum();
<*
 @require @is_same_vector_or_scalar(self, mul) : `mul must be a vector of the same type as self, or be an integer scalar`
 @require @is_same_vector_or_scalar(self, div) : `div must be a vector of the same type as self, or be an integer scalar`
*>
macro char[<*>] char[<*>].muldiv(self, mul, div)  => math::mul_div_helper(self, mul, div);

macro bool[<*>] ushort[<*>].comp_lt(ushort[<*>] x, ushort[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] ushort[<*>].comp_le(ushort[<*>] x, ushort[<*>] y) => $$veccomple(x, y);
macro bool[<*>] ushort[<*>].comp_eq(ushort[<*>] x, ushort[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] ushort[<*>].comp_gt(ushort[<*>] x, ushort[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] ushort[<*>].comp_ge(ushort[<*>] x, ushort[<*>] y) => $$veccompge(x, y);
macro bool[<*>] ushort[<*>].comp_ne(ushort[<*>] x, ushort[<*>] y) => $$veccompne(x, y);
macro ushort ushort[<*>].sum(ushort[<*>] x) => $$reduce_add(x);
macro ushort ushort[<*>].product(ushort[<*>] x) => $$reduce_mul(x);
macro ushort ushort[<*>].and(ushort[<*>] x) => $$reduce_and(x);
macro ushort ushort[<*>].or(ushort[<*>] x) => $$reduce_or(x);
macro ushort ushort[<*>].xor(ushort[<*>] x) => $$reduce_xor(x);
macro ushort ushort[<*>].max(ushort[<*>] x) => $$reduce_max(x);
macro ushort ushort[<*>].min(ushort[<*>] x) => $$reduce_min(x);
macro ushort ushort[<*>].dot(ushort[<*>] x, ushort[<*>] y) => (x * y).sum();
<*
 @require @is_same_vector_or_scalar(self, mul) : `mul must be a vector of the same type as self, or be an integer scalar`
 @require @is_same_vector_or_scalar(self, div) : `div must be a vector of the same type as self, or be an integer scalar`
*>
macro ushort[<*>] ushort[<*>].muldiv(self, mul, div) => math::mul_div_helper(self, mul, div);

macro bool[<*>] uint[<*>].comp_lt(uint[<*>] x, uint[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] uint[<*>].comp_le(uint[<*>] x, uint[<*>] y) => $$veccomple(x, y);
macro bool[<*>] uint[<*>].comp_eq(uint[<*>] x, uint[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] uint[<*>].comp_gt(uint[<*>] x, uint[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] uint[<*>].comp_ge(uint[<*>] x, uint[<*>] y) => $$veccompge(x, y);
macro bool[<*>] uint[<*>].comp_ne(uint[<*>] x, uint[<*>] y) => $$veccompne(x, y);
macro uint uint[<*>].sum(uint[<*>] x) => $$reduce_add(x);
macro uint uint[<*>].product(uint[<*>] x) => $$reduce_mul(x);
macro uint uint[<*>].and(uint[<*>] x) => $$reduce_and(x);
macro uint uint[<*>].or(uint[<*>] x) => $$reduce_or(x);
macro uint uint[<*>].xor(uint[<*>] x) => $$reduce_xor(x);
macro uint uint[<*>].max(uint[<*>] x) => $$reduce_max(x);
macro uint uint[<*>].min(uint[<*>] x) => $$reduce_min(x);
macro uint uint[<*>].dot(uint[<*>] x, uint[<*>] y) => (x * y).sum();
<*
 @require @is_same_vector_or_scalar(self, mul) : `mul must be a vector of the same type as self, or be an integer scalar`
 @require @is_same_vector_or_scalar(self, div) : `div must be a vector of the same type as self, or be an integer scalar`
*>
macro uint[<*>] uint[<*>].muldiv(self, mul, div) => math::mul_div_helper(self, mul, div);

macro bool[<*>] ulong[<*>].comp_lt(ulong[<*>] x, ulong[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] ulong[<*>].comp_le(ulong[<*>] x, ulong[<*>] y) => $$veccomple(x, y);
macro bool[<*>] ulong[<*>].comp_eq(ulong[<*>] x, ulong[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] ulong[<*>].comp_gt(ulong[<*>] x, ulong[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] ulong[<*>].comp_ge(ulong[<*>] x, ulong[<*>] y) => $$veccompge(x, y);
macro bool[<*>] ulong[<*>].comp_ne(ulong[<*>] x, ulong[<*>] y) => $$veccompne(x, y);
macro ulong ulong[<*>].sum(ulong[<*>] x) => $$reduce_add(x);
macro ulong ulong[<*>].product(ulong[<*>] x) => $$reduce_mul(x);
macro ulong ulong[<*>].and(ulong[<*>] x) => $$reduce_and(x);
macro ulong ulong[<*>].or(ulong[<*>] x) => $$reduce_or(x);
macro ulong ulong[<*>].xor(ulong[<*>] x) => $$reduce_xor(x);
macro ulong ulong[<*>].max(ulong[<*>] x) => $$reduce_max(x);
macro ulong ulong[<*>].min(ulong[<*>] x) => $$reduce_min(x);
macro ulong ulong[<*>].dot(ulong[<*>] x, ulong[<*>] y) => (x * y).sum();
<*
 @require @is_same_vector_or_scalar(self, mul) : `mul must be a vector of the same type as self, or be an integer scalar`
 @require @is_same_vector_or_scalar(self, div) : `div must be a vector of the same type as self, or be an integer scalar`
*>
macro ulong[<*>] ulong[<*>].muldiv(self, mul, div) => math::mul_div_helper(self, mul, div);

macro bool[<*>] uint128[<*>].comp_lt(uint128[<*>] x, uint128[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] uint128[<*>].comp_le(uint128[<*>] x, uint128[<*>] y) => $$veccomple(x, y);
macro bool[<*>] uint128[<*>].comp_eq(uint128[<*>] x, uint128[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] uint128[<*>].comp_gt(uint128[<*>] x, uint128[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] uint128[<*>].comp_ge(uint128[<*>] x, uint128[<*>] y) => $$veccompge(x, y);
macro bool[<*>] uint128[<*>].comp_ne(uint128[<*>] x, uint128[<*>] y) => $$veccompne(x, y);
macro uint128 uint128[<*>].sum(uint128[<*>] x) => $$reduce_add(x);
macro uint128 uint128[<*>].product(uint128[<*>] x) => $$reduce_mul(x);
macro uint128 uint128[<*>].and(uint128[<*>] x) => $$reduce_and(x);
macro uint128 uint128[<*>].or(uint128[<*>] x) => $$reduce_or(x);
macro uint128 uint128[<*>].xor(uint128[<*>] x) => $$reduce_xor(x);
macro uint128 uint128[<*>].max(uint128[<*>] x) => $$reduce_max(x);
macro uint128 uint128[<*>].min(uint128[<*>] x) => $$reduce_min(x);
macro uint128 uint128[<*>].dot(uint128[<*>] x, uint128[<*>] y) => (x * y).sum();

