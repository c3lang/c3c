// Vector supplemental methods

module std::math::vector;
import std::math;

<*
 @require $Type.kindof == VECTOR &&& $kindof(($Type){}[0]) == BOOL : "Expected a bool vector"
 @require $kindof(mask).is_int() : "Expected an integer mask"
*>
macro bool[<*>] mask_from_int($Type, mask)
{
	return $$int_to_mask(mask, $Type.len);
}

macro bool[<*>].mask_to_int(self)
{
	return $$mask_to_int(self);
}

macro double double[<*>].sq_magnitude(self) => self.dot(self);
macro float float[<*>].sq_magnitude(self) => self.dot(self);

macro double double[<*>].distance_sq(self, double[<*>] v2) => (self - v2).sq_magnitude();
macro float float[<*>].distance_sq(self, float[<*>] v2) => (self - v2).sq_magnitude();

macro float[<2>] float[<2>].transform(self, Matrix4f mat) => transform2(self, mat);
macro float[<2>] float[<2>].rotate(self, float angle) => rotate(self, angle);
macro float[<2>] float[<2>].angle(self, float[<2>] v2) => math::atan2(v2.y, v2.x) - math::atan2(self.y, self.x);

macro double[<2>] double[<2>].transform(self, Matrix4 mat) => transform2(self, mat);
macro double[<2>] double[<2>].rotate(self, double angle) => rotate(self, angle);
macro double[<2>] double[<2>].angle(self, double[<2>] v2) => math::atan2(v2.y, v2.x) - math::atan2(self.y, self.x);

macro float[<*>] float[<*>].clamp_mag(self, float min, float max) => clamp_magnitude{$typeof(self)}(self, min, max);
macro double[<*>] double[<*>].clamp_mag(self, double min, double max) => clamp_magnitude{$typeof(self)}(self, min, max);

macro float[<*>] float[<*>].towards(self, float[<*>] target, float max_distance) => towards{$typeof(self)}(self, target, max_distance);
macro double[<*>] double[<*>].towards(self, double[<*>] target, double max_distance) => towards{$typeof(self)}(self, target, max_distance);

fn float float[<2>].cross(self, float[<2>] v2) => cross2(self, v2);
fn float[<3>] float[<3>].cross(self, float[<3>] v2) => cross3(self, v2);
fn double double[<2>].cross(self, double[<2>] v2) => cross2(self, v2);
fn double[<3>] double[<3>].cross(self, double[<3>] v2) => cross3(self, v2);

fn float[<3>] float[<3>].perpendicular(self) => perpendicular3(self);
fn double[<3>] double[<3>].perpendicular(self) => perpendicular3(self);

fn float[<3>] float[<3>].barycenter(self, float[<3>] a, float[<3>] b, float[<3>] c) => barycenter3(self, a, b, c);
fn double[<3>] double[<3>].barycenter(self, double[<3>] a, double[<3>] b, double[<3>] c) => barycenter3(self, a, b, c);

fn float[<3>] float[<3>].transform(self, Matrix4f mat) => transform3(self, mat);
fn double[<3>] double[<3>].transform(self, Matrix4 mat) => transform3(self, mat);

fn float float[<3>].angle(self, float[<3>] v2) => angle3(self, v2);
fn double double[<3>].angle(self, double[<3>] v2) => angle3(self, v2);

fn float[<3>] float[<3>].refract(self, float[<3>] n, float r) => refract3(self, n, r);
fn double[<3>] double[<3>].refract(self, double[<3>] n, double r) => refract3(self, n, r);

fn float[<3>] float[<3>].rotate_quat(self, Quaternionf q) => q * self;
fn double[<3>] double[<3>].rotate_quat(self, Quaternion q) => q * self;

fn float[<3>] float[<3>].rotate_axis(self, float[<3>] axis, float angle) => rotate_axis_angle(self, axis, angle);
fn double[<3>] double[<3>].rotate_axis(self, double[<3>] axis, double angle) => rotate_axis_angle(self, axis, angle);

fn float[<3>] float[<3>].rejection(v1, float[<3>] v2) => rejection3(v1, v2);
fn double[<3>] double[<3>].rejection(v1, double[<3>] v2) => rejection3(v1, v2);

fn float[<3>] float[<3>].project(self, Matrix4f model, Matrix4f projection, float[<4>] viewport) => project3(self, model, projection, viewport);
fn double[<3>] double[<3>].project(self, Matrix4 model, Matrix4 projection, double[<4>] viewport) => project3(self, model, projection, viewport);

fn float[<3>] float[<3>].unproject(self, Matrix4f model, Matrix4f projection, float[<4>] viewport) => unproject3(self, model, projection, viewport);
fn double[<3>] double[<3>].unproject(self, Matrix4 model, Matrix4 projection, double[<4>] viewport) => unproject3(self, model, projection, viewport);

fn void ortho_normalize(float[<3>]* v1, float[<3>]* v2) => ortho_normalize3(v1, v2);
fn void ortho_normalized(double[<3>]* v1, double[<3>]* v2) => ortho_normalize3(v1, v2);

fn float[<3>] cubic_hermitef(float[<3>] v1, float[<3>] tangent1, float[<3>] v2, float[<3>] tangent2, float amount) => cubic_hermite3(v1, tangent1, v2, tangent2, amount);
fn double[<3>] cubic_hermite(double[<3>] v1, double[<3>] tangent1, double[<3>] v2, double[<3>] tangent2, double amount) => cubic_hermite3(v1, tangent1, v2, tangent2, amount);

module std::math::vector @private;
import std::math;

macro cubic_hermite3(v1, tangent1, v2, tangent2, amount)
{
	var amount_pow2 = amount * amount;
	var amount_pow3 = amount_pow2 * amount;

	return (2 * amount_pow3 - 3 *amount_pow2 + 1) * v1
		+ (amount_pow3 - 2 * amount_pow2 + amount) * tangent1
		+ (-2 * amount_pow3 + 3 * amount_pow2) * v2
		+ (amount_pow3 - amount_pow2) * tangent2;
}

fn TowardsType towards(TowardsType v, TowardsType target, $typefrom(TowardsType.inner) max_distance) <TowardsType>
{
	TowardsType delta = target - v;
	var square @safeinfer = delta.sq_magnitude();

	if (square == 0 || (max_distance >= 0 && (square <= max_distance * max_distance))) return target;

	return v + delta * max_distance / math::sqrt(square);
}

fn ClampType clamp_magnitude(ClampType v, $typefrom(ClampType.inner) min, $typefrom(ClampType.inner) max) <ClampType>
{
	var length @safeinfer = v.sq_magnitude();
	if (length > 0)
	{
		length = math::sqrt(length);

		if (length < min) return v * (min / length);
		if (length > max) return v * (max / length);
	}
	return v;
}

macro rotate(v, angle)
{
	var c = math::cos(angle);
	var s = math::sin(angle);
	return ($typeof(v)) { v[0] * c - v[1] * s, v[0] * s + v[1] * c };
}

macro perpendicular3(v)
{
	var min = math::abs(v[0]);
	$typeof(v) cardinal_axis = { 1, 0, 0 };

	if (var vy = math::abs(v[1]), vy < min)
	{
		min = vy;
		cardinal_axis = { 0, 1, 0 };
	}

	if (var vz = math::abs(v[2]), vz < min)
	{
		cardinal_axis = { 0, 0, 1 };
	}

	return cross3(v, cardinal_axis);
}

macro cross2(v1, v2)
{
	return v1.x * v2.y - v1.y * v2.x;
}

macro cross3(v1, v2)
{
	var a = v1.yzx * v2.zxy;
	var b = v1.zxy * v2.yzx;
	return a - b;
}

macro transform2(v, mat)
{
	return ($typeof(v)) {
		mat.m00 * v[0] + mat.m10 * v[1] + mat.m30 ,
		mat.m01 * v[0] + mat.m11 * v[1] + mat.m31 };
}

macro transform3(v, mat)
{
	return ($typeof(v)){
		mat.m00 * v[0] + mat.m10 * v[1] + mat.m20 * v[2] + mat.m30,
		mat.m01 * v[0] + mat.m11 * v[1] + mat.m21 * v[2] + mat.m31,
		mat.m02 * v[0] + mat.m12 * v[1] + mat.m22 * v[2] + mat.m32
	};
}

macro angle3(v1, v2)
{
	var len = v1.cross(v2).length();
	var dot = v1.dot(v2);
	return math::atan2(len, dot);
}

macro void ortho_normalize3(v1, v2)
{
	var v1n = *v1 = v1.normalize();
	var vn1 = v1n.cross(*v2).normalize();
	*v2 = v1n.cross(vn1);
}


macro rotate_axis_angle(v, axis, angle)
{
	axis = axis.normalize();

	angle /= 2;
	var w = axis * math::sin(angle);
	var wv = w.cross(v);
	var wwv = w.cross(wv);
	wv *= math::cos(($typeof(v))angle) * 2;
	wwv *= 2;

	return v + wv + wwv;
}

macro rejection3(v1, v2)
{
	return v1 - v2 * (v1.dot(v2) / v2.dot(v2));
}

macro project3(v, model, proj, viewport)
{
	var $Element = $typeof(v).inner;
	$Element[<4>] tmp = { ...v, 1 };
	tmp = model * tmp;
	tmp = proj * tmp;
	tmp /= tmp.w;
	tmp = { (tmp.x * 0.5 + 0.5) * viewport.z + viewport.x, (tmp.y * 0.5 + 0.5) * viewport.w + viewport.y, tmp.z, tmp.w };
	return tmp.xyz;
}

macro unproject3(v, model, proj, viewport)
{
	var mat_view_inverse = (proj * model).inverse()!!;

	var $Element = $typeof(v).inner;

	$Element[<4>] tmp = {
		2 * (v.x - viewport.x) / viewport.z - 1,
		2 * (v.y - viewport.y) / viewport.w - 1,
		v.z,
		1 };
	tmp = mat_view_inverse * tmp;
	return tmp.xyz / tmp.w;
}

macro barycenter3(p, a, b, c)
{
	var v0 = b - a;
	var v1 = c - a;
	var v2 = p - a;
	var d00 = v0.dot(v0);
	var d01 = v0.dot(v1);
	var d11 = v1.dot(v1);
	var d20 = v2.dot(v0);
	var d21 = v2.dot(v1);
	var denom = d00 * d11 - d01 * d01;
	var y = (d11 * d20 - d01 * d21) / denom;
	var z = (d00 * d21 - d01 * d20) / denom;
	return ($typeof(p)){ 1 - y - z, y, z };
}

macro refract3(v, n, r)
{
	var dot = v.dot(n);
	var d = 1 - r * r * (1 - dot * dot);

	return d < 0 ? v : r * v - (r * dot + math::sqrt(d)) * n;
}
