// Vector supplemental methods

module std::math::vector;
import std::math;

macro double[<*>].sq_magnitude(self) => self.dot(self);
macro float[<*>].sq_magnitude(self) => self.dot(self);

macro double[<*>].distance_sq(self, double[<*>] v2) => (self - v2).sq_magnitude();
macro float[<*>].distance_sq(self, float[<*>] v2) => (self - v2).sq_magnitude();

macro float[<2>].transform(self, Matrix4f mat) => transform2(self, mat);
macro float[<2>].rotate(self, float angle) => rotate(self, angle);
macro float[<2>].angle(self, float[<2>] v2) => math::atan2(v2.y, v2.x) - math::atan2(self.y, self.x);

macro double[<2>].transform(self, Matrix4 mat) => transform2(self, mat);
macro double[<2>].rotate(self, double angle) => rotate(self, angle);
macro double[<2>].angle(self, double[<2>] v2) => math::atan2(v2.y, v2.x) - math::atan2(self.y, self.x);

macro float[<*>].clamp_mag(self, float min, float max) => clamp_magnitude(self, min, max);
macro double[<*>].clamp_mag(self, double min, double max) => clamp_magnitude(self, min, max);

macro float[<*>].towards(self, float[<*>] target, float max_distance) => towards(self, target, max_distance);
macro double[<*>].towards(self, double[<*>] target, double max_distance) => towards(self, target, max_distance);

fn float[<3>] float[<3>].cross(self, float[<3>] v2) => cross3(self, v2);
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

fn float[<3>] float[<3>].rotate_quat(self, Quaternionf q) => rotate_by_quat3(self, q);
fn double[<3>] double[<3>].rotate_quat(self, Quaternion q) => rotate_by_quat3(self, q);

fn float[<3>] float[<3>].rotate_axis(self, float[<3>] axis, float angle) => rotate_axis_angle(self, axis, angle);
fn double[<3>] double[<3>].rotate_axis(self, double[<3>] axis, double angle) => rotate_axis_angle(self, axis, angle);

fn float[<3>] float[<3>].unproject(self, Matrix4f projection, Matrix4f view) => unproject3(self, projection, view);
fn double[<3>] double[<3>].unproject(self, Matrix4 projection, Matrix4 view) => unproject3(self, projection, view);

fn void ortho_normalize(float[<3>]* v1, float[<3>]* v2) => ortho_normalize3(v1, v2);
fn void ortho_normalized(double[<3>]* v1, double[<3>]* v2) => ortho_normalize3(v1, v2);

// -- private helpers

macro towards(v, target, max_distance) @private
{
	var delta = target - v;
	var square = delta.sq_magnitude();

	if (square == 0 || (max_distance >= 0 && (square <= max_distance * max_distance))) return target;

	var dist = math::sqrt(square);

	return v + delta * max_distance / dist;
}

macro clamp_magnitude(v, min, max) @private
{
	var length = v.sq_magnitude();
	if (length > 0)
	{
		length = math::sqrt(length);

		if (length < min) return v * (min / length);
		if (length > max) return v * (max / length);
	}
	return v;
}

macro rotate(v, angle) @private
{
	var c = math::cos(angle);
	var s = math::sin(angle);
	return ($typeof(v)) { v[0] * c - v[1] * s, v[0] * s + v[1] * c };
}

macro perpendicular3(v) @private
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

macro cross3(v1, v2) @private
{
	var a = v1.yzx * v2.zxy;
	var b = v1.zxy * v2.yzx;
	return a - b;
}

macro transform2(v, mat) @private
{
	return ($typeof(v)) {
		mat.m00 * v[0] + mat.m10 * v[1] + mat.m30 ,
		mat.m01 * v[0] + mat.m11 * v[1] + mat.m31 };
}

macro transform3(v, mat) @private
{
	return ($typeof(v)){
		mat.m00 * v[0] + mat.m10 * v[1] + mat.m20 * v[2] + mat.m30,
		mat.m01 * v[0] + mat.m11 * v[1] + mat.m21 * v[2] + mat.m31,
		mat.m02 * v[0] + mat.m12 * v[1] + mat.m22 * v[2] + mat.m32
	};
}


macro angle3(v1, v2) @private
{
	var len = v1.cross(v2).length();
	var dot = v1.dot(v2);
	return math::atan2(len, dot);
}

macro void ortho_normalize3(v1, v2) @private
{
	var v1n = *v1 = v1.normalize();
	var vn1 = v1n.cross(*v2).normalize();
	*v2 = v1n.cross(vn1);
}

macro rotate_by_quat3(v, q) @private
{
	return ($typeof(v)){
		v[0] * (q.i * q.i + q.l * q.l - q.j * q.j - q.k * q.k)
			+ v[1] * (2 * q.i * q.j - 2 * q.l * q.k)
			+ v[2] * (2 * q.i * q.k - 2 * q.l * q.j),
		v[0] * (2 * q.l * q.k + 2 * q.i * q.j)
			+ v[1] * (q.l * q.l - q.i * q.i + q.j * q.j - q.k * q.k)
			+ v[2] * (-2 * q.l * q.i + 2 * q.j * q.k),
		v[0] * (-2 * q.l * q.j + 2 * q.i * q.k)
			+ v[1] * (2 * q.l * q.i + 2 * q.j * q.k)
			+ v[2] * (q.l * q.l - q.i * q.i - q.j * q.j + q.k * q.k)
	};
}

macro rotate_axis_angle(v, axis, angle) @private
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

macro unproject3(v, m1, m2) @private
{
return v;
/*
	var view_proj = m1.mul(m2);
	var invert = view_proj.invert();
	// Create quaternion from source point
	$if @typeid(v[0]) == float.typeid:
		Quaternionf quat = { v.x, v.y, v.z, 1 };
	$else
		Quaternion quat = { v.x, v.y, v.z, 1 };
	$endif

	// Multiply quat point by unproject matrix
	var qtransformed = quat.transform(invert);

	// Normalized world points in vectors
	return {
		qtransformed.i / qtransformed.l,
		qtransformed.j / qtransformed.l,
		qtransformed.k  / qtransformed.l
	};*/
}

macro barycenter3(p, a, b, c) @private
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

macro refract3(v, n, r) @private
{
	var dot = v.dot(n);
	var d = 1 - r * r * (1 - dot * dot);

	return d < 0 ? v : r * v - (r * dot + math::sqrt(d)) * n;
}
