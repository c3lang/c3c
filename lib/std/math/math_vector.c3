module std::math::vector;
import std::math;

typedef Vec2f = float[<2>];
typedef Vec3f = float[<3>];
typedef Vec4f = float[<4>];

typedef Vec2 = double[<2>];
typedef Vec3 = double[<3>];
typedef Vec4 = double[<4>];

macro Vec2f.length_sq(Vec2f v) => v.dot(v);
macro Vec3f.length_sq(Vec3f v) => v.dot(v);
macro Vec4f.length_sq(Vec4f v) => v.dot(v);
macro Vec2.length_sq(Vec2 v) => v.dot(v);
macro Vec3.length_sq(Vec3 v) => v.dot(v);
macro Vec4.length_sq(Vec4 v) => v.dot(v);

macro Vec2f.distance_sq(Vec2f v1, Vec2f v2) => (v1 - v2).length_sq();
macro Vec3f.distance_sq(Vec3f v1, Vec3f v2) => (v1 - v2).length_sq();
macro Vec4f.distance_sq(Vec4f v1, Vec4f v2) => (v1 - v2).length_sq();
macro Vec2.distance_sq(Vec2 v1, Vec2 v2) => (v1 - v2).length_sq();
macro Vec3.distance_sq(Vec3 v1, Vec3 v2) => (v1 - v2).length_sq();
macro Vec4.distance_sq(Vec4 v1, Vec4 v2) => (v1 - v2).length_sq();

macro Vec2f.transform(Vec2f v, Matrix4f mat) => transform2(v, mat);
macro Vec2f.rotate(Vec2f v, float angle) => rotate(v, angle);
macro Vec2f.angle(Vec2f v1, Vec2f v2) => math::atan2(v2[1], v2[0]) - math::atan2(v1[1], v2[0]);

macro Vec2.transform(Vec2 v, Matrix4 mat) => transform2(v, mat);
macro Vec2.rotate(Vec2 v, double angle) => rotate(v, angle);
macro Vec2.angle(Vec2 v1, Vec2 v2) => math::atan2(v2[1], v2[0]) - math::atan2(v1[1], v2[0]);

macro Vec2f.clamp_mag(Vec2f v, float min, float max) => clamp_magnitude(v, min, max);
macro Vec3f.clamp_mag(Vec3f v, float min, float max) => clamp_magnitude(v, min, max);
macro Vec4f.clamp_mag(Vec4f v, float min, float max) => clamp_magnitude(v, min, max);
macro Vec2.clamp_mag(Vec2 v, double min, double max) => clamp_magnitude(v, min, max);
macro Vec3.clamp_mag(Vec3 v, double min, double max) => clamp_magnitude(v, min, max);
macro Vec4.clamp_mag(Vec4 v, double min, double max) => clamp_magnitude(v, min, max);

fn Vec2f Vec2f.towards(Vec2f v, Vec2f target, float max_distance) => towards(v, target, max_distance);
fn Vec3f Vec3f.towards(Vec3f v, Vec3f target, float max_distance) => towards(v, target, max_distance);
fn Vec4f Vec4f.towards(Vec4f v, Vec4f target, float max_distance) => towards(v, target, max_distance);
fn Vec2 Vec2.towards(Vec2 v, Vec2 target, double max_distance) => towards(v, target, max_distance);
fn Vec3 Vec3.towards(Vec3 v, Vec3 target, double max_distance) => towards(v, target, max_distance);
fn Vec4 Vec4.towards(Vec4 v, Vec4 target, double max_distance) => towards(v, target, max_distance);

fn Vec3f Vec3f.cross(Vec3f v1, Vec3f v2) => cross3(v1, v2);
fn Vec3 Vec3.cross(Vec3 v1, Vec3 v2) => cross3(v1, v2);

fn Vec3f Vec3f.perpendicular(Vec3f v) => perpendicular3(v);
fn Vec3 Vec3.perpendicular(Vec3 v) => perpendicular3(v);

fn Vec3f Vec3f.barycenter(Vec3f p, Vec3f a, Vec3f b, Vec3f c) => barycenter3(p, a, b, c);
fn Vec3 Vec3.barycenter(Vec3 p, Vec3 a, Vec3 b, Vec3 c) => barycenter3(p, a, b, c);

fn Vec3f Vec3f.transform(Vec3f v, Matrix4f mat) => transform3(v, mat);
fn Vec3 Vec3.transform(Vec3 v, Matrix4 mat) => transform3(v, mat);

fn float Vec3f.angle(Vec3f v1, Vec3f v2) => angle3(v1, v2);
fn double Vec3.angle(Vec3 v1, Vec3 v2) => angle3(v1, v2);

fn Vec3f Vec3f.refract(Vec3f v, Vec3f n, float r) => refract3(v, n, r);
fn Vec3 Vec3.refract(Vec3 v, Vec3 n, double r) => refract3(v, n, r);

fn void ortho_normalize(Vec3f* v1, Vec3f* v2) => ortho_normalize3(v1, v2);
fn void ortho_normalized(Vec3* v1, Vec3* v2) => ortho_normalize3(v1, v2);

fn Matrix4f matrix4f_look_at(Vec3f eye, Vec3f target, Vec3f up) => matrix_look_at(Matrix4f, eye, target, up);
fn Matrix4 matrix4_look_at(Vec3 eye, Vec3 target, Vec3 up) => matrix_look_at(Matrix4, eye, target, up);

fn Vec3f Vec3f.rotate_quat(Vec3f v, Quaternionf q) => rotate_by_quat3(v, q);
fn Vec3 Vec3.rotate_quat(Vec3 v, Quaternion q) => rotate_by_quat3(v, q);

fn Vec3f Vec3f.rotate_axis(Vec3f v, Vec3f axis, float angle) => rotate_axis_angle(v, axis, angle);
fn Vec3 Vec3.rotate_axis(Vec3 v, Vec3 axis, double angle) => rotate_axis_angle(v, axis, angle);

fn Vec3f Vec3f.unproject(Vec3f v, Matrix4f projection, Matrix4f view) => unproject3(v, projection, view);
fn Vec3 Vec3.unproject(Vec3 v, Matrix4 projection, Matrix4 view) => unproject3(v, projection, view);

macro towards(v, target, max_distance) @private
{
	var delta = target - v;
    var square = delta.length_sq();

    if (square == 0 || (max_distance >= 0 && (square <= max_distance * max_distance))) return target;

    var dist = math::sqrt(square);

	return v + delta * max_distance / dist;
}

macro clamp_magnitude(v, min, max) @private
{
	var length = v.dot(v);
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
	return $typeof(v) { v[0] * c - v[1] * s, v[0] * s + v[1] * c };
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
	return $typeof(v) { mat.m00 * v[0] + mat.m10 * v[1] + mat.30,
		     mat.m01 * v[0] + mar.m11 * v[1] + mat.31 };
}

macro transform3(v, mat) @private
{
    return $typeof(v) {
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
	return $typeof(v) {
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
	wv *= math::cos(angle) * 2;
	wwv *= 2;

	return v + wv + wwv;
}

macro matrix_look_at($Type, eye, target, up) @private
{
    var vz = (eye - target).normalize();
	var vx = up.cross(vz).normalize();
	var vy = vz.cross(vx);

	return $Type {
		vx[0], vy[0], vz[0], 0,
		vx[1], vy[1], vz[1], 0,
		vx[2], vy[2], vz[2], 0,
		- vx.dot(eye), - vy.dot(eye), - vz.dot(eye), 1
	};
}

macro unproject3(v, m1, m2) @private
{
return v;
/*
	var view_proj = m1.mul(m2);
	var invert = view_proj.invert();
    // Create quaternion from source point
	$if ($typeof(v[0]).typeid == float.typeid):
	    Quaternionf quat = { v.x, v.y, v.z, 1 };
	$else:
	    Quaternion quat = { v.x, v.y, v.z, 1 };
	$endif;

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
	return $typeof(p) { 1 - y - z, y, z };
}

macro refract3(v, n, r) @private
{
    var dot = v.dot(n);
    var d = 1 - r * r * (1 - dot * dot);

    return d < 0 ? v : r * v - (r * dot + math::sqrt(d)) * n;
}