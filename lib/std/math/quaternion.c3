module std::math::quaternion;

// Predefined quaternion aliases.
alias Quaternionf = QuaternionNumber {float};
alias Quaternion = QuaternionNumber {double};

alias Quat = QuaternionNumber {float};

<*
 The generic quaternion module, for float or double based quaternion definitions.

 @require Real::kind == FLOAT : "A quaternion must use a floating type"
*>
module std::math::quaternion <Real>;
import std::math::vector, std::math::matrix, std::io;

union QuaternionNumber (Printable)
{
	struct
	{
		Real i, j, k, l;
	}
	Real[<4>] v;
}

fn sz? QuaternionNumber.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("%s", self.v);
}

const QuaternionNumber IDENTITY = { 0, 0, 0, 1 };

macro QuaternionNumber QuaternionNumber.add(self, QuaternionNumber b) @operator(+) => { .v = self.v + b.v };
macro QuaternionNumber QuaternionNumber.add_each(self, Real b) => { .v = self.v + b };
macro QuaternionNumber QuaternionNumber.sub(self, QuaternionNumber b) @operator(-) => { .v = self.v - b.v };
macro QuaternionNumber QuaternionNumber.negate(self) @operator(-) => { .v = -self.v };
macro QuaternionNumber QuaternionNumber.sub_each(self, Real b) => { .v = self.v - b };
macro QuaternionNumber QuaternionNumber.scale(self, Real s) @operator_s(*) => { .v = self.v * s };
macro QuaternionNumber.to_angle(self) => 2 * math::acos(self.v.w);
macro QuaternionNumber QuaternionNumber.normalize(self) => { .v = self.v.normalize() };
fn bool QuaternionNumber.is_normalized(self) => self.v.is_normalized();
macro bool QuaternionNumber.equals(self, QuaternionNumber other) => self.v.equals(other.v);
macro bool QuaternionNumber.not_equals(self, QuaternionNumber other) => self.v.not_equals(other.v);
macro Real QuaternionNumber.length(self) => self.v.length();
macro Real QuaternionNumber.length_sq(self) => self.v.length_sq();
macro QuaternionNumber QuaternionNumber.lerp(self, QuaternionNumber q2, Real amount) => { .v = self.v.lerp(q2.v, amount) };
fn QuaternionNumber QuaternionNumber.nlerp(self, QuaternionNumber q2, Real amount) => { .v = self.v.lerp(q2.v, amount).normalize() };

macro Matrix4x4{Real} QuaternionNumber.to_matrix(&self) => into_matrix(self, Matrix4x4{Real});


fn QuaternionNumber QuaternionNumber.invert(self)
{
	Real length_sq = self.v.dot(self.v);
	if (length_sq <= 0) return self;
	Real inv_length = 1 / length_sq;
	return { self.v[0] * -inv_length, self.v[1] * -inv_length, self.v[2] * -inv_length, self.v[3] * inv_length };
}

fn QuaternionNumber QuaternionNumber.transform(self, Matrix4x4{Real} transform)
{
	Real[<4>] v = self.v;
	Real[<16>] m = transform.m;
	return {
		m[0]  * v.x + m[1]  * v.y + m[2]  * v.z + m[3]  * v.w,
		m[4]  * v.x + m[5]  * v.y + m[6]  * v.z + m[7]  * v.w,
		m[8]  * v.x + m[9]  * v.y + m[10] * v.z + m[11] * v.w,
		m[12] * v.x + m[13] * v.y + m[14] * v.z + m[15] * v.w,
	};
}

fn QuaternionNumber QuaternionNumber.conjugate(self) => { -self.v.x, -self.v.y, -self.v.z, self.v.w };

fn QuaternionNumber QuaternionNumber.slerp(self, QuaternionNumber q2, Real amount)
{
	QuaternionNumber result = {};

	Real[<4>] q2v = q2.v;
	Real cos_half_theta = self.v.dot(q2v);

	if (cos_half_theta < 0)
	{
		q2v = -q2v;
		cos_half_theta = -cos_half_theta;
	}

	if (cos_half_theta >= 1) return self;

	Real[<4>] q1v = self.v;
	if (cos_half_theta > 0.95f) return { .v = q1v.lerp(q2v, amount) };

	Real half_theta = math::cos(cos_half_theta);
	Real sin_half_theta = math::sqrt(1 - cos_half_theta * cos_half_theta);
	if (math::abs(sin_half_theta) < 0.001f)
	{
		return { .v = (q1v + q2v) * 0.5f };
	}
	Real ratio_a = math::sin((1 - amount) * half_theta) / sin_half_theta;
	Real ratio_b = math::sin(amount * half_theta) / sin_half_theta;
	return { .v = q1v * ratio_a + q2v * ratio_b };
}

fn QuaternionNumber QuaternionNumber.mul(self, QuaternionNumber b) @operator(*)
{  
	Real[<3>] q1_axis = { self.v.x, self.v.y, self.v.z };
	Real[<3>] q2_axis = { b.v.x, b.v.y, b.v.z };

	Real scalar = (self.v.w * b.v.w - q1_axis.dot(q2_axis));
	Real[<3>] axis = self.v.w * q2_axis + b.v.w * q1_axis + q1_axis.cross(q2_axis);

	return { ...axis, scalar };
}


// Get the quaternion equivalent to Euler angles
// Rotation order is ZYX
fn QuaternionNumber from_euler(Real pitch, Real yaw, Real roll)
{
	Real x0 = math::cos(pitch / 2);
	Real x1 = math::sin(pitch / 2);
	Real y0 = math::cos(yaw / 2);
	Real y1 = math::sin(yaw / 2);
	Real z0 = math::cos(roll / 2);
	Real z1 = math::sin(roll / 2);

	return {
		x1*y0*z0 - x0*y1*z1,
		x0*y1*z0 + x1*y0*z1,
		x0*y0*z1 - x1*y1*z0,
		x0*y0*z0 + x1*y1*z1
	};
}

//
<*
 Get the Euler angles equivalent to quaternion (roll, pitch, yaw)

 @require math::is_approx(self.length_sq(), 1, QUATERNION_EPSILON) : "Expected a normalized quaternion"
 @return "a vector containing { roll, pitch, yaw }"
*>
fn Real[<3>] QuaternionNumber.to_euler(self)
{
	Real[<4>] q = self.v;
	// Roll (x-axis rotation)
	Real x0 = 2 * (q.w * q.x + q.y * q.z);
	Real x1 = 1 - 2 * (q.x * q.x + q.y * q.y);
	// Pitch (y-axis rotation)
	Real y0 = 2 * (q.w * q.y - q.z * q.x);
	y0 = math::clamp(y0, -1, 1);
	// Yaw (z-axis rotation)
	Real z0 = 2 * (q.w * q.z + q.x * q.y);
	Real z1 = 1 - 2 * (q.y * q.y + q.z * q.z);
	return { math::atan2(x0, x1), math::asin(y0), math::atan2(z0, z1) };
}

fn QuaternionNumber from_axis_angle(Real[<3>] axis, Real angle)
{
	Real[<3>] normal_axis = axis.normalize();
	Real half_angle = angle * 0.5;
	Real sin_half = math::sin(half_angle);

	return { ...(normal_axis * sin_half), math::cos(half_angle) };
}

<*
 @return "First 3 components is the axis, the last component is the angle"
*>
fn Real[<4>] QuaternionNumber.to_axis_angle(self)
{
	Real[<4>] v = self.v;
	if (math::abs(v.w) > 1)
	{
		v = v.normalize();
	}

	Real den = math::sqrt(1 - v.w * v.w);
	// Default to 1,0,0 when small
	return { ...(den > QUATERNION_EPSILON ? v.xyz / den : { 1, 0, 0 }), 2 * math::acos(v.w) };
}

fn Real[<3>] QuaternionNumber.rotate_vec3(self, Real[<3>] vector) @operator(*)
{
	QuaternionNumber p = { ...vector, 0 };
	QuaternionNumber result = self * p * self.conjugate();
	return result.v.xyz;
}

<*
 Create a quaternion transform from `from` to `to`, assuming they are not parallel and that they are
 normalized. Use `from_rotation` for non-normalized vectors.

 @require from.is_normalized() : "Expected 'from' to be normalized"
 @require to.is_normalized() : "Expected 'to' to be normalized"
 @ensure return.is_normalized()
*>
fn QuaternionNumber from_normalized_rotation(Real[<3>] from, Real[<3>] to)
{
	return (QuaternionNumber){ ...from.cross(to), 1 + from.dot(to) }.normalize();
}

<*
 Create a quaternion transform from `from` to `to`, assuming they are not parallel.
 Use `from_normalized_rotation` for normalized vectors.

 @ensure return.is_normalized()
*>
fn QuaternionNumber from_rotation(Real[<3>] from, Real[<3>] to)
{
	Real cos2_theta = from.dot(to);
	Real[<3>] cross = from.cross(to);
	QuaternionNumber num = { ...cross, math::sqrt(cross.length_sq() + cos2_theta * cos2_theta) + cos2_theta };
	return num.normalize();
}



macro into_matrix(QuaternionNumber* q, $Type) @private
{
	QuaternionNumber rotation = q.normalize();
	var x = rotation.i;
	var y = rotation.j;
	var z = rotation.k;
	var w = rotation.l;

	$if $feature(OLD_MATRIX):
	return ($Type) {
		1 - 2*y*y - 2*z*z,     2*x*y - 2*z*w,     2*x*z + 2*y*w,   0,
			2*x*y + 2*z*w, 1 - 2*x*x - 2*z*z,     2*y*z - 2*x*w,   0,
			2*x*z - 2*y*w,     2*y*z + 2*x*w, 1 - 2*x*x - 2*y*y,   0,
					  0.0,               0.0,               0.0, 1.0,
	};
	$else
	return ($Type) { .m = {
		1 - 2*y*y - 2*z*z,   2*x*y + 2*z*w,     2*x*z - 2*y*w,   0,
		2*x*y - 2*z*w,       1 - 2*x*x - 2*z*z, 2*y*z + 2*x*w,   0,
		2*x*z + 2*y*w,       2*y*z - 2*x*w,     1 - 2*x*x - 2*y*y, 0,
		0.0,                 0.0,               0.0,             1.0,
	} };
	$endif
}

const QUATERNION_EPSILON @private = Real::kind == FLOAT ? math::FLOAT_VECTOR_EPSILON : math::DOUBLE_VECTOR_EPSILON;
