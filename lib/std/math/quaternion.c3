module std::math;

// Predefined quaternion aliases.

alias Quaternionf = QuaternionNumber {float};
alias Quaternion = QuaternionNumber {double};
alias QUATERNION_IDENTITY  @builtin = quaternion::IDENTITY {double};
alias QUATERNIONF_IDENTITY @builtin = quaternion::IDENTITY {float};

<*
 The generic quaternion module, for float or double based quaternion definitions.

 @require Real.kindof == FLOAT : "A quaternion must use a floating type"
*>

module std::math::quaternion <Real>;
import std::math::vector;
union QuaternionNumber
{
	struct
	{
		Real i, j, k, l;
	}
	Real[<4>] v;
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
macro Real QuaternionNumber.length(self) => self.v.length();
macro QuaternionNumber QuaternionNumber.lerp(self, QuaternionNumber q2, Real amount) => { .v = self.v.lerp(q2.v, amount) };
fn QuaternionNumber QuaternionNumber.nlerp(self, QuaternionNumber q2, Real amount) => { .v = self.v.lerp(q2.v, amount).normalize() };

macro Matrix4f QuaternionNumber.to_matrixf(&self) => into_matrix(self, Matrix4f);
macro Matrix4 QuaternionNumber.to_matrix(&self) => into_matrix(self, Matrix4);


fn QuaternionNumber QuaternionNumber.invert(self)
{
	Real length_sq = self.v.dot(self.v);
	if (length_sq <= 0) return self;
	Real inv_length = 1 / length_sq;
	return { self.v[0] * -inv_length, self.v[1] * -inv_length, self.v[2] * -inv_length, self.v[3] * inv_length };
}

fn QuaternionNumber QuaternionNumber.conjugate(&self) => { -self.v.x, -self.v.y, -self.v.z, self.v.w };

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


fn QuaternionNumber from_axis_angle(Real[<3>] axis, Real angle)
{
	Real[<3>] normal_axis = axis.normalize();
	Real half_angle = angle * 0.5;
	Real sin_half = math::sin(half_angle);

	return { ...(normal_axis * sin_half), math::cos(half_angle) };
}

fn Real[<3>] QuaternionNumber.rotate_vec3(self, Real[<3>] vector) @operator(*)
{
	QuaternionNumber p = { ...vector, 0 };
	QuaternionNumber result = self * p * self.conjugate();
	return result.v.xyz;
}



macro into_matrix(QuaternionNumber* q, $Type) @private
{
    QuaternionNumber rotation = q.normalize();
    var x = rotation.i;
    var y = rotation.j;
    var z = rotation.k;
    var w = rotation.l;

    return ($Type) {
        1 - 2*y*y - 2*z*z,     2*x*y - 2*z*w,     2*x*z + 2*y*w,   0,
            2*x*y + 2*z*w, 1 - 2*x*x - 2*z*z,     2*y*z - 2*x*w,   0,
            2*x*z - 2*y*w,     2*y*z + 2*x*w, 1 - 2*x*x - 2*y*y,   0,
                      0.0,               0.0,               0.0, 1.0,
    };
}