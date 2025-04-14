module std::math;

// Predefined quaternion aliases.

alias Quaternionf = Quaternion {float};
alias Quaternion = Quaternion {double};
alias QUATERNION_IDENTITY  @builtin = quaternion::IDENTITY {double};
alias QUATERNIONF_IDENTITY @builtin = quaternion::IDENTITY {float};

<*
 The generic quaternion module, for float or double based quaternion definitions.

 @require Real.kindof == FLOAT : "A quaternion must use a floating type"
*>

module std::math::quaternion {Real};
import std::math::vector;
union Quaternion
{
	struct
	{
		Real i, j, k, l;
	}
	Real[<4>] v;
}

const Quaternion IDENTITY = { 0, 0, 0, 1 };

macro Quaternion Quaternion.add(self, Quaternion b) @operator(+) => { .v = self.v + b.v };
macro Quaternion Quaternion.add_each(self, Real b) => { .v = self.v + b };
macro Quaternion Quaternion.sub(self, Quaternion b) @operator(-) => { .v = self.v - b.v };
macro Quaternion Quaternion.negate(self) @operator(-) => { .v = -self.v };
macro Quaternion Quaternion.sub_each(self, Real b) => { .v = self.v - b };
macro Quaternion Quaternion.scale(self, Real s) @operator_s(*) => { .v = self.v * s };
macro Quaternion Quaternion.normalize(self) => { .v = self.v.normalize() };
macro Real Quaternion.length(self) => self.v.length();
macro Quaternion Quaternion.lerp(self, Quaternion q2, Real amount) => { .v = self.v.lerp(q2.v, amount) };
macro Matrix4f Quaternion.to_matrixf(&self) => into_matrix(self, Matrix4f);
macro Matrix4 Quaternion.to_matrix(&self) => into_matrix(self, Matrix4);
fn Quaternion Quaternion.nlerp(self, Quaternion q2, Real amount) => { .v = self.v.lerp(q2.v, amount).normalize() };

fn Quaternion Quaternion.invert(self)
{
	Real length_sq = self.v.dot(self.v);
	if (length_sq <= 0) return self;
	Real inv_length = 1 / length_sq;
	return { self.v[0] * -inv_length, self.v[1] * -inv_length, self.v[2] * -inv_length, self.v[3] * inv_length };
}

fn Quaternion Quaternion.slerp(self, Quaternion q2, Real amount)
{
	Quaternion result = {};

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

fn Quaternion Quaternion.mul(self, Quaternion b) @operator(*)
{
	return { self.i * b.l + self.l * b.i + self.j * b.k - self.k * b.j,
			 self.j * b.l + self.l * b.j + self.k * b.i - self.i * b.k,
			 self.k * b.l + self.l * b.k + self.i * b.j - self.j * b.i,
			 self.l * b.l - self.i * b.i - self.j * self.j - self.k * self.k };
}

macro into_matrix(Quaternion* q, $Type) @private
{
    Quaternion rotation = q.normalize();
    var x = rotation.i;
    var y = rotation.j;
    var z = rotation.k;
    var w = rotation.l;

    return ($Type) {
        1 - 2*y*y - 2*z*z, 2*x*y - 2*z*w, 2*x*z + 2*y*w, 0,
        2*x*y + 2*z*w, 1 - 2*x*x - 2*z*z, 2*y*z - 2*x*w, 0,
        2*x*z - 2*y*w, 2*y*z + 2*x*w , 1 - 2*x*x - 2*y*y, 0,
        0.0, 0.0, 0.0, 1.0,
    };
}