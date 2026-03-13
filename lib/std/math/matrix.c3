module std::math @if(!$feature(OLD_MATRIX));

// Predefined matrix types
alias Matrix2f = Matrix2x2 {float};
alias Matrix2  = Matrix2x2 {double};
alias Matrix3f = Matrix3x3 {float};
alias Matrix3  = Matrix3x3 {double};
alias Matrix4f = Matrix4x4 {float};
alias Matrix4  = Matrix4x4 {double};

// Predefined matrix functions
alias matrix4_ortho  @builtin = matrix::ortho {double};
alias matrix4f_ortho @builtin = matrix::ortho {float};
// Perspective is RH [0, 1]
alias matrix4_perspective  @builtin = matrix::perspective {double};
alias matrix4f_perspective @builtin = matrix::perspective {float};
alias matrix4_look_at @builtin = matrix::look_at {double};
alias matrix4f_look_at @builtin = matrix::look_at {float};

alias MATRIX2_IDENTITY  @builtin = matrix::IDENTITY2 {double};
alias MATRIX2F_IDENTITY @builtin = matrix::IDENTITY2 {float};
alias MATRIX3_IDENTITY  @builtin = matrix::IDENTITY3 {double};
alias MATRIX3F_IDENTITY @builtin = matrix::IDENTITY3 {float};
alias MATRIX4_IDENTITY  @builtin = matrix::IDENTITY4 {double};
alias MATRIX4F_IDENTITY @builtin = matrix::IDENTITY4 {float};

module std::math @if($feature(OLD_MATRIX));

// Predefined matrix types
alias Matrix2f = MatrixRM2x2 {float};
alias Matrix2  = MatrixRM2x2 {double};
alias Matrix3f = MatrixRM3x3 {float};
alias Matrix3  = MatrixRM3x3 {double};
alias Matrix4f = MatrixRM4x4 {float};
alias Matrix4  = MatrixRM4x4 {double};

// Predefined matrix functions
alias matrix4_ortho  @builtin = matrix::ortho_rm {double};
alias matrix4f_ortho @builtin = matrix::ortho_rm {float};
alias matrix4_perspective  @builtin = matrix::perspective_rm {double};
alias matrix4f_perspective @builtin = matrix::perspective_rm {float};
alias matrix4_look_at @builtin = matrix::look_at_rm {double};
alias matrix4f_look_at @builtin = matrix::look_at_rm {float};

alias MATRIX2_IDENTITY  @builtin = matrix::IDENTITY2_RM {double};
alias MATRIX2F_IDENTITY @builtin = matrix::IDENTITY2_RM {float};
alias MATRIX3_IDENTITY  @builtin = matrix::IDENTITY3_RM {double};
alias MATRIX3F_IDENTITY @builtin = matrix::IDENTITY3_RM {float};
alias MATRIX4_IDENTITY  @builtin = matrix::IDENTITY4_RM {double};
alias MATRIX4F_IDENTITY @builtin = matrix::IDENTITY4_RM {float};

<*
 The generic matrix module, for float or double based matrix definitions.

 @require Real.kindof == FLOAT : "A matrix must use a floating type"
*>
module std::math::matrix <Real>;
import std::math::vector, std::io;

struct Matrix2x2 (Printable)
{
	union
	{
		struct
		{
			Real m00, m10;
			Real m01, m11;
		}
		Real[<4>] m;
	}
}

struct Matrix3x3 (Printable)
{
	union
	{
		struct
		{
			Real m00, m10, m20;
			Real m01, m11, m21;
			Real m02, m12, m22;
		}
		Real[<9>] m;
	}
}

struct Matrix4x4 (Printable)
{
	union
	{
		struct
		{
			Real m00, m10, m20, m30;
			Real m01, m11, m21, m31;
			Real m02, m12, m22, m32;
			Real m03, m13, m23, m33;
		}
		Real[<16>] m;
	}
}

fn usz? Matrix4x4.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("[%g,%g,%g,%g|%g,%g,%g,%g|%g,%g,%g,%g|%g,%g,%g,%g]",
		self.m00, self.m01, self.m02, self.m03,
		self.m10, self.m11, self.m12, self.m13,
		self.m20, self.m21, self.m22, self.m23,
		self.m30, self.m31, self.m32, self.m33);
}

fn usz? Matrix3x3.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("[%g,%g,%g|%g,%g,%g|%g,%g,%g]",
		self.m00, self.m01, self.m02,
		self.m10, self.m11, self.m12,
		self.m20, self.m21, self.m22);
}

fn usz? Matrix2x2.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("[%g,%g|%g,%g]",
		self.m00, self.m01,
		self.m10, self.m11);
}


fn Real[<2>] Matrix2x2.apply(self, Real[<2>] vec) @operator(*)
{
	Real[<4>] m = self.m;
	return m.xy * vec.xx + m.zw * vec.yy;
}

fn Real[<3>] Matrix3x3.apply(&self, Real[<3>] vec) @operator(*)
{
	Real[<9>] m = self.m;
	return {
		m[0] * vec.x + m[1] * vec.y + m[2] * vec.z,
		m[3] * vec.x + m[4] * vec.y + m[5] * vec.z,
		m[6] * vec.x + m[7] * vec.y + m[8] * vec.z,
	};
}

fn Real[<4>] Matrix4x4.apply(&self, Real[<4>] vec) @operator(*)
{
	Real[<16>] m = self.m;
	return {
	    m[0]  * vec.x + m[4]  * vec.y + m[8]  * vec.z + m[12] * vec.w,
        m[1]  * vec.x + m[5]  * vec.y + m[9]  * vec.z + m[13] * vec.w,
        m[2]  * vec.x + m[6]  * vec.y + m[10] * vec.z + m[14] * vec.w,
        m[3]  * vec.x + m[7]  * vec.y + m[11] * vec.z + m[15] * vec.w,
	};
}


fn Matrix2x2 Matrix2x2.mul(self, Matrix2x2 b) @operator(*)
{
	return { .m = $$matrix_mul(self.m, b.m, 2, 2, 2) };
}

fn Matrix3x3 Matrix3x3.mul(&self, Matrix3x3 b) @operator(*)
{
	return { .m = $$matrix_mul(self.m, b.m, 3, 3, 3) };
}

fn Matrix4x4 Matrix4x4.mul(Matrix4x4* self, Matrix4x4 b) @operator(*)
{
	return { .m = $$matrix_mul(self.m, b.m, 4, 4, 4) };
}

fn Matrix2x2 Matrix2x2.component_mul(&self, Real v) @operator(*) => { .m = self.m * v };
fn Matrix3x3 Matrix3x3.component_mul(&self, Real v) @operator(*) => { .m = self.m * v };
fn Matrix4x4 Matrix4x4.component_mul(&self, Real v) @operator(*) => { .m = self.m * v };

fn void Matrix2x2.component_mul_inplace(&self, Real v) @operator(*=) => self.m *= v;
fn void Matrix3x3.component_mul_inplace(&self, Real v) @operator(*=) => self.m *= v;
fn void Matrix4x4.component_mul_inplace(&self, Real v) @operator(*=) => self.m *= v;

fn Matrix2x2 Matrix2x2.add(&self, Matrix2x2 mat2) @operator(+) => { .m = self.m + mat2.m };
fn Matrix3x3 Matrix3x3.add(&self, Matrix3x3 mat2) @operator(+) => { .m = self.m + mat2.m };
fn Matrix4x4 Matrix4x4.add(&self, Matrix4x4 mat2) @operator(+) => { .m = self.m + mat2.m };

fn void Matrix2x2.add_inplace(&self, Matrix2x2 mat2) @operator(+=) => self.m += mat2.m;
fn void Matrix3x3.add_inplace(&self, Matrix3x3 mat2) @operator(+=) => self.m += mat2.m;
fn void Matrix4x4.add_inplace(&self, Matrix4x4 mat2) @operator(+=) => self.m += mat2.m;

fn Matrix2x2 Matrix2x2.sub(&self, Matrix2x2 mat2) @operator(-) => { .m = self.m - mat2.m };
fn Matrix3x3 Matrix3x3.sub(&self, Matrix3x3 mat2) @operator(-) => { .m = self.m - mat2.m };
fn Matrix4x4 Matrix4x4.sub(&self, Matrix4x4 mat2) @operator(-) => { .m = self.m - mat2.m };

fn void Matrix2x2.sub_inplace(&self, Matrix2x2 mat2) @operator(-=) => self.m -= mat2.m;
fn void Matrix3x3.sub_inplace(&self, Matrix3x3 mat2) @operator(-=) => self.m -= mat2.m;
fn void Matrix4x4.sub_inplace(&self, Matrix4x4 mat2) @operator(-=) => self.m -= mat2.m;

fn Matrix2x2 Matrix2x2.negate(&self) @operator(-) => { .m = -self.m };
fn Matrix3x3 Matrix3x3.negate(&self) @operator(-) => { .m = -self.m };
fn Matrix4x4 Matrix4x4.negate(&self) @operator(-) => { .m = -self.m };

fn bool Matrix2x2.eq(&self, Matrix2x2 mat2) @operator(==) => self.m == mat2.m;
fn bool Matrix3x3.eq(&self, Matrix3x3 mat2) @operator(==) => (Real[<9>])self.m == (Real[<9>])mat2.m;
fn bool Matrix4x4.eq(&self, Matrix4x4 mat2) @operator(==) => (Real[<16>])self.m == (Real[<16>])mat2.m;

fn bool Matrix2x2.neq(&self, Matrix2x2 mat2) @operator(!=) => self.m != mat2.m;
fn bool Matrix3x3.neq(&self, Matrix3x3 mat2) @operator(!=) => (Real[<9>])self.m != (Real[<9>])mat2.m;
fn bool Matrix4x4.neq(&self, Matrix4x4 mat2) @operator(!=) => (Real[<16>])self.m != (Real[<16>])mat2.m;

fn Matrix4x4 look_at(Real[<3>] eye, Real[<3>] target, Real[<3>] up) => matrix_look_at(Matrix4x4, eye, target, up);

fn Matrix2x2 Matrix2x2.transpose(&self)
{
	return { .m = self.m.xzyw };
}

fn Matrix3x3 Matrix3x3.transpose(&self)
{
	return { .m = swizzle(self.m, 0, 3, 6, 1, 4, 7, 2, 5, 8) };
}

fn Matrix4x4 Matrix4x4.transpose(&self)
{
	return { .m = swizzle(self.m, 0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15) };
}

fn Real Matrix2x2.determinant(self)
{
	Real[<4>] m = self.m;
	return m.x * m.w - m.y * m.z;
}

fn Real Matrix3x3.determinant(&self)
{
	Real[<9>] m = self.m;
	return
		m[0] * (m[4] * m[8] - m[5] * m[7]) -
		m[3] * (m[1] * m[8] - m[2] * m[7]) +
		m[6] * (m[1] * m[5] - m[2] * m[4]);
}

fn Real Matrix4x4.determinant(&self)
{
	Real[<16>] m = self.m;
	return
		m[0] * (
			m[5] * (m[10] * m[15] - m[11] * m[14]) -
			m[9] * (m[6]  * m[15] - m[7]  * m[14]) +
			m[13] * (m[6] * m[11] - m[7]  * m[10])
		)
		- m[4] * (
			m[1] * (m[10] * m[15] - m[11] * m[14]) -
			m[9] * (m[2]  * m[15] - m[3]  * m[14]) +
			m[13] * (m[2] * m[11] - m[3]  * m[10])
		)
		+ m[8] * (
			m[1] * (m[6] * m[15] - m[7] * m[14]) -
			m[5] * (m[2] * m[15] - m[3] * m[14]) +
			m[13] * (m[2] * m[7] - m[3] * m[6])
		)
		- m[12] * (
			m[1] * (m[6] * m[11] - m[7] * m[10]) -
			m[5] * (m[2] * m[11] - m[3] * m[10]) +
			m[9] * (m[2] * m[7] - m[3] * m[6])
		);
}

fn Matrix2x2 Matrix2x2.adjoint(self)
{
	return { .m = { self.m.w, -self.m.y, -self.m.z, self.m.x } };
}

fn Matrix3x3 Matrix3x3.adjoint(&self)
{
	Real[<9>] m = self.m;
	return { .m = {
		(m[4] * m[8] - m[5] * m[7]),
		-(m[1] * m[8] - m[2] * m[7]),
		(m[1] * m[5] - m[2] * m[4]),

		-(m[3] * m[8] - m[5] * m[6]),
		(m[0] * m[8] - m[2] * m[6]),
		-(m[0] * m[5] - m[2] * m[3]),

		(m[3] * m[7] - m[4] * m[6]),
		-(m[0] * m[7] - m[1] * m[6]),
		(m[0] * m[4] - m[1] * m[3]),
	} };
}


fn Matrix4x4 Matrix4x4.adjoint(&self)
{
	Real[<16>] m = self.m;
	return { .m = {
		(m[5] * (m[10] * m[15] - m[11] * m[14]) -
		 m[9] * (m[6]  * m[15] - m[7]  * m[14]) +
		 m[13] * (m[6] * m[11] - m[7] * m[10])),

		-(m[1] * (m[10] * m[15] - m[11] * m[14]) -
		  m[9] * (m[2]  * m[15] - m[3]  * m[14]) +
		  m[13] * (m[2] * m[11] - m[3] * m[10])),

		(m[1] * (m[6] * m[15] - m[7] * m[14]) -
		 m[5] * (m[2] * m[15] - m[3] * m[14]) +
		 m[13] * (m[2] * m[7] - m[3] * m[6])),

		-(m[1] * (m[6] * m[11] - m[7] * m[10]) -
		  m[5] * (m[2] * m[11] - m[3] * m[10]) +
		  m[9] * (m[2] * m[7] - m[3] * m[6])),

		-(m[4] * (m[10] * m[15] - m[11] * m[14]) -
		  m[8] * (m[6]  * m[15] - m[7]  * m[14]) +
		  m[12] * (m[6] * m[11] - m[7] * m[10])),

		(m[0] * (m[10] * m[15] - m[11] * m[14]) -
		 m[8] * (m[2]  * m[15] - m[3]  * m[14]) +
		 m[12] * (m[2] * m[11] - m[3] * m[10])),

		-(m[0] * (m[6] * m[15] - m[7] * m[14]) -
		  m[4] * (m[2] * m[15] - m[3] * m[14]) +
		  m[12] * (m[2] * m[7] - m[3] * m[6])),

		(m[0] * (m[6] * m[11] - m[7] * m[10]) -
		 m[4] * (m[2] * m[11] - m[3] * m[10]) +
		 m[8] * (m[2] * m[7] - m[3] * m[6])),

		(m[4] * (m[9] * m[15] - m[11] * m[13]) -
		 m[8] * (m[5] * m[15] - m[7] * m[13]) +
		 m[12] * (m[5] * m[11] - m[7] * m[9])),

		-(m[0] * (m[9] * m[15] - m[11] * m[13]) -
		  m[8] * (m[1] * m[15] - m[3] * m[13]) +
		  m[12] * (m[1] * m[11] - m[3] * m[9])),

		(m[0] * (m[5] * m[15] - m[7] * m[13]) -
		 m[4] * (m[1] * m[15] - m[3] * m[13]) +
		 m[12] * (m[1] * m[7] - m[3] * m[5])),

		-(m[0] * (m[5] * m[11] - m[7] * m[9]) -
		  m[4] * (m[1] * m[11] - m[3] * m[9]) +
		  m[8] * (m[1] * m[7] - m[3] * m[5])),

		-(m[4] * (m[9] * m[14] - m[10] * m[13]) -
		  m[8] * (m[5] * m[14] - m[6] * m[13]) +
		  m[12] * (m[5] * m[10] - m[6] * m[9])),

		(m[0] * (m[9] * m[14] - m[10] * m[13]) -
		 m[8] * (m[1] * m[14] - m[2] * m[13]) +
		 m[12] * (m[1] * m[10] - m[2] * m[9])),

		-(m[0] * (m[5] * m[14] - m[6] * m[13]) -
		  m[4] * (m[1] * m[14] - m[2] * m[13]) +
		  m[12] * (m[1] * m[6] - m[2] * m[5])),

		(m[0] * (m[5] * m[10] - m[6] * m[9]) -
		 m[4] * (m[1] * m[10] - m[2] * m[9]) +
		 m[8] * (m[1] * m[6] - m[2] * m[5])),
	} };
}


fn Matrix2x2? Matrix2x2.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	Matrix2x2 adj = self.adjoint();
	return adj.component_mul(1 / det);
}

fn Matrix3x3? Matrix3x3.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	Matrix3x3 adj = self.adjoint();
	return adj.component_mul(1 / det);
}

fn Matrix4x4? Matrix4x4.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	Matrix4x4 adj = self.adjoint();
	return adj.component_mul(1 / det);
}

fn Matrix3x3 Matrix3x3.translate(&self, Real[<2>] v)
{
	Real[<9>] m = self.m;
	m[6] = m[0] * v.x + m[3] * v.y + m[6];
	m[7] = m[1] * v.x + m[4] * v.y + m[7];
	m[8] = m[2] * v.x + m[5] * v.y + m[8];
    return { .m = m };
}

fn Matrix4x4 Matrix4x4.translate(&self, Real[<3>] v)
{
	Real[<16>] m = self.m;
	m[12] = m[0] * v.x + m[4] * v.y + m[8]  * v.z + m[12];
	m[13] = m[1] * v.x + m[5] * v.y + m[9]  * v.z + m[13];
	m[14] = m[2] * v.x + m[6] * v.y + m[10] * v.z + m[14];
	m[15] = m[3] * v.x + m[7] * v.y + m[11] * v.z + m[15];
    return { .m = m };
}

// r in radians
fn Matrix3x3 Matrix3x3.rotate(&self, Real r)
{
	Real c = math::cos(r);
	Real s = math::sin(r);
	return self.mul({ .m = { c, s, 0, -s, c, 0, 0, 0, 1 }});
}


// r in radians
fn Matrix4x4 Matrix4x4.rotate_z(&self, Real r)
{
	Real c = math::cos(r);
	Real s = math::sin(r);
	return self.mul({ .m = { c, s, 0, 0, -s, c, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 }});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_y(&self, Real r)
{
	Real c = math::cos(r);
	Real s = math::sin(r);
	return self.mul({ .m = {
		c, 0, -s, 0,
		0, 1, 0, 0,
		s, 0, c, 0,
		0, 0, 0, 1,
	}});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_x(&self, Real r)
{
	Real c = math::cos(r);
	Real s = math::sin(r);
	return self.mul({ .m = {
		1, 0, 0, 0,
		0, c, s, 0,
		0, -s, c, 0,
		0, 0, 0, 1,
	}});
}

fn Real Matrix2x2.trace(&self) => self.m.xw.sum();
fn Real Matrix3x3.trace(&self)
{
    Real[<9>] m = self.m;
    return m[0] + m[4] + m[8];
}
fn Real Matrix4x4.trace(&self)
{
    Real[<16>] m = self.m;
    return m[0] + m[5] + m[10] + m[15];
}

fn Matrix3x3 Matrix3x3.scale(&self, Real[<2>] v)
{
	return { .m = self.m * { v.x, v.x, v.x, v.y, v.y, v.y, 1, 1, 1 } };
}

fn Matrix4x4 Matrix4x4.scale(&self, Real[<3>] v)
{
	return { .m = self.m * { v.x, v.x, v.x, v.x, v.y, v.y, v.y, v.y, v.z, v.z, v.z, v.z, 1, 1, 1, 1 } };
}

// Implementation is RH [0, 1] based on glm's orthoRH_ZO
fn Matrix4x4 ortho(Real left, Real right, Real bottom, Real top,  Real z_near, Real z_far)
{
	Real width_inv = 1 / (right - left);
	Real height_inv = 1 / (top - bottom);
	Real depth_inv = 1 / (z_far - z_near);
	return { .m = {
		[0] = 2 * width_inv,
		[5] = 2 * height_inv,
		[10] = - depth_inv,
		[12] = - (right + left) * width_inv,
		[13] = - (top + bottom) * height_inv,
		[14] = - z_near * depth_inv,
		[15] = 1, } };
}

// Implements right-handed [0, 1] perspective
fn Matrix4x4 perspective(Real fovy, Real aspect, Real z_near, Real z_far)
{
	Real tan_half_fovy = math::tan(fovy / 2);

    Real fn_scale = 1 / (z_near - z_far);

	return (Matrix4x4) { .m = {
		[0] = 1 / (aspect * tan_half_fovy),
		[5] = 1 / (tan_half_fovy),
		[10] = z_far * fn_scale,
		[11] = -1,
		[14] = z_far * z_near * fn_scale,
	} };
}


const Matrix2x2 IDENTITY2 = { .m = { 1, 0, 0, 1 } };
const Matrix3x3 IDENTITY3 = { .m = { [0] = 1, [4] = 1, [8] = 1 } };
const Matrix4x4 IDENTITY4 = { .m = { [0] = 1, [5] = 1, [10] = 1, [15] = 1 } };

macro matrix_look_at($Type, eye, target, up) @private
{
	var vz = (eye - target).normalize();
	var vx = up.cross(vz).normalize();
	var vy = vz.cross(vx);
	return ($Type){ .m = {
		vx[0], vy[0], vz[0], 0.0,
		vx[1], vy[1], vz[1], 0.0,
		vx[2], vy[2], vz[2], 0.0,
		-(Real)vx.dot(eye), -(Real)vy.dot(eye), -(Real)vz.dot(eye), 1.0
	}};
}

