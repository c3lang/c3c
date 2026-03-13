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

struct MatrixRM2x2
{
	union
	{
		struct
		{
			Real m00, m01;
			Real m10, m11;
		}
		Real[4] m;
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

struct MatrixRM3x3
{
	union
	{
		struct
		{
			Real m00, m01, m02;
			Real m10, m11, m12;
			Real m20, m21, m22;
		}
		Real[9] m;
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

struct MatrixRM4x4
{
	union
	{
		struct
		{
			Real m00, m01, m02, m03;
			Real m10, m11, m12, m13;
			Real m20, m21, m22, m23;
			Real m30, m31, m32, m33;
		}
		Real[16] m;
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


fn Real[<2>] Matrix2x2.apply(&self, Real[<2>] vec) @operator(*)
{
	Real[<4>] m = self.m;
	return m.xy * vec.xx + m.zw * vec.yy;
}

fn Real[<2>] MatrixRM2x2.apply(&self, Real[<2>] vec) @operator(*)
{
	return {
		self.m00 * vec[0] + self.m01 * vec[1],
		self.m10 * vec[0] + self.m11 * vec[1],
	};
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

fn Real[<3>] MatrixRM3x3.apply(&self, Real[<3>] vec) @operator(*)
{
	return {
		self.m00 * vec[0] + self.m01 * vec[1] + self.m02 * vec[2],
		self.m10 * vec[0] + self.m11 * vec[1] + self.m12 * vec[2],
		self.m20 * vec[0] + self.m21 * vec[1] + self.m22 * vec[2],
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

fn Real[<4>] MatrixRM4x4.apply(&self, Real[<4>] vec) @operator(*)
{
	return {
		self.m00 * vec[0] + self.m01 * vec[1] + self.m02 * vec[2] + self.m03 * vec[3],
		self.m10 * vec[0] + self.m11 * vec[1] + self.m12 * vec[2] + self.m13 * vec[3],
		self.m20 * vec[0] + self.m21 * vec[1] + self.m22 * vec[2] + self.m23 * vec[3],
		self.m30 * vec[0] + self.m31 * vec[1] + self.m32 * vec[2] + self.m33 * vec[3],
	};
}


fn MatrixRM2x2 MatrixRM2x2.mul(&self, MatrixRM2x2 b) @operator(*)
{
	return {
		self.m00 * b.m00 + self.m01 * b.m10, self.m00 * b.m01 + self.m01 * b.m11,
		self.m10 * b.m00 + self.m11 * b.m10, self.m10 * b.m01 + self.m11 * b.m11,
	};
}

fn Matrix2x2 Matrix2x2.mul(&self, Matrix2x2 b) @operator(*)
{
	Real[<4>] m = self.m;
	Real[<4>] m2 = b.m;
	return { .m = m.xyxy * m2.xxzz + m.zwzw * m2.yyww };
}

fn Matrix3x3 Matrix3x3.mul(&self, Matrix3x3 b) @operator(*)
{
	Real[<9>] m1 = self.m;
	Real[<9>] m2 = b.m;
	return { .m = {
		m1[0] * m2[0] + m1[3] * m2[1] + m1[6] * m2[2],
		m1[1] * m2[0] + m1[4] * m2[1] + m1[7] * m2[2],
		m1[2] * m2[0] + m1[5] * m2[1] + m1[8] * m2[2],

		m1[0] * m2[3] + m1[3] * m2[4] + m1[6] * m2[5],
		m1[1] * m2[3] + m1[4] * m2[4] + m1[7] * m2[5],
		m1[2] * m2[3] + m1[5] * m2[4] + m1[8] * m2[5],

		m1[0] * m2[6] + m1[3] * m2[7] + m1[6] * m2[8],
		m1[1] * m2[6] + m1[4] * m2[7] + m1[7] * m2[8],
		m1[2] * m2[6] + m1[5] * m2[7] + m1[8] * m2[8],
	}};
}

fn MatrixRM3x3 MatrixRM3x3.mul(&self, MatrixRM3x3 b) @operator(*)
{
	return {
		self.m00 * b.m00 + self.m01 * b.m10 + self.m02 * b.m20,
		self.m00 * b.m01 + self.m01 * b.m11 + self.m02 * b.m21,
		self.m00 * b.m02 + self.m01 * b.m12 + self.m02 * b.m22,

		self.m10 * b.m00 + self.m11 * b.m10 + self.m12 * b.m20,
		self.m10 * b.m01 + self.m11 * b.m11 + self.m12 * b.m21,
		self.m10 * b.m02 + self.m11 * b.m12 + self.m12 * b.m22,

		self.m20 * b.m00 + self.m21 * b.m10 + self.m22 * b.m20,
		self.m20 * b.m01 + self.m21 * b.m11 + self.m22 * b.m21,
		self.m20 * b.m02 + self.m21 * b.m12 + self.m22 * b.m22,
	};
}


fn Matrix4x4 Matrix4x4.mul(Matrix4x4* self, Matrix4x4 b) @operator(*)
{
	Real[<16>] m1 = self.m;
	Real[<16>] m2 = b.m;
	return { .m = {
		m1[0] * m2[0]  +  m1[4] * m2[1]  +  m1[8]  * m2[2]   +  m1[12] * m2[3],
		m1[1] * m2[0]  +  m1[5] * m2[1]  +  m1[9]  * m2[2]   +  m1[13] * m2[3],
		m1[2] * m2[0]  +  m1[6] * m2[1]  +  m1[10] * m2[2]   +  m1[14] * m2[3],
		m1[3] * m2[0]  +  m1[7] * m2[1]  +  m1[11] * m2[2]   +  m1[15] * m2[3],

		m1[0] * m2[4]  +  m1[4] * m2[5]  +  m1[8]  * m2[6]   +  m1[12] * m2[7],
		m1[1] * m2[4]  +  m1[5] * m2[5]  +  m1[9]  * m2[6]   +  m1[13] * m2[7],
		m1[2] * m2[4]  +  m1[6] * m2[5]  +  m1[10] * m2[6]   +  m1[14] * m2[7],
		m1[3] * m2[4]  +  m1[7] * m2[5]  +  m1[11] * m2[6]   +  m1[15] * m2[7],

		m1[0] * m2[8]  +  m1[4] * m2[9]  +  m1[8]  * m2[10]  +  m1[12] * m2[11],
		m1[1] * m2[8]  +  m1[5] * m2[9]  +  m1[9]  * m2[10]  +  m1[13] * m2[11],
		m1[2] * m2[8]  +  m1[6] * m2[9]  +  m1[10] * m2[10]  +  m1[14] * m2[11],
		m1[3] * m2[8]  +  m1[7] * m2[9]  +  m1[11] * m2[10]  +  m1[15] * m2[11],

		m1[0] * m2[12] +  m1[4] * m2[13] +  m1[8]  * m2[14]  +  m1[12] * m2[15],
		m1[1] * m2[12] +  m1[5] * m2[13] +  m1[9]  * m2[14]  +  m1[13] * m2[15],
		m1[2] * m2[12] +  m1[6] * m2[13] +  m1[10] * m2[14]  +  m1[14] * m2[15],
		m1[3] * m2[12] +  m1[7] * m2[13] +  m1[11] * m2[14]  +  m1[15] * m2[15],
	}};
}

fn MatrixRM4x4 MatrixRM4x4.mul(MatrixRM4x4* self, MatrixRM4x4 b) @operator(*)
{
	Real[<16>] m1 = self.m;
	Real[<16>] m2 = b.m;
	return { .m = {
		m1[0] * m2[0]  +  m1[4] * m2[1]  +  m1[8]  * m2[2]   +  m1[12] * m2[3],
		m1[1] * m2[0]  +  m1[5] * m2[1]  +  m1[9]  * m2[2]   +  m1[13] * m2[3],
		m1[2] * m2[0]  +  m1[6] * m2[1]  +  m1[10] * m2[2]   +  m1[14] * m2[3],
		m1[3] * m2[0]  +  m1[7] * m2[1]  +  m1[11] * m2[2]   +  m1[15] * m2[3],

		m1[0] * m2[4]  +  m1[4] * m2[5]  +  m1[8]  * m2[6]   +  m1[12] * m2[7],
		m1[1] * m2[4]  +  m1[5] * m2[5]  +  m1[9]  * m2[6]   +  m1[13] * m2[7],
		m1[2] * m2[4]  +  m1[6] * m2[5]  +  m1[10] * m2[6]   +  m1[14] * m2[7],
		m1[3] * m2[4]  +  m1[7] * m2[5]  +  m1[11] * m2[6]   +  m1[15] * m2[7],

		m1[0] * m2[8]  +  m1[4] * m2[9]  +  m1[8]  * m2[10]  +  m1[12] * m2[11],
		m1[1] * m2[8]  +  m1[5] * m2[9]  +  m1[9]  * m2[10]  +  m1[13] * m2[11],
		m1[2] * m2[8]  +  m1[6] * m2[9]  +  m1[10] * m2[10]  +  m1[14] * m2[11],
		m1[3] * m2[8]  +  m1[7] * m2[9]  +  m1[11] * m2[10]  +  m1[15] * m2[11],

		m1[0] * m2[12] +  m1[4] * m2[13] +  m1[8]  * m2[14]  +  m1[12] * m2[15],
		m1[1] * m2[12] +  m1[5] * m2[13] +  m1[9]  * m2[14]  +  m1[13] * m2[15],
		m1[2] * m2[12] +  m1[6] * m2[13] +  m1[10] * m2[14]  +  m1[14] * m2[15],
		m1[3] * m2[12] +  m1[7] * m2[13] +  m1[11] * m2[14]  +  m1[15] * m2[15],
	}};
}

fn Matrix2x2 Matrix2x2.component_mul(&self, Real s) @operator(*) => matrix_component_mul(self, s);
fn Matrix3x3 Matrix3x3.component_mul(&self, Real s) @operator(*) => matrix_component_mul(self, s);
fn Matrix4x4 Matrix4x4.component_mul(&self, Real s) @operator(*) => matrix_component_mul(self, s);

fn Matrix2x2 Matrix2x2.add(&self, Matrix2x2 mat2) @operator(+) => matrix_add(self, mat2);
fn Matrix3x3 Matrix3x3.add(&self, Matrix3x3 mat2) @operator(+) => matrix_add(self, mat2);
fn Matrix4x4 Matrix4x4.add(&self, Matrix4x4 mat2) @operator(+) => matrix_add(self, mat2);

fn Matrix2x2 Matrix2x2.sub(&self, Matrix2x2 mat2) @operator(-) => matrix_sub(self, mat2);
fn Matrix3x3 Matrix3x3.sub(&self, Matrix3x3 mat2) @operator(-) => matrix_sub(self, mat2);
fn Matrix4x4 Matrix4x4.sub(&self, Matrix4x4 mat2) @operator(-) => matrix_sub(self, mat2);

fn Matrix2x2 Matrix2x2.negate(&self) @operator(-) => { .m = -self.m };
fn Matrix3x3 Matrix3x3.negate(&self) @operator(-) => { .m = -(Real[<9>])self.m };
fn Matrix4x4 Matrix4x4.negate(&self) @operator(-) => { .m = -(Real[<16>])self.m };

fn bool Matrix2x2.eq(&self, Matrix2x2 mat2) @operator(==) => self.m == mat2.m;
fn bool Matrix3x3.eq(&self, Matrix3x3 mat2) @operator(==) => (Real[<9>])self.m == (Real[<9>])mat2.m;
fn bool Matrix4x4.eq(&self, Matrix4x4 mat2) @operator(==) => (Real[<16>])self.m == (Real[<16>])mat2.m;

fn bool Matrix2x2.neq(&self, Matrix2x2 mat2) @operator(!=) => self.m != mat2.m;
fn bool Matrix3x3.neq(&self, Matrix3x3 mat2) @operator(!=) => (Real[<9>])self.m != (Real[<9>])mat2.m;
fn bool Matrix4x4.neq(&self, Matrix4x4 mat2) @operator(!=) => (Real[<16>])self.m != (Real[<16>])mat2.m;

fn Matrix4x4 look_at(Real[<3>] eye, Real[<3>] target, Real[<3>] up) => matrix_look_at(Matrix4x4, eye, target, up);

fn MatrixRM4x4 look_at_rm(Real[<3>] eye, Real[<3>] target, Real[<3>] up) => matrix_look_at_rm(MatrixRM4x4, eye, target, up);


fn Matrix2x2 Matrix2x2.transpose(&self)
{
	return { .m = self.m.xzyw };
}

fn MatrixRM2x2 MatrixRM2x2.transpose(&self)
{
	return {
		self.m00, self.m10,
		self.m01, self.m11
	};
}

fn Matrix3x3 Matrix3x3.transpose(&self)
{
	Real[<9>] m = self.m;
	return { .m = { m[0], m[3], m[6], m[1], m[4], m[7], m[2], m[5], m[8] } };
}

fn MatrixRM3x3 MatrixRM3x3.transpose(&self)
{
	return {
		self.m00, self.m10, self.m20,
		self.m01, self.m11, self.m21,
		self.m02, self.m12, self.m22,
	};
}

fn Matrix4x4 Matrix4x4.transpose(&self)
{
	return {
		self.m00, self.m10, self.m20, self.m30,
		self.m01, self.m11, self.m21, self.m31,
		self.m02, self.m12, self.m22, self.m32,
		self.m03, self.m13, self.m23, self.m33,
	};
}

fn MatrixRM4x4 MatrixRM4x4.transpose(&self)
{
	return {
		self.m00, self.m10, self.m20, self.m30,
		self.m01, self.m11, self.m21, self.m31,
		self.m02, self.m12, self.m22, self.m32,
		self.m03, self.m13, self.m23, self.m33,
	};
}


fn Real Matrix2x2.determinant(self)
{
	Real[<4>] m = self.m;
	return m.x * m.w - m.y * m.z;
}

fn Real MatrixRM2x2.determinant(&self)
{
	return self.m00 * self.m11 - self.m01 * self.m10;
}

fn Real Matrix3x3.determinant(&self)
{
	Real[<9>] m = self.m;
	return
		m[0] * (m[4] * m[8] - m[5] * m[7]) -
		m[3] * (m[1] * m[8] - m[2] * m[7]) +
		m[6] * (m[1] * m[5] - m[2] * m[4]);
}

fn Real MatrixRM3x3.determinant(&self)
{
	return
		self.m00 * (self.m11 * self.m22 - self.m21 * self.m12) -
		self.m01 * (self.m10 * self.m22 - self.m20 * self.m12) +
		self.m02 * (self.m10 * self.m21 - self.m20 * self.m11);
}

fn Real Matrix4x4.determinant(&self)
{
	return
		self.m00 * (self.m11 * (self.m22 * self.m33 - self.m32 * self.m23) -
				   self.m12 * (self.m21 * self.m33 - self.m31 * self.m23) +
				   self.m13 * (self.m21 * self.m32 - self.m31 * self.m22) ) -
		self.m01 * (self.m10 * (self.m22 * self.m33 - self.m32 * self.m23) -
				   self.m12 * (self.m20 * self.m33 - self.m30 * self.m23) +
				   self.m13 * (self.m20 * self.m32 - self.m30 * self.m22) ) +
		self.m02 * (self.m10 * (self.m21 * self.m33 - self.m31 * self.m23) -
				   self.m11 * (self.m20 * self.m33 - self.m30 * self.m23) +
				   self.m13 * (self.m20 * self.m31 - self.m30 * self.m21) ) -
		self.m03 * (self.m10 * (self.m21 * self.m32 - self.m31 * self.m22) -
				   self.m11 * (self.m20 * self.m32 - self.m30 * self.m22) +
				   self.m12 * (self.m20 * self.m31 - self.m30 * self.m21) );
}

fn Real MatrixRM4x4.determinant(&self)
{
	return
		self.m00 * (self.m11 * (self.m22 * self.m33 - self.m32 * self.m23) -
				   self.m12 * (self.m21 * self.m33 - self.m31 * self.m23) +
				   self.m13 * (self.m21 * self.m32 - self.m31 * self.m22) ) -
		self.m01 * (self.m10 * (self.m22 * self.m33 - self.m32 * self.m23) -
				   self.m12 * (self.m20 * self.m33 - self.m30 * self.m23) +
				   self.m13 * (self.m20 * self.m32 - self.m30 * self.m22) ) +
		self.m02 * (self.m10 * (self.m21 * self.m33 - self.m31 * self.m23) -
				   self.m11 * (self.m20 * self.m33 - self.m30 * self.m23) +
				   self.m13 * (self.m20 * self.m31 - self.m30 * self.m21) ) -
		self.m03 * (self.m10 * (self.m21 * self.m32 - self.m31 * self.m22) -
				   self.m11 * (self.m20 * self.m32 - self.m30 * self.m22) +
				   self.m12 * (self.m20 * self.m31 - self.m30 * self.m21) );
}

fn Matrix2x2 Matrix2x2.adjoint(self)
{
	return { .m = { self.m.w, -self.m.y, -self.m.z, self.m.x } };
}

fn MatrixRM2x2 MatrixRM2x2.adjoint(&self)
{
	return { self.m11, -self.m01, -self.m10, self.m00 };
}

fn Matrix3x3 Matrix3x3.adjoint(&self)
{
	return { .m = {
		(self.m11 * self.m22 - self.m21 * self.m12),
		-(self.m10 * self.m22 - self.m20 * self.m12),
		(self.m10 * self.m21 - self.m20 * self.m11),

		-(self.m01 * self.m22 - self.m21 * self.m02),
		(self.m00 * self.m22 - self.m20 * self.m02),
		-(self.m00 * self.m21 - self.m20 * self.m01),

		(self.m01 * self.m12 - self.m11 * self.m02),
		-(self.m00 * self.m12 - self.m10 * self.m02),
		(self.m00 * self.m11 - self.m10 * self.m01),
	} };
}

fn MatrixRM3x3 MatrixRM3x3.adjoint(&self)
{
	return {
		(self.m11 * self.m22 - self.m21 * self.m12),
		-(self.m01 * self.m22 - self.m21 * self.m02),
		(self.m01 * self.m12 - self.m11 * self.m02),

		-(self.m10 * self.m22 - self.m20 * self.m12),
		(self.m00 * self.m22 - self.m20 * self.m02),
		-(self.m00 * self.m12 - self.m10 * self.m02),

		(self.m10 * self.m21 - self.m20 * self.m11),
		-(self.m00 * self.m21 - self.m20 * self.m01),
		(self.m00 * self.m11 - self.m10 * self.m01),
	};
}

fn Matrix4x4 Matrix4x4.adjoint(&self)
{
	return { .m = {
		(self.m11 * (self.m22 * self.m33 - self.m32 * self.m23) -
		 self.m12 * (self.m21 * self.m33 - self.m31 * self.m23) +
		 self.m13 * (self.m21 * self.m32 - self.m31 * self.m22)),
		-(self.m10 * (self.m22 * self.m33 - self.m32 * self.m23) -
		  self.m12 * (self.m20 * self.m33 - self.m30 * self.m23) +
		  self.m13 * (self.m20 * self.m32 - self.m30 * self.m22)),
		(self.m10 * (self.m21 * self.m33 - self.m31 * self.m23) -
		 self.m11 * (self.m20 * self.m33 - self.m30 * self.m23) +
		 self.m13 * (self.m20 * self.m31 - self.m30 * self.m21)),
		-(self.m10 * (self.m21 * self.m32 - self.m31 * self.m22) -
		  self.m11 * (self.m20 * self.m32 - self.m30 * self.m22) +
		  self.m12 * (self.m20 * self.m31 - self.m30 * self.m21)),

		-(self.m01 * (self.m22 * self.m33 - self.m32 * self.m23) -
		  self.m02 * (self.m21 * self.m33 - self.m31 * self.m23) +
		  self.m03 * (self.m21 * self.m32 - self.m31 * self.m22)),
		(self.m00 * (self.m22 * self.m33 - self.m32 * self.m23) -
		 self.m02 * (self.m20 * self.m33 - self.m30 * self.m23) +
		 self.m03 * (self.m20 * self.m32 - self.m30 * self.m22)),
		-(self.m00 * (self.m21 * self.m33 - self.m31 * self.m23) -
		  self.m01 * (self.m20 * self.m33 - self.m30 * self.m23) +
		  self.m03 * (self.m20 * self.m31 - self.m30 * self.m21)),
		(self.m00 * (self.m21 * self.m32 - self.m31 * self.m22) -
		 self.m01 * (self.m20 * self.m32 - self.m30 * self.m22) +
		 self.m02 * (self.m20 * self.m31 - self.m30 * self.m21)),

		(self.m01 * (self.m12 * self.m33 - self.m32 * self.m13) -
		 self.m02 * (self.m11 * self.m33 - self.m31 * self.m13) +
		 self.m03 * (self.m11 * self.m32 - self.m31 * self.m12)),
		-(self.m00 * (self.m12 * self.m33 - self.m32 * self.m13) -
		  self.m02 * (self.m10 * self.m33 - self.m30 * self.m13) +
		  self.m03 * (self.m10 * self.m32 - self.m30 * self.m12)),
		(self.m00 * (self.m11 * self.m33 - self.m31 * self.m13) -
		 self.m01 * (self.m10 * self.m33 - self.m30 * self.m13) +
		 self.m03 * (self.m10 * self.m31 - self.m30 * self.m11)),
		-(self.m00 * (self.m11 * self.m32 - self.m31 * self.m12) -
		  self.m01 * (self.m10 * self.m32 - self.m30 * self.m12) +
		  self.m02 * (self.m10 * self.m31 - self.m30 * self.m11)),

		-(self.m01 * (self.m12 * self.m23 - self.m22 * self.m13) -
		  self.m02 * (self.m11 * self.m23 - self.m21 * self.m13) +
		  self.m03 * (self.m11 * self.m22 - self.m21 * self.m12)),
		(self.m00 * (self.m12 * self.m23 - self.m22 * self.m13) -
		 self.m02 * (self.m10 * self.m23 - self.m20 * self.m13) +
		 self.m03 * (self.m10 * self.m22 - self.m20 * self.m12)),
		-(self.m00 * (self.m11 * self.m23 - self.m21 * self.m13) -
		  self.m01 * (self.m10 * self.m23 - self.m20 * self.m13) +
		  self.m03 * (self.m10 * self.m21 - self.m20 * self.m11)),
		(self.m00 * (self.m11 * self.m22 - self.m21 * self.m12) -
		 self.m01 * (self.m10 * self.m22 - self.m20 * self.m12) +
		 self.m02 * (self.m10 * self.m21 - self.m20 * self.m11)),
	} };
}

fn MatrixRM4x4 MatrixRM4x4.adjoint(&self)
{
	return {
		// row 0
		+ (self.m11*(self.m22*self.m33 - self.m32*self.m23) - self.m12*(self.m21*self.m33 - self.m31*self.m23) + self.m13*(self.m21*self.m32 - self.m31*self.m22)),
		- (self.m01*(self.m22*self.m33 - self.m32*self.m23) - self.m02*(self.m21*self.m33 - self.m31*self.m23) + self.m03*(self.m21*self.m32 - self.m31*self.m22)),
		+ (self.m01*(self.m12*self.m33 - self.m32*self.m13) - self.m02*(self.m11*self.m33 - self.m31*self.m13) + self.m03*(self.m11*self.m32 - self.m31*self.m12)),
		- (self.m01*(self.m12*self.m23 - self.m22*self.m13) - self.m02*(self.m11*self.m23 - self.m21*self.m13) + self.m03*(self.m11*self.m22 - self.m21*self.m12)),

		// row 1
		- (self.m10*(self.m22*self.m33 - self.m32*self.m23) - self.m12*(self.m20*self.m33 - self.m30*self.m23) + self.m13*(self.m20*self.m32 - self.m30*self.m22)),
		+ (self.m00*(self.m22*self.m33 - self.m32*self.m23) - self.m02*(self.m20*self.m33 - self.m30*self.m23) + self.m03*(self.m20*self.m32 - self.m30*self.m22)),
		- (self.m00*(self.m12*self.m33 - self.m32*self.m13) - self.m02*(self.m10*self.m33 - self.m30*self.m13) + self.m03*(self.m10*self.m32 - self.m30*self.m12)),
		+ (self.m00*(self.m12*self.m23 - self.m22*self.m13) - self.m02*(self.m10*self.m23 - self.m20*self.m13) + self.m03*(self.m10*self.m22 - self.m20*self.m12)),

		// row 2
		+ (self.m10*(self.m21*self.m33 - self.m31*self.m23) - self.m11*(self.m20*self.m33 - self.m30*self.m23) + self.m13*(self.m20*self.m31 - self.m30*self.m21)),
		- (self.m00*(self.m21*self.m33 - self.m31*self.m23) - self.m01*(self.m20*self.m33 - self.m30*self.m23) + self.m03*(self.m20*self.m31 - self.m30*self.m21)),
		+ (self.m00*(self.m11*self.m33 - self.m31*self.m13) - self.m01*(self.m10*self.m33 - self.m30*self.m13) + self.m03*(self.m10*self.m31 - self.m30*self.m11)),
		- (self.m00*(self.m11*self.m23 - self.m21*self.m13) - self.m01*(self.m10*self.m23 - self.m20*self.m13) + self.m03*(self.m10*self.m21 - self.m20*self.m11)),

		// row 3
		- (self.m10*(self.m21*self.m32 - self.m31*self.m22) - self.m11*(self.m20*self.m32 - self.m30*self.m22) + self.m12*(self.m20*self.m31 - self.m30*self.m21)),
		+ (self.m00*(self.m21*self.m32 - self.m31*self.m22) - self.m01*(self.m20*self.m32 - self.m30*self.m22) + self.m02*(self.m20*self.m31 - self.m30*self.m21)),
		- (self.m00*(self.m11*self.m32 - self.m31*self.m12) - self.m01*(self.m10*self.m32 - self.m30*self.m12) + self.m02*(self.m10*self.m31 - self.m30*self.m11)),
		+ (self.m00*(self.m11*self.m22 - self.m21*self.m12) - self.m01*(self.m10*self.m22 - self.m20*self.m12) + self.m02*(self.m10*self.m21 - self.m20*self.m11)),
	};
}


fn Matrix2x2? Matrix2x2.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	Matrix2x2 adj = self.adjoint();
	return adj.component_mul(1 / det);
}

fn MatrixRM2x2? MatrixRM2x2.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	MatrixRM2x2 adj = self.adjoint();
	return adj.component_mul(1 / det);
}


fn Matrix3x3? Matrix3x3.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	Matrix3x3 adj = self.adjoint();
	return adj.component_mul(1 / det);
}

fn MatrixRM3x3? MatrixRM3x3.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	MatrixRM3x3 adj = self.adjoint();
	return adj.component_mul(1 / det);
}

fn Matrix4x4? Matrix4x4.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	Matrix4x4 adj = self.adjoint();
	return adj.component_mul(1 / det);
}

fn MatrixRM4x4? MatrixRM4x4.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	MatrixRM4x4 adj = self.adjoint();
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

fn MatrixRM3x3 MatrixRM3x3.translate(&self, Real[<2>] v)
{
	return self.mul({
		1, 0, v[0],
		0, 1, v[1],
		0, 0, 1,
	});
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

fn MatrixRM4x4 MatrixRM4x4.translate(&self, Real[<3>] v)
{
	return self.mul({
		1, 0, 0, v[0],
		0, 1, 0, v[1],
		0, 0, 1, v[2],
		0, 0, 0, 1,
	});
}


// r in radians
fn Matrix3x3 Matrix3x3.rotate(&self, Real r)
{
	return self.mul({ math::cos(r), math::sin(r), 0,
	                 -math::sin(r), math::cos(r), 0,
	                 0, 0, 1,
	});
}

// r in radians
fn MatrixRM3x3 MatrixRM3x3.rotate(&self, Real r)
{
	return self.mul({
		math::cos(r), -math::sin(r), 0,
		math::sin(r), math::cos(r), 0,
		0, 0, 1,
	});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_z(&self, Real r)
{
	return self.mul({
		math::cos(r), math::sin(r), 0, 0,
		-math::sin(r), math::cos(r), 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1,
	});
}

// r in radians
fn MatrixRM4x4 MatrixRM4x4.rotate_z(&self, Real r)
{
	return self.mul({
		math::cos(r), -math::sin(r), 0, 0,
		math::sin(r), math::cos(r), 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1,
	});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_y(&self, Real r)
{
	return self.mul({ .m = {
		math::cos(r), 0, -math::sin(r), 0,
		0, 1, 0, 0,
		math::sin(r), 0, math::cos(r), 0,
		0, 0, 0, 1,
	}});
}

// r in radians
fn MatrixRM4x4 MatrixRM4x4.rotate_y(&self, Real r)
{
	return self.mul({
		math::cos(r), 0, math::sin(r), 0,
		0, 1, 0, 0,
		-math::sin(r), 0, math::cos(r), 0,
		0, 0, 0, 1,
	});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_x(&self, Real r)
{
	return self.mul({ .m = {
		1, 0, 0, 0,
		0, math::cos(r), math::sin(r), 0,
		0, -math::sin(r), math::cos(r), 0,
		0, 0, 0, 1,
	}});
}

// r in radians
fn MatrixRM4x4 MatrixRM4x4.rotate_x(&self, Real r)
{
	return self.mul({
		1, 0, 0, 0,
		0, math::cos(r), -math::sin(r), 0,
		0, math::sin(r), math::cos(r), 0,
		0, 0, 0, 1,
	});
}


fn Matrix3x3 Matrix3x3.scale(&self, Real[<2>] v)
{
	return { .m = self.m * { v.x, v.x, v.x, v.y, v.y, v.y, 1, 1, 1 } };
}

fn MatrixRM3x3 MatrixRM3x3.scale(&self, Real[<2>] v)
{
	return self.mul({
		v[0], 0, 0,
		0, v[1], 0,
		0, 0, 1,
	});
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

fn Real MatrixRM2x2.trace(&self) => self.m00 + self.m11;
fn Real MatrixRM3x3.trace(&self) => self.m00 + self.m11 + self.m22;
fn Real MatrixRM4x4.trace(&self) => self.m00 + self.m11 + self.m22 + self.m33;

fn Matrix4x4 Matrix4x4.scale(&self, Real[<3>] v)
{
	return { .m = self.m * { v.x, v.x, v.x, v.x, v.y, v.y, v.y, v.y, v.z, v.z, v.z, v.z, 1, 1, 1, 1 } };
}

fn MatrixRM4x4 MatrixRM4x4.scale(&self, Real[<3>] v)
{
	return self.mul({
		v[0], 0, 0, 0,
		0, v[1], 0, 0,
		0, 0, v[2], 0,
		0, 0, 0, 1,
	});
}


fn Matrix4x4 ortho(Real left, Real right, Real top, Real bottom, Real near, Real far)
{
	Real width = right - left;
	Real height = top - bottom;
	Real depth = far - near;
	return {
		2 / width,               0,                 0,                              0,
		0,                       2 / height,        0,                              -(top + bottom) / height,
		0,                       0,                 -2 / depth,                     0,
		-(right + left) / width, 0,                 -(far + near) / depth,          1
	};
}

fn MatrixRM4x4 ortho_rm(Real left, Real right, Real top, Real bottom, Real near, Real far)
{
	Real width = right - left;
	Real height = top - bottom;
	Real depth = far - near;
	return {
		2 / width, 0, 0, -(right + left) / width,
		0, 2 / height, 0, -(top + bottom) / height,
		0, 0, -2 / depth, -(far + near) / depth,
		0, 0, 0, 1
	};
}

// fov in radians
fn Matrix4x4 perspective(Real fov, Real aspect_ratio, Real near, Real far)
{
	Real top = (Real)(near * math::tan(fov * 0.5));
    Real bottom = -top;
    Real right = top * aspect_ratio;
    Real left = -right;

    Real rl_scale = 1 / (right - left);
    Real tb_scale = 1 / (top - bottom);
    Real fn_scale = 1 / (far - near);

	return { .m = {
		near * 2 * rl_scale,   0, (right + left) * rl_scale,   0,
		0, near * 2 * tb_scale, (top + bottom) * tb_scale, 0,
		0, 0, -(far + near) * fn_scale, -(far * near * 2) * fn_scale,
		0, 0, -1, 0 } };
}


// fov in radians
fn MatrixRM4x4 perspective_rm(Real fov, Real aspect_ratio, Real near, Real far)
{
	Real f = (Real)math::tan(math::PI * 0.5 - 0.5 * fov);
	Real range_inv = (Real)1.0 / (near - far);

	return {
		f / aspect_ratio, 0, 0, 0,
		0, f, 0, 0,
		0, 0, (near + far) * range_inv, -1,
		0, 0, near * far * range_inv * 2, 0,
	};
}

const Matrix2x2 IDENTITY2 = { .m = { 1, 0, 0, 1 } };
const Matrix3x3 IDENTITY3 = { .m = { [0] = 1, [4] = 1, [8] = 1 } };
const Matrix4x4 IDENTITY4 = { .m = { [0] = 1, [5] = 1, [10] = 1, [15] = 1 } };

const MatrixRM2x2 IDENTITY2_RM = { .m = { 1, 0, 0, 1 } };
const MatrixRM3x3 IDENTITY3_RM = { .m = { [0] = 1, [4] = 1, [8] = 1 } };
const MatrixRM4x4 IDENTITY4_RM = { .m = { [0] = 1, [5] = 1, [10] = 1, [15] = 1 } };

macro matrix_component_mul(mat, val) @private
{
	var $Type = Real[<$typeof(mat.m).len>];
	return ($typeof(*mat)) { .m = val * ($Type)mat.m };
}

macro matrix_add(mat, mat2) @private
{
	var $Type = Real[<$typeof(mat.m).len>];
	return ($typeof(*mat)) { .m = ($Type)mat.m + ($Type)mat2.m };
}

macro matrix_sub(mat, mat2) @private
{
	var $Type = Real[<$typeof(mat.m).len>];
	return ($typeof(*mat)) { .m = ($Type)mat.m - ($Type)mat2.m };
}

macro matrix_look_at($Type, eye, target, up) @private
{
	var vz = (eye - target).normalize();
	var vx = up.cross(vz).normalize();
	var vy = vz.cross(vx);
	return ($Type){
		vx[0], vy[0], vz[0], 0.0,
		vx[1], vy[1], vz[1], 0.0,
		vx[2], vy[2], vz[2], 0.0,
		-(Real)vx.dot(eye), -(Real)vy.dot(eye), -(Real)vz.dot(eye), 1.0
	};
}

macro matrix_look_at_rm($Type, eye, target, up) @private
{
	var vz = (eye - target).normalize();
	var vx = up.cross(vz).normalize();
	var vy = vz.cross(vx);

	return ($Type){
		vx[0], vx[1], vx[2], - (Real)vx.dot(eye),
		vy[0], vy[1], vy[2], - (Real)vy.dot(eye),
		vz[0], vz[1], vz[2], - (Real)vz.dot(eye),
		0.0, 0.0, 0.0, 1
	};
}

fn MatrixRM2x2 MatrixRM2x2.component_mul(&self, Real s) @operator(*) => matrix_component_mul(self, s);
fn MatrixRM3x3 MatrixRM3x3.component_mul(&self, Real s) @operator(*) => matrix_component_mul(self, s);
fn MatrixRM4x4 MatrixRM4x4.component_mul(&self, Real s) @operator(*) => matrix_component_mul(self, s);

fn MatrixRM2x2 MatrixRM2x2.add(&self, MatrixRM2x2 mat2) @operator(+) => matrix_add(self, mat2);
fn MatrixRM3x3 MatrixRM3x3.add(&self, MatrixRM3x3 mat2) @operator(+) => matrix_add(self, mat2);
fn MatrixRM4x4 MatrixRM4x4.add(&self, MatrixRM4x4 mat2) @operator(+) => matrix_add(self, mat2);

fn MatrixRM2x2 MatrixRM2x2.sub(&self, MatrixRM2x2 mat2) @operator(-) => matrix_sub(self, mat2);
fn MatrixRM3x3 MatrixRM3x3.sub(&self, MatrixRM3x3 mat2) @operator(-) => matrix_sub(self, mat2);
fn MatrixRM4x4 MatrixRM4x4.sub(&self, MatrixRM4x4 mat2) @operator(-) => matrix_sub(self, mat2);

fn MatrixRM2x2 MatrixRM2x2.negate(&self) @operator(-) => { .m = -(Real[<4>])self.m };
fn MatrixRM3x3 MatrixRM3x3.negate(&self) @operator(-) => { .m = -(Real[<9>])self.m };
fn MatrixRM4x4 MatrixRM4x4.negate(&self) @operator(-) => { .m = -(Real[<16>])self.m };

fn bool MatrixRM2x2.eq(&self, MatrixRM2x2 mat2) @operator(==) => self.m == mat2.m;
fn bool MatrixRM3x3.eq(&self, MatrixRM3x3 mat2) @operator(==) => (Real[<9>])self.m == (Real[<9>])mat2.m;
fn bool MatrixRM4x4.eq(&self, MatrixRM4x4 mat2) @operator(==) => (Real[<16>])self.m == (Real[<16>])mat2.m;

fn bool MatrixRM2x2.neq(&self, MatrixRM2x2 mat2) @operator(!=) => self.m != mat2.m;
fn bool MatrixRM3x3.neq(&self, MatrixRM3x3 mat2) @operator(!=) => (Real[<9>])self.m != (Real[<9>])mat2.m;
fn bool MatrixRM4x4.neq(&self, MatrixRM4x4 mat2) @operator(!=) => (Real[<16>])self.m != (Real[<16>])mat2.m;
