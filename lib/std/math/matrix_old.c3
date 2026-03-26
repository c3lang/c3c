module std::math;

// Predefined matrix types
alias MatrixRM2f = MatrixRM2x2 {float};
alias MatrixRM2  = MatrixRM2x2 {double};
alias MatrixRM3f = MatrixRM3x3 {float};
alias MatrixRM3  = MatrixRM3x3 {double};
alias MatrixRM4f = MatrixRM4x4 {float};
alias MatrixRM4  = MatrixRM4x4 {double};
<*
 The generic matrix module, for float or double based matrix definitions.

 @require Real.kindof == FLOAT : "A matrix must use a floating type"
*>
module std::math::matrix <Real>;
import std::math::vector;

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

fn bool MatrixRM2x2.eq(&self, MatrixRM2x2 mat2) @operator(==) => (Real[<4>])self.m == (Real[<4>])mat2.m;
fn bool MatrixRM3x3.eq(&self, MatrixRM3x3 mat2) @operator(==) => (Real[<9>])self.m == (Real[<9>])mat2.m;
fn bool MatrixRM4x4.eq(&self, MatrixRM4x4 mat2) @operator(==) => (Real[<16>])self.m == (Real[<16>])mat2.m;

fn bool MatrixRM2x2.neq(&self, MatrixRM2x2 mat2) @operator(!=) => (Real[<4>])self.m != (Real[<4>])mat2.m;
fn bool MatrixRM3x3.neq(&self, MatrixRM3x3 mat2) @operator(!=) => (Real[<9>])self.m != (Real[<9>])mat2.m;
fn bool MatrixRM4x4.neq(&self, MatrixRM4x4 mat2) @operator(!=) => (Real[<16>])self.m != (Real[<16>])mat2.m;

fn MatrixRM4x4 MatrixRM4x4.adjoint(&self)
{
	return {
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
	};
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

fn Real[<2>] MatrixRM2x2.apply(&self, Real[<2>] vec) @operator(*)
{
	return {
		self.m00 * vec[0] + self.m01 * vec[1],
		self.m10 * vec[0] + self.m11 * vec[1],
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

fn MatrixRM4x4 MatrixRM4x4.mul(MatrixRM4x4* self, MatrixRM4x4 b) @operator(*)
{
	return {
		self.m00 * b.m00 + self.m01 * b.m10 + self.m02 * b.m20 + self.m03 * b.m30,
		self.m00 * b.m01 + self.m01 * b.m11 + self.m02 * b.m21 + self.m03 * b.m31,
		self.m00 * b.m02 + self.m01 * b.m12 + self.m02 * b.m22 + self.m03 * b.m32,
		self.m00 * b.m03 + self.m01 * b.m13 + self.m02 * b.m23 + self.m03 * b.m33,
			
		self.m10 * b.m00 + self.m11 * b.m10 + self.m12 * b.m20 + self.m13 * b.m30,
		self.m10 * b.m01 + self.m11 * b.m11 + self.m12 * b.m21 + self.m13 * b.m31,
		self.m10 * b.m02 + self.m11 * b.m12 + self.m12 * b.m22 + self.m13 * b.m32,
		self.m10 * b.m03 + self.m11 * b.m13 + self.m12 * b.m23 + self.m13 * b.m33,
			
		self.m20 * b.m00 + self.m21 * b.m10 + self.m22 * b.m20 + self.m23 * b.m30,
		self.m20 * b.m01 + self.m21 * b.m11 + self.m22 * b.m21 + self.m23 * b.m31,
		self.m20 * b.m02 + self.m21 * b.m12 + self.m22 * b.m22 + self.m23 * b.m32,
		self.m20 * b.m03 + self.m21 * b.m13 + self.m22 * b.m23 + self.m23 * b.m33,
			
		self.m30 * b.m00 + self.m31 * b.m10 + self.m32 * b.m20 + self.m33 * b.m30,
		self.m30 * b.m01 + self.m31 * b.m11 + self.m32 * b.m21 + self.m33 * b.m31,
		self.m30 * b.m02 + self.m31 * b.m12 + self.m32 * b.m22 + self.m33 * b.m32,
		self.m30 * b.m03 + self.m31 * b.m13 + self.m32 * b.m23 + self.m33 * b.m33,
	};
}

fn MatrixRM4x4 look_at_rm(Real[<3>] eye, Real[<3>] target, Real[<3>] up) => matrix_look_at_rm(MatrixRM4x4, eye, target, up);


fn MatrixRM2x2 MatrixRM2x2.transpose(&self)
{
	return {
		self.m00, self.m10,
		self.m01, self.m11
	};
}

fn MatrixRM3x3 MatrixRM3x3.transpose(&self)
{
	return {
		self.m00, self.m10, self.m20,
		self.m01, self.m11, self.m21,
		self.m02, self.m12, self.m22,
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


fn Real MatrixRM2x2.determinant(&self)
{
	return self.m00 * self.m11 - self.m01 * self.m10;
}

fn Real MatrixRM3x3.determinant(&self)
{
	return
		self.m00 * (self.m11 * self.m22 - self.m21 * self.m12) -
		self.m01 * (self.m10 * self.m22 - self.m20 * self.m12) +
		self.m02 * (self.m10 * self.m21 - self.m20 * self.m11);
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


fn MatrixRM2x2 MatrixRM2x2.adjoint(&self)
{
	return { self.m11, -self.m01, -self.m10, self.m00 };
}

fn MatrixRM3x3 MatrixRM3x3.adjoint(&self)
{
	return {
		(self.m11 * self.m22 - self.m21 * self.m12),
		-(self.m10 * self.m22 - self.m20 * self.m12),
		(self.m10 * self.m21 - self.m20 * self.m11),
			
		-(self.m01 * self.m22 - self.m21 * self.m02),
		(self.m00 * self.m22 - self.m20 * self.m02),
		-(self.m00 * self.m21 - self.m20 * self.m01),
			
		(self.m01 * self.m12 - self.m11 * self.m02),
		-(self.m00 * self.m12 - self.m10 * self.m02),
		(self.m00 * self.m11 - self.m10 * self.m01),
	};
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

fn MatrixRM3x3 MatrixRM3x3.translate(&self, Real[<2>] v)
{
	return self.mul({
		1, 0, v[0],
		0, 1, v[1],
		0, 0, 1,
	});
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
fn MatrixRM4x4 MatrixRM4x4.rotate_x(&self, Real r)
{
	return self.mul({
		1, 0, 0, 0,
		0, math::cos(r), -math::sin(r), 0,
		0, math::sin(r), math::cos(r), 0,
		0, 0, 0, 1,
	});
}


fn MatrixRM3x3 MatrixRM3x3.scale(&self, Real[<2>] v)
{
	return self.mul({
		v[0], 0, 0,
		0, v[1], 0,
		0, 0, 1,
	});
}

fn MatrixRM2x2? MatrixRM2x2.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	MatrixRM2x2 adj = self.adjoint();
	return adj.component_mul(1 / det).transpose();
}

fn MatrixRM3x3? MatrixRM3x3.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	MatrixRM3x3 adj = self.adjoint();
	return adj.component_mul(1 / det).transpose();
}

fn MatrixRM4x4? MatrixRM4x4.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return math::MATRIX_INVERSE_DOESNT_EXIST~;
	MatrixRM4x4 adj = self.adjoint();
	return adj.component_mul(1 / det).transpose();
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
fn MatrixRM4x4 MatrixRM4x4.rotate_z(&self, Real r)
{
	return self.mul({
		math::cos(r), -math::sin(r), 0, 0,
		math::sin(r), math::cos(r), 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1,
	});
}

fn Real MatrixRM2x2.trace(&self) => self.m00 + self.m11;
fn Real MatrixRM3x3.trace(&self) => self.m00 + self.m11 + self.m22;
fn Real MatrixRM4x4.trace(&self) => self.m00 + self.m11 + self.m22 + self.m33;

fn MatrixRM4x4 MatrixRM4x4.scale(&self, Real[<3>] v)
{
	return self.mul({
		v[0], 0, 0, 0,
		0, v[1], 0, 0,
		0, 0, v[2], 0,
		0, 0, 0, 1,
	});
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
fn MatrixRM4x4 perspective_rm(Real fov, Real aspect_ratio, Real near, Real far)
{
	Real f = (Real)math::tan(math::PI * 0.5 - 0.5 * fov);
	Real range_inv = (Real)1.0 / (near - far);

	return {
		f / aspect_ratio, 0, 0, 0,
		0, f, 0, 0,
		0, 0, (near + far) * range_inv,  near * far * range_inv * 2,
		0, 0, -1, 0,
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

macro matrix_sub(mat, mat2) @local
{
	var $Type = Real[<$typeof(mat.m).len>];
	return ($typeof(*mat)) { .m = ($Type)mat.m - ($Type)mat2.m };
}

macro matrix_component_mul(mat, val) @local
{
	var $Type = Real[<$typeof(mat.m).len>];
	return ($typeof(*mat)) { .m = val * ($Type)mat.m };
}

macro matrix_add(mat, mat2) @local
{
	var $Type = Real[<$typeof(mat.m).len>];
	return ($typeof(*mat)) { .m = ($Type)mat.m + ($Type)mat2.m };
}

const MatrixRM2x2 IDENTITY2_RM = { .m = { 1, 0, 0, 1 } };
const MatrixRM3x3 IDENTITY3_RM = { .m = { [0] = 1, [4] = 1, [8] = 1 } };
const MatrixRM4x4 IDENTITY4_RM = { .m = { [0] = 1, [5] = 1, [10] = 1, [15] = 1 } };
