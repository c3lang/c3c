module std::math::matrix<Real>;

struct Matrix2x2
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

struct Matrix3x3
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

struct Matrix4x4
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

fn Real[<2>] Matrix2x2.apply(Matrix2x2* mat, Real[<2>] vec)
{
	return Real[<2>] {
		mat.m00 * vec[0] + mat.m01 * vec[1],
		mat.m10 * vec[0] + mat.m11 * vec[1],
	};
}

fn Real[<3>] Matrix3x3.apply(Matrix3x3* mat, Real[<3>] vec)
{
	return Real[<3>] {
		mat.m00 * vec[0] + mat.m01 * vec[1] + mat.m02 * vec[2],
		mat.m10 * vec[0] + mat.m11 * vec[1] + mat.m12 * vec[2],
		mat.m20 * vec[0] + mat.m21 * vec[1] + mat.m22 * vec[2],
	};
}

fn Real[<4>] Matrix4x4.apply(Matrix4x4* mat, Real[<4>] vec)
{
	return Real[<4>] {
		mat.m00 * vec[0] + mat.m01 * vec[1] + mat.m02 * vec[2] + mat.m03 * vec[3],
		mat.m10 * vec[0] + mat.m11 * vec[1] + mat.m12 * vec[2] + mat.m13 * vec[3],
		mat.m20 * vec[0] + mat.m21 * vec[1] + mat.m22 * vec[2] + mat.m23 * vec[3],
		mat.m30 * vec[0] + mat.m31 * vec[1] + mat.m32 * vec[2] + mat.m33 * vec[3],
	};
}


fn Matrix2x2 Matrix2x2.mul(Matrix2x2* a, Matrix2x2 b)
{
	return Matrix2x2 {
		a.m00 * b.m00 + a.m01 * b.m10, a.m00 * b.m01 + a.m01 * b.m11,
		a.m10 * b.m00 + a.m11 * b.m10, a.m10 * b.m01 + a.m11 * b.m11,
	};
}

fn Matrix3x3 Matrix3x3.mul(Matrix3x3* a, Matrix3x3 b)
{
	return Matrix3x3 {
		a.m00 * b.m00 + a.m01 * b.m10 + a.m02 * b.m20,
		a.m00 * b.m01 + a.m01 * b.m11 + a.m02 * b.m21,
		a.m00 * b.m02 + a.m01 * b.m12 + a.m02 * b.m22,

		a.m10 * b.m00 + a.m11 * b.m10 + a.m12 * b.m20,
		a.m10 * b.m01 + a.m11 * b.m11 + a.m12 * b.m21,
		a.m10 * b.m02 + a.m11 * b.m12 + a.m12 * b.m22,
			
		a.m20 * b.m00 + a.m21 * b.m10 + a.m22 * b.m20,
		a.m20 * b.m01 + a.m21 * b.m11 + a.m22 * b.m21,
		a.m20 * b.m02 + a.m21 * b.m12 + a.m22 * b.m22,
	};
}

fn Matrix4x4 Matrix4x4.mul(Matrix4x4* a, Matrix4x4 b)
{
	return Matrix4x4 {
		a.m00 * b.m00 + a.m01 * b.m10 + a.m02 * b.m20 + a.m03 * b.m30,
		a.m00 * b.m01 + a.m01 * b.m11 + a.m02 * b.m21 + a.m03 * b.m31,
		a.m00 * b.m02 + a.m01 * b.m12 + a.m02 * b.m22 + a.m03 * b.m32,
		a.m00 * b.m03 + a.m01 * b.m13 + a.m02 * b.m23 + a.m03 * b.m33,
			
		a.m10 * b.m00 + a.m11 * b.m10 + a.m12 * b.m20 + a.m13 * b.m30,
		a.m10 * b.m01 + a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31,
		a.m10 * b.m02 + a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32,
		a.m10 * b.m03 + a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33,
			
		a.m20 * b.m00 + a.m21 * b.m10 + a.m22 * b.m20 + a.m23 * b.m30,
		a.m20 * b.m01 + a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31,
		a.m20 * b.m02 + a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32,
		a.m20 * b.m03 + a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33,
			
		a.m30 * b.m00 + a.m31 * b.m10 + a.m32 * b.m20 + a.m33 * b.m30,
		a.m30 * b.m01 + a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31,
		a.m30 * b.m02 + a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32,
		a.m30 * b.m03 + a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33,
	};
}

fn Matrix2x2 Matrix2x2.component_mul(Matrix2x2* mat, Real s) => matrix_component_mul(mat, s);
fn Matrix3x3 Matrix3x3.component_mul(Matrix3x3* mat, Real s) => matrix_component_mul(mat, s);
fn Matrix4x4 Matrix4x4.component_mul(Matrix4x4* mat, Real s) => matrix_component_mul(mat, s);

fn Matrix2x2 Matrix2x2.add(Matrix2x2* mat, Matrix2x2 mat2) => matrix_add(mat, mat2);
fn Matrix3x3 Matrix3x3.add(Matrix3x3* mat, Matrix3x3 mat2) => matrix_add(mat, mat2);
fn Matrix4x4 Matrix4x4.add(Matrix4x4* mat, Matrix4x4 mat2) => matrix_add(mat, mat2);

fn Matrix2x2 Matrix2x2.sub(Matrix2x2* mat, Matrix2x2 mat2) => matrix_sub(mat, mat2);
fn Matrix3x3 Matrix3x3.sub(Matrix3x3* mat, Matrix3x3 mat2) => matrix_sub(mat, mat2);
fn Matrix4x4 Matrix4x4.sub(Matrix4x4* mat, Matrix4x4 mat2) => matrix_sub(mat, mat2);



fn Matrix2x2 Matrix2x2.transpose(Matrix2x2* mat)
{
	return Matrix2x2 {
		mat.m00, mat.m10,
		mat.m01, mat.m11
	};
}

fn Matrix3x3 Matrix3x3.transpose(Matrix3x3* mat)
{
	return Matrix3x3 {
		mat.m00, mat.m10, mat.m20,
		mat.m01, mat.m11, mat.m21,
		mat.m02, mat.m12, mat.m22,
	};
}

fn Matrix4x4 Matrix4x4.transpose(Matrix4x4* mat)
{
	return Matrix4x4 {
		mat.m00, mat.m10, mat.m20, mat.m30,
		mat.m01, mat.m11, mat.m21, mat.m31,
		mat.m02, mat.m12, mat.m22, mat.m32,
		mat.m03, mat.m13, mat.m23, mat.m33,
	};
}


fn Real Matrix2x2.determinant(Matrix2x2* mat)
{
	return mat.m00 * mat.m11 - mat.m01 * mat.m10;
}

fn Real Matrix3x3.determinant(Matrix3x3* mat)
{
	return
		mat.m00 * (mat.m11 * mat.m22 - mat.m21 * mat.m12) -
		mat.m01 * (mat.m10 * mat.m22 - mat.m20 * mat.m12) +
		mat.m02 * (mat.m10 * mat.m21 - mat.m20 * mat.m11);
}

fn Real Matrix4x4.determinant(Matrix4x4* mat)
{
	return
		mat.m00 * (mat.m11 * (mat.m22 * mat.m33 - mat.m32 * mat.m23) -
				   mat.m12 * (mat.m21 * mat.m33 - mat.m31 * mat.m23) +
				   mat.m13 * (mat.m21 * mat.m32 - mat.m31 * mat.m22) ) -
		mat.m01 * (mat.m10 * (mat.m22 * mat.m33 - mat.m32 * mat.m23) -
				   mat.m12 * (mat.m20 * mat.m33 - mat.m30 * mat.m23) +
				   mat.m13 * (mat.m20 * mat.m32 - mat.m30 * mat.m22) ) +
		mat.m02 * (mat.m10 * (mat.m21 * mat.m33 - mat.m31 * mat.m23) -
				   mat.m11 * (mat.m20 * mat.m33 - mat.m30 * mat.m23) +
				   mat.m13 * (mat.m20 * mat.m31 - mat.m30 * mat.m21) ) -
		mat.m03 * (mat.m10 * (mat.m21 * mat.m32 - mat.m31 * mat.m22) -
				   mat.m11 * (mat.m20 * mat.m32 - mat.m30 * mat.m22) +
				   mat.m12 * (mat.m20 * mat.m31 - mat.m30 * mat.m21) );
}


fn Matrix2x2 Matrix2x2.adjoint(Matrix2x2* mat)
{
	return Matrix2x2 { mat.m00, -mat.m01, -mat.m10, mat.m11 };
}

fn Matrix3x3 Matrix3x3.adjoint(Matrix3x3* mat)
{
	return Matrix3x3 {
		(mat.m11 * mat.m22 - mat.m21 * mat.m12),
		-(mat.m10 * mat.m22 - mat.m20 * mat.m12),
		(mat.m10 * mat.m21 - mat.m20 * mat.m11),
			
		-(mat.m01 * mat.m22 - mat.m21 * mat.m02),
		(mat.m00 * mat.m22 - mat.m20 * mat.m02),
		-(mat.m00 * mat.m21 - mat.m20 * mat.m01),
			
		(mat.m01 * mat.m12 - mat.m11 * mat.m02),
		-(mat.m00 * mat.m12 - mat.m10 * mat.m02),
		(mat.m00 * mat.m11 - mat.m10 * mat.m01),
	};
}

fn Matrix4x4 Matrix4x4.adjoint(Matrix4x4* mat)
{
	return Matrix4x4 {
		(mat.m11 * (mat.m22 * mat.m33 - mat.m32 * mat.m23) -
		 mat.m12 * (mat.m21 * mat.m33 - mat.m31 * mat.m23) +
		 mat.m13 * (mat.m21 * mat.m32 - mat.m31 * mat.m22)),
		-(mat.m10 * (mat.m22 * mat.m33 - mat.m32 * mat.m23) -
		  mat.m12 * (mat.m20 * mat.m33 - mat.m30 * mat.m23) +
		  mat.m13 * (mat.m20 * mat.m32 - mat.m30 * mat.m22)),
		(mat.m10 * (mat.m21 * mat.m33 - mat.m31 * mat.m23) -
		 mat.m11 * (mat.m20 * mat.m33 - mat.m30 * mat.m23) +
		 mat.m13 * (mat.m20 * mat.m31 - mat.m30 * mat.m21)),
		-(mat.m10 * (mat.m21 * mat.m32 - mat.m31 * mat.m22) -
		  mat.m11 * (mat.m20 * mat.m32 - mat.m30 * mat.m22) +
		  mat.m12 * (mat.m20 * mat.m31 - mat.m30 * mat.m21)),
			
		-(mat.m01 * (mat.m22 * mat.m33 - mat.m32 * mat.m23) -
		  mat.m02 * (mat.m21 * mat.m33 - mat.m31 * mat.m23) +
		  mat.m03 * (mat.m21 * mat.m32 - mat.m31 * mat.m22)),
		(mat.m00 * (mat.m22 * mat.m33 - mat.m32 * mat.m23) -
		 mat.m02 * (mat.m20 * mat.m33 - mat.m30 * mat.m23) +
		 mat.m03 * (mat.m20 * mat.m32 - mat.m30 * mat.m22)),
		-(mat.m00 * (mat.m21 * mat.m33 - mat.m31 * mat.m23) -
		  mat.m01 * (mat.m20 * mat.m33 - mat.m30 * mat.m23) +
		  mat.m03 * (mat.m20 * mat.m31 - mat.m30 * mat.m21)),
		(mat.m00 * (mat.m21 * mat.m32 - mat.m31 * mat.m22) -
		 mat.m01 * (mat.m20 * mat.m32 - mat.m30 * mat.m22) +
		 mat.m02 * (mat.m20 * mat.m31 - mat.m30 * mat.m21)),
			
		(mat.m01 * (mat.m12 * mat.m33 - mat.m32 * mat.m13) -
		 mat.m02 * (mat.m11 * mat.m33 - mat.m31 * mat.m13) +
		 mat.m03 * (mat.m11 * mat.m32 - mat.m31 * mat.m12)),
		-(mat.m00 * (mat.m12 * mat.m33 - mat.m32 * mat.m13) -
		  mat.m02 * (mat.m10 * mat.m33 - mat.m30 * mat.m13) +
		  mat.m03 * (mat.m10 * mat.m32 - mat.m30 * mat.m12)),
		(mat.m00 * (mat.m11 * mat.m33 - mat.m31 * mat.m13) -
		 mat.m01 * (mat.m10 * mat.m33 - mat.m30 * mat.m13) +
		 mat.m03 * (mat.m10 * mat.m31 - mat.m30 * mat.m11)),
		-(mat.m00 * (mat.m11 * mat.m32 - mat.m31 * mat.m12) -
		  mat.m01 * (mat.m10 * mat.m32 - mat.m30 * mat.m12) +
		  mat.m02 * (mat.m10 * mat.m31 - mat.m30 * mat.m11)),
			
		-(mat.m01 * (mat.m12 * mat.m23 - mat.m22 * mat.m13) -
		  mat.m02 * (mat.m11 * mat.m23 - mat.m21 * mat.m13) +
		  mat.m03 * (mat.m11 * mat.m22 - mat.m21 * mat.m12)),
		(mat.m00 * (mat.m12 * mat.m23 - mat.m22 * mat.m13) -
		 mat.m02 * (mat.m10 * mat.m23 - mat.m20 * mat.m13) +
		 mat.m03 * (mat.m10 * mat.m22 - mat.m20 * mat.m12)),
		-(mat.m00 * (mat.m11 * mat.m23 - mat.m21 * mat.m13) -
		  mat.m01 * (mat.m10 * mat.m23 - mat.m20 * mat.m13) +
		  mat.m03 * (mat.m10 * mat.m21 - mat.m20 * mat.m11)),
		(mat.m00 * (mat.m11 * mat.m22 - mat.m21 * mat.m12) -
		 mat.m01 * (mat.m10 * mat.m22 - mat.m20 * mat.m12) +
		 mat.m02 * (mat.m10 * mat.m21 - mat.m20 * mat.m11)),
	};
}


fn Matrix2x2! Matrix2x2.inverse(Matrix2x2* m)
{
	Real det = m.determinant();
	if (det == 0) return MatrixError.MATRIX_INVERSE_DOESNT_EXIST?;
	Matrix2x2 adj = m.adjoint();
	return adj.component_mul(1 / det).transpose();
}

fn Matrix3x3! Matrix3x3.inverse(Matrix3x3* m)
{
	Real det = m.determinant();
	if (det == 0) return MatrixError.MATRIX_INVERSE_DOESNT_EXIST?;
	Matrix3x3 adj = m.adjoint();
	return adj.component_mul(1 / det).transpose();
}

fn Matrix4x4! Matrix4x4.inverse(Matrix4x4* m)
{
	Real det = m.determinant();
	if (det == 0) return MatrixError.MATRIX_INVERSE_DOESNT_EXIST?;
	Matrix4x4 adj = m.adjoint();
	return adj.component_mul(1 / det).transpose();
}


fn Matrix3x3 Matrix3x3.translate(Matrix3x3* m, Real[<2>] v)
{
	return m.mul(Matrix3x3 {
		1, 0, v[0],
		0, 1, v[1],
		0, 0, 1,
	});
}

fn Matrix4x4 Matrix4x4.translate(Matrix4x4* m, Real[<3>] v)
{
	return m.mul(Matrix4x4 {
		1, 0, 0, v[0],
		0, 1, 0, v[1],
		0, 0, 1, v[2],
		0, 0, 0, 1,
	});
}

// r in radians
fn Matrix3x3 Matrix3x3.rotate(Matrix3x3* m, Real r)
{
	return m.mul(Matrix3x3 {
		math::cos(r), -math::sin(r), 0,
		math::sin(r), math::cos(r), 0,
		0, 0, 1,
	});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_z(Matrix4x4* m, Real r)
{
	return m.mul(Matrix4x4 {
		math::cos(r), -math::sin(r), 0, 0,
		math::sin(r), math::cos(r), 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1,
	});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_y(Matrix4x4* m, Real r)
{
	return m.mul(Matrix4x4 {
		math::cos(r), 0, -math::sin(r), 0,
		0, 1, 0, 0,
		math::sin(r), 0, math::cos(r), 0,
		0, 0, 0, 1,
	});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_x(Matrix4x4* m, Real r)
{
	return m.mul(Matrix4x4 {
		1, 0, 0, 0,
		0, math::cos(r), -math::sin(r), 0,
		0, math::sin(r), math::cos(r), 0,
		0, 0, 0, 1,
	});
}


fn Matrix3x3 Matrix3x3.scale(Matrix3x3* m, Real[<2>] v)
{
	return m.mul(Matrix3x3 {
		v[0], 0, 0,
		0, v[1], 0,
		0, 0, 1,
	});
}

fn Real Matrix2x2.trace(Matrix2x2* m) => m.m00 + m.m11;
fn Real Matrix3x3.trace(Matrix3x3* m) => m.m00 + m.m11 + m.m22;
fn Real Matrix4x4.trace(Matrix4x4* m) => m.m00 + m.m11 + m.m22 + m.m33;

fn Matrix4x4 Matrix4x4.scale(Matrix4x4* m, Real[<3>] v)
{
	return m.mul(Matrix4x4 {
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
    return Matrix4x4 {
        2 / width, 0, 0, 0,
        0, 2 / height, 0, 0,
        0, 0, -2 / depth, 0,
        -(right + left) / width, -(top + bottom) / height, -(far + near) / depth, 1
    };
}

// fov in radians
fn Matrix4x4 perspective(Real fov, Real aspect_ratio, Real near, Real far)
{
 	Real f = (Real)math::tan(math::PI * 0.5 - 0.5 * fov);
	Real rangeInv = (Real)1.0 / (near - far);
	
    return Matrix4x4 {
        f / aspect_ratio, 0, 0, 0,
        0, f, 0, 0,
        0, 0, (near + far) * rangeInv, -1,
        0, 0, near * far * rangeInv * 2, 0,
    };
}

const Matrix2x2 IDENTITY2 = { .m = { [0] = 1, [3] = 1 } };
const Matrix3x3 IDENTITY3 = { .m = { [0] = 1, [4] = 1, [8] = 1 } };
const Matrix4x4 IDENTITY4 = { .m = { [0] = 1, [5] = 1, [10] = 1, [15] = 1 } };

macro matrix_component_mul(mat, val) @private
{
	var $Type = Real[<$typeof(mat.m).len>];
	return $typeof(*mat) { .m = val * ($Type)mat.m };
}

macro matrix_add(mat, mat2) @private
{
	var $Type = Real[<$typeof(mat.m).len>];
	return $typeof(*mat) { .m = ($Type)mat.m + ($Type)mat2.m };
}

macro matrix_sub(mat, mat2) @private
{
	var $Type = Real[<$typeof(mat.m).len>];
	return $typeof(*mat) { .m = ($Type)mat.m - ($Type)mat2.m };
}
