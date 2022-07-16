module std::math;

fault MatrixError {
    MATRIX_INVERSE_DOESNT_EXIST,
}

struct Matrix2x2 {
	union {
		struct {
			float m00, m01;
			float m10, m11;
		}
		float[4] m;
	}
}

struct Matrix3x3 {
	union {
		struct {
			float m00; float m01; float m02;
			float m10; float m11; float m12;
			float m20; float m21; float m22;
		}
		float[9] m;
	}
}

struct Matrix4x4 {
	union {
		float[16] m;
		struct {
			float m00, m01, m02, m03;
			float m10, m11, m12, m13;
			float m20, m21, m22, m23;
			float m30, m31, m32, m33;
		}
	}
}

fn float[<2>] Matrix2x2.apply(Matrix2x2* mat, float[<2>] vec) {
	return float[<2>] {
		mat.m00 * vec[0] + mat.m01 * vec[1],
		mat.m10 * vec[0] + mat.m11 * vec[1],
	};
}

fn float[<3>] Matrix3x3.apply(Matrix3x3* mat, float[<3>] vec) {
	return float[<3>] {
		mat.m00 * vec[0] + mat.m01 * vec[1] + mat.m02 * vec[2],
		mat.m10 * vec[0] + mat.m11 * vec[1] + mat.m12 * vec[2],
		mat.m20 * vec[0] + mat.m21 * vec[1] + mat.m22 * vec[2],
	};
}

fn float[<4>] Matrix4x4.apply(Matrix4x4* mat, float[<4>] vec) {
	return float[<4>] {
		mat.m00 * vec[0] + mat.m01 * vec[1] + mat.m02 * vec[2] + mat.m03 * vec[3],
		mat.m10 * vec[0] + mat.m11 * vec[1] + mat.m12 * vec[2] + mat.m13 * vec[3],
		mat.m20 * vec[0] + mat.m21 * vec[1] + mat.m22 * vec[2] + mat.m23 * vec[3],
		mat.m30 * vec[0] + mat.m31 * vec[1] + mat.m32 * vec[2] + mat.m33 * vec[3],
	};
}


fn Matrix2x2 Matrix2x2.mul(Matrix2x2* a, Matrix2x2 b) {
	return Matrix2x2 { .m = {
			a.m00 * b.m00 + a.m01 * b.m10, a.m00 * b.m01 + a.m01 * b.m11,
			a.m10 * b.m01 + a.m11 * b.m11, a.m10 * b.m01 + a.m11 * b.m11,
		} };
}

fn Matrix3x3 Matrix3x3.mul(Matrix3x3* a, Matrix3x3 b) {
	return Matrix3x3 { .m = {
			a.m00 * b.m00 + a.m01 * b.m10 + a.m02 * b.m20,
			a.m00 * b.m01 + a.m01 * b.m11 + a.m02 * b.m21,
			a.m00 * b.m02 + a.m01 * b.m12 + a.m02 * b.m22,
			
			a.m10 * b.m00 + a.m11 * b.m10 + a.m12 * b.m20,
			a.m10 * b.m01 + a.m11 * b.m11 + a.m12 * b.m21,
			a.m10 * b.m02 + a.m11 * b.m12 + a.m12 * b.m22,
			
			a.m20 * b.m00 + a.m21 * b.m10 + a.m22 * b.m20,
			a.m20 * b.m01 + a.m21 * b.m11 + a.m22 * b.m21,
			a.m20 * b.m02 + a.m21 * b.m12 + a.m22 * b.m22,
		} };
}

fn Matrix4x4 Matrix4x4.mul(Matrix4x4* a, Matrix4x4 b) {
	return Matrix4x4 { .m = {
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
		} };
}


fn Matrix2x2 Matrix2x2.component_mul(Matrix2x2* mat, float s) {
	return Matrix2x2 { .m = {
			mat.m00 * s, mat.m01 * s,
			mat.m10 * s, mat.m11 * s,
		} };
}

fn Matrix3x3 Matrix3x3.component_mul(Matrix3x3* mat, float s) {
	return Matrix3x3 { .m = {
			mat.m00 * s, mat.m01 * s, mat.m02 * s,
			mat.m10 * s, mat.m11 * s, mat.m12 * s,
			mat.m20 * s, mat.m21 * s, mat.m22 * s,
		} };
}

fn Matrix4x4 Matrix4x4.component_mul(Matrix4x4* mat, float s) {
	return Matrix4x4 { .m = {
			mat.m00 * s, mat.m01 * s, mat.m02 * s, mat.m03 * s,
			mat.m10 * s, mat.m11 * s, mat.m12 * s, mat.m13 * s,
			mat.m20 * s, mat.m21 * s, mat.m22 * s, mat.m23 * s,
			mat.m30 * s, mat.m31 * s, mat.m32 * s, mat.m33 * s,
		} };
}


fn Matrix2x2 Matrix2x2.transpose(Matrix2x2* mat) {
	return Matrix2x2 { .m = { mat.m00, mat.m10, mat.m01, mat.m11 } };
}

fn Matrix3x3 Matrix3x3.transpose(Matrix3x3* mat) {
	return Matrix3x3 { .m = {
			mat.m00, mat.m10, mat.m20,
			mat.m01, mat.m11, mat.m21,
			mat.m02, mat.m12, mat.m22,
		} };
}

fn Matrix4x4 Matrix4x4.transpose(Matrix4x4* mat) {
	return Matrix4x4 { .m = {
			mat.m00, mat.m10, mat.m20, mat.m30,
			mat.m01, mat.m11, mat.m21, mat.m31,
			mat.m02, mat.m12, mat.m22, mat.m32,
			mat.m03, mat.m13, mat.m23, mat.m33,
		} };
}


fn float Matrix2x2.determinant(Matrix2x2* mat) {
	return mat.m00 * mat.m11 - mat.m01 * mat.m10;
}

fn float Matrix3x3.determinant(Matrix3x3* mat) {
	return
		mat.m00 * (mat.m11 * mat.m22 - mat.m21 * mat.m12) -
		mat.m01 * (mat.m10 * mat.m22 - mat.m20 * mat.m12) +
		mat.m02 * (mat.m10 * mat.m21 - mat.m20 * mat.m11);
}

fn float Matrix4x4.determinant(Matrix4x4* mat) {
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
				   mat.m12 * (mat.m20 * mat.m31 - mat.m30 * mat.m21) )
		;
}


fn Matrix2x2 Matrix2x2.adjoint(Matrix2x2* mat) {
	return Matrix2x2 {  .m = { mat.m00, -mat.m01, -mat.m10, mat.m11 } };
}

fn Matrix3x3 Matrix3x3.adjoint(Matrix3x3* mat) {
	return Matrix3x3 { .m = {
			(mat.m11 * mat.m22 - mat.m21 * mat.m12),
			-(mat.m10 * mat.m22 - mat.m20 * mat.m12),
			(mat.m10 * mat.m21 - mat.m20 * mat.m11),
			
			-(mat.m01 * mat.m22 - mat.m21 * mat.m02),
			(mat.m00 * mat.m22 - mat.m20 * mat.m02),
			-(mat.m00 * mat.m21 - mat.m20 * mat.m01),
			
			(mat.m01 * mat.m12 - mat.m11 * mat.m02),
			-(mat.m00 * mat.m12 - mat.m10 * mat.m02),
			(mat.m00 * mat.m11 - mat.m10 * mat.m01),
		} };
}

fn Matrix4x4 Matrix4x4.adjoint(Matrix4x4* mat) {
	return Matrix4x4 { .m = {
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
		} };
}


fn Matrix2x2! Matrix2x2.inverse(Matrix2x2* m) {
	float det = m.determinant();
	if (det == 0) return MatrixError.MATRIX_INVERSE_DOESNT_EXIST!;
	Matrix2x2 adj = m.adjoint();
	return adj.component_mul(1 / det).transpose();
}

fn Matrix3x3! Matrix3x3.inverse(Matrix3x3* m) {
	float det = m.determinant();
	if (det == 0) return MatrixError.MATRIX_INVERSE_DOESNT_EXIST!;
	Matrix3x3 adj = m.adjoint();
	return adj.component_mul(1 / det).transpose();
}

fn Matrix4x4! Matrix4x4.inverse(Matrix4x4* m) {
	float det = m.determinant();
	if (det == 0) return MatrixError.MATRIX_INVERSE_DOESNT_EXIST!;
	Matrix4x4 adj = m.adjoint();
	return adj.component_mul(1 / det).transpose();
}


fn Matrix3x3 Matrix3x3.translate(Matrix3x3* m, float[<2>] v) {
	return m.mul(Matrix3x3 { .m = {
						 1.f, 0.f, v[0],
						 0.f, 1.f, v[1],
						 0.f, 0.f, 1.f,
					 } });
}

fn Matrix4x4 Matrix4x4.translate(Matrix4x4* m, float[<3>] v) {
	return m.mul(Matrix4x4 { .m = {
						 1.f, 0.f, 0.f, v[0],
						 0.f, 1.f, 0.f, v[1],
						 0.f, 0.f, 1.f, v[2],
						 0.f, 0.f, 0.f, 1.f,
					 } });
}

// r in radians
fn Matrix3x3 Matrix3x3.rotate(Matrix3x3* m, float r) {
	return m.mul(Matrix3x3 { .m = {
						 (float)math::cos(r), (float)-math::sin(r), 0.f,
						 (float)math::sin(r), (float) math::cos(r), 0.f,
						 0.f, 0.f, 1.f,
					 } });
}

// r in radians
fn Matrix4x4 Matrix4x4.rotateZ(Matrix4x4* m, float r) {
	return m.mul(Matrix4x4 { .m = {
						 (float)math::cos(r), (float)-math::sin(r), 0.f, 0.f,
						 (float)math::sin(r), (float) math::cos(r), 0.f, 0.f,
						 0.f, 0.f, 1.f, 0.f,
						 0.f, 0.f, 0.f, 1.f,
					 } });
}

// r in radians
fn Matrix4x4 Matrix4x4.rotateY(Matrix4x4* m, float r) {
	return m.mul(Matrix4x4 { .m = {
						 (float)math::cos(r), 0.f, (float)-math::sin(r), 0.f,
						 0.f, 1.f, 0.f, 0.f,
						 (float)math::sin(r), 0.f, (float) math::cos(r), 0.f,
						 0.f, 0.f, 0.f, 1.f,
					 } });
}

// r in radians
fn Matrix4x4 Matrix4x4.rotateX(Matrix4x4* m, float r) {
	return m.mul(Matrix4x4 { .m = {
						 1.f, 0.f, 0.f, 0.f,
						 0.f, (float)math::cos(r), (float)-math::sin(r), 0.f,
						 0.f, (float)math::sin(r), (float) math::cos(r), 0.f,
						 0.f, 0.f, 0.f, 1.f,
					 } });
}


fn Matrix3x3 Matrix3x3.scale(Matrix3x3* m, float[<2>] v) {
	return m.mul(Matrix3x3 { .m = {
						 v[0], 0.f, 0.f,
						 0.f, v[1], 0.f,
						 0.f, 0.f, 1.f,
					 } });
}

fn Matrix4x4 Matrix4x4.scale(Matrix4x4* m, float[<3>] v) {
	return m.mul(Matrix4x4 { .m = {
						 v[0], 0.f, 0.f, 0.f,
						 0.f, v[1], 0.f, 0.f,
						 0.f, 0.f, v[2], 0.f,
						 0.f, 0.f, 0.f, 1.f,
					 } });
}


fn Matrix4x4 Matrix4x4.ortho(float left, float right, float top, float bottom, float near, float far) {
    float width = right - left;
    float height = top - bottom;
    float depth = far - near;
    return Matrix4x4 {
        .m = {
            2.f / width,             0.f,                      0.f,                   0.f,
            0.f,                     2.f / height,             0.f,                   0.f,
            0.f,                     0.f,                      -2.f / depth,          0.f,
            -(right + left) / width, -(top + bottom) / height, -(far + near) / depth, 1.f,
        }
    };
}

// fov in radians
fn Matrix4x4 Matrix4x4.perspective(float fov, float aspect_ratio, float near, float far) {
    float top = ((float)math::sin(fov / 2) / (float)math::cos(fov / 2)) * near;
    float right = top * aspect_ratio;
    float depth = far - near;
    return Matrix4x4 {
        .m = {
            1.f / right, 0.f,       0.f,                   0.f,
            0.f,         1.f / top, 0.f,                   0.f,
            0.f,         0.f,       -2.f / depth,          0.f,
            0.f,         0.f,       -(far + near) / depth, 1.f,
        }
    };
}

