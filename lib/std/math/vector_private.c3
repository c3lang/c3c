module std::math::vector @private;
import std::math;

macro cubic_hermite3(v1, tangent1, v2, tangent2, amount)
{
	var amount_pow2 = amount * amount;
	var amount_pow3 = amount_pow2 * amount;

	return (2 * amount_pow3 - 3 *amount_pow2 + 1) * v1
		+ (amount_pow3 - 2 * amount_pow2 + amount) * tangent1
		+ (-2 * amount_pow3 + 3 * amount_pow2) * v2
		+ (amount_pow3 - amount_pow2) * tangent2;
}

fn TowardsType towards(TowardsType v, TowardsType target, $typefrom(TowardsType::inner) max_distance) <TowardsType>
{
	TowardsType delta = target - v;
	var square @safeinfer = delta.length_sq();

	if (square == 0 || (max_distance >= 0 && (square <= max_distance * max_distance))) return target;

	return v + delta * max_distance / math::sqrt(square);
}

fn ClampType clamp_length(ClampType v, $typefrom(ClampType::inner) min, $typefrom(ClampType::inner) max) <ClampType>
{
	var length @safeinfer = v.length_sq();
	if (length > 0)
	{
		length = math::sqrt(length);

		if (length < min) return v * (min / length);
		if (length > max) return v * (max / length);
	}
	return v;
}

macro rotate(v, angle)
{
	var c = math::cos(angle);
	var s = math::sin(angle);
	return ($typeof(v)) { v[0] * c - v[1] * s, v[0] * s + v[1] * c };
}

macro perpendicular3(v)
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

macro cross2(v1, v2)
{
	return v1.x * v2.y - v1.y * v2.x;
}

macro cross3(v1, v2)
{
	var a = v1.yzx * v2.zxy;
	var b = v1.zxy * v2.yzx;
	return a - b;
}

macro transform2(v, mat)
{
	return ($typeof(v)) {
		mat.m00 * v[0] + mat.m10 * v[1] + mat.m30 ,
		mat.m01 * v[0] + mat.m11 * v[1] + mat.m31 };
}

macro transform3(v, mat)
{
	return ($typeof(v)){
		mat.m00 * v[0] + mat.m10 * v[1] + mat.m20 * v[2] + mat.m30,
		mat.m01 * v[0] + mat.m11 * v[1] + mat.m21 * v[2] + mat.m31,
		mat.m02 * v[0] + mat.m12 * v[1] + mat.m22 * v[2] + mat.m32
	};
}

macro angle3(v1, v2)
{
	var len = v1.cross(v2).length();
	var dot = v1.dot(v2);
	return math::atan2(len, dot);
}

macro void ortho_normalize3(v1, v2)
{
	var v1n = *v1 = v1.normalize();
	var vn1 = v1n.cross(*v2).normalize();
	*v2 = v1n.cross(vn1);
}


macro rotate_axis_angle(v, axis, angle)
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

macro rejection3(v1, v2)
{
	return v1 - v2 * (v1.dot(v2) / v2.dot(v2));
}

macro project3(v, model, proj, viewport)
{
	var $Element = $typeof(v)::inner;
	$Element[<4>] tmp = { ...v, 1 };
	tmp = model * tmp;
	tmp = proj * tmp;
	tmp /= tmp.w;
	tmp = { (tmp.x * 0.5 + 0.5) * viewport.z + viewport.x, (tmp.y * 0.5 + 0.5) * viewport.w + viewport.y, tmp.z, tmp.w };
	return tmp.xyz;
}

macro unproject3(v, model, proj, viewport)
{
	var mat_view_inverse = (proj * model).inverse()!!;

	var $Element = $typeof(v)::inner;

	$Element[<4>] tmp = {
		2 * (v.x - viewport.x) / viewport.z - 1,
		2 * (v.y - viewport.y) / viewport.w - 1,
		v.z,
		1 };
	tmp = mat_view_inverse * tmp;
	return tmp.xyz / tmp.w;
}

macro barycentric3(p, a, b, c)
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

macro refract3(v, n, r)
{
	var dot = v.dot(n);
	var d = 1 - r * r * (1 - dot * dot);

	return d < 0 ? v : r * v - (r * dot + math::sqrt(d)) * n;
}


macro equals(v1, v2)
{
	var epsilon = ($typeof(v1)::inner.kind == FLOAT ??? math::FLOAT_VECTOR_EPSILON : math::DOUBLE_VECTOR_EPSILON) * math::max(math::abs(v1), math::abs(v2), ($typeof(v1))1);
	return math::abs(v1 - v2).comp_le(epsilon).and();
}

macro not_equals(v1, v2)
{
	var epsilon = ($typeof(v1)::inner.kind == FLOAT ??? math::FLOAT_VECTOR_EPSILON : math::DOUBLE_VECTOR_EPSILON) * math::max(math::abs(v1), math::abs(v2), ($typeof(v1))1);

	return math::abs(v1 - v2).comp_gt(epsilon).or();
}

macro bool @is_same_vector_or_scalar(#vector_value, #vector_or_scalar)
{
	return (values::@is_vector(#vector_or_scalar) &&& values::@is_same_vector_type(#vector_value, #vector_or_scalar)) ||| values::@is_int(#vector_or_scalar);
}

macro reflect(x, y)
{
	return x - 2 * y * x.dot(y);
}

macro normalize(x)
{
	var len = x.length();
	if (len == 0) return x;
	return x * (1 / len);
}
