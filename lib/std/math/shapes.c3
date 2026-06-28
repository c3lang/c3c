module std::math;

alias Rect = Rectangle {float};

<*
 @require types::is_float(Num) || types::is_int(Num) : "Only integer or floating point rectangles are supported"
*>
module std::math::rectangle <Num>;

union Rectangle
{
	struct
	{
		Num x, y, width, height;
	}
	Num[<4>] v;
	struct
	{
		Num[<2>] origin;
		Num[<2>] size;
	}
}

// Accessors

<*
 Returns the minimum corner (same as origin)

 @return "The minimum corner"
*>
fn Num[<2>] Rectangle.min(self) @inline
{
	return self.origin;
}

<*
 Returns the maximum corner (origin + size)

 @return "The maximum corner"
*>
fn Num[<2>] Rectangle.max(self) @inline
{
	return self.origin + self.size;
}

<*
 Returns the center point of the rectangle.

 @return "The center point"
*>
fn Num[<2>] Rectangle.center(self)
{
	return self.origin + self.size / 2;
}

<*
 Returns the area of the rectangle.

 @return "The area"
*>
fn Num Rectangle.area(self)
{
	return self.size.product();
}

<*
 Check if the rectangle is empty.

 @require self.size.min() >= 0 : "Invalid rectangle with negative size"
 @return "True if the rectangle is empty"
*>
fn bool Rectangle.is_empty(self)
{
	return self.size.min() == 0;
}

// Spatial queries

<*
 Check if a point is contained in the rectangle.

 @return "true if the point is inside or on the border of the rectangle"
*>
fn bool Rectangle.contains_point(self, Num[<2>] point)
{
	return self.origin.comp_le(point).and() & (self.origin + self.size).comp_ge(point).and();
}

<*
 Checks if the rectangle fully contains another rectangle.

 @return "true if the rectangle fully contains the other rectangle"
*>
fn bool Rectangle.contains_rect(self, Rectangle other)
{
	return self.contains_point(other.origin) && self.contains_point(other.max());
}

<*
 Test if rectangles overlap.

 @param other : "The rectangle to compare with"
 @return "true if the rectangles overlap"
*>
fn bool Rectangle.intersects(self, Rectangle other)
{
	Num[<4>] a = self.v;
	Num[<4>] b = other.v;
	return a.x + a.z > b.x
		&& a.x < b.x + b.z
		&& a.y + a.w > b.y
		&& a.y < b.y + b.w;
}

// Geometry operations

<*
 Merge two rectangles into a rectangle large enough to contain both rectangles.

 @return "The smallest rectangle containing both rectangles"
*>
fn Rectangle Rectangle.merge(self, Rectangle other)
{
	Num[<2>] min = math::min(self.min(), other.min());
	Num[<2>] max = math::max(self.max(), other.max());
	return { .origin = min, .size = max - min };
}

<*
 Return the rectangle intersecting both rectangles.

 @param other : "The other rectangle to form the intersection with"

 @return? math::NO_INTERSECTION : "If the rectangles do not intersect"
 @return "The resulting intersection between the two"
*>
fn Rectangle? Rectangle.intersection(self, Rectangle other)
{
	Num[<2>] min = math::max(self.min(), other.min());
	Num[<2>] max = math::min(self.max(), other.max());

	if (min.x >= max.x || min.y >= max.y) return math::NO_INTERSECTION~;

	return { .origin = min, .size = max - min };
}

// Modification

<*
 Expands the rectangle to have integral boundaries.

 @return "The resulting rectangle"
*>
fn Rectangle Rectangle.integral_outer(self) @if(types::is_float(Num))
{
	Num[<2>] min = math::floor(self.origin);
	Num[<2>] max = math::ceil(self.max());

	return { .origin = min, .size = max - min };
}

<*
 Shrinks the rectangle to have integral boundaries.

 @return "The resulting rectangle"
*>
fn Rectangle Rectangle.integral_inner(self) @if(types::is_float(Num))
{
	Num[<2>] min = math::ceil(self.origin);
	Num[<2>] max = math::floor(self.max());

	return { .origin = min, .size = math::max(max - min, (Num[<2>])0) };
}

<*
 Shrinks the rectangle by amount from all sides.

 @require amount >= 0 : "Expected a positive or zero amount"
 @require (self.size - amount * 2).min() >= 0 : "Inset must result in a zero or positive size"
 @return "The resulting rectangle"
*>
fn Rectangle Rectangle.inset(self, Num amount)
{
	return { .origin = self.origin + amount, .size = self.size - amount * 2};
}

<*
 Shrinks the rectangle by amount from all sides, clamping to zero size if necessary.

 @require amount >= 0 : "Expected a positive or zero amount"
 @return "The resulting rectangle"
*>
fn Rectangle Rectangle.inset_clamped(self, Num amount)
{
	Num[<2>] new_size = math::max(self.size - amount * 2, (Num[<2>])0);
	Num[<2>] decrease = self.size - new_size;
	return { .origin = self.origin + decrease / 2, .size = new_size};
}

<*
 Expand the rectangle by the amount in all directions.

 @require amount >= 0 : "Expected a positive or zero amount"
 @require amount * 2 + self.width >= 0 : "The amount would make the width negative"
 @require amount * 2 + self.height >= 0 : "The amount would make the height negative"
 @ensure return.contains_rect(self)

 @return "The resulting rectangle"
*>
fn Rectangle Rectangle.expand(self, Num amount)
{
	return { .origin = self.origin - amount, .size = self.size + amount * 2};
}

<*
 Expands the rectangle minimally to include a point.

 @param point : "The point to expand the rectangle with"
 @return "The resulting rectangle"

 @ensure return.contains_point(point)
*>
fn Rectangle Rectangle.merge_point(self, Num[<2>] point)
{
	Num[<2>] min = math::min(self.min(), point);
	Num[<2>] max = math::max(self.max(), point);
	Num[<2>] size = max - min;
	$if types::is_float(Num):
		bool[<2>] size_comp = (size + min).comp_lt(max);
		if (size_comp.x) size.x = size.x.nexthigher();
		if (size_comp.y) size.y = size.y.nexthigher();
	$endif
	return { .origin = min, .size = size };
}

<*
 Clamps a point to lie inside a rectangle.

 @return "The resulting point"
 @ensure self.contains_point(return)
*>
fn Num[<2>] Rectangle.clamp_point(self, Num[<2>] point)
{
	return math::min(self.max(), math::max(self.min(), point));
}
