module std::math;

alias Rect = Rectangle {float};

module std::math::rectangle <Real>;

union Rectangle
{
	struct
	{
		Real x, y, width, height;
	}
	Real[<4>] v;
	struct
	{
		Real[<2>] origin;
		Real[<2>] size;
	}
}

// Accessors

<*
 Returns the minimum corner (same as origin)

 @return "The minimum corner"
*>
fn Real[<2>] Rectangle.min(self) @inline
{
	return self.origin;
}

<*
 Returns the maximum corner (origin + size)

 @return "the maximum corner"
*>
fn Real[<2>] Rectangle.max(self) @inline
{
	return self.origin + self.size;
}

<*
 Returns the center point of the rectangle.

 @return "The center point"
*>
fn Real[<2>] Rectangle.center(self)
{
	return self.origin + self.size / 2;
}

<*
 Returns the area of the rectangle.

 @return "The area"
*>
fn Real Rectangle.area(self)
{
	return self.size.product();
}

<*
 Return error if the rectangle is empty.

 @require self.size.min() >= 0 : "Invalid rectangle with negative size"
 @return "True is the rectangle is empty"
*>
fn bool Rectangle.is_empty(self)
{
	return self.size.min() == 0;
}

// Spatial queries

<*
 Check if a point is contained in a rectangle

 @return "true if the point is inside or on the border of the rectangle"
*>
fn bool Rectangle.contains_point(self, Real[<2>] point)
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
	Real[<4>] a = self.v;
	Real[<4>] b = other.v;
	return a.x + a.z > b.x
		&& a.x < b.x + b.z
		&& a.y + a.w > b.y
		&& a.y < b.y + b.w;
}


// Geometry operations

<*
 Merge two rectangles into a rectangle enough to contain both rectangles.

 @return "the smallest rectangle containing both rectangles"
*>
fn Rectangle Rectangle.merge(self, Rectangle other)
{
	Real[<2>] min = math::min(self.min(), other.min());
	Real[<2>] max = math::max(self.max(), other.max());
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
	Real[<2>] min = math::max(self.min(), other.min());
	Real[<2>] max = math::min(self.max(), other.max());

	if (min.x >= max.x || min.y >= max.y) return math::NO_INTERSECTION~;

	return { .origin = min, .size = max - min };
}

// Modification

<*
 Expands the rectangle to have integral boundaries.
 @return "the resulting rectangle"
*>
fn Rectangle Rectangle.integral_outer(self)
{
	Real[<2>] min = math::floor(self.origin);
	Real[<2>] max = math::ceil(self.max());

	return { .origin = min, .size = max - min };
}

<*
 Shrinks the rectangle to have integral boundaries.
 @return "the resulting rectangle"
*>
fn Rectangle Rectangle.integral_inner(self)
{
	Real[<2>] min = math::ceil(self.origin);
	Real[<2>] max = math::floor(self.max());

	return { .origin = min, .size = math::max(max - min, (Real[<2>])0) };
}

<*
 Shrinks the rectangle by amount from all sides.

 @require amount >= 0 : "Expected a positive or zero amount"
 @require (self.size - amount * 2).min() >= 0 : "Inset must result in a zero or positive size"
 @return "the resulting rectangle"
*>
fn Rectangle Rectangle.inset(self, Real amount)
{
	return { .origin = self.origin + amount, .size = self.size - amount * 2};
}

<*
 Shrinks the rectangle by amount from all sides, clamping to zero size if necessary.

 @require amount >= 0 : "Expected a positive or zero amount"
 @return "the resulting rectangle"
*>
fn Rectangle Rectangle.inset_clamped(self, Real amount)
{
	Real[<2>] new_size = math::max(self.size - amount * 2, (Real[<2>])0);
	Real[<2>] decrease = self.size - new_size;
	return { .origin = self.origin + decrease / 2, .size = new_size};
}

<*
 Expand the rectangle by the amount in all directions.

 @require amount >= 0 : "Expected a positive or zero amount"
 @require amount * 2 + self.width >= 0 : "The amount would make the width negative"
 @require amount * 2 + self.height >= 0 : "The amount would make the height negative"
 @ensure return.contains_rect(self)

 @return "the resulting rectangle"
*>
fn Rectangle Rectangle.expand(self, Real amount)
{
	return { .origin = self.origin - amount, .size = self.size + amount * 2};
}

<*
 Expands the rectangle minimally to include a point.

 @param point : "The point to expand the rectangle with"
 @return "the resulting rectangle"

 @ensure return.contains_point(point)
*>
fn Rectangle Rectangle.merge_point(self, Real[<2>] point)
{
	Real[<2>] min = math::min(self.min(), point);
	Real[<2>] max = math::max(self.max(), point);
	return { .origin = min, .size = max - min };
}

<*
 Clamps a point to lie inside a rectangle.

 @return "the resulting point"
 @ensure self.contains_point(return)
*>
fn Real[<2>] Rectangle.clamp_point(self, Real[<2>] point)
{
	return math::min(self.max(), math::max(self.min(), point));
}