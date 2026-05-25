<*
 @require types::is_float(Num) || types::is_int(Num) : "Only integer or floating point Rects are supported"
*>
module std::math::geometry::bounding_box <Num>;
import std::io;
import std::math;

alias Vec @local = Num[<2>];
alias Rect @local = Rectangle{Num};

<*
 An axis-aligned rectangle defined by its minimum and maximum corners.
*>
union BoundingBox (Printable)
{
	struct
	{
		Vec min, max;
	}
	struct
	{
		Vec p1, p2;
	}
	struct {
		float x1, y1, x2, y2;
	}
	Num[<4>] v;
}

// Accessors

<*
 Returns the center point of the bounding box.

 @return "The center point"
*>
fn Vec BoundingBox.center(self) @inline => (self.min + self.max) / 2;

<*
 Returns the width of the bounding box.

 @return "the width"
*>
fn float BoundingBox.width(self) @inline => self.max.x - self.min.x;

<*
 Returns the height of the bounding box.

 @return "the height"
*>
fn float BoundingBox.height(self) @inline => self.max.y - self.min.y;

<*
 Returns the size of the bounding box.

 @return "the size"
*>
fn Vec BoundingBox.size(self) @inline => { self.width(), self.height() };

<*
 Returns the area of the bounding box.

 @return "The area"
*>
fn Num BoundingBox.area(self) @inline => self.size().product();

<*
 Check if the bounding box is empty.

 @require self.size().min() >= 0 : "Invalid bounding box with negative size"
 @return "True if the bounding box is empty"
*>
fn bool BoundingBox.is_empty(self) @inline => self.size().min() == 0;

// Spatial queries

<*
 Checks if the bounding box fully contains another bounding box.

 @return "true if the bounding box fully contains the other bounding box"
*>
fn bool BoundingBox.contains(self, BoundingBox other)
{
	return self.min.comp_le(other.min).and() && self.max.comp_ge(other.max).and();
}

<*
 Check if a point is contained in the bounding box.

 @return "true if the point is inside or on the border of the bounding box"
*>
fn bool BoundingBox.contains_point(self, Vec point)
{
	return self.min.comp_le(point).and() && self.max.comp_ge(point).and();
}

<*
 Checks if the bounding box fully contains another rectangle.

 @return "true if the bounding box fully contains the other rectangle"
*>
fn bool BoundingBox.contains_rect(self, Rect other)
{
	return self.min.comp_le(other.min()).and() && self.max.comp_ge(other.max()).and();
}

<*
 Test if bounding boxes overlap.

 @param other : "The bounding box to compare with"
 @return "true if the bounding box overlap"
*>
fn bool BoundingBox.intersects(self, BoundingBox other)
{
	return self.min.comp_le(other.max).and() && self.max.comp_ge(other.min).and();
}

<*
 Test if a rectangle overlaps the bounding box.

 @param other : "The rectangle to compare with"
 @return "true if the rectangle overlaps"
*>
fn bool BoundingBox.intersects_rect(self, Rect other)
{
	return self.min.comp_le(other.max()).and() && self.max.comp_ge(other.min()).and();
}

// Geometry operations

<*
 Merge two bounding boxes into one enough to contain both.

 @ensure return.contains(self) && return.contains(other)

 @return "The smallest bounding box containing both"
*>
fn BoundingBox BoundingBox.merge(self, BoundingBox other) => {
	math::min(self.min, other.min),
	math::max(self.max, other.max),
};

<*
 Merge a bounding box and rectangle into one enough to contain both.

 @ensure return.contains(self) && return.contains(other)

 @return "The smallest bounding box containing both"
*>
fn BoundingBox BoundingBox.merge_rect(self, BoundingBox other) => {
	math::min(self.min, other.min),
	math::max(self.max, other.max),
};

<*
 Expands the bounding box minimally to include a point.

 @param point : "The point to expand the bounding box with"
 @return "The resulting bounding box"

 @ensure return.contains_point(point)
*>
fn BoundingBox BoundingBox.merge_point(self, Vec point) => {
	math::min(self.min, point),
	math::max(self.max, point),
};

<*
 Return the bounding box intersecting both bounding boxes.

 @param other : "The other bounding box to form the intersection with"

 @return? math::NO_INTERSECTION : "If the bounding boxes do not intersect"
 @return "The resulting intersection between the two"
*>
fn BoundingBox? BoundingBox.intersection(self, BoundingBox other)
{
	Num[<2>] min = math::max(self.min, other.min);
	Num[<2>] max = math::min(self.max, other.max);

	if (min.x > max.x || min.y > max.y) return math::NO_INTERSECTION~;

	return { min, max };
}

// Modification

<*
 Clamps a point to lie inside a bounding box.

 @return "The resulting point"
 @ensure self.contains_point(return)
*>
fn Vec BoundingBox.clamp_point(self, Vec point)
{
	return math::clamp(point, self.min, self.max);
}

<*
 Expand the bounding box by the amount in all directions.

 @require amount >= 0 : "Expected a positive or zero amount"
 @ensure return.contains(self)

 @return "The resulting bounding box"
*>
fn BoundingBox BoundingBox.expand(self, Num amount) => {
	self.min - amount,
	self.max + amount,
};

<*
 Shrinks the bounding box by amount from all sides.

 @require amount >= 0 : "Expected a positive or zero amount"
 @require (self.size() - amount * 2).min() >= 0 : "Inset must result in a zero or positive size"
 @ensure self.contains(return)

 @return "The resulting bounding box"
*>
fn BoundingBox BoundingBox.inset(self, Num amount) => {
	self.min + amount,
	self.max - amount,
};

<*
 Shrinks the bounding box by amount from all sides, clamping to zero size if necessary.

 @require amount >= 0 : "Expected a positive or zero amount"
 @ensure self.contains(return)

 @return "The resulting bounding box"
*>
fn BoundingBox BoundingBox.inset_clamped(self, Num amount)
{
	Vec decrease = math::min(self.size() / 2, (Vec)amount);
	return {
		self.min + decrease,
		self.max - decrease,
	};
}

<*
 Expands the bounding box to have integral boundaries.

 @return "The resulting bounding box"
*>
fn BoundingBox BoundingBox.integral_outer(self) @if(types::is_float(Num))
{
	return { math::floor(self.min), math::ceil(self.max) };
}

<*
 Shrinks the bounding box to have integral boundaries.

 @return "The resulting bounding box"
*>
fn BoundingBox BoundingBox.integral_inner(self) @if(types::is_float(Num))
{
	Vec min = math::ceil(self.min);
	Vec max = math::floor(self.max);
	return { min, math::max(min, max) };
}

<*
 Moves the bounding box by offset.

 @return "The resulting bounding box"
*>
fn BoundingBox BoundingBox.offset(self, Vec offset) @operator_s(+) => { self.min + offset, self.max + offset };

<*
 Moves the bounding box by subtracting offset.

 @return "The resulting bounding box"
*>
fn BoundingBox BoundingBox.offset_sub(self, Vec offset) @operator_s(-) => { self.min - offset, self.max - offset };

<*
 Moves the bounding box by offset.

 @return "The resulting bounding box"
*>
fn BoundingBox BoundingBox.offset_self(&self, Vec offset) @operator(+=)
{
	self.min += offset;
	self.max += offset;
	return *self;
}

<*
 Moves the bounding box by subtracting offset.

 @return "The resulting bounding box"
*>
fn BoundingBox BoundingBox.offset_self_sub(&self, Vec offset) @operator(-=)
{
	self.min -= offset;
	self.max -= offset;
	return *self;
}

// Conversions

<*
 Check if two bounding boxes are equal.

 @return "true if both are equal"
*>
fn bool BoundingBox.equals(self, BoundingBox other) @operator_s(==)
{
	return self.v == other.v;
}

<*
 Converts the bounding box into a rectangle.

 @return "The rectangle"
*>
fn Rect BoundingBox.to_rect(self) => { .origin = self.min, .size = self.size() };

fn sz? BoundingBox.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%f, %f > %f, %f]", self.min.x, self.min.y, self.max.x, self.max.y);
}
