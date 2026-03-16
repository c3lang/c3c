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

fn bool Rectangle.contains(self, Real[<2>] point)
{
	return self.origin.comp_le(point).and() & (self.origin + self.size).comp_ge(point).and();
}

fn Rectangle Rectangle.max(self, Rectangle other)
{
	Real[<2>] min = math::min(self.origin, other.origin);
	Real[<2>] max = math::min(self.origin + self.size, other.origin + other.size);
	return { .origin = min, .size = max - min };
}

fn bool Rectangle.intersects(self, Rectangle other_rect)
{
	Real[<4>] a = self.v;
	Real[<4>] b = other_rect.v;
	return a.x + a.z > b.x
		&& a.x < b.x + b.z
		&& a.y + a.w > b.y
		&& a.y < b.y + b.w;
}
