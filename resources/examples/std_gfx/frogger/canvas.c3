module frogger;
import std::math;

alias Vec = float[<2>];
alias Color = char[<4>];

struct Image
{
	Color[] px;
	int w, h;
}

fn Image new_image(int w, int h) => { .px = mem::new_array(Color, (sz)w * h), .w = w, .h = h };

fn Color rgba(char r, char g, char b, char a = 255) => { r, g, b, a };

<*
 Scale the colour channels by f, keeping alpha.
*>
fn Color Color.shade(self, float f)
{
	float[<3>] rgb = self.rgb;
	self.rgb = (char[<3>])math::min(rgb * f, (float[<3>]) { 255, 255, 255 });
	return self;
}

<*
 Scale the alpha by f.
*>
fn Color Color.fade(self, float f)
{
	self.a = (char)(self.a * f);
	return self;
}

fn Color Color.mix(self, Color other, float t)
{
	float[<4>] from = self;
	float[<4>] to = other;
	return (Color)(from + (to - from) * t);
}

<*
 Composite colour c with opacity a (0..1) over the pixel at dst.
*>
fn void blend(Color* dst, Color c, float a) @inline
{
	float sa = c.a / 255.0f * a;
	if (sa <= 0) return;
	Color d = *dst;
	float oa = sa + d.a / 255.0f * (1 - sa);
	float[<3>] rgb = d.rgb;
	float[<3>] src = c.rgb;
	d.rgb = (char[<3>])(rgb + (src - rgb) * (sa / oa));
	d.a = (char)(oa * 255 + 0.5f);
	*dst = d;
}

// --- Target and transform

Image* target;
Mat3 world = matrix::IDENTITY3;
Mat3 local = matrix::IDENTITY3;
float scale_factor = 1;

<*
 Shapes are given in local coordinates which are scaled, then rotated by angle and placed at (x, y).
*>
fn void set_origin(float x, float y, float angle = 0, float scale = 1)
{
	Mat3 identity = matrix::IDENTITY3;
	world = identity.translate({ x, y }).rotate(angle).scale({ scale, scale });
	local = identity.scale({ 1 / scale, 1 / scale }).rotate(-angle).translate({ -x, -y });
	scale_factor = scale;
}

fn void set_target(Image* image)
{
	target = image;
	set_origin(0, 0);
}

fn Vec to_world(Vec p) => (world * (float[<3>]) { p.x, p.y, 1 }).xy;

fn Vec to_local(Vec p) => (local * (float[<3>]) { p.x, p.y, 1 }).xy;

// --- Shapes

enum ShapeKind
{
	CIRCLE,
	ELLIPSE,
	BOX,
	CAPSULE,
	TRIANGLE,
	RING,
}

struct Shape
{
	ShapeKind kind;
	Vec a, b, c;
	float r;
}

fn float Shape.distance(&self, Vec p)
{
	switch (self.kind)
	{
		case CIRCLE:
			return (p - self.a).length() - self.r;
		case RING:
			return math::abs((p - self.a).length() - self.r) - self.b.x;
		case ELLIPSE:
			float k = ((p - self.a) / self.b).length();
			return (k - 1) * math::min(self.b.x, self.b.y);
		case BOX:
			Vec q = math::abs(p - self.a) - self.b + self.r;
			return math::max(q, (Vec) {}).length() + math::min(math::max(q.x, q.y), 0f) - self.r;
		case CAPSULE:
			Vec pa = p - self.a;
			Vec ba = self.b - self.a;
			float h = math::clamp(pa.dot(ba) / ba.dot(ba), 0, 1);
			return (pa - ba * h).length() - self.r;
		case TRIANGLE:
			Vec[3] v = { self.a, self.b, self.c };
			Vec e1 = v[1] - v[0];
			Vec e2 = v[2] - v[0];
			float side = e1.x * e2.y - e1.y * e2.x < 0 ? -1 : 1;
			float d = -float::inf;
			for (int i = 0; i < 3; i++)
			{
				Vec e = v[(i + 1) % 3] - v[i];
				Vec n = (Vec) { e.y, -e.x } * side / e.length();
				d = math::max(d, (p - v[i]).dot(n));
			}
			return d;
	}
}

<*
 The bounding box of the shape, in local coordinates.
*>
fn Vec[2] Shape.bounds(&self)
{
	switch (self.kind)
	{
		case CIRCLE:
			return { self.a - self.r, self.a + self.r };
		case RING:
			return { self.a - self.r - self.b.x, self.a + self.r + self.b.x };
		case ELLIPSE:
		case BOX:
			return { self.a - self.b, self.a + self.b };
		case CAPSULE:
			return { math::min(self.a, self.b) - self.r, math::max(self.a, self.b) + self.r };
		case TRIANGLE:
			return { math::min(self.a, self.b, self.c), math::max(self.a, self.b, self.c) };
	}
}

fn void draw_shape(Shape shape, Color color)
{
	Vec[2] b = shape.bounds();
	Vec[4] corners = { to_world(b[0]), to_world({ b[1].x, b[0].y }), to_world({ b[0].x, b[1].y }), to_world(b[1]) };
	Vec lo = corners[0];
	Vec hi = corners[0];
	foreach (c : corners[1..])
	{
		lo = math::min(lo, c);
		hi = math::max(hi, c);
	}
	int x0 = math::max((int)lo.x - 1, 0);
	int y0 = math::max((int)lo.y - 1, 0);
	int x1 = math::min((int)hi.x + 2, target.w);
	int y1 = math::min((int)hi.y + 2, target.h);
	for (int y = y0; y < y1; y++)
	{
		Color* row = &target.px[y * target.w];
		for (int x = x0; x < x1; x++)
		{
			float d = shape.distance(to_local({ x + 0.5f, y + 0.5f })) * scale_factor;
			if (d < 0.5f) blend(row + x, color, math::min(0.5f - d, 1f));
		}
	}
}

fn void circle(float x, float y, float r, Color color) => draw_shape({ .kind = CIRCLE, .a = { x, y }, .r = r }, color);

fn void ring(float x, float y, float r, float width, Color color) => draw_shape({ .kind = RING, .a = { x, y }, .b = { width / 2, 0 }, .r = r }, color);

fn void ellipse(float x, float y, float rx, float ry, Color color) => draw_shape({ .kind = ELLIPSE, .a = { x, y }, .b = { rx, ry } }, color);

<*
 A box centred on (x, y) with half extents (hw, hh) and corner radius r.
*>
fn void box(float x, float y, float hw, float hh, float r, Color color)
{
	draw_shape({ .kind = BOX, .a = { x, y }, .b = { hw, hh }, .r = math::min(r, hw, hh) }, color);
}

fn void capsule(float x0, float y0, float x1, float y1, float r, Color color)
{
	draw_shape({ .kind = CAPSULE, .a = { x0, y0 }, .b = { x1, y1 }, .r = r }, color);
}

fn void triangle(Vec a, Vec b, Vec c, Color color) => draw_shape({ .kind = TRIANGLE, .a = a, .b = b, .c = c }, color);

// --- Pixel operations

fn void fill_rect(int x, int y, int w, int h, Color color)
{
	for (int j = math::max(y, 0); j < math::min(y + h, target.h); j++)
	{
		for (int i = math::max(x, 0); i < math::min(x + w, target.w); i++) blend(&target.px[j * target.w + i], color, 1);
	}
}

<*
 Draw the image with its top left corner at (x, y). A tint with non-zero alpha replaces the colour but keeps the alpha.
*>
fn void blit(Image* image, int x, int y, bool flip = false, Color tint = {})
{
	int x0 = math::max(x, 0);
	int x1 = math::min(x + image.w, target.w);
	for (int j = math::max(y, 0); j < math::min(y + image.h, target.h); j++)
	{
		Color* src = &image.px[(j - y) * image.w];
		Color* dst = &target.px[j * target.w];
		for (int i = x0; i < x1; i++)
		{
			Color c = src[flip ? image.w - 1 - (i - x) : i - x];
			if (!c.a) continue;
			if (tint.a) c.rgb = tint.rgb;
			if (c.a == 255)
			{
				dst[i] = c;
				continue;
			}
			blend(dst + i, c, 1);
		}
	}
}

// --- Text

<*
 Rows of a 5x7 glyph, bit 4 is the leftmost column.
*>
fn char[7] glyph(char c)
{
	switch (c)
	{
		case '0': return x"0E11131519110E";
		case '1': return x"040C040404040E";
		case '2': return x"0E11010204081F";
		case '3': return x"1F02040201110E";
		case '4': return x"02060A121F0202";
		case '5': return x"1F101E0101110E";
		case '6': return x"0608101E11110E";
		case '7': return x"1F010204080808";
		case '8': return x"0E11110E11110E";
		case '9': return x"0E11110F01020C";
		case 'A': return x"0E1111111F1111";
		case 'C': return x"0E11101010110E";
		case 'E': return x"1F10101E10101F";
		case 'G': return x"0E11101711110F";
		case 'H': return x"1111111F111111";
		case 'I': return x"0E04040404040E";
		case 'L': return x"1010101010101F";
		case 'M': return x"111B1515111111";
		case 'N': return x"11111915131111";
		case 'O': return x"0E11111111110E";
		case 'P': return x"1E11111E101010";
		case 'R': return x"1E11111E141211";
		case 'S': return x"0F10100E01011E";
		case 'T': return x"1F040404040404";
		case 'V': return x"11111111110A04";
		case 'W': return x"1111111515150A";
		case 'Y': return x"1111110A040404";
		default: return {};
	}
}

fn float text_width(String s, float size) => 6 * size * s.len - size;

<*
 Draw text in a 5x7 pixel font, each font pixel a square of the given size, with a drop shadow.
*>
fn void text(String s, float x, float y, float size, Color color)
{
	set_origin(0, 0);
	foreach (pass : (bool[2]) { false, true })
	{
		Vec pos = pass ? (Vec) { x, y } : (Vec) { x, y } + size * 0.5f;
		Color c = pass ? color : rgba(0, 0, 0, color.a).fade(0.55f);
		foreach (ch : s)
		{
			char[7] g = glyph(ch);
			for (int row = 0; row < 7; row++)
			{
				for (int col = 0; col < 5; col++)
				{
					if (!((g[row] >> (4 - col)) & 1)) continue;
					Vec centre = pos + size * (Vec) { col, row } + size * 0.5f;
					box(centre.x, centre.y, size * 0.56f, size * 0.56f, size * 0.12f, c);
				}
			}
			pos.x += 6 * size;
		}
	}
}
