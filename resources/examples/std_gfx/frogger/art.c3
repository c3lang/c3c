module frogger;
import std::math;

const int FROG_FRAMES = 5;
const int FROG_BLINK = 4;
const int WHEEL_FRAMES = 4;
const int JAW_STEPS = 5;
const int SWAY_STEPS = 8;
const int CROC_LEN = 3;
const int SWIM_STEPS = 4;
const int DEPTH_STEPS = 4;
const int CAR_COLORS = 5;
enum Vehicle : char (int tiles)
{
	CAR    { 1 },
	TRUCK  { 2 },
	SPORTS { 1 },
	POLICE { 1 },
	BUS    { 3 },
	BIKE   { 1 },
}

const Color[CAR_COLORS] CAR_PAINT = { { 216, 64, 58, 255 }, { 240, 140, 45, 255 }, { 48, 160, 224, 255 }, { 245, 184, 64, 255 }, { 192, 80, 176, 255 } };

struct Sprites
{
	Image[FROG_FRAMES][4] frog;
	Image life_icon;
	Image[WHEEL_FRAMES][CAR_COLORS][Vehicle::len] vehicle;
	Image[3] log;
	Image[SWAY_STEPS][JAW_STEPS] croc;
	Image[SWIM_STEPS][DEPTH_STEPS] turtle;
	Image[2] lily;
	Image background;
}

Sprites sprites;

const Color OUTLINE = { 28, 58, 20, 255 };
const Color WATER = { 48, 120, 192, 255 };
const Color BAY_WATER = { 36, 110, 176, 255 };
const Color GLASS = { 48, 74, 106, 255 };

<*
 Paint a frog facing up, centred on the origin. Stretch 0 sits, 1 is fully extended.
*>
fn void paint_frog(float stretch, bool blink)
{
	Color skin = rgba(110, 196, 70);
	Color dark = rgba(58, 130, 46);
	foreach (side : (float[2]) { -1, 1 })
	{
		Vec hip = { side * 8, 6 };
		Vec knee = { side * (16 - stretch * 4), 10 + stretch * 6 };
		Vec foot = { side * (11 - stretch * 5), 15 + stretch * 11 };
		capsule(hip.x, hip.y, knee.x, knee.y, 4.6f, OUTLINE);
		capsule(knee.x, knee.y, foot.x, foot.y, 3.8f, OUTLINE);
		capsule(hip.x, hip.y, knee.x, knee.y, 3.4f, dark);
		capsule(knee.x, knee.y, foot.x, foot.y, 2.6f, dark);
		for (int toe = -1; toe <= 1; toe++)
		{
			capsule(foot.x, foot.y, foot.x + side * 3 + toe * 2.6f, foot.y + 4.5f, 1.4f, dark);
		}
		Vec hand = { side * (14 + stretch * 1), -12 - stretch * 5 };
		capsule(side * 7, -4, hand.x, hand.y, 3.4f, OUTLINE);
		capsule(side * 7, -4, hand.x, hand.y, 2.3f, dark);
		for (int toe = -1; toe <= 1; toe++)
		{
			capsule(hand.x, hand.y, hand.x + side * 1.5f + toe * 2.2f, hand.y - 3.5f, 1.2f, dark);
		}
	}
	ellipse(0, 1, 13, 15.5f, OUTLINE);
	ellipse(0, 1, 11.6f, 14.2f, skin);
	ellipse(0, -1, 7, 10, rgba(140, 214, 90));
	capsule(0, -6, 0, 11, 1.4f, rgba(196, 232, 120));
	circle(-6, 6, 2.3f, dark);
	circle(6, 7, 1.8f, dark);
	circle(-4, -2, 1.5f, dark);
	circle(5, 0, 1.6f, dark);
	foreach (side : (float[2]) { -1, 1 })
	{
		circle(side * 7, -10, 5.4f, OUTLINE);
		circle(side * 7, -10, 4.4f, skin);
		if (blink)
		{
			capsule(side * 5, -11, side * 9.5f, -10, 1, OUTLINE);
			continue;
		}
		circle(side * 7.4f, -11, 3.2f, rgba(250, 240, 170));
		circle(side * 7.6f, -11.5f, 1.9f, rgba(20, 20, 20));
		circle(side * 6.8f, -12.4f, 0.8f, rgba(255, 255, 255));
	}
}

<*
 Paint a wheel at (x, y) whose tread has rolled by phase (0..1).
*>
fn void paint_wheel(float x, float y, float hw, float phase)
{
	box(x, y, hw, 3.6f, 1.6f, rgba(24, 24, 26));
	for (int i = 0; i < 4; i++)
	{
		float t = (i + phase) / 4;
		float tx = x - hw + 1.4f + t * (hw * 2 - 2.8f);
		box(tx, y, 0.7f, 2.8f, 0.4f, rgba(80, 80, 84));
	}
}

<*
 A car facing right, centred on the origin.
*>
fn void paint_car(Color paint, float phase)
{
	box(2, 4, 21, 13, 7, rgba(0, 0, 0, 80));
	foreach (wx : (float[2]) { -12, 12 })
	{
		foreach (wy : (float[2]) { -12.5f, 12.5f }) paint_wheel(wx, wy, 5.5f, phase);
	}
	box(0, 0, 21.5f, 13, 7, paint.shade(0.45f));
	box(0, 0, 20.3f, 11.8f, 6.2f, paint);
	box(11, 0, 7.5f, 9, 4, paint.shade(1.12f));
	box(-3, 0, 9, 10.4f, 4, paint.shade(0.7f));
	box(4.5f, 0, 2.8f, 9.4f, 2, GLASS);
	box(-11, 0, 1.8f, 8.6f, 1.4f, GLASS.shade(0.8f));
	box(-3.2f, 0, 5.8f, 9, 3, paint.shade(1.05f));
	capsule(-6, -5, 0, -5, 1.1f, rgba(255, 255, 255, 90));
	capsule(4, -7, 4.5f, -2, 0.9f, rgba(255, 255, 255, 110));
	foreach (side : (float[2]) { -1, 1 })
	{
		circle(19, side * 8, 2.3f, rgba(255, 246, 190));
		box(-20.3f, side * 8, 0.9f, 2.4f, 0.6f, rgba(220, 40, 40));
		ellipse(5.5f, side * 12.6f, 1.7f, 1.3f, paint.shade(0.8f));
	}
}

<*
 A lorry facing right, two tiles long, centred on the origin.
*>
fn void paint_truck(Color paint, float phase)
{
	box(2, 4, 45, 14, 4, rgba(0, 0, 0, 80));
	foreach (wx : (float[3]) { -34, -22, 31 })
	{
		foreach (wy : (float[2]) { -13, 13 }) paint_wheel(wx, wy, 5.5f, phase);
	}
	box(-11, 0, 34, 14, 2.5f, rgba(90, 90, 96));
	box(-11, 0, 33, 13, 2, rgba(222, 224, 228));
	for (float x = -40; x < 20; x += 6) capsule(x, -11.5f, x, 11.5f, 0.7f, rgba(190, 192, 198));
	capsule(-42, -11, 20, -11, 1, rgba(255, 255, 255, 140));
	box(26, 0, 4, 6, 1, rgba(50, 50, 54));
	box(35, 0, 10, 12.5f, 4, paint.shade(0.45f));
	box(35, 0, 9, 11.5f, 3.5f, paint);
	box(38.5f, 0, 2.6f, 9.6f, 1.6f, GLASS);
	box(31, 0, 3.5f, 8.5f, 1.5f, paint.shade(1.15f));
	foreach (side : (float[2]) { -1, 1 })
	{
		circle(43.5f, side * 8, 2, rgba(255, 246, 190));
		box(-44.2f, side * 9, 0.9f, 2.2f, 0.5f, rgba(220, 40, 40));
		ellipse(37, side * 12.8f, 1.5f, 1.4f, paint.shade(0.8f));
	}
}

<*
 A low sports car facing right, with racing stripes and a spoiler.
*>
fn void paint_sports(Color paint, float phase)
{
	box(2, 4, 22, 12, 8, rgba(0, 0, 0, 80));
	foreach (wx : (float[2]) { -13, 12 })
	{
		foreach (wy : (float[2]) { -12, 12 }) paint_wheel(wx, wy, wx < 0 ? 6.5f : 5.5f, phase);
	}
	box(0, 0, 22.5f, 11.5f, 8, paint.shade(0.45f));
	box(0, 0, 21.4f, 10.4f, 7.4f, paint);
	box(12, 0, 9, 7, 5, paint.shade(1.15f));
	foreach (y : (float[2]) { -2.4f, 2.4f }) capsule(-20, y, 21, y, 1.1f, rgba(250, 250, 250, 230));
	ellipse(-3, 0, 8, 7.5f, paint.shade(0.45f));
	ellipse(-3, 0, 7, 6.5f, GLASS);
	ellipse(-1, -2.5f, 3.5f, 2, rgba(255, 255, 255, 80));
	box(-20, 0, 2, 11.5f, 1, rgba(30, 30, 34));
	foreach (side : (float[2]) { -1, 1 })
	{
		box(20.5f, side * 6.5f, 1.2f, 2.6f, 0.8f, rgba(255, 246, 190));
		box(-21.8f, side * 6.5f, 0.8f, 2.4f, 0.5f, rgba(230, 40, 40));
	}
}

<*
 A police car facing right. The flashing of the light bar is drawn while playing.
*>
fn void paint_police(float phase)
{
	Color black = rgba(30, 32, 40);
	Color white = rgba(240, 240, 244);
	box(2, 4, 21, 13, 7, rgba(0, 0, 0, 80));
	foreach (wx : (float[2]) { -12, 12 })
	{
		foreach (wy : (float[2]) { -12.5f, 12.5f }) paint_wheel(wx, wy, 5.5f, phase);
	}
	box(0, 0, 21.5f, 13, 7, rgba(10, 10, 14));
	box(0, 0, 20.3f, 11.8f, 6.2f, black);
	box(-2, 0, 9, 11.8f, 2, white);
	box(4.5f, 0, 2.8f, 9.4f, 2, GLASS);
	box(-11, 0, 1.8f, 8.6f, 1.4f, GLASS.shade(0.8f));
	box(-3.2f, 0, 5.8f, 9, 3, white.shade(0.95f));
	box(-3, 0, 1.8f, 8.5f, 1, rgba(40, 40, 46));
	box(-3, -4.5f, 1.4f, 3.4f, 0.8f, rgba(130, 30, 30));
	box(-3, 4.5f, 1.4f, 3.4f, 0.8f, rgba(30, 50, 140));
	foreach (side : (float[2]) { -1, 1 })
	{
		circle(19, side * 8, 2.3f, rgba(255, 246, 190));
		box(-20.3f, side * 8, 0.9f, 2.4f, 0.6f, rgba(220, 40, 40));
	}
}

<*
 A yellow bus facing right, three tiles long, with a stripe in the given paint.
*>
fn void paint_bus(Color paint, float phase)
{
	Color yellow = rgba(246, 190, 40);
	box(2, 4, 69, 14.5f, 5, rgba(0, 0, 0, 80));
	foreach (wx : (float[3]) { -50, -38, 50 })
	{
		foreach (wy : (float[2]) { -13.5f, 13.5f }) paint_wheel(wx, wy, 5.5f, phase);
	}
	box(0, 0, 70, 14.5f, 4.5f, yellow.shade(0.5f));
	box(0, 0, 69, 13.5f, 4, yellow);
	box(-2, 0, 63, 9.5f, 3, yellow.shade(1.06f));
	for (float x = -60; x < 58; x += 10)
	{
		foreach (side : (float[2]) { -1, 1 }) box(x, side * 11.4f, 4, 1.5f, 0.8f, GLASS);
	}
	foreach (side : (float[2]) { -1, 1 }) capsule(-66, side * 8.6f, 62, side * 8.6f, 0.9f, paint);
	foreach (vx : (float[2]) { -32, 12 })
	{
		box(vx, 0, 6, 4.5f, 1.5f, rgba(150, 150, 156));
		box(vx, 0, 5, 3.5f, 1.2f, rgba(214, 214, 220));
	}
	box(66.2f, 0, 2.2f, 11, 1.4f, GLASS);
	foreach (side : (float[2]) { -1, 1 })
	{
		circle(69, side * 10, 2.2f, rgba(255, 246, 190));
		box(-69.6f, side * 10, 0.8f, 2.4f, 0.5f, rgba(220, 40, 40));
	}
}

<*
 A motorbike with its rider, facing right.
*>
fn void paint_bike(Color paint, float phase)
{
	Color jacket = rgba(44, 48, 64);
	ellipse(2, 3, 19, 7, rgba(0, 0, 0, 70));
	paint_wheel(-12, 0, 6, phase);
	paint_wheel(12, 0, 6, phase);
	capsule(-10, 0, 10, 0, 4.2f, paint.shade(0.45f));
	capsule(-10, 0, 10, 0, 3.1f, paint);
	ellipse(5, 0, 5, 3.4f, paint.shade(1.15f));
	capsule(12, -7.5f, 12, 7.5f, 1, rgba(40, 40, 44));
	foreach (side : (float[2]) { -1, 1 })
	{
		capsule(-1, side * 5.5f, 11, side * 7, 1.9f, jacket);
		circle(12, side * 7.5f, 1.6f, rgba(20, 20, 22));
	}
	ellipse(-3, 0, 5, 7.5f, jacket.shade(0.7f));
	ellipse(-3, 0, 4.2f, 6.8f, jacket);
	circle(1, 0, 5, paint.shade(0.5f));
	circle(1, 0, 4.2f, paint.shade(0.85f));
	capsule(3, -2.5f, 3, 2.5f, 1.2f, rgba(30, 30, 40));
	circle(-0.5f, -1.8f, 1.2f, rgba(255, 255, 255, 120));
	circle(18.5f, 0, 2, rgba(255, 246, 190));
	box(-18.5f, 0, 0.8f, 1.8f, 0.5f, rgba(230, 40, 40));
}

fn void paint_vehicle(Vehicle vehicle, Color paint, float phase)
{
	switch (vehicle)
	{
		case CAR: paint_car(paint, phase);
		case TRUCK: paint_truck(paint, phase);
		case SPORTS: paint_sports(paint, phase);
		case POLICE: paint_police(phase);
		case BUS: paint_bus(paint, phase);
		case BIKE: paint_bike(paint, phase);
	}
}

<*
 A floating log of len tiles, centred on the origin.
*>
fn void paint_log(int len)
{
	float hw = len * TILE_F / 2 - 3;
	float hh = 15;
	box(0, 4, hw, hh, 5, rgba(10, 40, 70, 90));
	box(0, 0, hw, hh, 5, rgba(70, 40, 22));
	box(0, -1.5f, hw - 1, hh - 3, 5, rgba(112, 68, 36));
	box(0, -4, hw - 2, hh - 8, 4, rgba(138, 88, 48));
	capsule(-hw + 6, -8, hw - 8, -8, 1.6f, rgba(176, 124, 72));
	for (int i = 0; i < len * 7; i++)
	{
		float x = rand_float(-hw + 6, hw - 14);
		float y = rand_float(-11, 11);
		float l = rand_float(6, 18);
		capsule(x, y, x + l, y + rand_float(-1, 1), 0.9f, rgba(70, 40, 22, 200));
	}
	for (int i = 0; i < len; i++)
	{
		float x = rand_float(-hw + 14, hw - 18);
		float y = rand_float(-6, 6);
		ellipse(x, y, 3.6f, 2.6f, rgba(150, 100, 56));
		ellipse(x, y, 2.4f, 1.6f, rgba(64, 36, 20));
	}
	for (int i = 0; i < len * 2; i++)
	{
		circle(rand_float(-hw + 8, hw - 12), rand_float(-12, -4), rand_float(1.5f, 3), rgba(96, 150, 60, 220));
	}
	ellipse(-hw + 1, 0, 3, hh - 1, rgba(80, 48, 26));
	ellipse(hw - 2, 0, 5, hh, rgba(90, 54, 28));
	ellipse(hw - 2, 0, 4.2f, hh - 1.4f, rgba(214, 168, 108));
	ellipse(hw - 2, 0, 3.2f, hh - 5, rgba(196, 146, 88));
	ellipse(hw - 2, 0, 2.2f, hh - 9, rgba(214, 168, 108));
	ellipse(hw - 2, 0, 1.2f, hh - 12, rgba(170, 120, 70));
}

<*
 A crocodile in profile swimming left, CROC_LEN tiles long, centred on the origin.
 The jaw opens from 0 to 1, sway (0..1) is the phase of the tail and legs.
*>
fn void paint_croc(Vec centre, float jaw, float sway)
{
	Color hide = rgba(70, 122, 52);
	Color dark = rgba(40, 76, 34);
	Color light = rgba(120, 164, 74);
	Color belly = rgba(176, 190, 104);
	Color tooth = rgba(250, 248, 230);
	float wave = sway * 2 * PI;
	set_origin(centre.x, centre.y);
	// Far legs, behind the body.
	foreach (int k, lx : (float[2]) { -24, 10 })
	{
		float swing = math::sin(wave + k * PI) * 5;
		capsule(lx + 5, 4, lx + 5 - swing, 15, 2.8f, dark);
	}
	// Tail with a ridged top and pale underside.
	for (int pass = 0; pass < 3; pass++)
	{
		for (int i = 0; i <= 16; i++)
		{
			float t = i / 16f;
			float x = 18 + t * 52;
			float y = 2 + math::sin(wave - t * 4) * t * 3;
			float r = 8.5f * (1 - t) + 1.8f;
			switch (pass)
			{
				case 0:
					circle(x, y, r + 1.2f, OUTLINE);
					if (i % 2) triangle({ x - 2.4f, y - r + 1 }, { x + 2.4f, y - r + 1 }, { x, y - r - 3.5f * (1 - t) - 1.2f }, OUTLINE);
				case 1:
					circle(x, y, r, hide);
					if (i % 2) triangle({ x - 1.6f, y - r + 1 }, { x + 1.6f, y - r + 1 }, { x, y - r - 3.5f * (1 - t) }, dark);
				case 2:
					circle(x, y + r * 0.45f, r * 0.5f, belly);
			}
		}
	}
	// Body.
	for (float x = -28; x <= 16; x += 6)
	{
		triangle({ x - 3.4f, -6 }, { x + 3.4f, -6 }, { x, -13.5f }, OUTLINE);
		triangle({ x - 2.4f, -6 }, { x + 2.4f, -6 }, { x, -12.2f }, dark);
	}
	ellipse(-6, 1, 31.2f, 11.2f, OUTLINE);
	ellipse(-6, 1, 30, 10, hide);
	ellipse(-6, 6, 26, 4.5f, belly);
	capsule(-28, -5, 14, -5, 1.2f, light);
	for (float x = -26; x <= 14; x += 5) box(x, 0.5f, 1.8f, 1.4f, 0.6f, dark);
	// Near legs.
	foreach (int k, lx : (float[2]) { -24, 10 })
	{
		float swing = math::sin(wave + k * PI + 2) * 5;
		Vec foot = { lx + swing, 16 };
		capsule(lx, 6, foot.x, foot.y, 3.8f, OUTLINE);
		capsule(lx, 6, foot.x, foot.y, 2.7f, hide);
		for (int c = -1; c <= 1; c++) capsule(foot.x, foot.y, foot.x - 2 + c * 2.4f, foot.y + 3, 0.9f, dark);
	}
	// Mouth and lower jaw.
	Vec hinge = { -38, 1 };
	float angle = jaw * 0.5f;
	Vec upper_tip = hinge + (Vec) { -31 * math::cos(angle) - math::sin(angle), -31 * math::sin(angle) + math::cos(angle) };
	if (jaw > 0)
	{
		triangle(hinge + (Vec) { 4, 0 }, upper_tip, { -66, 4 }, rgba(214, 92, 104));
		triangle(hinge + (Vec) { 2, 1 }, upper_tip * 0.35f + hinge * 0.65f, { -50, 3 }, rgba(110, 24, 36));
	}
	capsule(-38, 5.5f, -64, 5.5f, 4.5f, OUTLINE);
	capsule(-38, 5.5f, -64, 5.5f, 3.3f, belly.shade(0.85f));
	for (float x = -62; x < -42; x += 4.5f) triangle({ x - 1.5f, 3 }, { x + 1.5f, 3 }, { x, -0.4f }, tooth);
	ellipse(-38, 1, 11.2f, 9.2f, OUTLINE);
	ellipse(-38, 1, 10, 8, hide);
	// Upper jaw, lifted around the hinge.
	set_origin(centre.x + hinge.x, centre.y + hinge.y, angle);
	capsule(2, -2.5f, -30, -1, 5.2f, OUTLINE);
	capsule(2, -2.5f, -30, -1, 4, hide);
	capsule(-4, -5, -25, -3.5f, 1, light);
	circle(-29, -4.5f, 2.8f, OUTLINE);
	circle(-29, -4.5f, 1.8f, hide);
	circle(-29.5f, -5, 0.8f, dark);
	for (float x = -27; x < -6; x += 4.5f) triangle({ x - 1.5f, 2.2f }, { x + 1.5f, 2.2f }, { x, 5.6f }, tooth);
	// Eye on top of the head.
	set_origin(centre.x, centre.y);
	circle(-33, -7.5f, 5.4f, OUTLINE);
	circle(-33, -7.5f, 4.2f, hide);
	circle(-33.5f, -8.5f, 2.9f, rgba(236, 204, 60));
	box(-33.5f, -8.5f, 0.6f, 2.3f, 0.5f, rgba(20, 20, 10));
}

<*
 A turtle swimming left, centred on the origin. Paddle (0..1) is the phase of the flippers.
*>
fn void paint_turtle(float paddle)
{
	Color skin = rgba(124, 176, 92);
	Color rim = rgba(92, 60, 32);
	Color shell = rgba(150, 102, 52);
	Color scute = rgba(184, 132, 66);
	float swing = math::sin(paddle * 2 * PI);
	foreach (side : (float[2]) { -1, 1 })
	{
		Vec front = { -12 + swing * side * 3, side * 17 };
		Vec back = { 13 - swing * side * 2, side * 13 };
		capsule(-5, side * 8, front.x, front.y, 4, OUTLINE);
		capsule(-5, side * 8, front.x, front.y, 2.9f, skin);
		capsule(8, side * 7, back.x, back.y, 3.3f, OUTLINE);
		capsule(8, side * 7, back.x, back.y, 2.2f, skin);
	}
	triangle({ 13, -2.5f }, { 13, 2.5f }, { 19, 0 }, OUTLINE);
	ellipse(-17, 0, 6.6f, 5.6f, OUTLINE);
	ellipse(-17, 0, 5.4f, 4.4f, skin);
	foreach (side : (float[2]) { -1, 1 }) circle(-19.5f, side * 2.4f, 1.1f, rgba(20, 30, 20));
	ellipse(0, 0, 15.5f, 13.5f, OUTLINE);
	ellipse(0, 0, 14.3f, 12.3f, rim);
	ellipse(0, 0, 12, 10, shell);
	circle(0, 0, 4.2f, scute);
	for (int i = 0; i < 6; i++)
	{
		float a = i * PI / 3 + PI / 6;
		circle(math::cos(a) * 7.6f, math::sin(a) * 6.2f, 2.8f, scute);
	}
	ellipse(-3, -4.5f, 5, 2.2f, rgba(255, 255, 255, 50));
}

<*
 Sink the image into the river: blend it towards the water and fade it out.
*>
fn void Image.submerge(&self, float depth)
{
	foreach (&c : self.px)
	{
		if (!c.a) continue;
		Color under = WATER;
		under.a = c.a;
		*c = c.mix(under, depth * 0.8f).fade(1 - depth * 0.5f);
	}
}

fn void paint_lily(Vec centre, bool flower)
{
	set_origin(centre.x, centre.y);
	circle(1.5f, 2.5f, 19, rgba(10, 60, 80, 90));
	circle(0, 0, 19, rgba(44, 110, 48));
	circle(0, 0, 17.6f, rgba(78, 158, 66));
	for (int i = 0; i < 9; i++)
	{
		float a = 0.75f + i * 0.62f;
		capsule(0, 0, math::cos(a) * 15, math::sin(a) * 15, 0.7f, rgba(56, 128, 52));
	}
	triangle({ 0, 0 }, { 21, -5 }, { 21, 8 }, BAY_WATER);
	if (!flower) return;
	for (int i = 0; i < 6; i++)
	{
		float a = i * PI / 3;
		ellipse(-5 + math::cos(a) * 3.6f, -4 + math::sin(a) * 3.6f, 3, 3, rgba(250, 170, 210));
	}
	circle(-5, -4, 2.2f, rgba(250, 220, 90));
}

fn void paint_grass(int row)
{
	float y0 = row * TILE_F;
	fill_rect(0, row * TILE, SCREEN_W, TILE, rgba(84, 152, 64));
	for (int i = 0; i < 260; i++)
	{
		float x = rand_float(0, SCREEN_W);
		float y = rand_float(y0 + 4, y0 + TILE - 3);
		Color c = random::next_bool(&rng) ? rgba(62, 128, 50) : rgba(118, 184, 82);
		capsule(x, y, x + rand_float(-2, 2), y - rand_float(3, 6), 0.8f, c);
	}
	for (int i = 0; i < 9; i++)
	{
		float x = rand_float(10, SCREEN_W - 10);
		float y = rand_float(y0 + 10, y0 + TILE - 10);
		Color petal = random::next_bool(&rng) ? rgba(255, 255, 255) : rgba(255, 220, 90);
		for (int p = 0; p < 5; p++)
		{
			float a = p * 1.2566f;
			circle(x + math::cos(a) * 2.2f, y + math::sin(a) * 2.2f, 1.6f, petal);
		}
		circle(x, y, 1.2f, rgba(240, 150, 40));
	}
}

fn void paint_background()
{
	set_target(&sprites.background);
	// River.
	for (int y = TILE; y < MEDIAN_ROW * TILE; y++)
	{
		float t = (y - TILE_F) / (4 * TILE_F);
		fill_rect(0, y, SCREEN_W, 1, rgba(36, 110, 176).mix(WATER, t));
	}
	// Hedge with bays.
	fill_rect(0, 0, SCREEN_W, TILE, rgba(34, 86, 40));
	for (int i = 0; i < 90; i++)
	{
		float x = rand_float(-6, SCREEN_W + 6);
		float y = rand_float(4, TILE - 2);
		float r = rand_float(6, 11);
		circle(x, y, r, rgba(30, 80, 36).mix(rgba(70, 136, 60), random::next_float(&rng)));
		circle(x - r * 0.3f, y - r * 0.3f, r * 0.35f, rgba(120, 180, 90, 90));
	}
	foreach (bay : BAYS)
	{
		float cx = (bay + 0.5f) * TILE_F;
		box(cx, 27, 23, 23, 10, rgba(20, 56, 30));
		box(cx, 28, 21, 21, 9, BAY_WATER);
	}
	paint_grass(MEDIAN_ROW);
	paint_grass(START_ROW);
	fill_rect(0, MEDIAN_ROW * TILE, SCREEN_W, 3, rgba(150, 120, 70));
	fill_rect(0, MEDIAN_ROW * TILE + 3, SCREEN_W, 2, rgba(110, 90, 54));
	// Road.
	int road_y = (MEDIAN_ROW + 1) * TILE;
	int road_h = (START_ROW - MEDIAN_ROW - 1) * TILE;
	fill_rect(0, road_y, SCREEN_W, road_h, rgba(54, 56, 60));
	for (int i = 0; i < 3000; i++)
	{
		circle(rand_float(0, SCREEN_W), road_y + rand_float(0, road_h), rand_float(0.5f, 1.4f), random::next_bool(&rng) ? rgba(40, 42, 46) : rgba(76, 78, 82));
	}
	for (int lane = 1; lane < 4; lane++)
	{
		for (int x = 8; x < SCREEN_W; x += TILE) box(x + 12f, road_y + lane * TILE_F, 12, 1.6f, 1, rgba(230, 230, 220));
	}
	foreach (y : (int[2]) { road_y, road_y + road_h })
	{
		fill_rect(0, y - 4, SCREEN_W, 8, rgba(170, 170, 164));
		for (int x = 0; x < SCREEN_W; x += 24) fill_rect(x, y - 4, 2, 8, rgba(120, 120, 116));
		fill_rect(0, y + (y == road_y ? 3 : -4), SCREEN_W, 1, rgba(100, 100, 96));
	}
	// Status bar.
	for (int i = 0; i < TILE; i++)
	{
		fill_rect(0, HUD_ROW * TILE + i, SCREEN_W, 1, rgba(36, 40, 50).mix(rgba(16, 18, 24), i / TILE_F));
	}
}

fn void paint_sprites()
{
	random::seed(&rng, 0x2545F491);
	sprites.background = new_image(SCREEN_W, SCREEN_H);
	paint_background();
	foreach (dir, &frames : sprites.frog)
	{
		const float[FROG_FRAMES] STRETCH = { 0, -0.25f, 1, 0.5f, 0 };
		foreach (f, &image : *frames)
		{
			*image = new_image(TILE, TILE);
			set_target(image);
			set_origin(TILE / 2.0f, TILE / 2.0f, dir * PI / 2);
			paint_frog(STRETCH[f], f == FROG_BLINK);
		}
	}
	sprites.life_icon = new_image(26, 26);
	set_target(&sprites.life_icon);
	set_origin(13, 14, 0, 0.5f);
	paint_frog(0, false);
	foreach (vehicle : Vehicle::values)
	{
		for (int c = 0; c < CAR_COLORS; c++)
		{
			for (int f = 0; f < WHEEL_FRAMES; f++)
			{
				Image* image = &sprites.vehicle[vehicle.ordinal][c][f];
				*image = new_image(vehicle.tiles * TILE, TILE);
				set_target(image);
				set_origin(vehicle.tiles * TILE_F / 2, TILE_F / 2, 0, vehicle == BIKE ? 1.15f : 1);
				paint_vehicle(vehicle, CAR_PAINT[c], (float)f / WHEEL_FRAMES);
			}
		}
	}
	foreach (int i, &image : sprites.log)
	{
		int len = i + 2;
		*image = new_image(len * TILE, TILE);
		set_target(image);
		set_origin(len * TILE_F / 2, TILE_F / 2);
		paint_log(len);
	}
	for (int j = 0; j < JAW_STEPS; j++)
	{
		for (int s = 0; s < SWAY_STEPS; s++)
		{
			sprites.croc[j][s] = new_image(CROC_LEN * TILE, TILE);
			set_target(&sprites.croc[j][s]);
			paint_croc({ CROC_LEN * TILE / 2.0f, TILE / 2.0f }, j / (JAW_STEPS - 1.0f), (float)s / SWAY_STEPS);
		}
	}
	for (int d = 0; d < DEPTH_STEPS; d++)
	{
		for (int s = 0; s < SWIM_STEPS; s++)
		{
			Image* image = &sprites.turtle[d][s];
			*image = new_image(TILE, TILE);
			set_target(image);
			set_origin(TILE_F / 2, TILE_F / 2);
			paint_turtle((float)s / SWIM_STEPS);
			image.submerge((float)d / DEPTH_STEPS);
		}
	}
	foreach (i, &image : sprites.lily)
	{
		*image = new_image(TILE, TILE);
		set_target(image);
		paint_lily({ TILE / 2.0f, TILE / 2.0f }, i == 1);
	}
}
