module main;
import std;
import libc;

Clock last_frame;
char* fb = null;

fn char* get_framebuffer() @export("get_framebuffer")
{
	if (!fb) fb = (char*)libc::calloc(WIDTH * HEIGHT * 4, 1);
	return fb;
}

Vec2 pos = { 400.0f, 300.0f };
Vec2 vel = { 220.0f, 180.0f };
const int WIDTH  = 800;
const int HEIGHT = 600;
const int RADIUS = 50;
const float DT_CAP = 0.1f;
typedef Color = char[<4>];

struct Circle
{
	Vec2 origin;
	float radius;
}

fn bool Circle.contains(Circle self, Vec2 p) @inline => self.origin.distance_sq(p) <= self.radius * self.radius;

fn void update_and_render() @export("update_and_render")
{
	if (!fb) return;

	Clock now = clock::now();
	float dt = (float)(now - last_frame).to_sec();
	last_frame = now;

	if (dt > DT_CAP) dt = DT_CAP;

	pos += vel * dt;

	Vec2 min_p = { (float)RADIUS, (float)RADIUS };
	Vec2 max_p = { (float)WIDTH - RADIUS, (float)HEIGHT - RADIUS };

	bool[<2>] hit = pos.comp_lt(min_p) | pos.comp_gt(max_p);
	if (hit.x) vel.x = -vel.x;
	if (hit.y) vel.y = -vel.y;

	pos = pos.clamp(min_p, max_p);

	fb[:WIDTH * HEIGHT * 4] = 0;

	Circle dot = { .origin = pos, .radius = (float)RADIUS };
	Color color = { 255, 0, 0, 255 };

	int y_min = math::max(0, (int)pos.y - RADIUS);
	int y_max = math::min(HEIGHT - 1, (int)pos.y + RADIUS);
	int x_min = math::max(0, (int)pos.x - RADIUS);
	int x_max = math::min(WIDTH - 1, (int)pos.x + RADIUS);

	for (int py = y_min; py <= y_max; py++)
	{
		for (int px = x_min; px <= x_max; px++)
		{
			if (dot.contains({ (float)px, (float)py }))
			{
				fb[(py * WIDTH + px) * 4:4] = color[..];
			}
		}
	}
}

fn void main()
{
	io::printn("Initialized...");
}
