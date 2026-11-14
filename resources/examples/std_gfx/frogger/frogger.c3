module frogger;
import std;

const int TILE = 48;
const float TILE_F = TILE;
const float PI = math::PI;
const int COLS = 15;
const int ROWS = 12;
const int SCREEN_W = COLS * TILE;
const int SCREEN_H = ROWS * TILE;

const int HOME_ROW = 0;
const int MEDIAN_ROW = 5;
const int START_ROW = 10;
const int HUD_ROW = 11;
const int[5] BAYS = { 1, 4, 7, 10, 13 };

const float HOP_TIME = 0.15f;
const float HOP_HEIGHT = 8;
const float DEATH_TIME = 1.4f;
const float LIFE_TIME = 40;
const float BANNER_TIME = 1.8f;
const float WRAP = SCREEN_W + 4 * TILE;
const float CROC_CYCLE = 3.2f;
const float CROC_BITE = 1.2f;
const float CROC_DEADLY = 0.35f;
const float DIVE_CYCLE = 5.5f;
const int MAX_PARTICLES = 256;

enum Kind
{
	CAR,
	LOG,
	CROC,
	TURTLE,
}

struct Thing
{
	float x;
	int len;
	Vehicle vehicle;
	bool dives;
	int look;
	float phase;
	float roll;
	float smoke;
}

struct Lane
{
	Kind kind;
	float speed;
	Thing[6] things;
	int count;
}

enum State
{
	PLAYING,
	DYING,
	GAME_OVER,
}

enum Death
{
	SQUASHED,
	DROWNED,
	EXPIRED,
}

struct Frog
{
	float x;
	int row;
	int facing;
	bool hopping;
	float hop_t;
	float from_x, to_x;
	int from_row, to_row;
}

struct Particle
{
	Vec pos, vel;
	float life, max_life;
	float r0, r1;
	Color color;
	bool ring;
}

Image screen;
Lane[ROWS] lanes;
bool[BAYS.len] filled;
Frog frog;
Particle[MAX_PARTICLES] particles;
int particle_count;
DefaultRandom rng;

State state;
Death death;
float death_t;
float banner_t;
float time_left;
float life_time;
float croc_cycle;
float now;
int best_row;
int lives;
int score;
int level;

fn float wrap(float x, float period) => x - math::floor(x / period) * period;

fn float rand_float(float lo, float hi) => lo + (hi - lo) * random::next_float(&rng);

// --- Setup

fn void add_lane(int row, Kind kind, float tiles_per_second, int len, int[] starts)
{
	Lane* lane = &lanes[row];
	lane.kind = kind;
	lane.speed = tiles_per_second * TILE_F * (0.85f + 0.15f * math::min(level, 8));
	lane.count = starts.len;
	foreach (int i, start : starts)
	{
		lane.things[i] = { .x = start * TILE_F, .len = len, .look = (row * 3 + i) % CAR_COLORS, .phase = rand_float(0, CROC_CYCLE) };
	}
}

<*
 A road lane, cycling through the given vehicles.
*>
fn void add_road(int row, float tiles_per_second, Vehicle[] vehicles, int[] starts)
{
	add_lane(row, CAR, tiles_per_second, 1, starts);
	foreach (int i, &t : lanes[row].things[:starts.len])
	{
		t.vehicle = vehicles[i % vehicles.len];
		t.len = t.vehicle.tiles;
	}
}

<*
 Later levels are faster, have less time, shorter logs, hungrier crocodiles, more diving turtles,
 denser traffic and new vehicles.
*>
fn void setup_level()
{
	lanes = {};
	life_time = math::max(LIFE_TIME - 3 * (level - 1f), 22f);
	croc_cycle = math::max(CROC_CYCLE - 0.3f * (level - 1f), 1.8f);
	add_lane(1, LOG, 0.95f, level < 3 ? 3 : 2, { 0, 5, 10 });
	add_lane(2, CROC, -1.1f, CROC_LEN, { 0, 7, 13 });
	add_lane(3, LOG, 1.6f, level < 5 ? 4 : 3, { 1, 10 });
	add_lane(4, TURTLE, -1.25f, level < 4 ? 3 : 2, { 0, 5, 10, 15 });
	foreach (int i, &t : lanes[4].things[:4]) t.dives = level >= 3 || i % 2;
	add_road(6, -1.25f, level < 3 ? (Vehicle[]) { CAR } : (Vehicle[]) { CAR, POLICE }, level < 5 ? (int[]) { 0, 4, 8, 12 } : (int[]) { 0, 3, 7, 10, 14 });
	add_road(7, 0.95f, level < 4 ? (Vehicle[]) { TRUCK } : (Vehicle[]) { BUS }, { 2, 9, 15 });
	add_road(8, -2.0f, level < 2 ? (Vehicle[]) { CAR } : (Vehicle[]) { SPORTS }, level < 4 ? (int[]) { 0, 5, 10 } : (int[]) { 0, 4, 9, 14 });
	add_road(9, 1.45f, level < 4 ? (Vehicle[]) { CAR } : (Vehicle[]) { CAR, BIKE }, level < 5 ? (int[]) { 1, 3, 9, 11 } : (int[]) { 1, 4, 7, 10, 13, 16 });
	filled = {};
	banner_t = BANNER_TIME;
}

fn void spawn()
{
	frog = { .x = 7 * TILE, .row = START_ROW };
	best_row = START_ROW;
	time_left = life_time;
	state = PLAYING;
}

fn void new_game()
{
	level = 1;
	score = 0;
	lives = 3;
	setup_level();
	spawn();
}

// --- Particles

fn void emit(Particle p)
{
	if (particle_count == MAX_PARTICLES) return;
	p.max_life = p.life;
	particles[particle_count++] = p;
}

fn void splash(Vec at)
{
	emit({ .pos = at, .life = 0.9f, .r0 = 4, .r1 = 30, .color = rgba(255, 255, 255, 200), .ring = true });
	emit({ .pos = at, .life = 0.6f, .r0 = 2, .r1 = 18, .color = rgba(200, 235, 255, 200), .ring = true });
	for (int i = 0; i < 16; i++)
	{
		float a = rand_float(0, 2 * PI);
		float v = rand_float(30, 90);
		emit({ .pos = at, .vel = { math::cos(a) * v, math::sin(a) * v }, .life = rand_float(0.4f, 0.8f), .r0 = rand_float(2, 3.5f), .r1 = 0.5f, .color = rgba(220, 240, 255, 230) });
	}
}

fn void burst(Vec at)
{
	for (int i = 0; i < 18; i++)
	{
		float a = rand_float(0, 2 * PI);
		float v = rand_float(20, 80);
		Color c = i % 3 ? rgba(110, 196, 70) : rgba(255, 240, 120);
		emit({ .pos = at, .vel = { math::cos(a) * v, math::sin(a) * v }, .life = rand_float(0.5f, 1), .r0 = rand_float(2, 3.5f), .r1 = 0.5f, .color = c });
	}
}

fn void update_particles(float dt)
{
	int alive = 0;
	for (int i = 0; i < particle_count; i++)
	{
		Particle* p = &particles[i];
		p.life -= dt;
		if (p.life <= 0) continue;
		p.pos += p.vel * dt;
		p.vel *= 1 - 2.5f * dt;
		particles[alive++] = *p;
	}
	particle_count = alive;
}

// --- Rules

fn bool is_river(int row) => row > HOME_ROW && row < MEDIAN_ROW;

fn bool is_road(int row) => row > MEDIAN_ROW && row < START_ROW;

fn Vec frog_centre() => { frog.x + TILE_F / 2, frog.row * TILE_F + TILE_F / 2 };

<*
 How far open the croc's jaws are, 0..1.
*>
fn float croc_jaw(Thing* t)
{
	float c = wrap(now + t.phase, croc_cycle);
	return c < CROC_BITE ? math::sin(c / CROC_BITE * PI) : 0;
}

<*
 How far a turtle group has dived: 0 is surfaced, 1 is fully under.
*>
fn float turtle_depth(Thing* t)
{
	if (!t.dives) return 0;
	float c = wrap(now + t.phase, DIVE_CYCLE);
	switch
	{
		case c < 3.2f: return 0;
		case c < 3.9f: return (c - 3.2f) / 0.7f;
		case c < 4.8f: return 1;
		default: return 1 - (c - 4.8f) / 0.7f;
	}
}

fn Thing*? thing_at(int row, float x)
{
	Lane* lane = &lanes[row];
	for (int i = 0; i < lane.count; i++)
	{
		Thing* t = &lane.things[i];
		if (x >= t.x && x < t.x + t.len * TILE_F) return t;
	}
	return NOT_FOUND~;
}

fn void kill(Death cause)
{
	state = DYING;
	death = cause;
	death_t = DEATH_TIME;
	frog.hopping = false;
	switch (cause)
	{
		case DROWNED: splash(frog_centre());
		case SQUASHED: burst(frog_centre());
		case EXPIRED: break;
	}
}

fn void update_lanes(float dt)
{
	foreach (int row, &lane : lanes)
	{
		for (int i = 0; i < lane.count; i++)
		{
			Thing* t = &lane.things[i];
			t.x += lane.speed * dt;
			t.roll += math::abs(lane.speed) * dt;
			float len = t.len * TILE_F;
			if (lane.speed > 0 && t.x >= SCREEN_W) t.x -= WRAP;
			if (lane.speed < 0 && t.x + len <= 0) t.x += WRAP;
			if (lane.kind != CAR || t.x + len < 0 || t.x > SCREEN_W) continue;
			t.smoke -= dt;
			if (t.smoke > 0) continue;
			t.smoke = rand_float(0.15f, 0.35f);
			float dir = lane.speed > 0 ? 1 : -1;
			float rear = dir > 0 ? t.x + 3 : t.x + len - 3;
			emit({ .pos = { rear, row * TILE_F + TILE_F / 2 + 7 }, .vel = { -dir * rand_float(10, 30), rand_float(-8, 8) },
			       .life = 0.9f, .r0 = 2.5f, .r1 = 9, .color = rgba(210, 210, 215, 60) });
		}
	}
}

fn void try_hop(int dx, int dy, int facing)
{
	frog.facing = facing;
	int row = frog.row + dy;
	float x = frog.x + dx * TILE_F;
	if (row < HOME_ROW || row >= HUD_ROW || x < 0 || x > SCREEN_W - TILE) return;
	frog.hopping = true;
	frog.hop_t = 0;
	frog.from_x = frog.x;
	frog.from_row = frog.row;
	frog.to_x = is_river(row) ? x : math::round(x / TILE) * TILE;
	frog.to_row = row;
}

fn void land()
{
	frog.hopping = false;
	frog.x = frog.to_x;
	frog.row = frog.to_row;
	if (frog.row < best_row)
	{
		best_row = frog.row;
		score += 10;
	}
	if (frog.row != HOME_ROW) return;
	foreach (i, bay : BAYS)
	{
		if (math::abs(frog.x - bay * TILE_F) > TILE / 2 || filled[i]) continue;
		filled[i] = true;
		score += 50 + (int)time_left * 2;
		foreach (f : filled) if (!f) { spawn(); return; }
		score += 1000;
		level++;
		setup_level();
		spawn();
		return;
	}
	kill(SQUASHED);
}

fn void update(float dt)
{
	now += dt;
	banner_t -= dt;
	update_lanes(dt);
	update_particles(dt);
	switch (state)
	{
		case GAME_OVER:
			if (gfx::is_key_pressed(SPACE)) new_game();
			return;
		case DYING:
			death_t -= dt;
			if (death_t > 0) return;
			if (--lives == 0)
			{
				state = GAME_OVER;
				return;
			}
			spawn();
			return;
		case PLAYING:
			break;
	}
	time_left -= dt;
	if (time_left <= 0)
	{
		kill(EXPIRED);
		return;
	}
	if (frog.hopping)
	{
		frog.hop_t += dt;
		if (frog.hop_t >= HOP_TIME) land();
		if (state != PLAYING || frog.hopping) return;
	}
	switch
	{
		case gfx::is_key_pressed(UP) || gfx::is_key_pressed(GfxKey.W): try_hop(0, -1, 0);
		case gfx::is_key_pressed(RIGHT) || gfx::is_key_pressed(D): try_hop(1, 0, 1);
		case gfx::is_key_pressed(DOWN) || gfx::is_key_pressed(S): try_hop(0, 1, 2);
		case gfx::is_key_pressed(LEFT) || gfx::is_key_pressed(A): try_hop(-1, 0, 3);
	}
	if (frog.hopping) return;
	Vec centre = frog_centre();
	Lane* lane = &lanes[frog.row];
	if (is_river(frog.row))
	{
		Thing*? t = thing_at(frog.row, centre.x);
		if (catch t)
		{
			kill(DROWNED);
			return;
		}
		if (lane.kind == TURTLE && turtle_depth(t) >= 1)
		{
			kill(DROWNED);
			return;
		}
		if (lane.kind == CROC && croc_jaw(t) > CROC_DEADLY)
		{
			float head = lane.speed < 0 ? t.x : t.x + t.len * TILE_F - TILE_F;
			if (centre.x >= head && centre.x < head + TILE)
			{
				kill(SQUASHED);
				return;
			}
		}
		frog.x += lane.speed * dt;
		if (centre.x < 0 || centre.x >= SCREEN_W) kill(DROWNED);
		return;
	}
	if (is_road(frog.row) && (@ok(thing_at(frog.row, frog.x + 9)) || @ok(thing_at(frog.row, frog.x + TILE - 9)))) kill(SQUASHED);
}

// --- Drawing

fn void draw_water()
{
	for (int row = HOME_ROW + 1; row < MEDIAN_ROW; row++)
	{
		float drift = now * lanes[row].speed * 0.35f;
		for (int i = 0; i < 10; i++)
		{
			float x = wrap(i * 79f + row * 37f + drift, SCREEN_W + 40) - 20;
			int jitter = (i * 17 + row * 11) % 32;
			float y = row * TILE_F + 8 + jitter;
			int len = 8 + (i * 7) % 10;
			float alpha = 0.22f + 0.18f * math::sin(now * 2.2f + i * 1.7f);
			capsule(x, y, x + len, y, 1.1f, rgba(255, 255, 255).fade(alpha));
		}
	}
}

<*
 The light bar alternates between red and blue.
*>
fn void draw_siren(Thing* t, int x, int y, bool right)
{
	bool red = wrap(now * 3 + t.phase, 1) < 0.5f;
	Color glow = red ? rgba(255, 60, 60) : rgba(70, 130, 255);
	float cx = x + TILE_F / 2 + (right ? -3f : 3f);
	float cy = y + TILE_F / 2 + (red ? -4.5f : 4.5f);
	set_origin(0, 0);
	circle(cx, cy, 13, glow.fade(0.3f));
	circle(cx, cy, 7, glow.fade(0.5f));
	circle(cx, cy, 3.2f, rgba(255, 255, 255).mix(glow, 0.3f));
}

<*
 A group of turtles, or the bubbles they leave while under.
*>
fn void draw_turtles(Thing* t, int x, int y, bool right)
{
	float depth = turtle_depth(t);
	for (int k = 0; k < t.len; k++)
	{
		int tx = x + k * TILE;
		float cx = tx + TILE_F / 2;
		float cy = y + TILE_F / 2;
		if (depth >= 1)
		{
			for (int b = 0; b < 3; b++)
			{
				float rise = wrap(now * 1.5f + b * 0.33f + k * 0.2f, 1);
				ring(cx - 8 + b * 8f, cy - 3 + (b % 2) * 6f, 1.5f + rise * 3, 1, rgba(255, 255, 255).fade(0.6f * (1 - rise)));
			}
			continue;
		}
		ring(cx, cy, 15 + 1.5f * math::sin(now * 3 + k), 1.2f, rgba(255, 255, 255, 50));
		int swim = (int)(wrap(now * 2 + t.phase + k * 0.3f, 1) * SWIM_STEPS) % SWIM_STEPS;
		blit(&sprites.turtle[(int)(depth * DEPTH_STEPS)][swim], tx, y, flip: right);
	}
}

fn void draw_lanes()
{
	foreach (int row, &lane : lanes)
	{
		bool right = lane.speed > 0;
		int y = row * TILE;
		for (int i = 0; i < lane.count; i++)
		{
			Thing* t = &lane.things[i];
			int x = (int)math::floor(t.x);
			float len = t.len * TILE_F;
			switch (lane.kind)
			{
				case CAR:
					int frame = (int)(t.roll / 2.4f) % WHEEL_FRAMES;
					blit(&sprites.vehicle[t.vehicle.ordinal][t.look][frame], x, y, flip: !right);
					if (t.vehicle == POLICE) draw_siren(t, x, y, right);
				case LOG:
					foreach (end : (float[2]) { t.x + 4, t.x + len - 4 })
					{
						ring(end, y + TILE / 2f, 6 + 2 * math::sin(now * 3 + t.phase + end * 0.01f), 1.4f, rgba(255, 255, 255, 80));
					}
					blit(&sprites.log[t.len - 2], x, y, flip: !right);
				case CROC:
					float tail = right ? t.x + 4 : t.x + len - 4;
					ring(tail, y + TILE / 2f, 7 + 2 * math::sin(now * 4 + t.phase), 1.4f, rgba(255, 255, 255, 70));
					int jaw = (int)math::round(croc_jaw(t) * (JAW_STEPS - 1));
					int sway = (int)(wrap(now * 1.4f + t.phase, 1) * SWAY_STEPS) % SWAY_STEPS;
					blit(&sprites.croc[jaw][sway], x, y, flip: right);
				case TURTLE:
					draw_turtles(t, x, y, right);
			}
		}
	}
}

fn void draw_homes()
{
	foreach (i, bay : BAYS)
	{
		int x = bay * TILE;
		blit(&sprites.lily[i % 2], x, 3);
		if (!filled[i]) continue;
		bool blink = wrap(now + i * 0.7f, 4) < 0.15f;
		blit(&sprites.frog[2][blink ? FROG_BLINK : 0], x, 2);
	}
}

fn void draw_frog()
{
	if (state == GAME_OVER) return;
	if (state == DYING)
	{
		if (death == DROWNED) return;
		Color tint = (int)(death_t * 10) % 2 ? rgba(230, 40, 40) : rgba(255, 255, 255);
		blit(&sprites.frog[frog.facing][0], (int)frog.x, frog.row * TILE, tint: tint);
		return;
	}
	float x = frog.x;
	float y = frog.row * TILE_F;
	float lift = 0;
	int frame = wrap(now, 3.3f) < 0.12f ? FROG_BLINK : 0;
	if (frog.hopping)
	{
		float p = frog.hop_t / HOP_TIME;
		x = frog.from_x + (frog.to_x - frog.from_x) * p;
		y = (frog.from_row * (1 - p) + frog.to_row * p) * TILE_F;
		lift = math::sin(p * PI) * HOP_HEIGHT;
		frame = p < 0.25f ? 1 : p < 0.75f ? 2 : 3;
	}
	set_origin(0, 0);
	ellipse(x + TILE / 2f, y + TILE / 2f + 4, 12 - lift * 0.4f, 9 - lift * 0.3f, rgba(0, 0, 0, 70));
	blit(&sprites.frog[frog.facing][frame], (int)x, (int)(y - lift));
}

fn void draw_particles()
{
	set_origin(0, 0);
	for (int i = 0; i < particle_count; i++)
	{
		Particle* p = &particles[i];
		float t = p.life / p.max_life;
		float r = p.r1 + (p.r0 - p.r1) * t;
		Color c = p.color.fade(t);
		if (p.ring)
		{
			ring(p.pos.x, p.pos.y, r, 2, c);
		}
		else
		{
			circle(p.pos.x, p.pos.y, r, c);
		}
	}
}

fn void draw_panel(float cy, String title, String subtitle, Color color, float fade = 1)
{
	set_origin(0, 0);
	float width = text_width(title, 6);
	box(SCREEN_W / 2f, cy, width / 2 + 30, subtitle ? 64 : 34, 16, rgba(10, 14, 24, 190).fade(fade));
	text(title, (SCREEN_W - width) / 2, subtitle ? cy - 46 : cy - 21, 6, color.fade(fade));
	if (subtitle) text(subtitle, (SCREEN_W - text_width(subtitle, 3)) / 2, cy + 18, 3, rgba(255, 255, 255));
}

fn void draw_hud()
{
	float y = HUD_ROW * TILE;
	text(string::tformat("SCORE %d", score), 16, y + 13, 3, rgba(255, 255, 255));
	text(string::tformat("LEVEL %d", level), 248, y + 13, 3, rgba(255, 220, 90));
	for (int i = 0; i < lives - 1; i++) blit(&sprites.life_icon, 404 + i * 30, HUD_ROW * TILE + 11);
	float t = math::max(time_left, 0f) / life_time;
	set_origin(0, 0);
	box(632, y + 24, 72, 9, 9, rgba(0, 0, 0, 160));
	if (t > 0) box(562 + 70 * t, y + 24, 70 * t, 6.5f, 6.5f, rgba(230, 50, 40).mix(rgba(110, 200, 60), math::min(t * 2, 1f)));
}

fn void draw()
{
	mem::copy(screen.px.ptr, sprites.background.px.ptr, screen.px.len * 4);
	set_target(&screen);
	draw_water();
	draw_homes();
	draw_lanes();
	draw_frog();
	draw_particles();
	draw_hud();
	switch
	{
		case state == GAME_OVER:
			draw_panel(4.5f * TILE, "GAME OVER", "PRESS SPACE", rgba(255, 210, 60));
		case banner_t > 0:
			draw_panel((MEDIAN_ROW + 0.5f) * TILE, string::tformat("LEVEL %d", level), "", rgba(140, 230, 100), math::min(banner_t / 0.4f, 1f));
	}
}

Image output;

<*
 Show the frame in a window of any size, scaled to fit and centred.
*>
fn void present(GfxWindow* win, GfxSurface* surface)
{
	GfxSize size = win.get_size();
	if (size == { SCREEN_W, SCREEN_H })
	{
		if (surface.data != screen.px.ptr)
		{
			surface.release();
			surface.init(screen.px.ptr, SCREEN_W, SCREEN_H, RGBA8, win)!!;
		}
		win.blit_surface(surface);
		return;
	}
	if (size != { output.w, output.h })
	{
		free(output.px);
		output = new_image(size.x, size.y);
		surface.release();
		surface.init(output.px.ptr, size.x, size.y, RGBA8, win)!!;
	}
	float scale = math::min(size.x / TILE_F / COLS, size.y / TILE_F / ROWS);
	int w = (int)(SCREEN_W * scale);
	int h = (int)(SCREEN_H * scale);
	int left = (size.x - w) / 2;
	int top = (size.y - h) / 2;
	output.px[..] = rgba(0, 0, 0);
	for (int y = 0; y < h; y++)
	{
		Color* src = &screen.px[y * SCREEN_H / h * SCREEN_W];
		Color* dst = &output.px[(top + y) * size.x + left];
		for (int x = 0; x < w; x++) dst[x] = src[x * SCREEN_W / w];
	}
	win.blit_surface(surface);
}

fn void main()
{
	random::seed_entropy(&rng);
	gfx::init("C3 Frogger", {})!!;
	GfxWindow* win = gfx::create_window("C3 Frogger", 100, 100, SCREEN_W, SCREEN_H)!!;
	paint_sprites();
	screen = new_image(SCREEN_W, SCREEN_H);
	GfxSurface surface;
	surface.init(screen.px.ptr, SCREEN_W, SCREEN_H, RGBA8, win)!!;

	new_game();
	Clock clock = clock::now();
	NanoDuration frame = time::ms(16).to_nano();
	while (!win.should_close())
	{
		while (try gfx::poll(win));
		if (gfx::is_key_pressed(ESCAPE) || gfx::is_key_pressed(Q)) break;
		if (gfx::is_key_pressed(F)) win.set_fullscreen(!win.is_fullscreen());
		update(math::min((float)clock.mark().to_sec(), 0.05f));
		@pool() { draw(); };
		present(win, &surface);
		NanoDuration spent = clock.to_now();
		if (spent < frame) thread::sleep_ns(frame - spent);
	}
	surface.release();
	free(output.px);
	win.destroy();
	gfx::finalize();
}
