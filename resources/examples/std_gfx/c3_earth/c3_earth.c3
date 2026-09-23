<*
 Build instructions:
 Native:
   c3c compile-run c3_earth.c3 -o build/c3_earth
 Emscripten:
   c3c compile c3_earth.c3 -o build/index.html --target emscripten -z "-sALLOW_MEMORY_GROWTH=1 -sMIN_WEBGL_VERSION=2 -sMAX_WEBGL_VERSION=2 -sSTACK_SIZE=2097152 --profiling-funcs --shell-file minshell.html"
*>
module c3_earth;
import std, opengl::gl;

alias EmCallback @feat(EMSCRIPTEN | WASM) = fn void();
extern fn void emscripten_set_main_loop(EmCallback func, int fps, int simulate_infinite_loop) @cname("emscripten_set_main_loop") @feat(EMSCRIPTEN | WASM);

const String WINDOW_TITLE    = "C3 Earth Explorer";
const int    WINDOW_WIDTH    = 1280;
const int    WINDOW_HEIGHT   = 768;

const char[*] EARTH_QOI_DATA   = $embed("earth.qoi");
const char[*] STARMAP_QOI_DATA = $embed("starmap.qoi");

const float DEG2RAD = (float)(math::PI / 180.0);
const float RAD2DEG = (float)(180.0 / math::PI);

const float EARTH_RADIUS            =   3.0f;
const float SKYBOX_RADIUS           = 100.0f;
const float CAMERA_SETTLE_THRESHOLD =   0.5f * DEG2RAD;
const float CAMERA_LERP_SPEED       =  10.0f;
const float CAMERA_ZOOM_SPEED       =  10.0f;
const float CAMERA_ANIM_DURATION    =   1.0f;
const float CAMERA_MIN_RADIUS       =   3.5f;
const float CAMERA_MAX_RADIUS       =  40.0f;
const float CAMERA_JUMP_RADIUS      =   5.0f;
const float CAMERA_INITIAL_RADIUS   =  12.0f;
const float CAMERA_INITIAL_PITCH    =  20.0f * DEG2RAD;
const float CAMERA_INITIAL_YAW      = (float)math::HALF_PI - (-30.0f * DEG2RAD);
const float CAMERA_PITCH_LIMIT      =   1.5f;
const float CAMERA_FOVY             =  45.0f;
const float CAMERA_NEAR_Z           =   0.1f;
const float CAMERA_FAR_Z            = 200.0f;
const float SUN_GEO_ADJUSTMENT      = -15.0f * DEG2RAD;
const float FADE_RADIUS_MIN         =   4.5f;
const float FADE_RADIUS_RANGE       =   2.5f;

const float PIN_BASE_RADIUS  = 0.01f;
const float PIN_HEAD_RADIUS  = 0.03f;
const float PIN_HEIGHT_SCALE = 1.03f;
const int   PIN_SIDES        = 8;

constdef Ui : inline int
{
	MARGIN      = 20,
	FONT_SIZE   = 20,
	LIST_OFFSET = 55,
	LINE_HEIGHT = 25
}

// ─────────────────────────────────────────────────────────────────────────────
// Application State
// ─────────────────────────────────────────────────────────────────────────────

struct AppState
{
	GfxWindow*   win;
	GlobeCamera  cam;
	Renderer     renderer;
	Texture      earth_tex;
	Texture      star_tex;
	Shader       shader;
	Mesh         earth_mesh;
	Mesh         skybox_mesh;
	int          sun_pos_loc;
	int          is_skybox_loc;
	int          sun_fade_loc;
	List{Pin}    pins;
	InputState   input;
	FrameTimer   timer;
	double       last_sun_update;
	Vec3         sun_pos_cache;
}

AppState state;

// ─────────────────────────────────────────────────────────────────────────────
// Entry Point & Main Loop
// ─────────────────────────────────────────────────────────────────────────────

fn void main() @winmain
{
	init();

	$if $feat(EMSCRIPTEN) || $feat(WASM):
		emscripten_set_main_loop(&step, 0, 1);
	$else
		while (!state.win.should_close())
		{
			step();
		}
		cleanup();
	$endif
}

fn void step() => @pool()
{
	state.input.begin_frame(state.win);
	state.timer.update();

	foreach (idx, city : City::values)
	{
		if (state.input.is_key_pressed((int)idx + 1))
		{
			state.cam.jump_to(city);
		}
	}

	float dt = math::min(state.timer.dt, 0.1f);
	GfxSize win_sz = state.win.get_size();
	float aspect   = (float)win_sz.x / (float)win_sz.y;
	Mat4 proj      = perspective(state.cam.fovy, aspect, CAMERA_NEAR_Z, CAMERA_FAR_Z);
	Mat4 view      = look_at(state.cam.position, { 0.0f, 0.0f, 0.0f }, state.cam.up);

	state.cam.update(&state.input, win_sz, proj, view, dt);
	view = look_at(state.cam.position, { 0.0f, 0.0f, 0.0f }, state.cam.up);

	float sun_fade = math::clamp((state.cam.current.radius - FADE_RADIUS_MIN) / FADE_RADIUS_RANGE, 0.0f, 1.0f);
	state.shader.set_float(state.sun_fade_loc, sun_fade);

	if (state.timer.elapsed() - state.last_sun_update > 1.0)
	{
		DateTime now        = datetime::from_time_tz(time::now(), 0).date_time;
		float hours_utc     = now.hour + now.min / 60.0f + now.sec / 3600.0f;
		float day_of_year   = now.year_day;

		float day_angle     = 6.24004077f + 0.01720197f * (day_of_year - 1.0f);
		float eq_time_mins  = -7.659f * math::sin(day_angle) + 9.863f * math::sin(2.0f * day_angle + 3.5932f);
		float eq_time_hours = eq_time_mins / 60.0f;
		float sun_yaw       = (float)math::HALF_PI + (hours_utc + eq_time_hours - 12.0f) * ((float)math::PI / 12.0f) + SUN_GEO_ADJUSTMENT;
		float orbit_angle   = 2.0f * (float)math::PI * (day_of_year - 81.0f) / 365.0f;
		float declination   = 23.45f * DEG2RAD * math::sin(orbit_angle);
		state.sun_pos_cache  = { math::cos(declination) * math::cos(sun_yaw), math::sin(declination), math::cos(declination) * math::sin(sun_yaw) };
		state.last_sun_update = state.timer.elapsed();
	}
	state.shader.set_vec3(state.sun_pos_loc, state.sun_pos_cache);

	Ray ray                = get_screen_to_world_ray(state.input.mouse_pos, state.cam.position, win_sz, proj, view);
	RayCollision collision = ray.get_collision_sphere({ 0.0f, 0.0f, 0.0f }, EARTH_RADIUS);

	gl::clearColor(0.0f, 0.0f, 0.0f, 1.0f);
	gl::clear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	gl::disable(GL_CULL_FACE);
	state.shader.set_int(state.is_skybox_loc, 1);
	draw_mesh(state.skybox_mesh, state.star_tex, state.shader, proj, view, { 0.0f, 0.0f, 0.0f }, 1.0f);
	gl::enable(GL_CULL_FACE);

	state.shader.set_int(state.is_skybox_loc, 0);
	draw_mesh(state.earth_mesh, state.earth_tex, state.shader, proj, view, { 0.0f, 0.0f, 0.0f }, 1.0f);

	foreach (p : state.pins)
	{
		Vec3 base = spherical_to_vec3((float)math::HALF_PI - (p.lon * DEG2RAD), p.lat * DEG2RAD, EARTH_RADIUS);
		Vec3 head = base * PIN_HEIGHT_SCALE;
		state.renderer.draw_cylinder(proj, view, base, head, PIN_BASE_RADIUS, PIN_BASE_RADIUS, PIN_SIDES, COLOR_WHITE);
		state.renderer.draw_sphere(proj, view, head, PIN_HEAD_RADIUS, COLOR_RED);
	}

	if (collision.hit && (state.cam.dragging || math::abs(state.cam.target.yaw - state.cam.current.yaw) < CAMERA_SETTLE_THRESHOLD))
	{
		Vec3 p    = collision.point.normalize();
		float lat = math::asin(p[1]) * RAD2DEG;
		float lon = math::atan2(p[0], p[2]) * RAD2DEG;
		state.renderer.draw_text(win_sz, string::tformat("Lat: %.3f Lon: %.3f", lat, lon), Ui.MARGIN, win_sz.y - (Ui.MARGIN + 10), Ui.FONT_SIZE, COLOR_SKYBLUE);
		if (state.input.is_mouse_pressed(RIGHT))
		{
			state.pins.push({ lat, lon });
		}
	}

	float ui_fade     = math::clamp((state.cam.current.radius - FADE_RADIUS_MIN) / FADE_RADIUS_RANGE, 0.0f, 1.0f);
	char ui_alpha     = (char)(ui_fade * 255.0f);
	Color ui_white    = { 255, 255, 255, ui_alpha };
	Color ui_raywhite = { 245, 245, 245, ui_alpha };

	state.renderer.draw_text(win_sz, "C3 Earth Explorer (Left Click: Rotate, Right Click: Pin, Wheel: Zoom, 1-9: Cities)", Ui.MARGIN, Ui.MARGIN, Ui.FONT_SIZE, ui_raywhite);
	$foreach $city : City::values:
		state.renderer.draw_text(win_sz, string::tformat("%d: %s", $city.ordinal + 1, $city.name), Ui.MARGIN, Ui.LIST_OFFSET + $city.ordinal * Ui.LINE_HEIGHT, Ui.FONT_SIZE, ui_white);
	$endforeach

	state.win.swap_opengl_buffers();
}

fn void init()
{
	GfxInitOption opts = { .init_opengl };
	gfx::init(WINDOW_TITLE, opts)!!;

	GfxWindowFlags wflags = { .center, .focus, .open_gl, .maximize };
	state.win = gfx::create_window(WINDOW_TITLE, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT, wflags)!!;
	$if !$feat(EMSCRIPTEN) && !$feat(WASM):
		state.win.set_exit_key(ESCAPE);
	$endif

	gfx::set_current_opengl_window(state.win);
	state.win.set_opengl_swap_interval(1);

	gl::init((gl::GLLoadFn)&gfx::get_opengl_proc);

	GfxSize initial_sz = state.win.get_size();
	gl::viewport(0, 0, initial_sz.x, initial_sz.y);
	gl::enable(GL_DEPTH_TEST);
	gl::enable(GL_CULL_FACE);
	gl::depthFunc(GL_LESS);
	gl::clearColor(0.0f, 0.0f, 0.0f, 1.0f);

	state.renderer.init();
	state.cam.init();

	state.earth_tex = load_texture_from_memory(&EARTH_QOI_DATA);
	assert(state.earth_tex.id != 0, "Failed to decode Earth texture");

	state.star_tex = load_texture_from_memory(&STARMAP_QOI_DATA);
	assert(state.star_tex.id != 0, "Failed to decode Star texture");

	state.shader = load_shader_from_memory(EARTH_VERTEX_SHADER, EARTH_FRAGMENT_SHADER);

	state.earth_mesh  = gen_mesh_sphere(EARTH_RADIUS, 64, 64);
	state.skybox_mesh = gen_mesh_sphere(SKYBOX_RADIUS, 64, 64);

	state.sun_pos_loc   = state.shader.get_location("sunPos");
	state.is_skybox_loc = state.shader.get_location("isSkybox");
	state.sun_fade_loc  = state.shader.get_location("sunFade");

	state.pins.init(mem);
	state.timer.init();
	state.last_sun_update = -1.0;
}

fn void cleanup()
{
	state.pins.free();
	state.earth_mesh.destroy();
	state.skybox_mesh.destroy();
	state.shader.destroy();
	state.star_tex.destroy();
	state.earth_tex.destroy();
	state.renderer.destroy();
	state.win.destroy();
	gfx::finalize();
	state = {};
}

// ─────────────────────────────────────────────────────────────────────────────
// Camera & Raycasting
// ─────────────────────────────────────────────────────────────────────────────

struct SphericalCoord
{
	float yaw;
	float pitch;
	float radius;
}

struct GlobeCamera
{
	SphericalCoord target;
	SphericalCoord current;
	SphericalCoord start;
	Vec3 position;
	Vec3 up;
	float fovy;
	bool dragging;
	float anim_timer;
	Vec2 vel;
}

fn void GlobeCamera.init(&self)
{
	SphericalCoord initial_pos = {
		.yaw    = CAMERA_INITIAL_YAW,
		.pitch  = CAMERA_INITIAL_PITCH,
		.radius = CAMERA_INITIAL_RADIUS
	};

	*self = {
		.target     = initial_pos,
		.current    = initial_pos,
		.up         = { 0.0f, 1.0f, 0.0f },
		.fovy       = CAMERA_FOVY,
		.anim_timer = CAMERA_ANIM_DURATION,
		.vel        = { 0.0f, 0.0f }
	};
	self.position = spherical_to_vec3(initial_pos.yaw, initial_pos.pitch, initial_pos.radius);
}

fn void GlobeCamera.update(&self, InputState* in, GfxSize win_sz, Mat4 proj, Mat4 view, float dt)
{
	if (in.mouse_wheel != 0.0f)
	{
		float wheel = math::copysign(1.0f, in.mouse_wheel);
		self.target.radius = math::clamp(self.target.radius - wheel * self.target.radius * 0.1f, CAMERA_MIN_RADIUS, CAMERA_MAX_RADIUS);
	}
	self.current.radius += (self.target.radius - self.current.radius) * CAMERA_ZOOM_SPEED * dt;

	if (in.is_mouse_pressed(LEFT))
	{
		Ray ray = get_screen_to_world_ray(in.mouse_pos, self.position, win_sz, proj, view);
		if (ray.get_collision_sphere({ 0.0f, 0.0f, 0.0f }, EARTH_RADIUS).hit)
		{
			self.dragging     = true;
			self.anim_timer   = CAMERA_ANIM_DURATION;
			self.target.yaw   = self.current.yaw;
			self.target.pitch = self.current.pitch;
		}
	}

	if (in.is_mouse_released(LEFT))
	{
		self.dragging = false;
	}

	switch
	{
		case self.dragging:
			Vec2 delta            = in.mouse_delta;
			float dist_to_surface = self.current.radius - EARTH_RADIUS;
			float viewport_height = 2.0f * dist_to_surface * math::tan(self.fovy * 0.5f * DEG2RAD);
			float angle_per_pixel = viewport_height / (float)win_sz.y / EARTH_RADIUS;

			self.vel = {
				delta.x * angle_per_pixel / math::max(0.1f, math::cos(self.current.pitch)),
				delta.y * angle_per_pixel
			};

			self.target.yaw   += self.vel.x;
			self.target.pitch  = math::clamp(self.target.pitch + self.vel.y, -CAMERA_PITCH_LIMIT, CAMERA_PITCH_LIMIT);
			self.current.yaw   = self.target.yaw;
			self.current.pitch = self.target.pitch;

		case self.anim_timer < CAMERA_ANIM_DURATION:
			self.anim_timer += dt;
			float t       = math::min(self.anim_timer / CAMERA_ANIM_DURATION, 1.0f);
			float eased_t = t * t * (3.0f - 2.0f * t);
			self.current.yaw    = self.start.yaw    + (self.target.yaw    - self.start.yaw)    * eased_t;
			self.current.pitch  = self.start.pitch  + (self.target.pitch  - self.start.pitch)  * eased_t;
			self.current.radius = self.start.radius + (self.target.radius - self.start.radius) * eased_t;
			self.vel = { 0.0f, 0.0f };

		default:
			self.target.yaw   += self.vel.x;
			self.target.pitch  = math::clamp(self.target.pitch + self.vel.y, -CAMERA_PITCH_LIMIT, CAMERA_PITCH_LIMIT);
			self.vel          *= math::pow(0.8f, dt * 60.0f);

			self.current.yaw   += (self.target.yaw   - self.current.yaw)   * CAMERA_LERP_SPEED * dt;
			self.current.pitch += (self.target.pitch - self.current.pitch) * CAMERA_LERP_SPEED * dt;
	}

	self.position = spherical_to_vec3(self.current.yaw, self.current.pitch, self.current.radius);
}

fn void GlobeCamera.jump_to(&self, City city)
{
	self.start = self.current;

	self.target.pitch  = city.lat * DEG2RAD;
	self.target.yaw    = (float)math::HALF_PI - (float)(city.lon * DEG2RAD);
	self.target.radius = CAMERA_JUMP_RADIUS;

	float diff = self.target.yaw - self.start.yaw;
	switch
	{
		case diff > (float)math::PI:
			self.start.yaw += (float)math::TAU;
		case diff < -(float)math::PI:
			self.start.yaw -= (float)math::TAU;
	}

	self.anim_timer = 0.0f;
}

fn Vec3 spherical_to_vec3(float yaw, float pitch, float radius)
{
	float cos_pitch = math::cos(pitch);
	return radius * (Vec3){ cos_pitch * math::cos(yaw), math::sin(pitch), cos_pitch * math::sin(yaw) };
}

struct Ray
{
	Vec3 position;
	Vec3 direction;
}

struct RayCollision
{
	bool hit;
	Vec3 point;
}

fn Ray get_screen_to_world_ray(Vec2 mouse_pos, Vec3 cam_pos, GfxSize win_sz, Mat4 proj, Mat4 view)
{
	float nx = (2.0f * mouse_pos.x) / (float)win_sz.x - 1.0f;
	float ny = 1.0f - (2.0f * mouse_pos.y) / (float)win_sz.y;

	float tan_half_fov = 1.0f / proj.m[1][1];
	float aspect       = proj.m[1][1] / proj.m[0][0];
	Vec3 ray_eye       = { nx * tan_half_fov * aspect, ny * tan_half_fov, -1.0f };

	Vec3 right   = { view.m[0][0], view.m[1][0], view.m[2][0] };
	Vec3 up      = { view.m[0][1], view.m[1][1], view.m[2][1] };
	Vec3 forward = { -view.m[0][2], -view.m[1][2], -view.m[2][2] };

	Vec3 dir = (ray_eye.x * right + ray_eye.y * up + forward).normalize();
	return { .position = cam_pos, .direction = dir };
}

fn RayCollision Ray.get_collision_sphere(&self, Vec3 center, float radius)
{
	Vec3 oc = self.position - center;
	float b = 2.0f * self.direction.dot(oc);
	float c = oc.dot(oc) - radius * radius;
	float disc = b * b - 4.0f * c;
	if (disc < 0.0f)
	{
		return {};
	}
	float t = (-b - math::sqrt(disc)) * 0.5f;
	if (t < 0.0f)
	{
		return {};
	}
	Vec3 pt = self.position + t * self.direction;
	return { .hit = true, .point = pt };
}

// ─────────────────────────────────────────────────────────────────────────────
// Graphics, Shaders & Rendering
// ─────────────────────────────────────────────────────────────────────────────

struct Mesh
{
	uint vao;
	uint vbo;
	uint ebo;
	int indices_count;
}

fn void Mesh.destroy(&self)
{
	if (self.vao != 0)
	{
		gl::deleteVertexArrays(1, &self.vao);
		gl::deleteBuffers(1, &self.vbo);
		gl::deleteBuffers(1, &self.ebo);
		self.vao = 0;
	}
}

fn Mesh gen_mesh_sphere(float radius, int rings, int slices) => @pool()
{
	List{float} verts;
	verts.init(tmem);
	List{uint} indices;
	indices.init(tmem);

	for (int r = 0; r <= rings; r++)
	{
		float phi = (float)r / (float)rings * (float)math::PI;
		for (int s = 0; s <= slices; s++)
		{
			float theta = (float)s / (float)slices * 2.0f * (float)math::PI;
			float x = math::sin(phi) * math::sin(theta);
			float y = math::cos(phi);
			float z = math::sin(phi) * math::cos(theta);
			verts.push(radius * x);
			verts.push(radius * y);
			verts.push(radius * z);
		}
	}

	for (int r = 0; r < rings; r++)
	{
		for (int s = 0; s < slices; s++)
		{
			uint a = (uint)(r * (slices + 1) + s);
			uint b = a + (uint)(slices + 1);
			indices.push(a);
			indices.push(b);
			indices.push(a + 1);
			indices.push(b);
			indices.push(b + 1);
			indices.push(a + 1);
		}
	}

	uint vao;
	uint vbo;
	uint ebo;
	gl::genVertexArrays(1, &vao);
	gl::genBuffers(1, &vbo);
	gl::genBuffers(1, &ebo);

	gl::bindVertexArray(vao);
	gl::bindBuffer(GL_ARRAY_BUFFER, vbo);
	gl::bufferData(GL_ARRAY_BUFFER, (usz)(verts.len() * float::size), verts.entries, GL_STATIC_DRAW);

	gl::bindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
	gl::bufferData(GL_ELEMENT_ARRAY_BUFFER, (usz)(indices.len() * uint::size), indices.entries, GL_STATIC_DRAW);

	gl::enableVertexAttribArray(0);
	gl::vertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, (int)(3 * float::size), null);
	gl::bindVertexArray(0);

	return { .vao = vao, .vbo = vbo, .ebo = ebo, .indices_count = (int)indices.len() };
}

struct Texture
{
	uint id;
}

fn void Texture.destroy(&self)
{
	if (self.id != 0)
	{
		gl::deleteTextures(1, &self.id);
		self.id = 0;
	}
}

fn Texture load_texture_from_memory(char[] bytes)
{
	qoi::QOIDesc desc;
	char[]? data = qoi::decode(mem, bytes, &desc, RGBA);
	if (catch data)
	{
		return {};
	}
	defer free(data);

	uint id;
	gl::genTextures(1, &id);
	gl::bindTexture(GL_TEXTURE_2D, id);
	gl::texImage2D(GL_TEXTURE_2D, 0, (GLint)GL_RGBA, desc.width, desc.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data.ptr);
	gl::texParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, (GLint)GL_LINEAR);
	gl::texParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, (GLint)GL_LINEAR);
	return { .id = id };
}

struct Shader
{
	uint id;
}

fn void Shader.destroy(&self)
{
	if (self.id != 0)
	{
		gl::deleteProgram(self.id);
		self.id = 0;
	}
}

fn int Shader.get_location(&self, ZString name) => gl::getUniformLocation(self.id, (GLchar*)name);

fn void Shader.set_float(&self, int loc, float val)
{
	if (loc < 0)
	{
		return;
	}
	gl::useProgram(self.id);
	gl::uniform1f(loc, val);
}

fn void Shader.set_vec3(&self, int loc, Vec3 val)
{
	if (loc < 0)
	{
		return;
	}
	gl::useProgram(self.id);
	gl::uniform3fv(loc, 1, (float*)&val);
}

fn void Shader.set_int(&self, int loc, int val)
{
	if (loc < 0)
	{
		return;
	}
	gl::useProgram(self.id);
	gl::uniform1i(loc, val);
}

fn Shader load_shader_from_memory(ZString vs_src, ZString fs_src)
{
	uint vs = gl::createShader(GL_VERTEX_SHADER);
	gl::shaderSource(vs, 1, (GLchar**)&vs_src, null);
	gl::compileShader(vs);

	uint fs = gl::createShader(GL_FRAGMENT_SHADER);
	gl::shaderSource(fs, 1, (GLchar**)&fs_src, null);
	gl::compileShader(fs);

	uint prog = gl::createProgram();
	gl::attachShader(prog, vs);
	gl::attachShader(prog, fs);
	gl::linkProgram(prog);

	gl::deleteShader(vs);
	gl::deleteShader(fs);

	return { .id = prog };
}

fn void draw_mesh(Mesh mesh, Texture texture, Shader s, Mat4 proj, Mat4 view, Vec3 position, float scale)
{
	gl::useProgram(s.id);

	Mat4 mat_model = mat4_translate_scale(position, scale);
	Mat4 mvp       = mat4_mul(mat4_mul(proj, view), mat_model);

	int loc_mvp   = gl::getUniformLocation(s.id, "mvp");
	int loc_model = gl::getUniformLocation(s.id, "matModel");
	if (loc_mvp >= 0)
	{
		gl::uniformMatrix4fv(loc_mvp, 1, GL_FALSE, (float*)&mvp);
	}
	if (loc_model >= 0)
	{
		gl::uniformMatrix4fv(loc_model, 1, GL_FALSE, (float*)&mat_model);
	}

	gl::activeTexture(GL_TEXTURE0);
	gl::bindTexture(GL_TEXTURE_2D, texture.id);

	gl::bindVertexArray(mesh.vao);
	gl::drawElements(GL_TRIANGLES, mesh.indices_count, GL_UNSIGNED_INT, null);
	gl::bindVertexArray(0);
}

struct Renderer
{
	uint color_shader;
	int loc_color_mvp;
	int loc_color_col;

	Mesh unit_sphere;
	uint dyn_vao;
	uint dyn_vbo;

	uint font_tex;
	uint text_shader;
	int loc_text_col;
	uint text_vao;
	uint text_vbo;
}

fn void Renderer.init(&self)
{
	self.color_shader  = load_shader_from_memory(COLOR_VERTEX_SHADER, COLOR_FRAGMENT_SHADER).id;
	self.loc_color_mvp = gl::getUniformLocation(self.color_shader, "mvp");
	self.loc_color_col = gl::getUniformLocation(self.color_shader, "color");

	self.unit_sphere = gen_mesh_sphere(1.0f, 16, 16);

	gl::genVertexArrays(1, &self.dyn_vao);
	gl::genBuffers(1, &self.dyn_vbo);
	gl::bindVertexArray(self.dyn_vao);
	gl::bindBuffer(GL_ARRAY_BUFFER, self.dyn_vbo);
	gl::enableVertexAttribArray(0);
	gl::vertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, (int)(3 * float::size), null);
	gl::bindVertexArray(0);

	char[] atlas = mem::new_array(char, 128 * 128 * 4);
	defer free(atlas);
	for (int i = 32; i < 127; i++)
	{
		FontGlyph? g = FontGlyph::lookup_field(ch, (char)i);
		if (catch g)
		{
			continue;
		}
		ulong bits = g.bitmap;
		int gx = (i % 16) * 8;
		int gy = (i / 16) * 8;
		for (int r = 0; r < 8; r++)
		{
			for (int c = 0; c < 5; c++)
			{
				if (((bits >> (39 - (r * 5 + c))) & 1) != 0)
				{
					int p = ((gy + r) * 128 + (gx + c)) * 4;
					atlas[p..p + 3] = 255;
				}
			}
		}
	}

	gl::genTextures(1, &self.font_tex);
	gl::bindTexture(GL_TEXTURE_2D, self.font_tex);
	gl::texImage2D(GL_TEXTURE_2D, 0, (GLint)GL_RGBA, 128, 128, 0, GL_RGBA, GL_UNSIGNED_BYTE, atlas.ptr);
	gl::texParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, (GLint)GL_NEAREST);
	gl::texParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, (GLint)GL_NEAREST);

	self.text_shader  = load_shader_from_memory(TEXT_VERTEX_SHADER, TEXT_FRAGMENT_SHADER).id;
	self.loc_text_col = gl::getUniformLocation(self.text_shader, "color");

	gl::genVertexArrays(1, &self.text_vao);
	gl::genBuffers(1, &self.text_vbo);
	gl::bindVertexArray(self.text_vao);
	gl::bindBuffer(GL_ARRAY_BUFFER, self.text_vbo);
	gl::enableVertexAttribArray(0);
	gl::vertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, (int)(4 * float::size), null);
	gl::enableVertexAttribArray(1);
	gl::vertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, (int)(4 * float::size), (void*)(usz)(2 * float::size));
	gl::bindVertexArray(0);
}

fn void Renderer.destroy(&self)
{
	if (self.color_shader != 0)
	{
		gl::deleteProgram(self.color_shader);
	}
	if (self.text_shader != 0)
	{
		gl::deleteProgram(self.text_shader);
	}
	if (self.font_tex != 0)
	{
		gl::deleteTextures(1, &self.font_tex);
	}
	self.unit_sphere.destroy();
	if (self.dyn_vao != 0)
	{
		gl::deleteVertexArrays(1, &self.dyn_vao);
		gl::deleteBuffers(1, &self.dyn_vbo);
	}
	if (self.text_vao != 0)
	{
		gl::deleteVertexArrays(1, &self.text_vao);
		gl::deleteBuffers(1, &self.text_vbo);
	}
}

fn void Renderer.draw_cylinder(&self, Mat4 proj, Mat4 view, Vec3 start, Vec3 end, float r1, float r2, int sides, Color c)
{
	if (sides < 3)
	{
		sides = 3;
	}
	Vec3 axis = end - start;
	float len = axis.length();
	if (len < 0.0001f)
	{
		return;
	}
	Vec3 dir = axis / len;

	Vec3 up = math::abs(dir.y) < 0.99f ? (Vec3){ 0.0f, 1.0f, 0.0f } : (Vec3){ 1.0f, 0.0f, 0.0f };
	Vec3 u  = dir.cross(up).normalize();
	Vec3 v  = dir.cross(u).normalize();

	List{float} verts;
	verts.init(tmem);

	for (int i = 0; i < sides; i++)
	{
		float a1 = (float)i / (float)sides * 2.0f * (float)math::PI;
		float a2 = (float)(i + 1) / (float)sides * 2.0f * (float)math::PI;

		Vec3 rad1 = math::cos(a1) * u + math::sin(a1) * v;
		Vec3 rad2 = math::cos(a2) * u + math::sin(a2) * v;

		Vec3 b1 = start + r1 * rad1;
		Vec3 b2 = start + r1 * rad2;
		Vec3 t1 = end   + r2 * rad1;
		Vec3 t2 = end   + r2 * rad2;

		verts.push(b1.x); verts.push(b1.y); verts.push(b1.z);
		verts.push(b2.x); verts.push(b2.y); verts.push(b2.z);
		verts.push(t1.x); verts.push(t1.y); verts.push(t1.z);

		verts.push(b2.x); verts.push(b2.y); verts.push(b2.z);
		verts.push(t2.x); verts.push(t2.y); verts.push(t2.z);
		verts.push(t1.x); verts.push(t1.y); verts.push(t1.z);
	}

	gl::useProgram(self.color_shader);
	Mat4 mvp = mat4_mul(proj, view);
	gl::uniformMatrix4fv(self.loc_color_mvp, 1, GL_FALSE, (float*)&mvp);

	Vec4 col = { (float)c.r / 255.0f, (float)c.g / 255.0f, (float)c.b / 255.0f, (float)c.a / 255.0f };
	gl::uniform4fv(self.loc_color_col, 1, (float*)&col);

	gl::bindVertexArray(self.dyn_vao);
	gl::bindBuffer(GL_ARRAY_BUFFER, self.dyn_vbo);
	gl::bufferData(GL_ARRAY_BUFFER, (usz)(verts.len() * float::size), verts.entries, GL_DYNAMIC_DRAW);
	gl::drawArrays(GL_TRIANGLES, 0, sides * 6);
	gl::bindVertexArray(0);
}

fn void Renderer.draw_sphere(&self, Mat4 proj, Mat4 view, Vec3 pos, float r, Color c)
{
	gl::useProgram(self.color_shader);
	Mat4 mat_model = mat4_translate_scale(pos, r);
	Mat4 mvp       = mat4_mul(mat4_mul(proj, view), mat_model);
	gl::uniformMatrix4fv(self.loc_color_mvp, 1, GL_FALSE, (float*)&mvp);

	Vec4 col = { (float)c.r / 255.0f, (float)c.g / 255.0f, (float)c.b / 255.0f, (float)c.a / 255.0f };
	gl::uniform4fv(self.loc_color_col, 1, (float*)&col);

	gl::bindVertexArray(self.unit_sphere.vao);
	gl::drawElements(GL_TRIANGLES, self.unit_sphere.indices_count, GL_UNSIGNED_INT, null);
	gl::bindVertexArray(0);
}

fn void Renderer.draw_text(&self, GfxSize win_sz, String text, int x, int y, int size, Color color)
{
	if (text.len == 0 || win_sz.x <= 0 || win_sz.y <= 0)
	{
		return;
	}

	float scale   = (float)size / 8.0f;
	float advance = 6.0f * scale;
	float cur_x   = (float)x;
	float cur_y   = (float)y;

	List{float} verts;
	verts.init(tmem);

	foreach (char c : text)
	{
		if (c == '\n')
		{
			cur_x = (float)x;
			cur_y += 10.0f * scale;
			continue;
		}
		if (c < 32 || c > 126)
		{
			cur_x += advance;
			continue;
		}

		int idx = (int)c;
		int gx  = (idx % 16) * 8;
		int gy  = (idx / 16) * 8;

		float u0 = (float)gx / 128.0f;
		float v0 = (float)gy / 128.0f;
		float u1 = (float)(gx + 5) / 128.0f;
		float v1 = (float)(gy + 8) / 128.0f;

		float x0 = (cur_x / (float)win_sz.x) * 2.0f - 1.0f;
		float y0 = 1.0f - (cur_y / (float)win_sz.y) * 2.0f;
		float x1 = ((cur_x + 5.0f * scale) / (float)win_sz.x) * 2.0f - 1.0f;
		float y1 = 1.0f - ((cur_y + 8.0f * scale) / (float)win_sz.y) * 2.0f;

		verts.push(x0); verts.push(y0); verts.push(u0); verts.push(v0);
		verts.push(x0); verts.push(y1); verts.push(u0); verts.push(v1);
		verts.push(x1); verts.push(y0); verts.push(u1); verts.push(v0);

		verts.push(x1); verts.push(y0); verts.push(u1); verts.push(v0);
		verts.push(x0); verts.push(y1); verts.push(u0); verts.push(v1);
		verts.push(x1); verts.push(y1); verts.push(u1); verts.push(v1);

		cur_x += advance;
	}

	if (verts.len() == 0)
	{
		return;
	}

	gl::disable(GL_DEPTH_TEST);
	gl::disable(GL_CULL_FACE);
	gl::enable(GL_BLEND);
	gl::blendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

	gl::useProgram(self.text_shader);
	Vec4 col = { (float)color.r / 255.0f, (float)color.g / 255.0f, (float)color.b / 255.0f, (float)color.a / 255.0f };
	gl::uniform4fv(self.loc_text_col, 1, (float*)&col);

	gl::activeTexture(GL_TEXTURE0);
	gl::bindTexture(GL_TEXTURE_2D, self.font_tex);

	gl::bindVertexArray(self.text_vao);
	gl::bindBuffer(GL_ARRAY_BUFFER, self.text_vbo);
	gl::bufferData(GL_ARRAY_BUFFER, (usz)(verts.len() * float::size), verts.entries, GL_DYNAMIC_DRAW);
	gl::drawArrays(GL_TRIANGLES, 0, (int)(verts.len() / 4));
	gl::bindVertexArray(0);

	gl::enable(GL_DEPTH_TEST);
	gl::enable(GL_CULL_FACE);
}

// ─────────────────────────────────────────────────────────────────────────────
// Input & Window Helpers
// ─────────────────────────────────────────────────────────────────────────────

struct InputState
{
	Vec2 mouse_pos;
	Vec2 mouse_delta;
	float mouse_wheel;
	bool[3] mouse_pressed;
	bool[3] mouse_released;
	bool[10] keys_pressed;
	GfxPoint last_mouse;
	bool has_last_mouse;
}

fn void InputState.begin_frame(&self, GfxWindow* w)
{
	self.mouse_delta    = { 0.0f, 0.0f };
	self.mouse_wheel    = 0.0f;
	self.mouse_pressed  = {};
	self.mouse_released = {};
	self.keys_pressed   = {};

	while (try event = gfx::poll(w))
	{
		switch (event.type)
		{
			case WINDOW_RESIZED:
				GfxSize win_sz = w.get_size();
				gl::viewport(0, 0, win_sz.x, win_sz.y);
			case MOUSE_BUTTON_PRESSED:
				int b = event.button.value == LEFT ? 0 : (event.button.value == RIGHT ? 1 : 2);
				self.mouse_pressed[b] = true;
			case MOUSE_BUTTON_RELEASED:
				int b = event.button.value == LEFT ? 0 : (event.button.value == RIGHT ? 1 : 2);
				self.mouse_released[b] = true;
			case MOUSE_SCROLL:
				self.mouse_wheel += event.delta.vector.y;
			case KEY_PRESSED:
				GfxKey k = event.key.value;
				if (k >= NUM1 && k <= NUM9)
				{
					self.keys_pressed[(int)(k - NUM1) + 1] = true;
				}
				if (k == NUM0)
				{
					self.keys_pressed[0] = true;
				}
			default:
				break;
		}
	}

	GfxPoint cur = w.get_mouse();
	self.mouse_pos = { (float)cur.x, (float)cur.y };
	if (self.has_last_mouse)
	{
		self.mouse_delta = { (float)(cur.x - self.last_mouse.x), (float)(cur.y - self.last_mouse.y) };
	}
	else
	{
		self.has_last_mouse = true;
	}
	self.last_mouse = cur;
}

fn bool InputState.is_mouse_pressed(&self, MouseButton b) => self.mouse_pressed[(int)b];
fn bool InputState.is_mouse_released(&self, MouseButton b) => self.mouse_released[(int)b];
fn bool InputState.is_key_pressed(&self, int k) => (k >= 0 && k < 10) && self.keys_pressed[k];

struct FrameTimer
{
	Time last_time;
	double start_time;
	float dt;
}

fn void FrameTimer.init(&self)
{
	self.last_time  = time::now();
	self.start_time = (double)self.last_time / (double)time::SEC;
}

fn void FrameTimer.update(&self)
{
	Time now = time::now();
	self.dt = (float)((double)(now - self.last_time) / (double)time::SEC);
	self.last_time = now;
}

fn double FrameTimer.elapsed(&self) => (double)time::now() / (double)time::SEC - self.start_time;

// ─────────────────────────────────────────────────────────────────────────────
// Matrix Math
// ─────────────────────────────────────────────────────────────────────────────

struct Mat4
{
	float[4][4] m;
}

fn Mat4 perspective(float fovy_deg, float aspect, float near_z, float far_z)
{
	float f  = 1.0f / math::tan(fovy_deg * DEG2RAD * 0.5f);
	float nf = 1.0f / (near_z - far_z);
	Mat4 m = {};
	m.m[0][0] =  f / aspect;
	m.m[1][1] =  f;
	m.m[2][2] =  (far_z + near_z) * nf;
	m.m[2][3] = -1.0f;
	m.m[3][2] =  2.0f * far_z * near_z * nf;
	return m;
}

fn Mat4 look_at(Vec3 eye, Vec3 center, Vec3 up)
{
	Vec3 f = (center - eye).normalize();
	Vec3 r = f.cross(up).normalize();
	Vec3 u = r.cross(f);

	Mat4 m = {};
	m.m[0][0] =  r.x;
	m.m[1][0] =  r.y;
	m.m[2][0] =  r.z;
	m.m[3][0] = -r.dot(eye);
	m.m[0][1] =  u.x;
	m.m[1][1] =  u.y;
	m.m[2][1] =  u.z;
	m.m[3][1] = -u.dot(eye);
	m.m[0][2] = -f.x;
	m.m[1][2] = -f.y;
	m.m[2][2] = -f.z;
	m.m[3][2] =  f.dot(eye);
	m.m[3][3] =  1.0f;
	return m;
}

fn Mat4 mat4_translate_scale(Vec3 pos, float scale)
{
	Mat4 m = {};
	m.m[0][0] = scale;
	m.m[1][1] = scale;
	m.m[2][2] = scale;
	m.m[3][0] = pos.x;
	m.m[3][1] = pos.y;
	m.m[3][2] = pos.z;
	m.m[3][3] = 1.0f;
	return m;
}

fn Mat4 mat4_mul(Mat4 a, Mat4 b)
{
	Mat4 r = {};
	for (int col = 0; col < 4; col++)
	{
		for (int row = 0; row < 4; row++)
		{
			for (int k = 0; k < 4; k++)
			{
				r.m[col][row] += a.m[k][row] * b.m[col][k];
			}
		}
	}
	return r;
}

// ─────────────────────────────────────────────────────────────────────────────
// Types & Constants
// ─────────────────────────────────────────────────────────────────────────────

enum City : int (float lat, float lon, String name)
{
	SALA         {  59.9f,  16.5f, "Sala, Sweden" },
	BUENOS_AIRES { -34.6f, -58.4f, "Buenos Aires" },
	TOKYO        {  35.7f, 139.7f, "Tokyo" },
	DELHI        {  28.6f,  77.2f, "Delhi" },
	SHANGHAI     {  31.2f, 121.5f, "Shanghai" },
	SAO_PAULO    { -23.5f, -46.6f, "Sao Paulo" },
	MEXICO       {  19.4f, -99.1f, "Mexico City" },
	CAIRO        {  30.0f,  31.2f, "Cairo" },
	MUMBAI       {  19.1f,  72.9f, "Mumbai" },
}

enum MouseButton : int
{
	LEFT,
	RIGHT,
	MIDDLE
}

struct Color
{
	char r;
	char g;
	char b;
	char a;
}

const Color COLOR_WHITE   = { 255, 255, 255, 255 };
const Color COLOR_RED     = { 230, 41, 55, 255 };
const Color COLOR_SKYBLUE = { 102, 191, 255, 255 };

struct Pin
{
	float lat;
	float lon;
}

// ─────────────────────────────────────────────────────────────────────────────
// Shaders
// ─────────────────────────────────────────────────────────────────────────────

const String SHADER_PREFIX @feat(EMSCRIPTEN)  = "#version 300 es\nprecision highp float;\n";
const String SHADER_PREFIX @feat(!EMSCRIPTEN) = "#version 330 core\n";

const ZString EARTH_VERTEX_SHADER = SHADER_PREFIX +++ `layout(location = 0) in vec3 vertexPosition;
uniform mat4 mvp;
uniform mat4 matModel;
out vec3 fragPos;
void main() {
    fragPos = (matModel * vec4(vertexPosition, 1.0)).xyz;
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}
`;

const ZString EARTH_FRAGMENT_SHADER = SHADER_PREFIX +++ `in vec3 fragPos;
uniform sampler2D texture0;
uniform vec3 sunPos;
uniform int isSkybox;
uniform float sunFade;
out vec4 finalColor;
const float PI = 3.14159265359;
void main() {
    vec3 p = normalize(fragPos);
    vec2 uv = vec2(0.5 + atan(p.x, p.z) / (2.0 * PI), 0.5 - asin(p.y) / PI);
    vec4 tex = texture(texture0, uv);
    if (isSkybox != 0) {
        float angle = dot(p, sunPos);
        float sunEffect = smoothstep(0.9995, 1.0, angle);
        vec3 sunColor = vec3(1.0, 0.96, 0.7) * sunEffect * 1.5 * sunFade;
        finalColor = vec4(tex.rgb + sunColor, 1.0);
        return;
    }
    float diff = max(dot(p, sunPos), 0.05);
    float finalDiff = mix(1.0, diff, sunFade);
    float dither = (fract(sin(dot(gl_FragCoord.xy, vec2(12.9898, 78.233))) * 43758.5453) - 0.5) / 255.0;
    finalColor = vec4(tex.rgb * finalDiff + dither, 1.0);
}
`;

const ZString COLOR_VERTEX_SHADER = SHADER_PREFIX +++ `layout(location = 0) in vec3 vertexPosition;
uniform mat4 mvp;
void main() {
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}
`;

const ZString COLOR_FRAGMENT_SHADER = SHADER_PREFIX +++ `uniform vec4 color;
out vec4 finalColor;
void main() {
    finalColor = color;
}
`;

const ZString TEXT_VERTEX_SHADER = SHADER_PREFIX +++ `layout(location = 0) in vec2 vertexPosition;
layout(location = 1) in vec2 vertexTexCoord;
out vec2 fragTexCoord;
void main() {
    fragTexCoord = vertexTexCoord;
    gl_Position = vec4(vertexPosition, 0.0, 1.0);
}
`;

const ZString TEXT_FRAGMENT_SHADER = SHADER_PREFIX +++ `in vec2 fragTexCoord;
uniform sampler2D fontTex;
uniform vec4 color;
out vec4 finalColor;
void main() {
    float a = texture(fontTex, fragTexCoord).a;
    if (a < 0.1) discard;
    finalColor = vec4(color.rgb, color.a * a);
}
`;

// ─────────────────────────────────────────────────────────────────────────────
// Font Bitmap
// ─────────────────────────────────────────────────────────────────────────────

enum FontGlyph : (char ch, ulong bitmap)
{
	A { 'A', 0b01110_10001_10001_11111_10001_10001_10001_00000UL },
	B { 'B', 0b11110_10001_10001_11110_10001_10001_11110_00000UL },
	C { 'C', 0b01111_10000_10000_10000_10000_10000_01111_00000UL },
	D { 'D', 0b11110_10001_10001_10001_10001_10001_11110_00000UL },
	E { 'E', 0b11111_10000_10000_11110_10000_10000_11111_00000UL },
	F { 'F', 0b11111_10000_10000_11110_10000_10000_10000_00000UL },
	G { 'G', 0b01111_10000_10000_10111_10001_10001_01111_00000UL },
	H { 'H', 0b10001_10001_10001_11111_10001_10001_10001_00000UL },
	I { 'I', 0b01110_00100_00100_00100_00100_00100_01110_00000UL },
	J { 'J', 0b00111_00010_00010_00010_00010_10010_01100_00000UL },
	K { 'K', 0b10001_10010_10100_11000_10100_10010_10001_00000UL },
	L { 'L', 0b10000_10000_10000_10000_10000_10000_11111_00000UL },
	M { 'M', 0b10001_11011_10101_10101_10001_10001_10001_00000UL },
	N { 'N', 0b10001_11001_10101_10011_10001_10001_10001_00000UL },
	O { 'O', 0b01110_10001_10001_10001_10001_10001_01110_00000UL },
	P { 'P', 0b11110_10001_10001_11110_10000_10000_10000_00000UL },
	Q { 'Q', 0b01110_10001_10001_10001_10101_10010_01101_00000UL },
	R { 'R', 0b11110_10001_10001_11110_10100_10010_10001_00000UL },
	S { 'S', 0b01111_10000_10000_01110_00001_00001_11110_00000UL },
	T { 'T', 0b11111_00100_00100_00100_00100_00100_00100_00000UL },
	U { 'U', 0b10001_10001_10001_10001_10001_10001_01110_00000UL },
	V { 'V', 0b10001_10001_10001_10001_10001_01010_00100_00000UL },
	W { 'W', 0b10001_10001_10001_10101_10101_11011_10001_00000UL },
	X { 'X', 0b10001_10001_01010_00100_01010_10001_10001_00000UL },
	Y { 'Y', 0b10001_10001_01010_00100_00100_00100_00100_00000UL },
	Z { 'Z', 0b11111_00001_00010_00100_01000_10000_11111_00000UL },

	LOWER_A { 'a', 0b00000_00000_01111_00001_01111_10001_01111_00000UL },
	LOWER_B { 'b', 0b10000_10000_11110_10001_10001_10001_11110_00000UL },
	LOWER_C { 'c', 0b00000_00000_01111_10000_10000_10000_01111_00000UL },
	LOWER_D { 'd', 0b00001_00001_01111_10001_10001_10001_01111_00000UL },
	LOWER_E { 'e', 0b00000_00000_01111_10001_11111_10000_01111_00000UL },
	LOWER_F { 'f', 0b00110_01001_01000_11110_01000_01000_01000_00000UL },
	LOWER_G { 'g', 0b00000_00000_01111_10001_10001_01111_00001_11110UL },
	LOWER_H { 'h', 0b10000_10000_11110_10001_10001_10001_10001_00000UL },
	LOWER_I { 'i', 0b00100_00000_01100_00100_00100_00100_01110_00000UL },
	LOWER_J { 'j', 0b00010_00000_00010_00010_00010_00010_10010_01100UL },
	LOWER_K { 'k', 0b10000_10000_10010_10100_11000_10100_10010_00000UL },
	LOWER_L { 'l', 0b01100_00100_00100_00100_00100_00100_01110_00000UL },
	LOWER_M { 'm', 0b00000_00000_11010_10101_10101_10101_10101_00000UL },
	LOWER_N { 'n', 0b00000_00000_11110_10001_10001_10001_10001_00000UL },
	LOWER_O { 'o', 0b00000_00000_01110_10001_10001_10001_01110_00000UL },
	LOWER_P { 'p', 0b00000_00000_11110_10001_10001_11110_10000_10000UL },
	LOWER_Q { 'q', 0b00000_00000_01111_10001_10001_01111_00001_00001UL },
	LOWER_R { 'r', 0b00000_00000_10110_11001_10000_10000_10000_00000UL },
	LOWER_S { 's', 0b00000_00000_01111_10000_01110_00001_11110_00000UL },
	LOWER_T { 't', 0b00100_00100_11111_00100_00100_00100_00011_00000UL },
	LOWER_U { 'u', 0b00000_00000_10001_10001_10001_10011_01101_00000UL },
	LOWER_V { 'v', 0b00000_00000_10001_10001_10001_01010_00100_00000UL },
	LOWER_W { 'w', 0b00000_00000_10001_10001_10101_10101_01010_00000UL },
	LOWER_X { 'x', 0b00000_00000_10001_01010_00100_01010_10001_00000UL },
	LOWER_Y { 'y', 0b00000_00000_10001_10001_10001_01111_00001_11110UL },
	LOWER_Z { 'z', 0b00000_00000_11111_00010_00100_01000_11111_00000UL },

	ZERO        { '0', 0b01110_10001_10011_10101_11001_10001_01110_00000UL },
	ONE         { '1', 0b00100_01100_00100_00100_00100_00100_01110_00000UL },
	TWO         { '2', 0b01110_10001_00001_00010_00100_01000_11111_00000UL },
	THREE       { '3', 0b11111_00010_00100_00010_00001_10001_01110_00000UL },
	FOUR        { '4', 0b00010_00110_01010_10010_11111_00010_00010_00000UL },
	FIVE        { '5', 0b11111_10000_11110_00001_00001_10001_01110_00000UL },
	SIX         { '6', 0b00110_01000_10000_11110_10001_10001_01110_00000UL },
	SEVEN       { '7', 0b11111_00001_00010_00100_01000_01000_01000_00000UL },
	EIGHT       { '8', 0b01110_10001_10001_01110_10001_10001_01110_00000UL },
	NINE        { '9', 0b01110_10001_10001_01111_00001_00010_01100_00000UL },
	SPACE       { ' ', 0b00000_00000_00000_00000_00000_00000_00000_00000UL },
	DOT         { '.', 0b00000_00000_00000_00000_00000_00110_00110_00000UL },
	COMMA       { ',', 0b00000_00000_00000_00000_00000_00110_00100_01000UL },
	EXCLAMATION { '!', 0b00100_00100_00100_00100_00100_00000_00100_00000UL },
	QUESTION    { '?', 0b01110_10001_00001_00010_00100_00000_00100_00000UL },
	COLON       { ':', 0b00000_00000_00110_00110_00000_00110_00110_00000UL },
	SEMICOLON   { ';', 0b00000_00000_00110_00110_00000_00110_00100_01000UL },
	QUOTE       { '"', 0b01010_01010_01010_00000_00000_00000_00000_00000UL },
	APOSTROPHE  { '\'', 0b00100_00100_00100_00000_00000_00000_00000_00000UL },
	HYPHEN      { '-', 0b00000_00000_00000_11111_00000_00000_00000_00000UL },
	PLUS        { '+', 0b00000_00100_00100_11111_00100_00100_00000_00000UL },
	EQUALS      { '=', 0b00000_00000_11111_00000_11111_00000_00000_00000UL },
	LESS        { '<', 0b00010_00100_01000_10000_01000_00100_00010_00000UL },
	GREATER     { '>', 0b01000_00100_00010_00001_00010_00100_01000_00000UL },
	LPAREN      { '(', 0b00010_00100_01000_01000_01000_00100_00010_00000UL },
	RPAREN      { ')', 0b01000_00100_00010_00010_00010_00100_01000_00000UL },
	LBRACKET    { '[', 0b01110_01000_01000_01000_01000_01000_01110_00000UL },
	RBRACKET    { ']', 0b01110_00010_00010_00010_00010_00010_01110_00000UL },
	LBRACE      { '{', 0b00011_00100_00100_01000_00100_00100_00011_00000UL },
	RBRACE      { '}', 0b11000_00100_00100_00010_00100_00100_11000_00000UL },
	SLASH       { '/', 0b00001_00010_00010_00100_00100_01000_01000_10000UL },
	BACKSLASH   { '\\', 0b10000_01000_01000_00100_00100_00010_00010_00001UL },
	UNDERSCORE  { '_', 0b00000_00000_00000_00000_00000_00000_00000_11111UL },
	ASTERISK    { '*', 0b00000_01010_00100_11111_00100_01010_00000_00000UL },
	HASH        { '#', 0b01010_01010_11111_01010_11111_01010_01010_00000UL },
	AT          { '@', 0b01110_10001_10111_10101_10110_10000_01111_00000UL },
	AMPERSAND   { '&', 0b01100_10010_01100_01010_10001_10010_01101_00000UL },
	TILDE       { '~', 0b00000_00000_01101_10010_00000_00000_00000_00000UL },
	BACKTICK    { '`', 0b01000_00100_00010_00000_00000_00000_00000_00000UL },
	DOLLAR      { '$', 0b00100_01111_10100_01110_00101_11110_00100_00000UL },
	PERCENT     { '%', 0b11000_11001_00010_00100_01000_10011_00011_00000UL },
	CARET       { '^', 0b00100_01010_10001_00000_00000_00000_00000_00000UL },
	PIPE        { '|', 0b00100_00100_00100_00100_00100_00100_00100_00000UL },
}

// ─────────────────────────────────────────────────────────────────────────────
// Minimal OpenGL Loader & Bindings
// ─────────────────────────────────────────────────────────────────────────────

module opengl::gl;

fn void init(GLLoadFn load)
{
	load_gl(load);
}

alias GLenum     = uint;
alias GLboolean  = char;
alias GLbitfield = uint;
alias GLint      = int;
alias GLuint     = uint;
alias GLsizei    = int;
alias GLfloat    = float;
alias GLchar     = char;
alias GLsizeiptr = sz;
alias GLLoadFn   = fn void*(char*);

const GLenum GL_DEPTH_BUFFER_BIT     @builtin = 0x00000100;
const GLenum GL_COLOR_BUFFER_BIT     @builtin = 0x00004000;
const GLboolean GL_FALSE             @builtin = 0;
const GLenum GL_TRIANGLES            @builtin = 0x0004;
const GLenum GL_LESS                 @builtin = 0x0201;
const GLenum GL_SRC_ALPHA            @builtin = 0x0302;
const GLenum GL_ONE_MINUS_SRC_ALPHA  @builtin = 0x0303;
const GLenum GL_CULL_FACE            @builtin = 0x0B44;
const GLenum GL_DEPTH_TEST           @builtin = 0x0B71;
const GLenum GL_BLEND                @builtin = 0x0BE2;
const GLenum GL_TEXTURE_2D           @builtin = 0x0DE1;
const GLenum GL_UNSIGNED_BYTE        @builtin = 0x1401;
const GLenum GL_UNSIGNED_INT         @builtin = 0x1405;
const GLenum GL_FLOAT                @builtin = 0x1406;
const GLenum GL_RGBA                 @builtin = 0x1908;
const GLenum GL_NEAREST              @builtin = 0x2600;
const GLenum GL_LINEAR               @builtin = 0x2601;
const GLenum GL_TEXTURE_MAG_FILTER   @builtin = 0x2800;
const GLenum GL_TEXTURE_MIN_FILTER   @builtin = 0x2801;
const GLenum GL_TEXTURE0             @builtin = 0x84C0;
const GLenum GL_ARRAY_BUFFER         @builtin = 0x8892;
const GLenum GL_ELEMENT_ARRAY_BUFFER @builtin = 0x8893;
const GLenum GL_STATIC_DRAW          @builtin = 0x88E4;
const GLenum GL_DYNAMIC_DRAW         @builtin = 0x88E8;
const GLenum GL_FRAGMENT_SHADER      @builtin = 0x8B30;
const GLenum GL_VERTEX_SHADER        @builtin = 0x8B31;

alias PfnViewport = fn void(GLint, GLint, GLsizei, GLsizei);
alias PfnEnable = fn void(GLenum);
alias PfnDisable = fn void(GLenum);
alias PfnDepthFunc = fn void(GLenum);
alias PfnClearColor = fn void(GLfloat, GLfloat, GLfloat, GLfloat);
alias PfnClear = fn void(GLbitfield);
alias PfnBlendFunc = fn void(GLenum, GLenum);
alias PfnGenTextures = fn void(GLsizei, GLuint*);
alias PfnDeleteTextures = fn void(GLsizei, GLuint*);
alias PfnBindTexture = fn void(GLenum, GLuint);
alias PfnTexImage2D = fn void(GLenum, GLint, GLint, GLsizei, GLsizei, GLint, GLenum, GLenum, void*);
alias PfnTexParameteri = fn void(GLenum, GLenum, GLint);
alias PfnCreateShader = fn GLuint(GLenum);
alias PfnShaderSource = fn void(GLuint, GLsizei, GLchar**, GLint*);
alias PfnCompileShader = fn void(GLuint);
alias PfnCreateProgram = fn GLuint();
alias PfnAttachShader = fn void(GLuint, GLuint);
alias PfnLinkProgram = fn void(GLuint);
alias PfnDeleteShader = fn void(GLuint);
alias PfnDeleteProgram = fn void(GLuint);
alias PfnGetUniformLocation = fn GLint(GLuint, GLchar*);
alias PfnUseProgram = fn void(GLuint);
alias PfnUniform1f = fn void(GLint, GLfloat);
alias PfnUniform3fv = fn void(GLint, GLsizei, GLfloat*);
alias PfnUniform1i = fn void(GLint, GLint);
alias PfnUniform4fv = fn void(GLint, GLsizei, GLfloat*);
alias PfnUniformMatrix4fv = fn void(GLint, GLsizei, GLboolean, GLfloat*);
alias PfnGenVertexArrays = fn void(GLsizei, GLuint*);
alias PfnDeleteVertexArrays = fn void(GLsizei, GLuint*);
alias PfnBindVertexArray = fn void(GLuint);
alias PfnGenBuffers = fn void(GLsizei, GLuint*);
alias PfnDeleteBuffers = fn void(GLsizei, GLuint*);
alias PfnBindBuffer = fn void(GLenum, GLuint);
alias PfnBufferData = fn void(GLenum, GLsizeiptr, void*, GLenum);
alias PfnEnableVertexAttribArray = fn void(GLuint);
alias PfnVertexAttribPointer = fn void(GLuint, GLint, GLenum, GLboolean, GLsizei, void*);
alias PfnActiveTexture = fn void(GLenum);
alias PfnDrawElements = fn void(GLenum, GLsizei, GLenum, void*);
alias PfnDrawArrays = fn void(GLenum, GLint, GLsizei);

PfnViewport viewport;
PfnEnable enable;
PfnDisable disable;
PfnDepthFunc depthFunc;
PfnClearColor clearColor;
PfnClear clear;
PfnBlendFunc blendFunc;
PfnGenTextures genTextures;
PfnDeleteTextures deleteTextures;
PfnBindTexture bindTexture;
PfnTexImage2D texImage2D;
PfnTexParameteri texParameteri;
PfnCreateShader createShader;
PfnShaderSource shaderSource;
PfnCompileShader compileShader;
PfnCreateProgram createProgram;
PfnAttachShader attachShader;
PfnLinkProgram linkProgram;
PfnDeleteShader deleteShader;
PfnDeleteProgram deleteProgram;
PfnGetUniformLocation getUniformLocation;
PfnUseProgram useProgram;
PfnUniform1f uniform1f;
PfnUniform3fv uniform3fv;
PfnUniform1i uniform1i;
PfnUniform4fv uniform4fv;
PfnUniformMatrix4fv uniformMatrix4fv;
PfnGenVertexArrays genVertexArrays;
PfnDeleteVertexArrays deleteVertexArrays;
PfnBindVertexArray bindVertexArray;
PfnGenBuffers genBuffers;
PfnDeleteBuffers deleteBuffers;
PfnBindBuffer bindBuffer;
PfnBufferData bufferData;
PfnEnableVertexAttribArray enableVertexAttribArray;
PfnVertexAttribPointer vertexAttribPointer;
PfnActiveTexture activeTexture;
PfnDrawElements drawElements;
PfnDrawArrays drawArrays;

fn void load_gl(GLLoadFn l)
{
	viewport = l("glViewport");
	enable = l("glEnable");
	disable = l("glDisable");
	depthFunc = l("glDepthFunc");
	clearColor = l("glClearColor");
	clear = l("glClear");
	blendFunc = l("glBlendFunc");
	genTextures = l("glGenTextures");
	deleteTextures = l("glDeleteTextures");
	bindTexture = l("glBindTexture");
	texImage2D = l("glTexImage2D");
	texParameteri = l("glTexParameteri");
	createShader = l("glCreateShader");
	shaderSource = l("glShaderSource");
	compileShader = l("glCompileShader");
	createProgram = l("glCreateProgram");
	attachShader = l("glAttachShader");
	linkProgram = l("glLinkProgram");
	deleteShader = l("glDeleteShader");
	deleteProgram = l("glDeleteProgram");
	getUniformLocation = l("glGetUniformLocation");
	useProgram = l("glUseProgram");
	uniform1f = l("glUniform1f");
	uniform3fv = l("glUniform3fv");
	uniform1i = l("glUniform1i");
	uniform4fv = l("glUniform4fv");
	uniformMatrix4fv = l("glUniformMatrix4fv");
	genVertexArrays = l("glGenVertexArrays");
	deleteVertexArrays = l("glDeleteVertexArrays");
	bindVertexArray = l("glBindVertexArray");
	genBuffers = l("glGenBuffers");
	deleteBuffers = l("glDeleteBuffers");
	bindBuffer = l("glBindBuffer");
	bufferData = l("glBufferData");
	enableVertexAttribArray = l("glEnableVertexAttribArray");
	vertexAttribPointer = l("glVertexAttribPointer");
	activeTexture = l("glActiveTexture");
	drawElements = l("glDrawElements");
	drawArrays = l("glDrawArrays");
}