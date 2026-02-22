/*******************************************************************************************
*
*   raylib [core] example - 2d camera platformer
*
*   Example complexity rating: [★★★☆] 3/4
*
*   Example originally created with raylib 2.5, last time updated with raylib 3.0
*
*   Example contributed by arvyy (@arvyy) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2019-2025 arvyy (@arvyy)
*   converted to C3 by Christoffer Lerno
*
********************************************************************************************/

module raylib_camera_platformer;
import raylib55;

const G = 400;
const float PLAYER_JUMP_SPD = 350;
const float PLAYER_HOR_SPD = 200;

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------
struct Player
{
	RLVector2 position;
	float speed;
	bool can_jump;
}

struct EnvItem
{
	RLRectangle rect;
	int blocking;
	RLColor color;
}


alias CameraUpdateFn = fn void(RLCamera2D* camera, Player* player, EnvItem[] env_items, float delta, int width, int height);

enum CameraUpdateType : (ZString text, CameraUpdateFn func)
{
	CENTER               { "Follow player center", &update_camera_center },
	CENTER_INSIDE_MAP    { "Follow player center, but clamp to map edges", &update_camera_center_inside_map },
	CENTER_SMOOTH_FOLLOW { "Follow player center; smoothed", &update_camera_center_smooth_follow },
	EVEN_OUT_ON_LANDING  { "Follow player center horizontally; update player center vertically after landing", &update_camera_even_out_on_landing },
	PLAYER_BOUNDS_PUSH   { "Player push camera on getting too close to screen edge", &update_camera_player_bounds_push }
}

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
fn int main()
{
	// Initialization
	//--------------------------------------------------------------------------------------
	const int SCREEN_WIDTH = 800;
	const int SCREEN_HEIGHT = 450;

	rl::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "raylib [core] example - 2d camera platformer");

	Player player = {
		.position = { 400, 280 },
		.speed = 0,
		.can_jump = false
	};
	EnvItem[*] env_items = {
		{{ 0, 0, 1000, 400 },   0, rl::LIGHTGRAY },
		{{ 0, 400, 1000, 200 }, 1, rl::GRAY },
		{{ 300, 200, 400, 10 }, 1, rl::GRAY },
		{{ 250, 300, 100, 10 }, 1, rl::GRAY },
		{{ 650, 300, 100, 10 }, 1, rl::GRAY }
	};

	RLCamera2D camera = {
		.target = player.position,
		.offset = { SCREEN_WIDTH / 2.0f, SCREEN_HEIGHT / 2.0f },
		.rotation = 0.0f,
		.zoom = 1.0f
	};


	CameraUpdateType camera_option = CENTER;

	rl::set_target_fps(60);
	//--------------------------------------------------------------------------------------

	// Main game loop
	while (!rl::window_should_close())
	{
		// Update
		//----------------------------------------------------------------------------------
		float delta_time = rl::get_frame_time();

		update_player(&player, &env_items, delta_time);
		camera.zoom += ((float)rl::get_mouse_wheel_move() * 0.05f);

		switch
		{
			case camera.zoom > 3.0f: camera.zoom = 3.0f;
			case camera.zoom < 0.25f: camera.zoom = 0.25f;
		}

		if (rl::is_key_pressed(R))
		{
			camera.zoom = 1.0f;
			player.position = { 400, 280 };
		}

		if (rl::is_key_pressed(C)) camera_option = CameraUpdateType.from_ordinal(((int)camera_option + 1) % (int)CameraUpdateType.len);

		// Call update camera function by its pointer
		camera_option.func(&camera, &player, &env_items, delta_time, SCREEN_WIDTH, SCREEN_HEIGHT);
		//----------------------------------------------------------------------------------

		// Draw
		//----------------------------------------------------------------------------------
		rl::@drawing()
		{
			rl::clear_background(rl::LIGHTGRAY);
			rl::@mode2d(camera)
			{
				foreach (item : env_items) rl::draw_rectangle_rec(item.rect, item.color);
				RLRectangle player_rect = { player.position.x - 20, player.position.y - 40, 40.0f, 40.0f };
				rl::draw_rectangle_rec(player_rect, rl::RED);
				rl::draw_circle_v(player.position, 5.0f, rl::GOLD);
			};
			rl::draw_text("Controls:", 20, 20, 10, rl::BLACK);
			rl::draw_text("- Right/Left to move", 40, 40, 10, rl::DARKGRAY);
			rl::draw_text("- Space to jump", 40, 60, 10, rl::DARKGRAY);
			rl::draw_text("- Mouse Wheel to Zoom in-out, R to reset zoom", 40, 80, 10, rl::DARKGRAY);
			rl::draw_text("- C to change camera mode", 40, 100, 10, rl::DARKGRAY);
			rl::draw_text("Current camera mode:", 20, 120, 10, rl::BLACK);
			rl::draw_text(camera_option.text, 40, 140, 10, rl::DARKGRAY);
		};
		//----------------------------------------------------------------------------------
	}

	// De-Initialization
	//--------------------------------------------------------------------------------------
	rl::close_window();        // Close window and OpenGL context
	//--------------------------------------------------------------------------------------

	return 0;
}
fn void update_player(Player* player, EnvItem[] env_items, float delta)
{
	if (rl::is_key_down(LEFT)) player.position.x -= PLAYER_HOR_SPD * delta;
	if (rl::is_key_down(RIGHT)) player.position.x += PLAYER_HOR_SPD * delta;
	if (rl::is_key_down(SPACE) && player.can_jump)
	{
		player.speed = -PLAYER_JUMP_SPD;
		player.can_jump = false;
	}

	bool hit_obstacle = false;
	foreach (item : env_items)
	{
		RLVector2* p = &player.position;
		if (item.blocking
			&& item.rect.x <= p.x
			&& item.rect.x + item.rect.width >= p.x
			&& item.rect.y >= p.y
			&& item.rect.y <= p.y + player.speed * delta)
		{
			hit_obstacle = true;
			player.speed = 0.0f;
			p.y = item.rect.y;
			break;
		}
	}

	if (hit_obstacle)
	{
		player.can_jump = true;
	}
	else
	{
		player.position.y += player.speed * delta;
		player.speed += G * delta;
		player.can_jump = false;
	}
}

fn void update_camera_center(RLCamera2D* camera, Player* player, EnvItem[] env_items, float delta, int width, int height)
{
	camera.offset = { width / 2.0f, height / 2.0f };
	camera.target = player.position;
}

fn void update_camera_center_inside_map(RLCamera2D* camera, Player* player, EnvItem[] env_items, float delta, int width, int height)
{
	camera.target = player.position;
	camera.offset = { width / 2.0f, height / 2.0f };
	float min_x = 1000;
	float min_y = 1000;
	float max_x = -1000;
	float max_y = -1000;

	foreach (item : env_items)
	{
		min_x = min(item.rect.x, min_x);
		max_x = max(item.rect.x + item.rect.width, max_x);
		min_y = min(item.rect.y, min_y);
		max_y = max(item.rect.y + item.rect.height, max_y);
	}

	RLVector2 max = rl::get_world_to_screen2d({ max_x, max_y }, *camera);
	RLVector2 min = rl::get_world_to_screen2d({ min_x, min_y }, *camera);

	if (max.x < width) camera.offset.x = width - (max.x - width / 2.0f);
	if (max.y < height) camera.offset.y = height - (max.y - height / 2.0f);
	if (min.x > 0) camera.offset.x = width / 2.0f - min.x;
	if (min.y > 0) camera.offset.y = height / 2.0f - min.y;
}

fn void update_camera_center_smooth_follow(RLCamera2D* camera, Player* player, EnvItem[] env_items, float delta, int width, int height)
{
	const float MIN_SPEED = 30;
	const float MIN_EFFECT_LENGTH = 10;
	const float FRACTION_SPEED = 0.8f;

	camera.offset = { width / 2.0f, height / 2.0f };
	float length = player.position.distance(camera.target);

	if (length > MIN_EFFECT_LENGTH)
	{
		float speed = max(FRACTION_SPEED * length, MIN_SPEED);
		camera.target += (player.position - camera.target) * speed * delta / length;
	}
}

fn void update_camera_even_out_on_landing(RLCamera2D* camera, Player* player, EnvItem[] env_items, float delta, int width, int height)
{
	const float EVEN_OUT_SPEED = 700;
	static bool evening_out = false;
	static float even_out_target;

	camera.offset = { width / 2.0f, height / 2.0f };
	camera.target.x = player.position.x;

	if (evening_out)
	{
		if (even_out_target > camera.target.y)
		{
			camera.target.y += EVEN_OUT_SPEED * delta;

			if (camera.target.y > even_out_target)
			{
				camera.target.y = even_out_target;
				evening_out = false;
			}
		}
		else
		{
			camera.target.y -= EVEN_OUT_SPEED * delta;

			if (camera.target.y < even_out_target)
			{
				camera.target.y = even_out_target;
				evening_out = false;
			}
		}
	}
	else
	{
		if (player.can_jump && player.speed == 0 && player.position.y != camera.target.y)
		{
			evening_out = true;
			even_out_target = player.position.y;
		}
	}
}

fn void update_camera_player_bounds_push(RLCamera2D* camera, Player* player, EnvItem[] env_items, float delta, int width, int height)
{
	const RLVector2 BBOX = { 0.2f, 0.2f };

	RLVector2 bboxWorldMin = rl::get_screen_to_world2d({ (1 - BBOX.x) * 0.5f * width, (1 - BBOX.y) * 0.5f*height }, *camera);
	RLVector2 bboxWorldMax = rl::get_screen_to_world2d({ (1 + BBOX.x) * 0.5f * width, (1 + BBOX.y) * 0.5f*height }, *camera);
	camera.offset = { (1 - BBOX.x) * 0.5f * width, (1 - BBOX.y) * 0.5f * height };

	if (player.position.x < bboxWorldMin.x) camera.target.x = player.position.x;
	if (player.position.y < bboxWorldMin.y) camera.target.y = player.position.y;
	if (player.position.x > bboxWorldMax.x) camera.target.x = bboxWorldMin.x + (player.position.x - bboxWorldMax.x);
	if (player.position.y > bboxWorldMax.y) camera.target.y = bboxWorldMin.y + (player.position.y - bboxWorldMax.y);
}