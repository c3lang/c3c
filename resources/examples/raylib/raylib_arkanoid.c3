/**
 *
 *   raylib - classic game: arkanoid
 *
 *   Sample game developed by Marc Palau and Ramon Santamaria
 *   converted to C3 by Christoffer Lerno
 *
 *   Copyright (c) 2015 Ramon Santamaria (@raysan5)
 */

module arkanoid;
import raylib55, std::math;

const int SCREEN_WIDTH = 800;
const int SCREEN_HEIGHT = 450;

//----------------------------------------------------------------------------------
// Some Defines
//----------------------------------------------------------------------------------
const PLAYER_MAX_LIFE = 5;
const LINES_OF_BRICKS = 5;
const BRICKS_PER_LINE = 20;

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------
enum GameScreen
{
	LOGO,
	TITLE,
	GAMEPLAY,
	ENDING
}

struct Player
{
	RLVector2 position;
	RLVector2 size;
	int life;
}

struct Ball
{
	RLVector2 position;
	RLVector2 speed;
	int radius;
	bool active;
}

struct Brick
{
	RLVector2 position;
	bool active;
}

//------------------------------------------------------------------------------------
// Global Variables Declaration
//------------------------------------------------------------------------------------

bool game_over = false;
bool pause = false;

Player player;
Ball ball;
Brick[BRICKS_PER_LINE][LINES_OF_BRICKS] brick;
RLVector2 brick_size;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
fn void main()
{
	// Initialization (Note windowTitle is unused on Android)
	//---------------------------------------------------------
	rl::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "classic game: arkanoid");

	init_game();

	rl::set_target_fps(60);
	//--------------------------------------------------------------------------------------

	// Main game loop
	while (!rl::window_should_close())    // Detect window close button or ESC key
	{
		// Update and Draw
		//----------------------------------------------------------------------------------
		update_draw_frame();
		//----------------------------------------------------------------------------------
	}
	// De-Initialization
	//--------------------------------------------------------------------------------------
	unload_game();         // Unload loaded data (textures, sounds, models...)

	rl::close_window();        // Close window and OpenGL context
	//--------------------------------------------------------------------------------------

}

//------------------------------------------------------------------------------------
// Module Functions Definitions (local)
//------------------------------------------------------------------------------------

// Initialize game variables
fn void init_game()
{
	brick_size = { (float)rl::get_screen_width() / BRICKS_PER_LINE, 40 };

	// Initialize player
	player.position = { SCREEN_WIDTH / 2, SCREEN_HEIGHT * 7 / 8 };
	player.size = { SCREEN_WIDTH / 10, 20 };
	player.life = PLAYER_MAX_LIFE;

	// Initialize ball
	ball.position = { SCREEN_WIDTH / 2, SCREEN_HEIGHT * 7 / 8 - 30 };
	ball.speed = { 0, 0 };
	ball.radius = 7;
	ball.active = false;

	// Initialize bricks
	int initial_down_position = 50;

	for (int i = 0; i < LINES_OF_BRICKS; i++)
	{
		for (int j = 0; j < BRICKS_PER_LINE; j++)
		{
			brick[i][j].position = { j * brick_size.x + brick_size.x / 2, i * brick_size.y + initial_down_position };
			brick[i][j].active = true;
		}
	}
}

// Update game (one frame)
fn void update_game()
{
	if (game_over)
	{
		if (rl::is_key_pressed(ENTER))
		{
			init_game();
			game_over = false;
		}
	}
	if (rl::is_key_pressed(P)) pause = !pause;

	if (pause) return;
	// Player movement logic
	if (rl::is_key_down(LEFT)) player.position.x -= 5;
	if ((player.position.x - player.size.x / 2) <= 0) player.position.x = player.size.x / 2;
	if (rl::is_key_down(RIGHT)) player.position.x += 5;
	if ((player.position.x + player.size.x / 2) >= SCREEN_WIDTH) player.position.x = SCREEN_WIDTH - player.size.x / 2;

	// Ball launching logic
	if (!ball.active)
	{
		if (rl::is_key_pressed(SPACE))
		{
			ball.active = true;
			ball.speed = { 0, -5 };
		}
	}

	// Ball movement logic
	if (ball.active)
	{

		ball.position += ball.speed;
	}
	else
	{
		ball.position = { player.position.x, SCREEN_HEIGHT * 7 / 8 - 30 };
	}

	// Collision logic: ball vs walls

	if ((ball.position.x + ball.radius >= SCREEN_WIDTH) || (ball.position.x - ball.radius <= 0)) ball.speed.x *= -1;
	if (ball.position.y - ball.radius <= 0) ball.speed.y *= -1;
	if (ball.position.y + ball.radius >= SCREEN_HEIGHT)
	{
		ball.speed = { 0, 0 };
		ball.active = false;
		player.life--;
	}

	// Collision logic: ball vs player
	if (rl::check_collision_circle_rec(ball.position, ball.radius, { player.position.x - player.size.x / 2, player.position.y - player.size.y / 2, player.size.x, player.size.y}))
	{
		if (ball.speed.y > 0)
		{
			ball.speed.y *= -1;
			ball.speed.x = (ball.position.x - player.position.x) / (player.size.x / 2) * 5;
		}
	}

	// Collision logic: ball vs bricks
	for (int i = 0; i < LINES_OF_BRICKS; i++)
	{
		for (int j = 0; j < BRICKS_PER_LINE; j++)
		{
			if (brick[i][j].active)
			{
				// Hit below

				if (ball.position.y - ball.radius <= brick[i][j].position.y + brick_size.y / 2
					&& ball.position.y - ball.radius > brick[i][j].position.y + brick_size.y / 2 + ball.speed.y
					&& math::abs(ball.position.x - brick[i][j].position.x) < brick_size.x / 2 + ball.radius * 2.0f / 3
					&& ball.speed.y < 0)
				{
					brick[i][j].active = false;
					ball.speed.y *= -1;
				}
				// Hit above
				else if (ball.position.y + ball.radius >= brick[i][j].position.y - brick_size.y / 2
						&& ball.position.y + ball.radius < brick[i][j].position.y - brick_size.y/2 + ball.speed.y
						&& math::abs(ball.position.x - brick[i][j].position.x) < brick_size.x / 2 + ball.radius * 2.0f / 3
						&& ball.speed.y > 0)
				{
					brick[i][j].active = false;
					ball.speed.y *= -1;
				}
				// Hit left
				else if (ball.position.x + ball.radius >= brick[i][j].position.x - brick_size.x / 2
						&& ball.position.x + ball.radius < brick[i][j].position.x - brick_size.x / 2 + ball.speed.x
						&& math::abs(ball.position.y - brick[i][j].position.y) < brick_size.y / 2 + ball.radius * 2.0f / 3
						&& ball.speed.x > 0)
				{
					brick[i][j].active = false;
					ball.speed.x *= -1;
				}
				// Hit right
				else if (ball.position.x - ball.radius <= brick[i][j].position.x + brick_size.x / 2
						&& ball.position.x - ball.radius > brick[i][j].position.x + brick_size.x / 2 + ball.speed.x
						&& math::abs(ball.position.y - brick[i][j].position.y) < brick_size.y / 2 + ball.radius * 2.0f / 3
						&& ball.speed.x < 0)
				{
					brick[i][j].active = false;
					ball.speed.x *= -1;
				}
			}
		}
	}

	// Game over logic
	if (player.life <= 0)
	{
		game_over = true;
		return;
	}
	for (int i = 0; i < LINES_OF_BRICKS; i++)
	{
		for (int j = 0; j < BRICKS_PER_LINE; j++)
		{
			if (brick[i][j].active)
			{
				game_over = false;
				return;
			}
		}
	}
	game_over = true;
}

// Draw game (one frame)
fn void draw_game()
{
	rl::begin_drawing();
	defer rl::end_drawing();

	rl::clear_background(rl::RAYWHITE);

	if (!game_over)
	{
		// Draw player bar
		rl::draw_rectangle((int)(player.position.x - player.size.x/2), (int)(player.position.y - player.size.y/2), (int)player.size.x, (int)player.size.y, rl::BLACK);

		// Draw player lives
		for (int i = 0; i < player.life; i++) rl::draw_rectangle(20 + 40*i, SCREEN_HEIGHT - 30, 35, 10, rl::LIGHTGRAY);

		// Draw ball
		rl::draw_circle_v(ball.position, ball.radius, rl::MAROON);

		// Draw bricks
		for (int i = 0; i < LINES_OF_BRICKS; i++)
		{
			for (int j = 0; j < BRICKS_PER_LINE; j++)
			{
				if (brick[i][j].active)
				{
					RLVector2 brick_pos = brick[i][j].position - brick_size / 2;
					if ((i + j) % 2 == 0)
					{
						rl::draw_rectangle((int)brick_pos.x, (int)brick_pos.y, (int)brick_size.x, (int)brick_size.y, rl::GRAY);
					}
					else
					{
						rl::draw_rectangle((int)brick_pos.x, (int)brick_pos.y, (int)brick_size.x, (int)brick_size.y, rl::DARKGRAY);
					}
				}
			}
		}

		if (pause) rl::draw_text("GAME PAUSED", SCREEN_WIDTH / 2 - rl::measure_text("GAME PAUSED", 40) / 2, SCREEN_HEIGHT / 2 - 40, 40, rl::GRAY);
	}
	else
	{
		rl::draw_text("PRESS [ENTER] TO PLAY AGAIN", rl::get_screen_width()/2 - rl::measure_text("PRESS [ENTER] TO PLAY AGAIN", 20) / 2, rl::get_screen_height() / 2 - 50, 20, rl::GRAY);
	}
}

// Unload game variables
fn void unload_game()
{
	// TODO: Unload all dynamic loaded data (textures, sounds, models...)
}

// Update and Draw (one frame)
fn void update_draw_frame()
{
	update_game();
	draw_game();
}