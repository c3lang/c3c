module snake;
import raylib55;
/**
 *
 *   raylib - classic game: snake
 *
 *   Sample game developed by Ian Eito, Albert Martos and Ramon Santamaria,
 *   converted to C3 and modified by Christoffer Lerno
 *
 *   Copyright (c) 2015 Ramon Santamaria (@raysan5)
 *
 */

const SNAKE_LENGTH = 256;
const SQUARE_SIZE = 32;
const int SCREEN_WIDTH = 800;
const int SCREEN_HEIGHT = 450;

enum SnakeDirection : (RLVector2 dir)
{
	RIGHT {{  SQUARE_SIZE,  0 }},
	DOWN  {{  0,  SQUARE_SIZE }},
	LEFT  {{ -SQUARE_SIZE,  0 }},
	UP    {{  0, -SQUARE_SIZE }}
}
struct Snake
{
	RLVector2 position;
	RLVector2 size;
	RLColor color;
}

struct Food
{
	RLVector2 position;
	RLVector2 size;
	bool active;
	RLColor color;
}


int frames_counter = 0;
bool game_over = false;
bool pause = false;

Food fruit;
SnakeDirection snake_direction;
Snake[SNAKE_LENGTH] snake;
RLVector2[SNAKE_LENGTH] snake_position;
bool allow_move = false;
RLVector2 offset;
int counter_tail = 0;

fn void main()
{
	rl::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "classic game: snake");
	init_game();
	rl::set_target_fps(60);

	while (!rl::window_should_close())    // Detect window close button or ESC key
	{
		update_draw_frame();
	}

	unload_game();

	rl::close_window();
}
// Initialize game variables
fn void init_game()
{
	frames_counter = 0;
	game_over = false;
	pause = false;

	counter_tail = 1;
	allow_move = false;
	snake_direction = RIGHT;
	offset.x = SCREEN_WIDTH % SQUARE_SIZE;
	offset.y = SCREEN_HEIGHT % SQUARE_SIZE;
	foreach (i, &s : snake)
	{
		s.position = offset / 2;
		s.size = (RLVector2)SQUARE_SIZE;
		s.color = i ? rl::BLUE : rl::DARKBLUE;
	}

	foreach (&pos : snake_position)
	{
		*pos = { 0, 0 };
	}

	fruit = { .size = (RLVector2)SQUARE_SIZE, .color = rl::SKYBLUE, .active = false };
}

fn RLVector2 random_fruit_position()
{
	return offset / 2 + { (float)rl::get_random_value(0, SCREEN_WIDTH / SQUARE_SIZE - 1) * SQUARE_SIZE, (float)rl::get_random_value(0, (SCREEN_HEIGHT / SQUARE_SIZE) - 1) * SQUARE_SIZE };
}

fn void update_game()
{
	if (game_over)
	{
		if (rl::is_key_pressed(ENTER))
		{
			init_game();
			game_over = false;
		}
		return;
	}

	if (rl::is_key_pressed(P)) pause = !pause;

	if (pause) return;

	if (rl::is_key_pressed(RIGHT) && allow_move)
	{
		snake_direction = SnakeDirection.from_ordinal((snake_direction.ordinal + 1) % 4);
		allow_move = false;
	}
	if (rl::is_key_pressed(LEFT) && allow_move)
	{
		snake_direction = SnakeDirection.from_ordinal((snake_direction.ordinal + 3) % 4);
		allow_move = false;
	}

	// Snake movement
	foreach (i, &sp : snake_position[:counter_tail]) *sp = snake[i].position;

	if (frames_counter++ % 20 != 0) return;

	allow_move = true;
	snake[0].position += snake_direction.dir;
	foreach (i, &s : snake[1 .. counter_tail - 1]) s.position = snake_position[i];

	// Wall behaviour
	if (snake[0].position.comp_ge({SCREEN_WIDTH - offset.x, SCREEN_HEIGHT - offset.y}).or() || snake[0].position.comp_lt({ 0, 0 }).or())
	{
		game_over = true;
	}

	// Collision with yourself
	for (int i = 1; i < counter_tail; i++)
	{
		if (snake[0].position == snake[i].position) game_over = true;
	}

	// Fruit position calculation
	if (!fruit.active)
	{
		fruit.active = true;
		fruit.position = random_fruit_position();

		for (int i = 0; i < counter_tail; i++)
		{
			while (fruit.position == snake[i].position)
			{
				fruit.position = random_fruit_position();
				i = 0;
			}
		}
	}

	// Collision
	if (snake[0].position.comp_lt(fruit.position + fruit.size).and() && fruit.position.comp_lt(snake[0].position + snake[0].size).and())
	{
		snake[counter_tail].position = snake_position[counter_tail - 1];
		counter_tail += 1;
		fruit.active = false;
	}
}

// Draw game (one frame)
fn void draw_game()
{
	rl::begin_drawing();

	rl::clear_background(rl::RAYWHITE);

	defer rl::end_drawing();
	if (game_over)
	{
		rl::draw_text("PRESS [ENTER] TO PLAY AGAIN", rl::get_screen_width() / 2 - rl::measure_text("PRESS [ENTER] TO PLAY AGAIN", 20)/2, rl::get_screen_height() / 2 - 50, 20, rl::GRAY);
		return;
	}
	// Draw grid lines
	for (int i = 0; i < SCREEN_WIDTH / SQUARE_SIZE + 1; i++)
	{
		rl::draw_line_v(offset / 2 + { (float)SQUARE_SIZE * i, 0}, {(float)SQUARE_SIZE * i + offset.x / 2, SCREEN_HEIGHT - offset.y / 2}, rl::LIGHTGRAY);
	}

	for (int i = 0; i < SCREEN_HEIGHT/SQUARE_SIZE + 1; i++)
	{
		rl::draw_line_v(offset / 2 + { 0, (float)SQUARE_SIZE * i }, { SCREEN_WIDTH - offset.x/2, (float)SQUARE_SIZE * i + offset.y / 2 }, rl::LIGHTGRAY);
	}

	// Draw snake
	foreach (&s : snake[:counter_tail]) rl::draw_rectangle_v(s.position, s.size, s.color);

	// Draw fruit to pick
	rl::draw_rectangle_v(fruit.position, fruit.size, fruit.color);

	if (pause) rl::draw_text("GAME PAUSED", SCREEN_WIDTH / 2 - rl::measure_text("GAME PAUSED", 40) / 2, SCREEN_HEIGHT / 2 - 40, 40, rl::GRAY);

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