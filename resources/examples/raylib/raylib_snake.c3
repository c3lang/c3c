module snake;
import raylib5;
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

enum SnakeDirection
{
	RIGHT,
	DOWN,
	LEFT,
	UP
}
struct Snake
{
    Vector2 position;
    Vector2 size;
    Color color;
}

struct Food
{
    Vector2 position;
    Vector2 size;
    bool active;
    Color color;
}


int frames_counter = 0;
bool game_over = false;
bool pause = false;

Food fruit;
SnakeDirection snake_direction;
Snake[SNAKE_LENGTH] snake;
Vector2[SNAKE_LENGTH] snake_position;
bool allow_move = false;
Vector2 offset;
int counter_tail = 0;

fn void main()
{
	rl::initWindow(SCREEN_WIDTH, SCREEN_HEIGHT, "classic game: snake");
	init_game();
    rl::setTargetFPS(60);

    while (!rl::windowShouldClose())    // Detect window close button or ESC key
    {
        update_draw_frame();
    }

    unload_game();

	rl::closeWindow();
}

// Initialize game variables
fn void init_game()
{
    frames_counter = 0;
    game_over = false;
    pause = false;

    counter_tail = 1;
    allow_move = false;
	snake_direction = SnakeDirection.RIGHT;
    offset.x = SCREEN_WIDTH % SQUARE_SIZE;
    offset.y = SCREEN_HEIGHT % SQUARE_SIZE;

    for (int i = 0; i < SNAKE_LENGTH; i++)
    {
        snake[i].position = { offset.x / 2, offset.y / 2 };
        snake[i].size = { SQUARE_SIZE, SQUARE_SIZE };

        if (i == 0)
        {
            snake[i].color = rl::DARKBLUE;
        }
        else
        {
            snake[i].color = rl::BLUE;
        }
    }

    for (int i = 0; i < SNAKE_LENGTH; i++)
    {
        snake_position[i] = { 0.0f, 0.0f };
    }

    fruit.size = { SQUARE_SIZE, SQUARE_SIZE };
    fruit.color = rl::SKYBLUE;
    fruit.active = false;
}


fn void update_game()
{
	if (game_over)
	{
		if (rl::isKeyPressed(rl::KEY_ENTER))
        {
            init_game();
            game_over = false;
        }
		return;
	}

    if (rl::isKeyPressed((KeyboardKey)'P')) pause = !pause;

	if (pause) return;

    if (rl::isKeyPressed(rl::KEY_RIGHT) && allow_move)
    {
        snake_direction = SnakeDirection.from_ordinal((snake_direction.ordinal + 1) % 4);
        allow_move = false;
    }
    if (rl::isKeyPressed(rl::KEY_LEFT) && allow_move)
    {
        snake_direction = SnakeDirection.from_ordinal((snake_direction.ordinal + 3) % 4);
        allow_move = false;
    }

    // Snake movement
    for (int i = 0; i < counter_tail; i++) snake_position[i] = snake[i].position;

    if (frames_counter++ % 5 != 0) return;

    allow_move = true;
	switch (snake_direction)
	{
		case RIGHT:
			snake[0].position.x += SQUARE_SIZE;
            snake[0].position.y += 0;
		case UP:
			snake[0].position.x += 0;
            snake[0].position.y += -SQUARE_SIZE;
		case DOWN:
			snake[0].position.x += 0;
            snake[0].position.y += SQUARE_SIZE;
		case LEFT:
			snake[0].position.x += -SQUARE_SIZE;
            snake[0].position.y += 0;
		default:
			unreachable();
	}
    for (int i = 1; i < counter_tail; i++)
    {
        snake[i].position = snake_position[i - 1];
    }

    // Wall behaviour
    if (((snake[0].position.x) > (SCREEN_WIDTH - offset.x)) ||
        ((snake[0].position.y) > (SCREEN_HEIGHT - offset.y)) ||
        (snake[0].position.x < 0) || (snake[0].position.y < 0))
    {
        game_over = true;
    }

    // Collision with yourself
    for (int i = 1; i < counter_tail; i++)
    {
        if ((snake[0].position.x == snake[i].position.x) && (snake[0].position.y == snake[i].position.y)) game_over = true;
    }

    // Fruit position calculation
    if (!fruit.active)
    {
        fruit.active = true;
        fruit.position = { (float)rl::getRandomValue(0, (SCREEN_WIDTH / SQUARE_SIZE) - 1) * SQUARE_SIZE + offset.x / 2, (float)rl::getRandomValue(0, (SCREEN_HEIGHT / SQUARE_SIZE) - 1) * SQUARE_SIZE + offset.y / 2 };

        for (int i = 0; i < counter_tail; i++)
        {
            while ((fruit.position.x == snake[i].position.x) && (fruit.position.y == snake[i].position.y))
            {
                fruit.position = { (float)rl::getRandomValue(0, (SCREEN_WIDTH / SQUARE_SIZE) - 1) * SQUARE_SIZE + offset.x/2, (float)rl::getRandomValue(0, (SCREEN_HEIGHT / SQUARE_SIZE) - 1) * SQUARE_SIZE + offset.y / 2 };
                i = 0;
            }
        }
    }

    // Collision
    if ((snake[0].position.x < (fruit.position.x + fruit.size.x) && (snake[0].position.x + snake[0].size.x) > fruit.position.x) &&
        (snake[0].position.y < (fruit.position.y + fruit.size.y) && (snake[0].position.y + snake[0].size.y) > fruit.position.y))
    {
        snake[counter_tail].position = snake_position[counter_tail - 1];
        counter_tail += 1;
        fruit.active = false;
    }
}

// Draw game (one frame)
fn void draw_game()
{
	rl::beginDrawing();

    rl::clearBackground(rl::RAYWHITE);

    if (!game_over)
    {
        // Draw grid lines
        for (int i = 0; i < SCREEN_WIDTH / SQUARE_SIZE + 1; i++)
        {
            rl::drawLineV({(float)SQUARE_SIZE * i + offset.x/2, offset.y/2}, {(float)SQUARE_SIZE * i + offset.x/2, SCREEN_HEIGHT - offset.y/2}, rl::LIGHTGRAY);
        }

            for (int i = 0; i < SCREEN_HEIGHT/SQUARE_SIZE + 1; i++)
            {
                rl::drawLineV({offset.x/2, (float)SQUARE_SIZE * i + offset.y / 2 }, { SCREEN_WIDTH - offset.x/2, (float)SQUARE_SIZE * i + offset.y / 2 }, rl::LIGHTGRAY);
            }

            // Draw snake
            for (int i = 0; i < counter_tail; i++) rl::drawRectangleV(snake[i].position, snake[i].size, snake[i].color);

            // Draw fruit to pick
            rl::drawRectangleV(fruit.position, fruit.size, fruit.color);

            if (pause) rl::drawText("GAME PAUSED", SCREEN_WIDTH/2 - rl::measureText("GAME PAUSED", 40)/2, SCREEN_HEIGHT / 2 - 40, 40, rl::GRAY);
        }
        else
        {
            rl::drawText("PRESS [ENTER] TO PLAY AGAIN", rl::getScreenWidth()/2 - rl::measureText("PRESS [ENTER] TO PLAY AGAIN", 20)/2, rl::getScreenHeight()/2 - 50, 20, rl::GRAY);
        }

    rl::endDrawing();
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