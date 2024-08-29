module tetris;
import raylib5;
/**
 *   raylib - classic game: tetris
 *
 *   Sample game developed by Marc Palau and Ramon Santamaria,
 *   converted to C3 by Christoffer Lerno.
 *
 *   This game has been created using raylib v1.3 (www.raylib.com)
 *
 *   Copyright (c) 2015 Ramon Santamaria (@raysan5)
 */

//----------------------------------------------------------------------------------
// Some Defines
//----------------------------------------------------------------------------------
const SQUARE_SIZE = 20;
const GRID_HORIZONTAL_SIZE = 12;
const GRID_VERTICAL_SIZE = 20;

const LATERAL_SPEED = 10;
const TURNING_SPEED = 12;
const FAST_FALL_AWAIT_COUNTER = 30;
const FADING_TIME = 33;

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------
enum GridSquare { EMPTY, MOVING, FULL, BLOCK, FADING }

//------------------------------------------------------------------------------------
// Global Variables Declaration
//------------------------------------------------------------------------------------
const int SCREEN_WIDTH = 800;
const int SCREEN_HEIGHT = 450;

bool game_over = false;
bool pause = false;

// Matrices
GridSquare[GRID_VERTICAL_SIZE][GRID_HORIZONTAL_SIZE] grid;
GridSquare[4][4] piece;
GridSquare[4][4] incoming_piece;

struct IntVec
{
	int x;
	int y;
}
// These variables keep track of the active piece position
int piece_position_x = 0;
int piece_position_y = 0;

// Game parameters
Color fading_color;
//int fallingSpeed;           // In frames

bool begin_play = true;      // This var is only true at the begining of the game, used for the first matrix creations
bool piece_active = false;
bool detection = false;
bool line_to_delete = false;

// Statistics
int level = 1;
int lines = 0;

// Counters
int gravity_movement_counter = 0;
int lateral_movement_counter = 0;
int turn_movement_counter = 0;
int fast_fall_movement_counter = 0;

int fade_line_counter = 0;

// Based on level
int gravity_speed = 30;


//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
fn void main()
{
    // Initialization (Note windowTitle is unused on Android)
    //---------------------------------------------------------
    rl::initWindow(SCREEN_WIDTH, SCREEN_HEIGHT, "classic game: tetris");

    init_game();

    rl::setTargetFPS(60);
    //--------------------------------------------------------------------------------------

    // Main game loop
    while (!rl::windowShouldClose())    // Detect window close button or ESC key
    {
        // Update and Draw
        //----------------------------------------------------------------------------------
        update_draw_frame();
        //----------------------------------------------------------------------------------
    }
    // De-Initialization
    //--------------------------------------------------------------------------------------
    unload_game();         // Unload loaded data (textures, sounds, models...)

    rl::closeWindow();        // Close window and OpenGL context
    //--------------------------------------------------------------------------------------

}

//--------------------------------------------------------------------------------------
// Game Module Functions Definition
//--------------------------------------------------------------------------------------

// Initialize game variables
fn void init_game()
{
    // Initialize game statistics
    level = 1;
    lines = 0;

    fading_color = rl::GRAY;

    piece_position_x = 0;
    piece_position_y = 0;

    pause = false;

    begin_play = true;
    piece_active = false;
    detection = false;
    line_to_delete = false;

    // Counters
    gravity_movement_counter = 0;
    lateral_movement_counter = 0;
    turn_movement_counter = 0;
    fast_fall_movement_counter = 0;

    fade_line_counter = 0;
    gravity_speed = 30;

    // Initialize grid matrices
    for (int i = 0; i < GRID_HORIZONTAL_SIZE; i++)
    {
        for (int j = 0; j < GRID_VERTICAL_SIZE; j++)
        {
            if ((j == GRID_VERTICAL_SIZE - 1) || (i == 0) || (i == GRID_HORIZONTAL_SIZE - 1))
            {
                grid[i][j] = BLOCK;
            }
            else
            {
                grid[i][j] = EMPTY;
	        }
        }
    }

    // Initialize incoming piece matrices
    for (int i = 0; i < 4; i++)
    {
        for (int j = 0; j< 4; j++)
        {
            incoming_piece[i][j] = EMPTY;
        }
    }
}

// Update game (one frame)
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
	if (line_to_delete)
	{
        // Animation when deleting lines
        fade_line_counter++;

		fading_color = fade_line_counter % 8 < 4 ? rl::MAROON : rl::GRAY;

        if (fade_line_counter >= FADING_TIME)
        {
            lines += delete_complete_lines();
            fade_line_counter = 0;
            line_to_delete = false;
        }
        return;
	}

    if (!piece_active)
    {
        // Get another piece
        piece_active = create_piece();

        // We leave a little time before starting the fast falling down
        fast_fall_movement_counter = 0;
    }
    else    // Piece falling
    {
        // Counters update
        fast_fall_movement_counter++;
        gravity_movement_counter++;
        lateral_movement_counter++;
        turn_movement_counter++;

        // We make sure to move if we've pressed the key this frame
        if (rl::isKeyPressed(rl::KEY_LEFT) || rl::isKeyPressed(rl::KEY_RIGHT)) lateral_movement_counter = LATERAL_SPEED;
        if (rl::isKeyPressed(rl::KEY_UP)) turn_movement_counter = TURNING_SPEED;

        // Fall down
        if (rl::isKeyDown(rl::KEY_DOWN) && (fast_fall_movement_counter >= FAST_FALL_AWAIT_COUNTER))
        {
            // We make sure the piece is going to fall this frame
            gravity_movement_counter += gravity_speed;
        }

        if (gravity_movement_counter >= gravity_speed)
        {
            // Basic falling movement
            if (check_detection()) detection = true;

            // Check if the piece has collided with another piece or with the boundings
            resolve_falling_movement(&detection, &piece_active);

            // Check if we fullfilled a line and if so, erase the line and pull down the the lines above
            check_completion(&line_to_delete);

            gravity_movement_counter = 0;
        }

        // Move laterally at player's will
        if (lateral_movement_counter >= LATERAL_SPEED)
        {
            // Update the lateral movement and if success, reset the lateral counter
            if (!resolve_lateral_movement()) lateral_movement_counter = 0;
        }

        // Turn the piece at player's will
        if (turn_movement_counter >= TURNING_SPEED)
        {
            // Update the turning movement and reset the turning counter
            if (resolve_turn_movement()) turn_movement_counter = 0;
        }
    }

    // Game over logic
    for (int j = 0; j < 2; j++)
    {
        for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)
        {
            if (grid[i][j] == GridSquare.FULL)
            {
                game_over = true;
            }
        }
    }
}

// Draw game (one frame)
fn void draw_game()
{
    rl::beginDrawing();
	defer rl::endDrawing();
    rl::clearBackground(rl::RAYWHITE);

	if (game_over)
	{
		rl::drawText("PRESS [ENTER] TO PLAY AGAIN", rl::getScreenWidth() / 2 - rl::measureText("PRESS [ENTER] TO PLAY AGAIN", 20) / 2, rl::getScreenHeight() / 2 - 50, 20, rl::GRAY);
		return;
	}

    // Draw gameplay area
    IntVec offset = {
        SCREEN_WIDTH / 2 - (GRID_HORIZONTAL_SIZE * SQUARE_SIZE / 2) - 50,
        SCREEN_HEIGHT / 2 - ((GRID_VERTICAL_SIZE - 1) * SQUARE_SIZE / 2) + SQUARE_SIZE * 2
    };
    offset.y -= 50;     // NOTE: Harcoded position!

    int controller = offset.x;

    for (int j = 0; j < GRID_VERTICAL_SIZE; j++)
    {
        for (int i = 0; i < GRID_HORIZONTAL_SIZE; i++)
        {
            // Draw each square of the grid
            switch (grid[i][j])
            {
                case EMPTY:
	                rl::drawLine(offset.x, offset.y, offset.x + SQUARE_SIZE, offset.y, rl::LIGHTGRAY );
	                rl::drawLine(offset.x, offset.y, offset.x, offset.y + SQUARE_SIZE, rl::LIGHTGRAY );
	                rl::drawLine(offset.x + SQUARE_SIZE, offset.y, offset.x + SQUARE_SIZE, offset.y + SQUARE_SIZE, rl::LIGHTGRAY );
	                rl::drawLine(offset.x, offset.y + SQUARE_SIZE, offset.x + SQUARE_SIZE, offset.y + SQUARE_SIZE, rl::LIGHTGRAY );
	                offset.x += SQUARE_SIZE;
	            case FULL:
	                rl::drawRectangle(offset.x, offset.y, SQUARE_SIZE, SQUARE_SIZE, rl::GRAY);
                    offset.x += SQUARE_SIZE;
                case MOVING:
	                rl::drawRectangle(offset.x, offset.y, SQUARE_SIZE, SQUARE_SIZE, rl::DARKGRAY);
	                offset.x += SQUARE_SIZE;
	            case BLOCK:
	                rl::drawRectangle(offset.x, offset.y, SQUARE_SIZE, SQUARE_SIZE, rl::LIGHTGRAY);
	                offset.x += SQUARE_SIZE;
	            case FADING:
	                rl::drawRectangle(offset.x, offset.y, SQUARE_SIZE, SQUARE_SIZE, fading_color);
                    offset.x += SQUARE_SIZE;
                default:
            }
        }

        offset.x = controller;
        offset.y += SQUARE_SIZE;
    }

    // Draw incoming piece (hardcoded)
    offset.x = 500;
    offset.y = 45;

    controller = offset.x;

    for (int j = 0; j < 4; j++)
    {
        for (int i = 0; i < 4; i++)
        {
            switch (incoming_piece[i][j])
            {
                case EMPTY:
	                rl::drawLine(offset.x, offset.y, offset.x + SQUARE_SIZE, offset.y, rl::LIGHTGRAY);
	                rl::drawLine(offset.x, offset.y, offset.x, offset.y + SQUARE_SIZE, rl::LIGHTGRAY);
	                rl::drawLine(offset.x + SQUARE_SIZE, offset.y, offset.x + SQUARE_SIZE, offset.y + SQUARE_SIZE, rl::LIGHTGRAY);
	                rl::drawLine(offset.x, offset.y + SQUARE_SIZE, offset.x + SQUARE_SIZE, offset.y + SQUARE_SIZE, rl::LIGHTGRAY);
	                offset.x += SQUARE_SIZE;
	            case MOVING:
	                rl::drawRectangle(offset.x, offset.y, SQUARE_SIZE, SQUARE_SIZE, rl::GRAY);
                    offset.x += SQUARE_SIZE;
                default:
                    break;
            }
        }

        offset.x = controller;
        offset.y += SQUARE_SIZE;
    }

    rl::drawText("INCOMING:", offset.x, offset.y - 100, 10, rl::GRAY);
    rl::drawText(rl::textFormat("LINES:      %04i", lines), offset.x, offset.y + 20, 10, rl::GRAY);

    if (pause)
    {
        rl::drawText("GAME PAUSED", SCREEN_WIDTH / 2 - rl::measureText("GAME PAUSED", 40)/2, SCREEN_HEIGHT/2 - 40, 40, rl::GRAY);
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

//--------------------------------------------------------------------------------------
// Additional module functions
//--------------------------------------------------------------------------------------
fn bool create_piece()
{
    piece_position_x = (int)((GRID_HORIZONTAL_SIZE - 4)/2);
    piece_position_y = 0;

    // If the game is starting and you are going to create the first piece, we create an extra one
    if (begin_play)
    {
        get_random_piece();
        begin_play = false;
    }

    // We assign the incoming piece to the actual piece
    for (int i = 0; i < 4; i++)
    {
        for (int j = 0; j< 4; j++)
        {
            piece[i][j] = incoming_piece[i][j];
        }
    }

    // We assign a random piece to the incoming one
    get_random_piece();

    // Assign the piece to the grid
    for (int i = piece_position_x; i < piece_position_x + 4; i++)
    {
        for (int j = 0; j < 4; j++)
        {
            if (piece[i - (int)piece_position_x][j] == GridSquare.MOVING) grid[i][j] = MOVING;
        }
    }

    return true;
}

fn void get_random_piece()
{
    int random = rl::getRandomValue(0, 6);

    for (int i = 0; i < 4; i++)
    {
        for (int j = 0; j < 4; j++)
        {
            incoming_piece[i][j] = EMPTY;
        }
    }

    switch (random)
    {
        case 0:
            incoming_piece[1][1] = MOVING;
            incoming_piece[2][1] = MOVING;
            incoming_piece[1][2] = MOVING;
            incoming_piece[2][2] = MOVING;    //Cube
        case 1:
            incoming_piece[1][0] = MOVING;
            incoming_piece[1][1] = MOVING;
            incoming_piece[1][2] = MOVING;
            incoming_piece[2][2] = MOVING; //L
        case 2:
            incoming_piece[1][2] = MOVING;
            incoming_piece[2][0] = MOVING;
            incoming_piece[2][1] = MOVING;
            incoming_piece[2][2] = MOVING;  //L inversa
        case 3:
            incoming_piece[0][1] = MOVING;
            incoming_piece[1][1] = MOVING;
            incoming_piece[2][1] = MOVING;
            incoming_piece[3][1] = MOVING; //Recta
        case 4:
            incoming_piece[1][0] = MOVING;
            incoming_piece[1][1] = MOVING;
            incoming_piece[1][2] = MOVING;
            incoming_piece[2][1] = MOVING; //Creu tallada
        case 5:
            incoming_piece[1][1] = MOVING;
            incoming_piece[2][1] = MOVING;
            incoming_piece[2][2] = MOVING;
            incoming_piece[3][2] = MOVING; //S
        case 6:
            incoming_piece[1][2] = MOVING;
            incoming_piece[2][2] = MOVING;
            incoming_piece[2][1] = MOVING;
            incoming_piece[3][1] = MOVING; //S inversa
        default:
            unreachable();
    }
}

fn void resolve_falling_movement(bool* detection_ref, bool* piece_active_ref)
{
    // If we finished moving this piece, we stop it
    if (*detection_ref)
    {
        for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
        {
            for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)
            {
                if (grid[i][j] == GridSquare.MOVING)
                {
                    grid[i][j] = FULL;
                    *detection_ref = false;
                    *piece_active_ref = false;
                }
            }
        }
    }
    else    // We move down the piece
    {
        for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
        {
            for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)
            {
                if (grid[i][j] == GridSquare.MOVING)
                {
                    grid[i][j+1] = MOVING;
                    grid[i][j] = EMPTY;
                }
            }
        }

        piece_position_y++;
    }
}

fn bool resolve_lateral_movement()
{
    bool collision = false;

    // Piece movement
    if (rl::isKeyDown(rl::KEY_LEFT))        // Move left
    {
        // Check if is possible to move to left
        for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
        {
            for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)
            {
                if (grid[i][j] == GridSquare.MOVING)
                {
                    // Check if we are touching the left wall or we have a full square at the left
                    if ((i-1 == 0) || (grid[i-1][j] == GridSquare.FULL)) collision = true;
                }
            }
        }

        // If able, move left
        if (!collision)
        {
            for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
            {
                for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)             // We check the matrix from left to right
                {
                    // Move everything to the left
                    if (grid[i][j] == GridSquare.MOVING)
                    {
                        grid[i-1][j] = MOVING;
                        grid[i][j] = EMPTY;
                    }
                }
            }

            piece_position_x--;
        }
    }
    else if (rl::isKeyDown(rl::KEY_RIGHT))  // Move right
    {
        // Check if is possible to move to right
        for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
        {
            for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)
            {
                if (grid[i][j] == GridSquare.MOVING)
                {
                    // Check if we are touching the right wall or we have a full square at the right
                    if ((i+1 == GRID_HORIZONTAL_SIZE - 1) || (grid[i+1][j] == GridSquare.FULL))
                    {
                        collision = true;

                    }
                }
            }
        }

        // If able move right
        if (!collision)
        {
            for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
            {
                for (int i = GRID_HORIZONTAL_SIZE - 1; i >= 1; i--)             // We check the matrix from right to left
                {
                    // Move everything to the right
                    if (grid[i][j] == GridSquare.MOVING)
                    {
                        grid[i+1][j] = MOVING;
                        grid[i][j] = EMPTY;
                    }
                }
            }

            piece_position_x++;
        }
    }

    return collision;
}

fn bool resolve_turn_movement()
{
    // Input for turning the piece
    if (rl::isKeyDown(rl::KEY_UP))
    {
        GridSquare aux;
        bool checker = false;

        // Check all turning possibilities
        if ((grid[piece_position_x + 3][piece_position_y] == GridSquare.MOVING) &&
            (grid[piece_position_x][piece_position_y] != GridSquare.EMPTY) &&
            (grid[piece_position_x][piece_position_y] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 3][piece_position_y + 3] == GridSquare.MOVING) &&
            (grid[piece_position_x + 3][piece_position_y] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 3][piece_position_y] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x][piece_position_y + 3] == GridSquare.MOVING) &&
            (grid[piece_position_x + 3][piece_position_y + 3] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 3][piece_position_y + 3] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x][piece_position_y] == GridSquare.MOVING) &&
            (grid[piece_position_x][piece_position_y + 3] != GridSquare.EMPTY) &&
            (grid[piece_position_x][piece_position_y + 3] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 1][piece_position_y] == GridSquare.MOVING) &&
            (grid[piece_position_x][piece_position_y + 2] != GridSquare.EMPTY) &&
            (grid[piece_position_x][piece_position_y + 2] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 3][piece_position_y + 1] == GridSquare.MOVING) &&
            (grid[piece_position_x + 1][piece_position_y] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 1][piece_position_y] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 2][piece_position_y + 3] == GridSquare.MOVING) &&
            (grid[piece_position_x + 3][piece_position_y + 1] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 3][piece_position_y + 1] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x][piece_position_y + 2] == GridSquare.MOVING) &&
            (grid[piece_position_x + 2][piece_position_y + 3] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 2][piece_position_y + 3] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 2][piece_position_y] == GridSquare.MOVING) &&
            (grid[piece_position_x][piece_position_y + 1] != GridSquare.EMPTY) &&
            (grid[piece_position_x][piece_position_y + 1] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 3][piece_position_y + 2] == GridSquare.MOVING) &&
            (grid[piece_position_x + 2][piece_position_y] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 2][piece_position_y] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 1][piece_position_y + 3] == GridSquare.MOVING) &&
            (grid[piece_position_x + 3][piece_position_y + 2] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 3][piece_position_y + 2] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x][piece_position_y + 1] == GridSquare.MOVING) &&
            (grid[piece_position_x + 1][piece_position_y + 3] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 1][piece_position_y + 3] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 1][piece_position_y + 1] == GridSquare.MOVING) &&
            (grid[piece_position_x + 1][piece_position_y + 2] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 1][piece_position_y + 2] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 2][piece_position_y + 1] == GridSquare.MOVING) &&
            (grid[piece_position_x + 1][piece_position_y + 1] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 1][piece_position_y + 1] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 2][piece_position_y + 2] == GridSquare.MOVING) &&
            (grid[piece_position_x + 2][piece_position_y + 1] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 2][piece_position_y + 1] != GridSquare.MOVING)) checker = true;

        if ((grid[piece_position_x + 1][piece_position_y + 2] == GridSquare.MOVING) &&
            (grid[piece_position_x + 2][piece_position_y + 2] != GridSquare.EMPTY) &&
            (grid[piece_position_x + 2][piece_position_y + 2] != GridSquare.MOVING)) checker = true;

        if (!checker)
        {
            aux = piece[0][0];
            piece[0][0] = piece[3][0];
            piece[3][0] = piece[3][3];
            piece[3][3] = piece[0][3];
            piece[0][3] = aux;

            aux = piece[1][0];
            piece[1][0] = piece[3][1];
            piece[3][1] = piece[2][3];
            piece[2][3] = piece[0][2];
            piece[0][2] = aux;

            aux = piece[2][0];
            piece[2][0] = piece[3][2];
            piece[3][2] = piece[1][3];
            piece[1][3] = piece[0][1];
            piece[0][1] = aux;

            aux = piece[1][1];
            piece[1][1] = piece[2][1];
            piece[2][1] = piece[2][2];
            piece[2][2] = piece[1][2];
            piece[1][2] = aux;
        }

        for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
        {
            for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)
            {
                if (grid[i][j] == GridSquare.MOVING)
                {
                    grid[i][j] = EMPTY;
                }
            }
        }

        for (int i = piece_position_x; i < piece_position_x + 4; i++)
        {
            for (int j = piece_position_y; j < piece_position_y + 4; j++)
            {
                if (piece[i - piece_position_x][j - piece_position_y] == GridSquare.MOVING)
                {
                    grid[i][j] = MOVING;
                }
            }
        }

        return true;
    }

    return false;
}

fn bool check_detection()
{
    for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
    {
        for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)
        {
            if ((grid[i][j] == GridSquare.MOVING) && ((grid[i][j+1] == GridSquare.FULL)
                || (grid[i][j+1] == GridSquare.BLOCK))) return true;
        }
    }
    return false;
}

fn void check_completion(bool *line_to_delete_ref)
{
    int calculator = 0;

    for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
    {
        calculator = 0;
        for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)
        {
            // Count each square of the line
            if (grid[i][j] == GridSquare.FULL)
            {
                calculator++;
            }

            // Check if we completed the whole line
            if (calculator == GRID_HORIZONTAL_SIZE - 2)
            {
                *line_to_delete_ref = true;
                calculator = 0;
                // points++;

                // Mark the completed line
                for (int z = 1; z < GRID_HORIZONTAL_SIZE - 1; z++)
                {
                    grid[z][j] = FADING;
                }
            }
        }
    }
}

fn int delete_complete_lines()
{
	int lines_to_erase = 0;
    // Erase the completed line
    for (int j = GRID_VERTICAL_SIZE - 2; j >= 0; j--)
    {
        while (grid[1][j] == GridSquare.FADING)
        {
            lines_to_erase++;
            for (int i = 1; i < GRID_HORIZONTAL_SIZE - 1; i++)
            {
                grid[i][j] = GridSquare.EMPTY;
            }

            for (int j2 = j-1; j2 >= 0; j2--)
            {
                for (int i2 = 1; i2 < GRID_HORIZONTAL_SIZE - 1; i2++)
                {
                    switch (grid[i2][j2])
                    {
                        case FULL:
	                        grid[i2][j2+1] = GridSquare.FULL;
                            grid[i2][j2] = GridSquare.EMPTY;
                        case FADING:
	                        grid[i2][j2+1] = GridSquare.FADING;
                            grid[i2][j2] = GridSquare.EMPTY;
                        default:
                    }
                }
            }
        }
    }
    return lines_to_erase;
}