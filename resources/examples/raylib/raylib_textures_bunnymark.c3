/*******************************************************************************************
*
*   raylib [textures] example - bunnymark
*
*   Example complexity rating: [★★★☆] 3/4
*
*   Example originally created with raylib 1.6, last time updated with raylib 2.5
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
*   converted to C3 by Christoffer Lerno
*
********************************************************************************************/
module raylib_audio_module;
import raylib55;

const int MAX_BUNNIES       = 50000;    // 50K bunnies limit

// This is the maximum amount of elements (quads) per batch
// NOTE: This value is defined in [rlgl] module and can be changed there
const int MAX_BATCH_ELEMENTS = 8192;

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------
struct Bunny
{
	RLVector2 position;
	RLVector2 speed;
	RLColor color;
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

	rl::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "raylib [textures] example - bunnymark");

	// Load bunny texture
	RLTexture2D tex_bunny = rl::load_texture("raybunny.png");

	Bunny[] bunnies = mem::alloc_array(Bunny, MAX_BUNNIES);   // Bunnies array

	int bunnies_count = 0;           // Bunnies counter

	rl::set_target_fps(60);               // Set our game to run at 60 frames-per-second
	//--------------------------------------------------------------------------------------

	// Main game loop
	while (!rl::window_should_close())    // Detect window close button or ESC key
	{
		// Update
		//----------------------------------------------------------------------------------
		if (rl::is_mouse_button_down(LEFT))
		{
			// Create more bunnies
			for (int i = 0; i < 100; i++)
			{
				if (bunnies_count < MAX_BUNNIES)
				{
					bunnies[bunnies_count] = {
						.position = rl::get_mouse_position(),
						.speed = { rl::get_random_value(-250, 250) / 60.0f, rl::get_random_value(-250, 250) / 60.0f },
						.color = { (char)rl::get_random_value(50, 240), (char)rl::get_random_value(80, 240), (char)rl::get_random_value(100, 240), 255 }
					};
					bunnies_count++;
				}
			}
		}

		// Update bunnies
		foreach (&bunny : bunnies[:bunnies_count])
		{
			bunny.position += bunny.speed;
			if (bunny.position.x + tex_bunny.width / 2.0f > rl::get_screen_width()
				|| bunny.position.x + tex_bunny.width / 2.0f < 0)
			{
				bunny.speed.x *= -1;
			}
			if (bunny.position.y + tex_bunny.height / 2.0f > rl::get_screen_height()
				|| bunny.position.y + tex_bunny.height / 2.0f - 40 < 0)
			{
				bunny.speed.y *= -1;
			}
		}
		//----------------------------------------------------------------------------------

		// Draw
		//----------------------------------------------------------------------------------
		rl::@drawing()
		{
			rl::clear_background(rl::RAYWHITE);

			foreach (&bunny : bunnies[:bunnies_count])
			{
				// NOTE: When internal batch buffer limit is reached (MAX_BATCH_ELEMENTS),
				// a draw call is launched and buffer starts being filled again;
				// before issuing a draw call, updated vertex data from internal CPU buffer is send to GPU...
				// Process of sending data is costly and it could happen that GPU data has not been completely
				// processed for drawing while new data is tried to be sent (updating current in-use buffers)
				// it could generates a stall and consequently a frame drop, limiting the number of drawn bunnies
				rl::draw_texture(tex_bunny, (int)bunny.position.x, (int)bunny.position.y, bunny.color);
			}

			rl::draw_rectangle(0, 0, SCREEN_WIDTH, 40, rl::BLACK);
			rl::draw_text(rl::text_format("bunnies: %i", bunnies_count), 120, 10, 20, rl::GREEN);
			rl::draw_text(rl::text_format("batched draw calls: %i", 1 + bunnies_count / MAX_BATCH_ELEMENTS), 320, 10, 20, rl::MAROON);

			rl::draw_fps(10, 10);

		};
		//----------------------------------------------------------------------------------
	}

	// De-Initialization
	//--------------------------------------------------------------------------------------
	free(bunnies);              // Unload bunnies data array

	rl::unload_texture(tex_bunny);    // Unload bunny texture

	rl::close_window();              // Close window and OpenGL context
	//--------------------------------------------------------------------------------------

	return 0;
}