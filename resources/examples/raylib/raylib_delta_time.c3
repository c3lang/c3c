/*******************************************************************************************
*
*   raylib [core] example - delta time
*
*   Example complexity rating: [★☆☆☆] 1/4
*
*   Example originally created with raylib 5.5, last time updated with raylib 5.6-dev
*
*   Example contributed by Robin (@RobinsAviary) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2025 Robin (@RobinsAviary)
*   converted to C3 by Christoffer Lerno
*
********************************************************************************************/

module raylib_delta_time;
import raylib55;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
fn int main()
{
	// Initialization
	//--------------------------------------------------------------------------------------
	const int SCREEN_WIDTH = 800;
	const int SCREEN_HEIGHT = 450;

	rl::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "raylib [core] example - delta time");

   int current_fps = 60;

	// Store the position for the both of the circles
	RLVector2 delta_circle = { 0, SCREEN_HEIGHT / 3.0f };
	RLVector2 frame_circle = { 0, SCREEN_HEIGHT * (2.0f / 3.0f) };

	// The speed applied to both circles
	const float SPEED = 10.0f;
	const float CIRCLE_RADIUS = 32.0f;

	rl::set_target_fps(current_fps);

	//--------------------------------------------------------------------------------------
	// Main game loop
	while (!rl::window_should_close())    // Detect window close button or ESC key
	{
		// Update
		//----------------------------------------------------------------------------------
		// Adjust the FPS target based on the mouse wheel
		float mouse_wheel = rl::get_mouse_wheel_move();
		if (mouse_wheel)
		{
			current_fps += (int)mouse_wheel;
			if (current_fps < 0) current_fps = 0;
			rl::set_target_fps(current_fps);
		}

		// GetFrameTime() returns the time it took to draw the last frame, in seconds (usually called delta time)
		// Uses the delta time to make the circle look like it's moving at a "consistent" speed regardless of FPS

		// Multiply by 6.0 (an arbitrary value) in order to make the speed
		// visually closer to the other circle (at 60 fps), for comparison
		delta_circle.x += rl::get_frame_time() * 6 * SPEED;
		// This circle can move faster or slower visually depending on the FPS
		frame_circle.x += 0.1f * SPEED;

		// If either circle is off the screen, reset it back to the start
		if (delta_circle.x > SCREEN_WIDTH) delta_circle.x = 0;
		if (frame_circle.x > SCREEN_WIDTH) frame_circle.x = 0;

		// Reset both circles positions
		if (rl::is_key_pressed(R))
		{
			delta_circle.x = 0;
			frame_circle.x = 0;
		}
		//----------------------------------------------------------------------------------

		// Draw
		//----------------------------------------------------------------------------------
		rl::@drawing()
		{
			rl::clear_background(rl::RAYWHITE);

			// Draw both circles to the screen
			rl::draw_circle_v(delta_circle, CIRCLE_RADIUS, rl::RED);
			rl::draw_circle_v(frame_circle, CIRCLE_RADIUS, rl::BLUE);

			// Draw the help text
			// Determine what help text to show depending on the current FPS target
			ZString fps_text = null;
			if (current_fps <= 0)
			{
				fps_text = rl::text_format("FPS: unlimited (%i)", rl::get_fps());
			}
			else
			{
				fps_text = rl::text_format("FPS: %i (target: %i)", rl::get_fps(), current_fps);
			}
			rl::draw_text(fps_text, 10, 10, 20, rl::DARKGRAY);
			rl::draw_text(rl::text_format("Frame time: %02.02f ms", rl::get_frame_time()), 10, 30, 20, rl::DARKGRAY);
			rl::draw_text("Use the scroll wheel to change the fps limit, r to reset", 10, 50, 20, rl::DARKGRAY);

				// Draw the text above the circles
			rl::draw_text("FUNC: x += GetFrameTime()*speed", 10, 90, 20, rl::RED);
			rl::draw_text("FUNC: x += speed", 10, 240, 20, rl::BLUE);

		};
		//----------------------------------------------------------------------------------
	}

	// De-Initialization
	//--------------------------------------------------------------------------------------
	rl::close_window();          // Close window and OpenGL context
	//--------------------------------------------------------------------------------------

	return 0;
}
