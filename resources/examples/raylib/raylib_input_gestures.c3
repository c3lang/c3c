
/*******************************************************************************************
*
*   raylib [core] example - input gestures
*
*   Example complexity rating: [★★☆☆] 2/4
*
*   Example originally created with raylib 1.4, last time updated with raylib 4.2
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2016-2025 Ramon Santamaria (@raysan5)
*   converted to C3 by Christoffer Lerno
*
********************************************************************************************/

module raylib_input_gestures;
import raylib55;


const MAX_GESTURE_STRINGS = 20;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
fn int main()
{
	// Initialization
	//--------------------------------------------------------------------------------------
	const int SCREEN_WIDTH = 800;
	const int SCREEN_HEIGHT = 450;

	rl::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "raylib [core] example - input gestures");

	RLRectangle touch_area = { 220, 10, SCREEN_WIDTH - 230.0f, SCREEN_HEIGHT - 20.0f };

	int gestures_count = 0;
	ZString[MAX_GESTURE_STRINGS] gesture_strings;

	RLGesture current_gesture = NONE;
	RLGesture last_gesture = NONE;

	//SetGesturesEnabled(0b0000000000001001);   // Enable only some gestures to be detected

	rl::set_target_fps(60);               // Set our game to run at 60 frames-per-second
	//--------------------------------------------------------------------------------------

	// Main game loop
	while (!rl::window_should_close())    // Detect window close button or ESC key
	{
		// Update
		//----------------------------------------------------------------------------------
		last_gesture = current_gesture;
		current_gesture = rl::get_gesture_detected();
		RLVector2 touch_position = rl::get_touch_position(0);

		if (rl::check_collision_point_rec(touch_position, touch_area) && (current_gesture != NONE))
		{
			if (current_gesture != last_gesture)
			{
				// Reset gestures strings
            	if (gestures_count >= MAX_GESTURE_STRINGS)
            	{
            		foreach (&s : gesture_strings) *s = "";
            		gestures_count = 0;
            	}
				// Store gesture string
				switch (current_gesture)
				{
					case TAP:         gesture_strings[gestures_count++] = "GESTURE TAP";
					case DOUBLETAP:   gesture_strings[gestures_count++] = "GESTURE DOUBLETAP";
					case HOLD:        gesture_strings[gestures_count++] = "GESTURE HOLD";
					case DRAG:        gesture_strings[gestures_count++] = "GESTURE DRAG";
					case SWIPE_RIGHT: gesture_strings[gestures_count++] = "GESTURE SWIPE RIGHT";
					case SWIPE_LEFT:  gesture_strings[gestures_count++] = "GESTURE SWIPE LEFT";
					case SWIPE_UP:    gesture_strings[gestures_count++] = "GESTURE SWIPE UP";
					case SWIPE_DOWN:  gesture_strings[gestures_count++] = "GESTURE SWIPE DOWN";
					case PINCH_IN:    gesture_strings[gestures_count++] = "GESTURE PINCH IN";
					case PINCH_OUT:   gesture_strings[gestures_count++] = "GESTURE PINCH OUT";
				}


			}
		}
		//----------------------------------------------------------------------------------

		// Draw
		//----------------------------------------------------------------------------------
		rl::@drawing()
		{
			rl::clear_background(rl::RAYWHITE);

			rl::draw_rectangle_rec(touch_area, rl::GRAY);
			rl::draw_rectangle(225, 15, SCREEN_WIDTH - 240, SCREEN_HEIGHT - 30, rl::RAYWHITE);

			rl::draw_text("GESTURES TEST AREA", SCREEN_WIDTH - 270, SCREEN_HEIGHT - 40, 20, rl::GRAY.fade(0.5f));

			for (int i = 0; i < gestures_count; i++)
			{
				rl::draw_rectangle(10, 30 + 20 * i, 200, 20, rl::LIGHTGRAY.fade(i % 2 ? 0.3f : 0.5f));
				rl::draw_text(gesture_strings[i], 35, 36 + 20 * i, 10, i < gestures_count - 1 ? rl::DARKGRAY : rl::MAROON);
			}

			rl::draw_rectangle_lines(10, 29, 200, SCREEN_HEIGHT - 50, rl::GRAY);
			rl::draw_text("DETECTED GESTURES", 50, 15, 10, rl::GRAY);

			if (current_gesture != NONE) rl::draw_circle_v(touch_position, 30, rl::MAROON);

		};
		//----------------------------------------------------------------------------------
	}

	// De-Initialization
	//--------------------------------------------------------------------------------------
	rl::close_window();        // Close window and OpenGL context
	//--------------------------------------------------------------------------------------

	return 0;
}