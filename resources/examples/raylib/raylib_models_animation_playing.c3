/*******************************************************************************************
*
*   raylib [models] example - animation playing
*
*   Example complexity rating: [★★☆☆] 2/4
*
*   Example originally created with raylib 2.5, last time updated with raylib 3.5
*
*   Example contributed by Culacant (@culacant) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2019-2025 Culacant (@culacant) and Ramon Santamaria (@raysan5)
*   converted to C3 by Christoffer Lerno
*
********************************************************************************************
*
*   NOTE: To export a model from blender, make sure it is not posed, the vertices need to be
*         in the same position as they would be in edit mode and the scale of your models is
*         set to 0. Scaling can be done from the export menu
*
********************************************************************************************/

module raylib_models;
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

	rl::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "raylib [models] example - animation playing");

	// Define the camera to look into our 3d world
	RLCamera camera = {
		.position = { .xyz = 10 },               // Camera position
		.target = { .xyz = 0 },                  // Camera looking at point
		.up = { 0, 1, 0 },                       // Camera up vector (rotation towards target)
		.fovy = 45,                              // Camera field-of-view Y
		.projection = PERSPECTIVE                // Camera mode type
	};

	RLModel model = rl::load_model("guy.iqm");                    // Load the animated model mesh and basic data
	RLTexture2D texture = rl::load_texture("guytex.png");         // Load model texture and set material
	model.materials[0].set_texture(DIFFUSE, texture);             // Set model material map texture

	RLVector3 position;                                           // Set model position to zero

	// Load animation data
	RLModelAnimation[] anims = rl::load_model_animations("guyanim.iqm");
	int anim_frame_counter = 0;

	rl::disable_cursor();                    // Catch cursor
	rl::set_target_fps(60);                  // Set our game to run at 60 frames-per-second
	//--------------------------------------------------------------------------------------

	// Main game loop
	while (!rl::window_should_close())        // Detect window close button or ESC key
	{
		// Update
		//----------------------------------------------------------------------------------
		rl::update_camera(&camera, FIRST_PERSON);

		// Play animation when spacebar is held down
		if (rl::is_key_down(SPACE))
		{
			anim_frame_counter++;
			rl::update_model_animation(model, anims[0], anim_frame_counter);
			if (anim_frame_counter >= anims[0].frame_count) anim_frame_counter = 0;
		}
		//----------------------------------------------------------------------------------

		// Draw
		//----------------------------------------------------------------------------------
		rl::@drawing()
		{
			rl::clear_background(rl::RAYWHITE);
			rl::@mode3d(camera)
			{
				rl::draw_model_ex(model, position, { 1, 0, 0 }, -90, (RLVector3)1, rl::WHITE);

				foreach (RLTransform pose : anims[0].frame_poses[anim_frame_counter][:model.bone_count])
				{
					rl::draw_cube(pose.translation, 0.2f, 0.2f, 0.2f, rl::RED);
				}

				rl::draw_grid(10, 1);         // Draw a grid
			};

			rl::draw_text("PRESS SPACE to PLAY MODEL ANIMATION", 10, 10, 20, rl::MAROON);
			rl::draw_text("(c) Guy IQM 3D model by @culacant", SCREEN_WIDTH - 200, SCREEN_HEIGHT - 20, 10, rl::GRAY);

		};
		//----------------------------------------------------------------------------------
	}

	// De-Initialization
	//--------------------------------------------------------------------------------------
	rl::unload_texture(texture);         // Unload texture
	rl::unload_model_animations(anims);  // Unload model animations data
	rl::unload_model(model);             // Unload model

	rl::close_window();                  // Close window and OpenGL context
	//--------------------------------------------------------------------------------------

	return 0;
}