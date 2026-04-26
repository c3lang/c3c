/*******************************************************************************************
*
*   raylib [models] example - basic voxel
*
*   Example complexity rating: [★★☆☆] 2/4
*
*   Example originally created with raylib 5.5, last time updated with raylib 5.5
*
*   Example contributed by Tim Little (@timlittle) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2025 Tim Little (@timlittle)
*   converted to C3 by Christoffer Lerno
*
********************************************************************************************/
module raylib_basic_voxel;
import raylib6, std::math;

const WORLD_SIZE = 8;   // Size of our voxel world (8x8x8 cubes)

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
fn int main()
{
	// Initialization
	//--------------------------------------------------------------------------------------
	const int SCREEN_WIDTH = 800;
	const int SCREEN_HEIGHT = 450;

	rl::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "raylib [models] example - basic voxel");

	rl::disable_cursor();                    // Lock mouse to window center

	// Define the camera to look into our 3d world (first person)
	RLCamera3D camera = {
		.position = { -2.0f, 0.0f, -2.0f },  // Camera position at ground level
		.target = { 0.0f, 0.0f, 0.0f },      // Camera looking at point
		.up = { 0.0f, 1.0f, 0.0f },          // Camera up vector
		.fovy = 45.0f,                       // Camera field-of-view Y
		.projection = PERSPECTIVE            // Camera projection type
	};

	// Create a cube model
	RLMesh cube_mesh = rl::gen_mesh_cube(1.0f, 1.0f, 1.0f);      // Create a unit cube mesh
	RLModel cube_model = rl::load_model_from_mesh(cube_mesh);    // Convert mesh to a model
	cube_model.materials[0].maps[RLMaterialMapIndex.DIFFUSE].color = rl::BEIGE;

	// Initialize voxel world - fill with voxels
	bool[WORLD_SIZE][WORLD_SIZE][WORLD_SIZE] voxels;
	mem::set(&voxels, 1, WORLD_SIZE * WORLD_SIZE * WORLD_SIZE);

	rl::set_target_fps(60);
	//--------------------------------------------------------------------------------------

	// Main game loop
	while (!rl::window_should_close())    // Detect window close button or ESC key
	{
		// Update
		//----------------------------------------------------------------------------------
		rl::update_camera(&camera, FIRST_PERSON);

		// Handle voxel removal with mouse click
		if (rl::is_mouse_button_pressed(LEFT))
		{
			// Cast a ray from the screen center (where crosshair would be)
			Vec2 screen_center = { SCREEN_WIDTH / 2.0f, SCREEN_HEIGHT / 2.0f };
			RLRay ray = rl::get_screen_to_world_ray(screen_center, camera);

			// Check ray collision with all voxels
			bool voxel_removed = false;
			for OUTER: (int x = 0; x < WORLD_SIZE; x++)
			{
				for (int y = 0; y < WORLD_SIZE; y++)
				{
					for (int z = 0; z < WORLD_SIZE; z++)
					{
						if (!voxels[x][y][z]) continue; // Skip empty voxels

						// Build a bounding box for this voxel
						Vec3 position = { x, y, z };
						RLBoundingBox box = { position - 0.5, position + 0.5 };

						// Check ray-box collision
						RLRayCollision collision = ray.get_collision_box(box);
						if (collision.hit)
						{
							voxels[x][y][z] = false;    // Remove this voxel
							break OUTER;
						}
					}
				}
			}
		}
		//----------------------------------------------------------------------------------

		// Draw
		//----------------------------------------------------------------------------------
		rl::@drawing()
        {
        	rl::clear_background(rl::RAYWHITE);
        	rl::@mode3d(camera)
        	{
        		rl::draw_grid(10, 1);         // Draw a grid
				// Draw all voxels
				for (int x = 0; x < WORLD_SIZE; x++)
				{
					for (int y = 0; y < WORLD_SIZE; y++)
					{
						for (int z = 0; z < WORLD_SIZE; z++)
						{
							if (!voxels[x][y][z]) continue;

							Vec3 position = { x, y, z };
							rl::draw_model(cube_model, position, 1.0f, rl::BEIGE);
							rl::draw_cube_wires(position, 1.0f, 1.0f, 1.0f, rl::BLACK);
						}
					}
				}
			};
        	rl::draw_text("Left-click a voxel to remove it!", 10, 10, 20, rl::DARKGRAY);
        	rl::draw_text("WASD to move, mouse to look around", 10, 35, 10, rl::GRAY);

        };
		//----------------------------------------------------------------------------------
	}

	// De-Initialization
	//--------------------------------------------------------------------------------------
	rl::unload_model(cube_model);
	rl::close_window();
	//--------------------------------------------------------------------------------------

	return 0;
}