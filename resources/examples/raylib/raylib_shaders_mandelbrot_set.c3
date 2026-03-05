module shader_mandelbrot_set;
import raylib55, std::math;
/*******************************************************************************************
*
*   raylib [shaders] example - mandelbrot set
*
*   Example complexity rating: [★★★☆] 3/4
*
*   NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
*         OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
*
*   NOTE: Shaders used in this example are #version 330 (OpenGL 3.3)
*
*   Example originally created with raylib 5.6, last time updated with raylib 5.6
*
*   Example contributed by Jordi Santonja (@JordSant)
*   Based on previous work by Josh Colclough (@joshcol9232)
*   converted to C3 by Christoffer Lerno
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2025 Jordi Santonja (@JordSant)
*
********************************************************************************************/

const GLSL_VERSION = 330;

// A few good interesting places
const float[<3>][6] POINTS_OF_INTEREST =
{
	{ -1.76826775f, -0.00422996283f, 28435.9238f },
	{ 0.322004497f, -0.0357099883f, 56499.7266f },
	{ -0.748880744f, -0.0562955774f, 9237.59082f },
	{ -1.78385007f, -0.0156200649f, 14599.5283f },
	{ -0.0985441282f, -0.924688697f, 26259.8535f },
	{ 0.317785531f, -0.0322612226f, 29297.9258f },
};

const int SCREEN_WIDTH = 800;
const int SCREEN_HEIGHT = 450;
const float ZOOM_SPEED = 1.01f;
const float OFFSET_SPEED_MUL = 2.0f;

const float STARTING_ZOOM = 0.6f;
const float[<2>] STARTING_OFFSET = { -0.5f, 0.0f };

fn int main()
{
	// Initialization
	//--------------------------------------------------------------------------------------
	rl::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "raylib [shaders] example - mandelbrot set");

	// Load mandelbrot set shader
	// NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
	RLShader shader = rl::load_shader_from_memory(null, SHADER_CODE);

	// Create a RenderTexture2D to be used for render to texture
	RLRenderTexture2D target = rl::load_render_texture(rl::get_screen_width(), rl::get_screen_height());

	// Offset and zoom to draw the mandelbrot set at. (centered on screen and default size)
	float[<2>] offset = STARTING_OFFSET;
	float zoom = STARTING_ZOOM;
	// Depending on the zoom the mximum number of iterations must be adapted to get more detail as we zzoom in
	// The solution is not perfect, so a control has been added to increase/decrease the number of iterations with UP/DOWN keys
	int max_iterations = 333;
	float max_iterations_multiplier = 166.5f;

	// Get variable (uniform) locations on the shader to connect with the program
	// NOTE: If uniform variable could not be found in the shader, function returns -1
	int zoom_loc = shader.get_location("zoom");
	int offset_loc = shader.get_location("offset");
	int max_iterations_loc = shader.get_location("maxIterations");

	// Upload the shader uniform values!
	shader.set_value(zoom_loc, &zoom, FLOAT);
	shader.set_value(offset_loc, &offset, VEC2);
	shader.set_value(max_iterations_loc, &max_iterations, INT);

	bool show_controls = true;           // Show controls

	rl::set_target_fps(60);                   // Set our game to run at 60 frames-per-second
	//--------------------------------------------------------------------------------------

	// Main game loop
	while (!rl::window_should_close())        // Detect window close button or ESC key
	{
		// Update
		//----------------------------------------------------------------------------------
		bool update_shader = false;

		// Press [1 - 6] to reset c to a point of interest
		if (rl::is_key_pressed(ONE) || rl::is_key_pressed(TWO) || rl::is_key_pressed(THREE)
			|| rl::is_key_pressed(FOUR) || rl::is_key_pressed(FIVE) || rl::is_key_pressed(SIX))
		{
			int interest_index = 0;
			switch
			{
				case rl::is_key_pressed(ONE): interest_index = 0;
				case rl::is_key_pressed(TWO): interest_index = 1;
				case rl::is_key_pressed(THREE): interest_index = 2;
				case rl::is_key_pressed(FOUR): interest_index = 3;
				case rl::is_key_pressed(FIVE): interest_index = 4;
				case rl::is_key_pressed(SIX): interest_index = 5;
			}
			offset = POINTS_OF_INTEREST[interest_index].xy;
			zoom = POINTS_OF_INTEREST[interest_index].z;
			update_shader = true;
		}

		// If "R" is pressed, reset zoom and offset
		if (rl::is_key_pressed(R))
		{
			offset = STARTING_OFFSET;
			zoom = STARTING_ZOOM;
			update_shader = true;
		}

		switch
		{
			case rl::is_key_pressed(F1):
				show_controls = !show_controls;  // Toggle whether or not to show controls
			// Change number of max iterations with UP and DOWN keys
			// WARNING: Increasing the number of max iterations greatly impacts performance
			case rl::is_key_pressed(UP):
				max_iterations_multiplier *= 1.4f;
				update_shader = true;
			case rl::is_key_pressed(DOWN):
				max_iterations_multiplier /= 1.4f;
				update_shader = true;

		}

		// If either left or right button is pressed, zoom in/out
		if (rl::is_mouse_button_down(LEFT) || rl::is_mouse_button_down(RIGHT))
		{
			// Change zoom. If Mouse left -> zoom in. Mouse right -> zoom out
			zoom *= rl::is_mouse_button_down(LEFT) ? ZOOM_SPEED : (1.0f / ZOOM_SPEED);

			RLVector2 mouse_pos = rl::get_mouse_position();
			// Find the velocity at which to change the camera. Take the distance of the mouse
			// From the center of the screen as the direction, and adjust magnitude based on the current zoom
			RLVector2 offset_velocity = ((mouse_pos / { SCREEN_WIDTH, SCREEN_HEIGHT }) - 0.5) * OFFSET_SPEED_MUL / zoom;

			// Apply move velocity to camera
			offset += rl::get_frame_time() * offset_velocity;

			update_shader = true;
		}

		// In case a parameter has been changed, update the shader values
		if (update_shader)
		{
			// As we zoom in, increase the number of max iterations to get more detail
			// Aproximate formula, but it works-ish
			max_iterations = (int)(math::sqrt(2.0f * math::sqrt(math::abs(1.0f - math::sqrt(37.5f * zoom)))) * max_iterations_multiplier);

			// Update the shader uniform values!
			shader.set_value(zoom_loc, &zoom, FLOAT);
			shader.set_value(offset_loc, &offset, VEC2);
			shader.set_value(max_iterations_loc, &max_iterations, INT);
		}
		//----------------------------------------------------------------------------------

		// Draw
		//----------------------------------------------------------------------------------
		// Using a render texture to draw Mandelbrot set
		rl::begin_texture_mode(target);       // Enable drawing to texture
		rl::clear_background(rl::BLACK);     // Clear the render texture

		// Draw a rectangle in shader mode to be used as shader canvas
		// NOTE: Rectangle uses font white character texture coordinates,
		// So shader can not be applied here directly because input vertexTexCoord
		// Do not represent full screen coordinates (space where want to apply shader)
		rl::draw_rectangle(0, 0, rl::get_screen_width(), rl::get_screen_height(), rl::BLACK);
		rl::end_texture_mode();

		rl::begin_drawing();
		rl::clear_background(rl::BLACK);     // Clear screen background

		// Draw the saved texture and rendered mandelbrot set with shader
		// NOTE: We do not invert texture on Y, already considered inside shader
		rl::begin_shader_mode(shader);
		// WARNING: If FLAG_WINDOW_HIGHDPI is enabled, HighDPI monitor scaling should be considered
		// When rendering the RenderTexture2D to fit in the HighDPI scaled Window
		rl::draw_texture_ex(target.texture, { 0.0f, 0.0f }, 0.0f, 1.0f, rl::WHITE);
		rl::end_shader_mode();

		if (show_controls)
		{
			rl::draw_text("Press Mouse buttons right/left to zoom in/out and move", 10, 15, 10, rl::RAYWHITE);
			rl::draw_text("Press F1 to toggle these controls", 10, 30, 10, rl::RAYWHITE);
			rl::draw_text("Press [1 - 6] to change point of interest", 10, 45, 10, rl::RAYWHITE);
			rl::draw_text("Press UP | DOWN to change number of iterations", 10, 60, 10, rl::RAYWHITE);
			rl::draw_text("Press R to recenter the camera", 10, 75, 10, rl::RAYWHITE);
		}
		rl::end_drawing();
		//----------------------------------------------------------------------------------
	}

	// De-Initialization
	//--------------------------------------------------------------------------------------
	rl::unload_shader(shader);               // Unload shader
	rl::unload_render_texture(target);        // Unload render texture

	rl::close_window();                      // Close window and OpenGL context
	//--------------------------------------------------------------------------------------

	return 0;
}

const ZString SHADER_CODE = `
#version 330

#define PI 3.1415926535897932384626433832795

// Input vertex attributes (from vertex shader)
in vec2 fragTexCoord;
in vec4 fragColor;

// Output fragment color
out vec4 finalColor;

uniform vec2 offset;            // Offset of the scale
uniform float zoom;             // Zoom of the scale
uniform int maxIterations;      // Max iterations per pixel

const float max = 4.0;          // We consider infinite as 4.0: if a point reaches a distance of 4.0 it will escape to infinity
const float max2 = max*max;     // Square of max to avoid computing square root

void main()
{
	// The pixel coordinates are scaled so they are on the mandelbrot scale
	// NOTE: fragTexCoord already comes as normalized screen coordinates but offset must be normalized before scaling and zoom
	vec2 c = vec2((fragTexCoord.x - 0.5)*2.5, (fragTexCoord.y - 0.5)*1.5)/zoom;
	c.x += offset.x;
	c.y += offset.y;
	float a = 0.0;
	float b = 0.0;

	// The Mandelbrot set is a two-dimensional set defined in the complex plane on which the iteration of the function
	// Fc(z) = z^2 + c on the complex numbers c from the plane does not diverge to infinity starting at z = 0
	// Here: z = a + bi. Iterations: z -> z^2 + c = (a + bi)^2 + (c.x + c.yi) = (a^2 - b^2 + c.x) + (2ab + c.y)i

	int iter = 0;
	for (iter = 0; iter < maxIterations; ++iter)
	{
		float aa = a*a;
		float bb = b*b;
		if (aa + bb > max2)
			break;

		float twoab = 2.0*a*b;
		a = aa - bb + c.x;
		b = twoab + c.y;
	}

	if (iter >= maxIterations)
	{
		finalColor = vec4(0.0, 0.0, 0.0, 1.0);
	}
	else
	{
		float normR = float(iter%55)/55.0;
		float normG = float(iter%69)/69.0;
		float normB = float(iter%40)/40.0;

		finalColor = vec4(sin(normR*PI), sin(normG*PI), sin(normB*PI), 1.0);
	}
}
`;