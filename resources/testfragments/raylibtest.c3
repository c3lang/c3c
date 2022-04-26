module foo;
extern fn void printf(char*, ...);

enum Foo : int (int offset, char* extra_name, double x)
{
	BAZ(12, "hello", 3.0),
	BOO(33, "oekfe", 4.0) = 3,
}

fn void main()
{
	int screenWidth = 800;
    int screenHeight = 450;

    raylib::init_window(screenWidth, screenHeight, "raylib [core] example - keyboard input");

    Vector2 ballPosition = { (float)screenWidth/2, (float)screenHeight/2 };

    raylib::set_target_fps(60);               // Set our game to run at 60 frames-per-second
    //--------------------------------------------------------------------------------------
	foo2::tester(screenHeight);
    // Main game loop
    while (!raylib::window_should_close())    // Detect window close button or ESC key
    {
        // Update
        //----------------------------------------------------------------------------------
        if (raylib::is_key_down(KeyboardKey.RIGHT)) ballPosition.x += 2.0f;
        if (raylib::is_key_down(KeyboardKey.LEFT)) ballPosition.x -= 2.0f;
        if (raylib::is_key_down(KeyboardKey.UP)) ballPosition.y -= 2.0f;
        if (raylib::is_key_down(KeyboardKey.DOWN)) ballPosition.y += 2.0f;
        //----------------------------------------------------------------------------------

        // Draw
        //----------------------------------------------------------------------------------
        raylib::begin_drawing();

            raylib::clear_background(raylib::RAYWHITE);

            raylib::draw_text("move the ball with arrow keys", 10, 10, 20, raylib::DARKGRAY);

            raylib::draw_circle_v(ballPosition, 50, raylib::MAROON);

        raylib::end_drawing();
        //----------------------------------------------------------------------------------
    }

    // De-Initialization
    //--------------------------------------------------------------------------------------
    raylib::close_window();        // Close window and OpenGL context
    //--------------------------------------------------------------------------------------
}
/*
struct Game
{
	int answer;
	bool done;
	int guesses;
    int high;
}

int err_count = 0;


import "core:bufio"
import "core:fmt"
import "core:io"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:time"
import "core:math/rand"

Game :: struct {
	answer: int,
	done: bool,
	guesses: int,
	high: int,
}

int err_count = 0;
*/
/*
fn int ask_guess(int high)
{
	libc::printf("Guess a number between 1 and %d: ", high);

}
ask_guess :: proc(high: int) -> (result: int, ok: bool) {
	fmt.printf("Guess a number between 1 and %d: ", high)
	if text, ok := read_line(); ok {
		defer mem.delete(text)
		return strconv.parse_int(s = text, base = 10)
	}
	return
}

ask_guess_multi :: proc(high: int) -> int {
	for {
		if result, ok := ask_guess(high); ok {
			return result
		}
		fmt.println("I didn't understand")
		err_count += 1
	}
}

pick_answer :: proc(high: int, r: ^rand.Rand) -> int {
	return rand.int_max(high, r) + 1
}

play :: proc(game: ^Game) {
	for !game.done {
		guess := ask_guess_multi(game.high)
		report(game^, guess)
		game^ = update(game^, guess)
	}
}

read_line :: proc() -> (result: string, ok: bool) {
	// See also:
	// - https://github.com/odin-lang/Odin/issues/1214
	// - https://p.teknik.io/Raw/IT996
	s := os.stream_from_handle(os.stdin)
	r: bufio.Reader
	bufio.reader_init(&r, io.Reader{s})
	defer bufio.reader_destroy(&r)
	if line, err := bufio.reader_read_string(&r, '\n'); err == .None {
		return line[:len(line) - 1], true
	}
	return
}

report :: proc(game: Game, guess: int) {
	// game.done = true
	// fmt.println(&game)
	description := (
		"too low" if guess < game.answer else
		"too high" if guess > game.answer else
		"the answer!"
	)
	fmt.println(guess, "is", description)
}

update :: proc(game: Game, guess: int) -> (next: Game) {
	next = game
	next.done = guess == game.answer
	next.guesses += 1
	return
}

main :: proc() {
	high :: 100
	r := rand.create(transmute(u64)time.now())
	// Or use nil for default random.
	answer := pick_answer(high, &r)
	game := Game {answer = answer, done = false, guesses = 0, high = high}
	play(&game)
	fmt.println("Finished in", game.guesses, "guesses");
	fmt.println("Total input errors:", err_count)
}
*/

module foo2;
fn void tester(int x)
{
	assert(x > 1000, "Shit!");
}
