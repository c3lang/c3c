module guess_number;
import std::io;
import libc;

extern fn isz getline(char** linep, usz* linecapp, CFile stream);

struct Game
{
	int answer;
	bool done;
	int guesses;
	int high;
}

fault InputResult
{
	NOT_AN_INT,
	FAILED_TO_READ,
}

int err_count = 0;

fn int! askGuess(int high)
{
	libc::printf("Guess a number between 1 and %d: ", high);
	String text = readLine()?;
	char* end = null;
	int value = (int)libc::strtol(text.ptr, &end, 10);
	if (end && end[0] >= ' ') return InputResult.NOT_AN_INT!;
	return value;
}

fn String! readLine()
{
	char* chars = tmalloc(1024)?;
	isz loaded = getline(&chars, &&(usz)1023, libc::stdin());
	if (loaded < 0) return InputResult.FAILED_TO_READ!;
	chars[loaded] = 0;
	return chars[0..(loaded - 1)];
}

fn int! askGuessMulti(int high)
 {
	while (true)
	{
		int! result = askGuess(high);
		if (catch(result) == InputResult.NOT_AN_INT)
		{
		  libc::printf("I didn't understand that.\n");
		  err_count++;
		  continue;
		}
		return result;
	}
	unreachable();
}

fn void! Game.play(Game *game)
{
	while (!game.done)
	{
		int guess = askGuessMulti(game.high)?;
		game.report(guess);
		game.update(guess);
	}
}

fn void Game.report(Game *game, int guess)
{
	String desc = {|
		if (guess < game.answer) return "too low";
		if (guess > game.answer) return "too high";
		return "the answer";
	|};
	libc::printf("%d is %.*s.\n", guess, (int)desc.len, desc.ptr);
}

fn void Game.update(Game *game, int guess)
{
	if (guess == game.answer) game.done = true;
	game.guesses++;
}

fn void! main()
{
	libc::srand((int)libc::clock());
	int high = 100;
	int answer = libc::rand() % high + 1;
	Game game = { .answer = answer, .high = high };
	game.play();
	libc::printf("Finished in %d guesses.\n", game.guesses);
	libc::printf("Total input errors: %d.\n", err_count);
}