module guess_number;
import std::mem;
import std::io;
import libc;

extern fn void printf(char*, ...);
extern fn isize getline(char** linep, usize* linecapp, CFile stream);

struct Game
{
	int answer;
	bool done;
	int guesses;
	int high;
}

optnum InputResult
{
	NOT_AN_INT,
	FAILED_TO_READ,
}

int err_count = 0;

fn int! askGuess(int high)
{
	printf("Guess a number between 1 and %d: ", high);
	char[] text = readLine()?;
	char* end = null;
	int value = (int)libc::strtol(text.ptr, &end, 10);
	if (end && end[0] >= ' ') return InputResult.NOT_AN_INT!;
	return value;
}

fn char[]! readLine()
{
	char* chars = mem::talloc(1024)?;
	isize loaded = getline(&chars, &&(usize)1023, @libc::stdin());
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
		  printf("I didn't understand that.\n");
		  err_count++;
		  continue;
		}
		return result;
	}
	$unreachable;
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
	char[] desc = {|
		if (guess < game.answer) return "too low";
		if (guess > game.answer) return "too high";
		return "the answer";
	|};
	printf("%d is %.*s.\n", guess, (int)desc.len, desc.ptr);
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
	printf("Finished in %d guesses.\n", game.guesses);
	printf("Total input errors: %d.\n", err_count);
}