module game_of_life;
import std::io, std::thread, std::math;

struct GameBoard
{
	int h, w;
	char* world;
	char* temp;
}

fn void GameBoard.show(GameBoard *board)
{
	io::printf("\e[H");
	char* current = board.world;
	for (int y = 0; y < board.h; y++)
	{
		for (int x = 0; x < board.w; x++)
		{
			io::printf(*current ? "\e[07m  \e[m" : "  ");
			current++;
		}
		io::printf("\e[E");
	}
	(void)io::stdout().flush();
}

fn void GameBoard.evolve(GameBoard *board)
{
	for (int y = 0; y < board.h; y++)
	{
		for (int x = 0; x < board.w; x++)
		{
			int n = 0;
			for (int y1 = y - 1; y1 <= y + 1; y1++)
			{
				for (int x1 = x - 1; x1 <= x + 1; x1++)
				{
					int actual_x = (x1 + board.w) % board.w;
					int actual_y = (y1 + board.h) % board.h;
					if (board.world[actual_x + actual_y * board.w]) n++;
				}
			}
			if (board.world[x + y * board.w]) n--;
			board.temp[x + y * board.w] = (char)(n == 3 || (n == 2 && board.world[x + y * board.w]));
		}
	}
	for (int i = 0; i < board.w * board.h; i++)
	{
		board.world[i] = board.temp[i];
	}
}

fn int main(String[] v)
{
	int w = 30;
	int h = 30;
	if (v.len > 1) w = v[1].to_int() ?? w;
	if (v.len > 2) h = v[2].to_int() ?? h;

	GameBoard board = { .w = w, .h = h, .world = malloc((usz)h * w), .temp = malloc((usz)h * w) };

	for (int i = h * w - 1; i >= 0; i--)
	{
		board.world[i] = rand(10) == 0 ? 1 : 0;
	}
	for (int j = 0; j < 1000; j++)
	{
		board.show();
		board.evolve();
		thread::sleep_ms(200);
	}
	return 1;
}