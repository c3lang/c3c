module game_of_life;

struct GameBoard
{
    int h;
    int w;
    byte* world;
    byte* temp;
}

func void GameBoard.show(GameBoard *board)
{
	printf("\033[H");
	byte* current = board.world;
	for (int y = 0; y < board.h; y++)
	{
	    for (int x = 0; x < board.w; x++)
	    {
	        printf(*current ? "\033[07m  \033[m" : "  ");
	    }
		printf("\033[E");
	}
	stdout.fflush();
}

func void GameBoard.evolve(GameBoard *board)
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
		            int actualX = (x1 + w) % w;
		            int actualY = (y1 + h) % h;
		            if (board.world[x + y * w]) n++;
		        }
		    }
		    if (board.world(x + y * w)) n--;
            board.temp[x + y * w] = (n == 3 || (n == 2 && board.world(x + y * w)));
	    }
	}
	for (int i = 0; i < w * h; i++)
	{
	    board.world[i] = board.temp[i];
	}
}

int main(int c, string[] v)
{
	int w = 0, h = 0;
	if (c > 1) w = atoi(v[1]);
	if (c > 2) h = atoi(v[2]);
	if (w <= 0) w = 30;
	if (h <= 0) h = 30;

    GameBoard board;
    board.w = w;
    board.h = h;
    board.board = malloc(h * w);
    board.temp = malloc(h * w);

    for (int i = h * w - 1; i >= 0; i--)
    {
        board.world[i] = rand() < RAND_MAX / 10 ? 1 : 0;
    }
	while (1)
	{

		board.show();
		board.evolve();
		usleep(200000);
	}
}