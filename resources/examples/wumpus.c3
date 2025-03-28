/*
 * The classic "Wumpus" from Ahl's More Basic Computer Games
 */
module wumpus;
import std::io, std::math::random;

struct Room
{
	bool bats;
	bool pit;
}

int[3][LOCATIONS] rooms = {
	{  2,  5,  8 },
	{  1,  3, 10 },
	{  2,  4, 12 },
	{  3,  5, 14 },
	{  1,  4,  6 },
	{  5,  7, 15 },
	{  6,  8, 17 },
	{  1,  7,  9 },
	{  8, 10, 18 },
	{  2,  9, 11 },
	{ 10, 12, 19 },
	{  3, 11, 13 },
	{ 12, 14, 20 },
	{  4, 13, 15 },
	{  6, 14, 16 },
	{ 15, 17, 20 },
	{  7, 16,  8 },
	{  9, 17, 19 },
	{ 11, 18, 20 },
	{ 13, 16, 19 }
};

enum GameState
{
	RUNNING,
	WON,
	LOST
}

const LOCATIONS = 20;
const PITS = 2;
const BATS = 2;
Room[LOCATIONS] dungeon;
GameState state;
int arrows;
int location;
int wumpus;

fn void init_game()
{
	state = RUNNING;
	dungeon[..] = {};

	for (int i = 0; i < BATS;)
	{
		Room* room = &dungeon[rand(LOCATIONS)];
		if (room.bats) continue; // No pits yet!
		room.bats = true;
		i++;
	}

	for (int i = 0; i < PITS;)
	{
		Room* room = &dungeon[rand(LOCATIONS)];
		if (room.bats || room.pit) continue;
		room.pit = true;
		i++;
	}

	int room_idx;
	Room r;
	do
	{
		r = dungeon[room_idx = rand(LOCATIONS)];
	} while (r.bats || r.pit);

	location = room_idx;
	do
	{
		r = dungeon[room_idx = rand(LOCATIONS)];
	} while (r.bats || r.pit || room_idx == location);
	wumpus = room_idx;
	arrows = 5;
}

fn void print_location()
{
	int[3] exits = rooms[location];
	foreach (exit : exits)
    {
        int idx = exit - 1;
        Room r = dungeon[idx];
        switch
        {
            case r.bats: io::printn("Bats nearby!");
            case r.pit:  io::printn("I feel a draft!");
            case idx == wumpus: io::printn("I smell a WUMPUS!");
		}
	}
	io::printfn("You are in room %d.", location + 1);
	io::printfn("Tunnels lead to %d, %d, %d", ...exits);
	io::printn();
}

fn bool check_arrow(int arrow)
{
	switch (arrow)
	{
		case wumpus:
			io::printn();
			io::printn("Aha! You got the WUMPUS!");
			state = WON;
			return true;
		case location:
			io::printn();
			io::printn("OUCH! Arrow got you!");
			state = LOST;
			return true;
		default:
			return false;
	}
}

fn int input_move()
{
	while (true)
	{
		io::print("Where to ");
		int number = io::treadline().to_int() ?? 0;
		if (number < 1 || number > 20) continue;
		if (number == location + 1) return location;
		foreach (exit : rooms[location])
		{
			if (exit == number) return number - 1;
		}
		io::printn("Not possible");
	}
}

fn void move_wumpus()
{
	int k = rand(4);
	if (k < 3)
	{
		wumpus = rooms[wumpus][k] - 1;
	}
	if (wumpus == location)
	{
		state = LOST;
		io::printn("Tsk tsk tsk - WUMPUS got you!");
	}
}

fn void move()
{
	int new_room = input_move();
	location = new_room;
	while (true)
	{
		Room r = dungeon[location];
		switch
		{
			case location == wumpus:
				io::printn("... Oops! Bumped a WUMPUS!");
				move_wumpus();
				if (state != RUNNING) return;
				continue;
			case r.pit:
				io::printn("Yyyiiiieeeee . . . fell in a pit!");
				state = LOST;
				return;
			case r.bats:
				io::printn("Zap--Super bat snatch! Elsewhereville for you!");
				location = new_room = rand(20);
				continue;
			default:
				return;
		}
	}
}

fn void shoot()
{
	int number;
	while (true)
	{
		io::print("No. of rooms (1-5) ");
		number = io::treadline().to_int() ?? 0;
		if (number < 1 || number > 5) continue;
		break;
	}
	int[5] path;
	for (int i = 0; i < number; i++)
	{
		io::print("Room # ");
		int room_number;
		do
		{
			room_number = io::treadline().to_int() ?? 0;

		} while (room_number < 1 || room_number > 20);
		path[i] = room_number;
		if (i > 1 && path[i - 2] == path[i])
		{
			io::printn("Arrows aren't that crooked - try another room.");
			i--;
			continue;
		}
	}

	int arrow = location;
	for PATH: (int i = 0; i < number; i++)
	{
		int new_room = path[i];
		foreach (exit : rooms[arrow])
		{
			if (new_room == exit)
			{
				arrow = new_room - 1;
				if (check_arrow(arrow)) return;
				continue PATH;
			}
		}
		arrow = rooms[arrow][rand(3)] - 1;
		if (check_arrow(arrow)) return;
	}
	io::printn("Missed!");
	move_wumpus();
	arrows--;
	if (arrows == 0)
	{
		io::printn("You're out of arrows.");
		state = LOST;
	}
}
fn void play_game()
{
	init_game();
	io::printfn("%24sHUNT THE WUMPUS", "");
	io::printn();
	while (state == RUNNING) @pool()
	{
		print_location();
    	while WHILE: (true)
    	{
    		io::printn("Shoot or move (S-M)");
    		switch (io::treadline()!!)
    		{
    			case "s":
    			case "S":
    				shoot();
    				break WHILE;
    			case "m":
    			case "M":
    				move();
    				break WHILE;
    		}
    	}
	};
	switch (state)
	{
		case WON:
			io::printn("Hee hee gee - The WUMPUS'll get you next time!");
		case LOST:
			io::printn("Ha ha ha - you lose!");
		default:
			unreachable();
	}
}

fn void main()
{
	play_game();
}