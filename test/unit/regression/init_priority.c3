module init_priority;

int[3] order;
int count;

fn void late() @init(3000) { order[count++] = 3000; }
fn void early() @init(2000) { order[count++] = 2000; }
fn void last() @init { order[count++] = 65535; }

fn void init_runs_by_priority() @test
{
	assert(count == 3);
	assert(order[0] == 2000 && order[1] == 3000 && order[2] == 65535);
}
