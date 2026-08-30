module main;
struct Data { char[<3>] member; }
fn int main ()
{
	Data* variable;
	usz idk = &(variable.member) - &(variable.member); // #error: A pointer diff cannot implicitly be cast to 'usz' (ulong), but you may use an explicit cast
	return 0;
}
