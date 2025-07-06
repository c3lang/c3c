import std;
int y;
enum Test
{
	ABC,
	DEF
}
fn int main()
{

	$assert (void*[<1>]){ &y } == { &y };
	$assert (void*[<1>]){ &y } != { null };
	$assert (float[<1>]){ 1.0 } == { 1.0 };
	$assert (float[<1>]){ 1.0 } != { 1.2 };
	$assert (Test[<2>]){ ABC, DEF } == { ABC, DEF };
	$assert (Test[<2>]){ ABC, DEF } != { DEF, ABC };
	$assert (int[<3>]){0, 1, 2} == (int[<3>]){0, 1, 2} : "Vectors must be equivalent";
	$assert !((void*[<1>]){ &y } != { &y });
	$assert !((void*[<1>]){ &y } == { null });
	$assert !((float[<1>]){ 1.0 } != { 1.0 });
	$assert !((float[<1>]){ 1.0 } == { 1.2 });
	$assert !((Test[<2>]){ ABC, DEF } != { ABC, DEF });
	$assert !((Test[<2>]){ ABC, DEF } == { DEF, ABC });
	$assert !((int[<3>]){0, 1, 2} != (int[<3>]){0, 1, 2}) : "Vectors must be equivalent";
	return 0;
}