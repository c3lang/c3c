module test;

fn void main()
{
	static int hidden @export; // #error: '@export' is not a valid local static variable attribute.
}
