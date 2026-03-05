import std;
const bool FOO = false;
struct Foo
{
    struct
    {
        struct // #error: members exist for this struct after processing
        {
            char d_pad0 @if(FOO);
        }
    }
}
fn void main()
{
	Foo f;
}