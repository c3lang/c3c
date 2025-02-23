
fn void init1() @init("hello") // #error: Expected an integer value
{
}

fn void init2() @init(1, 2) // #error: Too many arguments for
{
}

fn void init3() @init(0) // #error: The priority must be a value
{
}

fn void init4() @init(65536) // #error: The priority must be a value
{
}