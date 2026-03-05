module main;

fn int? may_fault() => 2;

fn void main()
{
    if (may_fault(), true); // #error: You can either assign it to a variable
}
