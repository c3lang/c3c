macro continue(var) // #error: This is a reserved
{
}

fn int continue(int x) // #error: This is a reserved
{
    return 0;
}

fn int do_continue(var) {} // #error: 'var' is a keyword