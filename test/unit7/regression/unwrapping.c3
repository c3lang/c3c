module unwrapping;

fn bool! get_bool()
{
    return true;
}

fn void bool_chain_unwrap() @test
{
    bool b;
    if (try v = get_bool() && b)
    {
        assert(v == true);
    }
    if (try v = get_bool() && v)
    {
        assert(v == true);
    }
}