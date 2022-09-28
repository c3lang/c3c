
static initialize @priority("hello") // #error: Expected an argument to '@priority'
{
}

static initialize @priority(1, 2) // #error: Too many arguments for
{
}

static initialize @priority(0) // #error: Expected an argument to '@priority'
{
}

static initialize @priority(65536) // #error: Expected an argument to '@priority'
{
}