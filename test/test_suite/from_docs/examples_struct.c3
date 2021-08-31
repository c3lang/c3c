define Callback = func int(char c);
struct Person {}
struct Company {}
enum Status : int
{
    IDLE,
    BUSY,
    DONE,
}

struct MyData
{
    char* name;
    Callback open;
    Callback close;
    //Status status;

    // named sub-structs (x.other.value)
    struct other
    {
        int value;
        int status;   // ok, no name clash with other status
    }

    // anonymous sub-structs (x.value)
    struct
    {
        int value;
        int status;   // error, name clash with other status in MyData
    }

    // anonymous union (x.person)
    union
    {
        Person* person;
        Company* company;
    }

    // named sub-unions (x.either.this)
    union either
    {
        int this;
        bool or;
        char* that;
    }
}