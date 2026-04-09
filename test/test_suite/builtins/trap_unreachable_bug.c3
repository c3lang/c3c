module std;

fn int main()
{
    int128 x = (int128)!$$trap(); // #error: This expression of type 'void' can't be converted to a boolean value
    return 0;
}