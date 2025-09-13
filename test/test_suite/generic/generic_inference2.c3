import std;
alias TestMap = HashMap {int, List {int}};
fn int main(String[] args)
{
    TestMap a;
    a.set(0, (List){});
    return 0;
}
