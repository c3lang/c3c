import std;

fn int main()
{
    char[10] x = "0123456789";
    String y = (String)x; // #error: To convert a 'char[10]' array to a String, you first have to take its address: '(String)&x'
    return 0;
}
