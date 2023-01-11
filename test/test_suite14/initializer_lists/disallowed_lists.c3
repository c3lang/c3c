fn void test()
{
    char* hello = "123";
    String a = { '1', '2', '3' };
    char[*] b = { '1', '2', '3' };
    char[3] c = { '1', '2', '3' };
    char* d = { '1', '2', '3' }; // #error: Pointers cannot be initialized using an initializer list, instead you need to take the address of an array
}