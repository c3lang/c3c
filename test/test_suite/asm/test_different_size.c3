// #target: macos-x64
import std;
fn int main()
{
    asm
    {
        movl $eax, [$ax+$ecx]; // #error: Index register size (16) does not match base register size (32)
    }
    return 0;
}