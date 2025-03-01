module foo;
import std::io;

fn void main()
{
    int i = 0;
    int j = i ^ i | i; // #error: You need to add explicit parentheses to clarify precedence.
    bool x = i == j == i; // #error: You need to add explicit parentheses to clarify precedence.
    int k = i >> j << k; // #error: You need to add explicit parentheses to clarify precedence.
    bool xk = i && j || i;
    int y = i + j + i;
    int yy = i * i / i + i - i;
}