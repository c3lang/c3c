module antiprime;
import std::io;

fn int countDivisors(int n)
{
    if (n < 2) return 1;
    int count = 2;
    for (int i = 2; i <= n / 2; i++)
    {
        if (n % i == 0) count++;
    }
    return count;
}

fn int main()
{
    int maxDiv;
    int count;
    io::printn("The first 20 anti-primes are:");
    int n = 1;
    while (count < 20)
    {
        int d = countDivisors(n);
        if (d > maxDiv)
        {
            io::printf("%d ", n);
            maxDiv = d;
            count++;
        }
        n++;
    }
    io::printn("");
    return 0;
}