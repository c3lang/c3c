macro int factorial($n)
{
    $if ($n == 0):
        return 1;
    $else:
        return $n * @factorial($n - 1);
    $endif;
}

extern func void printf(char *fmt, ...);

func void main()
{
    int x = @factorial(12);
    printf("12! = %d\n", x);
}