module test;


public macro retry(#function, int retries = 3)
{
    error e;
    while (1)
    {
        auto! result = #function;
        try (result) return result;
        catch (e = result);
    } while (retries-- > 0)
    return e!;
}

func void main()
{
    int! result = @retry(eventually_succeed());
}