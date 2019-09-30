module foo;

public struct Foo
{
    int i;
}

public func int Foo.test(Foo *foo)
{
    if (!foo) return 0;
    return foo.i;
}

public func void gonk()
{
    Foo.test(nil);
    printf("Bob\n");
}
func void test()
{
    int i = 0;
}

func void main()
{
	printf("Helo\n");
}