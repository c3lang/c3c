
extern func void printf(char* message, ...);

public func void defer1() {}
public func void defer2() {}
public func void defer3() {}
public func void defer4() {}
public func void defer5() {}
public func void defer6() {}
public func void defer7() {}
public func void defer8() {}
public func void defer9() {}
public func void defer10() {}
public func void defer11() {}

public func int main(int argc)
{
	int a = 0;
    {
        defer
        {
            if (a == 1) break;
            defer1();
        }
        defer2();
    }
    defer defer3();
    while (a)
    {
	    defer defer4();
        if (argc == 1) break;
        defer defer5();
        defer6();
    }
    defer defer7();
    while (a)
    {
	    defer defer8();
        if (argc == 1) break;
        defer defer9();
        defer10();
		break;
		defer11();
    }
    return 0;
}