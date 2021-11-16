
extern fn void printf(char* message, ...);

public fn void defer1() {}
public fn void defer2() {}
public fn void defer3() {}
public fn void defer4() {}
public fn void defer5() {}
public fn void defer6() {}
public fn void defer7() {}
public fn void defer8() {}
public fn void defer9() {}
public fn void defer10() {}
public fn void defer11() {}

public fn int main(int argc)
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