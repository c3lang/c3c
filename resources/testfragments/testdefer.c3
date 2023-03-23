
extern fn void printf(char* message, ...);

fn void defer1() {}
fn void defer2() {}
fn void defer3() {}
fn void defer4() {}
fn void defer5() {}
fn void defer6() {}
fn void defer7() {}
fn void defer8() {}
fn void defer9() {}
fn void defer10() {}
fn void defer11() {}

fn int main(int argc)
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