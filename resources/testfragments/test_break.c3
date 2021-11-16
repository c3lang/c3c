module foo;

extern fn void printf(char *hello, ...);

error MyError;
error YourError;
fn void main()
{
    int! i = 1;
    catch (i)
    {
        printf("Caught\n");
        return;
    }
    i = MyError!;
    catch FOO: (i)
    {
        case YourError:
            printf("YourError\n");
        case MyError:
            printf("MyError\n");
            next FOO : YourError;
        default:
            printf("Any error\n");
    }
    do
    {
        printf("Test\n");
    }
}