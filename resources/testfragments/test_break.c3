module foo;

extern func void printf(char *hello, ...);

error MyError;
error YourError;
func void main()
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