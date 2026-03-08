module abc;
import std;

int abc @weak = 3;
int abc @weak = 5; // #error:  The symbol 'abc' matches multiple @weak declarations. Remove @weak from one of them to disambiguate.


fn void main()
{
	abc = 1;
}