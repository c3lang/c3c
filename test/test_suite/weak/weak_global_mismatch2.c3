module abc;
import std;

int abc @weak = 3; // #error:  The symbol 'abc' matches multiple @weak declarations. Remove @weak from one of them to disambiguate.
int abc @weak = 5;


fn void main()
{
	abc = 1;
}