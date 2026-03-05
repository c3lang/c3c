enum Foo
{
	COMMENT
}

fn int main()
{
	Foo state = COMMENT;
	char c;
	switch (c)
	{
		case '#':
			(void)(state == COMMENT);
			state == COMMENT; // #error: This equals check was discarded
			int a = 0;
			a == 0; // #error: This equals check was discarded
	}
	return 0;
}