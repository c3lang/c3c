fault Error
{
	ABC
}

distinct Foo1 = Error; // #error: You cannot create a distinct type

distinct Foo3 = void; // #error: create a distinct type from 'void'

distinct Foo4 = typeid; // #error: create a distinct type from 'typeid'
