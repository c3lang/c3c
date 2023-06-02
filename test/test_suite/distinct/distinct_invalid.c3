fault Error
{
	ABC
}

def Foo1 = distinct Error; // #error: You cannot create a distinct type

def Foo3 = distinct void; // #error: create a distinct type from 'void'

def Foo4 = distinct typeid; // #error: create a distinct type from 'typeid'
