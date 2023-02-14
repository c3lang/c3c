fault Error
{}

typedef Foo1 = distinct Error; // #error: You cannot create a distinct type

typedef Foo3 = distinct void; // #error: create a distinct type from 'void'

typedef Foo4 = distinct typeid; // #error: create a distinct type from 'typeid'
