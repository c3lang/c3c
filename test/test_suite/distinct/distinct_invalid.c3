error Error
{}

typedef Error as distinct Foo1; // #error: You cannot create a distinct type from an error

typedef error as distinct Foo2; // #error: You cannot create a distinct type from an error union

typedef void as distinct Foo3; // #error: create a distinct type from 'void'

typedef typeid as distinct Foo4; // #error: create a distinct type from 'typeid'
