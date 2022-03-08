optenum Error
{}

define Foo1 = distinct Error; // #error: You cannot create a distinct type from an error

define Foo3 = distinct void; // #error: create a distinct type from 'void'

define Foo4 = distinct typeid; // #error: create a distinct type from 'typeid'
