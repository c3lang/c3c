
define int = int; // #error: 'int' is the name of a built-in type and can't be used as an alias.
define main = int; // #error: 'main' is reserved and cannot be used as an alias.

define hello = int; // #error: A type alias must start with an uppercase letter and contain at least one lower case letter
define hello = Foo; // #error: A type alias must start with an uppercase letter and contain at least one lower case letter

define HELLO = int; // #error: A type alias must start with an uppercase letter and contain at least one lower case letter
define HELLO = Foo; // #error: A type alias must start with an uppercase letter and contain at least one lower case letter