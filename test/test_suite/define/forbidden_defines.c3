define Abc = int[*]; // #error: Inferred array types can only
define Bcd = anyerr; // #error: 'anyerr' may not be aliased.
define Efd = variant; // #error: 'variant' may not be aliased.
