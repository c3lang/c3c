typedef Abc = int[*]; // #error: Inferred array types can only
typedef Bcd = anyerr; // #error: 'anyerr' may not be aliased.
typedef Efd = variant; // #error: 'variant' may not be aliased.
