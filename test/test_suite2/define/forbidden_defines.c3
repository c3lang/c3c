define Abc = int[*]; // #error: Inferred array types can only be used in declarations with initializers.
define Bcd = anyerr; // #error: 'anyerr' may not be aliased.
define Efd = variant; // #error: 'variant' may not be aliased.
