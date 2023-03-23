typedef Abc = int[*]; // #error: Inferred array types can only
typedef Bcd = anyfault; // #error: 'anyfault' may not be aliased.
typedef Efd = any; // #error: 'any' may not be aliased.
