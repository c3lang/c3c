// #skip



struct FooInt = Foo(int);
func void someFunctionIntBool = someFunction(int, bool);
// const int A_CONST_INT = A_CONST(int); DOESN'T WORK
// void* standard_foo = __stdin; DOESN'T WORK


// Define <name> as <name>
define __stdin as standard_foo;
define Foo(int) as FooInt;
define void someFunction(int, bool) as someFunctionIntBool;
define A_CONST(int) as A_CONST_INT;


// <decl> aliasof <name>
void* standard_foo aliasof __stdin;
func void someFunctionIntBool aliasof someFunction(int, bool);
struct FooInt aliasof Foo(int);
A_CONST_INT aliasof A_CONST(int);


// define <name> = <name>
define standard_foo = __stdin;
define someFunctionIntBool = someFunction(int, bool);
define FooInt = Foo(int);
define A_CONST_INT = A_CONST(int);

// <decl> :: <name>
void* standard_foo :: __stdin;
func void someFunctionIntBool :: someFunction(int, bool);
struct FooInt :: Foo(int);
const int A_CONST_INT :: A_CONST(int);



struct IntArray :: VarArray(int);
func IntArray* newIntArray :: var_array::newArray(int)
const int MAX_INT_CAPACITY ::MAX_INT_CAPACITY(int);

func void foo()
{
    IntArray* array = newIntArray();
    array.append(10);
}


struct IntArray :: VarArray(int);
func IntArray* newIntArray :: var_array::newArray(int)
const int MAX_INT_CAPACITY ::MAX_INT_CAPACITY(int);

func void foo()
{
    IntArray* array = newIntArray();
    array.append(10);
}



struct IntArray :: VarArray(int);
func IntArray* newIntArray(int capacity) :: var_array::newArray(int)
const int MAX_INT_CAPACITY :: var_array::MAX_CAPACITY(int);

func void foo(int max_elements)
{
    assert(max_elements <= MAX_INT_CAPACITY)
    IntArray* array = newIntArray(max_elements);
    array.append(10);
}



define IntArray = VarArray(int);
define newIntArray = var_array::newArray;
define MAX_INT_CAPACITY = var_array::MAX_INT_CAPACITY(int);
define FOOBAR = 123;

func void foo(int max_elements)
{
    assert(max_elements <= MAX_INT_CAPACITY)
    IntArray* array = newIntArray(max_elements);
    array.append(10);
}


void* standard_foo aliasof __stdin;
func void someFunctionIntBool aliasof someFunction(int, bool);
struct FooInt aliasof Foo(int);
A_CONST_INT aliasof A_CONST(int);

func void foo(int max_elements)
{
    assert(max_elements <= MAX_INT_CAPACITY)
    IntArray* array = newIntArray(max_elements);
    array.append(10);
}

struct IntArray = VarArray(int);
func newIntArray = var_array::newArray;
const MAX_INT_CAPACITY = var_array::MAX_INT_CAPACITY(int);

func void foo(int max_elements)
{
    assert(max_elements <= MAX_INT_CAPACITY)
    IntArray* array = newIntArray(max_elements);
    array.append(10);
}
