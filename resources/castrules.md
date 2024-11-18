## Special rules for distinct types

1. Implicit conversion will happen from constant values that can be implicitly converted to the underlying type.
2. Implicit conversion will happen to the underlying type if inline.

## Special rules for vectors

1. Implicit conversion will happen for non-vector numeric types, bools and pointers for init only.
2. Explicit conversion is available in all other cases.

## Special rules for bool conversion in conditionals

Types that may convert to a bool, will implicitly do so in a conditional.

### Bool

#### Implicit conversion
None

#### Explicit conversion

1. To float => 0.0 and 1.0
2. To int => 0 and 1 (note, there is an argument for ~0 instead)

### Int

#### Implicit conversion

1. To float if simple expression
2. To wider int if simple expression

#### Explicit conversion

1. To bool/float/int always works
2. To pointer if the int is pointer sized
3. To enum (const will be range checked)
4. To bitstruct if size matches
5. To bool

### Float

#### Implicit conversion

1. To wider float if simple expression 

#### Explicit conversion

1. To int / float
2. To bool

### Non "void\*" pointer

#### Implicit conversion

1. To void*
2. To `any`
3. To slice if it is a pointer to a vector or array
4. To an interface if the pointee implements the interface.

#### Explicit conversion

1. To any pointer
2. To any interface
3. To an int size pointer
4. To bool

### "void\*" pointer

#### Implicit conversion

1. To any pointer

#### Explicit conversion

1. To a pointer sized int.
2. To bool

### Slice

#### Implicit conversion

1. To a pointer of the same base pointer. For constant strings the ichar and char are equivalent.
2. To an array or vector of the same compile time known length if the array/vector conversion exists.

#### Explicit conversion

1. To a pointer of the same structure.
2. To a slice of the same structure.
3. To an array of the vector of the same compile time known length if the explicit array/vector conversion exists.

### Vector

#### Implicit conversion

1. To a slice of the same type *if* the vector is constant.
2. To another vector if the base type conversion is implicit
3. To an array if the conversion would work if this was an array -> array conversion.

#### Explicit conversion

1. To a slice of the same structural equivalent type *if* the vector is constant.
2. To another vector if the base type conversion is valid.
3. To an array if the conversion would work if this was an explicit array -> array conversion.

### Array

#### Implicit conversion

1. To a slice of the same type *if* the array is constant.
2. To another array of the same length but with compatible types.
3. To a vector if the conversion would work if this was an array -> array conversion.

#### Explicit conversion

1. To a slice of the same structural equivalent type *if* the array is constant.
2. To another array of the same length with structurally equivalent elements.
3. To a vector if the conversion would work if this was an explicit array -> array conversion.

### Bitstruct

#### Implicit conversion
None

#### Explicit conversion

1. To the underlying type (integer or char array).
2. To bool

### Struct

#### Implicit conversion

1. To inline member

#### Explicit conversion

None

### Union

No conversions

### "any"

#### Implicit conversion

1. To `void*`

#### Explicit conversion

1. To any interface
2. To any pointer
3. To bool

### Interface

#### Implicit conversion

1. To `void*`
2. To `any`
2. To a parent interface

#### Explicit conversion

1. To any other interface
2. To any pointer
3. To bool

### Fault

#### Implicit conversion

1. To `anyfault`

#### Explicit conversion

1. To a pointer sized int
2. To bool
3. To pointer

### "anyfault"

#### Explicit conversion

1. To a pointer sized int
2. To bool
3. To pointer
4. To a `fault`

### "typeid"

#### Explicit conversion

1. To a pointer size int
2. To bool
3. To pointer