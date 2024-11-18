Some short names:
1. expl - explicit
2. sw/sn - simple widening + subexpression narrowing
3. yes - always allowed
4. explptr - explicit if pointer sized
5. ptrconv - to/from void* is fine, other cast must be explicit
6. saconv - explicit if same element size
7. explbase - explicit to base disregaring sign
8. distel - explicit to same element disregarding distinct 
9. inline - implicit if type subtype 
10. cond - implicit in cond, explicit otherwise 
11. edist - explicit to anything underlying type can convert to, if inline as underlying 
12. arve - if array or vec ptr

### Bool

1. Explicitly convert to float, int, bool vector
2. Implicitly convert to bool vector init
3. Implicitly convert to distinct bool if constant value.

### Int

1. Implicitly convert to unsigned counterpart
2. Implicitly convert to float / wider int for simple expressions.
3. Implicitly convert to same int vector init (or any?)
4. Explicitly convert to pointer if pointer sized
5. Explicitly convert to same size int vector, enum with same backing size, bitstruct with same backing size.
6. Explicitly convert to bool, any int or float.
7. Implicitly convert to bool in conditionals.
8. Implicitly convert to any distinct integer. (Same size only?)
9. Implicitly convert to any float/int/distinct float/int if constant value that fits.

### Float

1. Implicitly convert to wider float for simple expressions.
2. Implicitly convert to same float vector init (or any?)
3. Explicitly convert to bool, any int or float.
4. Explicitly convert to any distinct float. (Same size only?)
5. Implicitly convert to any float / distinct float constant value that fits.

### Non void* pointer

1. Implicitly convert to void* and `any`.
2. Implicitly convert to an interface if the pointee implements the interface.
3. Explicitly convert to pointer sized int.
4. Implicitly convert to slice if it is a pointer to a vector or array.
5. Explicitly convert to any other pointer.
6. Explicitly convert to any distinct pointer.

### void* pointer

1. Implicitly convert to a pointer sized int.
2. Implicitly convert to any other pointer.
3. Explicitly convert to any distinct pointer.
4. Implicitly convert to any distinct pointer if constant value.

### Slice

1. Implicitly convert to a pointer of the same type.
2. 


| from, to | bool   | int      | float  | pointer | subarr | vec      | bits     | distc | array    | struct | union  | any    | fault  | enum   | typeid |
|----------|--------|----------|--------|---------|--------|----------|----------|-------|----------|--------|--------|--------|--------|--------|--------|
| bool     | n/a    | expl     | expl   | no      | no     | expand   | no       | edist | no       | no     | no     | no     | no     | no     | no     |
| int      | cond   | sw/sn    | always | explptr | no     | expand   | explbase | edist | no       | no     | no     | no     | no     | edist  | no     |
| float    | cond   | expl     | sw/sn  | no      | no     | expand   | no       | edist | no       | no     | no     | no     | no     | no     | no     |
| pointer  | cond   | explptr  | no     | ptrconv | arve   | expand   | no       | edist | no       | no     | no     | yes    | expl   | no     | expl   |
| slice    | cond   | no       | no     | no      | saconv | no       | no       | edist | no?      | no     | no     | no     | no     | no     | no     |
| vec      | cond   | no       | no     | no      | no     | as base  | no       | edist | expl     | no     | no     | no     | no     | no     | no     |
| bits     | no     | explbase | no     | no      | no     | no       | no?      | edist | explbase | no     | no     | no     | no     | no     | no     |
| distc    | edist  | edist    | edist  | edist   | edist  | edist    | edist    | edist | edist    | edist  | edist  | edist  | edist  | edist  | edist  |
| array    | no     | no       | no     | no      | no     | explbase | explbase | edist | distel   | no     | no     | no     | no     | no     | no     |
| struct   | inline | inline   | inline | inline  | inline | inline   | inline   | edist | inline   | inline | inline | inline | inline | inline | inline |
| union    | no     | no       | no     | no      | no     | no       | no       | edist | no       | no     | no     | no     | no     | no     | no     |
| any      | cond   | no       | no     | expl    | no     | no       | no       | edist | no       | no     | no     | n/a    | no     | no     | no     |
| fault    | cond   | explptr  | no     | expl    | no     | no       | no       | edist | no       | no     | no     | no     | anyf   | no     | no     |
| enum     | no     | expl     | no     | no      | no     | expand   | no       | edist | no       | no     | no     | no     | no     | no     | no     |
| typeid   | cond   | no       | no     | expl    | no     | no       | no       | edist | no       | no     | no     | no     | no     | no     | n/a    |
