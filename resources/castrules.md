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
