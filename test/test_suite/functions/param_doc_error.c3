<*
@param some 1      *> // #error: parameter name

fn int foo(int some){
    return some + 1;
}

<*
@param some stuff *>  // #error: did you forget enclosing the description in

fn int foo(int some){
    return some + 1;
}
