struct Aa
{}
macro Aa.@a(Aa , ...) @operator(len) // #error: Methods with '@operator' may not be variadic
{}