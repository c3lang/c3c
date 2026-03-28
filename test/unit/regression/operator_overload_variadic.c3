struct Aa
{}
macro Aa.@a(Aa , ...) @operator(len) // #error: fokefok
{}