attrdef @Aa(x) = @tag("", @pool() { return; }); // #error: Expected a constant value here, usage is: '@tag(name, value)'

fn void a() @Aa({})
{}
