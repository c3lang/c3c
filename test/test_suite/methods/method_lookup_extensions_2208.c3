module okok;
fn void main() {}
module foo @private;

typedef Int = int;

module foo::bar @private;
import foo @public @norecurse;

fn Int Int.foo(x) => x + 0xc3;

fn Int bar(Int x) => x * x.foo();