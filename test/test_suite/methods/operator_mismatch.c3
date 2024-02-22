struct Foo { int a; }
struct Bar { int a; }

fn int Foo.x(&self, int x) @operator([]) => 1;
fn int Foo.y(&self, int x) @operator(&[]) => 1;       // #error: The return type must be a pointer
fn int** Foo.y2(&self, int x) @operator(&[]) => null; // #error: There is a mismatch of the 'value' type
fn void Foo.z(&self, uint x, int a) @operator([]=) {} // #error: There is a mismatch of the 'index'

fn double Bar.x(&self, int x) @operator([]) => 0.1;
fn void Bar.y(&self, int x, float y) @operator([]=) {} // #error: There is a mismatch of the 'value'
