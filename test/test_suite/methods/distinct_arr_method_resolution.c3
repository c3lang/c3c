module abc;
distinct Foo = int;
distinct Bar = int;
fn void Foo[2].foo(&self) {}
// Same with:
macro Bar[2].bar(&self) {}

fn void main()
{}