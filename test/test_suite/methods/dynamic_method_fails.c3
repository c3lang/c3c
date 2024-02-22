
interface Abc {}

fn void any.blob(any* x) @dynamic {} // #error: 'any' may not implement
fn void Abc.blob(Abc* y) @dynamic {} // #error: Interfaces may not implement
fn void int.blob(int z) @dynamic {}  // #error: The first parameter must always
