module mylib::ifaces;
interface IOp {
    fn void op();
}
module mylib(<Type>);
import std::io;
import mylib::ifaces;
struct Op (IOp){
    Type data;
}
fn void Op.op(&self) @dynamic => io::printn("op");
module myapp;
import mylib;
fn void test(void* tptr){
    // this work
    IOp iop = (Op(<int>)*)tptr;
    iop.op();

    // this don't work
    iop = tptr; // #error: Casting a 'void*' to 'IOp' is not permitted
    iop.op();
}
fn void main(String[] args) {
    Op(<int>)* t = mem::new(Op(<int>), {.data = 1});
    test(&t);
}
