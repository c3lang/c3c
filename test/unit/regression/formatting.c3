module format_test;
import std::io;

interface Decl
{
}

struct TestS (Decl, Printable)
{
    int a;
}
fn usz? TestS.to_format(&self, Formatter* formatter) @dynamic => formatter.printf("[%d]", self.a);

faultdef DERP;

fn void various_cornercases() @test
{
    TestS a = {100};
    Decl z = &a;
    test::eq(string::tformat("%s", z), "[100]");

    fault p; fault o = DERP;
    test::eq(string::tformat("%s|%s", p, o), "(empty-fault)|format_test::DERP");

    Decl y;
    test::eq(string::tformat("%s", y), "(empty-interface)");

    any xx;
    test::eq(string::tformat("%s", xx), "(empty-any)");

    int* x;
    test::eq(string::tformat("%s", x), "0x0");
}
