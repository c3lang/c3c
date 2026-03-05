module instring_comments;

import std;

// this test proves the compiler can handle in string comments reliabely.

fn void test_len() @test {
    
    String foo = "Hello, // world!";
    String bar = "Hello, /* world */!";
    
    assert(foo.len == 16);
    assert(bar.len == 19);

}
