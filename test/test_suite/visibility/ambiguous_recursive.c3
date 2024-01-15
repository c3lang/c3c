module ttf::utilities;

struct Point
{
    float x;
    float z;
    bitstruct : uint {
        uint spl : 0..0;
        uint onc : 1..1;
        uint shd : 2..2;
        uint res : 3..31;
    }
}

module ttf;
import std::io;
struct Point (Printable) {
    float x;
    float y;
    bitstruct : uint {
        uint spl : 0..0;
        uint onc : 1..1;
        uint shd : 2..2;
        uint res : 3..31;
    }
}

module test;
import ttf;
fn void main()
{
	Point x = { .z = 2 }; // #error: resolve the ambiguity
}