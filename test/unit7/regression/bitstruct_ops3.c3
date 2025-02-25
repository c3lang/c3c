module bitstruct_ops_inc @test;
import std::io;

struct Data {
       bitstruct : ushort @overlap {
       		 char a : 0..7;
	}
	bitstruct : ushort @overlap {
		 ushort ab : 0..15;
       }
}

bitstruct Foo : char[10]
{
	int a : 10..15;
}

struct NewData {
       char a;
       ushort ab;
}

fn void test_inc()
{
	NewData newdata;
	newdata.a = 0x55;
	newdata.ab = 0xAABB;
	NewData newdata2 = newdata;

	newdata.a += 1;
	newdata.ab += 1;
	newdata2.a++;
	newdata2.ab++;
	assert(newdata.a == newdata2.a, "Structs: Expected %x, found %x", newdata.a, newdata2.a);
	assert(newdata.ab == newdata2.ab, "Structs: Expected %x, found %x", newdata.ab, newdata2.ab);

	Data data;
	data.a = 0x55;
	data.ab = 0xAABB;
	Data data2 = data;

	data.a += 1;
	data.ab += 1;
	data2.a++;
	data2.ab++;
	assert(data.a == data2.a, "BitStructs: Expected %x, found %x", data.a, data2.a);
	assert(data.ab == data2.ab, "BitStructs: Expected %x, found %x", data.ab, data2.ab);

	data.a -= 1;
	data.ab -= 1;
	data2.a--;
	data2.ab--;
	assert(data.a == data2.a, "BitStructs: Expected %x, found %x", data.a, data2.a);
	assert(data.ab == data2.ab, "BitStructs: Expected %x, found %x", data.ab, data2.ab);

	char a = data.a;
	assert(a == data.a++);
	assert(a != data.a);
	assert(a == --data.a);

}


fn void test_inc_array()
{
	Foo x;
	x.a = 10;
	assert(x.a == 10);
	assert(x.a++ == 10);
	assert(x.a == 11, "Value was %d", x.a);
	assert(--x.a == 10);
}

bitstruct Flags : int
{
	bool flag1;
	bool flag2;
}

fn void negate()
{
	Flags flags;
	flags = ~flags;
	assert(~0 == (int)flags);
	flags = ~(Flags){};
	assert(3 == (int)flags);
	const Flags FLAGS = {.flag1 };
	flags = ~FLAGS;
	assert(2 == (int)flags);
}