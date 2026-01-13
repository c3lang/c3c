module std::io @test;

struct Test (InStream)
{
	char[] data;
	int index;
}

fn char? Test.read_byte(&self) @dynamic
{
	if (self.index >= self.data.len) return io::EOF~;
	return self.data[self.index++];
}

fn usz? Test.read(&self, char[] buffer) @dynamic
{
	io::printn(*self);
	if (self.index >= self.data.len) return io::EOF~;
	int len = min((int)self.data.len - self.index, 16);
	buffer[:len] = self.data[self.index:len];
	self.index += len;
	return len;
}

fn void read_fully_stream()
{
	Test t = { .data = "Heokep o opewkfpewkfpoew kfopwekfop ewkf ewopfkewop fopkewfopk ewopfkewpo fkopew kfpoewk fej!" };
	char[] data = io::read_fully(tmem, &t)!!;
	test::eq(data, t.data);
}
