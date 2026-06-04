module test;
import std::io, std::time, std::math, std::collections;

List{long} stones;

struct Foo
{
	int[3] x;
}

fn int Foo.get(self, usz i) @operator([]) => self.x[i];
fn void Foo.set(&self, usz i, int z) @operator([]=) => self.x[i] = z;

fn void main()
{
  stones.push(1);
  stones.push(100);
  stones[0] *= 3;
  Foo z;
  z[$reflect(1)] += 5; // #error: You cannot index using a reflection reference
}