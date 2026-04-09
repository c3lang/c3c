import std::thread;

fn int main()
{
	BufferedChannel{int}* c = channel::create_buffered(mem, 0); // #error: @require "size > 0" violated: 'channel size must be > 0'.
	return 0;
}
