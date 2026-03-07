module std::compression::gzip_test;
import std::compression::gzip @public;

fn void gzip_fhcrc() @test
{
	char[*] data =
	x"1F8B080200000000020B179D010100FEFF008DEF02D201000000";

	Gzip gz = gzip::uncompress(mem, &data)!!;
	defer gz.free();

	assert(gz.header.flg & gzip::FHCRC);
}

fn void gzip_fextra() @test
{
	char[*] data =
	x"1F8B080400000000020B040064617461010100FEFF008DEF02D201000000";

	Gzip gz = gzip::uncompress(mem, &data)!!;
	defer gz.free();

	assert(gz.header.flg & gzip::FEXTRA);
	assert(gz.extra.len);
	test::eq(gz.extra, "data");
}

fn void gzip_fname() @test
{
	char[*] data =
	x"1F8B080800000000020B666F6F2E6300010100FEFF008DEF02D201000000";

	Gzip gz = gzip::uncompress(mem, &data)!!;
	defer gz.free();

	assert(gz.header.flg & gzip::FNAME);
	assert(gz.filename.len);
	test::eq(gz.filename, "foo.c");
}

fn void gzip_fcomment() @test
{
	char[*] data =
	x"1F8B081000000000020B68656C6C6F00010100FEFF008DEF02D201000000";

	Gzip gz = gzip::uncompress(mem, &data)!!;
	defer gz.free();

	assert(gz.header.flg & gzip::FCOMMENT);
	assert(gz.comment.len);
	test::eq(gz.comment, "hello");
}

fn void gzip_round_trip() @test
{
	char[] extra    = "extra data";
	String filename = "filename_éä.c";
	String comment  = "comment\nline";
	char[] data     = "foo bar";

	char[] gz = gzip::compress(mem, data, hcrc: true,
	extra: extra, name: filename, comment: comment)!!;
	defer allocator::free(mem, gz.ptr);

	Gzip decode = gzip::uncompress(mem, gz)!!;
	defer decode.free();

	test::eq(decode.extra, extra);
	test::eq(decode.filename, filename);
	test::eq(decode.comment, comment);
	test::eq(decode.uncompressed, data);
}
