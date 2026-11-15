module embed_missing_file;
import std::io;

fn void missing_embed_is_file_not_found() @test
{
	char*? ptr = $embed("does_not_exist");
	assert(@catch(ptr) == io::FILE_NOT_FOUND);
	String? s = $embed("does_not_exist");
	assert(@catch(s) == io::FILE_NOT_FOUND);
}
