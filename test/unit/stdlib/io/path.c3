module std::io::path @test;


fn void! test_parent()
{
	Path p = path::new("")?;
	assert(catch(p.parent()));
	p = path::new("/")?;
	assert(catch(p.parent()));
	p = path::new("/a/b/c")?;
	assert(path::new("/a/b")? == p.parent()?);
	p = path::new("/a/b/c/")?;
	assert(path::new("/a/b")? == p.parent()?);
}