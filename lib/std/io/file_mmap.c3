module std::io::file::mmap @if(env::LIBC &&& env::POSIX);
import std::core::mem::vm, std::io::file;

struct FileMmap
{
	File file;
	VirtualMemory vm;
	usz offset;
	usz len;
}

<*
 Provides a slice of bytes to the expected mapped range discarding the extra bytes due to misaligment of offset and/or size.

 @return "Slice of the mapped range where the first byte matches the file's byte at the offset specified to File::file_mmap()"
*>
fn char[] FileMmap.bytes(&self)
{
	return self.vm.ptr[self.offset:self.len];
}

<*
  Destroys the underlying VirtualMemory object ie. calls munmap()"
*>
fn void? FileMmap.destroy(&self) @maydiscard
{
	fault err1 = @catch(self.file.close());
	fault err2 = @catch(self.vm.destroy());
	if (err1) return err1~;
	if (err2) return err2~;
}

module std::io::file @if(env::LIBC &&& env::POSIX);

<*
 Maps a region of an already-opened file into memory

 @param file : "Already opened file created on the caller scope"
 @param offset : "Byte offset in file, will be rounded down to page size"
 @param len : "Size in bytes to map starting from offset, will be rounded up to page size"
 @return? mem::OUT_OF_MEMORY, vm::ACCESS_DENIED, vm::RANGE_OVERFLOW, vm::INVALID_ARGS, vm::UNKNOWN_ERROR, io::NO_PERMISSION, io::FILE_NOT_VALID, io::WOULD_BLOCK, io::FILE_NOT_FOUND
 @return "Memory mapped region. Must be released with FileMmap.destroy(). Provided File will not be closed"
*>
fn FileMmap? mmap_file(File file, usz offset = 0, usz len = 0, VirtualMemoryAccess access = READ, bool shared = false)
{
	if (len == 0)
	{
		usz cur = file.seek(0, CURSOR)!;
		defer file.seek(cur, SET)!!;
		usz file_size = file.seek(0, END)!;
		len = file_size - offset;
	}

	// get the page size
	usz page_size = vm::aligned_alloc_size(0);

	// align the offset specified by the user (might be not aligned)
	usz page_offset = offset & (page_size - 1);
	usz map_offset = offset - page_offset;
	
	// adjust map length (both the region start and the region end might be not aligned)
	usz map_len = len + page_offset; // when region start not aligned
	map_len = vm::aligned_alloc_size(map_len); // when region end not aligned

	void* ptr = vm::mmap_file(file.fd(), map_len, map_offset, access, shared)!;

	// FileMmap does not own the supplied file
	return {{}, {ptr, map_len, access}, page_offset, len};
}

<*
 Maps a region of the given file into memory

 @param filename : "File path"
 @param mode : "File opening mode"
 @param offset : "Byte offset in file, will be rounded down to page size"
 @param len : "Size in bytes to map starting from offset, will be rounded up to page size"
 @return? mem::OUT_OF_MEMORY, vm::ACCESS_DENIED, vm::RANGE_OVERFLOW, vm::INVALID_ARGS, vm::UNKNOWN_ERROR, io::NO_PERMISSION, io::FILE_NOT_VALID, io::WOULD_BLOCK, io::FILE_NOT_FOUND
 @return "Memory mapped region. Must be released with FileMmap.destroy()"
*>
fn FileMmap? mmap_open(String filename, String mode, usz offset = 0, usz len = 0, VirtualMemoryAccess access = READ, bool shared = false)
{
	File file = open(filename, mode)!;
	defer catch (void)file.close();
	FileMmap mm = mmap_file(file, offset, len, access, shared)!;

	// FileMmap owns the file and it will close it on destroy()
	mm.file = file;

	return mm;
}

