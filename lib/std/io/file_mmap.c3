module std::io::file::mmap;
import std::core::mem::vm, std::io::file;

struct FileMap
{
	VirtualMemory vm;
	usz offset;
	usz len;
}

<*
 Provides a slice of bytes to the expected mapped range discarding the extra bytes due to misaligment of offset and/or size.

 @return "Slice of the mapped range where the first byte matches the file's byte at the offset specified to File.Map()"
*>
fn char[] FileMap.bytes(&self)
{
	return self.vm.ptr[self.offset:self.len];
}

<*
  Destroys the underlyng VirtualMemory object ie. calls munmap()"
*>
fn void? FileMap.destroy(&self)
{
	self.vm.destroy()!;
}

<*
 Maps a region of an already-opened file into memory

 @param offset : "Byte offset in file, will be rounded down to page size"
 @param len : "Size in bytes to map starting from offset, will be rounded up to page size"
 @return? mem::OUT_OF_MEMORY, vm::ACCESS_DENIED, vm::RANGE_OVERFLOW, vm::INVALID_ARGS, vm::UNKNOWN_ERROR
 @return "Memory mapped region. Must be released with FileMap.destroy()"
*>
fn FileMap? File.map(&self, usz offset = 0, usz len = 0, vm::VirtualMemoryAccess access = READ, bool shared = false)
{
	if (len == 0)
	{
		usz file_size = self.seek(0, END)!;
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

	void* ptr = vm::map_file(self.fd(), map_len, map_offset, access, shared)!;
	return {{ptr, map_len, access}, page_offset, len}; 
}

