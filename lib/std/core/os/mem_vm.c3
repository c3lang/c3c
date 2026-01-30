<*
 The VM module holds code for working with virtual memory on supported platforms (currently Win32 and Posix)
*>
module std::core::mem::vm;
import std::io, std::os::win32, std::os::posix, libc;

<*
 VirtualMemory is an abstraction for working with an allocated virtual memory area. It will invoke vm:: functions
 but will perform more checks and track its size (required to unmap the memory on Posix)
*>
struct VirtualMemory
{
	void* ptr;
	usz size;
	VirtualMemoryAccess default_access;
}

faultdef RANGE_OVERFLOW, UNKNOWN_ERROR, ACCESS_DENIED, UNMAPPED_ACCESS, UNALIGNED_ADDRESS, RELEASE_FAILED, UPDATE_FAILED, INVALID_ARGS;

enum VirtualMemoryAccess
{
	PROTECTED,
	READ,
	WRITE,
	READWRITE,
	EXEC,
	EXECREAD,
	EXECWRITE,
	ANY
}

fn usz aligned_alloc_size(usz size)
{
	$if env::WIN32:
		return size > 0 ? mem::aligned_offset(size, win32::allocation_granularity()) : win32::allocation_granularity();
	$else
		return size > 0 ? mem::aligned_offset(size, mem::os_pagesize()) : mem::os_pagesize();
	$endif
}

<*
 Allocate virtual memory, size is rounded up to platform granularity (Win32) / page size (Posix).

 @param size : "The size of the memory to allocate, will be rounded up"
 @param access : "The initial access permissions."
 @return? mem::OUT_OF_MEMORY, RANGE_OVERFLOW, UNKNOWN_ERROR, ACCESS_DENIED, INVALID_ARGS
 @return "Pointer to the allocated memory, page aligned"
*>
fn void*? alloc(usz size, VirtualMemoryAccess access)
{
	$switch:
		$case env::POSIX:
			void* ptr = posix::mmap(null, aligned_alloc_size(size), access.to_posix(), posix::MAP_PRIVATE | posix::MAP_ANONYMOUS, -1, 0);
			if (ptr != posix::MAP_FAILED) return ptr;
			switch (libc::errno())
			{
				case errno::ENOMEM:    return mem::OUT_OF_MEMORY~;
				case errno::EOVERFLOW: return RANGE_OVERFLOW~;
				case errno::EPERM:     return ACCESS_DENIED~;
				case errno::EINVAL:    return INVALID_ARGS~;
				default:               return UNKNOWN_ERROR~;
			}
		$case env::WIN32:
			void* ptr = win32::virtualAlloc(null, aligned_alloc_size(size), MEM_RESERVE, access.to_win32());
			if (ptr) return ptr;
			switch (win32::getLastError())
			{
				case win32::ERROR_NOT_ENOUGH_MEMORY:
				case win32::ERROR_COMMITMENT_LIMIT:  return mem::OUT_OF_MEMORY~;
				default:                             return UNKNOWN_ERROR~;
			}
		$default:
			unsupported("Virtual alloc only available on Win32 and Posix");
	$endswitch
}

<*
 Release memory allocated with "alloc".

 @param [&inout] ptr : "Pointer to page to release, should be allocated using vm::alloc"
 @param size : "The size of the allocated pointer"
 @require mem::ptr_is_page_aligned(ptr) : "The pointer should be page aligned"
*>
fn void? release(void* ptr, usz size)
{
	$switch:
		$case env::POSIX:
			if (posix::munmap(ptr, aligned_alloc_size(size)))
			{
				switch (libc::errno())
				{
					case errno::EINVAL: return INVALID_ARGS~;    // Not a valid mapping or size
					case errno::ENOMEM: return UNMAPPED_ACCESS~; // Address not mapped
					default:            return RELEASE_FAILED~;
				}
			}
		$case env::WIN32:
			if (win32::virtualFree(ptr, 0, MEM_RELEASE)) return;
			switch (win32::getLastError())
			{
				case win32::ERROR_INVALID_ADDRESS:   return INVALID_ARGS~;
				case win32::ERROR_NOT_ENOUGH_MEMORY: return mem::OUT_OF_MEMORY~;
				default:                             return RELEASE_FAILED~;
			}
		$default:
			unsupported("Virtual free only available on Win32 and Posix");
	$endswitch
}

<*
 Change the access protection of a region in memory. The region must be page aligned.

 @param [&inout] ptr : "Pointer to page to update, must be page aligned"
 @param len : "To what len to update, must be page aligned"
 @param access : "The new access"
 @require mem::ptr_is_page_aligned(ptr) : "The pointer should be page aligned"
 @require mem::ptr_is_page_aligned(ptr + len) : "The length must be page aligned"
 @return? ACCESS_DENIED, UNALIGNED_ADDRESS, RANGE_OVERFLOW, UPDATE_FAILED, UNMAPPED_ACCESS, INVALID_ARGS
*>
fn void? protect(void* ptr, usz len, VirtualMemoryAccess access)
{
	$switch:
		$case env::POSIX:
			if (!posix::mprotect(ptr, len, access.to_posix())) return;
			switch (libc::errno())
			{
				case errno::EACCES:    return ACCESS_DENIED~;
				case errno::EINVAL:    return UNALIGNED_ADDRESS~;
				case errno::EOVERFLOW: return RANGE_OVERFLOW~;
				case errno::ENOMEM:    return UNMAPPED_ACCESS~;
				default:               return UPDATE_FAILED~;
			}
		$case env::WIN32:
			Win32_Protect old;
			if (win32::virtualProtect(ptr, len, access.to_win32(), &old)) return;
			switch (win32::getLastError())
			{
				case win32::ERROR_INVALID_ADDRESS:   return UNALIGNED_ADDRESS~;
				case win32::ERROR_ACCESS_DENIED:     return ACCESS_DENIED~;
				case win32::ERROR_INVALID_PARAMETER: return INVALID_ARGS~;
				default:                             return UPDATE_FAILED~;
			}
		$default:
			unsupported("'virtual_protect' is only available on Win32 and Posix.");
	$endswitch
}

<*
 Makes a region of memory available that was previously retrieved using 'alloc'. This is necessary on Win32,
 but optional on Posix.

 @param [&inout] ptr : "Pointer to page to update, must be page aligned"
 @param len : "To what len to commit, must be page aligned"
 @require mem::ptr_is_page_aligned(ptr) : "The pointer should be page aligned"
 @require mem::ptr_is_page_aligned(ptr + len) : "The length must be page aligned"
 @return? UNKNOWN_ERROR, mem::OUT_OF_MEMORY, ACCESS_DENIED, UNALIGNED_ADDRESS, RANGE_OVERFLOW, UPDATE_FAILED, UNMAPPED_ACCESS, INVALID_ARGS
*>
fn void? commit(void* ptr, usz len, VirtualMemoryAccess access = READWRITE)
{
	$switch:
		$case env::POSIX:
			return protect(ptr, len, READWRITE) @inline;
		$case env::WIN32:
			void* result = win32::virtualAlloc(ptr, len, MEM_COMMIT, access.to_win32());
			if (result) return;
			switch (win32::getLastError())
			{
				case win32::ERROR_INVALID_ADDRESS:   return UNALIGNED_ADDRESS~;
				case win32::ERROR_ACCESS_DENIED:     return ACCESS_DENIED~;
				case win32::ERROR_COMMITMENT_LIMIT:
				case win32::ERROR_NOT_ENOUGH_MEMORY: return mem::OUT_OF_MEMORY~;
				case win32::ERROR_INVALID_PARAMETER: return INVALID_ARGS~;
				default:                             return UNKNOWN_ERROR~;
			}
		$default:
			unsupported("'virtual_commit' is only available on Win32 and Posix.");
	$endswitch
}

<*
 Notifies that the memory in the region can be released back to the OS. On Win32 this decommits the region,
 whereas on Posix it tells the system that it may be reused using madvise. The "block" parameter is only
 respected on Posix, and protects the region from read/write/exec. On Win32 this always happens.

 @param [&inout] ptr : "Pointer to page to update, must be page aligned"
 @param len : "To what len to commit, must be page aligned"
 @param block : "Set the released memory to protected"
 @require mem::ptr_is_page_aligned(ptr) : "The pointer should be page aligned"
 @require mem::ptr_is_page_aligned(ptr + len) : "The length must be page aligned"
 @return? ACCESS_DENIED, UNALIGNED_ADDRESS, RANGE_OVERFLOW, UPDATE_FAILED, UNMAPPED_ACCESS, INVALID_ARGS
*>
fn void? decommit(void* ptr, usz len, bool block = true)
{
	$switch:
		$case env::POSIX:
			if (posix::madvise(ptr, len, posix::MADV_DONTNEED))
			{
				switch (libc::errno())
				{
					case errno::EINVAL: return UNALIGNED_ADDRESS~;
					case errno::ENOMEM: return UNMAPPED_ACCESS~;
					default:            return UPDATE_FAILED~;
				}
			}
			if (block) (void)protect(ptr, len, PROTECTED) @inline;
		$case env::WIN32:
			if (!win32::virtualFree(ptr, len, MEM_DECOMMIT))
			{
				switch (win32::getLastError())
				{
					case win32::ERROR_INVALID_ADDRESS:   return UNALIGNED_ADDRESS~;
					case win32::ERROR_INVALID_PARAMETER: return INVALID_ARGS~;
					case win32::ERROR_ACCESS_DENIED:     return ACCESS_DENIED~;
					default:                             return UPDATE_FAILED~;
				}
			}
		$default:
			unsupported("'virtual_decommit' is only available on Win32 and Posix.");
	$endswitch
}

<*
 Map a portion of an already-opened file into memory.

 @param fd : "File descriptor"
 @param size : "Number of bytes to map, will be rounded up to page size"
 @param offset : "Byte offset in file, must be page size aligned"
 @param access : "The initial access permissions"
 @param shared : "if True then MAP_SHARED else MAP_PRIVATE"
 @return? mem::OUT_OF_MEMORY, RANGE_OVERFLOW, UNKNOWN_ERROR, ACCESS_DENIED, INVALID_ARGS, io::NO_PERMISSION, io::FILE_NOT_VALID, io::WOULD_BLOCK, io::FILE_NOT_FOUND
 @return "Pointer to the mapped region"
*>
fn void*? mmap_file(Fd fd, usz size, usz offset = 0, VirtualMemoryAccess access = READ, bool shared = false) @if (env::POSIX)
{
	CInt flags = shared ? posix::MAP_SHARED : posix::MAP_PRIVATE;
	void* ptr = posix::mmap(null, aligned_alloc_size(size), access.to_posix(), flags, fd, offset);
	if (ptr != posix::MAP_FAILED) return ptr;
	switch (libc::errno())
	{
		case errno::ENOMEM:    return mem::OUT_OF_MEMORY~;
		case errno::EOVERFLOW: return RANGE_OVERFLOW~;
		case errno::EPERM:     return ACCESS_DENIED~;
		case errno::EINVAL:    return INVALID_ARGS~;
		case errno::EACCES:    return io::NO_PERMISSION~;
		case errno::EBADF:     return io::FILE_NOT_VALID~;
		case errno::EAGAIN:    return io::WOULD_BLOCK~;
		case errno::ENXIO:     return io::FILE_NOT_FOUND~;
		default:               return UNKNOWN_ERROR~;
	}
}

<*
 Create a VirtualMemory using

 @param size : "The size of the memory to allocate."
 @require size > 0 : "The size must be non-zero"
 @return? mem::OUT_OF_MEMORY, RANGE_OVERFLOW, UNKNOWN_ERROR, ACCESS_DENIED, INVALID_ARGS
*>
fn VirtualMemory? virtual_alloc(usz size, VirtualMemoryAccess access = PROTECTED)
{
	size = aligned_alloc_size(size);
	void* ptr = alloc(size, access)!;
	return { ptr, size, access };
}

<*
 Commits memory, using vm::commit

 @param offset : "Starting from what offset to commit"
 @param len : "To what len to commit"
 @require mem::ptr_is_page_aligned(self.ptr + offset) : "The offset should be page aligned"
 @require mem::ptr_is_page_aligned(self.ptr + offset + len) : "The length must be page aligned"
 @require offset < self.size : "Offset out of range"
 @require offset + len <= self.size : "Length out of range"
 @return? UPDATE_FAILED, ACCESS_DENIED, UNALIGNED_ADDRESS, RANGE_OVERFLOW, UNKNOWN_ERROR
*>
macro void? VirtualMemory.commit(self, usz offset, usz len)
{
	return commit(self.ptr + offset, len, self.default_access);
}

<*
 Changes protection of a part of memory using vm::protect

 @param offset : "Starting from what offset to update"
 @param len : "To what len to update"
 @require mem::ptr_is_page_aligned(self.ptr + offset) : "The offset should be page aligned"
 @require mem::ptr_is_page_aligned(self.ptr + offset + len) : "The length must be page aligned"
 @require offset < self.size : "Offset out of range"
 @require offset + len < self.size : "Length out of range"
 @return? UPDATE_FAILED, ACCESS_DENIED, UNALIGNED_ADDRESS, RANGE_OVERFLOW, UNKNOWN_ERROR
*>
macro void? VirtualMemory.protect(self, usz offset, usz len, VirtualMemoryAccess access)
{
	return protect(self.ptr + offset, len, access);
}
<*
 Decommits a part of memory using vm::decommit

 @param offset : "Starting from what offset to decommit"
 @param len : "To what len to decommit"
 @param block : "Should the memory be blocked from access after decommit"
 @require mem::ptr_is_page_aligned(self.ptr + offset) : "The offset should be page aligned"
 @require mem::ptr_is_page_aligned(self.ptr + offset + len) : "The length must be page aligned"
 @require offset < self.size : "Offset out of range"
 @require offset + len < self.size : "Length out of range"
 @return? UPDATE_FAILED
*>
fn void? VirtualMemory.decommit(self, usz offset, usz len, bool block = true)
{
	return decommit(self.ptr + offset, len, block);
}

<*
 Releases the memory region

 @require self.ptr != null : "Virtual memory must be initialized to call destroy"
*>
fn void? VirtualMemory.destroy(&self)
{
	return release(self.ptr, self.size);
}

fn CInt VirtualMemoryAccess.to_posix(self) @if(env::POSIX) @private
{
	switch (self)
	{
		case PROTECTED: return posix::PROT_NONE;
		case READ: return posix::PROT_READ;
		case WRITE: return posix::PROT_WRITE;
		case EXEC: return posix::PROT_EXEC;
		case READWRITE: return posix::PROT_READ | posix::PROT_WRITE;
		case EXECREAD: return posix::PROT_READ | posix::PROT_EXEC;
		case EXECWRITE: return posix::PROT_WRITE | posix::PROT_EXEC;
		case ANY: return posix::PROT_WRITE | posix::PROT_READ | posix::PROT_EXEC;
	}
}

fn Win32_Protect VirtualMemoryAccess.to_win32(self) @if(env::WIN32) @private
{
	switch (self)
	{
		case PROTECTED: return PAGE_NOACCESS;
		case READ: return PAGE_READONLY;
		case WRITE: return PAGE_READWRITE;
		case EXEC: return PAGE_EXECUTE;
		case READWRITE: return PAGE_READWRITE;
		case EXECWRITE: return PAGE_EXECUTE_READWRITE;
		case EXECREAD: return PAGE_EXECUTE_READ;
		case ANY: return PAGE_EXECUTE_READWRITE;
	}
}

