module std::core::mem;
import std::core::env;

struct VirtualMemory
{
	void* ptr;
	usz size;
}

faultdef VMEM_OVERFLOW, VMEM_UNKNOWN_ERROR, VMEM_ACCESS_NOT_ALLOWED, VMEM_PAGE_NOT_ALIGNED, VMEM_UPDATE_FAILED;


module std::core::mem @if(env::POSIX);
import std::os::posix, libc::errno, std::math;

enum VirtualMemoryAccess : int(int posix_val)
{
	PROTECTED = posix::PROT_NONE,
	READABLE = posix::PROT_READ,
	WRITABLE = posix::PROT_WRITE,
	READWRITE = posix::PROT_READ | posix::PROT_WRITE
}

<*
 @param size : "The size of the memory to allocate."
 @param access : "The initial access."
 @require size > 0 : "The size must be non-zero"
 @return? OUT_OF_MEMORY, VMEM_OVERFLOW, VMEM_UNKNOWN_ERROR
*>
fn VirtualMemory? virtual_alloc(usz size, VirtualMemoryAccess access)
{
	void *ptr = posix::mmap(null, size, access.posix_val, posix::MAP_PRIVATE | posix::MAP_ANONYMOUS, -1, 0);
	if (ptr != posix::MAP_FAILED && ptr) return { ptr, size  };
	switch (libc::errno())
    {
        case errno::ENOMEM: return OUT_OF_MEMORY?;
    	case errno::EOVERFLOW: return VMEM_OVERFLOW?;
    	default: return VMEM_UNKNOWN_ERROR?;
	}
}

macro void? VirtualMemory.commit(self, usz offset, usz len) {}

<*
 @param offset : "Starting from what offset to update"
 @param len : "To what len to update"
 @param access : "The new access"
 @require offset < self.size : "Offset out of range"
 @require offset + len < self.size : "Length out of range"
 @require offset == 0 || math::is_power_of_2(offset) : "Offset should be a power of 2"
 @require math::is_power_of_2(len) : "Length should be a multiple of the page size"
 @return? VMEM_ACCESS_NOT_ALLOWED, VMEM_PAGE_NOT_ALIGNED, VMEM_OVERFLOW, VMEM_UNKNOWN_ERROR
*>
fn void? VirtualMemory.set_access(self, usz offset, usz len, VirtualMemoryAccess access)
{
	if (posix::mprotect(self.ptr + offset, len, access.posix_val))
	{
		switch (libc::errno())
		{
            case errno::EACCES: return VMEM_ACCESS_NOT_ALLOWED?;
            case errno::EINVAL: return VMEM_PAGE_NOT_ALIGNED?;
    	    case errno::EOVERFLOW: return VMEM_OVERFLOW?;
    	    case errno::ENOMEM: abort("Vmem access out of range");
       	    default: return VMEM_UNKNOWN_ERROR?;
		}
	}
}

<*
 @param offset : "Starting from what offset to release"
 @param len : "To what len to release"
 @require offset < self.size : "Offset out of range"
 @require offset + len < self.size : "Length out of range"
 @require offset == 0 || math::is_power_of_2(offset) : "Offset should be a power of 2"
 @require math::is_power_of_2(len) : "Length should be a multiple of the page size"
 @return? VMEM_UPDATE_FAILED
*>
fn void? VirtualMemory.release(self, usz offset, usz len)
{
	if (posix::madvise(self.ptr + offset, len, posix::MADV_DONTNEED)) return VMEM_UPDATE_FAILED?;
}

<*
 @require self.ptr != null : "Virtual memory must be initialized to call destroy"
*>
fn void VirtualMemory.destroy(&self)
{
	posix::munmap(self.ptr, self.size);
	self.ptr = null;
}

module std::core::mem @if(env::WIN32);
import std::os::win32, std::math;

enum VirtualMemoryAccess : int(Win32_Protect win32_val)
{
	PROTECTED = PAGE_NOACCESS,
	READABLE = PAGE_READONLY,
	WRITABLE = PAGE_READWRITE,
	READWRITE = PAGE_READWRITE
}

<*
 @param size : "The size of the memory to allocate."
 @param access : "The initial access."
 @require size > 0 : "The size must be non-zero"
 @return? OUT_OF_MEMORY, VMEM_UNKNOWN_ERROR
*>
fn VirtualMemory? virtual_alloc(usz size, VirtualMemoryAccess access)
{
	void *ptr = win32::virtualAlloc(null, size, MEM_RESERVE, access.win32_val);
	if (ptr) return { ptr, size };
	switch (win32::getLastError())
	{
		case win32::ERROR_NOT_ENOUGH_MEMORY:
		case win32::ERROR_COMMITMENT_LIMIT: return OUT_OF_MEMORY?;
		default: return VMEM_UNKNOWN_ERROR?;
	}
}

<*
 @param offset : "Starting from what offset to commit"
 @param len : "To what len to commit"
 @require offset < self.size : "Offset out of range"
 @require offset + len < self.size : "Length out of range"
 @require offset == 0 || math::is_power_of_2(offset) : "Offset should be a power of 2"
 @require math::is_power_of_2(len) : "Length should be a multiple of the page size"
 @return? VMEM_UPDATE_FAILED
*>
macro void? VirtualMemory.commit(self, usz offset, usz len)
{
	void *res = win32::virtualAlloc(self.ptr + offset, len, MEM_COMMIT, PAGE_READWRITE);
	if (!res) return VMEM_UPDATE_FAILED?;
}

<*
 @param offset : "Starting from what offset to commit"
 @param len : "To what len to commit"
 @require offset < self.size : "Offset out of range"
 @require offset + len < self.size : "Length out of range"
 @require offset == 0 || math::is_power_of_2(offset) : "Offset should be a power of 2"
 @require math::is_power_of_2(len) : "Length should be a multiple of the page size"
 @return? VMEM_UPDATE_FAILED
*>
macro void? VirtualMemory.set_access(self, usz offset, usz len, VirtualMemoryAccess access)
{
	Win32_Protect old;
	if (!win32::virtualProtect(self.ptr + offset, len, access.win32_val, &old))
	{
		return VMEM_UPDATE_FAILED?;
	}
}

<*
 @param offset : "Starting from what offset to release"
 @param len : "To what len to release"
 @require offset < self.size : "Offset out of range"
 @require offset + len < self.size : "Length out of range"
 @require offset == 0 || math::is_power_of_2(offset) : "Offset should be a power of 2"
 @require math::is_power_of_2(len) : "Length should be a multiple of the page size"
 @return? VMEM_UPDATE_FAILED
*>
fn void? VirtualMemory.release(self, usz offset, usz len)
{
	if (!win32::virtualFree(self.ptr + offset, len, MEM_DECOMMIT)) return VMEM_UPDATE_FAILED?;
}

<*
 @require self.ptr != null : "Virtual memory must be initialized to call destroy"
*>
fn void VirtualMemory.destroy(&self)
{
	win32::virtualFree(self.ptr, 0, MEM_RELEASE);
	self.ptr = null;
}

