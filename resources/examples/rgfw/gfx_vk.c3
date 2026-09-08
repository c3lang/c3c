module std::gfx;
import libc, std::os::win32;


alias VkResult = int;
alias VkInstance = void*;
alias VkPhysicalDevice = void*;
alias VkSurfaceKHR = ulong;

alias GfxVkGetInstanceProcAddressProc = fn GfxFn(VkInstance instance, ZString procname);


fn void? _load_vulkan_native() @weak @private => NOT_IMPLEMENTED~;
fn void _unload_vulkan_native() @weak @private {}

// public
fn GfxFn rgfw_getInstanceProcAddress_Vulkan(VkInstance instance, ZString procname)
{
	if (!_gfx_ctx.vkGetInstanceProcAddress) return null;
	return _gfx_ctx.vkGetInstanceProcAddress(instance, procname);
}

// public
fn ZString* rgfw_getRequiredInstanceExtensions_Vulkan(sz* count) @feat(!WASM)
{
	if (count != null) *count = 2;
	return &rgfw_vulkanExtensions;
}


module std::gfx @feat(MACOS, LINUX) @private;
import libc;

fn void _unload_vulkan_native()
{
	if (!_gfx_ctx.vulkan_handle) return;
	libc::dlclose(_gfx_ctx.vulkan_handle);
	_gfx_ctx.vulkan_handle = null;
	_gfx_ctx.vkGetInstanceProcAddress = null;
}

fn void? _load_vulkan_native()
{
	if (_gfx_ctx.vulkan_handle) return;
	$if $feat(MACOS):
		ZString[3] names = {"libvulkan.1.dylib", "libvulkan.dylib", "libMoltenVK.dylib"};
	$else
		ZString[3] names = {"libvulkan.so.1", "libvulkan.so", "libMoltenVK.so"};
	$endif
	_gfx_ctx.vulkan_handle = _dlopen_any(...names) ?? MISSING_VULKAN_LIB~!;
	void* proc = libc::dlsym(_gfx_ctx.vulkan_handle, "vkGetInstanceProcAddr");
	if (!proc) return MISSING_VK_GET_INSTANCE_PROC_ADDR~;
	_gfx_ctx.vkGetInstanceProcAddress = proc;
}

module std::gfx @feat(WIN32) @private;
import libc, std::os::win32;

fn void? _load_vulkan_native()
{
	if (_gfx_ctx.vulkan_handle) return;
	_gfx_ctx.vulkan_handle = win32::loadLibraryA("vulkan-1.dll");
	if (!_gfx_ctx.vulkan_handle) return MISSING_VULKAN_LIB~;
	void* proc = win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.vulkan_handle, "vkGetInstanceProcAddr");
	if (!proc) return MISSING_VK_GET_INSTANCE_PROC_ADDR~;
	_gfx_ctx.vkGetInstanceProcAddress = proc;
}

fn void _unload_vulkan_native()
{
	if (!_gfx_ctx.vulkan_handle) return;
	win32::freeLibrary((Win32_HMODULE*)_gfx_ctx.vulkan_handle);
	_gfx_ctx.vulkan_handle = null;
	_gfx_ctx.vkGetInstanceProcAddress = null;
}
