module std::gfx @feat(MACOS, X11, X11_WAYLAND, WAYLAND, WASM, WIN32);
import libc, std::os::win32;


alias VkResult = int;
alias VkInstance = void*;
alias VkPhysicalDevice = void*;
alias VkSurfaceKHR = ulong;


const VkResult VK_SUCCESS                     = 0;
const VkResult VK_NOT_READY                   = 1;
const VkResult VK_TIMEOUT                     = 2;
const VkResult VK_EVENT_SET                   = 3;
const VkResult VK_EVENT_RESET                 = 4;
const VkResult VK_INCOMPLETE                  = 5;
const VkResult VK_ERROR_OUT_OF_HOST_MEMORY    = -1;
const VkResult VK_ERROR_OUT_OF_DEVICE_MEMORY  = -2;
const VkResult VK_ERROR_INITIALIZATION_FAILED = -3;
const VkResult VK_ERROR_DEVICE_LOST           = -4;
const VkResult VK_ERROR_MEMORY_MAP_FAILED     = -5;
const VkResult VK_ERROR_LAYER_NOT_PRESENT     = -6;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
const VkResult VK_ERROR_FEATURE_NOT_PRESENT   = -8;
const VkResult VK_ERROR_INCOMPATIBLE_DRIVER   = -9;
const VkResult VK_ERROR_TOO_MANY_OBJECTS      = -10;
const VkResult VK_ERROR_FORMAT_NOT_SUPPORTED  = -11;
const VkResult VK_ERROR_FRAGMENTED_POOL       = -12;
const VkResult VK_ERROR_UNKNOWN               = -13;
const VkResult VK_ERROR_OUT_OF_POOL_MEMORY                = -1000069000;
const VkResult VK_ERROR_INVALID_EXTERNAL_HANDLE           = -1000072003;
const VkResult VK_ERROR_FRAGMENTATION                     = -1000161000;
const VkResult VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS    = -1000257000;
const VkResult VK_PIPELINE_COMPILE_REQUIRED               = 1000297000;

const int VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR   = 1000009000;
const int VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = 1000006000;
const int VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR    = 1000004000;

alias GfxVkGetInstanceProcAddressProc = fn GfxFn(VkInstance instance, ZString procname);
alias VkCreateSurfaceKHRProc = fn VkResult(VkInstance instance, void* create_info, void* allocator, VkSurfaceKHR* surface);

fn bool vulkan_get_physical_device_presentation_support_native(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	return _vulkan_get_physical_device_presentation_support_native(instance, physical_device, queue_family_index);
}

// public
fn GfxFn todo_fix_getInstanceProcAddress_Vulkan(VkInstance instance, ZString procname) @feat(!WASM)
{
	if (!_gfx_ctx.vk_get_instance_proc_address) return null;
	return _gfx_ctx.vk_get_instance_proc_address(instance, procname);
}

// public
fn String[] get_required_vulkan_extensions()
{
	$switch:
		$case $feat(LINUX) &&& WAYLAND_ONLY:
			static String[] ext = { "VK_KHR_surface", "VK_KHR_wayland_surface" };
		$case $feat(LINUX) &&& X11_ONLY:
			static String[] ext = { "VK_KHR_surface", "VK_KHR_xlib_surface" };
		$case $feat(LINUX):
			static String[] ext = { "VK_KHR_surface", "VK_KHR_xlib_surface", "VK_KHR_wayland_surface" };
		$case $feat(MACOS):
			static String[] ext = { "VK_KHR_surface", "VK_EXT_metal_surface"};
		$case $feat(WIN32):
			static String[] ext = { "VK_KHR_surface", "VK_KHR_win32_surface" };
		$default:
			static String[] ext = {};
	$endswitch
	return ext;
}

fn VkSurfaceKHR? GfxWindow.create_vulkan_surface(&self, VkInstance instance)
{
	return _window_create_vulkan_surface_native(self, instance);
}

fn VkSurfaceKHR? _window_create_vulkan_surface_native(GfxWindow* win, VkInstance instance) @weak @private => NOT_IMPLEMENTED~;

fn void? _load_vulkan_native() @weak @private => NOT_IMPLEMENTED~;
fn void _unload_vulkan_native() @weak @private {}

module std::gfx @feat(MACOS, X11, WAYLAND, X11_WAYLAND) @private;
import libc;

fn void _unload_vulkan_native()
{
	if (!_gfx_ctx.vulkan_handle) return;
	libc::dlclose(_gfx_ctx.vulkan_handle);
	_gfx_ctx.vulkan_handle = null;
	_gfx_ctx.vk_get_instance_proc_address = null;
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
	_gfx_ctx.vk_get_instance_proc_address = proc;
}

module std::gfx @feat(WIN32) @private;
import libc, std::os::win32;

struct VkWin32SurfaceCreateInfoKHR
{
	int sType;
	void* pNext;
	uint flags;
	void* hinstance;
	void* hwnd;
}


fn void? _load_vulkan_native()
{
	if (_gfx_ctx.vulkan_handle) return;
	_gfx_ctx.vulkan_handle = win32::loadLibraryA("vulkan-1.dll");
	if (!_gfx_ctx.vulkan_handle) return MISSING_VULKAN_LIB~;
	void* proc = win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.vulkan_handle, "vkGetInstanceProcAddr");
	if (!proc) return MISSING_VK_GET_INSTANCE_PROC_ADDR~;
	_gfx_ctx.vk_get_instance_proc_address = proc;
}

fn void _unload_vulkan_native()
{
	if (!_gfx_ctx.vulkan_handle) return;
	win32::freeLibrary((Win32_HMODULE*)_gfx_ctx.vulkan_handle);
	_gfx_ctx.vulkan_handle = null;
	_gfx_ctx.vk_get_instance_proc_address = null;
}



// public

