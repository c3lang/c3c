module std::gfx @feat(MACOS, LINUX, BSD, WASM, WIN32);
import libc, std::os;


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
const int VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT   = 1000217000;

alias GfxVkGetInstanceProcAddressProc = fn GfxFn(VkInstance instance, ZString procname);
alias VkCreateSurfaceKHRProc = fn VkResult(VkInstance instance, void* create_info, void* allocator, VkSurfaceKHR* surface);

fn bool vulkan_get_physical_device_presentation_support_native(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	return _vulkan_get_physical_device_presentation_support_native(instance, physical_device, queue_family_index);
}

// public
fn GfxFn _get_instance_proc_vulkan(VkInstance instance, ZString procname) @feat(!WASM)
{
	if (!_gfx.vk_get_instance_proc_address) return null;
	return _gfx.vk_get_instance_proc_address(instance, procname);
}

macro get_required_vulkan_extensions()
{
	String[1] $ext = { "VK_KHR_surface" };
	$switch:
		$case $feat(LINUX) || $feat(BSD):
			$if !$feat(NO_X11): $ext +++= "VK_KHR_xlib_surface"; $endif
			$if $feat(WAYLAND): $ext +++= "VK_KHR_wayland_surface"; $endif
		$case $feat(MACOS):
			$ext +++= "VK_EXT_metal_surface";
		$case $feat(WIN32):
			$ext +++= "VK_KHR_win32_surface";
		$default:
	$endswitch
	return $ext;
}

fn VkSurfaceKHR? _window_create_vulkan_surface_native(GfxWindow* win, VkInstance instance) @weak @private => NOT_IMPLEMENTED~;

fn void _unload_vulkan_native()
{
	if (!_gfx.vulkan_handle) return;
	_gfx.vulkan_handle.close();
	_gfx.vk_get_instance_proc_address = null;
}

fn void? _load_vulkan_native()
{
	if (_gfx.vulkan_handle) return;
	DylibHandle? handle;
	$switch:
		$case $feat(MACOS):
			handle = os::dlopen_any("libvulkan.1.dylib", "libvulkan.dylib", "libMoltenVK.dylib");
		$case $feat(WIN32):
			handle = os::dlopen_try("vulkan-1.dll", { .system_only });
		$default:
			handle = os::dlopen_any("libvulkan.so.1", "libvulkan.so", "libMoltenVK.so");
	$endswitch
	if (catch handle) return MISSING_VULKAN_LIB~;
	_gfx.vulkan_handle = handle;
	void* proc = os::dlsym(handle, "vkGetInstanceProcAddr") ?: MISSING_VK_GET_INSTANCE_PROC_ADDR~!;
	_gfx.vk_get_instance_proc_address = proc;
}
