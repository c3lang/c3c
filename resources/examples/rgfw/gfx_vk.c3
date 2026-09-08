module std::gfx;
import libc;

alias VkResult = int;
alias VkInstance = void*;
alias VkPhysicalDevice = void*;
alias VkSurfaceKHR = ulong;

alias GfxVkGetInstanceProcAddressProc = fn GfxFn(VkInstance instance, ZString procname);



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

fn void unload_vulkan_native() @private
{
	$if $feat(WASM):
		return;
	$else
		if (!_gfx_ctx.vulkan_handle) return;
		$if $feat(WIN32):
			freeLibrary((HModule*)_gfx_ctx.vulkan_handle);
		$else
			libc::dlclose(_gfx_ctx.vulkan_handle);
		$endif
		_gfx_ctx.vulkan_handle = null;
		_gfx_ctx.vkGetInstanceProcAddress = null;
	$endif
}