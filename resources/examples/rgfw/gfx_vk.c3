module std::gfx;

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
