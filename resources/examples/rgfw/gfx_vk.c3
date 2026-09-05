module std::gfx;

alias VkResult = int;
alias VkInstance = void*;
alias VkPhysicalDevice = void*;
alias VkSurfaceKHR = ulong;

alias GfxVkGetInstanceProcAddressProc = fn GfxFn(VkInstance instance, ZString procname);
