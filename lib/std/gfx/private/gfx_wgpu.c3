module std::gfx;

const WGPUSType WGPU_STYPE_SURFACE_SOURCE_METAL_LAYER = 0x00000004;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WINDOWS_HWND = 0x00000005;
const WGPUSType WGPU_STYPE_EMSCRIPTEN_SURFACE_SOURCE_CANVAS_HTML_SELECTOR = 0x00040000;

extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuStringView { char* data; sz length; }
