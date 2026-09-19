module std::gfx @feat(WASM, WIN32, BSD, LINUX, MACOS);

const WGPUSType WGPU_STYPE_SURFACE_SOURCE_METAL_LAYER                     = 0x00000004;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WINDOWS_HWND                    = 0x00000005;
const WGPUSType WGPU_STYPE_EMSCRIPTEN_SURFACE_SOURCE_CANVAS_HTML_SELECTOR = 0x00040000;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE                 = 0x00000007;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW                     = 0x00000006;

alias WGPUInstance @weak = void*;
alias WGPUSurface @weak = void*;
alias WGPUSType @weak  = int;
struct WgpuChainedStruct @weak  { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor @weak  { WgpuChainedStruct* nextInChain; }
struct WgpuStringView @weak  { char* data; sz length; }

macro WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @feat(!WGPU)
{
	log::error("WGPU not supported, code was not compiled with -D WGPU");
	return null;
}

extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @feat(WGPU);

