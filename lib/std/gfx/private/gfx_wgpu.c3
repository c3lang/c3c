module std::gfx;

const WGPUSType WGPU_STYPE_SURFACE_SOURCE_METAL_LAYER = 0x00000004;
alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuStringView { char* data; sz length; }
