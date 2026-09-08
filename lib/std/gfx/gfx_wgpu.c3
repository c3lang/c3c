module std::gfx;

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuStringView { char* data; sz length; }
