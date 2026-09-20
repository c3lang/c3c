module std::os::win32 @feat(WIN32);

struct Win32_DXGI_SWAP_CHAIN_DESC
{
	Win32_DXGI_MODE_DESC        bufferDesc;
	Win32_DXGI_SAMPLE_DESC      sampleDesc;
	Win32_DXGI_USAGE            bufferUsage;
	Win32_UINT                  bufferCount;
	Win32_HWND                  outputWindow;
	Win32_BOOL                  windowed;
	Win32_DXGI_SWAP_EFFECT      swapEffect;
	Win32_DXGI_SWAP_CHAIN_FLAG  flags;
}

constset Win32_DXGI_SWAP_EFFECT : int
{
	DISCARD         = 0,
	SEQUENTIAL      = 1,
	FLIP_SEQUENTIAL = 3,   // 2 is unused
	FLIP_DISCARD    = 4,
}

const Win32_DXGI_USAGE DXGI_USAGE_SHADER_INPUT         = 0x10;
const Win32_DXGI_USAGE DXGI_USAGE_RENDER_TARGET_OUTPUT = 0x20;
const Win32_DXGI_USAGE DXGI_USAGE_BACK_BUFFER          = 0x40;
const Win32_DXGI_USAGE DXGI_USAGE_SHARED               = 0x80;
const Win32_DXGI_USAGE DXGI_USAGE_READ_ONLY            = 0x100;
const Win32_DXGI_USAGE DXGI_USAGE_DISCARD_ON_PRESENT   = 0x200;
const Win32_DXGI_USAGE DXGI_USAGE_UNORDERED_ACCESS     = 0x400;

constset Win32_DXGI_SWAP_CHAIN_FLAG : Win32_UINT
{
	NONPREROTATED                          = 0x1,
	ALLOW_MODE_SWITCH                      = 0x2,
	GDI_COMPATIBLE                         = 0x4,
	RESTRICTED_CONTENT                     = 0x8,
	RESTRICT_SHARED_RESOURCE_DRIVER        = 0x10,
	DISPLAY_ONLY                           = 0x20,
	FRAME_LATENCY_WAITABLE_OBJECT          = 0x40,
	FOREGROUND_LAYER                       = 0x80,
	FULLSCREEN_VIDEO                       = 0x100,
	YUV_VIDEO                              = 0x200,
	HW_PROTECTED                           = 0x400,
	ALLOW_TEARING                          = 0x800,
	RESTRICTED_TO_ALL_HOLOGRAPHIC_DISPLAYS = 0x1000,
}


alias Win32_DXGI_CreateSwapChain = fn Win32_HRESULT(void* this, void* device, Win32_DXGI_SWAP_CHAIN_DESC* desc, void** swapchain);

struct Win32_IDXGIFactoryVtbl
{
	// IUnknown
	void* queryInterface;
	void* addRef;
	void* release;
	// IDXGIObject
	void* setPrivateData;
	void* setPrivateDataInterface;
	void* getPrivateData;
	void* getParent;
	// IDXGIFactory
	void* enumAdapters;
	void* makeWindowAssociation;
	void* getWindowAssociation;
	Win32_DXGI_CreateSwapChain createSwapChain;
	void* createSoftwareAdapter;
}

struct Win32_IDXGIFactory
{
	Win32_IDXGIFactoryVtbl* lpVtbl;
}