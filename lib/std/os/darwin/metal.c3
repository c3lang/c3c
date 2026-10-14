module std::os::darwin @feat(DARWIN) @link("Metal.framework");

fn ObjClass cametal_layer() => objc::@class("CAMetalLayer");

extern fn ObjId mtlCreateSystemDefaultDevice() @cname("MTLCreateSystemDefaultDevice");

<*
 The pixel formats of Metal textures and layers, `MTLPixelFormat` in MTLPixelFormat.h.

 Not every format is available on every platform and OS version, see the header.
*>
constset MTLPixelFormat : inline NSUInteger
{
	INVALID                = 0,

	// Normal 8 bit formats
	A8_UNORM               = 1,
	R8_UNORM               = 10,
	R8_UNORM_SRGB          = 11,
	R8_SNORM               = 12,
	R8_UINT                = 13,
	R8_SINT                = 14,

	// Normal 16 bit formats
	R16_UNORM              = 20,
	R16_SNORM              = 22,
	R16_UINT               = 23,
	R16_SINT               = 24,
	R16_FLOAT              = 25,
	RG8_UNORM              = 30,
	RG8_UNORM_SRGB         = 31,
	RG8_SNORM              = 32,
	RG8_UINT               = 33,
	RG8_SINT               = 34,

	// Packed 16 bit formats
	B5G6R5_UNORM           = 40,
	A1BGR5_UNORM           = 41,
	ABGR4_UNORM            = 42,
	BGR5A1_UNORM           = 43,

	// Normal 24 bit formats
	RGB8_UNORM             = 45,
	RGB8_SNORM             = 46,
	RGB8_UINT              = 47,
	RGB8_SINT              = 48,

	// Normal 32 bit formats
	R32_UINT               = 53,
	R32_SINT               = 54,
	R32_FLOAT              = 55,
	RG16_UNORM             = 60,
	RG16_SNORM             = 62,
	RG16_UINT              = 63,
	RG16_SINT              = 64,
	RG16_FLOAT             = 65,
	RGBA8_UNORM            = 70,
	RGBA8_UNORM_SRGB       = 71,
	RGBA8_SNORM            = 72,
	RGBA8_UINT             = 73,
	RGBA8_SINT             = 74,
	BGRA8_UNORM            = 80,
	BGRA8_UNORM_SRGB       = 81,

	// Packed 32 bit formats
	RGB10A2_UNORM          = 90,
	RGB10A2_UINT           = 91,
	RG11B10_FLOAT          = 92,
	RGB9E5_FLOAT           = 93,
	BGR10A2_UNORM          = 94,
	BGR10_XR               = 554,
	BGR10_XR_SRGB          = 555,

	// Normal 48 bit formats
	RGB16_UNORM            = 95,
	RGB16_SNORM            = 96,
	RGB16_UINT             = 97,
	RGB16_SINT             = 98,
	RGB16_FLOAT            = 99,

	// Normal 64 bit formats
	RG32_UINT              = 103,
	RG32_SINT              = 104,
	RG32_FLOAT             = 105,
	RGBA16_UNORM           = 110,
	RGBA16_SNORM           = 112,
	RGBA16_UINT            = 113,
	RGBA16_SINT            = 114,
	RGBA16_FLOAT           = 115,
	BGRA10_XR              = 552,
	BGRA10_XR_SRGB         = 553,

	// Normal 96 bit formats
	RGB32_UINT             = 120,
	RGB32_SINT             = 121,
	RGB32_FLOAT            = 122,

	// Normal 128 bit formats
	RGBA32_UINT            = 123,
	RGBA32_SINT            = 124,
	RGBA32_FLOAT           = 125,

	// Compressed formats.

	// S3TC/DXT
	BC1_RGBA               = 130,
	BC1_RGBA_SRGB          = 131,
	BC2_RGBA               = 132,
	BC2_RGBA_SRGB          = 133,
	BC3_RGBA               = 134,
	BC3_RGBA_SRGB          = 135,

	// RGTC
	BC4_R_UNORM            = 140,
	BC4_R_SNORM            = 141,
	BC5_RG_UNORM           = 142,
	BC5_RG_SNORM           = 143,

	// BPTC
	BC6H_RGB_FLOAT         = 150,
	BC6H_RGB_UFLOAT        = 151,
	BC7_RGBA_UNORM         = 152,
	BC7_RGBA_UNORM_SRGB    = 153,

	// PVRTC
	PVRTC_RGB_2BPP         = 160,
	PVRTC_RGB_2BPP_SRGB    = 161,
	PVRTC_RGB_4BPP         = 162,
	PVRTC_RGB_4BPP_SRGB    = 163,
	PVRTC_RGBA_2BPP        = 164,
	PVRTC_RGBA_2BPP_SRGB   = 165,
	PVRTC_RGBA_4BPP        = 166,
	PVRTC_RGBA_4BPP_SRGB   = 167,

	// ETC2
	EAC_R11_UNORM          = 170,
	EAC_R11_SNORM          = 172,
	EAC_RG11_UNORM         = 174,
	EAC_RG11_SNORM         = 176,
	EAC_RGBA8              = 178,
	EAC_RGBA8_SRGB         = 179,
	ETC2_RGB8              = 180,
	ETC2_RGB8_SRGB         = 181,
	ETC2_RGB8A1            = 182,
	ETC2_RGB8A1_SRGB       = 183,

	// ASTC
	ASTC_4X4_SRGB          = 186,
	ASTC_5X4_SRGB          = 187,
	ASTC_5X5_SRGB          = 188,
	ASTC_6X5_SRGB          = 189,
	ASTC_6X6_SRGB          = 190,
	ASTC_8X5_SRGB          = 192,
	ASTC_8X6_SRGB          = 193,
	ASTC_8X8_SRGB          = 194,
	ASTC_10X5_SRGB         = 195,
	ASTC_10X6_SRGB         = 196,
	ASTC_10X8_SRGB         = 197,
	ASTC_10X10_SRGB        = 198,
	ASTC_12X10_SRGB        = 199,
	ASTC_12X12_SRGB        = 200,
	ASTC_4X4_LDR           = 204,
	ASTC_5X4_LDR           = 205,
	ASTC_5X5_LDR           = 206,
	ASTC_6X5_LDR           = 207,
	ASTC_6X6_LDR           = 208,
	ASTC_8X5_LDR           = 210,
	ASTC_8X6_LDR           = 211,
	ASTC_8X8_LDR           = 212,
	ASTC_10X5_LDR          = 213,
	ASTC_10X6_LDR          = 214,
	ASTC_10X8_LDR          = 215,
	ASTC_10X10_LDR         = 216,
	ASTC_12X10_LDR         = 217,
	ASTC_12X12_LDR         = 218,
	ASTC_4X4_HDR           = 222,
	ASTC_5X4_HDR           = 223,
	ASTC_5X5_HDR           = 224,
	ASTC_6X5_HDR           = 225,
	ASTC_6X6_HDR           = 226,
	ASTC_8X5_HDR           = 228,
	ASTC_8X6_HDR           = 229,
	ASTC_8X8_HDR           = 230,
	ASTC_10X5_HDR          = 231,
	ASTC_10X6_HDR          = 232,
	ASTC_10X8_HDR          = 233,
	ASTC_10X10_HDR         = 234,
	ASTC_12X10_HDR         = 235,
	ASTC_12X12_HDR         = 236,
	GBGR422                = 240,
	BGRG422                = 241,

	// Depth
	DEPTH16_UNORM          = 250,
	DEPTH32_FLOAT          = 252,

	// Stencil
	STENCIL8               = 253,

	// Depth Stencil
	DEPTH24_UNORM_STENCIL8 = 255,
	DEPTH32_FLOAT_STENCIL8 = 260,
	X32_STENCIL8           = 261,
	X24_STENCIL8           = 262,
	UNSPECIALIZED          = 263,
}
