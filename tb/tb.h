//
//  _______ _ _     _      ____  ______
// |__   __(_) |   | |    |  _ \|  ____|
//    | |   _| | __| | ___| |_) | |__
//    | |  | | |/ _` |/ _ \  _ <|  __|
//    | |  | | | (_| |  __/ |_) | |____
//    |_|  |_|_|\__,_|\___|____/|______|
//
//    It's a relatively small compiler backend all
//    behind a simple C API!
//
#ifndef _TILDEBACKEND_H_
#define _TILDEBACKEND_H_

#ifdef __cplusplus
extern "C" {
#endif

// Windows likes it's secure functions, i kinda do too
// but only sometimes and this isn't one of them
#if defined(_WIN32) && !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// https://semver.org/
#define TB_VERSION_MAJOR 0
#define TB_VERSION_MINOR 2
#define TB_VERSION_PATCH 0

#ifndef TB_MAX_THREADS
#define TB_MAX_THREADS 16
#endif

// Per-thread
#ifndef TB_TEMPORARY_STORAGE_SIZE
#define TB_TEMPORARY_STORAGE_SIZE (1 << 20)
#endif

#ifndef TB_MAX_FUNCTIONS
#define TB_MAX_FUNCTIONS (1 << 22)
#endif

#define TB_API extern

#define TB_HOST_UNKNOWN 0
#define TB_HOST_X86_64  1

// If on, the labels aren't marked in the object file
// might save on performance at the cost of some assembly
// readability.
#define TB_STRIP_LABELS 1

#if defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64)
#define TB_HOST_ARCH TB_HOST_X86_64
#else
#define TB_HOST_ARCH TB_HOST_UNKNOWN
#endif

typedef enum {
	// No overflow will assume the value does not
	// overflow and if it does this can be considered
	// undefined behavior with unknown consequences.
	// no signed wrap
	TB_ASSUME_NSW,
	// no unsigned wrap
	TB_ASSUME_NUW,

	// Wrapping will allow the integer to safely wrap.
	TB_CAN_WRAP,

	// Overflow check will throw an error if the result
	// cannot be represented in the resulting type.
	TB_SIGNED_TRAP_ON_WRAP,
	TB_UNSIGNED_TRAP_ON_WRAP,

	// Saturated arithmatic will clamp the results in the
	// event of overflow/underflow.
	TB_SATURATED_UNSIGNED,
	TB_SATURATED_SIGNED
} TB_ArithmaticBehavior;

typedef enum {
	TB_DEBUGFMT_NONE,

	TB_DEBUGFMT_DWARF,
	TB_DEBUGFMT_CODEVIEW,

	TB_DEBUGFMT_COLINPILLED
} TB_DebugFormat;

typedef enum {
	TB_ARCH_UNKNOWN,

	TB_ARCH_X86_64,
	// unsupported but planned
	TB_ARCH_AARCH64
} TB_Arch;

typedef enum {
	TB_SYSTEM_WINDOWS,
	TB_SYSTEM_LINUX,

	// TODO(NeGate): Actually implement these lol
	TB_SYSTEM_MACOS,
	TB_SYSTEM_ANDROID
} TB_System;

typedef enum {
	TB_CDECL,
	TB_STDCALL
} TB_CallingConv;

typedef struct TB_FeatureSet {
	struct {
		bool sse3 : 1;

		bool popcnt : 1;
		bool lzcnt : 1;
		bool sse41 : 1;
		bool sse42 : 1;

		bool clmul : 1;
		bool f16c : 1;

		bool bmi1 : 1;
		bool bmi2 : 1;

		bool avx : 1;
		bool avx2 : 1;
	} x64;
	struct {
		bool bf16 : 1;
	} aarch64;
} TB_FeatureSet;

typedef enum
{
	TB_BRANCH_HINT_NONE,
	TB_BRANCH_HINT_LIKELY,
	TB_BRANCH_HINT_UNLIKELY
} TB_BranchHint;

typedef enum
{
	TB_LINKAGE_PUBLIC,
	TB_LINKAGE_PRIVATE
} TB_Linkage;

typedef enum
{
	// data is the default way to handle storage, this is usually
	// passed onto the .data section
	TB_STORAGE_DATA,

	// Thread local storage will have the values defined per thread
	// and only accessible within that thread
	TB_STORAGE_TLS
} TB_StorageClass;

typedef enum
{
	TB_MEM_ORDER_RELAXED,
	TB_MEM_ORDER_CONSUME,
	TB_MEM_ORDER_ACQUIRE,
	TB_MEM_ORDER_RELEASE,
	TB_MEM_ORDER_ACQ_REL,
	TB_MEM_ORDER_SEQ_CST,
} TB_MemoryOrder;

typedef enum
{
	// no optimizer run
	TB_OPT_O0,

	// run optimizer with all optimizations
	TB_OPT_O1,

	// same as O1 but favors size, will aggresively deduplicate
	// (at least that's the plan :P)
	TB_OPT_SIZE,

	// same as O1 but favors speed, will aggresively unroll
	// sometimes (at least that's the plan :P)
	TB_OPT_SPEED,
} TB_OptLevel;

typedef enum
{
	// FastISel
	TB_ISEL_FAST,
	TB_ISEL_COMPLEX
} TB_ISelMode;

typedef enum
{
	TB_VOID,
	// Boolean
	TB_BOOL,
	// Integers
	TB_I8,
	TB_I16,
	TB_I32,
	TB_I64,
	// IEEE 754 Floating point
	TB_F32,
	TB_F64,
	// Pointers
	TB_PTR,

	TB_MAX_TYPES
} TB_DataTypeEnum;

typedef enum
{
	TB_NULL = 0,

	/* metadata */
	TB_LINE_INFO,
	TB_KEEPALIVE,
	TB_POISON,

	TB_ICALL, /* internal use only, inline call */
	TB_CALL,  /* standard function call */
	TB_VCALL, /* virtual call */
	TB_ECALL, /* extern call */

	/* Memory operations */
	TB_STORE,

	TB_MEMCLR,
	TB_MEMCPY,
	TB_MEMSET,
	TB_MEMCMP,
	TB_INITIALIZE,

	/* Atomics */
	TB_ATOMIC_TEST_AND_SET,
	TB_ATOMIC_CLEAR,

	TB_ATOMIC_XCHG,
	TB_ATOMIC_ADD,
	TB_ATOMIC_SUB,
	TB_ATOMIC_AND,
	TB_ATOMIC_XOR,
	TB_ATOMIC_OR,

	TB_ATOMIC_CMPXCHG, /* These are always bundled together */
	TB_ATOMIC_CMPXCHG2,
	TB_DEBUGBREAK,

	/* Terminators */
	TB_LABEL,
	TB_GOTO,
	TB_SWITCH,
	TB_IF,
	TB_RET,
	TB_TRAP,
	TB_UNREACHABLE,

	/* Load */
	TB_LOAD,

	/* Pointers */
	TB_RESTRICT,
	TB_LOCAL,
	TB_PARAM_ADDR,

	TB_PARAM,
	TB_FUNC_ADDRESS,
	TB_EXTERN_ADDRESS,
	TB_GLOBAL_ADDRESS,

	TB_MEMBER_ACCESS,
	TB_ARRAY_ACCESS,

	/* Immediates */
	TB_UNSIGNED_CONST,
	TB_SIGNED_CONST,
	TB_FLOAT_CONST,
	TB_STRING_CONST,

	/* Conversions */
	TB_TRUNCATE,
	TB_FLOAT_EXT,
	TB_SIGN_EXT,
	TB_ZERO_EXT,
	TB_INT2PTR,
	TB_PTR2INT,
	TB_INT2FLOAT,
	TB_FLOAT2INT,
	TB_BITCAST,

	/* Select */
	TB_SELECT,

	/* Unary operations */
	TB_NOT,
	TB_NEG,

	/* Integer arithmatic */
	TB_AND,
	TB_OR,
	TB_XOR,
	TB_ADD,
	TB_SUB,
	TB_MUL,

	TB_SHL,
	TB_SHR,
	TB_SAR,
	TB_UDIV,
	TB_SDIV,
	TB_UMOD,
	TB_SMOD,

	/* Float arithmatic */
	TB_FADD,
	TB_FSUB,
	TB_FMUL,
	TB_FDIV,

	/* Comparisons */
	TB_CMP_EQ,
	TB_CMP_NE,
	TB_CMP_SLT,
	TB_CMP_SLE,
	TB_CMP_ULT,
	TB_CMP_ULE,
	TB_CMP_FLT,
	TB_CMP_FLE,

	/* PHI */
	// NOTE(NeGate): phi1 and phi2 are just to avoid
	// using extra space for the common cases
	TB_PHI1,
	TB_PHI2,
	TB_PHIN,

	// NOTE(NeGate): only used internally, if you
	// see one in normal IR things went wrong in
	// an optimization pass
	TB_PASS,

	// variadic
	TB_VA_START,

	// x86 intrinsics
	TB_X86INTRIN_SQRT,
	TB_X86INTRIN_RSQRT,
} TB_NodeTypeEnum;
typedef uint8_t TB_NodeType;

#define TB_IS_NODE_SIDE_EFFECT(type) ((type) >= TB_LINE_INFO && (type) <= TB_DEBUGBREAK)
#define TB_IS_NODE_TERMINATOR(type)  ((type) >= TB_LABEL && (type) <= TB_RET)

#define TB_IS_INTEGER_TYPE(x) ((x) >= TB_I8 && (x) <= TB_I64)
#define TB_IS_FLOAT_TYPE(x)   ((x) >= TB_F32 && (x) <= TB_F64)
#define TB_IS_POINTER_TYPE(x) ((x) == TB_PTR)

typedef struct {
	uint8_t type;

	// 2^N where N is the width value.
	// Only integers and floats can be wide.
	uint8_t width;
} TB_DataType;

typedef int TB_Label;

typedef struct {
	int32_t  key;
	TB_Label value;
} TB_SwitchEntry;

// just represents some region of bytes, usually in file parsing crap
typedef struct {
	size_t length;
	uint8_t* data;
} TB_Slice;

// represents byte counts
typedef uint32_t TB_CharUnits;

typedef unsigned int TB_AttributeID;
typedef unsigned int TB_FileID;
typedef unsigned int TB_FunctionID;
typedef unsigned int TB_ExternalID; // 0 means NULL
typedef unsigned int TB_GlobalID;
typedef unsigned int TB_InitializerID;

typedef struct TB_Module            TB_Module;
typedef struct TB_Function          TB_Function;
typedef struct TB_AttribList        TB_AttribList;
typedef struct TB_FunctionPrototype TB_FunctionPrototype;

// references to a node within a TB_Function
// these are virtual registers so they don't necessarily
// map to any hardware but instead represent some operation
typedef int TB_Reg, TB_Register;

#define TB_NULL_REG ((TB_Reg)0)
#define TB_REG_MAX  ((TB_Reg)INT_MAX)

typedef struct TB_Node {
	TB_NodeType    type;
	TB_DataType    dt;
	TB_Reg         next;
	TB_AttribList* first_attrib;

	union {
		struct TB_NodeUint {
			uint64_t value;
		} uint;
		struct TB_NodeSint {
			int64_t value;
		} sint;
		struct TB_NodeFloat {
			double value;
		} flt;
		struct TB_NodeString {
			size_t      length;
			const char* data;
		} string;
		struct TB_NodeFunction {
			const TB_Function* value;
		} func;
		struct TB_NodeExtern {
			TB_ExternalID value;
		} external;
		struct TB_NodeGlobal {
			TB_GlobalID value;
		} global;
		struct TB_NodeLine {
			TB_FileID file;
			int       line;
		} line_info;
		struct TB_NodeMemberAccess {
			TB_Reg  base;
			int32_t offset;
		} member_access;
		struct TB_NodeArrayAccess {
			TB_Reg       base;
			TB_Reg       index;
			TB_CharUnits stride;
		} array_access;
		struct TB_NodePtrdiff {
			TB_Reg       a;
			TB_Reg       b;
			TB_CharUnits stride;
		} ptrdiff;
		struct TB_NodeParam {
			uint32_t     id;
			TB_CharUnits size;
		} param;
		struct TB_NodeParamAddr {
			TB_Reg param;

			TB_CharUnits size;
			TB_CharUnits alignment;
		} param_addr;
		struct TB_NodeLocal {
			TB_CharUnits size;
			TB_CharUnits alignment;
		} local;
		struct TB_NodeUnary {
			TB_Reg src;
		} unary;
		struct TB_NodeIArith {
			TB_Reg                a;
			TB_Reg                b;
			TB_ArithmaticBehavior arith_behavior;
		} i_arith;
		struct TB_NodeFArith {
			TB_Reg a;
			TB_Reg b;
		} f_arith;
		struct TB_NodeCompare {
			TB_Reg      a;
			TB_Reg      b;
			TB_DataType dt;
		} cmp;
		struct TB_NodeSelect {
			TB_Reg a;
			TB_Reg b;
			TB_Reg cond;
		} select;
		struct TB_NodeLoad {
			TB_Reg address;
			// this is only here to make load and store
			// payloads match in data layout... just because
			TB_Reg       _;
			TB_CharUnits alignment;
			bool         is_volatile;
		} load;
		struct TB_NodeStore {
			TB_Reg       address;
			TB_Reg       value;
			TB_CharUnits alignment;
			bool         is_volatile;
		} store;
		struct TB_NodeAtomicRMW {
			TB_Reg         addr;
			TB_Reg         src;
			TB_MemoryOrder order;

			// NOTE(NeGate): this is used for fail
			TB_MemoryOrder order2;
		} atomic;
		struct TB_NodeReturn {
			TB_Reg value;
		} ret;
		struct TB_NodePass {
			TB_Reg value;
		} pass;
		struct TB_NodePhi1 {
			TB_Reg a_label;
			TB_Reg a;
		} phi1;
		struct TB_NodePhi2 {
			TB_Reg a_label;
			TB_Reg a;
			TB_Reg b_label;
			TB_Reg b;
		} phi2;
		struct TB_NodePhi {
			int param_start, param_end;
		} phin;
		struct TB_NodeLabel {
			TB_Label id;
			TB_Reg   terminator;
		} label;
		struct TB_NodeIf {
			TB_Reg   cond;
			TB_Label if_true;
			TB_Label if_false;
		} if_;
		struct TB_NodeGoto {
			TB_Label label;
		} goto_;
		struct TB_NodeExternCall {
			int           param_start, param_end;
			TB_ExternalID target;
		} ecall;
		struct TB_NodeDynamicCall {
			int    param_start, param_end;
			TB_Reg target;
		} vcall;
		struct TB_NodeFunctionCall {
			int                param_start, param_end;
			const TB_Function* target;
		} call;
		struct TB_NodeSwitch {
			TB_Reg   key;
			TB_Label default_label;
			int      entries_start, entries_end;
		} switch_;
		struct TB_NodeMemoryOp {
			TB_Reg       dst;
			TB_Reg       src;
			TB_Reg       size;
			TB_CharUnits align;
		} mem_op;
		struct TB_NodeMemoryClear {
			TB_Reg       dst;
			TB_CharUnits size;
			TB_CharUnits align;
		} clear;
		struct TB_NodeInitialize {
			TB_Reg           addr;
			TB_InitializerID id;
		} init;
	};
} TB_Node;
static_assert(sizeof(TB_Node) <= 32, "sizeof(TB_Node) <= 32");

// represents the atomic cmpxchg result since it's two values
typedef struct {
	TB_Reg success;
	TB_Reg old_value;
} TB_CmpXchgResult;

typedef struct {
	size_t count;
	struct TB_Loop {
		int parent_loop;

		// the terminator of the header will exit
		TB_Register header;

		// this is where the contents of the loop begin
		TB_Register body;

		// this is not part of the loop but instead where
		// the loop goes on exit
		TB_Register exit;
	} loops[];
} TB_LoopInfo;

typedef enum {
	TB_OBJECT_RELOC_NONE, // how?

	// Target independent
	TB_OBJECT_RELOC_ADDR32,
	TB_OBJECT_RELOC_ADDR64, // unsupported on 32bit platforms
	TB_OBJECT_RELOC_SECREL,
	TB_OBJECT_RELOC_SECTION,

	// x64 only
	TB_OBJECT_RELOC_REL32,   // relative 32bit displacement
	TB_OBJECT_RELOC_REL32_1, //   plus 1
	TB_OBJECT_RELOC_REL32_2, //   plus 2
	TB_OBJECT_RELOC_REL32_3, //   and so on
	TB_OBJECT_RELOC_REL32_4, //   ...
	TB_OBJECT_RELOC_REL32_5,

	// Aarch64 only
	TB_OBJECT_RELOC_BRANCH26, // 26bit displacement for B and BL instructions
	TB_OBJECT_RELOC_REL21,    // for ADR instructions

	// TODO(NeGate): fill in the rest of this later
} TB_ObjectRelocType;

typedef struct {
	TB_ObjectRelocType type;
	uint32_t symbol_index;
	size_t virtual_address;
} TB_ObjectReloc;

typedef struct {
	TB_Slice name;

	size_t virtual_address;
	size_t virtual_size;

	// You can have a virtual size without having a raw
	// data size, that's how the BSS section works
	TB_Slice raw_data;

	size_t relocation_count;
	TB_ObjectReloc* relocations;
} TB_ObjectSection;

typedef enum {
	TB_OBJECT_SYMBOL_SECTION
} TB_ObjectSymbolType;

typedef struct {
	TB_Slice name;
} TB_ObjectSymbol;

typedef enum {
	TB_OBJECT_FILE_UNKNOWN,

	TB_OBJECT_FILE_COFF,
	TB_OBJECT_FILE_ELF64
} TB_ObjectFileType;

typedef struct {
	TB_ObjectFileType type;
	TB_Arch           arch;

	size_t           symbol_count;
	TB_ObjectSymbol* symbols;

	size_t           section_count;
	TB_ObjectSection sections[];
} TB_ObjectFile;

typedef struct {
	size_t  object_file_count;

	// Name table maps to the object files directly
	char**   object_file_names;
	TB_Slice object_files[];
} TB_ArchiveFile;

// *******************************
// Public macros
// *******************************
#ifdef __cplusplus
#define TB_TYPE_VOID TB_DataType{ TB_VOID }

#define TB_TYPE_I8 TB_DataType{ TB_I8 }
#define TB_TYPE_I16 TB_DataType{ TB_I16 }
#define TB_TYPE_I32 TB_DataType{ TB_I32 }
#define TB_TYPE_I64 TB_DataType{ TB_I64 }

#define TB_TYPE_F32 TB_DataType{ TB_F32 }
#define TB_TYPE_F64 TB_DataType{ TB_F64 }

#define TB_TYPE_BOOL TB_DataType{ TB_BOOL }
#define TB_TYPE_PTR TB_DataType{ TB_PTR }

#else

#define TB_TYPE_VOID (TB_DataType){ TB_VOID, 0 }
#define TB_TYPE_I8   (TB_DataType){ TB_I8, 0 }
#define TB_TYPE_I16  (TB_DataType){ TB_I16, 0 }
#define TB_TYPE_I32  (TB_DataType){ TB_I32, 0 }
#define TB_TYPE_I64  (TB_DataType){ TB_I64, 0 }

#define TB_TYPE_F32  (TB_DataType){ TB_F32, 0 }
#define TB_TYPE_F64  (TB_DataType){ TB_F64, 0 }
#define TB_TYPE_BOOL (TB_DataType){ TB_BOOL, 0 }
#define TB_TYPE_PTR  (TB_DataType){ TB_PTR, 0 }

#endif

typedef void (*TB_PrintCallback)(void* user_data, const char* fmt, ...);

////////////////////////////////
// Module management
////////////////////////////////
// Creates a module with the correct target and settings
TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, TB_DebugFormat debug_fmt, const TB_FeatureSet* features);

// Validates IR & compiles the function into machine code.
// For isel_mode, TB_ISEL_FAST will compile faster but worse codegen
// TB_ISEL_COMPLEX will compile slower but better codegen
//
// returns false if it fails.
TB_API bool tb_module_compile_func(TB_Module* m, TB_Function* f, TB_ISelMode isel_mode);

// Frees all resources for the TB_Module and it's functions, globals and
// compiled code.
TB_API void tb_module_destroy(TB_Module* m);

// When targetting windows & thread local storage, you'll need to bind a tls index
// which is usually just a global that the runtime support has initialized, if you
// dont and the tls_index is used, it'll crash
TB_API void tb_module_set_tls_index(TB_Module* m, TB_ExternalID e);

// Exports an object file with all the machine code and symbols generated.
TB_API bool tb_module_export(TB_Module* m, const char* path);

// For isel_mode, TB_ISEL_FAST will compile faster but worse codegen
// TB_ISEL_COMPLEX will compile slower but better codegen
TB_API void tb_module_export_jit(TB_Module* m, TB_ISelMode isel_mode);

TB_API void* tb_module_get_jit_func_by_name(TB_Module* m, const char* name);
TB_API void* tb_module_get_jit_func_by_id(TB_Module* m, size_t i);
TB_API void* tb_module_get_jit_func(TB_Module* m, TB_Function* f);

// Binds an external to an address
TB_API bool tb_jit_import(TB_Module* m, const char* name, void* address);

TB_API TB_ExternalID tb_extern_create(TB_Module* m, const char* name);
TB_API TB_FileID     tb_file_create(TB_Module* m, const char* path);

////////////////////////////////
// Function Prototypes
////////////////////////////////
// creates a function prototype used to define a function's parameters and
// return type.
//
// function prototypes do not get freed individually and last for the entire run
// of the backend, they can also be reused for multiple functions which have
// matching signatures.
TB_API TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv conv, TB_DataType return_dt, int num_params, bool has_varargs);

// adds a parameter to the function prototype, TB doesn't support struct
// parameters so the frontend must lower them to pointers or any other type
// depending on their preferred ABI.
TB_API void tb_prototype_add_param(TB_FunctionPrototype* p, TB_DataType dt);

// same as tb_prototype_add_param(...) but takes an array
TB_API void tb_prototype_add_params(TB_FunctionPrototype* p, size_t count, const TB_DataType* dt);

// adds a parameter to the function prototype, TB doesn't support struct
// parameters so the frontend must lower them to pointers or any other type
// depending on their preferred ABI.
TB_API TB_Function* tb_prototype_build(TB_Module* m, TB_FunctionPrototype* p, const char* name, TB_Linkage linkage);

////////////////////////////////
// Constant Initializers
////////////////////////////////
// NOTE: the max objects is a cap and thus it can be bigger than the actual
// number used.
TB_API TB_InitializerID tb_initializer_create(TB_Module* m, size_t size, size_t align, size_t max_objects);

// returns a buffer which the user can fill to then have represented in the
// initializer
TB_API void* tb_initializer_add_region(TB_Module* m, TB_InitializerID id, size_t offset, size_t size);

// places a relocation for a global at offset, the size of the relocation
// depends on the pointer size
TB_API void tb_initializer_add_global(TB_Module* m, TB_InitializerID id, size_t offset, TB_GlobalID global);
TB_API void tb_initializer_add_function(TB_Module* m, TB_InitializerID id, size_t offset, TB_FunctionID func);
TB_API void tb_initializer_add_extern(TB_Module* m, TB_InitializerID id, size_t offset, TB_ExternalID external);

////////////////////////////////
// Constant Initializers
////////////////////////////////
TB_API TB_GlobalID tb_global_create(TB_Module* m, const char* name, TB_StorageClass storage, TB_Linkage linkage);
TB_API void tb_global_set_initializer(TB_Module* m, TB_GlobalID global, TB_InitializerID initializer);

////////////////////////////////
// Function Attributes
////////////////////////////////
// These are parts of a function that describe metadata for instructions

// restrict is applied onto loads and store operations meaning that the pointer
// being accessed doesn't alias with any of the other pointers within the scope
TB_API TB_AttributeID tb_function_attrib_restrict(TB_Function* f, TB_AttributeID scope);

// This defines a some scope which can have a set of restrict pointers defined
// in it
TB_API TB_AttributeID tb_function_attrib_scope(TB_Function* f, TB_AttributeID parent_scope);

// places an attribute on a function, note that there's no limit to how many
// registers can share an attribute
TB_API void tb_function_append_attrib(TB_Function* f, TB_Reg r, TB_AttributeID a);

////////////////////////////////
// Function IR Generation
////////////////////////////////
// the user_data is expected to be a valid FILE*
TB_API void tb_default_print_callback(void* user_data, const char* fmt, ...);

// this only allows for power of two vector types
TB_API TB_DataType tb_vector_type(TB_DataTypeEnum type, int width);

TB_API TB_Function* tb_function_clone(TB_Module* m, TB_Function* f, const char* name);
TB_API void tb_function_print(TB_Function* f, TB_PrintCallback callback, void* user_data);

// Prints out the IR in the GraphViz format
TB_API void tb_function_print_cfg(TB_Function* f, TB_PrintCallback callback, void* user_data);
TB_API void tb_function_free(TB_Function* f);

TB_API TB_Label tb_inst_get_current_label(TB_Function* f);
TB_API void tb_inst_loc(TB_Function* f, TB_FileID file, int line);

TB_API void tb_inst_set_scope(TB_Function* f, TB_AttributeID scope);
TB_API TB_AttributeID tb_inst_get_scope(TB_Function* f);

TB_API void tb_inst_debugbreak(TB_Function* f);

TB_API TB_Reg tb_inst_param(TB_Function* f, int param_id);
TB_API TB_Reg tb_inst_param_addr(TB_Function* f, int param_id);

TB_API TB_Reg tb_inst_fpxt(TB_Function* f, TB_Reg src, TB_DataType dt);
TB_API TB_Reg tb_inst_sxt(TB_Function* f, TB_Reg src, TB_DataType dt);
TB_API TB_Reg tb_inst_zxt(TB_Function* f, TB_Reg src, TB_DataType dt);
TB_API TB_Reg tb_inst_trunc(TB_Function* f, TB_Reg src, TB_DataType dt);
TB_API TB_Reg tb_inst_int2ptr(TB_Function* f, TB_Reg src);
TB_API TB_Reg tb_inst_ptr2int(TB_Function* f, TB_Reg src, TB_DataType dt);
TB_API TB_Reg tb_inst_int2float(TB_Function* f, TB_Reg src, TB_DataType dt);
TB_API TB_Reg tb_inst_float2int(TB_Function* f, TB_Reg src, TB_DataType dt);
TB_API TB_Reg tb_inst_bitcast(TB_Function* f, TB_Reg src, TB_DataType dt);

TB_API TB_Reg tb_inst_local(TB_Function* f, uint32_t size, TB_CharUnits align);
TB_API TB_Reg tb_inst_load(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_CharUnits align);
TB_API void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_Reg val, TB_CharUnits align);

TB_API TB_Reg tb_inst_volatile_load(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_CharUnits alignment);
TB_API void tb_inst_volatile_store(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_Reg val, TB_CharUnits alignment);

TB_API TB_Reg tb_inst_bool(TB_Function* f, bool imm);
TB_API TB_Reg tb_inst_ptr(TB_Function* f, uint64_t imm);
TB_API TB_Reg tb_inst_sint(TB_Function* f, TB_DataType dt, int64_t imm);
TB_API TB_Reg tb_inst_uint(TB_Function* f, TB_DataType dt, uint64_t imm);
TB_API TB_Reg tb_inst_float(TB_Function* f, TB_DataType dt, double imm);
TB_API TB_Reg tb_inst_cstring(TB_Function* f, const char* str);
TB_API TB_Reg tb_inst_string(TB_Function* f, size_t len, const char* str);

// Applies an initializer to a memory region
TB_API void tb_inst_initialize_mem(TB_Function* f, TB_Reg addr, TB_InitializerID src);

// Broadcasts 'val' across 'count' elements starting 'dst'
TB_API void tb_inst_memset(TB_Function* f, TB_Reg dst, TB_Reg val, TB_Reg count, TB_CharUnits align);

// performs a copy of 'count' elements from one memory location to another
// both locations cannot overlap.
TB_API void tb_inst_memcpy(TB_Function* f, TB_Reg dst, TB_Reg src, TB_Reg count, TB_CharUnits align);

// Clears a memory region to zeroes
TB_API void tb_inst_memclr(TB_Function* f, TB_Reg addr, TB_CharUnits size, TB_CharUnits align);

// result = base + (index * stride)
TB_API TB_Reg tb_inst_array_access(TB_Function* f, TB_Reg base, TB_Reg index, uint32_t stride);

// result = base + offset
// where base is a pointer
TB_API TB_Reg tb_inst_member_access(TB_Function* f, TB_Reg base, int32_t offset);

TB_API TB_Reg tb_inst_get_func_address(TB_Function* f, const TB_Function* target);
TB_API TB_Reg tb_inst_get_extern_address(TB_Function* f, TB_ExternalID target);
TB_API TB_Reg tb_inst_get_global_address(TB_Function* f, TB_GlobalID target);

// Performs a conditional select between two values, if the operation is
// performed wide then the cond is expected to be the same type as a and b where
// the condition is resolved as true if the MSB (per component) is 1.
//
// result = cond ? a : b
// a, b must match in type
TB_API TB_Reg tb_inst_select(TB_Function* f, TB_Reg cond, TB_Reg a, TB_Reg b);

// Integer arithmatic
TB_API TB_Reg tb_inst_add(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Reg tb_inst_sub(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Reg tb_inst_mul(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Reg tb_inst_div(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);
TB_API TB_Reg tb_inst_mod(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);

// Bitwise operations
TB_API TB_Reg tb_inst_not(TB_Function* f, TB_Reg n);
TB_API TB_Reg tb_inst_neg(TB_Function* f, TB_Reg n);
TB_API TB_Reg tb_inst_and(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_or(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_xor(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_sar(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_shl(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior);
TB_API TB_Reg tb_inst_shr(TB_Function* f, TB_Reg a, TB_Reg b);

// Atomics
// By default you can use TB_MEM_ORDER_SEQ_CST for the memory order to get
// correct but possibly slower results on certain platforms (those with relaxed
// memory models).
TB_API TB_Reg tb_inst_atomic_test_and_set(TB_Function* f, TB_Reg addr, TB_MemoryOrder order);
TB_API TB_Reg tb_inst_atomic_clear(TB_Function* f, TB_Reg addr, TB_MemoryOrder order);

// All atomic operations here return the old value and the operations are
// performed in the same data type as 'src' with alignment of 'addr' being
// the natural alignment of 'src'
TB_API TB_Reg tb_inst_atomic_xchg(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
TB_API TB_Reg tb_inst_atomic_add(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
TB_API TB_Reg tb_inst_atomic_sub(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
TB_API TB_Reg tb_inst_atomic_and(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
TB_API TB_Reg tb_inst_atomic_xor(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
TB_API TB_Reg tb_inst_atomic_or(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);

// if (*addr == expected) {
//   old_value = atomic_xchg(addr, desired);
//   return { true, old_value };
// } else {
//   return { false };
// }
TB_API TB_CmpXchgResult tb_inst_atomic_cmpxchg(TB_Function* f, TB_Reg addr, TB_Reg expected, TB_Reg desired, TB_MemoryOrder succ, TB_MemoryOrder fail);

// Float math
TB_API TB_Reg tb_inst_fadd(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_fsub(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_fmul(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_fdiv(TB_Function* f, TB_Reg a, TB_Reg b);

// Comparisons
TB_API TB_Reg tb_inst_cmp_eq(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_cmp_ne(TB_Function* f, TB_Reg a, TB_Reg b);

TB_API TB_Reg tb_inst_cmp_ilt(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);
TB_API TB_Reg tb_inst_cmp_ile(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);
TB_API TB_Reg tb_inst_cmp_igt(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);
TB_API TB_Reg tb_inst_cmp_ige(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);

TB_API TB_Reg tb_inst_cmp_flt(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_cmp_fle(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_cmp_fgt(TB_Function* f, TB_Reg a, TB_Reg b);
TB_API TB_Reg tb_inst_cmp_fge(TB_Function* f, TB_Reg a, TB_Reg b);

TB_API TB_Reg tb_inst_restrict(TB_Function* f, TB_Reg value);

// General intrinsics
TB_API TB_Reg tb_inst_va_start(TB_Function* f, TB_Reg a);

// x86 Intrinsics
TB_API TB_Reg tb_inst_x86_sqrt(TB_Function* f, TB_Reg a);
TB_API TB_Reg tb_inst_x86_rsqrt(TB_Function* f, TB_Reg a);

// Control flow
TB_API TB_Reg tb_inst_call(TB_Function* f, TB_DataType dt, const TB_Function* target, size_t param_count, const TB_Reg* params);
TB_API TB_Reg tb_inst_vcall(TB_Function* f, TB_DataType dt, TB_Reg target, size_t param_count, const TB_Reg* params);
TB_API TB_Reg tb_inst_ecall(TB_Function* f, TB_DataType dt, const TB_ExternalID target, size_t param_count, const TB_Reg* params);

TB_API TB_Label tb_inst_new_label_id(TB_Function* f);
TB_API TB_Reg tb_inst_phi2(TB_Function* f, TB_Label a_label, TB_Reg a, TB_Label b_label, TB_Reg b);
TB_API TB_Reg tb_inst_label(TB_Function* f, TB_Label id);
TB_API void tb_inst_goto(TB_Function* f, TB_Label id);
TB_API TB_Reg tb_inst_if(TB_Function* f, TB_Reg cond, TB_Label if_true, TB_Label if_false);
TB_API void tb_inst_switch(TB_Function* f, TB_DataType dt, TB_Reg key, TB_Label default_label, size_t entry_count, const TB_SwitchEntry* entries);
TB_API void tb_inst_ret(TB_Function* f, TB_Reg value);

////////////////////////////////
// Optimizer
////////////////////////////////
// Applies single function optimizations until it runs out
TB_API bool tb_function_optimize(TB_Function* f);

// Applies whole program optimizations until it runs out
TB_API bool tb_module_optimize(TB_Module* m);

// analysis
TB_API TB_LoopInfo* tb_function_get_loop_info(TB_Function* f);

// passes
TB_API bool tb_opt_mem2reg(TB_Function* f);
TB_API bool tb_opt_subexpr_elim(TB_Function* f);
TB_API bool tb_opt_dead_expr_elim(TB_Function* f);
TB_API bool tb_opt_dead_block_elim(TB_Function* f);
TB_API bool tb_opt_fold(TB_Function* f);
TB_API bool tb_opt_load_elim(TB_Function* f);
TB_API bool tb_opt_hoist_invariants(TB_Function* f);
TB_API bool tb_opt_inline(TB_Function* f);
TB_API bool tb_opt_hoist_locals(TB_Function* f);
TB_API bool tb_opt_canonicalize(TB_Function* f);
TB_API bool tb_opt_deshort_circuit(TB_Function* f);
TB_API bool tb_opt_remove_pass_node(TB_Function* f);
TB_API bool tb_opt_strength_reduction(TB_Function* f);
TB_API bool tb_opt_compact_dead_regs(TB_Function* f);
TB_API bool tb_opt_copy_elision(TB_Function* f);

// analysis
TB_API TB_LoopInfo* tb_function_get_loop_info(TB_Function* f);

////////////////////////////////
// IR access
////////////////////////////////
TB_API TB_FunctionID tb_function_get_id(TB_Module* m, TB_Function* f);
TB_API TB_Function* tb_function_from_id(TB_Module* m, TB_FunctionID id);

TB_API TB_Reg tb_node_get_last_register(TB_Function* f);

TB_API TB_Node* tb_function_get_node(TB_Function* f, TB_Reg r);

// either an unsigned or signed constant
TB_API bool tb_node_is_constant_int(TB_Function* f, TB_Reg r, uint64_t imm);

// returns true if it's a signed or unsigned constant
// in which case *imm is the raw bits and *is_signed is
// signedness
//
// notes:
//   imm cannot be NULL
//   is_signed can be NULL
TB_API bool tb_node_get_constant_int(TB_Function* f, TB_Reg r, uint64_t* imm, bool* is_signed);

// Returns the size and alignment of a LOCAL node, both must
// be valid addresses
TB_API void tb_get_function_get_local_info(TB_Function* f, TB_Reg r, int* size, int* align);

// is an IF node?
TB_API bool tb_node_is_conditional(TB_Function* f, TB_Reg r);

// is an IF, GOTO, RET, SWITCH, or LABEL node?
TB_API bool tb_node_is_terminator(TB_Function* f, TB_Reg r);

TB_API bool tb_node_is_label(TB_Function* f, TB_Reg r);

TB_API TB_Reg tb_node_store_get_address(TB_Function* f, TB_Reg r);
TB_API TB_Reg tb_node_store_get_value(TB_Function* f, TB_Reg r);

TB_API TB_Reg tb_node_load_get_address(TB_Function* f, TB_Reg r);

// These work for any floating point, comparison, or integer arithmatic ops
TB_API TB_Reg tb_node_arith_get_left(TB_Function* f, TB_Reg r);
TB_API TB_Reg tb_node_arith_get_right(TB_Function* f, TB_Reg r);

////////////////////////////////
// Objects
////////////////////////////////
TB_ArchiveFile* tb_archive_parse_lib(const TB_Slice file);
void tb_archive_free(TB_ArchiveFile* archive);

TB_ObjectFile* tb_object_parse_coff(const TB_Slice file);
void tb_object_free(TB_ObjectFile* obj);

#ifdef __cplusplus
}
#endif
#endif /* _TINYBACKEND_H_ */
