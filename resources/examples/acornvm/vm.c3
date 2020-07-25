module acornvm::vm;


void vm_litinit(Value th); // Initializer for literals
void vm_stdinit(Value th); // Initializer for standard symbols
void core_init(Value th); // Initialize all core types


/** Manage the Virtual Machine instance.
 *
 * This is the heart of the Acorn Virtual Machine. It manages:
 * - All memory and garbage collection (avm_memory.h), working with the
 *   different encoding types.
 * - The symbol table, which is shared across everywhere
 * - The main thread, which is the recursive root for garbage collection.
 *   The thread manages the global namespace, including all registered
 *   core types (including the Acorn compiler and resource types).
 *
 * See newVm() for more detailed information on VM initialization.
 *
 * @file
 *
 * This source file is part of avm - Acorn Virtual Machine.
 * See Copyright Notice in avm.h
*/


	/** Virtual Machine instance information
	 *  Is never garbage collected, but is the root for garbage collection. */
struct VmInfo
{
    inline MemCommonInfoGray;	//!< Common header for value-containing object

	ulong pcgrng_state;		    //!< PCG random-number generator state
	ulong pcgrng_inc;	    	//!< PCG random-number generator inc value

	Value global;				//!< VM's "built in" Global hash table

	Value main_thread;			//!< VM's main thread
	ThreadInfo main_thr;        //!< State space for main thread

	SymTable sym_table;	        //!< global symbol table
	AuintIdx hashseed;			//!< randomized seed for hashing strings
	Value literals;				//!< array of all built-in symbol and type literals
	Value stdidx;				//!< Table to convert std symbol to index
	Value* stdsym;				//!< c-array to convert index to std symbol

		// Garbage Collection state
	MemInfo* objlist;			//!< linked list of all collectable objects
	MemInfo** sweepgc;			//!< current position of sweep in list 'objlist'
	MemInfoGray* gray;			//!< list of gray objects
	MemInfo* threads;			//!< list of all threads

	Auint sweepsymgc;			//!< position of sweep in symbol table

		// Metrics used to govern when GC runs
	int gctrigger;				//!< Memory alloc will trigger GC step when this >= 0
	int gcstepdelay;			//!< How many alloc's to wait between steps
	int gcnbrnew;				//!< number of new objects created since last GC cycle
	int gcnbrold;				//!< number of old objects created since last gen GC cycle
	int gcnewtrigger;			//!< Start a new GC cycle after this exceeds gcnbrnew
	int gcoldtrigger;			//!< Make next GC cycle full if this exceeds gcnbrold
	int gcstepunits;			//!< How many work units left to consume in GC step

	// Statistics gathering for GC
	int gcnbrmarks;				//!< How many objects were marked this cycle
	int gcnbrfrees;				//!< How many objects were freed during cycle's sweep
	int gcmicrodt;				//!< The clock's micro-seconds measured at start of cycle

	Auint totalbytes;			//!< number of bytes currently allocated

	char gcmode;				//!< Collection mode: Normal, Emergency, Gen
	char gcnextmode;			//!< Collection mode for next cycle
	char gcstate;				//!< state of garbage collector
	char gcrunning;				//!< true if GC is running
	char currentwhite;			//!< Current white color for new objects

	char gcbarrieron;			//!< Is the write protector on? Yes prevents black->white
}

/** Mark all in-use thread values for garbage collection
 * Increments how much allocated memory the thread uses.
 */
macro @vmMark(th, v)
{
	mem_markobj(th, v.main_thread);
	mem_markobj(th, v.global);
	mem_markobj(th, v.literals);
	mem_markobj(th, v.stdidx);
}

macro vmStdSym(th, idx) { return vm(th).stdsym[idx]; }

const N_STD_SYM = 256;

	/** C index values for all VM literal values used throughout the code
	    for common symbols and core types. They are forever immune from garbage collection
		by being anchored to the VM. */
enum VmLiteral : int (string name = nil)
{
	// Compiler symbols
	SYM_NULL("null"),
	SYM_FALSE("false"),
	SYM_TRUE("true"),
	SYM_AND("and"),
	SYM_ASYNC("async"),
	SYM_BASE_URL("baseurl"),
	SYM_BREAK("break"),
	SYM_CONTEXT("context"),
	SYM_CONTINUE("continue"),
	SYM_DO("do"),
	SYM_EACH("each"),
	SYM_ELSE("else"),
	SYM_ELIF("elif"),
	SYM_IF("if"),
	SYM_IN("in",
	SYM_INTO("into"),
    SYM_MATCH("match"),
    SYM_NOT("not"),
    SYM_OR("or"),
    SYM_RETURN("return"),
    SYM_SELF("self",
    SYM_SELF_METH("selfmethod"),
	SYM_THIS("this"),
	SYM_USING("using"),
		SymVar,			//!< 'var'
	SYM_WAIT("wait"),
	SYM_WHILE("while"),
	SYM_WITH("with"),
	SYM_YIELD("yield",

	SYM_LBRACE("{"),
	SYM_RBRACE("}"),
	SYM_SEMICOLON(";"),
	SYM_COMMA(","),
	SYM_QUESTION("?"),
	SYM_AT("@"),
	SYM_SPLAT("..."),
	SYM_DOT("."),
	SYM_COLONS("::"),
	SYM_DOT_COLON(".:"),

	// Compiler symbols that are also methods
	SYM_APPEND("<<"),
	SYM_PREPENT(">>"),
	SYM_PLUS("+"),
	SYM_MINUS("-"),
	SYM_MULT("*"),
	SYM_DIV("/"),
	SYM_ROCKET("<=>"),
	SYM_EQUIV("==="),
	SYM_MATCHOP("~~"),
	SYM_LT("<"),
	SYM_LE("<="),
	SYM_GT(">"),
	SYM_GE(">="),
	SYM_EQ("=="),
	SYM_NE("!="),

	// Methods that are not compiler symbols
    // Byte-code (and parser) standard methods
    SYM_NEW("New"),
    SYM_INIT("Init"),
    SYM_LOAD("Load"),
    SYM_GET("Get"),
    SYM_PARAS("()"),
    SYM_BRACKETS("[]"),
    SYM_NEG("-@"),
    SYM_VALUE("value"),
    SYM_EACH_METH("Each"),
    SYM_BEGIN("Begin"),
    SYM_END("End"),

    SYM_FINALIZER("_finalizer") // method for CData
    SYM_NAME('_type')           // symbol

	// AST symbols
	SYM_METHOD("method"),
	SYM_ASSGN("="),
	SYM_OR_ASSGN("||="),
	SYM_COLON(":"),
	SYM_THIS_BLOCK("thisblock"),
	SYM_CALL_PROP("callprop"),
	SYM_ACT_PROP("activeprop"),
	SYM_RAW_PROP("rawprop"),
	SYM_LOCAL("local"),
	SYM_LIT("lit"),
	SYM_EXT("ext"),
	SYM_RANGE("Range"),
	SYM_CLOSURE("Closure"),
	SYM_YIELDER("Yielder"),
	SYM_RESOURCE("Resource"),

		// Core type type
		TypeObject,	//!< Type
		TypeMixinc,  //!< Mixin class
		TypeMixinm, //!< Mixin mixin
		TypeNullc,	//!< Null class
		TypeNullm,	//!< Null mixin
		TypeBoolc,	//!< Float class
		TypeBoolm,	//!< Float mixin
		TypeIntc,	//!< Integer class
		TypeIntm,	//!< Integer mixin
		TypeFloc,	//!< Float class
		TypeFlom,	//!< Float mixin
		TypeMethc,	//!< Method class
		TypeMethm,	//!< Method mixin
		TypeYieldc,	//!< Yielder class
		TypeYieldm,	//!< Yielder mixin
		TypeVmc,	//!< Vm class
		TypeVmm,	//!< Vm mixin
		TypeSymc,	//!< Symbol class
		TypeSymm,	//!< Symbol mixin
		TypeRangec, //!< Range class
		TypeRangem, //!< Range mixin
		TypeTextc,	//!< Text class
		TypeTextm,	//!< Text mixin
		TypeListc,	//!< List class
		TypeListm,	//!< List mixin
		TypeCloc,	//!< Closure class
		TypeClom,	//!< Closure mixin
		TypeIndexc,	//!< Index class
		TypeIndexm,	//!< Index mixin
		TypeResc,	//!< Index class
		TypeResm,	//!< Index mixin
		TypeAll,	//!< All

		//! Number of literals
		nVmLits
}

macro vmlit!(VmLiteral lit)
{
    return arr_inf(vm(th)->literals)->arr[list];
}



/** Used by vm_init to build random seed */
macro memcpy_Auint(i, val)
{
    Auint anint = @cast(val as Auint);
	memcpy(seedstr + i * sizeof(Auint), &anint, sizeof(Auint));
}

/** Create and initialize new Virtual Machine
 * When a VM is started:
 * - Iit dynamically allocates the VmInfo
 *   which holds all universal information about the VM instance.
 * - Memory management and garbage collection (avm_memory.h) is managed at this level.
 *   The GC root value (the main thread) determines what allocated values to keep
 *   and which to discard.
 * - All value encodings are initialized next, including the single symbol table
 *   used across the VM.
 * - The main thread is started up, initializing its global namespace.
 * - All core types are progressively loaded, establishing the default types for
 *   each encoding. This includes the resource types and Acorn compiler. */
func Value new(void)
{
	logInfo(AVM_RELEASE " started.");

	// Create VM info block and start up memory management
	VmInfo* vm = @amalloc(VmInfo);
	vm.enctyp = VmEnc;
	mem_init(vm); /* Initialize memory & garbage collection */

	// VM is GC Root: Never marked or collected. Black will trigger write barrier
	vm.marked = bitmask(BLACKBIT);

	// Initialize main thread (allocated as part of VmInfo)
	Value th = cast(vm->main_thread = &vm->main_thr as Value);
	ThreadInfo* threadInfo = cast(th as threadInfo);
	threadInfo.marked = vm.currentwhite;
	threadInfo.enctyp = ThrEnc;
	threadInfo.next = nil;
	thrInit(&vm.main_thr, vm, aNull, STACK_NEWSIZE, 0);
	vm.threads = nil;

	// Initialize PCG random number generator to starting values
	vm.pcgrng_state = 0x853c49e6748fea9b;
	vm.pcgrng_inc = 0xda3e39cb94b95bdb;

	// Compute a randomized seed, using address space layout to increaase randomness
	// Seed is used to help calculate randomly distributed symbol hashes
	char seedstr[4 * sizeof(Auint)];
	Time timehash = time(nil);
	memcpy_Auint(0, vm)			// heap pointe
	memcpy_Auint(1, timehash)	// current time in seconds
	memcpy_Auint(2, &timehash)	// local variable pointe
	memcpy_Auint(3, &newVM)		// public function
	vm->hashseed = tblCalcStrHash(seedstr, sizeof(seedstr), (AuintIdx) timehash);

	// Initialize vm-wide symbol table, global table and literals
	sym_init(th); // Initialize hash table for symbols
	newTbl(th, &vm->global, aNull, GLOBAL_NEWSIZE); // Create global hash table
	mem_markChk(th, vm, vm->global);
	vm_litinit(th); // Load reserved and standard symbols into literal list
	core_init(th); // Load up global table and literal list with core types
	setType(th, vm->global, vmlit(TypeIndexm)); // Fix up type info for global table

	// Initialize byte-code standard methods and the Acorn compiler
	vm_stdinit(th);

	// Start garbage collection
	mem_gcstart(th);

	return th;
}

/* Close down the virtual machine, freeing all allocated memory */
void vmClose(Value th) {
	th = vm(th)->main_thread;
	VmInfo* vm = vm(th);
	mem::freeAll(th);  /* collect all objects */
	mem::reallocvector(th, vm->stdsym, nStdSym, 0, Value);
	sym_free(th);
	thrFreeStacks(th);
	assert(vm(th)->totalbytes == sizeof(VmInfo));
	mem::frealloc(vm(th), 0);  /* free main block */
	logInfo(AVM_RELEASE " ended.");
}

/* Lock the Vm */
void vm_lock(Value th)
{
}

/* Unlock the Vm */
void vm_unlock(Value th)
{
}

/* Interval timer */
$if ($platform == "win32" || $platform == "win64")
{

int64_t vmStartTimer()
{
	LARGE_INTEGER li;
	QueryPerformanceCounter(&li);
	return li.QuadPart;
}

float vmEndTimer(int64_t starttime)
{
	LARGE_INTEGER now, freq;
	QueryPerformanceCounter(&now);
	QueryPerformanceFrequency(&freq);
	return float(now.QuadPart-starttime)/float(freq.QuadPart);
}

}
$else
{
func int64_t vmStartTimer()
{
	TimeVal start;
	start.gettimeofday();
	return start.tv_sec * 1000000 + start.tv_usec;
}

float vmEndTimer(int64_t starttime)
{
	TimeVal now;
	now.gettimeofday();
	int64_t end = now.tv_sec * 1000000 + end.tv_usec;
    return @cast(end - starttime)/1000000.0 as float);
}
}

#include <stdarg.h>
/* Log a message to the logfile */

void vmLog(const char *msg, ...)
{
	// Start line with timestamp
	time_t ltime;
	char timestr[80];
	ltime=time(NULL);
	strftime (timestr, sizeof(timestr), "%X %x  ", localtime(&ltime));
	fputs(timestr, stderr);

	// Do a formatted output, passing along all parms
	va_list argptr;
	va_start(argptr, msg);
	vfprintf(stderr, msg, argptr);
	va_end(argptr);
	fputs("\n", stderr);

	// Ensure log file gets it
	fflush(stderr);
}

/** Mapping structure correlating a VM literal symbol's number with its name */
struct vmLitSymEntry
{
	int litindex;		//!< Literal symbol's number
	string symnm;	    //!< Literal symbol's string
};

/** Constant array that identifies and maps all VM literal symbols */
vmLitSymEntry[+] vmLitSymTable = {
	// Compiler reserved names



	// End of literal table
	{0, NULL}
};

/** Initialize vm's literals. */
void vm_litinit(Value th) {
	// Allocate untyped array for literal storage
	VmInfo* vm = vm(th);
	newArr(th, &vm->literals, aNull, nVmLits);
	mem_markChk(th, vm, vm->literals);
	arrSet(th, vm->literals, nVmLits-1, aNull);  // Ensure it is full with nulls

	Value *vmlits = arr_info(vm->literals)->arr;
	vmlits[TypeObject] = aNull;

	// Load up literal symbols from table
	const struct vmLitSymEntry *vmlittblp = &vmLitSymTable[0];
	for (VmLiteral i = 0; i <= SYM_RESOUCE; i++)
	{
	    newSym(th, &vmlits[i], i.name);
	}
}

/** Map byte-code's standard symbols to VM's literals (max. number at 256) */
const int stdTblMap[] = {
	// Commonly-called methods
	SYM_NEW,		// 'new'
	SYM_PARAS,	    // '()'
	SYM_APPEND,	    // '<<'
	SYM_PLUS,	    // '+'
	SYM_MINUS,	    // '-'
	SYM_MULT,	    // '*'
	SYM_DIV,		// '/'
	SYM_NET,		// '-@'
	-1
};

/** Initialize vm's standard symbols */
void vm_stdinit(Value th) {
	// Allocate mapping tables
	VmInfo* vm = vm(th);
	Value stdidx =  newTbl(th, &vm->stdidx, aNull, nStdSym);
	mem_markChk(th, vm, vm->stdidx);
	vm->stdsym = NULL;
	mem_reallocvector(th, vm->stdsym, 0, nStdSym, Value);

	// Populate the mapping tables with the corresponding VM literals
	const int *mapp = &stdTblMap[0];
	int idx = 0;
	while (*mapp >= 0 && idx<nStdSym) {
		tblSet(th, stdidx, vmlit(*mapp), anInt(idx));
		vm->stdsym[idx] = vmlit(*mapp);
		idx++;
		mapp++;
	}
}

void core_null_init(Value th);
void core_bool_init(Value th);
void core_int_init(Value th);
void core_float_init(Value th);
void core_symbol_init(Value th);
void core_range_init(Value th);
void core_text_init(Value th);
void core_list_init(Value th);
void core_clo_init(Value th);
void core_index_init(Value th);
void core_object_init(Value th);
void core_mixin_init(Value th);

void core_thread_init(Value th);
void core_vm_init(Value th);
void core_all_init(Value th);

void core_resource_init(Value th);
void core_method_init(Value th);
void core_file_init(Value th);

/** Initialize all core types */
void core_init(Value th) {

	core_object_init(th); // Type must be first, so other types can use this as their type
	vmlit(TypeAll) = pushType(th, aNull, 0);
	popGloVar(th, "All");
	core_mixin_init(th);

	core_null_init(th);
	core_bool_init(th);
	core_int_init(th);
	core_float_init(th);
	core_symbol_init(th);
	core_range_init(th);
	core_text_init(th);
	core_list_init(th);
	core_clo_init(th);
	core_index_init(th);

	core_thread_init(th);
	core_vm_init(th);
	core_all_init(th);

	// Load resource before the types it uses
	core_resource_init(th);
	core_method_init(th);
	core_file_init(th);
}

