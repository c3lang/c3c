module std::core::machoruntime @feat(DARWIN) @private;

extern Callback c3_dtor_start @asmname("section$start$__DATA$__c3dtor");
extern Callback c3_dtor_end   @asmname("section$end$__DATA$__c3dtor");

Callback sentinel_ctor @section("__DATA,__c3ctor") @nostrip;
Callback sentinel_dtor @section("__DATA,__c3dtor") @nostrip;

extern fn int _dyld_image_count();
extern fn void* _dyld_get_image_header(int index);
extern fn char* getsectiondata(void* header, char* segname, char* sectname, usz* size);

macro section_body(first, end)
{
	return first[:(end - first)];
}

fn DataType[] section_data(void* header, char* sectname) <DataType>
{
	usz size;
	DataType* data = (void*)getsectiondata(header, "__DATA", sectname, &size);
	if (!data) return {};
	return data[:size / DataType::size];
}

alias CallbackFn = fn void();
struct Callback
{
	int priority;
	CallbackFn xtor;
	Callback* next;
}
struct DynamicMethod
{
	void* fn_ptr;
	char* sel;
	union
	{
		DynamicMethod* next;
		TypeId* type;
	}
}

enum StartupState
{
	NOT_STARTED,
	INIT,
	RUN_CTORS,
	READ_DYLIB,
	RUN_DTORS,
	SHUTDOWN
}

StartupState runtime_state = NOT_STARTED;

Callback* dtor_first;

fn bool claim_image(Callback[] ctors)
{
	foreach (&cb : ctors)
	{
		if (cb.xtor) continue;
		return mem::compare_exchange(&cb.priority, 0, 1) == 0;
	}
	return false;
}

fn void runtime_startup() @cname("__c3_runtime_startup") @nostrip
{
	if (runtime_state != NOT_STARTED) return;
	runtime_state = INIT;

	foreach (&cb : section_body(&c3_dtor_start, &c3_dtor_end))
	{
		if (!cb.xtor) continue; // Sentinel
		append_xxlizer(&dtor_first, cb, false);
	}

	Callback* ctor_first;
	int count = _dyld_image_count();
	// Dylibs in load order first, the main executable (index 0) last.
	for (int k = 1; k <= count; k++)
	{
		void* header = _dyld_get_image_header(k == count ? 0 : k);
		if (!header) continue;
		Callback[] ctors = section_data{Callback}(header, "__c3ctor");
		if (!claim_image(ctors)) continue;
		foreach (&dm : section_data{DynamicMethod}(header, "__c3_dynamic"))
		{
			TypeId* type = dm.type;
			dm.next = type.dtable;
			type.dtable = dm;
		}
		foreach (&cb : ctors)
		{
			if (!cb.xtor) continue; // Sentinel
			append_xxlizer(&ctor_first, cb, true);
		}
	}

	runtime_state = RUN_CTORS;
	Callback* ctor = ctor_first;
	while (ctor)
	{
		ctor.xtor();
		ctor = ctor.next;
	}
	assert(runtime_state == RUN_CTORS);
	runtime_state = READ_DYLIB;
}

fn void runtime_finalize() @cname("__c3_runtime_finalize") @nostrip
{
	if (runtime_state != READ_DYLIB) return;
	runtime_state = RUN_DTORS;
	Callback* dtor = dtor_first;
	while (dtor)
	{
		dtor.xtor();
		dtor = dtor.next;
	}
	assert(runtime_state == RUN_DTORS);
	runtime_state = SHUTDOWN;
}

fn void append_xxlizer(Callback** ref, Callback* cb, bool is_ctor)
{
	while (Callback* current = *ref, current)
	{
		if ((is_ctor && current.priority > cb.priority) || (!is_ctor && current.priority <= cb.priority))
		{
			cb.next = current;
			break;
		}
		ref = &current.next;
	}
	*ref = cb;
}

struct TypeId
{
	char type;
	TypeId* parentof;
	DynamicMethod* dtable;
	sz sizeof;
	TypeId* inner;
	sz len;
	typeid[*] additional;
}
