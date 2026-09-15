module std::os::darwin::objc @feat(DARWIN) @link("CoreFoundation.framework");

typedef ObjClass = void*;
typedef ObjMethod = void*;
typedef ObjIvar = void*;
typedef ObjSel @constinit = void*;

typedef ObjId @constinit = void*;
alias SendVoid = fn void(ObjId, ObjSel);
alias SendId = fn ObjId(ObjId, ObjSel);
alias ObjcVoidBool = fn void(ObjId, ObjSel, bool);

extern fn void msgSend(...) @cname("objc_msgSend") @builtin;
extern fn ObjSel sel_getUid(ZString);
extern fn ObjClass allocateClassPair(ObjClass superclass, ZString name, sz extra_bytes) @cname("objc_allocateClassPair");
extern fn void registerClassPair(ObjClass cls) @cname("objc_registerClassPair");
extern fn void* objc_autoreleasePoolPush();
extern fn void objc_autoreleasePoolPop(void* context);
extern fn ObjClass getClass(ZString name) @cname("objc_getClass");
extern fn int getClassList(ObjClass* buffer, int buffer_count) @cname("objc_getClassList");
extern fn ObjClass lookUpClass(ZString name) @cname("objc_lookUpClass");

extern fn ZString class_getName(ObjClass cls);
extern fn ObjClass class_getSuperclass(ObjClass cls);
extern fn ObjMethod class_getClassMethod(ObjClass cls, ObjSel name);
extern fn bool class_respondsToSelector(ObjClass cls, ObjSel name);
extern fn ObjSel sel_registerName(ZString str);
extern fn bool class_addIvar(ObjClass cls, ZString name, int size, char alignment, ZString types);
extern fn bool class_addMethod(ObjClass cls, ObjSel name, void* imp, ZString types);

extern fn ObjIvar object_getInstanceVariable(ObjId id, ZString name, void* outValue);
extern fn ObjIvar object_setInstanceVariable(ObjId id, ZString name, void* value);

macro ZString ObjClass.name(ObjClass cls) => class_getName(cls);
macro ObjClass ObjClass.superclass(ObjClass cls) => class_getSuperclass(cls);
macro bool ObjClass.responds_to(ObjClass cls, ObjSel sel) => class_respondsToSelector(cls, sel);
macro ObjMethod ObjClass.method(ObjClass cls, ObjSel name) => class_getClassMethod(cls, name);
macro ObjClass ObjClass.new_subclass(self, ZString name, sz extra_bytes = 0) => allocateClassPair(self, name, extra_bytes);


macro bool ObjSel.equals(ObjSel a, ObjSel b) => a == b;

macro ObjClass class(ZString c) => objc::lookUpClass(c);
macro void register_new_subclass(ObjClass cls) => registerClassPair(cls);

macro ObjIvar ObjId.set_ivar(self, ZString name, void* value) => object_setInstanceVariable(self, name, value);
macro ObjIvar ObjId.get_ivar(self, ZString name, void* value) => object_getInstanceVariable(self, name, value);
macro void ObjId.release(self) => objc::msg_send(self, SendVoid, "release");
macro ObjId ObjId.retain(self) => retain(self);

fn void release(ObjId id) => objc::msg_send(id, SendVoid, "release");
fn ObjId retain(ObjId id)
{
	if (id) objc::msg_send(id, SendId, "retain");
	return id;
}

macro ObjClass @class(ZString $class)
{
	static ObjClass cls;
	if (cls) return cls;
	return cls = lookUpClass($class);
}

macro ObjClass[] class_get_list(Allocator allocator)
{
	int num_classes = objc::getClassList(null, 0);
	if (!num_classes) return {};
	ObjClass[] entries = allocator.new_array(ObjClass, num_classes);
	objc::getClassList(entries.ptr, entries.len);
	return entries;
}


macro msg_send(id, $FunctionType, ZString $selector, ...)
{
	return (($FunctionType)&msgSend)((ObjId)id, sel_getUid($selector), ...$vaarg);
}

fn ObjId alloc(ObjClass cls)
{
	static ObjSel s;
	if (!s) s = sel_getUid("alloc");
	return ((SendId)&msgSend)((ObjId)cls, s);
}

fn ObjId alloc_by_name(ZString name)
{
	ObjClass cls = objc::lookUpClass(name);
	if (!cls) return null;
	return alloc(cls);
}

macro ObjId.send(self, $RType, ZString $selector, ...)  => send(self, $RType, $selector, ...$vaarg);
macro ObjId.send_void(self, ZString $selector, ...)  => send_void(self, $selector, ...$vaarg);

macro send_void(id, ZString $selector, ...)
{
	typeid[] $types = { };
	var $string = "var $FunctionType = $Typeof(fn void(ObjId, ObjSel";
	$for sz $i = 0; $i < $vaarg.len; $i++:
		$types = $types +++ $Typeof($vaarg[$i]);
		$string = $string +++ @sprintf(",$Typefrom($types[%s])", $i);
	$endfor
	$string = $string +++ ") => {});";
	$expand($string);
	if (id)
	{
		return (($FunctionType)&msgSend)((ObjId)id, sel_getUid($selector), ...$vaarg);
	}
}

macro send(id, $RType, ZString $selector, ...)
{
	typeid[] $types = { };
	var $string = "var $FunctionType = $Typeof(fn $RType(ObjId, ObjSel";
	$for sz $i = 0; $i < $vaarg.len; $i++:
		$types = $types +++ $Typeof($vaarg[$i]);
		$string = $string +++ @sprintf(",$Typefrom($types[%s])", $i);
	$endfor
	$string = $string +++ ") => {});";
	$expand($string);
	if (id)
	{
		return (($FunctionType)&msgSend)((ObjId)id, sel_getUid($selector), ...$vaarg);
	}
	return ($RType){};
}

macro bool ObjClass.equals(ObjClass a, ObjClass b) => a == b;
macro bool ObjClass.add_ivar(self, ZString name, $Type, ZString encoding)
{
	var $align = 0;
	var $size = $Type::alignment;
	$for var $i = 0; $size > 1; $i++:
		$size /= 2;
		$align++;
	$endfor
	return class_addIvar(self, name, $Type::size, $align, encoding);
}

macro bool ObjClass.add_method(self, ZString name, void* imp, ZString encoding)
{
	return class_addMethod(self, sel_registerName(name), imp, encoding);
}

macro void @autoreleasepool(;@body())
{
	void* ctx = objc_autoreleasePoolPush();
	defer objc_autoreleasePoolPop(ctx);
	@body();
}

alias pool_push = objc_autoreleasePoolPush;
alias pool_pop = objc_autoreleasePoolPop;
