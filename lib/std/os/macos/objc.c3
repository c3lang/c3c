module std::os::macos::objc @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

distinct ObjcClass = void*;
distinct ObjcMethod = void*;
distinct ObjcIvar = void*;
distinct ObjcSelector = void*;
def ObjcId = void*;

fault ObjcFailure
{
	CLASS_NOT_FOUND
}

macro ZString ObjcClass.name(ObjcClass cls) => class_getName(cls);
macro ObjcClass ObjcClass.superclass(ObjcClass cls) => class_getSuperclass(cls);
macro bool ObjcClass.responds_to(ObjcClass cls, ObjcSelector sel) => class_respondsToSelector(cls, sel);
macro ObjcMethod ObjcClass.method(ObjcClass cls, ObjcSelector name) => class_getClassMethod(cls, name);

macro bool ObjcSelector.equals(ObjcSelector a, ObjcSelector b) => a == b;
macro bool ObjcClass.equals(ObjcClass a, ObjcClass b) => a == b;

macro ObjcClass! class_by_name(ZString c)
{
	ObjcClass cls = objc::lookUpClass(c);
	return cls ?: ObjcFailure.CLASS_NOT_FOUND?;
}

macro ObjcClass[] class_get_list(Allocator allocator = allocator::heap())
{
	int num_classes = objc::getClassList(null, 0);
	if (!num_classes) return {};
	ObjcClass[] entries = allocator.new_array(ObjcClass, num_classes);
	objc::getClassList(entries.ptr, entries.len);
	return entries;
}

extern fn void msgSend(...) @extern("objc_msgSend") @builtin;
extern fn ObjcSelector sel_getUid(ZString);

macro msg_send(id, $FunctionType, ZString $selector, ...)
{
	return (($FunctionType)&msgSend)((ObjcId)id, sel_getUid($selector), $vasplat);
}

extern fn ObjcClass getClass(ZString name) @extern("objc_getClass");
extern fn int getClassList(ObjcClass* buffer, int buffer_count) @extern("objc_getClassList");
extern fn ObjcClass lookUpClass(ZString name) @extern("objc_lookUpClass") @builtin;

extern fn ZString class_getName(ObjcClass cls);
extern fn ObjcClass class_getSuperclass(ObjcClass cls);
extern fn ObjcMethod class_getClassMethod(ObjcClass cls, ObjcSelector name);
extern fn bool class_respondsToSelector(ObjcClass cls, ObjcSelector name);
extern fn ObjcSelector sel_registerName(ZString str);
