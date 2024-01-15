module std::os::macos::objc @if(env::DARWIN);

distinct Class = void*;
distinct Method = void*;
distinct Ivar = void*;
distinct Selector = void*;

fault ObjcFailure
{
	CLASS_NOT_FOUND
}

macro char* Class.name(Class cls) => _macos_class_getName(cls);
macro Class Class.superclass(Class cls) => _macos_class_getSuperclass(cls);
macro bool Class.responds_to(Class cls, Selector sel) => _macos_class_respondsToSelector(cls, sel);
macro Method Class.method(Class cls, Selector name) => _macos_class_getClassMethod(cls, name);

macro bool Selector.equals(Selector a, Selector b) => a == b;
macro bool Class.equals(Class a, Class b) => a == b;

macro Selector selector_register(char* c) => _macos_sel_registerName(c);
macro Class! class_by_name(char* c)
{
	Class cls = _macos_objc_lookUpClass(c);
	if (!cls) return ObjcFailure.CLASS_NOT_FOUND?;
	return cls;
}

macro Class[] class_get_list(Allocator *allocator = allocator::heap())
{
	int num_classes = _macos_objc_getClassList(null, 0);
	if (!num_classes) return {};
	Class[] entries = allocator.new_array(Class, num_classes);
	_macos_objc_getClassList(entries.ptr, entries.len);
	return entries;
}

extern fn Class _macos_objc_getClass(char* name) @extern("objc_getClass");
extern fn int _macos_objc_getClassList(Class* buffer, int buffer_count) @extern("objc_getClassList");
extern fn char* _macos_class_getName(Class cls) @extern("class_getName");
extern fn Class _macos_class_getSuperclass(Class cls) @extern("class_getSuperclass");
extern fn Method _macos_class_getClassMethod(Class cls, Selector name) @extern("class_getClassMethod");
extern fn bool _macos_class_respondsToSelector(Class cls, Selector name) @extern("class_respondsToSelector");
extern fn Selector _macos_sel_registerName(char* str) @extern("sel_registerName");
extern fn Class _macos_objc_lookUpClass(char* name) @extern("objc_lookUpClass");


