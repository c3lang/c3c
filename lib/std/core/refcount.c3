<*
 Ref provides a general *external* ref counted wrapper for a pointer. For convenience, a ref count of 0
 means the reference is still valid.

 When the rc drops to -1, it will first run the dealloc function on the underlying pointer (if it exists),
 then free the pointer and the atomic variable assuming that they are allocated using the Allocator in the Ref.

 @require !$defined(Type.dealloc) ||| $defined(Type.dealloc(&&(Type){})) : "'dealloc' must only take a pointer to the underlying type"
 @require !$defined(Type.dealloc) ||| $typeof((Type){}.dealloc()) ==  void : "'dealloc' must return 'void'"
*>
module std::core::mem::ref { Type };
import std::thread, std::atomic;

const OVERALIGNED @private = Type.alignof > mem::DEFAULT_MEM_ALIGNMENT;

alias DeallocFn = fn void(void*);

fn Ref wrap(Type* ptr, Allocator allocator = mem)
{
	return { .refcount = allocator::new(allocator, Atomic{int}), .ptr = ptr, .allocator = allocator };
}
<*
 @require $vacount < 2 : "Too many arguments."
 @require $vacount == 0 ||| $defined(Type a = $vaexpr[0]) : "The first argument must be an initializer for the type"
*>
macro Ref new(..., Allocator allocator = mem)
{

	$switch:
		$case OVERALIGNED && !$vacount:
			Type* ptr = allocator::calloc_aligned(allocator, Type.sizeof, Type.alignof)!!;
		$case OVERALIGNED:
			Type* ptr = allocator::malloc_aligned(allocator, Type.sizeof, Type.alignof)!!;
			*ptr = $vaexpr[0];
		$case !$vacount:
			Type* ptr = allocator::calloc(allocator, Type.sizeof);
		$default:
			Type* ptr = allocator::malloc(allocator, Type.sizeof);
			*ptr = $vaexpr[0];
	$endswitch
	return { .refcount = allocator::new(allocator, Atomic{int}),
	         .ptr = ptr,
	         .allocator = allocator };
}

struct Ref
{
	Atomic{int}* refcount;
	Type* ptr;
	Allocator allocator;
}

fn Ref* Ref.retain(&self)
{
	assert(self.refcount != null, "Reference already released");
	assert(self.refcount.load(RELAXED) >= 0, "Retaining zombie");
	self.refcount.add(1, RELAXED);
	return self;
}

fn void Ref.release(&self)
{
	assert(self.refcount != null, "Reference already released");
    assert(self.refcount.load(RELAXED) >= 0, "Overrelease of refcount");
	if (self.refcount.sub(1, RELAXED) == 0)
	{
		thread::fence(ACQUIRE);
		$if $defined(Type.dealloc):
			self.ptr.dealloc();
		$endif
		$if OVERALIGNED:
			allocator::free_aligned(self.allocator, self.ptr);
		$else
			allocator::free(self.allocator, self.ptr);
		$endif
		allocator::free(self.allocator, self.refcount);
		*self = {};
	}
}

module std::core::mem::rc;
import std::thread, std::atomic;

<*
 A RefCounted struct should be an inline base of a struct.
 If a `dealloc` is defined, then it will be called rather than `free`

 For convenience, a ref count of 0 is still valid, and the struct is
 only freed when when ref count drops to -1.

 The macros rc::retain and rc::release must be used on the full pointer,
 not on the RefCounted substruct.

 So `Foo* f = ...; RefCounted* rc = f; rc::release(rc);` will not do the right thing.
*>
struct RefCounted
{
	Atomic{int} refcount;
}

<*
 @require $defined(RefCounted* c = refcounted) : "Expected a ref counted value"
*>
macro retain(refcounted)
{
	if (refcounted)
	{
		assert(refcounted.refcount.load(RELAXED) >= 0, "Retaining zombie");
    	refcounted.refcount.add(1, RELAXED);
	}
	return refcounted;
}

<*
 @require $defined(RefCounted* c = refcounted) : "Expected a ref counted value"
 @require !$defined(refcounted.dealloc()) ||| $typeof(refcounted.dealloc()) == void
   : "Expected refcounted type to have a valid dealloc"
*>
macro void release(refcounted)
{
	if (!refcounted) return;
	assert(refcounted.refcount.load(RELAXED) >= 0, "Overrelease of refcount");
	if (refcounted.refcount.sub(1, RELAXED) == 0)
	{
		thread::fence(ACQUIRE);
		$if $defined(refcounted.dealloc):
			refcounted.dealloc();
		$else
			free(refcounted);
		$endif
	}
}