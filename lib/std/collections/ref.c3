<*
 Reference wrappers that move a pointer's nullability into its type.

 `NonNull{Type}` is a pointer asserted to never be null. It is `inline`, so it
 converts implicitly to `Type*` and costs nothing to pass around. Its invariant
 rests on contracts: it is checked in safe builds and assumed under `--safe=no`.

 `Nullable{Type}` is a pointer that may be null and cannot be read without
 handling that. It has no implicit conversion to `Type*` and cannot be
 dereferenced; every accessor yields an Optional instead. The type checker
 enforces this in every build, safe or not.

 Widen with `NonNull.to_nullable`, which always succeeds, and narrow with
 `Nullable.to_nonnull`, which returns an Optional.

	int x = 42;
	NonNull{int} nn = ref::nonnull(&x);
	Nullable{int} n = nn.to_nullable();

	if (try p = n.get()) *p = 7;       // checked access
	int v = n.value() ?? -1;           // or fall back to a default
	NonNull{int} back = n.to_nonnull()!!;
*>
module std::collections::ref;
import std::io;

<* A non null reference *>
typedef NonNull <Type> = inline Type*;

<* A reference that may be null, and that cannot be read without checking *>
typedef Nullable <Type> = Type*;

<* Returned when a null pointer is used where a value was required *>
excuse NULL;

<*
 Wrap a pointer in a non-null reference.

 @param [&in] value : "The pointer to wrap, which may not be null"
 @return "A reference holding the pointer"

 @require [value] @kindof(value) == POINTER : "Expected a pointer"
*>
macro nonnull(value) => (NonNull{$Typeof(value)::inner})value;

<*
 Wrap a pointer in a nullable reference.

 @param [in] value : "The pointer to wrap, which may be null"
 @return "A reference holding the pointer"

 @require [value] @kindof(value) == POINTER : "Expected a pointer"
*>
macro nullable(value) => (Nullable{$Typeof(value)::inner})value;

<*
 Wrap a pointer in a non-null reference, failing if it is null.

 @param [in] value : "The pointer to wrap"
 @return "A reference holding the pointer"
 @return? NULL : "If the value was null"

 @require [value] @kindof(value) == POINTER : "Expected a pointer"
*>
macro nonnull_try(value) => value ? nonnull(value) : NULL~;

<*
 Point a non-null reference at a new value.

 @param [&out] self
 @param [&in] value : "The new pointer, which may not be null"
*>
macro void NonNull.set(&self, Type* value)
{
	*self = (NonNull)value;
}


<*
 Point a nullable reference at a new value, which may be null.

 @param [&out] self
 @param [in] value : "The new pointer"
*>
macro void Nullable.set(&self, Type* value)
{
	*self = (Nullable)value;
}

<*
 Point a non-null reference at a new value, failing if it is null.

 The reference is left untouched when the value is null.

 @param [&out] self
 @param [in] value : "The new pointer"
 @return? NULL : "If the value was null"
*>
fn void? NonNull.set_try(&self, Type* value) @inline
{
	if (!value) return NULL~;
	*self = (NonNull)value;
}

<*
 Return the pointer held by a non-null reference.

 @param self : "The reference to unwrap"
 @return "The pointer, which is never null"

 @require (void*)self != null : "The value must be initialized"
 @ensure return != null
*>
macro Type* NonNull.get(self) => (Type*)self;

<*
 Return the pointer held by a nullable reference.

 @param self : "The reference to unwrap"
 @return "The pointer, which is never null"
 @return? NULL : "If it is null"
 @ensure return != null
*>
macro Type*? Nullable.get(self) => self ? (Type*)self : NULL~;

<*
 Narrow a nullable reference to a non-null one.

 @param self : "The reference to narrow"
 @return "A non-null reference to the same pointee"
 @return? NULL : "If it is null"
*>
macro NonNull? Nullable.to_nonnull(self) => nonnull_try((Type*)self);

<*
 Widen a non-null reference to a nullable one. This always succeeds.

 @param self : "The reference to widen"
 @return "A nullable reference to the same pointee"
*>
macro Nullable NonNull.to_nullable(self) => (Nullable)(Type*)self;

<*
 Return a copy of the value a non-null reference points at.

 @param self : "The reference to read"
 @return "A copy of the value"

 @require (void*)self != null : "The value must be initialized"
*>
macro Type NonNull.value(self) @if(Type::kind != VOID) => *(Type*)self;

<*
 Return a copy of the value a nullable reference points at.

 @param self : "The reference to read"
 @return "A copy of the value"
 @return? NULL : "If it is null"
*>
macro Type? Nullable.value(self) @if(Type::kind != VOID) => *self.get();

<*
 Compare two references by pointer identity, not by value.

 @param self : "The reference to compare"
 @param other : "A pointer, or a reference, to the same type"
 @return "True if both hold the same pointer"

 @require (void*)self != null : "The value must be initialized"
 @require [other] @_is_valid_arg(Type, other) : "Expected the parameter to hold a pointer of the same type"
*>
macro bool NonNull.equals(self, other) @operator(==)
{
	return (void*)self == (void*)other;
}

<*
 Compare two references by pointer identity, not by value.

 Two null references are equal.

 @param self : "The reference to compare"
 @param other : "A pointer, or a reference, to the same type"
 @return "True if both hold the same pointer"

 @require [other] @_is_valid_arg(Type, other) : "Expected the parameter to hold a pointer of the same type"
*>
macro bool Nullable.equals(self, other) @operator(==)
{
	return (void*)self == (void*)other;
}

<*
 Compare the pointed-at values of two references.

 Available when `Type` is comparable with `==`.

 @param self : "The reference to compare"
 @param other : "A pointer, or a reference, to the same type"
 @return "True if both point at equal values"

 @require (void*)self != null : "The value must be initialized"
 @require [other] @_is_valid_arg(Type, other) : "Expected the parameter to hold a pointer of the same type"
*>
macro bool NonNull.value_equals(self, other) @if(Type::kind != VOID &&& $defined((Type){} == (Type){}))
{
	Type* other_cast = (Type*)other;
	Type* self_cast = (Type*)self;
	if (!other) return false;
	if (other_cast == self_cast) return true;
	return *self_cast == *other_cast;
}

<*
 Compare the pointed-at values of two references.

 References to the same pointer are equal, so two null references are equal.
 A null reference never equals a non-null one. Available when `Type` is
 comparable with `==`.

 @param self : "The reference to compare"
 @param other : "A pointer, or a reference, to the same type"
 @return "True if both point at equal values, or both are null"

 @require [other] @_is_valid_arg(Type, other) : "Expected the parameter to hold a pointer of the same type"
*>
macro bool Nullable.value_equals(self, other) @if(Type::kind != VOID &&& $defined((Type){} == (Type){}))
{
	Type* other_cast = (Type*)other;
	Type* self_cast = (Type*)self;
	if (other_cast == self_cast) return true;
	if (!self_cast || !other) return false;
	return *self_cast == *other_cast;
}


<*
 Print the pointed-at value, or `<INVALID REF>` if the reference is null.

 @param [&in] self : "The reference to print"
 @param [&inout] f : "The formatter to print to"
 @return "The number of characters written"
*>
fn sz? NonNull.to_format(&self, Formatter* f) @dynamic
{
	Type* t = (Type*)*self;
	if (!t) return f.printf("<INVALID REF>");
	return Type == void ??? f.printf("%p", t) : f.printf("%s", *t);
}

<*
 Print the pointed-at value, or `<NULL>` if the reference is null.

 @param [&in] self : "The reference to print"
 @param [&inout] f : "The formatter to print to"
 @return "The number of characters written"
*>
fn sz? Nullable.to_format(&self, Formatter* f) @dynamic
{
	Type* t = (Type*)*self;
	if (!t) return f.printf("<NULL>");
	return Type == void ??? f.printf("%p", t) : f.printf("%s", *t);
}


<*
 True if `#other` holds a pointer, or a reference, to `$Type`.

 @param $Type : "The pointee type to match against"
 @param #other : "The expression to test"
 @return "True if the expression is a compatible pointer or reference"
*>
macro @_is_valid_arg($Type, #other) @local @const
{
	return $defined($Type* x = #other) ||| $defined(Nullable{$Type} y = #other);
}

<*
 Read the pointed-at value, as an Optional. Only index 0 is meaningful:
 a nullable reference points at a single value, not at an array.

 @param self : "The reference to read"
 @param index : "The index, should always be 0"
 @return "A copy of the value"
 @return? NULL : "If it is null"

 @require index == 0 : "Only index 0 is valid"
*>
fn Type? Nullable._get_index(self, sz index) @operator([]) @if(Type::kind != VOID) => *self.get();

<*
 Set a value to the address of the nullable, returning an Empty if it fails. Only index 0 is meaningful:
 a nullable reference points at a single value, not at an array.

 @param self : "The reference to write through"
 @param index : "The index, should always be 0"
 @param #val : "The value to set it to"
 @return "The value that was written"
 @return? NULL : "If it is null"

 @require [index] index == 0 : "Only index 0 is valid"
*>
macro Type? Nullable._set_index(self, sz index, Type #val) @safemacro @operator([]=) @if(Type::kind != VOID) => self ? (*(Type*)self = #val) : NULL~;
