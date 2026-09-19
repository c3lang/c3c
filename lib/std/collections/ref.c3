module std::collections::ref <Type>;
import std::io;

<* A non null reference *>
typedef Ref = inline Type*;

excuse NULL_REF;

<*
	@require [val] val != null
*>
fn Ref from(Type* val) @inline => (Ref)val;

fn Ref? try_from(Type* val) @inline => (Ref)val ?: NULL_REF~;

<*
	@require [self] (Type*)*self != null
*>
fn sz? Ref.to_format(&self, Formatter* f) @dynamic
{
  return f.printf("%s", *((Type*)*self));
}

<*
	@require [val] val != null
*>
fn void Ref.set(&self, Type* val) @inline
{
	*self = (Ref)val;
}

fn void? Ref.try_set(&self, Type* val) @inline
{
	if (!val) return NULL_REF~;
	*self = (Ref)val;
}

<*
	@ensure return != null
*>
fn Type* Ref.get(self) @inline => (Type*)self;

fn bool Ref.equals(self, Ref other) @operator(==) @if(types::is_equatable_type(Type))
{
	return ((Type*)self) == ((Type*)other);
}
