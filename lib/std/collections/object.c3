// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::object;
import std::collections::map, std::collections::list, std::io;

const Object TRUE_OBJECT = { .b = true, .type = bool::typeid };
const Object FALSE_OBJECT = { .b = false, .type = bool::typeid };
const Object NULL_OBJECT = { .type = void*::typeid };

struct Object (Printable)
{
	typeid type;
	Allocator allocator;
	union
	{
		int128 i;
		double f;
		bool b;
		String s;
		void* other;
		ObjectInternalList array;
		ObjectInternalMap map;
	}
}


fn sz? Object.to_format(&self, Formatter* formatter) @dynamic
{
	switch (self.type)
	{
		case void:
			return formatter.printf("{}")!;
		case void*:
			return formatter.printf("null")!;
		case String:
			return formatter.printf(`"%s"`, self.s)!;
		case bool:
			return formatter.printf(self.b ? "true" : "false")!;
		case ObjectInternalList:
			sz n = formatter.printf("[")!;
			foreach (i, ol : self.array)
			{
				if (i > 0) n += formatter.printf(",")!;
				n += ol.to_format(formatter)!;
			}
			n += formatter.printf("]")!;
			return n;
		case ObjectInternalMap:
			sz n = formatter.printf("{")!;
			@stack_mem(1024; Allocator mem)
			{
				foreach (i, key : self.map.keys(mem))
				{
					if (i > 0) n += formatter.printf(",")!;
					n += formatter.printf(`"%s":`, key)!;
					n += self.map.get(key).to_format(formatter)!;
				}
			};
			n += formatter.printf("}")!;
			return n;
		default:
			switch (self.type.kind)
			{
				case SIGNED_INT:
					return formatter.printf("%d", (int128)self.i)!;
				case UNSIGNED_INT:
					return formatter.printf("%d", (uint128)self.i)!;
				case FLOAT:
					return formatter.printf("%g", self.f)!;
				case ENUM:
					return formatter.printf("%d", self.i)!;
				default:
					return formatter.printf("<>")!;
			}
	}
}

fn Object* new_obj(Allocator allocator)
{
	return @alloc_obj(allocator, { .allocator = allocator, .type = void::typeid });
}

fn Object* new_null()
{
	return &NULL_OBJECT;
}

fn Object* new_int(Allocator allocator, int128 i)
{
	return @alloc_obj(allocator, { .i = i, .allocator = allocator, .type = int128::typeid });
}

<* @require @kindof(e) == ENUM *>
macro Object* new_enum(Allocator allocator, e)
{
	return @alloc_obj(allocator, { .i = (int128) e.ordinal, .allocator = allocator, .type = @typeid(e) });
}

fn Object* new_float(Allocator allocator, double f)
{
	return @alloc_obj(allocator, { .f = f, .allocator = allocator, .type = double::typeid });
}

fn Object* new_string(Allocator allocator, String s)
{
	return @alloc_obj(allocator, { .s = s.copy(allocator), .allocator = allocator, .type = String::typeid });
}


fn Object* new_bool(bool b)
{
	return b ? &TRUE_OBJECT : &FALSE_OBJECT;
}

fn void Object.free(&self)
{
	switch (self.type)
	{
		case void:
			break;
		case String:
			alloc::free(self.allocator, self.s);
		case ObjectInternalList:
			foreach (ol : self.array)
			{
				ol.free();
			}
			self.array.free();
		case ObjectInternalMap:
			self.map.@each_entry(; ObjectInternalMapEntry* entry) {
				entry.value.free();
			};
			self.map.free();
		default:
			break;
	}
	if (self.allocator)
	{
		$if Object::alignment > mem::DEFAULT_MEM_ALIGNMENT:
			alloc::free_aligned(self.allocator, self);
		$else
			alloc::free(self.allocator, self);
		$endif
	}
}

fn bool Object.is_null(&self) @inline => self == &NULL_OBJECT;
fn bool Object.is_empty(&self) @inline => self.type == void::typeid;
fn bool Object.is_map(&self) @inline => self.type == ObjectInternalMap::typeid;
fn bool Object.is_array(&self) @inline => self.type == ObjectInternalList::typeid;
fn bool Object.is_bool(&self) @inline => self.type == bool::typeid;
fn bool Object.is_string(&self) @inline => self.type == String::typeid;
fn bool Object.is_float(&self) @inline => self.type == double::typeid;
fn bool Object.is_int(&self) @inline => self.type == int128::typeid;
fn bool Object.is_enum(&self) @inline => self.type.kind == ENUM;
fn bool Object.is_keyable(&self) => self.is_empty() || self.is_map();
fn bool Object.is_indexable(&self) => self.is_empty() || self.is_array();

macro Object.to_value(&self, $Type, strict = true)
{
	$switch:
		$case types::is_int($Type):
			if (!strict)
			{
				if (self.is_string())
				{
					$if $Type::kind == TypeKind.SIGNED_INT:
						return ($Type)self.s.to_int128();
					$else
						return ($Type)self.s.to_uint128();
					$endif
				}
				if (self.is_float())
				{
					return ($Type)self.f;
				}
			}
			if (!self.is_int()) return TYPE_MISMATCH~;
			return ($Type)self.i;
		$case types::is_float($Type):
			if (!strict)
			{
				if (self.is_int())
				{
					return ($Type)self.i;
				}
				if (self.is_string())
				{
					return ($Type)self.s.to_double();
				}
			}
			if (!self.is_float()) return TYPE_MISMATCH~;
			return ($Type)self.f;
		$case $Type == String:
			if (self.is_string()) return self.s;
			if (strict) return TYPE_MISMATCH~;
			return string::format(self.allocator, "%d", self);
		$case $Type == bool:
			if (self.is_bool()) return self.b;
			return TYPE_MISMATCH~;
		$case $Type == Object*:
			return self;
		$case $Type == void*:
			return self.other;
		$case $Type::kind == ENUM:
			if (!strict)
			{
				if (self.is_int())
				{
					return $Type::from_ordinal(self.i);
				}
			}
			if ($Type::typeid != self.type) return TYPE_MISMATCH~;
			return $Type::from_ordinal(self.i);
		$default:
			$error "Unsupported object type '" +++ $Type::name +++ "'";
	$endswitch
}

<*
 @require self.is_keyable()
*>
fn void object_init_map_if_needed(Object* self) @private
{
	if (self.is_empty())
	{
		self.type = ObjectInternalMap::typeid;
		self.map.init(self.allocator);
	}
}

<*
 @require self.is_indexable()
*>
fn void object_init_array_if_needed(Object* self) @private
{
	if (self.is_empty())
	{
		self.type = ObjectInternalList::typeid;
		self.array.init(self.allocator);
	}
}

<*
 @require self.is_keyable()
*>
fn void object_set_object(Object* self, String key, Object* new_object) @private
{
	object_init_map_if_needed(self);
	Object*? val = self.map.get_entry(key).value;
	defer (void)val.free();
	self.map.set(key, new_object);
}

<*
 @require self.allocator != null : "This object is not properly initialized, was it really created using 'new'"
 @require $typeof(value) != void* ||| value == null : "void pointers cannot be stored in an object"
*>
macro Object* Object.object_from_value(&self, value) @private
{
	var $Type = $typeof(value);
	$switch:
		$case types::is_int($Type):
			return new_int(self.allocator, value);
		$case types::is_float($Type):
			return new_float(self.allocator, value);
		$case $Type::typeid == String::typeid:
			return new_string(self.allocator, value);
		$case $Type::typeid == bool::typeid:
			return new_bool(value);
		$case $Type::typeid == Object*::typeid:
			return value;
		$case $Type::typeid == void*::typeid:
			return &NULL_OBJECT;
		$case $defined(String x = value):
			return new_string(self.allocator, value);
		$case $Type::kind == ENUM:
			return new_enum(self.allocator, value);
		$default:
			$error "Unsupported object type '" +++ $Type::name +++ "'";
	$endswitch
}

macro Object* Object.set(&self, String key, value)
{
	Object* val = self.object_from_value(value);
	object_set_object(self, key, val);
	return val;
}

<*
 @require self.is_indexable()
*>
macro Object* Object.set_at(&self, sz index, String key, value)
{
	Object* val = self.object_from_value(value);
	self.set_object_at(key, index, val);
	return val;
}

<*
 @require self.is_indexable()
 @ensure return != null
*>
macro Object* Object.push(&self, value)
{
	Object* val = self.object_from_value(value);
	self.push_object(val);
	return val;
}

<*
 @require self.is_keyable()
*>
fn Object*? Object.get(&self, String key) => self.is_empty() ? NOT_FOUND~ : self.map.get(key);

fn bool Object.has_key(&self, String key) => self.is_map() && self.map.has_key(key);

<*
 Iterate over all key/value pairs in this object when it is a map.
 Does nothing if the object is not a map.
*>
macro void Object.@each_entry(&self; @body(String key, Object* value))
{
	if (!self.is_map()) return;
	self.map.@each_entry(; ObjectInternalMapEntry* e)
	{
		@body(e.key, e.value);
	};
}

<*
 @require self.is_indexable()
*>
fn Object* Object.get_at(&self, sz index)
{
	return self.array.get(index);
}

<*
 @require self.is_indexable()
*>
fn sz Object.get_len(&self)
{
	return self.array.len();
}

<*
 @require self.is_indexable()
*>
fn void Object.push_object(&self, Object* to_append)
{
	object_init_array_if_needed(self);
	self.array.push(to_append);
}

<*
 @require self.is_indexable()
*>
fn void Object.set_object_at(&self, sz index, Object* to_set)
{
	object_init_array_if_needed(self);
	while (self.array.len() < index)
	{
		self.array.push(&NULL_OBJECT);
	}
	if (self.array.len() == index)
	{
		self.array.push(to_set);
		return;
	}
	self.array.get(index).free();
	self.array.set_at(index, to_set);
}

<*
 @require $Type::kind.is_int() : "Expected an integer type."
*>
macro get_integer_value(Object* value, $Type)
{
	if (value.is_float())
	{
		return ($Type)value.f;
	}
	if (value.is_string())
	{
		$if $Type::kind == TypeKind.SIGNED_INT:
			return ($Type)value.s.to_int128();
		$else
			return ($Type)value.s.to_uint128();
		$endif
	}
	if (!value.is_int()) return string::MALFORMED_INTEGER~;
	return ($Type)value.i;
}


<*
 @require self.is_indexable()
 @require $Type::kind.is_int() : "Expected an integer type"
*>
macro Object.get_integer_at(&self, $Type, sz index) @private
{
	return get_integer_value(self.get_at(index), $Type);
}

<*
 @require self.is_keyable()
 @require $Type::kind.is_int() : "Expected an integer type"
*>
macro Object.get_integer(&self, $Type, String key) @private
{
	return get_integer_value(self.get(key), $Type);
}

fn ichar? Object.get_ichar(&self, String key) => self.get_integer(ichar, key);
fn short? Object.get_short(&self, String key) => self.get_integer(short, key);
fn int? Object.get_int(&self, String key) => self.get_integer(int, key);
fn long? Object.get_long(&self, String key) => self.get_integer(long, key);
fn int128? Object.get_int128(&self, String key) => self.get_integer(int128, key);

fn ichar? Object.get_ichar_at(&self, sz index) => self.get_integer_at(ichar, index);
fn short? Object.get_short_at(&self, sz index) => self.get_integer_at(short, index);
fn int? Object.get_int_at(&self, sz index) => self.get_integer_at(int, index);
fn long? Object.get_long_at(&self, sz index) => self.get_integer_at(long, index);
fn int128? Object.get_int128_at(&self, sz index) => self.get_integer_at(int128, index);

fn char? Object.get_char(&self, String key) => self.get_integer(char, key);
fn ushort? Object.get_ushort(&self, String key) => self.get_integer(ushort, key);
fn uint? Object.get_uint(&self, String key) => self.get_integer(uint, key);
fn ulong? Object.get_ulong(&self, String key) => self.get_integer(ulong, key);
fn uint128? Object.get_uint128(&self, String key) => self.get_integer(uint128, key);

fn char? Object.get_char_at(&self, sz index) => self.get_integer_at(char, index);
fn ushort? Object.get_ushort_at(&self, sz index) => self.get_integer_at(ushort, index);
fn uint? Object.get_uint_at(&self, sz index) => self.get_integer_at(uint, index);
fn ulong? Object.get_ulong_at(&self, sz index) => self.get_integer_at(ulong, index);
fn uint128? Object.get_uint128_at(&self, sz index) => self.get_integer_at(uint128, index);

<*
 @require self.is_keyable()
*>
fn String? Object.get_string(&self, String key)
{
	Object* value = self.get(key)!;
	if (!value.is_string()) return TYPE_MISMATCH~;
	return value.s;
}

<*
 @require self.is_indexable()
*>
fn String? Object.get_string_at(&self, sz index)
{
	Object* value = self.get_at(index);
	if (!value.is_string()) return TYPE_MISMATCH~;
	return value.s;
}

<*
 @require self.is_keyable()
 @require $EnumType::kind == ENUM
*>
macro Object.get_enum(&self, $EnumType, String key)
{
	Object* value = self.get(key)!;
	if ($EnumType::typeid != value.type) return TYPE_MISMATCH~;
	return $EnumType::from_ordinal(value.i);
}

<*
 @require self.is_indexable()
 @require $EnumType::kind == ENUM
*>
macro Object.get_enum_at(&self, $EnumType, sz index)
{
	Object* value = self.get_at(index);
	if ($EnumType::typeid != value.type) return TYPE_MISMATCH~;
	return $EnumType::from_ordinal(value.i);
}

<*
 @require self.is_keyable()
*>
fn bool? Object.get_bool(&self, String key)
{
	Object* value = self.get(key)!;
	if (!value.is_bool()) return TYPE_MISMATCH~;
	return value.b;
}


<*
 @require self.is_indexable()
*>
fn bool? Object.get_bool_at(&self, sz index)
{
	Object* value = self.get_at(index);
	if (!value.is_bool()) return TYPE_MISMATCH~;
	return value.b;
}

<*
 @require self.is_keyable()
*>
fn double? Object.get_float(&self, String key)
{
	Object* value = self.get(key)!;
	switch (value.type.kind)
	{
		case SIGNED_INT:
			return (double)value.i;
		case UNSIGNED_INT:
			return (double)(uint128)value.i;
		case FLOAT:
			return value.f;
		default:
			return TYPE_MISMATCH~;
	}
}

<*
 @require self.is_indexable()
*>
fn double? Object.get_float_at(&self, sz index)
{
	Object* value = self.get_at(index);
	switch (value.type.kind)
	{
		case SIGNED_INT:
			return (double)value.i;
		case UNSIGNED_INT:
			return (double)(uint128)value.i;
		case FLOAT:
			return value.f;
		default:
			return TYPE_MISMATCH~;
	}
}

fn Object* Object.get_or_create_obj(&self, String key)
{
	if (try obj = self.get(key) && !obj.is_null()) return obj;
	Object* container = new_obj(self.allocator);
	self.set(key, container);
	return container;
}

alias ObjectInternalMap      @private = HashMap {String, Object*};
alias ObjectInternalList     @private = List {Object*};
alias ObjectInternalMapEntry @private = Entry {String, Object*};

macro Object* @alloc_obj(Allocator a, #init) @private
{
	$if Object::alignment > mem::DEFAULT_MEM_ALIGNMENT:
		return alloc::new_aligned(a, Object, #init)!!;
	$else
		return alloc::new(a, Object, #init);
	$endif
}
