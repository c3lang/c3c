module std::collections::object;
import std::collections::map;
import std::collections::list;

const Object TRUE_OBJECT = { .b = true, .type = bool.typeid };
const Object FALSE_OBJECT = { .b = false, .type = bool.typeid };
const Object NULL_OBJECT = { .type = void*.typeid };

struct Object
{
	typeid type;
	Allocator* allocator;
	union
	{
		uint128 i;
		double f;
		bool b;
		String s;
		void* other;
		ObjectInternalList array;
		ObjectInternalMap map;
	}
}

fn Object* new_obj(Allocator* allocator = mem::heap())
{
	Object* o = malloc(Object, .using = allocator);
	*o = { .allocator = allocator, .type = void.typeid };
	return o;
}

fn Object* new_null()
{
	return &NULL_OBJECT;
}

fn Object* new_int(int128 i, Allocator* allocator = mem::heap())
{
	Object* o = malloc(Object, .using = allocator);
	*o = { .i = i, .allocator = allocator, .type = int128.typeid };
	return o;
}

macro Object* new_enum(e, Allocator* allocator = mem::heap())
{
	Object* o = malloc(Object, .using = allocator);
	*o = { .i = (int128)e, .allocator = allocator, .type = $typeof(e).typeid };
	return o;
}

fn Object* new_float(double f, Allocator* allocator = mem::current_allocator())
{
	Object* o = malloc(Object, .using = allocator);
	*o = { .f = f, .allocator = allocator, .type = double.typeid };
	return o;
}

fn Object* new_string(String s, Allocator* allocator = mem::heap())
{
	Object* o = malloc(Object, .using = allocator);
	*o = { .s = s.copyz(allocator), .allocator = allocator, .type = String.typeid };
	return o;
}


fn Object* new_bool(bool b)
{
	return b ? &TRUE_OBJECT : &FALSE_OBJECT;
}

/**
 * @param [&inout] o
 **/
fn void Object.free(Object* o)
{
	switch (o.type)
	{
		case void:
			break;
		case String:
			free(o.s, .using = o.allocator);
		case ObjectInternalList:
			foreach (ol : o.array)
			{
				ol.free();
			}
			o.array.free();
		case ObjectInternalMap:
			@pool()
			{
				foreach (key : o.map.key_tlist())
				{
					o.map.get(key).free();
					free(key, .using = o.allocator);
				}
				o.map.free();
			};
		default:
			break;
	}
	if (o.allocator) free(o, .using = o.allocator);
}

fn bool Object.is_null(Object* this) @inline => this == &NULL_OBJECT;
fn bool Object.is_empty(Object* this) @inline => this.type == void.typeid;
fn bool Object.is_map(Object* this) @inline => this.type == ObjectInternalMap.typeid;
fn bool Object.is_array(Object* this) @inline => this.type == ObjectInternalList.typeid;
fn bool Object.is_bool(Object* this) @inline => this.type == bool.typeid;
fn bool Object.is_string(Object* this) @inline => this.type == String.typeid;
fn bool Object.is_float(Object* this) @inline => this.type == double.typeid;
fn bool Object.is_int(Object* this) @inline => this.type == int128.typeid;
fn bool Object.is_keyable(Object* this) => this.is_empty() || this.is_map();
fn bool Object.is_indexable(Object* this) => this.is_empty() || this.is_array();

/**
 * @require o.is_keyable()
 **/
fn void Object.init_map_if_needed(Object* o) @private
{
	if (o.is_empty())
	{
		o.type = ObjectInternalMap.typeid;
		o.map.init(.allocator = o.allocator);
	}
}

/**
 * @require o.is_indexable()
 **/
fn void Object.init_array_if_needed(Object* o) @private
{
	if (o.is_empty())
	{
		o.type = ObjectInternalList.typeid;
		o.array.init(.allocator = o.allocator);
	}
}

/**
 * @require o.is_keyable()
 **/
fn void Object.set_object(Object* o, String key, Object* new_object) @private
{
	o.init_map_if_needed();
	ObjectInternalMapEntry*! entry = o.map.get_entry(key);
	defer
	{
		(void)free(entry.key, .using = o.allocator);
		entry.value.free();
	}
	o.map.set(str::copyz(key, o.map.allocator), new_object);
}

macro Object* object_from_value(value) @private
{
	var $Type = $typeof(value);
	$if (types::is_int($Type)):
		return new_int(value);
	$elif (types::is_float($Type)):
		return new_float(value);
	$elif ($Type.typeid == String.typeid):
		return new_string(value);
	$elif ($Type.typeid == bool.typeid):
		return new_bool(value);
	$elif ($Type.typeid == Object*.typeid):
		return value;
	$elif ($Type.typeid == void*.typeid):
		assert(value == null);
		return &NULL_OBJECT;
	$elif ($checks(String s = value)):
		return new_string(value);
	$else:
		$assert(false, "Unsupported object type.");
	$endif;
}

macro Object* Object.set(Object* o, String key, value)
{
	Object* val = object_from_value(value);
	o.set_object(key, val);
	return val;
}

/**
 * @require o.is_indexable()
 **/
macro Object* Object.set_at(Object* o, usz index, String key, value)
{
	Object* val = object_from_value(value);
	o.set_object_at(key, index, val);
	return val;
}

/**
 * @require o.is_indexable()
 * @ensure return != null
 **/
macro Object* Object.append(Object* o, value)
{
	Object* val = object_from_value(value);
	o.append_object(val);
	return val;
}

/**
 * @require o.is_keyable()
 **/
fn Object*! Object.get(Object* o, String key) => o.is_empty() ? SearchResult.MISSING! : o.map.get(key);

/**
 * @require o.is_indexable()
 **/
fn Object* Object.get_at(Object* o, usz index)
{
	return o.array.get(index);
}

/**
 * @require o.is_indexable()
 **/
fn void Object.append_object(Object* o, Object* to_append)
{
	o.init_array_if_needed();
	o.array.append(to_append);
}

/**
 * @require o.is_indexable()
 **/
fn void Object.set_object_at(Object* o, usz index, Object* to_set)
{
	o.init_array_if_needed();
	while (o.array.len() < index)
	{
		o.array.append(&NULL_OBJECT);
	}
	if (o.array.len() == index)
	{
		o.array.append(to_set);
		return;
	}
	o.array.get(index).free();
	o.array.set_at(index, to_set);
}

macro get_integer_value(Object* value, $Type)
{
	if (value.is_float())
	{
		return ($Type)value.f;
	}
	if (value.is_string())
	{
		$if ($Type.kindof == TypeKind.SIGNED_INT):
			return ($Type)str::to_int128(value.s);
		$else:
			return ($Type)str::to_uint128(value.s);
		$endif;
	}
	if (!value.is_int()) return NumberConversion.MALFORMED_INTEGER!;
	return ($Type)value.i;
}

/**
 * @require o.is_indexable()
 **/
macro Object.get_integer_at(Object* o, $Type, usz index) @private
{
	return get_integer_value(o.get_at(index), $Type);
}

/**
 * @require o.is_keyable()
 **/
macro Object.get_integer(Object* o, $Type, String key) @private
{
	return get_integer_value(o.get(key), $Type);
}

fn ichar! Object.get_ichar(Object* o, String key) => o.get_integer(ichar, key);
fn short! Object.get_short(Object* o, String key) => o.get_integer(short, key);
fn int! Object.get_int(Object* o, String key) => o.get_integer(int, key);
fn long! Object.get_long(Object* o, String key) => o.get_integer(long, key);
fn int128! Object.get_int128(Object* o, String key) => o.get_integer(int128, key);

fn ichar! Object.get_ichar_at(Object* o, usz index) => o.get_integer_at(ichar, index);
fn short! Object.get_short_at(Object* o, usz index) => o.get_integer_at(short, index);
fn int! Object.get_int_at(Object* o, usz index) => o.get_integer_at(int, index);
fn long! Object.get_long_at(Object* o, usz index) => o.get_integer_at(long, index);
fn int128! Object.get_int128_at(Object* o, usz index) => o.get_integer_at(int128, index);

fn char! Object.get_char(Object* o, String key) => o.get_integer(ichar, key);
fn short! Object.get_ushort(Object* o, String key) => o.get_integer(ushort, key);
fn uint! Object.get_uint(Object* o, String key) => o.get_integer(uint, key);
fn ulong! Object.get_ulong(Object* o, String key) => o.get_integer(ulong, key);
fn uint128! Object.get_uint128(Object* o, String key) => o.get_integer(uint128, key);

fn char! Object.get_char_at(Object* o, usz index) => o.get_integer_at(char, index);
fn ushort! Object.get_ushort_at(Object* o, usz index) => o.get_integer_at(ushort, index);
fn uint! Object.get_uint_at(Object* o, usz index) => o.get_integer_at(uint, index);
fn ulong! Object.get_ulong_at(Object* o, usz index) => o.get_integer_at(ulong, index);
fn uint128! Object.get_uint128_at(Object* o, usz index) => o.get_integer_at(uint128, index);

/**
 * @require o.is_keyable()
 **/
fn String! Object.get_string(Object* o, String key)
{
	Object* value = o.get(key)?;
	assert(value.is_string());
	return value.s;
}

/**
 * @require o.is_indexable()
 **/
fn String Object.get_string_at(Object* o, usz index)
{
	Object* value = o.get_at(index);
	assert(value.is_string());
	return value.s;
}

/**
 * @require o.is_keyable()
 **/
macro String! Object.get_enum(Object* o, $EnumType, String key)
{
	Object value = o.get(key)?;
	assert($EnumType.typeid == value.type);
	return ($EnumType)value.i;
}

/**
 * @require o.is_indexable()
 **/
macro String Object.get_enum_at(Object* o, $EnumType, usz index)
{
	Object value = o.get_at(index);
	assert($EnumType.typeid == value.type);
	return ($EnumType)value.i;
}

/**
 * @require o.is_keyable()
 **/
fn bool! Object.get_bool(Object* o, String key)
{
	Object* value = o.get(key)?;
	assert(value.is_bool());
	return value.b;
}


/**
 * @require o.is_indexable()
 **/
fn bool Object.get_bool_at(Object* o, usz index)
{
	Object* value = o.get_at(index);
	assert(value.is_bool());
	return value.b;
}

/**
 * @require o.is_keyable()
 **/
fn double! Object.get_float(Object* o, String key)
{
	Object* value = o.get(key)?;
	switch (value.type.kindof)
	{
		case SIGNED_INT:
			return (double)value.i;
		case UNSIGNED_INT:
			return (double)(uint128)value.i;
		case FLOAT:
			return value.f;
		default:
			unreachable();
	}
}

/**
 * @require o.is_indexable()
 **/
fn double Object.get_float_at(Object* o, usz index)
{
	Object* value = o.get_at(index);
	switch (value.type.kindof)
	{
		case SIGNED_INT:
			return (double)value.i;
		case UNSIGNED_INT:
			return (double)(uint128)value.i;
		case FLOAT:
			return value.f;
		default:
			unreachable();
	}
}

fn Object* Object.get_or_create_obj(Object* o, String key)
{
	if (try obj = o.get(key) && !obj.is_null()) return obj;
	Object* container = new_obj();
	o.set(key, container);
	return container;
}

typedef ObjectInternalMap @private = HashMap<String, Object*>;
typedef ObjectInternalList @private = List<Object*>;
typedef ObjectInternalMapEntry @private = Entry<String, Object*>;

