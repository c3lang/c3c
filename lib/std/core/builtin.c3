// Copyright (c) 2021-2022 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::builtin;
import libc;

fault IteratorResult
{
	NO_MORE_ELEMENT
}

fault VarCastResult
{
	TYPE_MISMATCH
}

/**
 * Stores a variable on the stack, then restores it at the end of the
 * macro scope.
 *
 * @param variable `the variable to store and restore`
 **/
macro void @scope(&variable; @body) @builtin
{
	$typeof(variable) temp = variable;
	defer variable = temp;
	@body();
}

macro void @swap(&a, &b) @builtin
{
	$typeof(a) temp = a;
    a = b;
    b = temp;
}

/**
 * Convert a variant type to a type, returning an failure if there is a type mismatch.
 *
 * @param v `the variant to convert to the given type.`
 * @param $Type `the type to convert to`
 * @return `The variant.ptr converted to its type.`
 **/
macro varcast(variant v, $Type) @builtin
{
	if (v.type != $Type.typeid) return VarCastResult.TYPE_MISMATCH!;
	return ($Type*)v.ptr;
}

extern fn void printf(char*, ...);

struct CallstackElement
{
	CallstackElement* prev;
	char* function;
	char* file;
	uint line;
}
fn void panic(char* message, char *file, char *function, uint line) @builtin
{
	CallstackElement* stack = $$stacktrace();
	$if ($defined(libc::stderr) && $defined(libc::fprintf)):

	if (stack) stack = stack.prev;
	if (stack)
	{
		libc::fprintf(libc::stderr(), "\nERROR: '%s'\n", message);
	}
	else
	{
		libc::fprintf(libc::stderr(), "\nERROR: '%s', function %s (%s:%d)\n", message, function, file, line);
	}
	while (stack)
	{
		libc::fprintf(libc::stderr(), "  at function %s (%s:%u)\n", stack.function, stack.file, stack.line);
		if (stack == stack.prev) break;
		stack = stack.prev;
	}

	$endif;

	$$trap();
}

macro unreachable($string = "Unreachable statement reached.") @builtin @noreturn
{
	panic($string, $$FILE, $$FUNC, $$LINE);
	$$unreachable();
}



/*
enum TypeKind
{
    VOID,
    BOOL,
    FLOAT,
    INTEGER,
    STRUCT,
    UNION,
    ERROR,
    ENUM,
    ARRAY,
    POINTER,
    VAR_ARRAY,
    SUBARRAY,
    OPAQUE
    // ALIAS,
}

struct TypeData
{
    typeid typeId;
    TypeKind kind;
    int size;
    int alignment;
    char* name;
    char* fullName;
}

struct TypeAlias
{
    TypeData data;
    typeid aliasType;
}

struct TypeError
{
    TypeData data;
    TypeErrorValue[] errors;
}

struct TypeArray
{
    TypeData data;
    typeid elementType;
    ulong elements;
}

struct TypeVarArray
{
    TypeData data;
    typeid elementType;
}

struct TypeSubarray
{
    TypeData data;
    typeid elementType;
}

struct TypePointer
{
    TypeData data;
    typeid baseType;
}

struct TypeStruct
{
    TypeData data;
    TypeData*[] fields;
}

struct TypeUnion
{
    TypeData data;
    TypeData*[] variants;
}

struct TypeEnum
{
    TypeData data;
    typeid valueType;
    TypeData*[] associated_value_types;
}


struct TypeEnumValue
{
    char* name;
    ulong value;
    void*[] associated_values;
}

struct TypeErrorValue
{
    char* name;
    ulong value;
}
*/