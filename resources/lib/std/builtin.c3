module std::builtin;
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