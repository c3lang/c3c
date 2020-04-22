module system::builtin;

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
    TypeData;
    typeid aliasType;
}

struct TypeError
{
    TypeData;
    TypeErrorValue[] errors;
}

struct TypeArray
{
    TypeData;
    typeid elementType;
    ulong elements;
}

struct TypeVarArray
{
    TypeData;
    typeid elementType;
}

struct TypeSubarray
{
    TypeData;
    typeid elementType;
}

struct TypePointer
{
    TypeData;
    typeid baseType;
}

struct TypeStruct
{
    TypeData;
    TypeData*[] fields;
}

struct TypeUnion
{
    TypeData;
    TypeData*[] variants;
}

struct TypeEnum
{
    TypeData;
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
