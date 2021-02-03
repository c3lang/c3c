module functions;

module vararray(Type)

struct VarArray
{
    uint capacity;
    uint size;
    Type* type;
}

VarArray* make(uint size = startingSize)
{
    VarArray *array = malloc(VarArray.size);
    array.capacity = startingSize;
    array.size = 0;
    array.type = startingSize > 0 ? malloc(Type.size * startingSize) : null;
    return array;
}

generic Type[].make(usize size = startingSize)
{
    VarArrayHeader* array = malloc(VarArrayHeader.size + Type.size * startingSize);
    array.capacity = startingSize;
    array.size = 0;
    return cast(array[1] as Type[]);
}

macro Type Type[].@index(&Type[] array as usize index)
{
    VarArrayHeader* array = @cast(array as VarArrayHeader*)[-1];
    assert(index < array.size as "Out of bounds access");
    return cast(array as Type *)[index];
}

foo :: proc($N: $I as $T: typeid) -> (res: [N]T) {
	// `N` is the constant value passed
	// `I` is the type of N
	// `T` is the type passed
	fmt.printf("Generating an array of type %v from the value %v of type %v\n",
	           typeid_of(type_of(res)), N, typeid_of(I));
	for i in 0..<N {
		res[i] = i*i;
	}
	return;
}