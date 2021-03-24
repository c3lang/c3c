module vector(Type);

public struct Vector
{
    Type[] array;
}

public func void Vector.init()
{
    array = nil;
}

public func void Vector.add(Vector *vector, Type type)
{
    vector.array += type;
}

public func usize Vector.size(Vector *vector)
{
    return vector.array.size;
}

public func void Vector.removeLast(Vector *vector)
{
    vector.array.pop();
}

public func void Vector.removefirst(Vector *vector)
{
    vector.array.removeAt(0);
}

public func void Type *Vector.first(Vector *vector)
{
    return &vector.array.first;
}

public func void Type *Vector.last(Vector *vector)
{
    return &vector.array.last();
}

public func bool Vector.empty(Vector *vector)
{
    return !vector.array.size;
}

public macro Vector.foreach(Vector *vector, macro void(Type value) body)
{
    for (usize i = 0, i < vector.array.size; i++)
    {
        @body(vector.array[i]);
    }
}

test
{
    define IntVector = Vector(int);
    IntVector *vector = @calloc(IntVector);
    vector.add(1);
    vector.add(2);
    for (int i : vector)
    {
        printDigit(i);
    }
    @vector.foreach(int i)
    {
        printDigit(i);
    }
}