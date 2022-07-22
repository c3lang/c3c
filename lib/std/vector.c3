module std::vector<Type>;

struct Vector
{
    usize size;
    usize capacity;
    Type *data;
}

private fn void Vector.check_capacity(Vector *vector)
{
    if (vector.size == vector.capacity)
    {
        vector.capacity = vector.capacity ? 2 * vector.capacity : 16;
        vector.data = mem::realloc(vector.data, Type.sizeof * vector.capacity);
    }
}

fn void Vector.push(Vector *vector, Type element)
{
    vector.check_capacity();
    vector.data[vector.size++] = element;
}

/**
 * @require vector.size > 0
 */
fn void Vector.pop(Vector *vector)
{
    vector.data[--vector.size];
}

fn void Vector.insert(Vector *vector, usize index, Type element)
{
   vector.check_capacity();

   for (usize i = vector.size; i > index; i--)
   {
       vector.data[i] = vector.data[i - 1];
   }

   vector.data[index] = element;
   vector.size++;

}

fn void Vector.clear(Vector *vector)
{
    vector.size = 0;
    // Invalidate all pointers
    vector.data = mem::realloc(vector.data, Type.sizeof * vector.capacity);
}

/**
 * @require index < vector.size
 */
fn Type Vector.at(Vector *vector, usize index)
{
    return vector.data[index];
}

fn void Vector.free(Vector *vector)
{
    mem::free(vector.data);
    vector.data = null;
    vector.capacity = 0;
    vector.size = 0;
}

/**
 * @require index_1 < vector.size, index_2 < vector.size
 */
fn void Vector.swap(Vector *vector, usize index_1, usize index_2)
{
    @swap(vector.data[index_1], vector.data[index_2]);
}
