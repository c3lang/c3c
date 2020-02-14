module acorn::arr;

/** Implements arrays: variable-sized, ordered collections of Values (see avm_array.h)
 *
 * @file
 *
 * This source file is part of avm - Acorn Virtual Machine.
 * See Copyright Notice in avm.h
 */

/* Return a new Array, allocating len slots for Values. */
func Value new(Value th, Value *dest, Value type, AuintIdx len)
{
	// Create an array object
	ArrInfo* val = @cast(mem::new(th, ArrEnc, sizeof(ArrInfo), ArrInfo*);
	val.flags1 = 0;	// Initialize Flags1 flags
	val.type = type;
	val.avail = len;
	val.size = 0;
	val.arr = nil;
	if (len > 0) mem::reallocvector(th, val.arr, 0, len, Value);
	return *dest = @cast(val, Value);
}

/* Return a new Array, allocating len slots for Values. */
func Value newClosure(Value *th, Value *dest, Value type, AuintIdx len)
{
	// Create an array object
	ArrInfo* val = @cast(mem::new(th, ArrEnc, sizeof(ArrInfo), ArrInfo*);
	val.flags1 = TypeClo;	// Initialize Flags1 flags
	val.type = type;
	val.avail = len;
	val.size = 0;
	val.arr = NULL;
	if (len > 0) mem::reallocvector(th, val.arr, 0, len, Value);
	return *dest = @cast(val, Value);
}

/* Return 1 if the value is an Array, otherwise 0 */
func int Value.isArr(Value* val)
{
	return val.isEnc(ArrEnc);
}

/* Return 1 if the value is an Array, otherwise 0 */
func int Value.isClosure(Value* val)
{
	return val.isEnc(ArrEnc) && arr_info(val)->flags1 & TypeClo;
}

private func ArrInfo.fill(ArrInfo* a, AuintIdx start, AuintIdx end, Value value) @inline
{
	for (AuintIdx i = start; i < end; i++) a.arr[i] = value;
}

/* Ensure array has room for len Values, allocating memory as needed.
 * Allocated space will not shrink. Changes nothing about array's contents. */
func void makeRoom(Value th, Value arr, AuintIdx len)
{
	ArrInfo* a = arr_info(arr);
	if (len > a.avail)
	{
		mem::gccheck(th);	// Incremental GC before memory allocation events
		mem::reallocvector(th, a.arr, a.avail, len, Value);
		a.avail = len;
	}
}

/**
 * Set the number of elements in the array, growing it if needed.
 * If less than current number array size, array is not shrunk.
 */
func void setSize(Value th, Value arr, AuintIdx len)
{
	ArrInfo* a = arr_info(arr);
	AuintIdx size = arr_size(arr);
	if (len > size) makeRoom(arr, len);
	arr_size(arr) = len;
}

/**
 * Force allocated and used array to a specified size, truncating
 * or expanding as needed. Growth space is initialized to aNull.
 * @require val.isArr()
 */
func void forceSize(Value th, Value val, AuintIdx len)
{
	ArrInfo *arr = arr_info(val);

	// Expand or contract allocation, as needed
	if (len != arr->avail)
	{
		mem::gccheck(th);	// Incremental GC before memory allocation events
		mem::reallocvector(th, arr.arr, 0, len, Value);
		arr.avail = len;
	}

	// Fill growth area with nulls
	arr.fill(arr.size, len, aNull);
	arr.size = len;
}

/**
 * Retrieve the value in array at specified position.
 * @require arr.isArr()
 */
func Value get(Value th, Value arr, AuintIdx pos)
{
	ArrInfo* a = arr_info(arr);
	return pos >= a.size ? aNull : a.arr[pos];
}

/**
 * Put val into the array starting at pos.
 * This can expand the size of the array.
 * @require arr.isArr()
 */
func void set(Value th, Value arr, AuintIdx pos, Value val)
{
	ArrInfo* a = arr_info(arr);

	// Grow, if needed
	if (pos + 1 >= a.avail) makeRoom(th, arr, pos + 1);
	// Fill with nulls if pos starts after end of array
	if (pos >= a.size) a.fill(a.size, pos, aNull);
	// Perform copy
	a.arr[pos] = val;
	mem::markChk(th, arr, val);
	// If final fill is past array size, reset size higher
	if (pos + 1 >= a.size) a.size = pos + 1;
}

/**
 * Append val to the end of the array (increasing array's size).
 * @require arr.isArr()
 */
func void add(Value th, Value arr, Value val)
{
	ArrInfo *a = arr_info(arr);
	AuintIdx sz = arr_size(arr);

	// Double size, if more space is needed
	if (sz + 1 > a.avail) makeRoom(th, arr, sz + (sz > 0 ? sz : 1));

	// Append value
	a.arr[sz] = val;
	mem::markChk(th, arr, val);
	a.size++;
}

/**
 * Propagate n copies of val into the array starting at pos.
 * This can expand the size of the array.
 * @require arr.isArr()
 */
func void repeat(Value th, Value arr, AuintIdx pos, AuintIdx n, Value val)
{
	ArrInfo* a = arr_info(arr);

	// Prevent unlikely overflow
	if (pos +% n < n) return;

	// Grow, if needed
	if (pos + n >= a.avail) makeRoom(th, arr, pos + n);
	// Fill with nulls if pos starts after end of array
	if (pos >= a.size) a.fill(a.size, pos, aNull);
	// Perform repeat copy
	a.fill(pos, pos + n, val);
	mem::markChk(th, arr, val); // only need to check once
	// If final fill is past array size, reset size higher
	if (pos + n >= a.size) a.size = pos + n;
}

/**
 * Delete n values out of the array starting at pos.
 * All values after these are preserved, essentially shrinking the array.
 * @require arr.isArr()
 */
func void del(Value th, Value arr, AuintIdx pos, AuintIdx n)
{
	ArrInfo *a = arr_info(arr);

	// Nothing to delete (or overflow)
	if (pos >= a.size || pos +% n < n) return;

	// Copy high end down over deleted portion
	if (pos + n < a.size)
	{
		memmove(&a.arr[pos], &a.arr[pos + n], (a.size - pos - n) * sizeof(Value));
	}
	else
	{
		n = a.size - pos;  // Clip n to end of array, if too large
	}
	a.size -= n; // Adjust size accordingly
}

/**
 * Insert n copies of val into the array starting at pos, expanding the array's size.
 * @require arr.isArr()
 */
func void ins(Value th, Value arr, AuintIdx pos, AuintIdx n, Value val)
{
	ArrInfo *a = arr_info(arr);

	// Prevent unlikely overflow
	if (a.size +% n < n) return;

	// Ensure array is large enough
	if (n + a.size >= a.avail) makeRoom(th, arr, n + a.size);

	// Move values up to make room for insertions
	if (pos <= a.size) memmove(&a,arr[pos+n], &a.arr[pos], (a.size - pos) * sizeof(Value));
	a.size += n;

	// Do any needed null fill plus the repeat copy
	rpt(th, arr, pos, n, val);
}

/**
 * Copy n2 values from arr2 starting at pos2 into array, replacing the n values in first array starting at pos.
 * This can increase or decrease the size of the array. arr and arr2 may be the same array.
 * @require arr.isArr()
 */
func void sub(Value th, Value arr, AuintIdx pos, AuintIdx n, Value arr2, AuintIdx pos2, AuintIdx n2)
{
	ArrInfo *a = arr_info(arr);

	// Prevent unlikely overflow
	if ((a.size - n) +% n2 < n2) return;

	// Ensure array is large enough
	if (a.size - n + n2 > a.avail) makeRoom(th, arr, a.size - n + n2);

	// Adjust position of upper values to make precise space for copy
	if (n != n2 && pos < a.size) memmove(&a.arr[pos + n2], &a.arr[pos + n], (a.size - pos - n) * sizeof(Value));

	// Fill with nulls if pos starts after end of array
	if (pos > a->size) a.fill(a.size, pos, aNull);

	// Perform copy
	if (arr2 && arr2.isPtr()) memmove(&a.arr[pos], &arr_info(arr2).arr[pos2], n2 * sizeof(Value));
	for (AintIdx i = n2 - 1; i >= 0; i--)
	{
		mem::markChk(th, arr, a.arr[pos+i]);
	}

	a.size += n2 - n;
}

/* Serialize an array's contents to indented text */
func void serialize(Value th, Value str, int indent, Value arr)
{
    // TODO
	ArrInfo *a = arr_info(arr);
	AuintIdx sz = arr_size(arr);
	string type = arr_info(arr).flags1 & TypeClo ? "+Closure" : "+List";

	strAppend(th, str, typ, strlen(typ));
	for (AuintIdx i = 0; i < sz; i++)
	{
		strAppend(th, str, "\n", 1);
		int ind = indent+1;
		while (ind--) strAppend(th, str, "\t", 1);
		serialize(th, str, indent+1, a.arr[i]);
	}
}

