module std::core::array;

import std::core::array::slice;
import std::collections::pair;


<*
 Returns true if the array contains at least one element, else false
 
 @param [in] array
 @param [in] element
 @require @typekind(array) == SLICE || @typekind(array) == ARRAY
 @require @typeis(array[0], $typeof(element)) : "array and element must have the same type"
*>
macro bool contains(array, element)
{
	foreach (&item : array)
	{
		if (*item == element) return true;
	}
	return false;	
}


<*
 Return the first index of element found in the array, searching from the start.
 
 @param [in] array
 @param [in] element
 @return "the first index of the element"
 @return? NOT_FOUND
*>
macro index_of(array, element)
{
	foreach (i, &e : array)
	{
		if (*e == element) return i;
	}
	return NOT_FOUND?;
}


<*
 Slice a 2d array and create a Slice2d from it.

 @param array_ptr : "the pointer to create a slice from"
 @param x : "The starting position of the slice x, optional"
 @param y : "The starting position of the slice y, optional"
 @param xlen : "The length of the slice in x, defaults to the length of the array"
 @param ylen : "The length of the slice in y, defaults to the length of the array"
 @return "A Slice2d from the array"
 @require @typekind(array_ptr) == POINTER
 @require @typekind(*array_ptr) == VECTOR || @typekind(*array_ptr) == ARRAY
 @require @typekind((*array_ptr)[0]) == VECTOR || @typekind((*array_ptr)[0]) == ARRAY
*>
macro slice2d(array_ptr, x = 0, xlen = 0, y = 0, ylen = 0)
{
	if (xlen < 1) xlen = $typeof((*array_ptr)[0]).len + xlen;
	if (ylen < 1) ylen = $typeof((*array_ptr)).len + ylen;
	var $ElementType = $typeof((*array_ptr)[0][0]);
	return (Slice2d{$ElementType}) { ($ElementType*)array_ptr, $typeof((*array_ptr)[0]).len, y, ylen, x, xlen };
}


<*
 Return the first index of element found in the array, searching in reverse from the end.
 
 @param [in] array
 @param [in] element
 @return "the last index of the element"
 @return? NOT_FOUND
*>
macro rindex_of(array, element)
{
	foreach_r (i, &e : array)
	{
		if (*e == element) return i;
	}
	return NOT_FOUND?;
}


<*
 Concatenate two arrays or slices, returning a slice containing the concatenation of them.

 @param [in] arr1
 @param [in] arr2
 @param [&inout] allocator : "The allocator to use, default is the heap allocator"
 @require @typekind(arr1) == SLICE || @typekind(arr1) == ARRAY
 @require @typekind(arr2) == SLICE || @typekind(arr2) == ARRAY
 @require @typeis(arr1[0], $typeof(arr2[0])) : "Arrays must have the same type"
 @ensure result.len == arr1.len + arr2.len
*>
macro concat(Allocator allocator, arr1, arr2) @nodiscard
{
	var $Type = $typeof(arr1[0]);
	$Type[] result = allocator::alloc_array(allocator, $Type, arr1.len + arr2.len);
	if (arr1.len > 0)
	{
		mem::copy(result.ptr, &arr1[0], arr1.len * $Type.sizeof, $Type.alignof, $Type.alignof);
	}
	if (arr2.len > 0)
	{
		mem::copy(&result[arr1.len], &arr2[0], arr2.len * $Type.sizeof, $Type.alignof, $Type.alignof);
	}
	return result;
}

<*
 Concatenate two arrays or slices, returning a slice containing the concatenation of them,
 allocated using the temp allocator.

 @param [in] arr1
 @param [in] arr2
 @require @typekind(arr1) == SLICE || @typekind(arr1) == ARRAY
 @require @typekind(arr2) == SLICE || @typekind(arr2) == ARRAY
 @require @typeis(arr1[0], $typeof(arr2[0])) : "Arrays must have the same type"
 @ensure return.len == arr1.len + arr2.len
*>
macro tconcat(arr1, arr2) @nodiscard => concat(tmem, arr1, arr2);


<*
 Zip together two separate arrays/slices into a single array of Pairs. Values will be collected
 up to the length of the shorter array. Pair elements (and thus arrays) do not have to be the same type.

 @param [in] left
 @param [in] right
 @param [&inout] allocator : "The allocator to use, default is the heap allocator"
 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require left.len > 0 || right.len > 0 : `At least one array/slice must contain an item.`
 @ensure return.len == min(left.len, right.len)
*>
macro zip(Allocator allocator, left, right) @nodiscard
{
	var $LeftType = $typeof(left[0]);
	var $RightType = $typeof(right[0]);
	var $Type = Pair { $LeftType, $RightType };

	usz result_len = min(left.len, right.len);
	$Type[] result = allocator::alloc_array(allocator, $Type, result_len)[:result_len];

	$LeftType[] left_clone = allocator::clone_slice(allocator, left);
	$RightType[] right_clone = allocator::clone_slice(allocator, right);

	foreach (idx, &item : result) *item = { left_clone[idx], right_clone[idx] };

	return result;
}

<*
 Array 'zip' using the temp allocator.

 @param [in] left
 @param [in] right
 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require left.len > 0 || right.len > 0 : `At least one array/slice must contain an item.`
 @ensure return.len == min(left.len, right.len)
*>
macro tzip(left, right) @nodiscard => zip(tmem, left, right);


<*
 Zip together two separate arrays/slices into a single array of Pairs. Values will be collected
 up to the length of the LONGER array. Pair elements (and thus arrays) do not have to be the same type.

 @param [in] left
 @param [in] right
 @param [&inout] allocator : "The allocator to use, default is the heap allocator"
 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require left.len > 0 || right.len > 0 : `At least one array/slice must contain an item.`
 @ensure return.len == max(left.len, right.len)
*>
macro zip_longest(Allocator allocator, left, right, void* fill_value = null) @nodiscard
{
	var $LeftType = $typeof(left[0]);
	var $RightType = $typeof(right[0]);
	var $Type = Pair { $LeftType, $RightType };

	$LeftType filled_left = (null == fill_value || left.len > right.len) ? ($LeftType){} : *($LeftType*)fill_value;
	$RightType filled_right = (null == fill_value || right.len > left.len) ? ($RightType){} : *($RightType*)fill_value;

	usz result_len = max(left.len, right.len);
	$Type[] result = allocator::alloc_array(allocator, $Type, result_len)[:result_len];

	$LeftType[] left_clone = allocator::clone_slice(allocator, left);
	$RightType[] right_clone = allocator::clone_slice(allocator, right);

	foreach (idx, &item : result) *item = {
		idx >= left_clone.len ? filled_left : left_clone[idx],
		idx >= right_clone.len ? filled_right : right_clone[idx]
	};

	allocator::free(allocator, left_clone.ptr);
	allocator::free(allocator, right_clone.ptr);

	return result;
}

<*
 Array 'zip_longest' but with the temp allocator.

 @param [in] left
 @param [in] right
 @param [in] fill_value
 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require left.len > 0 || right.len > 0 : `At least one array/slice must contain an item.`
 @ensure return.len == max(left.len, right.len)
*>
macro tzip_longest(left, right, void* fill_value = null) @nodiscard => zip_longest(tmem, left, right, fill_value);


<*
 Apply an operation to each element of two slices or arrays and return the results of
 each operation into a new array.

 @param [inout] left
 @param [in] right
 @param operation : "The function to apply. Must have a signature of $typeof(a) (a, b), where the type of 'a' and 'b' is the element type of left/right respectively."

 @require @typeis(left, $typeof(right)) : `Both operands must be the same type.`
 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require left.len > 0 || right.len > 0 : `At least one array/slice must contain an item.`
 @ensure return.len == min(left.len, right.len)
*>
macro zip_with(Allocator allocator, left, right, operation) @nodiscard
{
	var $Type = $typeof(left[0]);

	usz result_len = min(left.len, right.len);
	$Type[] new_arr = allocator::alloc_array(allocator, $Type, result_len)[:result_len];

	foreach (i, &v : new_arr) *v = operation(left[i], right[i]);

	return new_arr;
}

<*
 Array 'zip_with' using the temp allocator.

 @param [inout] left
 @param [in] right
 @param operation : `The function to apply. Must have a signature of $typeof(a) (a, b), where the type of 'a' and 'b' is the element type of left/right respectively.`

 @require @typeis(left, $typeof(right)) : `Both operands must be the same type.`
 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require left.len > 0 || right.len > 0 : `At least one array/slice must contain an item.`
 @ensure return.len == min(left.len, right.len)
*>
macro tzip_with(left, right, operation) @nodiscard => zip_with(tmem, left, right, operation);


<*
 Apply an operation to each element of two slices or arrays and return the results of
 each operation into a new array.

 @param [inout] left
 @param [in] right
 @param operation : "The function to apply. Must have a signature of $typeof(a) (a, b), where the type of 'a' and 'b' is the element type of left/right respectively."

 @require @typeis(left, $typeof(right)) : `Both operands must be the same type.`
 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require @typeis(left, $typeof(left)) : `The fill value must be the same type as the zipped array.`
 @require left.len > 0 || right.len > 0 : `At least one array/slice must contain an item.`
 @ensure return.len == max(left.len, right.len)
*>
macro zip_with_longest(Allocator allocator, left, right, operation, fill_value = 0) @nodiscard
{
	var $Type = $typeof(left[0]);

	usz result_len = max(left.len, right.len);
	$Type[] new_arr = allocator::alloc_array(allocator, $Type, result_len)[:result_len];

	foreach (i, &v : new_arr)
	{
		$Type lval = i >= left.len ? fill_value : left[i];
		$Type rval = i >= right.len ? fill_value : right[i];

		*v = operation(lval, rval);
	}

	return new_arr;
}

<*
 Array 'zip_with_longest' using the temp allocator.

 @param [inout] left
 @param [in] right
 @param operation : `The function to apply. Must have a signature of $typeof(a) (a, b), where the type of 'a' and 'b' is the element type of left/right respectively.`

 @require @typeis(left, $typeof(right)) : `Both operands must be the same type.`
 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require @typeis(left, $typeof(left)) : `The fill value must be the same type as the zipped array.`
 @require left.len > 0 || right.len > 0 : `At least one array/slice must contain an item.`
 @ensure return.len == max(left.len, right.len)
*>
macro tzip_with_longest(left, right, operation, fill_value = {}) @nodiscard => zip_with_longest(tmem, left, right, operation, fill_value);


<*
 Apply an operation to each element of two slices or arrays and store the results of
 each operation into the 'left' value.

 This is useful because no memory allocations are required in order to perform the operation.

 A good example of using this might be using algorithmic transformations on data in-place:
 ```
 char[] partial_cipher = get_next_plaintext_block();

 array::zip_with_into(
     partial_cipher[ENCRYPT_OFFSET:XOR_KEY.len],
	 XOR_KEY,
	 fn char (char a, char b) => a ^ (b * 5) % 37
 );
 ```

 This applies the lambda expression to the left (`partial_cipher`) and right (`XOR_KEY`) slices
 and stores the end result in-place within the `partial_cipher` slice. This is in contrast to a
 regular `zip_with` which will create a cloned final result and return it.

 @param [inout] left
 @param [in] right
 @param operation : "The function to apply. Must have a signature of `$typeof(a) (a, b)`, where the type of 'a' and 'b' is the element type of left/right respectively."

 @require @typeis(left, $typeof(right)) : `Both operands must be the same type.`
 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require left.len > 0 : `The destination slice/array cannot be empty.`
 @require right.len >= left.len : `Right side of operation must be >= the destination (left) side; consider using a sub-array of data for the assignment.`
*>
macro zip_with_into(left, right, operation)
{
	foreach (i, &v : left) *v = operation(left[i], right[i]);
}
