module std::core::array;
import std::core::array::slice;

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