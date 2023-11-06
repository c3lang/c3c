module std::core::array;

/**
 * @param [in] array
 * @param [in] element
 * @return "the first index of the element"
 * @return! SearchResult.MISSING
 **/
macro index_of(array, element)
{
	foreach (i, &e : array)
	{
		if (*e == element) return i;
	}
	return SearchResult.MISSING?;
}

/**
 * @param [in] array
 * @param [in] element
 * @return "the last index of the element"
 * @return! SearchResult.MISSING
 **/
macro rindex_of(array, element)
{
	foreach_r (i, &e : array)
	{
		if (*e == element) return i;
	}
	return SearchResult.MISSING?;
}

/**
 * Concatenate two arrays or subarrays, returning a subarray containing the concatenation of them.
 *
 * @param [in] arr1
 * @param [in] arr2
 * @param [&inout] allocator "The allocator to use, default is the heap allocator"
 * @require @typekind(arr1) == SUBARRAY || @typekind(arr1) == ARRAY
 * @require @typekind(arr2) == SUBARRAY || @typekind(arr2) == ARRAY
 * @require @typeis(arr1[0], $typeof(arr2[0])) "Arrays must have the same type"
 * @ensure result.len == arr1.len + arr2.len
 **/
macro concat_new(arr1, arr2, Allocator* allocator = mem::heap())
{
	var $Type = $typeof(arr1[0]);
	$Type[] result = allocator.new_array($Type, arr1.len + arr2.len);
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

/**
 * Concatenate two arrays or subarrays, returning a subarray containing the concatenation of them,
 * allocated using the temp allocator.
 *
 * @param [in] arr1
 * @param [in] arr2
 * @require @typekind(arr1) == SUBARRAY || @typekind(arr1) == ARRAY
 * @require @typekind(arr2) == SUBARRAY || @typekind(arr2) == ARRAY
 * @require @typeis(arr1[0], $typeof(arr2[0])) "Arrays must have the same type"
 * @ensure result.len == arr1.len + arr2.len
 **/
macro tconcat(arr1, arr2) => concat(arr1, arr2, mem::temp());
