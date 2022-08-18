module std::core::array;
import std::core::mem::array;

macro tconcat(arr1, arr2)
{
	var $Type = $typeof(arr1[0]);
	$Type[] result = mem::array::talloc($Type, arr1.len + arr2.len);
	if (arr1.len > 0)
	{
		mem::copy(result.ptr, &arr1[0], arr1.len * $Type.sizeof, $alignof($Type), $alignof($Type));
	}
	if (arr2.len > 0)
	{
		mem::copy(&result[arr1.len], &arr2[0], arr2.len * $Type.sizeof, $alignof($Type), $alignof($Type));
	}
	return result;
}

macro concat(arr1, arr2)
{
	var $Type = $typeof(arr1[0]);
	$Type[] result = mem::array::alloc($Type, arr1.len + arr2.len);
	if (arr1.len > 0)
	{
		mem::copy(result.ptr, &arr1[0], arr1.len * $Type.sizeof, $alignof($Type), $alignof($Type));
	}
	if (arr2.len > 0)
	{
		mem::copy(&result[arr1.len], &arr2[0], arr2.len * $Type.sizeof, $alignof($Type), $alignof($Type));
	}
	return result;
}