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

 For example:
 ```
 uint[] chosen_session_ids = server::get_random_sessions(instance)[:128];
 String[200] refreshed_session_keys = prng::new_keys_batch();

 Pair { uint, String }[] sessions_meta = array::zip(mem, chosen_session_ids, refreshed_session_keys);
 // The resulting Pair{}[] slice is then length of the shortest of the two arrays, so 128.

 foreach (i, &sess : sessions:meta) {
     // distribute new session keys to associated instance IDs
 }
 ```

 @param [&inout] allocator : "The allocator to use; default is the heap allocator."
 @param [in] left
 @param [in] right

 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @ensure return.len == min(left.len, right.len)
*>
macro zip(Allocator allocator, left, right) @nodiscard
{
	var $LeftType = $typeof(left[0]);
	var $RightType = $typeof(right[0]);

	var $Type = Pair { $LeftType, $RightType };

	usz result_len = min(left.len, right.len);
	if (result_len == 0) return ($Type[]){};

	$Type[] result = allocator::alloc_array(allocator, $Type, result_len)[:result_len];

	$LeftType[] left_clone = allocator::clone_slice(allocator, left);
	$RightType[] right_clone = allocator::clone_slice(allocator, right);

	foreach (idx, &item : result) *item = { left_clone[idx], right_clone[idx] };

	allocator::free(allocator, left_clone.ptr);
	allocator::free(allocator, right_clone.ptr);

	return result;
}

<*
 Array 'zip' using the temp allocator.

 @param [in] left
 @param [in] right
*>
macro tzip(left, right) @nodiscard
{
	return zip(tmem, left, right);
}


<*
 Zip together two separate arrays/slices into a single array of Pairs. Values will be collected
 up to the length of the LONGER array. Pair elements (and thus arrays) do not have to be the same type.

 The optional `fill_value` specifies the value to use when padding the shorter array up to the length of
 the longer array when zipping.

 @param [&inout] allocator : "The allocator to use, default is the heap allocator"
 @param [in] left
 @param [in] right
 @param fill_value : "The value used to fill or pad the shorter iterable to the length of the longer one while zipping. Defaults to an empty value (0 or {})."

 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require @is_empty_macro_slot(fill_value) ||| (@is_valid_macro_slot(fill_value) &&& (@assignable_to(fill_value, $typeof(left[0])) ||| @assignable_to(fill_value, $typeof(right[0])))) :
	"The specified fill value does not match either the left or the right array's underlying type. Try an explicit cast on `fill_value` instead."
 @ensure return.len == max(left.len, right.len)
*>
macro zip_longest(Allocator allocator, left, right, fill_value = EMPTY_MACRO_SLOT) @nodiscard
{
	var $LeftType = $typeof(left[0]);
	var $RightType = $typeof(right[0]);
	var $Type = Pair { $LeftType, $RightType };

	usz result_len = max(left.len, right.len);
	if (result_len == 0) return ($Type[]){};

	Pair { $LeftType, $RightType } fills = @get_zip_fill_values($LeftType, $RightType, fill_value);

	$Type[] result = allocator::alloc_array(allocator, $Type, result_len)[:result_len];

	$LeftType[] left_clone = allocator::clone_slice(allocator, left);
	$RightType[] right_clone = allocator::clone_slice(allocator, right);

	foreach (idx, &item : result) *item = {
		idx >= left_clone.len ? fills.first : left_clone[idx],
		idx >= right_clone.len ? fills.second : right_clone[idx]
	};

	allocator::free(allocator, left_clone.ptr);
	allocator::free(allocator, right_clone.ptr);

	return result;
}

<*
 Array `zip_longest` with the temp allocator.

 @param [in] left
 @param [in] right
 *>
macro tzip_longest(left, right, fill_value = EMPTY_MACRO_SLOT) @nodiscard
{
	return zip_longest(tmem, left, right, fill_value);
}


<*
 Apply an operation to each element of two slices or arrays and return the results of
 each operation into a newly allocated array.

 This essentially combines Iterable1 with Iterable2 using the `operation` functor.

 See the functional `zipWith` construct, which has a more appropriate name than, e.g., `map`;
   a la: https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:zipWith

 @param [&inout] allocator : "The allocator to use; default is the heap allocator."
 @param [in] left
 @param [in] right
 @param operation : "The function to apply. Must have a signature of `fn c (a, b)`, where the type of 'a' and 'b' is the element type of left/right respectively."

 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require @assignable_to(left[0], $typeof(operation).paramsof[0].type) : "The first functor param must be assignable to the type of `left`."
 @require @assignable_to(right[0], $typeof(operation).paramsof[1].type) : "The second functor param must be assignable to the type of `right`."
 @ensure return.len == min(left.len, right.len)
*>
macro zip_with(Allocator allocator, left, right, operation) @nodiscard
{
	var $ReturnType = $typeof(operation).returns;

	usz result_len = min(left.len, right.len);
	if (result_len == 0) return ($ReturnType[]){};

	$ReturnType[] new_arr = allocator::alloc_array(allocator, $ReturnType, result_len)[:result_len];

	foreach (i, &v : new_arr) *v = operation(left[i], right[i]);

	return new_arr;
}

<*
 Array `zip_with` using the temp allocator.

 @param [in] left
 @param [in] right
*>
macro tzip_with(left, right, operation) @nodiscard
{
	return zip_with(tmem, left, right, operation);
}


<*
 Apply an operation to each element of two slices or arrays and return the results of
 each operation into a new array.

 Similar to `zip_longest`, this macro pads the shorter input array with a given `fill_value`,
 or an empty value if one isn't supplied. This `fill_value` is supplied to the `operation`
 functor BEFORE calculating its result while zipping.

 For example: a functor of `fn char (char a, char b) => a + b` with a `fill_value` of 7,
 where the `left` array is the shorter iterable, will put 7 into that lambda in each place
 where `left` is being filled in during the zip operation.

 @param [&inout] allocator : "The allocator to use; default is the heap allocator."
 @param [in] left
 @param [in] right
 @param operation : "The function to apply. Must have a signature of `fn c (a, b)`, where the type of 'a' and 'b' is the element type of left/right respectively."
 @param fill_value : "The default value to apply to the shorter iterable BEFORE the application of the `operation` functor."

 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require @assignable_to(left[0], $typeof(operation).paramsof[0].type) : "The first functor param must be assignable to the type of `left`."
 @require @assignable_to(right[0], $typeof(operation).paramsof[1].type) : "The second functor param must be assignable to the type of `right`."
 @require @is_empty_macro_slot(fill_value) ||| (@is_valid_macro_slot(fill_value) &&& (@assignable_to(fill_value, $typeof(left[0])) ||| @assignable_to(fill_value, $typeof(right[0])))) :
	"The specified fill value does not match either the left or the right array's underlying type. Try an explicit cast on `fill_value` instead."
 @ensure return.len == max(left.len, right.len)
*>
macro zip_with_longest(Allocator allocator, left, right, operation, fill_value = EMPTY_MACRO_SLOT) @nodiscard
{
	var $LeftType = $typeof(left[0]);
	var $RightType = $typeof(right[0]);
	var $ReturnType = $typeof(operation).returns;

	usz result_len = max(left.len, right.len);
	if (result_len == 0) return ($ReturnType[]){};

	Pair { $LeftType, $RightType } fills = @get_zip_fill_values($LeftType, $RightType, fill_value);

	$ReturnType[] new_arr = allocator::alloc_array(allocator, $ReturnType, result_len)[:result_len];

	foreach (i, &v : new_arr)
	{
		$LeftType lval = i >= left.len ? fills.first : left[i];
		$RightType rval = i >= right.len ? fills.second : right[i];

		*v = operation(lval, rval);
	}

	return new_arr;
}

<*
 Array 'zip_with_longest' using the temp allocator.

 @param [in] left
 @param [in] right
*>
macro tzip_with_longest(left, right, operation, fill_value = EMPTY_MACRO_SLOT) @nodiscard
{
	return zip_with_longest(tmem, left, right, operation, fill_value);
}


<*
 Apply an operation to each element of two slices or arrays and store the results of
 each operation into the 'left' value.

 This is useful because no memory allocations are required in order to perform the operation.

 A good example of using this might be using algorithmic transformations on data in-place:
 ```
 char[] partial_cipher = get_next_plaintext_block();

 array::zip_with_into(
     partial_cipher[ENCRYPT_OFFSET:BASE_KEY.len],
	 BASE_KEY,
	 fn char (char a, char b) => a ^ (b * 5) % 37
 );
 ```

 This parameterizes the lambda function with left (`partial_cipher`) and right (`BASE_KEY`) slice
 elements and stores the end result in-place within the left slice. This is in contrast to a
 regular `zip_with` which will create a cloned final result and return it.

 @param [inout] left : `Slice to store results of applied functor/operation.`
 @param [in] right : `Slice to apply in the functor/operation.`
 @param operation : "The function to apply. Must have a signature of `$typeof(a) (a, b)`, where the type of 'a' and 'b' is the element type of left/right respectively."

 @require @typekind(left) == SLICE || @typekind(left) == ARRAY
 @require @typekind(right) == SLICE || @typekind(right) == ARRAY
 @require right.len >= left.len : `Right side length must be >= the destination (left) side; consider using a sub-array of data for the assignment.`
 @require @assignable_to(operation, $typeof(fn $typeof(left[0]) ($typeof(left[0]) l, $typeof(right[0]) r) => l)) :
 	"The functor must use the same types as the `left` and `right` inputs, and return a value of the `left` type."
*>
macro zip_with_into(left, right, operation)
{
	foreach (i, &v : left) *v = operation(left[i], right[i]);
}


macro @get_zip_fill_values($LeftType, $RightType, fill_value) @local
{
	Pair { $LeftType, $RightType } result;

	// This would be cleaner if it were easier to set a dynamic type to a default or 0'd value without
	//   caring about the underlying type.
	$if @is_empty_macro_slot(fill_value):   // empty fill value
		$if types::is_numerical($LeftType):
			result.first = 0;
		$else
			result.first = {};
		$endif
		$if types::is_numerical($RightType):
			result.second = 0;
		$else
			result.second = {};
		$endif
	$else   // supplied fill value - one of left/right MUST get filled, but since it's not a CT len, we don't know which one at CT
		$if @assignable_to(fill_value, $LeftType):
			result.first = ($LeftType)fill_value;
		$else
			$if types::is_numerical($LeftType):
				result.first = 0;
			$else
				result.first = {};
			$endif
		$endif
		$if @assignable_to(fill_value, $RightType):
			result.second = ($RightType)fill_value;
		$else
			$if types::is_numerical($RightType):
				result.second = 0;
			$else
				result.second = {};
			$endif
		$endif
	$endif

	return result;
}
