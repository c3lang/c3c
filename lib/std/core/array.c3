module std::core::array;
import std::collections::pair, std::io;

<*
 Returns true if the array contains at least one element, else false

 @param [in] array
 @param [in] element
 @require $kindof(array) == SLICE || $kindof(array) == ARRAY
 @require @typematch(array[0], element) : "array and element must have the same type"
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
 @require $kindof(array) == SLICE || $kindof(array) == ARRAY
 @require @typematch(array[0], element) : "array and element must have the same type"
 @return "the first index of the element"
 @return? NOT_FOUND
*>
macro usz? index_of(array, element)
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
 @require $kindof(array_ptr) == POINTER
 @require $kindof(*array_ptr) == VECTOR || $kindof(*array_ptr) == ARRAY
 @require $kindof((*array_ptr)[0]) == VECTOR || $kindof((*array_ptr)[0]) == ARRAY
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
macro usz? rindex_of(array, element)
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
 @require $kindof(arr1) == SLICE || $kindof(arr1) == ARRAY
 @require $kindof(arr2) == SLICE || $kindof(arr2) == ARRAY
 @require @typematch(arr1[0], arr2[0]) : "Arrays must have the same type"
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
 @require $kindof(arr1) == SLICE || $kindof(arr1) == ARRAY
 @require $kindof(arr2) == SLICE || $kindof(arr2) == ARRAY
 @require @typematch(arr1[0], arr2[0]) : "Arrays must have the same type"
 @ensure return.len == arr1.len + arr2.len
*>
macro tconcat(arr1, arr2) @nodiscard => concat(tmem, arr1, arr2);


<*
 Apply a reduction/folding operation to an iterable type. This walks along the input array
 and applies an `#operation` to each value, returning it to the `identity` (or "accumulator")
 base value.

 For example:
 ```c3
 int[] my_slice = { 1, 8, 12 };
 int folded = array::@reduce(my_slice, 2, fn (i, e) => i * e);
 assert(folded == (2 * 1 * 8 * 12));
 ```

 Notice how the given `identity` value started the multiplication chain at 2. When enumerating
 `my_slice`, each element is accumulated onto the `identity` value with each sequential iteration.
 ```
 i = 2;   // identity value
 i *= 1;   // my_slice[0]
 i *= 8;   // my_slice[1]
 i *= 12;   // my_slice[2]
 ```

 @param [in] array
 @param identity
 @param #operation : "The reduction/folding lambda function or function pointer to apply."

 @require @is_valid_list(array) : "Expected a valid list"
 @require $defined($typefrom(@reduce_fn(array, identity)) $func = #operation) : "Invalid lambda or function pointer type"
*>
macro @reduce(array, identity, #operation)
{
	$typefrom(@reduce_fn(array, identity)) $func = #operation;
	foreach (index, element : array) identity = $func(identity, element, index);
	return identity;
}

<*
 Apply a summation operator (+) to an identity value across a span of array elements
 and return the final accumulated result.

 @pure

 @param [in] array
 @param identity_value : "The base accumulator value to use for the sum"

 @require @is_valid_list(array) : "Expected a valid list"
 @require $defined(array[0] + array[0]) : "Array element type must implement the '+' operator"
 @require $defined($typeof(array[0]) t = identity_value) : "The identity type must be assignable to the array element type"
*>
macro @sum(array, identity_value = 0)
{
	return @reduce(array, ($typeof(array[0]))identity_value, fn (acc, e, u) => acc + e);
}

<*
 Apply a product operator (*) to an identity value across a span of array elements
 and return the final accumulated result.

 @pure

 @param [in] array
 @param identity_value : "The base accumulator value to use for the product"

 @require @is_valid_list(array) : "Expected a valid list"
 @require $defined(array[0] * array[0]) : "Array element type must implement the '*' operator"
 @require $defined($typeof(array[0]) t = identity_value) : "The identity type must be assignable to the array element type"
*>
macro @product(array, identity_value = 1)
{
	return @reduce(array, ($typeof(array[0]))identity_value, fn (acc, e, u) => acc * e);
}

<*
 Applies a given predicate function to each element of an array and returns a new
 array of `usz` values, each element representing an index within the original array
 where the predicate returned `true`.

 The `.len` value of the returned array can also be used to quickly identify how many
 input array elements matched the predicate.

 For example:
 ```c3
 int[] arr = { 0, 20, 4, 30 };
 int[] matched_indices = array::@indices_of(mem, arr, fn (u, a) => a > 10);
 ```

 The `matched_indices` variable should contain a dynamically-allocated array of `[1, 3]`,
 and thus its count indicates that 2 of the 4 elements matched the predicate condition.

 @param [&inout] allocator
 @param [in] array
 @param #predicate

 @require @is_valid_list(array) : "Expected a valid list"
 @require $defined($typefrom(@predicate_fn(array)) p = #predicate)
*>
macro usz[] @indices_of(Allocator allocator, array, #predicate)
{
	usz[] results = allocator::new_array(allocator, usz, lengthof(array));
	usz matches;

	$typefrom(@predicate_fn(array)) $predicate = #predicate;
	foreach (index, element : array)
	{
		if ($predicate(element, index)) results[matches++] = index;
	}

	return results[:matches];
}

<*
 Array `@indices_of` using the temp allocator.

 @param [in] array
 @param #predicate

 @require @is_valid_list(array) : "Expected a valid list"
 @require $defined($typefrom(@predicate_fn(array)) p = #predicate)
*>
macro usz[] @tindices_of(array, #predicate)
{
	return @indices_of(tmem, array, #predicate);
}


<*
 Applies a predicate function to each element of an input array and returns a new array
 containing shallow copies of _only_ the elements for which the predicate function returned
 a `true` value.

 For example:
 ```c3
 int[] my_arr = { 1, 2, 4, 10, 11, 45 };
 int[] evens = array::@filter(mem, my_arr, fn (e, u) => !(e % 2));
 assert(evens == (int[]){2, 4, 10 });
 ```

 @param [&inout] allocator
 @param [in] array
 @param #predicate

 @require @is_valid_list(array) : "Expected a valid list"
 @require $defined($typefrom(@predicate_fn(array)) p = #predicate)
*>
macro @filter(Allocator allocator, array, #predicate) @nodiscard
{
	var $InnerType = $typeof(array[0]);

	usz[] matched_indices = @indices_of(allocator, array, #predicate);
	defer allocator::free(allocator, matched_indices.ptr);   // can free this upon leaving this call

	if (!matched_indices.len) return ($InnerType[]){};

	$InnerType[] result = allocator::new_array(allocator, $InnerType, matched_indices.len);

	foreach (i, index : matched_indices) result[i] = array[index];

	return result;
}

<*
 Array `@filter` using the temp allocator.

 @param [in] array
 @param #predicate

 @require @is_valid_list(array) : "Expected a valid list"
 @require $defined($typefrom(@predicate_fn(array)) p = #predicate)
*>
macro @tfilter(array, #predicate) @nodiscard
{
	return @filter(tmem, array, #predicate);
}


<*
 Returns `true` if _any_ element of the input array returns `true` when
 the `#predicate` function is applied.

 @param [in] array
 @param #predicate

 @require @is_valid_list(array) : "Expected a valid list"
 @require $defined($typefrom(@predicate_fn(array)) p = #predicate)
*>
macro bool @any(array, #predicate)
{
	$typefrom(@predicate_fn(array)) $predicate = #predicate;
	foreach (index, element : array) if ($predicate(element, index)) return true;
	return false;
}


<*
 Returns `true` if _all_ elements of the input array return `true` when
 the `#predicate` function is applied.

 @param [in] array
 @param #predicate

 @require @is_valid_list(array) : "Expected a valid list"
 @require $defined($typefrom(@predicate_fn(array)) p = #predicate)
*>
macro bool @all(array, #predicate)
{
	$typefrom(@predicate_fn(array)) $predicate = #predicate;
	foreach (index, element : array) if (!$predicate(element, index)) return false;
	return true;
}


<*
 Zip together two separate arrays/slices into a single array of Pairs or return values. Values will
 be collected up to the length of the shorter array if `fill_with` is left undefined; otherwise, they
 will be collected up to the length of the LONGER array, with missing values in the shorter array being
 assigned to the value of `fill_with`. Return array elements do not have to be of the same type.

 For example:
 ```c3
 uint[] chosen_session_ids = server::get_random_sessions(instance)[:128];
 String[200] refreshed_session_keys = prng::new_keys_batch();

 Pair { uint, String }[] sessions_meta = array::zip(mem, chosen_session_ids, refreshed_session_keys);
 // The resulting Pair{}[] slice is then length of the shortest of the two arrays, so 128.

 foreach (i, &sess : sessions:meta) {
     // distribute new session keys to associated instance IDs
 }
 ```

 Or:
 ```c3
 String[] client_names = server::online_usernames(instance);
 uint128[] session_ids = server::user_keys();

 // in this example, we 'know' ahead of time that 'session_ids' can only ever be SHORTER
 //   than 'client_names', but never longer, because it's possible new users have logged
 //   in without getting whatever this 'session ID' is delegated to them.
 Pair { String, uint128 }[] zipped = array::tzip(client_names, session_ids, fill_with: uint128.max);

 server::refresh_session_keys_by_pair(zipped)!;
 ```

 ### When an `operation` is supplied...
 Apply an operation to each element of two slices or arrays and return the results of
 each operation into a newly allocated array.

 This essentially combines Iterable1 with Iterable2 using the `operation` functor.

 See the functional `zipWith` construct, which has a more appropriate name than, e.g., `map`;
   a la: https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:zipWith

 Similar to "normal" `zip`, this macro pads the shorter input array with a given `fill_with`, or
 an empty value if one isn't supplied. This `fill_with` is supplied to the `operation` functor
 _BEFORE_ calculating its result while zipping.

 For example: a functor of `fn char (char a, char b) => a + b` with a `fill_with` of 7,
 where the `left` array is the shorter iterable, will put 7 into that lambda in each place
 where `left` is being filled in during the zip operation.

 @param [&inout] allocator : "The allocator to use; default is the heap allocator."
 @param [in] left : "The left-side array. These items will be placed as the First in each Pair"
 @param [in] right : "The right-side array. These items will be placed as the Second in each Pair"
 @param #operation : "The function to apply. Must have a signature of `$typeof(a) (a, b)`, where the type of 'a' and 'b' is the element type of left/right respectively."
 @param fill_with : "The value used to fill or pad the shorter iterable to the length of the longer one while zipping."

 @require @is_valid_list(left) &&& @is_valid_list(right) : "Left and right sides must be integer indexable"
 @require @is_valid_operation(left, right, ...#operation) : "The operator must take two parameters matching the elements of the left and right side"
 @require @is_valid_fill(left, right, ...fill_with) : "The specified fill value does not match either the left or the right array's underlying type."
*>
macro @zip(Allocator allocator, left, right, #operation = ..., fill_with = ...) @nodiscard
{
	var $LeftType = $typeof(left[0]);
	var $RightType = $typeof(right[0]);

	var $Type = Pair { $LeftType, $RightType };
	bool $is_op = $defined(#operation);
	$if $is_op:
		$Type = $typeof(#operation).returns;
	$endif

	usz left_len = lengthof(left);
	usz right_len = lengthof(right);

	$LeftType left_fill;
	$RightType right_fill;
	usz result_len = min(left_len, right_len);
	$if $defined(fill_with):
		switch
		{
			case left_len > right_len:
				$if !$defined(($RightType)fill_with):
					unreachable();
				$else
					right_fill = ($RightType)fill_with;
					result_len = left_len;
				$endif
			case left_len < right_len:
				$if !$defined(($LeftType)fill_with):
					unreachable();
				$else
					left_fill = ($LeftType)fill_with;
					result_len = right_len;
				$endif
		}
	$endif

	if (result_len == 0) return ($Type[]){};

	$Type[] result = allocator::alloc_array(allocator, $Type, result_len);

	foreach (idx, &item : result)
	{
		$if $is_op:
			var $LambdaType = $typeof(fn $Type ($LeftType a, $RightType b) => ($Type){});
			$LambdaType $operation = ($LambdaType)#operation;
			$LeftType lval = idx >= left_len ? left_fill : left[idx];
			$RightType rval = idx >= right_len ? right_fill : right[idx];
			*item = $operation(lval, rval);
		$else
			*item = {
				idx >= left_len ? left_fill : left[idx],
				idx >= right_len ? right_fill : right[idx]
			};
		$endif
	}

	return result;
}

<*
 Array 'zip' using the temp allocator.

 @param [in] left : "The left-side array. These items will be placed as the First in each Pair"
 @param [in] right : "The right-side array. These items will be placed as the Second in each Pair"
 @param #operation : "The function to apply. Must have a signature of `$typeof(a) (a, b)`, where the type of 'a' and 'b' is the element type of left/right respectively."
 @param fill_with : "The value used to fill or pad the shorter iterable to the length of the longer one while zipping."

 @require @is_valid_list(left) &&& @is_valid_list(right) : "Left and right sides must be integer indexable"
 @require @is_valid_operation(left, right, ...#operation) : "The operator must take two parameters matching the elements of the left and right side"
 @require @is_valid_fill(left, right, ...fill_with) : "The specified fill value does not match either the left or the right array's underlying type."
*>
macro @tzip(left, right, #operation = ..., fill_with = ...) @nodiscard
{
	return @zip(tmem, left, right, #operation: ...#operation, fill_with: ...fill_with);
}


<*
 Apply an operation to each element of two slices or arrays and store the results of
 each operation into the 'left' value.

 This is useful because no memory allocations are required in order to perform the operation.

 A good example of using this might be using algorithmic transformations on data in-place:
 ```
 char[] partial_cipher = get_next_plaintext_block();

 array::@zip_into(
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
 @param #operation : "The function to apply. Must have a signature of `$typeof(a) (a, b)`, where the type of 'a' and 'b' is the element type of left/right respectively."

 @require @is_valid_list(left) : "Expected a valid list"
 @require @is_valid_list(right) : "Expected a valid list"
 @require lengthof(right) >= lengthof(left) : `Right side length must be >= the destination (left) side length; consider using a sub-array of data for the assignment.`
 @require $defined($typefrom(@zip_into_fn(left, right)) x = #operation) : "The functor must use the same types as the `left` and `right` inputs, and return a value of the `left` type."
*>
macro @zip_into(left, right, #operation)
{
	$typefrom(@zip_into_fn(left, right)) $operation = #operation;

	foreach (i, &v : left) *v = $operation(left[i], right[i]);
}


// --- helper functions
module std::core::array @private;


macro typeid @predicate_fn(#array) @const
{
	return $typeof(fn bool ($typeof(#array[0]) a, usz index = 0) => true).typeid;
}

macro typeid @reduce_fn(#array, #identity) @const
{
	return @typeid(fn $typeof(#identity) ($typeof(#identity) i, $typeof(#array[0]) a, usz index = 0) => i);
}

macro typeid @zip_into_fn(#left, #right) @const
{
	return @typeid(fn $typeof(#left[0]) ($typeof(#left[0]) l, $typeof(#right[0]) r) => l);
}

macro bool @is_valid_operation(#left, #right, #operation = ...) @const
{
	$switch:
		$case !$defined(#operation):
			return true;
		$case $kindof(#operation) != FUNC:
			return false;
		$default:
			return $defined(#operation(#left[0], #right[0]));
	$endswitch
}

macro bool @is_valid_list(#expr) @const
{
	return $defined(#expr[0], lengthof(#expr));
}

macro bool @is_valid_fill(left, right, fill_with = ...)
{
	$if !$defined(fill_with):
		return true;
	$else
		usz left_len = lengthof(left);
		usz right_len = lengthof(right);
		if (left_len == right_len) return true;
		return left_len > right_len ? $defined(($typeof(right[0]))fill_with) : $defined(($typeof(left[0]))fill_with);
	$endif
}
