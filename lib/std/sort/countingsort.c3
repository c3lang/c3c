module std::sort;
import std::sort::is;
import std::sort::cs @public;
import std::sort::qs;

<*
 Sort list using the counting sort algorithm.

 @require @list_is_by_ref(list) : "Expected the list to be passed by reference"
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_cmp_key_fn(#key_fn: ...key_fn, #list: list) : "Expected a transformation function which returns an unsigned integer."
*>
macro void countingsort(list, key_fn = ...) @builtin
{
	var list_length = $kindof(list) == SLICE ??? list.len : lengthof(*list);
	var $ListType = $kindof(list) == SLICE ??? $typeof(list) : $typeof(*list);
	$if $defined(key_fn):
		cs::csort{$ListType, $typeof(key_fn)}(list, 0, list_length, key_fn, ~(uint)0);
	$else
		cs::csort{$ListType, void*}(list, 0, list_length, ~(uint)0);
	$endif
}

macro void insertionsort_indexed(list, start, end, cmp = ..., context = ...) @builtin
{
	// When the context or cmp functions are not defined, we can simply use a dummy/default type.
	var $CmpFnType = $defined(cmp) ??? $typeof(cmp) : uint;
	var $ContextType = $defined(context) ??? $typeof(context) : uint;
	$if $kindof(list) == SLICE:
		is::isort{$typeof((list)), $CmpFnType, $ContextType}(list, (usz)start, (usz)end, ...cmp, ...context);
	$else
		is::isort{$typeof((*list)), $CmpFnType, $ContextType}(list, (usz)start, (usz)end, ...cmp, ...context);
	$endif

}

macro void quicksort_indexed(list, start, end, cmp = ..., context = ...) @builtin
{
	// When the context or cmp functions are not defined, we can simply use a dummy/default type.
	var $CmpFnType = $defined(cmp) ??? $typeof(cmp) : uint;
	var $ContextType = $defined(context) ??? $typeof(context) : uint;
	$if $kindof(list) == SLICE:
		qs::qsort{$typeof((list)), $CmpFnType, $ContextType}(list, (isz)start, (isz)(end-1), ...cmp, ...context);
	$else
		qs::qsort{$typeof((*list)), $CmpFnType, $ContextType}(list, (isz)start, (isz)(end-1), ...cmp, ...context);
	$endif
}

module std::sort::cs <Type, KeyFn>;

alias Counts @private = usz[256];
alias Ranges @private = usz[257];
alias Indexs @private = char[256];
alias ElementType = $typeof((Type){}[0]);

const bool NO_KEY_FN @private = KeyFn.kindof != FUNC;
const bool KEY_BY_VALUE @private = NO_KEY_FN ||| $defined($typefrom(KeyFn.paramsof[0].type) x = (ElementType){});
const bool LIST_HAS_REF @private = $defined(&(Type){}[0]);

alias KeyFnReturnType @if(!NO_KEY_FN) = $typefrom(KeyFn.returns) ;
alias KeyFnReturnType @if(NO_KEY_FN) = ElementType;
alias CmpCallback @if(KEY_BY_VALUE && NO_KEY_FN) = fn int(ElementType, ElementType) ;
alias CmpCallback @if(!KEY_BY_VALUE && NO_KEY_FN) = fn int(ElementType*, ElementType*);
alias CmpCallback @if(KEY_BY_VALUE && !NO_KEY_FN) = fn int(ElementType, ElementType, KeyFn);
alias CmpCallback @if(!KEY_BY_VALUE && !NO_KEY_FN) = fn int(ElementType*, ElementType*, KeyFn);

const bool IS_SLICE = Type.kindof == SLICE;
alias ListType = $typefrom(IS_SLICE ??? Type : Type*);
macro list_get(ListType l, i) @if(!IS_SLICE) => (*l)[i];
macro list_get(ListType l, i) @if(IS_SLICE) => l[i];
macro list_get_ref(ListType l, i) @if(!IS_SLICE) => &(*l)[i];
macro list_get_ref(ListType l, i) @if(IS_SLICE) => &l[i];

fn void csort(ListType list, usz low, usz high, KeyFn key_fn, uint byte_idx) @if (!NO_KEY_FN)
{
	_csort(list, low, high, byte_idx, key_fn);
}

fn void csort(ListType list, usz low, usz high, uint byte_idx) @if (NO_KEY_FN)
{
	_csort(list, low, high, byte_idx);
}

<*
 COMPATIBILITY: With the deprecation of `EMPTY_MACRO_SLOT` in 0.7.9, this inner function was introduced
 to prevent needing to further deprecate this module's downstream API. Since `key_fn` in `csort` appears
 inline with other position-based parameters, it would be a hassle to require all callers of `csort` to
 use named parameters when supplying `key_fn`.

 In other words, `csort(a_list, low, high, key_fn: some_fn, byte_idx: index_val)` breaks the existing API
 since explicitly naming `byte_idx` becomes a requirement.
*>
macro void _csort(ListType list, usz low, usz high, uint byte_idx, KeyFn key_fn = ...) @private
{
	if (high <= low) return;
	$if NO_KEY_FN:
		CmpCallback compare_fn = fn (lhs, rhs) => compare_to(lhs, rhs);
	$else
		CmpCallback compare_fn = fn (lhs, rhs, key_fn) => compare_to(key_fn(lhs), key_fn(rhs));
	$endif;

	byte_idx = byte_idx >= KeyFnReturnType.sizeof ? KeyFnReturnType.sizeof - 1 : byte_idx;

	Counts counts;
	Ranges ranges;
	Indexs indexs;

	KeyFnReturnType mn = ~(KeyFnReturnType)0;
	KeyFnReturnType mx = 0;

	char last_key = 0;
	char keys_ordered = 1;

	for (usz i = low; i < high; i++)
	{
		$switch:
			$case NO_KEY_FN:
				KeyFnReturnType k = list_get(list, i);
			$case KEY_BY_VALUE:
				KeyFnReturnType k = key_fn(list_get(list, i));
			$case LIST_HAS_REF:
				KeyFnReturnType k = key_fn(list_get_ref(list, i));
			$default:
				KeyFnReturnType k = key_fn(&&list_get(list, i));
		$endswitch;

		char key_byte = (char)((k >> (byte_idx * 8)) & 0xff);
		++counts[key_byte];

		mn = k < mn ? k : mn;
		mx = k > mx ? k : mx;

		keys_ordered = keys_ordered & (char)(key_byte >= last_key);
		last_key = key_byte;
	}

	KeyFnReturnType diff = mx - mn;
	if (diff == 0) return;

	ushort fallback0_count = 0;
	ushort fallback1_count = 0;
	ushort recursion_count = 0;

	usz total = 0;
	foreach (char i, count : counts)
	{
		indexs[fallback0_count]       = i;
		indexs[255 - recursion_count] = i;

		fallback0_count += (ushort)(count > 1 && count <= 32);
		recursion_count += (ushort)(count > 128);

		counts[i] = total;
		ranges[i] = total;
		total += count;
	}
	ranges[256] = total;

	ushort remaining_indexs = 256 - (fallback0_count + recursion_count);
	for(ushort i = 0; (i < 256) && remaining_indexs; i++) {
		indexs[fallback0_count + fallback1_count] = (char)i;
		usz count = ranges[i + 1] - ranges[i];
		ushort within_fallback1_range = (ushort)(count > 32 && count <= 128);
		fallback1_count += within_fallback1_range;
		remaining_indexs -= within_fallback1_range;
	}

	if (!keys_ordered)
	{
		usz sorted_count = 0;

		do
		{
			foreach (x, s : counts)
			{
				usz e = ranges[x + 1];
				sorted_count += (e - s);
				for (; s < e; s++)
				{
					$switch:
					$case NO_KEY_FN:
						KeyFnReturnType k = list_get(list, low + s);
					$case KEY_BY_VALUE:
						KeyFnReturnType k = key_fn(list_get(list, low + s));
					$case LIST_HAS_REF:
						KeyFnReturnType k = key_fn(list_get_ref(list, low + s));
					$default:
						KeyFnReturnType k = key_fn(&&list_get(list, low + s));
					$endswitch;
					char k_idx = (char)(k >> (byte_idx * 8));
					usz target_idx = counts[k_idx];
					$if IS_SLICE:
						@swap(list[low + s], list[low + target_idx]);
					$else
						@swap((*list)[low + s], (*list)[low + target_idx]);
					$endif
					counts[k_idx]++;
				}
			}
		} while (sorted_count < ranges[256]);
	}

	if (byte_idx)
	{
		for (usz p = 0; p < fallback0_count; p++) {
			usz i = indexs[p];

			usz	start_offset = ranges[i];
			usz	end_offset   = ranges[i + 1];

			insertionsort_indexed(list, low + start_offset, low + end_offset, compare_fn, ...key_fn);
		}

		for (usz p = 0; p < fallback1_count; p++) {
			usz i = indexs[fallback0_count + p];

			usz	start_offset = ranges[i];
			usz	end_offset   = ranges[i + 1];

			quicksort_indexed(list, low + start_offset, low + end_offset, compare_fn, ...key_fn);
		}

		for (usz p = 0; p < recursion_count; p++)
		{
			usz i = indexs[255 - p];

			usz	start_offset = ranges[i];
			usz	end_offset   = ranges[i + 1];

			$if $defined(key_fn):
				csort(list, low + start_offset, low + end_offset, key_fn, byte_idx - 1);
			$else
				csort(list, low + start_offset, low + end_offset, byte_idx - 1);
			$endif
		}
	}
}
