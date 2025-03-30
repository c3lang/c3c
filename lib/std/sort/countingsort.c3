module std::sort;
import std::sort::is;
import std::sort::cs;
import std::sort::qs;

<*
Sort list using the counting sort algorithm.
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_cmp_key_fn(key_fn, list) : "Expected a transformation function which returns an unsigned integer."
*>
macro countingsort(list, key_fn = EMPTY_MACRO_SLOT) @builtin
{
	usz len = sort::len_from_list(list);
	cs::csort{$typeof(list), $typeof(key_fn)}(list, 0, len, key_fn, ~((uint)0));
}

macro insertionsort_indexed(list, start, end, cmp = EMPTY_MACRO_SLOT, context = EMPTY_MACRO_SLOT) @builtin
{
	is::isort{$typeof(list), $typeof(cmp), $typeof(context)}(list, (usz)start, (usz)end, cmp, context);
}

macro quicksort_indexed(list, start, end, cmp = EMPTY_MACRO_SLOT, context = EMPTY_MACRO_SLOT) @builtin
{
	qs::qsort{$typeof(list), $typeof(cmp), $typeof(context)}(list, (isz)start, (isz)(end-1), cmp, context);
}

module std::sort::cs{Type, KeyFn};

alias Counts @private = usz[256];
alias Ranges @private = usz[257];
alias Indexs @private = char[256];
alias ElementType = $typeof((Type){}[0]);

const bool NO_KEY_FN @private = types::is_same(KeyFn, EmptySlot);
const bool KEY_BY_VALUE @private = NO_KEY_FN ||| $assignable((Type){}[0], $typefrom(KeyFn.paramsof[0].type));
const bool LIST_HAS_REF @private = $defined(&(Type){}[0]);

alias KeyFnReturnType @if(!NO_KEY_FN) = $typefrom(KeyFn.returns) ;
alias KeyFnReturnType @if(NO_KEY_FN) = ElementType;
alias CmpCallback @if(KEY_BY_VALUE && NO_KEY_FN) = fn int(ElementType, ElementType) ;
alias CmpCallback @if(!KEY_BY_VALUE && NO_KEY_FN) = fn int(ElementType*, ElementType*);
alias CmpCallback @if(KEY_BY_VALUE && !NO_KEY_FN) = fn int(ElementType, ElementType, KeyFn);
alias CmpCallback @if(!KEY_BY_VALUE && !NO_KEY_FN) = fn int(ElementType*, ElementType*, KeyFn);

fn void csort(Type list, usz low, usz high, KeyFn key_fn, uint byte_idx)
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
				KeyFnReturnType k = list[i];
			$case KEY_BY_VALUE:
				KeyFnReturnType k = key_fn(list[i]);
			$case LIST_HAS_REF:
				KeyFnReturnType k = key_fn(&list[i]);
			$default:
				KeyFnReturnType k = key_fn(&&list[i]);
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
						KeyFnReturnType k = list[low + s];
					$case KEY_BY_VALUE:
						KeyFnReturnType k = key_fn(list[low + s]);
					$case LIST_HAS_REF:
						KeyFnReturnType k = key_fn(&list[low + s]);
					$default:
						KeyFnReturnType k = key_fn(&&list[low + s]);
					$endswitch;
					char k_idx = (char)(k >> (byte_idx * 8));
					usz target_idx = counts[k_idx];
					@swap(list[low + s], list[low + target_idx]);
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

			insertionsort_indexed(list, low + start_offset, low + end_offset, compare_fn, key_fn);
		}

		for (usz p = 0; p < fallback1_count; p++) {
			usz i = indexs[fallback0_count + p];

			usz	start_offset = ranges[i];
			usz	end_offset   = ranges[i + 1];

			quicksort_indexed(list, low + start_offset, low + end_offset, compare_fn, key_fn);
		}

		for (usz p = 0; p < recursion_count; p++)
		{
			usz i = indexs[255 - p];

			usz	start_offset = ranges[i];
			usz	end_offset   = ranges[i + 1];

			csort(list, low + start_offset, low + end_offset, key_fn, byte_idx - 1);
		}
	}
}
